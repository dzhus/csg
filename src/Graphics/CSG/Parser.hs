{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parser for textual CSG body definition format.
--
-- Body definition contains a number of solid definitions and ends
-- with the top level object definition. RHS of solid equations may
-- reference other solids to compose into complex bodies.
--
-- Multiple-body compositions are right-associative.
--
-- > # comment
-- >
-- > # define few primitives
-- > solid b1 = sphere (0, 0, 0; 5);
-- > solid p1 = plane (0, 0, 0; 1, 0, 0);
-- >
-- > # define a composition
-- > solid body = b1 and p1;
-- >
-- > # assign it to be the top level object
-- > tlo body;
--
-- Statements must end with a semicolon (newlines are optional).
-- Excessive spaces are ignored.
--
-- Top-level object line must reference a previously defined solid.
--
-- Syntax for primitives follows the signatures of 'CSG' constructors
-- for 'CSG.plane' and 'CSG.sphere', but differs for cylinder and
-- cone, as this module provides access only to frustums
-- ('CSG.cylinderFrustum' and 'CSG.coneFrustum').
--
-- [Half-space] @plane (px, py, pz; nx, ny, nz)@, where @(px, py, pz)@
-- is a point on a plane which defines the half-space and @(nx, ny,
-- nz)@ is a normal to the plane (outward to the half-space), not
-- necessarily a unit vector.
--
-- [Sphere] @sphere (cx, cy, cz; r)@, where @(cx, cy, cz)@ is a
-- central point of a sphere and @r@ is radius.
--
-- [Right circular cylinder] @cylinder (p1x, p1y, p1z; p2x, p2y, p2z;
-- r)@ where @(p1x, p1y, p1z)@ and @(p2x, p2y, p2z)@ are bottom and
-- top points on axis and @r@ is radius.
--
-- [Right circular conical frustum] @cone (p1x, p1y, p1z; r1; p2x,
-- p2y, p2z; r2)@ where @(p1x, p1y, p1z)@ and @(p2x, p2y, p2z)@ are
-- bottom and top points on cone axis and @r1@, @r2@ are the
-- corresponding radii.

module Graphics.CSG.Parser
    ( parseBody
    , parseBodyFile
    )

where

import Prelude as P

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Attoparsec.Char8
import Data.ByteString.Char8 as B

import qualified Data.Map as M

import Data.Vec3 hiding (Vec3, Matrix)

import qualified Graphics.CSG as CSG


type Point = SVec3


-- | Transformer which adds lookup table to underlying monad.
type TableT a k v = StateT (M.Map k v) a


-- | Add entry to the lookup table.
addEntry :: (Ord k, Monad a) => k -> v -> TableT a k v ()
addEntry key value = liftM (M.insert key value) get >>= put


-- | Lookup entry in the table.
getEntry :: (Ord k, Monad a) => k -> TableT a k v (Maybe v)
getEntry key = liftM (M.lookup key) get


-- | Parser with lookup table.
type CSGParser = TableT Parser String CSG.Body


lp :: Parser Char
lp = char '('


rp :: Parser Char
rp = char ')'


eq :: Parser Char
eq = char '='


cancer :: Parser Char
cancer = char ';'


comma :: Parser Char
comma = char ','


-- | Read comma-separated three doubles into point.
--
-- > <triple> ::= <double> ',' <double> ',' <double>
triple :: Parser Point
triple = liftM fromXYZ $
          (,,) <$> double
                   <*>
                   (skipSpace *> comma *> skipSpace *>
                    double
                    <* skipSpace <* comma <* skipSpace)
                   <*>
                   double


keywords :: [String]
keywords = [ "solid"
           , "tlo"
           , "plane"
           , "sphere"
           , "cylinder"
           , "cone"
           ]


-- | Read variable name or fail if it's a keyword.
varName :: CSGParser String
varName = do
  k <- lift $ many1 (letter_ascii <|> digit)
  case (P.elem k keywords) of
    False -> return k
    True -> fail $ "Unexpected keyword: " ++ k


-- | Lookup body in table by its name or fail if it is undefined.
readName :: CSGParser CSG.Body
readName = do
  k <- varName
  v <- getEntry k
  case v of
    Just b -> return b
    _ -> fail $ "Undefined solid: " ++ k


-- > <plane> ::=
-- >   'plane (' <triple> ';' <triple> ')'
plane :: Parser CSG.Body
plane = CSG.plane <$>
        (string "plane" *> skipSpace *> lp *> skipSpace *> triple) <*>
        (skipSpace *> cancer *> skipSpace *> triple <* skipSpace <* rp)


-- > <sphere> ::=
-- >   'sphere (' <triple> ';' <double> ')'
sphere :: Parser CSG.Body
sphere = CSG.sphere <$>
        (string "sphere" *> skipSpace *> lp *> skipSpace *> triple) <*>
        (skipSpace *> cancer *> skipSpace *> double <* skipSpace <* rp)


-- > <cylinder> ::=
-- >   'cylinder (' <triple> ';' <triple> ';' <double> ')'
cylinder :: Parser CSG.Body
cylinder = CSG.cylinderFrustum <$>
           (string "cylinder" *> skipSpace *> lp *> skipSpace *> triple) <*>
           (skipSpace *> cancer *> skipSpace *> triple) <*>
           (skipSpace *> cancer *> skipSpace *> double <* skipSpace <* rp)


-- > <cone> ::=
-- >   'cone (' <triple> ';' <double> ';' <triple> ';' <double> ')'
cone :: Parser CSG.Body
cone = CSG.coneFrustum <$>
       ((,) <$>
        (string "cone" *> skipSpace *> lp *> skipSpace *> triple) <*>
        (skipSpace *> cancer *> skipSpace *> double)) <*>
       ((,) <$>
        (skipSpace *> cancer *> skipSpace *> triple) <*>
        (skipSpace *> cancer *> skipSpace *> double <* skipSpace <* rp))


primitive :: Parser CSG.Body
primitive = plane <|> sphere <|> cylinder <|> cone


-- > <complement> ::= 'not' <body>
complement :: CSGParser CSG.Body
complement = CSG.complement <$> (lift (string "not" *> skipSpace) *> body)


-- > <union> ::= <uncomposed-body> 'or' <body>
union :: CSGParser CSG.Body
union = binary "or" CSG.unite


-- > <intersection> ::= <uncomposed-body> 'and' <body>
intersection :: CSGParser CSG.Body
intersection = binary "and" CSG.intersect


-- | Parse binary operation on two bodies with given composition
-- operators.
binary :: ByteString -> (CSG.Body -> CSG.Body -> CSG.Body) -> CSGParser CSG.Body
binary op compose = do
  b1 <- uncomposedBody
  lift (skipSpace *> string op *> skipSpace)
  b2 <- body
  return $ compose b1 b2


-- | Read stamement which adds new solid entry to lookup table.
--
-- > <statement> ::=
-- >   'solid' <varname> '=' <body> ';'
statement :: CSGParser ()
statement = do
  lift $ string "solid" *> skipSpace
  k <- varName
  lift $ skipSpace <* eq <* skipSpace
  v <- body <* lift (cancer *> skipSpace)
  addEntry k v


-- | Expression is either a primitive, a reference to previously
-- defined solid or an operation on expressions.
--
-- > <body> ::= <union> | <intersection> | <complement> | <primitive> | <reference>
body :: CSGParser CSG.Body
body = union <|> intersection <|> complement <|> uncomposedBody


-- Used to terminate left branch of binary compositions.
--
-- > <uncomposed-body> ::= <primitive> | <reference>
uncomposedBody :: CSGParser CSG.Body
uncomposedBody = lift primitive <|> readName


-- | Top-level object declaration.
--
-- > <tlo> ::= 'tlo' <body> ';'
topLevel :: CSGParser CSG.Body
topLevel = lift (string "tlo" *> skipSpace) *>
           readName
           <* lift (cancer <* skipSpace)


-- | Read one-line comment starting with hash sign.
comment :: Parser ()
comment = char '#' >> (manyTill anyChar endOfLine) >> return ()


-- | Read sequence of statements which define solids, and finally read
-- top level object definition.
--
-- > <geoFile> ::= <statement> <geoFile> | <comment> <geoFile> | <tlo>
geoFile :: CSGParser CSG.Body
geoFile = (many1 $ lift comment <|> statement) *> topLevel


-- | Try to read body definition from bytestring. Return body or error
-- message if parsing fails.
parseBody :: ByteString -> Either String CSG.Body
parseBody input =
    case (parseOnly (runStateT geoFile M.empty) input) of
      Right (b, _) -> Right b
      Left msg -> Left msg


-- | Read body definition from file. If parsing fails or IOError when
-- reading file occurs, return error message.
parseBodyFile :: FilePath -> IO (Either String CSG.Body)
parseBodyFile file = do
  res <- E.try $ B.readFile file
  return $ case res of
             Right d -> parseBody d
             Left e -> Left $ show (e :: E.IOException)
