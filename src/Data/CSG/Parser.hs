{-# LANGUAGE OverloadedStrings #-}

{-|

Parser for CSG solid definition format. The format uses text files and
is inspired by NETGEN 4.x @.geo@ format.

Each definition may contain several solid definitions and ends with
the top level object declaration. Right hand side of solid
equations may reference other solids to allow composing of complex
solids.

Multiple-solid compositions are right-associative.

> # comment
>
> # define several primitives
> solid b1 = sphere (0, 0, 0; 5);
> solid p1 = plane (0, 0, 0; 1, 0, 0);
>
> # define a composition
> solid comp = b1 and p1;
>
> # make it the top level object
> tlo comp;

Statements must end with a semicolon (newlines are optional).
Whitespace is ignored.

Top-level object line must reference a previously defined solid.

Syntax for primitives follows the signatures of 'CSG' constructors
for 'CSG.plane' and 'CSG.sphere', but differs for cylinder and
cone, as this module provides access only to frustums
('CSG.cylinderFrustum' and 'CSG.coneFrustum').

[Half-space] @plane (px, py, pz; nx, ny, nz)@, where @(px, py, pz)@
is a point on a plane which defines the half-space and @(nx, ny,
nz)@ is a normal to the plane (outward to the half-space), not
necessarily a unit vector.

[Brick] @orthobrick (ax, ay, az; bx, by, bz)@, where @(ax, ay, az)@ is
a vertex with minimum coordinates and @(bx, by, bz)@ is a vertex with
maximum coordinates.

[Sphere] @sphere (cx, cy, cz; r)@, where @(cx, cy, cz)@ is a
central point of a sphere and @r@ is radius.

[Right circular cylinder] @cylinder (p1x, p1y, p1z; p2x, p2y, p2z;
r)@ where @(p1x, p1y, p1z)@ and @(p2x, p2y, p2z)@ are bottom and
top points on axis and @r@ is radius.

[Right circular conical frustum] @cone (p1x, p1y, p1z; r1; p2x,
p2y, p2z; r2)@ where @(p1x, p1y, p1z)@ and @(p2x, p2y, p2z)@ are
bottom and top points on cone axis and @r1@, @r2@ are the
corresponding radii.

-}

module Data.CSG.Parser
    ( parseGeometry
    , parseGeometryFile
    )

where

import Prelude as P

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 as B

import qualified Data.Map as M

import Data.Vec3 hiding (Vec3, Matrix)

import qualified Data.CSG as CSG


-- | Transformer which adds a lookup table to a monad.
type TableT a k v = StateT (M.Map k v) a


-- | Add an entry to the lookup table.
addEntry :: (Ord k, Monad a) => k -> v -> TableT a k v ()
addEntry key value = fmap (M.insert key value) get >>= put


-- | Lookup entry in the table.
getEntry :: (Ord k, Monad a) => k -> TableT a k v (Maybe v)
getEntry key = fmap (M.lookup key) get


-- | Parser with a lookup table.
type CSGParser = TableT Parser String CSG.Solid


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


-- | Read three comma-separated doubles into point.
--
-- > <triple> ::= <double> ',' <double> ',' <double>
triple :: Parser CSG.Point
triple = fmap fromXYZ $
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
           , "orthobrick"
           , "plane"
           , "sphere"
           , "cylinder"
           , "cone"
           ]


-- | Read variable name or fail if it's a keyword.
varName :: CSGParser String
varName = do
  k <- lift $ many1 (letter_ascii <|> digit)
  if k `P.notElem` keywords
    then return k
    else fail ("Unexpected keyword when reading a solid name: " ++ k)


-- | Look up a solid in the table by its name or fail if it's not
-- defined yet.
readName :: CSGParser CSG.Solid
readName = do
  k <- varName
  v <- getEntry k
  case v of
    Just b -> return b
    _ -> fail $ "Undefined solid: " ++ k


-- > <plane> ::=
-- >   'plane (' <triple> ';' <triple> ')'
plane :: Parser CSG.Solid
plane = CSG.plane <$>
        (string "plane" *> skipSpace *> lp *> skipSpace *> triple) <*>
        (skipSpace *> cancer *> skipSpace *> triple <* skipSpace <* rp)


-- > <orthobrick> ::=
-- >   'orthobrick (' <triple> ';' <triple> ')'
orthobrick :: Parser CSG.Solid
orthobrick = CSG.cuboid <$>
        (string "orthobrick" *> skipSpace *> lp *> skipSpace *> triple) <*>
        (skipSpace *> cancer *> skipSpace *> triple <* skipSpace <* rp)


-- > <sphere> ::=
-- >   'sphere (' <triple> ';' <double> ')'
sphere :: Parser CSG.Solid
sphere = CSG.sphere <$>
        (string "sphere" *> skipSpace *> lp *> skipSpace *> triple) <*>
        (skipSpace *> cancer *> skipSpace *> double <* skipSpace <* rp)


-- > <cylinder> ::=
-- >   'cylinder (' <triple> ';' <triple> ';' <double> ')'
cylinder :: Parser CSG.Solid
cylinder = CSG.cylinderFrustum <$>
           (string "cylinder" *> skipSpace *> lp *> skipSpace *> triple) <*>
           (skipSpace *> cancer *> skipSpace *> triple) <*>
           (skipSpace *> cancer *> skipSpace *> double <* skipSpace <* rp)


-- > <cone> ::=
-- >   'cone (' <triple> ';' <double> ';' <triple> ';' <double> ')'
cone :: Parser CSG.Solid
cone = CSG.coneFrustum <$>
       ((,) <$>
        (string "cone" *> skipSpace *> lp *> skipSpace *> triple) <*>
        (skipSpace *> cancer *> skipSpace *> double)) <*>
       ((,) <$>
        (skipSpace *> cancer *> skipSpace *> triple) <*>
        (skipSpace *> cancer *> skipSpace *> double <* skipSpace <* rp))


primitive :: Parser CSG.Solid
primitive = plane <|> orthobrick <|> sphere <|> cylinder <|> cone


-- > <complement> ::= 'not' <solid>
complement :: CSGParser CSG.Solid
complement = CSG.complement <$> (lift (string "not" *> skipSpace) *> solid)


-- > <union> ::= <uncomposed-solid> 'or' <solid>
union :: CSGParser CSG.Solid
union = binary "or" CSG.unite


-- > <intersection> ::= <uncomposed-solid> 'and' <solid>
intersection :: CSGParser CSG.Solid
intersection = binary "and" CSG.intersect


-- | Parse binary operation on two bodies with given composition
-- operators.
binary :: ByteString -> (CSG.Solid -> CSG.Solid -> CSG.Solid) -> CSGParser CSG.Solid
binary op compose = do
  b1 <- uncomposedSolid
  lift (skipSpace *> string op *> skipSpace)
  b2 <- solid
  return $ compose b1 b2


-- | Read a stamement which adds a new solid entry to the lookup
-- table.
--
-- > <statement> ::=
-- >   'solid' <varname> '=' <solid> ';'
statement :: CSGParser ()
statement = do
  lift $ string "solid" *> skipSpace
  k <- varName
  lift $ skipSpace <* eq <* skipSpace
  v <- solid <* lift (cancer *> skipSpace)
  addEntry k v


-- | Expression is either a primitive, a reference to previously
-- defined solid or an operation on expressions.
--
-- > <solid> ::= <union> | <intersection> | <complement> | <primitive> | <reference>
solid :: CSGParser CSG.Solid
solid = union <|> intersection <|> complement <|> uncomposedSolid


-- | Used to terminate left branch of binary compositions.
--
-- > <uncomposed-solid> ::= <primitive> | <reference>
uncomposedSolid :: CSGParser CSG.Solid
uncomposedSolid = lift primitive <|> readName


-- | Top-level object declaration.
--
-- > <tlo> ::= 'tlo' <solid> ';'
topLevel :: CSGParser CSG.Solid
topLevel = lift (string "tlo" *> skipSpace) *>
           readName
           <* lift (cancer <* skipSpace)


-- | Read one-line comment starting with hash sign.
comment :: Parser ()
comment = char '#' >> manyTill anyChar endOfLine >> return ()


-- | Read sequence of statements which define solids, and finally read
-- top level object definition.
--
-- > <geoFile> ::= <statement> <geoFile> | <comment> <geoFile> | <tlo>
geoFile :: CSGParser CSG.Solid
geoFile = many1 (lift comment <|> statement) *> topLevel


-- | Read solid definition. If parsing fails, return error message as a
-- string.
parseGeometry :: ByteString -> Either String CSG.Solid
parseGeometry input =
    case parseOnly (runStateT geoFile M.empty) input of
      Right (b, _) -> Right b
      Left msg -> Left msg


-- | Read solid definition from a file. If parsing fails, return error
-- message as a string.
parseGeometryFile :: FilePath -> IO (Either String CSG.Solid)
parseGeometryFile file = do
  res <- E.try $ B.readFile file
  return $ case res of
             Right d -> parseGeometry d
             Left e -> Left $ show (e :: E.IOException)
