solid box = orthobrick (-150, -150, -150; 150, 150, 150);

solid rounded = sphere (0, 0, 0; 200);

solid roundedbox = rounded and box;

solid cylinder1 = cylinder (-160, 0, 0; 160, 0, 0; 100);
solid cylinder2 = cylinder (0, -160, 0; 0, 160, 0; 100);
solid cylinder3 = cylinder (0, 0, -160; 0, 0, 160; 100);

solid cross = cylinder1 or cylinder2 or cylinder3;

solid cutout = not cross;

solid top = roundedbox and cutout;

tlo top;
