solid box = orthobrick (-15, -15, -15; 15, 15, 15);

solid rounded = sphere (0, 0, 0; 20);

solid roundedbox = rounded and box;

solid cylinder1 = cylinder (-16, 0, 0; 16, 0, 0; 10);
solid cylinder2 = cylinder (0, -16, 0; 0, 16, 0; 10);
solid cylinder3 = cylinder (0, 0, -16; 0, 0, 16; 10);

solid cross = cylinder1 or cylinder2 or cylinder3;

solid cutout = not cross;

solid top = roundedbox and cutout;

tlo top;
