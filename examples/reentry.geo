# https://en.wikipedia.org/wiki/Atmospheric_entry#Blunt_body_entry_vehicles

solid body = cone (0, 0, 0; 100; 0, 0, 130; 50);
solid head = cylinder (0, 0, 130; 0, 0, 145; 46);

solid rounding = sphere (0, 0, 149.6629; 180);
solid cutoff = plane (0, 0, 0; 0, 0, 10);

solid butt = rounding and cutoff;

solid all = body or butt or head;

tlo all;
