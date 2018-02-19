# https://en.wikipedia.org/wiki/Atmospheric_entry#Blunt_body_entry_vehicles

solid body = cone (0, 0, 0; 10; 0, 0, 13; 5);
solid head = cylinder (0, 0, 13; 0, 0, 14.5; 4.6);

solid rounding = sphere (0, 0, 14.96629; 18);
solid cutoff = plane (0, 0, 0; 0, 0, 1);

solid butt = rounding and cutoff;

solid all = body or butt or head;

tlo all;
