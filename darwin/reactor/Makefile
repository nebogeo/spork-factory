reactor: reactor.o bbcore.o 
	gcc -o reactor reactor.o bbcore.o -ltinyscheme 

reactor.o: reactor.c ../core/bbcore.h
	gcc -c reactor.c -L../tinyscheme -I../tinyscheme -ggdb -Wall -O3 -ffast-math -Wno-unused -std=gnu99

bbcore.o: ../core/bbcore.c ../core/bbcore.h
	gcc -c ../core/bbcore.c -DEMU -ggdb -Wall -O3 -ffast-math -Wno-unused -std=gnu99 

