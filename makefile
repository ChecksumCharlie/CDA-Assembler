all : clean assembler

assembler : utility.o vector.o
	gcc assembler.c main.c utility.o vector.o -o assembler
	
utility.o : utility.c
	gcc -c utility.c

vector.o : vector.c
	gcc -c vector.c

clean :
	rm -rf *.o assembler
