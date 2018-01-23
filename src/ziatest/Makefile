# Created by Sue Kelly, smkelly@sandia.gov, January 2013
CC	= smpicc
MPICC	= smpicc
all:	ziatest ziaprobe
ziatest:	ziatest.c
	${CC} ziatest.c -o ziatest
ziaprobe:	ziaprobe.c
	${MPICC} ziaprobe.c -o ziaprobe
clean:
	rm -f *.o ziatest ziaprobe

