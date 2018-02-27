CC=smpicc
#CC=gcc

MPIINCLUDE=
MPILIBS=

CFLAGS=-O3 $(MPIINCLUDE)
#CFLAGS+=-DUSE_GETTIMEOFDAY
LDLIBS=$(MPILIBS) -lm

psnap: psnap-1.2.c
	$(CC) $(CFLAGS) $(LDFLAGS) psnap-1.2.c $(LDLIBS) -o $@

clean:
	rm -f psnap
