# 

SHELL = /bin/sh
.IGNORE:

# System-specific settings
CC =		smpicc
# CC =		cc
CCFLAGS =	-O2
LINK =		smpicc
# LINK =		cc
LINKFLAGS =	

# Compilation rules

.c.o:
	$(CC) $(CCFLAGS) -c $<

OBJS = mpi_overhead.o

mpi_overhead: $(OBJS) Makefile
	$(LINK) $(LINKFLAGS) -o mpi_overhead $(OBJS)

clean:
	rm -rf *.o mpi_overhead
