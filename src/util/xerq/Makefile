
# These lines are needed on some machines.
MAKE=make
SHELL=/bin/sh
INSTALL=/usr/bin/install -c
mkinstalldirs=$(SHELL) ../../mkinstalldirs
#
CC=gcc -std=gnu99

prefix=/usr/local/mud
exec_prefix=${prefix}
datarootdir=${prefix}/share

SUBDIRS = 
SED = sed

BINDIR=${exec_prefix}/bin
MUD_LIB=${exec_prefix}/lib
ERQ_DIR=${exec_prefix}/libexec


#PROFIL= -DOPCPROF -DVERBOSE_OPCPROF
#PROFIL=-p -DMARK
#PROFIL=-pg
PROFIL=
#Enable warnings from the compiler, if wanted.
WARN= # no warning options - will work with all compilers :-)
#WARN= -Wall -Wshadow -Dlint
#WARN= -Wall -Wshadow -Wno-parentheses # gcc settings
#
# Optimization and source level debugging options.
# adding a -fomit-frame-pointer on the NeXT (gcc version 1.93 (68k, MIT syntax))
# will corrupt the driver.
HIGH_OPTIMIZE = -O4 -fomit-frame-pointer  -g # high optimization
MED_OPTIMIZE= -O2 -g # medium optimization
LOW_OPTIMIZE = -O -g # minimal optimization
NO_OPTIMIZE= -g # no optimization; for frequent recompilations.

OPTIMIZE= $(MED_OPTIMIZE)

# The main debugging level is define in config.h 
# Add additional options here.
DEBUG=
#
MPATH=-I../.. -DMUD_LIB='"$(MUD_LIB)"' -DBINDIR='"$(BINDIR)"' -DERQ_DIR='"$(ERQ_DIR)"'
#
CFLAGS= -I/usr/include/mysql  -fwrapv $(OPTIMIZE) $(DEBUG) $(WARN) $(MPATH) $(PROFIL)
#
LIBS=-lnsl -lm 
#
LDFLAGS=

OBJ = erq.o execute.o socket.o lookup.o

erq:	$(OBJ)
	$(CC) $(CFLAGS) $(LDFLAGS) $(OBJ) -o $@ $(LIBS)

%.o : %.c
	$(CC) -I../.. $(CFLAGS) -c $< -o $@

*.o: erq.h defs.h \
	../../config.h \
	../../machine.h \
	../../port.h

install: erq
	$(mkinstalldirs) $(BINDIR) $(ERQ_DIR)
	$(INSTALL) erq $(BINDIR)/erq

clean:
	rm -f erq *.o *~

realclean: clean
	
