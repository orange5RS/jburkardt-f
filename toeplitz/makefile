
#include $(CGSLROOT)/src/makefile.inc
FC = ifort
LD = ifort

FCFLAGS = -O2 -xHost -ip -traceback -warn interfaces -assume byterecl -fp-model precise -fp-model source -assume protect_parens
LDFLAGS =

JOB = 
TARGET = $(JOB).app

OBJS = jburk_toeplitz_r4vec_.o \
       jburk_toeplitz_c4vec_.o \
       $(JOB).o

MODS = $(OBJS=:.o=.mod)
SRCS = $(OBJS=:.o=.f90)

.SUFFIXES: .o .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -o $@

all: $(TARGET)

$(TARGET): $(OBJS)
	$(LD) $(LDFLAGS) -o $(TARGET) $(OBJS)
	rm -f *__genmod.f90
	rm -f *.o *.mod
	chmod 700 $(TARGET)

clean:
	rm -f *__genmod.f90
	rm -f *.app *.o *.mod
	rm -f *.log
