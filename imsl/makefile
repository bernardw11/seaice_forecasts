# makefile for a subset of imsl library

FFLAGS = -O2
FC=gfortran

all: imsl.a

imsl.a: \
	imsl.a(ehobks.o) \
	imsl.a(ehouss.o) \
	imsl.a(eigrs.o) \
	imsl.a(eqrt2s.o) \
	imsl.a(leqt2f.o) \
	imsl.a(linv2f.o) \
	imsl.a(ludatn.o) \
	imsl.a(luelmn.o) \
	imsl.a(lurefn.o) \
	imsl.a(uertst.o) \
	imsl.a(ugetio.o) \
	imsl.a(uspkd.o) \
	imsl.a(vmulff.o) \
	imsl.a(vxadd.o) \
	imsl.a(vxmul.o) \
	imsl.a(vxsto.o) 

.f.a:
	$(FC) $(FFLAGS) -c $<
	ar rv $@ $*.o
	rm -f $*.o






