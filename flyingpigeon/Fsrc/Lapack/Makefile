#
#  Top Level Makefile for LAPACK
#  Version 3.4.1
#  April 2012
#

include make.inc

all: lib

lib: lapacklib tmglib
#lib: blaslib variants lapacklib tmglib

clean: cleanlib cleantesting cleanblas_testing cleancblas_testing

lapack_install:
	( cd INSTALL; $(MAKE); ./testlsame; ./testslamch; ./testdlamch; \
	./testsecond; ./testdsecnd; ./testieee; ./testversion )

lapacklib:	lapack_install
	( cd SRC; $(MAKE) )

tmglib:
	( cd TESTING/MATGEN; $(MAKE) )

cleanlib:
	( cd INSTALL; $(MAKE) clean )
	( cd BLAS/SRC; $(MAKE) clean )
	( cd CBLAS; $(MAKE) clean )
	( cd SRC; $(MAKE) clean )
	( cd SRC/VARIANTS; $(MAKE) clean )
	( cd TESTING/MATGEN; $(MAKE) clean )
	( cd LAPACKE; $(MAKE) clean )


cleanblas_testing:	
	( cd BLAS/TESTING; $(MAKE) -f Makeblat1 clean )
	( cd BLAS/TESTING; $(MAKE) -f Makeblat2 clean )
	( cd BLAS/TESTING; $(MAKE) -f Makeblat3 clean )
	( cd BLAS; rm -f xblat* )

cleancblas_testing:
	( cd CBLAS; $(MAKE) cleanexe )

cleantesting:
	( cd TESTING/LIN; $(MAKE) clean )
	( cd TESTING/EIG; $(MAKE) clean )
	( cd TESTING; rm -f xlin* xeig* )

cleanall: cleanlib cleanblas_testing cleancblas_testing cleantesting 
	rm -f *.a TESTING/*.out INSTALL/test*  BLAS/*.out

