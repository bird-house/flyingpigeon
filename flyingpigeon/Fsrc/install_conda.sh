#!/bin/sh

# install blas libraries
echo 'compiling blas ...'
cd Blas

gfortran -O2 -c *.f
ar cr libblas.a *.o
mv libblas.a ../

cd ..

# install lapack libraries
echo 'install lapack ...'

cd Lapack

make all

mv *.a ../

cd ..

# install lapack95 interface
echo 'install lapack95 interface ...'

cd Lapack95/SRC

make single_double_complex_dcomplex

cd ..
cp lapack95.a liblapack95.a
cd ..

#mv *.a ../

# compile CASTf90
echo 'compile CASTf90...'

export PREFIX="$HOME/.conda/envs/fortran"
export LD_LIBRARY_PATH="$PREFIX/lib":/usr/lib

make -f Makefile.pigeon


