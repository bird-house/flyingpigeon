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

cd ../..

#mv *.a ../

# install netcdf fortran 
echo 'install netcdf fortran...'

cd Netcdf_fortran

export NCDIR=/usr/lib64

./configure --prefix=/usr

make check

sudo make install

cd ..

# compile CASTf90
echo 'compile CASTf90...'

make -f Makefile.home


