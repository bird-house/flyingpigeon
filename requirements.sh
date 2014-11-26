#!/bin/bash

if [ -f /etc/debian_version ] ; then
    echo "Installing Debian/Ubuntu packages ..."
    sudo apt-get -y install gfortran
    sudo apt-get -y install nco
elif [ -f /etc/redhat-release ] ; then
    echo "Installing RedHat/CentOS packages ..."
    sudo yum -y install gcc-c++ gcc-gfortran
    sudo yum -y install nco
elif [ `uname -s` = "Darwin" ] ; then
    echo "Installing MacOSX/Homebrew packages ..."
    brew install gcc R
fi
