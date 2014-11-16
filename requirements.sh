#!/bin/bash

if [ -f /etc/debian_version ] ; then
    echo "Installing Debian/Ubuntu packages ..."
    sudo apt-get -y install gfortran
elif [ -f /etc/redhat-release ] ; then
    echo "Installing RedHat/CentOS packages ..."
    sudo yum -y install gcc-c++ gcc-gfortran
elif [ `uname -s` = "Darwin" ] ; then
    echo "Installing MacOSX/Homebrew packages ..."
    brew install gcc R
fi
