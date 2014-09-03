#!/bin/bash
if [ -f /etc/debian_version ] ; then
    sudo apt-get -y --force-yes install wget build-essential gfortran
elif [ -f /etc/redhat-release ] ; then
    sudo rpm -i http://dl.fedoraproject.org/pub/epel/6/x86_64/epel-release-6-8.noarch.rpm
    sudo yum -y install wget gcc-c++ gcc-gfortran
elif [ `uname -s` = "Darwin" ] ; then
    brew install wget
fi
