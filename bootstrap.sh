#!/bin/bash

usage() {
    cat <<EOT
    Usage : bootstrap.sh [option]

    Options:
        -h   - Print this help message.
        -i   - Installs required system packages for Birdhouse build. You *need* 'sudo' priviliges!"
        -u   - Updates Makefile for Birdhouse build. Python needs to be installed."
        -b   - Both system packages will be installed (-i) and Makefile will be updated (-u). Default."
EOT
    exit 1
}

install_pkgs() {
    if [ -f /etc/debian_version ] ; then
        echo "Install Debian/Ubuntu packages for Birdhouse build ..."
        sudo apt-get update && sudo apt-get -y install python wget curl build-essential
        sudo apt-get -y install vim-common # anaconda needs xxd
    elif [ -f /etc/redhat-release ] ; then
        echo "Install CentOS packages for Birdhouse build ..."
        #sudo rpm -i http://dl.fedoraproject.org/pub/epel/6/x86_64/epel-release-6-8.noarch.rpm
        sudo yum update -y
        sudo yum install -y epel-release
        sudo yum install -y wget
        sudo yum install -y curl
        sudo yum install -y gcc-c++
        sudo yum install -y make
        sudo yum install -y tar
        sudo yum install -y bzip2
        sudo yum install -y vim-common  # anaconda needs xxd
    elif [ `uname -s` = "Darwin" ] ; then
        echo "Install Homebrew packages for Birdhouse build ..."
        brew install wget curl libmagic
    fi
}

fetch_makefile() {
    echo "Fetching current Makefile for Birdhouse build ..."
    python -c 'import urllib; print urllib.urlopen("https://raw.githubusercontent.com/bird-house/birdhousebuilder.bootstrap/master/Makefile").read()' > Makefile
}

bootstrap() {
    echo "Bootstrapping ..."

    if [ $# -eq 0 ] || [ $1 = '-b' ] || [ $1 = '-i' ]; then
        install_pkgs
    fi

    if [ $# -eq 0 ] || [ $1 = '-b' ] || [ $1 = '-u' ]; then
        fetch_makefile
    fi

    echo "Bootstrapping done"
}

# Handling arguments

if [ $# -gt 1 ]; then
    echo -e "Too many arguments.\n"
    usage
fi

if [ $# -gt 0 ] && [ $1 = '-h' ]; then
    usage
fi

if [ $# -eq 0 ] || [ $1 = '-b' ] || [ $1 = '-i' ] || [ $1 = '-u' ]; then
    bootstrap $@
else
    echo -e "Unknown option: $1.\n"
    usage
fi

