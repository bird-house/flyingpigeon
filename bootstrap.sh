#!/bin/bash

usage() {
    cat <<EOT
    Usage : bootstrap.sh [option]

    Options:
        -h   - Print this help message.
        -i   - Install required system packages for Birdhouse build. You *need* 'sudo' priviliges!"
        -u   - Update Makefile for Birdhouse build. Python needs to be installed."
        -b   - Both system packages will be installed (-i) and Makefile will be updated (-u). Default."
EOT
    exit 1
}

install_pkgs() {
    if [ -f /etc/debian_version ] ; then
        echo "Install Debian/Ubuntu packages for Birdhouse build ..."
        sudo apt-get update && sudo apt-get -y install python wget build-essential
    elif [ -f /etc/redhat-release ] ; then
        echo "Install CentOS packages for Birdhouse build ..."
        sudo rpm -i http://dl.fedoraproject.org/pub/epel/6/x86_64/epel-release-6-8.noarch.rpm
        sudo yum -y install wget gcc-c++
    elif [ `uname -s` = "Darwin" ] ; then
        echo "Install Homebrew packages for Birdhouse build ..."
        brew install wget libmagic
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

