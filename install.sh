#!/bin/bash

# user settings
ANACONDA_HOME=$HOME/anaconda

# don't change these settings
BUILDOUT_DIR=`dirname $0`
DOWNLOAD_CACHE=$BUILDOUT_DIR/downloads
ANACONDA_URL=http://repo.continuum.io/miniconda
FN_LINUX=Miniconda-latest-Linux-x86_64.sh
FN_OSX=Miniconda-3.5.5-MacOSX-x86_64.sh

function install_anaconda() {
    # run miniconda setup, install in ANACONDA_HOME
    if [ -d $ANACONDA_HOME ]; then
        echo "Anaconda already installed in $ANACONDA_HOME."
    else
        FN=$FN_LINUX
        if [ `uname -s` = "Darwin" ] ; then
            FN=$FN_OSX
        fi

        echo "Installing $FN ..."

        # download miniconda setup script to download cache with wget
        mkdir -p $DOWNLOAD_CACHE
        wget -q -c -O "$DOWNLOAD_CACHE/$FN" $ANACONDA_URL/$FN
        bash "$DOWNLOAD_CACHE/$FN" -b -p $ANACONDA_HOME   
    fi

    # add anaconda path to user .bashrc
    echo -n "Add \"$ANACONDA_HOME/bin\" to your PATH: "
    echo "\"export PATH=$ANACONDA_HOME/bin:\$PATH\""

    echo "Installing Anaconda ... Done"
}

# set default configurion file for buildout
function setup_cfg() {
    if [ ! -d $DOWNLOAD_CACHE ]; then
        echo "Creating buildout downloads cache $DOWNLOAD_CACHE."
        mkdir -p $DOWNLOAD_CACHE
    fi

    if [ ! -f custom.cfg ]; then
        echo "Copy default configuration to $BUILDOUT_DIR/custom.cfg"
        cp custom.cfg.example custom.cfg
    else
        echo "Using custom configuration $BUILDOUT_DIR/custom.cfg"
    fi
}

# run install
function install() {
    echo "Installing ..."
    echo "BUILDOUT_DIR=$BUILDOUT_DIR"
    echo "DOWNLOAD_CACHE=$DOWNLOAD_CACHE"

    pushd $BUILDOUT_DIR || exit 1
    
    setup_cfg

    if [ ! -f "$BUILDOUT_DIR/bin/buildout" ]; then
        "$ANACONDA_HOME/bin/python" bootstrap.py -c custom.cfg
        echo "Bootstrap ... Done"
    fi

    "$BUILDOUT_DIR/bin/buildout" -c custom.cfg

    popd || exit 1

    echo "Installing ... Done"
}

function usage() {
    echo "Usage: $0"
    exit 1
}

install_anaconda
install

exit 0