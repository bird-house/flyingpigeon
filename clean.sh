#!/bin/bash
BUILDOUT_DIR=`dirname $0`

echo "Cleaning buildout ..."
if [ -f custom.cfg ]; then
    echo "removing custom.cfg ... backup is custom.cfg.bak"
    mv -f custom.cfg custom.cfg.bak
fi
rm -rf $BUILDOUT_DIR/downloads
rm -rf $BUILDOUT_DIR/eggs
rm -rf $BUILDOUT_DIR/develop-eggs
rm -rf $BUILDOUT_DIR/parts
rm -rf $BUILDOUT_DIR/bin
rm -f $BUILDOUT_DIR/.installed.cfg
rm -rf $BUILDOUT_DIR/*.egg-info
rm -rf $BUILDOUT_DIR/dist
rm -rf $BUILDOUT_DIR/build
echo "Cleaning buildout ... Done"
