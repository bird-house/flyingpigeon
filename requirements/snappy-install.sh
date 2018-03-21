#!/bin/bash

# call it like this:
# bash requirements/snappy-install.sh conda_env app_root

# configuration

CWD=$(pwd -P)

# These paths can be set by Makefile in the post-install target
APP_ROOT="$CWD"
CONDA_ENV="$HOME/.conda/envs/flyingpigeon"

if [ $# -ge 1 ]; then
    echo "Setting CONDA_ENV=$1"
    CONDA_ENV="$1"
fi

if [ $# -ge 2 ]; then
    echo "Setting APP_ROOT=$2"
    APP_ROOT="$2"
fi

DOWNLOAD_CACHE="$APP_ROOT/downloads"
PREFIX="$APP_ROOT/parts"

# misc
ESA_SNAP="esa-snap_sentinel_unix_6_0.sh"
PYTHON=$CONDA_ENV/bin/python
INSTALL_DIR=$PREFIX/snap
VARFILE=$DOWNLOAD_CACHE/response.varfile

# end of configuration



echo '***************************************************'
echo '*********** start snappy installation *************'
echo '***************************************************'

mkdir -p $INSTALL_DIR

[ -f "$DOWNLOAD_CACHE/$ESA_SNAP" ] && echo "ESA SNAP installation file already downloaded " || wget -P $DOWNLOAD_CACHE http://step.esa.int/downloads/6.0/installers/$ESA_SNAP


cat <<EOT >> $VARFILE
deleteSnapDir=ALL
executeLauncherWithPythonAction$Boolean=true
forcePython$Boolean=true
pythonExecutable=$PYTHON
sys.adminRights$Boolean=false
sys.component.RSTB$Boolean=true
sys.component.S1TBX$Boolean=true
sys.component.S2TBX$Boolean=true
sys.component.S3TBX$Boolean=true
sys.component.SNAP$Boolean=true
sys.installationDir=$INSTALL_DIR
sys.languageId=en
sys.programGroupDisabled$Boolean=false
sys.symlinkDir=/usr/local/bin
EOT

bash $DOWNLOAD_CACHE/$ESA_SNAP -q -varfile $VARFILE

PY=$PYTHON
$INSTALL_DIR/bin/snappy-conf $PY $INSTALL_DIR/snap-python/

cp -r $INSTALL_DIR/snap-python/snappy $CONDA_ENV/lib/python2.7/site-packages/

echo 'snappy copied to site-packages'

rm $VARFILE

echo '***************************************************'
echo '*********** snappy installation done **************'
echo '***************************************************'
