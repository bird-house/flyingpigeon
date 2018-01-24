# configuration

CWD=$(pwd -P)

# These paths can be set by Makefile in the post-install target
DOWNLOAD_CACHE="$CWD/downloads"
PREFIX="$CWD/parts"
CONDA_ENV="$HOME/.conda/envs/flyingpigeon"

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

[ -f "$DOWNLOAD_CACHE/$ESA_SNAP" ] && echo "esa snap installation file allready downloaded " || wget -P $DOWNLOAD_CACHE http://step.esa.int/downloads/6.0/installers/$ESA_SNAP

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
