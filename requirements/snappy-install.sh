echo '***************************************************'
echo '*********** start snappy installation *************'
echo '***************************************************'

CONDA_ENV=$1
PY=$(which python)
APP_ROOT=$2
installationDir=$APP_ROOT'/../snap'

echo 'snappy installing into:' $CONDA_ENV

[ -f esa-snap_sentinel_unix_6_0.sh ] && echo "esa snap installation file allready downloaded " || wget http://step.esa.int/downloads/6.0/installers/esa-snap_sentinel_unix_6_0.sh


cat <<EOT >> response.varfile
deleteSnapDir=ALL
executeLauncherWithPythonAction$Boolean=true
forcePython$Boolean=true
pythonExecutable=$PY
sys.adminRights$Boolean=false
sys.component.RSTB$Boolean=true
sys.component.S1TBX$Boolean=true
sys.component.S2TBX$Boolean=true
sys.component.S3TBX$Boolean=true
sys.component.SNAP$Boolean=true
sys.installationDir=$installationDir
sys.languageId=en
sys.programGroupDisabled$Boolean=false
sys.symlinkDir=/usr/local/bin
EOT

bash ./esa-snap_sentinel_unix_6_0.sh -q -varfile response.varfile

$installationDir'/bin/snappy-conf' $PY $installationDir'/snap-python/'

cp -r $installationDir'/snap-python/snappy' $CONDA_ENV'/flyingpigeon/lib/python2.7/site-packages/'

echo 'snappy copied to site-packages'

rm response.varfile

echo '***************************************************'
echo '*********** snappy installation done **************'
echo '***************************************************'
