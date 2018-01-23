echo '***************************************************'
echo '*********** start snappy installation *************'
echo '***************************************************'


[ -f esa-snap_sentinel_unix_6_0.sh ] && echo "esa snap installation file allready downloaded " || wget http://step.esa.int/downloads/6.0/installers/esa-snap_sentinel_unix_6_0.sh

cat <<EOT >> response.varfile
deleteSnapDir=ALL
executeLauncherWithPythonAction$Boolean=true
forcePython$Boolean=true
pythonExecutable=$HOME.conda/envs/flyingpigeon/bin/python
sys.adminRights$Boolean=false
sys.component.RSTB$Boolean=true
sys.component.S1TBX$Boolean=true
sys.component.S2TBX$Boolean=true
sys.component.S3TBX$Boolean=true
sys.component.SNAP$Boolean=true
sys.installationDir=$HOME/birdhouse/snap
sys.languageId=en
sys.programGroupDisabled$Boolean=false
sys.symlinkDir=/usr/local/bin
EOT

bash ./esa-snap_sentinel_unix_6_0.sh -q -varfile response.varfile

PY=$HOME/.conda/envs/flyingpigeon/bin/python
$HOME/birdhouse/snap/bin/snappy-conf $PY $HOME/birdhouse/snap/snap-python/

cp -r $HOME//birdhouse/snap/snap-python/snappy $HOME/.conda/envs/flyingpigeon/lib/python2.7/site-packages/

echo 'snappy copied to site-packages'

rm response.varfile

echo '***************************************************'
echo '*********** snappy installation done **************'
echo '***************************************************'
