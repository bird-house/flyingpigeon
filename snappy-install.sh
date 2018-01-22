

[ -f esa-snap_sentinel_unix_6_0.sh ] && echo "esa snap installation file allready downloaded " || wget http://step.esa.int/downloads/6.0/installers/esa-snap_sentinel_unix_6_0.sh

cat <<EOT >> response.varfile
# install4j response file for ESA SNAP 6.0
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
sys.installationDir=$HOME/birdhouse/
sys.languageId=en
sys.programGroupDisabled$Boolean=false
sys.symlinkDir=/usr/local/bin
EOT

bash ./esa-snap_sentinel_unix_6_0.sh -q -varfile response.varfile -dir $HOME/birdhouse/

cp -r $HOME/birdhouse/.snap/snap-python/snappy $HOME/.conda/envs/flyingpigeon/lib/python2.7/site-packages/

rm response.varfile
