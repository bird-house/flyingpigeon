# installation :
# ./esa-snap_sentinel_unix_6_0.sh
# cp -r /home/nils/.snap/snap-python/snappy /home/nils/.conda/envs/snap/lib/python2.7/site-packages/


#############################################################
# AUTHOR: Mateusz Kędzior
# PURPOSE: Python scripts to perform Sentinel-1 processing data using ESA SNAP
# PREREQUISITES:
# - install ESA SNAP, go to terminal and type:
#    cd ~/.snap/snap-python/snappy
#    sudo /usr/bin/python setup.py install
# - install HDF5 libraries for Java:
#    sudo apt install libjhdf5-jni libjhdf5-java
# - java_max_mem setting in ~/.snap/snap-python/snappy/snappy.ini
#   is not interpreted by snappy
#   so I set _JAVA_OPTIONS in the first lines of scripts to use 4 GB of RAM
# - to avoid errors jhdf5 errors as described here: http://forum.step.esa.int/t/snappy-hdf5-error/867/3
#    execute following lines:
#    SNAP_HOME=~/snap
#    cd $SNAP_HOME/snap/modules/lib/x86_64
#    ln -s ../amd64/libjhdf.so
#    ln -s ../amd64/libjhdf5.so

# DO NOT forget that snappy for ESA SNAP is not Google library!!
# API SNAP documentation:
# http://step.esa.int/docs/v3.0/apidoc/desktop/
#############################################################

# Prefixes added to file names:
# calibrated (calculated sigma), 'Subset', 'Masked', 'Terrain Corrected',
# 'soil moisture index', 'collocated' and 'histogram' files
prefixes = ["cal", "sub", "msk", "TC", "SMI", "_coll_", "_hist_"]

import os, sys
reload(sys)
sys.setdefaultencoding('utf8')

import snappy
from snappy import ProductIO
#from snappy import GPF
from snappy import jpy

from os.path import expanduser
home = expanduser("~")

# Set below-normal priority, so that computer remain responsive during computations
# You can check how to do that on non-Unix like machines at:
# http://stackoverflow.com/questions/1023038/change-process-priority-in-python-cross-platform
os.nice(20)

# To avoid RuntimeError: java.lang.OutOfMemoryError: Java heap space
print(("Current _JAVA_OPTIONS: '" + os.environ.get('_JAVA_OPTIONS', 'Not Set')))
print("will be changed to '-Xmx4096m' to avoid OutOfMemoryError")
os.environ["_JAVA_OPTIONS"] = "-Xmx4096m"
os.system('export _JAVA_OPTIONS=-Xmx4096m')
# To enable Java core dumping:
os.system('ulimit -c unlimited')

# Sample file used in testing:
