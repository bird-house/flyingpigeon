#!/bin/sh -l
# © LSCE – Laboratory related to CEA/DSM – CNRS – UVSQ, 
# Sabine Radanovics (sabine.radanovics@lsce.ipsl.fr) andPascal Yiou (pascal.yiou@lsce.ipsl.fr)
# This script is part of the CASTf90 software IDDN.FR.001.030008.000.S.P.2016.000.20700
#
#This software is governed by the CeCILL license under French law and abiding by the rules of distribution 
#of free software. You can use, modify and / or redistribute the software under the terms of the 
# CeCILL license as circulated by CEA, CNRS and INRIA at the following URL "http://www.cecill.info".
#
# As a counterpart to the access to the source code and rights to copy, modify and redistribute granted by 
# the license, users are provided only with a limited warranty and the software's author, 
# the holder of the economic rights, and the successive licensors have only limited liability.
#
#In this respect, the user's attention is drawn to the risks associated with loading, using, 
#modifying and/or developing or reproducing the software by the user in light of its specific status 
# of free software, that may mean that it is complicated to manipulate, and that also therefore means 
# that it is reserved for developers and experienced professionals having in-depth computer knowledge. 
# Users are therefore encouraged to load and test the software's suitability as regards their requirements 
# in conditions enabling the security of their systems and/or data to be ensured and, more generally, 
# to use and operate it in the same conditions as regards security.
#
#The fact that you are presently reading this means that you have had knowledge of the CeCILL license 
#and that you accept its terms.
#
# Calcul en temps continu d'analogues de SLP sur les reanalyses NCEP
# Pascal Yiou (LSCE)
## Version 1.0
# Se lance par:
# qsub -q mediump -l nodes=1:ppn=12 /home/users/yiou/RStat/A2C2/analogs_slp-genericpar.sh
#
# update S. Radanovics 04/2015:
# handling options
# replace calculation of analogues in R by fortran program

# write start date and time of script
start_date=`date +"%d/%m/%Y (%H:%M:%S)"`
echo -e "\n\nStarting script at: ${start_date}\n"
date_stamp=`date +%Y%m%d_%H%M%S`
echo $date_stamp

# set defaults for arguments
lwin=1
pdomain=-80.0,50.0,22.5,70.0
psim=1948-01-01,2014-12-31
pbase=1948-01-01,2014-12-31
region=NA
basedir=/home/users/sradanov/Data/NCEP/
simdir=/home/users/sradanov/Data/NCEP/
outdir=/home/scratch01/sradanov/A2C2/
seacycnorm=base
wma=91
calccor=TRUE

distancefun=rms
seasonwin=30
nanalog=20
varname=slp
level=surface
detrend=FALSE
silent=bla

#please set this line to the directory containing the source.
sourcedir=${HOME}/Code/Analogue/RSdev

# processing arguments
while getopts 'D:C:S:B:R:P:p:o:m:N:c:w:d:v:n:l:t:s:h:' opt ; do
 case $opt in
  D) pdomain=$OPTARG ;;
  C) lwin=$OPTARG ;;
  S) psim=$OPTARG ;;
  B) pbase=$OPTARG ;;
  R) region=$OPTARG ;;
  P) basedir=$OPTARG ;;
  p) simdir=$OPTARG ;;
  o) outdir=$OPTARG ;;
  m) wma=$OPTARG ;;
  N) seacycnorm=$OPTARG ;;
  c) calccor=$OPTARG ;;
  w) seasonwin=$OPTARG ;;
  d) distancefun=$OPTARG ;;
  v) varname=$OPTARG ;;
  n) nanalog=$OPTARG ;;
  l) level=$OPTARG ;;
  t) detrend=$OPTARG ;;
  s) silent=$OPTARG ;;
  h) echo -e "Usage: $0 [options] \n" ; 
   echo -e "Options: \n" ;
   echo "  -D<lonmin>,<lonmax>,<latmin>,<latmax> (predictor domain def: $pdomain )" ;
   echo "  -C<numberofdays> (number of days to average the analogy criterion def: $lwin )" ;
   echo "  -S<YYYY-MM-DD>,<YYYY-MM-DD> (simulation period def: $psim )" ;
   echo "  -B<YYYY-MM-DD>,<YYYY-MM-DD> (data base/archive period def: $pbase )" ;
   echo "  -R<region name> (known region names are NA and NHmid (def: $region)" ;
   echo "    you can add one by adding a case in retrieve.sh" ;
   echo "  -P<path to base data> (def: $basedir)" ;
   echo "  -p<path to simulation data> (def: $simdir)" ;
   echo "  -o<path to write output file> (def: $outdir)" ;
   echo "  -N<mode> define which seasonal cycle should be removed. Choose" ;
   echo "     base to remove cycle of the data base/archive from all data sets" ;
   echo "     sim to remove cycle of the simulation data set from all data sets (caution : make sure that the simulation is sufficiently long for meaningful cycle calculation)" ;
   echo "     own to remove its own cycle from each of the data sets" ;
   echo "     none to use raw data" ;
   echo "     (default: $seacycnorm)" ;
   echo "  -m<numberofdays> (number of days to calculate weighted moving average for seasonal cycle smoothing" ;
   echo "  -c<logical> TRUE if correlation should be calculated as an additional diagnostic, FALSE if not (def: TRUE)" ;
   echo "  -w<numberofdays> (number of days +- around the target day to consider as candidates. def: $seaonwin)" ;
   echo "  -d<distance> name of the distance to use for analogue calculation (def: $distancefun)" ;
   echo "  -n<numberofanalogues> Number of closest analogue dates to write to output (def: $nanalog)" ;
   echo "  -v<varname> name of the NCEP field to download" ;
   echo "     The name has to be the same as in the filename in the NCEP database (def:$varname)" ;
   echo "  -l<vertical level> Either 'surface' for variables like slp or pressure level in hPa, e.g. '500'" ;
   echo "  -t<logical> TRUE if the predictor variable (see -v) should be detrended, FALSE if not (def: FALSE)" ;
   echo "      For example for geopotential as a circulation variable in order to remove the temperature induced trend" ;
   echo "  -silent reduced standard output" ;
   echo " " ;
   exit 1 ;;
 esac
done

module load cdo/1.6
## Telechargement des reanalyses:
if [ $silent != "ilent" ]; then echo -e "Downloading NCEP SLP data"; fi
./getNCEP_slp.sh ${pdomain} ${region} ${basedir} ${pbase} ${simdir} ${psim} ${sourcedir} ${varname} ${level} ${detrend} ${seacycnorm} ${date_stamp} ${lwin} ${seasonwin} ${distancefun} ${outdir} ${silent}

case $seacycnorm in
  none) sesync=.FALSE. ;;
  *) sesync=.TRUE. ;;
esac

# write stuff to configuration file
cat <<EOF >> config_${date_stamp}.txt
&PARAM
 my_params%timewin = $lwin
 my_params%varname = "${varname}"
 my_params%seacyc = $sesync
 my_params%cycsmooth = $wma
 my_params%nanalog = $nanalog
 my_params%seasonwin = $seasonwin
 my_params%distfun = "${distancefun}"
 my_params%calccor = .${calccor}.
EOF

if [ $silent != "ilent" ]; then 
cat <<EOF >> config_${date_stamp}.txt
 my_params%silent = .FALSE.
/
EOF
else
cat <<EOF >> config_${date_stamp}.txt
 my_params%silent = .TRUE.
/
EOF
fi

module load netcdf/4p

#  start fortran program for analogue calculation
./analogue.out config_${date_stamp}.txt

if [[ !$silent ]]; then
# write finish date and time of script
end_date=`date +"%d/%m/%Y (%H:%M:%S)"`
echo -e "\nScript finished at: ${end_date}\n"
fi
