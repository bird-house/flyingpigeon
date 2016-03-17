#!/bin/sh
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
# Retrieves NCEP (SLP) reanalysis data with ncks and cdo
# Concatenates the yearly files into a single file from 1948 to the current year
# Pascal Yiou, LSCE, March 2014, April 2014
# Version 1.0
#
# update SR: 04/2015
# * tests now if files are present (does not assume present files any more)
# * actual retrieve is done in retrieve.sh
# * separate construction of data base file and simulation file
# * actual spatial domain and time periods are selected using cdo


# define domain to select
pdomain=$1
# define region to retrieve
region=$2
basedir=$3
pbase=$4
simdir=$5
psim=$6
sourcedir=$7
# get variables passed to the script
varname=$8
level=$9
detrend=${10}
yearnow=`date +"%Y"`

#echo $year0 $yearnow
seacycnorm=${11}
date_stamp=${12}
#echo ${10}
lwin=${13}
seasonwin=${14}
distancefun=${15}
outdir=${16}
silent=${17}

###########
# todo: should add a check if the selected domain is inside the region to retrieve...
##########

# get start and end year of data base
year0=`echo $pbase | awk '{split($pbase,a,",") ; printf "%.4s", a[1] }'` 
year1=`echo $pbase | awk '{split($pbase,a,",") ; printf "%.4s", a[2] }'` 
# get data base
if [ ${silent} != "ilent" ]; then echo Retrieving $varname ${year0}-${year1}  $region region ; fi

./retrieve.sh base $region $year0 $year1 $yearnow $basedir $varname $level $silent
# calculate and remove trend if set
if [ ${detrend} = TRUE ]
 then
  cdo -s trend ${basedir}base_${varname}_${region}_${year0}-${year1}.nc base_afile.nc base_bfile.nc
  cdo -s subtrend ${basedir}base_${varname}_${region}_${year0}-${year1}.nc base_afile.nc base_bfile.nc ${basedir}base_${varname}_dtr_${region}_${year0}-${year1}.nc
  dtrstr='dtr_'
  year0b=$year0
 else
  dtrstr=''
fi
 
namestring=`echo $pdomain | awk '{ gsub(/,/, "_"); print }'`
if [ ${silent} != "ilent" ]; then echo $namestring ; fi
datestring1=`echo $pbase | awk '{ gsub(/,/, "_"); print }'`
# domain and date selection
if [ ${silent} != "ilent" ]; then echo ${pbase} ; fi
cdo -s sellonlatbox,${pdomain} -seldate,${pbase} ${basedir}base_${varname}_${dtrstr}${region}_${year0}-${year1}.nc ${basedir}base_${varname}_${dtrstr}${region}_${datestring1}_${namestring}.nc
cdo -s showdate ${basedir}base_${varname}_${dtrstr}${region}_${datestring1}_${namestring}.nc | awk '{gsub(/-/,""); print}' > base_dates.txt

# change back to source directory
cd $sourcedir
# write data base/archive  filename to configuration file
cat <<EOF > config_${date_stamp}.txt
&FILES
 my_files%archivefile = "${basedir}base_${varname}_${dtrstr}${region}_${datestring1}_${namestring}.nc"
EOF
# get start and end year of simulation
year0=`echo $psim | awk '{split($psim,a,",") ; printf "%.4s", a[1] }'` 
year1=`echo $psim | awk '{split($psim,a,",") ; printf "%.4s", a[2] }'` 
# get data simulation
if [ ${silent} != "ilent" ]; then echo Retrieving $varname ${year0}-${year1}  $region region ; fi
./retrieve.sh sim $region $year0 $year1 $yearnow $simdir $varname $level $silent

#remove trend
if [ ${detrend} = TRUE ]
 then
 if [ ${year0b} != ${year0} ]
  then
   cdo -s subtrend ${simdir}sim_${varname}_${region}_${year0}-${year1}.nc base_afile.nc base_bfile.nc ${simdir}sim_${varname}_dtr1_${region}_${year0}-${year1}.nc
# subtract b times the difference between the two start dates of the files. 
# Since the exact time period is not yet selected, whole years can be assumed.
   date1=$(date -d "${year0b}-01-01" +%s)
   date2=$(date -d "${year0}-01-01" +%s)
# calculate difference in days
   nodays=$(( ($date2 - $date1) / 86400 ))
# multiply the field with the b (slope) parameter with the difference in number of days
   cdo -s mulc,$nodays base_bfile.nc base_bfile_sim.nc
# subtract it in addition to the already subtracted part of the trend
   cdo -s sub ${simdir}sim_${varname}_dtr1_${region}_${year0}-${year1}.nc base_bfile_sim.nc ${simdir}sim_${varname}_dtr_${region}_${year0}-${year1}.nc
 else
# no need for extra subtraction in the case of equal first dates
  cdo -s subtrend ${simdir}sim_${varname}_${region}_${year0}-${year1}.nc base_afile.nc base_bfile.nc ${simdir}sim_${varname}_dtr_${region}_${year0}-${year1}.nc
 fi
fi
# domain and dates selection
datestring2=`echo $psim | awk '{ gsub(/,/, "_"); print }'`
cdo -s sellonlatbox,${pdomain} -seldate,${psim} ${simdir}sim_${varname}_${dtrstr}${region}_${year0}-${year1}.nc  ${simdir}sim_${varname}_${dtrstr}${region}_${datestring2}_${namestring}.nc
cdo -s showdate ${simdir}sim_${varname}_${dtrstr}${region}_${datestring2}_${namestring}.nc | awk '{gsub(/-/,""); print}' > sim_dates.txt

# change back to source directory
cd $sourcedir
# write simulation filename to configuration file
cat <<EOF >> config_${date_stamp}.txt
 my_files%simulationfile = "${simdir}sim_${varname}_${dtrstr}${region}_${datestring2}_${namestring}.nc"
 my_files%outputfile ="${outdir}ana_${varname}_${level}_${dtrstr}${distancefun}_${region}_sim_${datestring2}_base_${datestring1}_${namestring}_${lwin}_${seasonwin}.txt"
/
EOF
date=`date +"%d/%m/%Y (%H:%M:%S)"`
if [ ${silent} != "ilent" ]; then echo -e "\n files read: ${date}\n" ; fi

# check if the variable name in the file is the same as the variable name used in the filename 
# and if not change the variable name in the input files
myvarname=`cdo -s showname ${basedir}base_${varname}_${dtrstr}${region}_${datestring1}_${namestring}.nc`
if [ ${myvarname} = ${varname} ]
then
 if [ ${silent} != "ilent" ]; then echo 'varnames fine' ; fi
else
 ncrename -v${myvarname},${varname} ${basedir}base_${varname}_${dtrstr}${region}_${datestring1}_${namestring}.nc
 ncrename -v${myvarname},${varname} ${simdir}sim_${varname}_${dtrstr}${region}_${datestring2}_${namestring}.nc
fi

# calculate seasonal cycle
case $seacycnorm in
 base) cdo -s ydaymean ${basedir}base_${varname}_${dtrstr}${region}_${datestring1}_${namestring}.nc seasoncyc_base.nc
       cp seasoncyc_base.nc seasoncyc_sim.nc ;;
 sim)  cdo -s ydaymean ${simdir}sim_${varname}_${dtrstr}${region}_${datestring2}_${namestring}.nc seasoncyc_sim.nc
       cp seasoncyc_sim.nc seasoncyc_base.nc ;;
 own)  cdo -s ydaymean ${basedir}base_${varname}_${dtrstr}${region}_${datestring1}_${namestring}.nc seasoncyc_base.nc
       cdo -s ydaymean ${simdir}sim_${varname}_${dtrstr}${region}_${datestring2}_${namestring}.nc seasoncyc_sim.nc ;;
 none) echo "analogues calculated from raw data, no anomalies" ;;
 *)    echo -e "Invalid value for anomalie option -N, \nplease choose one of base, sim, own or none" 
       exit 1 ;;
esac


date=`date +"%d/%m/%Y (%H:%M:%S)"`
if [ ${silent} != "ilent" ]; then echo -e "\n seasonal cycle calculated: ${date}\n" ; fi
