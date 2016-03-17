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

# Author: Sabine Radanovics, parts cut from getNCEP_slp.sh by Pascal Yiou
# Date: 04/2015
# Purpose: retrieve NCEP data if they are not already in the specified directory



# get passed variables
type=$1
region=$2
year0=$3
year1=$4
yearnow=$5
# change to data directory
dir=$6
cd ${dir}
varname=$7
level=$8
silent=$9


# define the regions
case $region in
 NA) lon1_0=280.0
     lon2_0=50.0
     lat1_0=22.5
     lat2_0=70.0 
     lon1_1=-80.0
     lon2_1=50.0;;
 NHmid) lon1_0=0.0
     lon2_0=360.0
     lat1_0=20.0
     lat2_0=70.0 
     lon1_1=-180.0
     lon2_1=180.0;;
 *) echo -e "Unknown region $region selected. \n Please select one of NA, NHmid"
    echo -e " or define new region in $0.";
    exit 1 ;;
esac

for (( year=${year0}; year <= ${year1}; year++ )) ;
do
# test for every year if the file is already present and retrieve if not, retrieve current year anyway
# echo $year $yearnow
 case $level in
  surface)
   levelstr="" ;;
  1000|925|850|700|600|500|400|300|250|200|150|100|70|50|30|20|10)
   levelstr=${level}hPa_ ;;
  esac
 file=${dir}${varname}_${levelstr}${region}_${year}.nc 
# echo $file $year $yearnow

   if [ -f ${file} ] && [ ${year} -lt ${yearnow} ] 
    then
     if [ ${silent} != "ilent" ]; then echo "${file} found" ; fi
    else
     case $level in 
       surface)
        fname=http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis.dailyavgs/surface/${varname}.${year}.nc ;
        if [ ${silent} != "ilent" ]; then echo "retrieving $fname " ; fi ;
# Get file over the specified region
# (-O overwrite output file without asking, -d cuts part of the specified dimension)
        ncks -O -d lon,${lon1_0},${lon2_0} -d lat,${lat1_0},${lat2_0} ${fname} toto.nc ;;
       1000|925|850|700|600|500|400|300|250|200|150|100|70|50|30|20|10)
        fname=http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis.dailyavgs/pressure/${varname}.${year}.nc ;
        ncks -O -d lon,${lon1_0},${lon2_0} -d lat,${lat1_0},${lat2_0} -d level,${level}.,${level}. -v ${varname} ${fname} toto_lev.nc  ;
        ncwa -O -a level toto_lev.nc toto.nc ;;
       *)
        echo "unknown level ${level}" ;;
     esac
   
    cdo -s splitgrid toto.nc toto
# Center longitudes at 0 meridian 
    cdo -s sellonlatbox,${lon1_1},${lon2_1},${lat1_0},${lat2_0} toto01.nc2 ${file}
   fi
done

# if toto.nc exists remove it
[ -f toto.nc ] && rm toto*

# Concatenation
# calculate the number of years
ny=$((year1-year0+1))
#echo $ny
# ncrcat -n option: first number is the number of files, 
#second number the number of digits before the .suffix to increment, and the third number is the increment.
# then the first inputfile is given.
ncrcat -O -n $((ny)),4,1 ${varname}_${levelstr}${region}_${year0}.nc ${type}_${varname}_${region}_${year0}-${year1}.nc
