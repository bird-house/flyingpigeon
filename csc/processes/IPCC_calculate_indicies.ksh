#!/bin/ksh
#--------------------------------------
# SGE directives
#$ -N indices
#$ -S /bin/ksh
#$ -cwd
#$ -q standard
#$ -t 7
#--------------------------------------
# user command section
date
hostname
echo $SGE_TASK_ID
echo ********START**********
##! /bin/ksh
#
# 
# =================================================================================================================================================================
# SKRIPT, UM DATEN FUER IPCC-EUROCHAPTER AUFZUBEREITEN 
#
# VERSION 3.0 (Februar 2013) - VERFASSER: Arne Kriegsman, Andreas Haensler
#
# .................................................................
# Es wird 1 externes Skript aufgerufen!
# =================================================================================================================================================================
#
#
set -ex
#
#
PIPCC=/scratch/mpi/ch0636/a_13_i/se_14/indices #Output
PFAD=/scratch/mpi/ch0636/a_13_i/se_14/data/day  #Input
#
#
#set true which indices shall be calculated
tmean=false
frostdays=false
summerdays=false
tnights=false
growseas=false
warmspell=false   #Warm- und coldspell duerfen nicht gleichzeitig laufen!!!
coldspell=true    #Warm- und coldspell duerfen nicht gleichzeitig laufen!!!
ptot=false
p99tot=false
ppcsth=false
HeatWave_WMO=false
HeatWave_pcs=false
dryspell=false
tmeanseas=false
ptotseas=false
growperiod=false
ffp=false
growperiodsumtmean=false
icedays=false
spii=false
r20mm=false

#
#
# ============================================================================================================================================
#
# IPCC - 0: Mittlere jaehrliche Temperatur
#
if ${tmean} && [ $SGE_TASK_ID -eq 1 ] ; then

PER="1971-00 2021-50 2071-00"
VAR=tas

cd ${PFAD}/${VAR}

mkdir -p ${PIPCC}/tmean/data
POUT="${PIPCC}/tmean/data"

for P in ${PER} ; do

 if [ $P == '1971-00' ] ; then
  SP="1971/2000"
  FILES=`ls ${VAR}*historical*.nc`
 elif [ $P == '2021-50' ] ; then
  SP="2021/2050"
  FILES=`ls ${VAR}*rcp*.nc`
 elif [ $P == '2071-00' ] ; then
  SP="2071/2100"
  FILES=`ls ${VAR}*rcp*.nc`
 else 
  echo "FEHLER"
  exit
 fi

 for F in $FILES ; do
  PNAM=${P}_YEAR_tmean_`echo $F | cut -d "_" -f2-`
  set +ex
   cdo timmean -selyear,${SP} $F ${POUT}/${PNAM}
  set -ex
 done
done  
fi  
#
#
# ============================================================================================================================================
#
# IPCC - 1: Anzahl von Frosttagen basierend auf tasmin
#
#
if ${frostdays} &&  [ $SGE_TASK_ID -eq 2 ] ; then

PER="1971-00 2021-50 2071-00"
VAR=tasmin

cd ${PFAD}/${VAR}

mkdir -p ${PIPCC}/fdays/data
POUT="${PIPCC}/fdays/data"

for P in ${PER} ; do

 if [ $P == '1971-00' ] ; then
  SP="1971/2000"
  FILES=`ls ${VAR}*historical*.nc`
 elif [ $P == '2021-50' ] ; then
  SP="2021/2050"
  FILES=`ls ${VAR}*rcp*.nc`
 elif [ $P == '2071-00' ] ; then
  SP="2071/2100"
  FILES=`ls ${VAR}*rcp*.nc`
 else 
  echo "FEHLER"
  exit
 fi

 for F in $FILES ; do
  PNAM=${P}_YEAR_fdays_`echo $F | cut -d "_" -f2-`
  set +ex
   cdo selyear,${SP} ${F} ${F}_${SGE_TASK_ID}
   cdo timmean -yearsum -ltc,273.15 ${F}_${SGE_TASK_ID} ${POUT}/${PNAM}
   rm ${F}_${SGE_TASK_ID}
  set -ex
 done 
done  
fi  
#
#
# ============================================================================================================================================
#
# IPCC - 2: Anzahl von Sommertagen basierend auf tasmax -> Vorsicht, nur 9 Modelle verfuegbar:
#
#
if ${summerdays} && [ $SGE_TASK_ID -eq 3 ] ; then
#  
PER="1971-00 2021-50 2071-00"
SEAS="YEAR"
VAR=tasmax
#
cd ${PFAD}/${VAR}

mkdir -p ${PIPCC}/sdays/data
POUT="${PIPCC}/sdays/data"

for P in ${PER} ; do
 
 if [ $P == '1971-00' ] ; then
  SP="1971/2000"
  FILES=`ls ${VAR}*historical*.nc`
 elif [ $P == '2021-50' ] ; then
  SP="2021/2050"
  FILES=`ls ${VAR}*rcp*.nc`
 elif [ $P == '2071-00' ] ; then
  SP="2071/2100"
  FILES=`ls ${VAR}*rcp*.nc`
 else 
  echo "FEHLER"
  exit
 fi

 for S in ${SEAS} ; do  
  for F in $FILES ; do
   PNAM=${P}_${S}_sdays_`echo $F | cut -d "_" -f2-`
   set +ex
    cdo selyear,${SP} $F ${F}_${SGE_TASK_ID}
    cdo timmean -yearsum -gtc,298.15  ${F}_${SGE_TASK_ID} ${POUT}/${PNAM}
    rm ${F}_${SGE_TASK_ID}
   set -ex
  done
 done 
done  
fi
#
#
# ============================================================================================================================================
#
# IPCC - 3: Anzahl von Tropennaechten basierend auf tasmin -> Vorsicht, nur 9 Modelle verfuegbar:
#
#
if ${tnights} && [ $SGE_TASK_ID -eq 4 ] ; then
#  
PER="1971-00 2021-50 2071-00"
SEAS="YEAR"
VAR=tasmin
#
cd ${PFAD}/${VAR}

mkdir -p ${PIPCC}/tnights/data
POUT="${PIPCC}/tnights/data"

for P in ${PER} ; do

 if [ $P == '1971-00' ] ; then
  SP="1971/2000"
  FILES=`ls ${VAR}*historical*.nc`
 elif [ $P == '2021-50' ] ; then
  SP="2021/2050"
  FILES=`ls ${VAR}*rcp*.nc`
 elif [ $P == '2071-00' ] ; then
  SP="2071/2100"
  FILES=`ls ${VAR}*rcp*.nc`
 else 
  echo "FEHLER"
  exit
 fi
 
 for S in ${SEAS} ; do 
  for F in $FILES ; do
   PNAM=${P}_${S}_tnights_`echo $F | cut -d "_" -f2-`
   set +ex
    cdo selyear,${SP} $F ${F}_${SGE_TASK_ID}
    cdo timmean -yearsum -gtc,293.15 ${F}_${SGE_TASK_ID} ${POUT}/${PNAM}
    rm ${F}_${SGE_TASK_ID}
   set -ex
  done
 done 
done  
fi
#
#
# ============================================================================================================================================
#
# IPCC - 4: Laenge der Vegetationsperiode:
#
#
if ${growseas} && [ $SGE_TASK_ID -eq 5 ] ; then
#  
PER="1971-00 2021-50 2071-00"
SEAS="YEAR"
VAR=tas

cd ${PFAD}/${VAR}

mkdir -p ${PIPCC}/growseas/data
POUT="${PIPCC}/growseas/data"

for P in ${PER} ; do

 if [ $P == '1971-00' ] ; then
  SP="1971/2000"
  FILES=`ls ${VAR}*historical*ALADIN*.nc`
 elif [ $P == '2021-50' ] ; then
  SP="2021/2050"
  FILES=`ls ${VAR}*rcp*ALADIN*.nc`
 elif [ $P == '2071-00' ] ; then
  SP="2071/2100"
  FILES=`ls ${VAR}*rcp*ALADIN*.nc`
 else 
  echo "FEHLER"
  exit
 fi

 for S in ${SEAS} ; do 

  for F in $FILES ; do
   PNAM=${P}_${S}_growseas_`echo $F | cut -d "_" -f2-`
   FNP1=`echo $F | cut -d "_" -f3`
   FNP2=`echo $F | cut -d "_" -f6`

    
    if [ $FNP1 == "CNRM-CM5" ] && [ $FNP2 == "CNRM-ALADIN52" ] ; then
     cp ${PIPCC}/../../inout/sftlf_MED-11_ECMWF-ERAINT_evaluation_r1i1p1_CNRM-ALADIN52_v1_fx.nc MASK_$SGE_TASK_ID
    elif [ $FNP1 == "CNRM-CM5" ] && [ $FNP2 == "CCLM4-8-17"  ] ; then
     cp ${PIPCC}/../../inout/sftlf_EUR-11_MPI-ESM-LR_historical_r0i0p0_CCLM4-8-17_v1_fx.nc MASK_$SGE_TASK_ID 
    elif [ $FNP1 == "EC-EARTH" ] || [ $FNP1 == "MPI-ESM-LR" ] ; then
     cp ${PIPCC}/../../inout/sftlf_EUR-11_MPI-ESM-LR_historical_r0i0p0_CCLM4-8-17_v1_fx.nc MASK_$SGE_TASK_ID
    elif [ $FNP1 == "HadGEM2" ] ; then
     cp ${PIPCC}/../../inout/lffd2005010100c_FR_LAND.nc MASK_$SGE_TASK_ID
    elif [ $FNP1 == "IPSL-CM5A-MR" ] ; then
     cp ${PIPCC}/../../inout/sftlf_EUR-11_ECMWF-ERAINT_evaluation_r0i0p0_IPSL-INERIS-WRF1_v331_fx.nc MASK_$SGE_TASK_ID
    elif [ $FNP1 == "ECHAM-MPIOM" ] ; then
     cp ${PIPCC}/../../inout/remo_019_535_historical_r1i1p1_n_c172_day.nc MASK_$SGE_TASK_ID
    elif [ $FNP1 == "MetEir-ECEARTH" ] ; then
     cp ${PIPCC}/../../inout/sftlf_EUR-11_MetEir-ECEARTH_historical_r1i1p1_KNMI-RACMO22E_v1_fx.nc MASK_$SGE_TASK_ID
    elif [ $FNP1 == "DMI-ECEARTH" ] ; then
     cp ${PIPCC}/../../inout/sftlf_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_DMI-HIRHAM5_v1_fx.nc MASK_$SGE_TASK_ID
    elif [ $FNP1 == "CNRM-CERFACS-CNRM-CM5" ] || [ $FNP1 == "ICHEC-EC-EARTH" ] || [ $FNP1 == "MOHC-HadGEM2-ES" ]  || [ $FNP1 == "IPSL-IPSL-CM5A-MR" ] || [ $FNP1 == "MPI-M-MPI-ESM-LR" ]; then
     cp ${PIPCC}/../../inout/sftlf_EUR-11_ECMWF-ERAINT_evaluation_r0i0p0_SMHI-RCA4_v1_fx.nc MASK_$SGE_TASK_ID
    else 
     echo ERROR: Mask not known
    fi

    set +ex 
     cdo selyear,${SP} $F FIN_$SGE_TASK_ID
     cdo seltimestep,1 FIN_$SGE_TASK_ID TMASK_$SGE_TASK_ID
     cdo timmean -selcode,-1 -eca_gsl FIN_$SGE_TASK_ID TMASK_$SGE_TASK_ID TEMP_$SGE_TASK_ID
     cdo gec,0.5 MASK_$SGE_TASK_ID MMASK_$SGE_TASK_ID
     cdo ifthen MMASK_$SGE_TASK_ID TEMP_$SGE_TASK_ID ${POUT}/${PNAM}
     rm MASK_$SGE_TASK_ID FIN_$SGE_TASK_ID TMASK_$SGE_TASK_ID TEMP_$SGE_TASK_ID MMASK_$SGE_TASK_ID
    set -ex
  done
 done 
done  
fi
#
#
# ============================================================================================================================================
#
# IPCC - 5: Warm-spell basierend auf tasmax -> Vorsicht, nur 9 Modelle verfuegbar::
#
#


if ${warmspell} && [ $SGE_TASK_ID -eq 6 ] ; then
#  
PER="1971-00 2021-50 2071-00"
VAR=tasmax
#
VRANGE="210,320"
#
cd ${PFAD}/${VAR}
#   
#   
set +ex
 mkdir -p ${PIPCC}/warmspell/data
 POUT="${PIPCC}/warmspell/data"
set -ex
#
FILES=`ls *MPI-ESM-LR_historical*.nc *MPI-M-MPI-ESM-LR_historical*.nc *MED-11_CNRM-CM5_historical*.nc`
#FILES=`ls *historical*.nc`
for F in $FILES ; do
 cdo setcode,1 -ydrunpctl,90,5 $F -ydrunmin,5 $F -ydrunmax,5 $F 90PCTL_$SGE_TASK_ID
#
 P1=`echo $F | cut -d "_" -f1-`
 P2=${P1/_historical_/_rcp*_} 

 #spezialfall WRFII1 <= WRF1
 P2_DUMMY=${P2/-WRF1_/-WRFII1_}
 P2=${P2_DUMMY}

 F1=`ls ${P2}`

 for P in ${PER} ; do
#  
  if [ $P == '1971-00' ] ; then
   SP="1971/2000"
   FILES1=$F 
  elif [ $P == '2021-50' ] ; then
   SP="2021/2050"
   FILES1=$F1
  elif [ $P == '2071-00' ] ; then
   SP="2071/2100"
   FILES1=$F1
  else 
   echo "FEHLER"
   exit
  fi
#

  for FI in ${FILES1} ; do
   FILE=${FI}
   PNAM=${P}_YEAR_warmspell_`echo $FILE | cut -d "_" -f2-`
# 
#
   set +ex
    cdo -s setcode,1 -setvrange,${VRANGE} -selyear,${SP} $FILE FIN_$SGE_TASK_ID
    NYEAR=`cdo nyear FIN_$SGE_TASK_ID`
#
#
# Aufruf FORTRANSKRIPT UM ZEITREIHE AUF GLEICHE GROESSE AUFZUFUELLEN -> OUTPUT-DATEI IST BASE !!
#
    ${PIPCC}/Enlarge_Baseline_Tseries.ksh 90PCTL_$SGE_TASK_ID FIN_$SGE_TASK_ID
#
# Warm Spell Duration pro Jahr:  
#
    mv BASE BASE_$SGE_TASK_ID
    cdo divc,${NYEAR} -selcode,-1 -eca_hwfi FIN_$SGE_TASK_ID BASE_$SGE_TASK_ID ${POUT}/${PNAM}
    rm FIN_$SGE_TASK_ID BASE_$SGE_TASK_ID
   set -ex
#
  done
 done 
 rm 90PCTL_$SGE_TASK_ID
done  
fi
#
#
# ============================================================================================================================================
#
# IPCC - 6: Cold-spell basierend auf tasmin -> Vorsicht, nur 9 Modelle verfuegbar::
#
#
# EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_SMHI-RCA4_v1.nc
# EUR-11_CNRM-CM5_historical_r1i1p1_CCLM4-8-17_v1.nc
# EUR-11_DMI-ECEARTH_historical_r3i1p1_DMI-HIRHAM5_v1.nc
# EUR-11_EC-EARTH_historical_r12i1p1_CCLM4-8-17_v1.nc
# EUR-11_ECHAM-MPIOM_historical_r1i1p1_remo.nc
# EUR-11_HadGEM2_historical_runid_RCM.nc
# EUR-11_ICHEC-EC-EARTH_historical_r12i1p1_SMHI-RCA4_v1.nc
# EUR-11_IPSL-CM5A-MR_historical_r1i1p1_IPSL-INERIS-WRF1_v331.nc
# EUR-11_IPSL-IPSL-CM5A-MR_historical_r1i1p1_SMHI-RCA4_v1.nc
# EUR-11_MetEir-ECEARTH_historical_r1i1p1_KNMI-RACMO22E_v1.nc
# EUR-11_MOHC-HadGEM2-ES_historical_r1i1p1_SMHI-RCA4_v1.nc
# EUR-11_MPI-ESM-LR_historical_r1i1p1_CCLM4-8-17_v1.nc
# EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_SMHI-RCA4_v1.nc
# MED-11_CNRM-CM5_historical_r8i1p1_CNRM-ALADIN52_v1.nc

if ${coldspell} && [ $SGE_TASK_ID -eq 7 ] ; then
#  
PER="1971-00 2021-50 2071-00"
VAR=tasmin
#
VRANGE="210,320"
#
cd ${PFAD}/${VAR}
#   
#   
set +ex
 mkdir -p ${PIPCC}/coldspell/data
 POUT="${PIPCC}/coldspell/data"
set -ex
#
FILES=`ls *EUR-11_IPSL-IPSL-CM5A-MR_historical_r1i1p1_SMHI-RCA4_v1.nc *EUR-11_MetEir-ECEARTH_historical_r1i1p1_KNMI-RACMO22E_v1.nc *EUR-11_MOHC-HadGEM2-ES_historical_r1i1p1_SMHI-RCA4_v1.nc *EUR-11_MPI-ESM-LR_historical_r1i1p1_CCLM4-8-17_v1.nc *EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_SMHI-RCA4_v1.nc *MED-11_CNRM-CM5_historical_r8i1p1_CNRM-ALADIN52_v1.nc`
#FILES=`ls *historical*.nc`

for F in $FILES ; do

 cdo setcode,1 -ydrunpctl,10,5 $F -ydrunmin,5 $F -ydrunmax,5 $F 10PCTL_$SGE_TASK_ID
#
 P1=`echo $F | cut -d "_" -f1-`
 P2=${P1/_historical_/_rcp*_} 
 
 #spezialfall WRFII1 <= WRF1
 P2_DUMMY=${P2/-WRF1_/-WRFII1_}
 P2=${P2_DUMMY}

 F1=`ls ${P2}`

 for P in ${PER} ; do
#  
  if [ $P == '1971-00' ] ; then
   SP="1971/2000"
   FILES1=$F 
  elif [ $P == '2021-50' ] ; then
   SP="2021/2050"
   FILES1=$F1
  elif [ $P == '2071-00' ] ; then
   SP="2071/2100"
   FILES1=$F1
  else 
   echo "FEHLER"
   exit
  fi
# 

  for FI in ${FILES1} ; do
   FILE=${FI}
   PNAM=${P}_YEAR_coldspell_`echo $FILE | cut -d "_" -f2-`
#
#
   set +ex
   cdo -s setcode,1 -setvrange,${VRANGE} -selyear,${SP} $FILE FIN_$SGE_TASK_ID
   NYEAR=`cdo nyear FIN_$SGE_TASK_ID`  
#
#
# Aufruf FORTRANSKRIPT UM ZEITREIHE AUF GLEICHE GROESSE AUFZUFUELLEN -> OUTPUT-DATEI IST BASE !!
#
   ${PIPCC}/Enlarge_Baseline_Tseries.ksh 10PCTL_$SGE_TASK_ID FIN_$SGE_TASK_ID
#
# Cold Spell Duration pro Jahr:  
#
   mv BASE BASE_$SGE_TASK_ID
   cdo divc,${NYEAR} -selcode,-1 -eca_cwfi FIN_$SGE_TASK_ID BASE_$SGE_TASK_ID ${POUT}/${PNAM}
   rm FIN_$SGE_TASK_ID BASE_$SGE_TASK_ID
   set -ex
#
  done
 done 
 rm 10PCTL_$SGE_TASK_ID
done  
fi
#
# #
# #  ============================================================================================================================================
#
# IPCC - 7: Mittelerer Jahresniederschlag (nur Wetdays => > 1mm pro Tag):
#
if ${ptot} && [ ${SGE_TASK_ID} -eq 8 ] ; then
#
PER="1971-00 2021-50 2071-00"
SEAS="YEAR"
VAR=pr
#
VRANGE="1,10000"
#
cd ${PFAD}/${VAR}
#
set +ex
 mkdir -p ${PIPCC}/ptot/data
 POUT="${PIPCC}/ptot/data"
set -ex

for P in ${PER} ; do
 
 if [ $P == '1971-00' ] ; then
   FILES=`ls *historical*.nc`
   SP="1971/2000"
 elif [ $P == '2021-50' ] ; then
   FILES=`ls *rcp*.nc`
   SP="2021/2050"
 elif [ $P == '2071-00' ] ; then
   FILES=`ls *rcp*.nc`
   SP="2071/2100"
 else 
   echo "FEHLER"
   exit
 fi

#
 for S in ${SEAS} ; do 
  for F in $FILES ; do
   PNAM=${P}_${S}_ptot_`echo $F | cut -d "_" -f2-`
   set +ex
   cdo timmean -yearsum -selyear,${SP} -setvrange,${VRANGE} $F ${POUT}/${PNAM} 
   set -ex
  done
 done 
done  
fi  
#
#
# ============================================================================================================================================
#
# IPCC - 8: Jahresniederschlagssumme von Wetdays, die mehr Niederschlag als das 99Percentil der Basisperiode haben
#
if ${p99tot} && [ $SGE_TASK_ID -eq 9 ] ; then
#  
PER="1971-00 2021-50 2071-00"
VAR=pr
#
VRANGE="1,10000"
#
cd ${PFAD}/${VAR}
#   
set +ex
 mkdir -p ${PIPCC}/p99tot/data
 POUT="${PIPCC}/p99tot/data"
set -ex
#
FILES=`ls *historical*.nc`

for F in $FILES ; do
 cdo setvrange,${VRANGE} -selyear,1971/2000 $F FIN_$SGE_TASK_ID
 cdo timpctl,99 FIN_$SGE_TASK_ID -timmin FIN_$SGE_TASK_ID -timmax FIN_$SGE_TASK_ID 99PCTL_$SGE_TASK_ID
 rm FIN_$SGE_TASK_ID
#
 P1=`echo $F | cut -d "_" -f1-`
 P2=${P1/_historical_/_rcp*_}

 #spezialfall WRFII1 <= WRF1
 P2_DUMMY=${P2/-WRF1_/-WRFII1_}
 P2=${P2_DUMMY}
# set +ex
 F1=`ls ${P2}`
 #F2=`ls 2071-00_*${V}*${P2}`
# set -ex

 for P in ${PER} ; do
#  
  if [ $P == '1971-00' ] ; then
   SP="1971/2000"
   FILES1=$F 
  elif [ $P == '2021-50' ] ; then
   SP="2021/2050"
   FILES1=$F1
  elif [ $P == '2071-00' ] ; then
   SP="2071/2100"
   FILES1=$F1
  else 
   echo "FEHLER"
   exit
  fi
# 

for FI in ${FILES1} ; do
  FILE=${FI}
  PNAM=${P}_YEAR_p99tot_`echo $FILE | cut -d "_" -f2-`
#
  set +ex
  cdo -s setvrange,${VRANGE} -selyear,${SP} $FILE FIN_$SGE_TASK_ID
  cdo timmean -yearsum -setvrange,0,10000 -sub FIN_$SGE_TASK_ID 99PCTL_$SGE_TASK_ID ${POUT}/${PNAM} 
  rm FIN_$SGE_TASK_ID
  set -ex
 done
 done
 rm 99PCTL_$SGE_TASK_ID
done  
fi  
#
#
# ============================================================================================================================================
#
# IPCC - 9: Percentile taeglicher Niederschlaege (nur Tage mit Niederschlag groesser -1mm und kliener 10000mm (Datenfehlerabfrage) werden beruecksichtigt)!!! Keine WETDAY_ABFRAGE
#
if ${ppcsth} && [ $SGE_TASK_ID -eq 10 ] ; then
#
PCS="95" #95 98
PER="2071-00"
SEAS="JJA" # YEAR DJF JJA MAM SON 
#
VRANGE="1,10000"
#
cd ${PFAD}/pr
#   
for PC in ${PCS}  ; do
#
set +ex
 mkdir -p ${PIPCC}/p${PC}th/data
 POUT="${PIPCC}/p${PC}th/data"
set -ex
for P in ${PER} ; do

  if [ $P == '1971-00' ] ; then
   SP="1971/2000"
   FILES=`ls *historical*.nc`
  elif [ $P == '2021-50' ] ; then
   SP="2021/2050"
   FILES=`ls *rcp*.nc`
  elif [ $P == '2071-00' ] ; then
   SP="2071/2100"
   FILES=`ls *IPSL-IPSL-CM5A-MR_rcp85*.nc`
  else 
   echo "FEHLER"
   exit
  fi
  
 

  for S in ${SEAS} ; do 
#  
   if [ $S == 'YEAR' ] ; then
    MONTHS="01,02,03,04,05,06,07,08,09,10,11,12"
   elif [ $S == 'DJF' ] ; then
    MONTHS="01,02,12"
   elif [ $S == 'MAM' ] ; then
    MONTHS="03,04,05"
   elif [ $S == 'JJA' ] ; then
    MONTHS="06,07,08"
   elif [ $S == 'SON' ] ; then
    MONTHS="09,10,11"
   fi
#
   for F in $FILES ; do
    PNAM="${P}_${S}_p${PC}th"_`echo $F | cut -d "_" -f2-`
    set +ex
     cdo selyear,${SP} -selmon,${MONTHS} -setvrange,${VRANGE} $F FIN_$SGE_TASK_ID
     cdo timmin FIN_$SGE_TASK_ID MIN_$SGE_TASK_ID
     cdo timmax FIN_$SGE_TASK_ID MAX_$SGE_TASK_ID
     cdo timpctl,${PC} FIN_$SGE_TASK_ID MIN_$SGE_TASK_ID MAX_$SGE_TASK_ID ${POUT}/${PNAM}
     rm MIN_$SGE_TASK_ID MAX_$SGE_TASK_ID FIN_$SGE_TASK_ID
    set -ex
   done
  done 
 done
done  
fi  
#
#
# ============================================================================================================================================
#
#
# IPCC - 10: Heatwave in den Monaten Mai bis September: mittlere Anzahl (NHeatWave) und mittlere Laenge (DHeatWave) -> Def nach WMO, aber fuer tas und tasmax
#
#
if ${HeatWave_WMO} && [ $SGE_TASK_ID -eq 11 ] ; then
 echo " "
  echo "Berechnung der Aenderung des mittleren Auftretens von Hitzewellen (NHeatWave) und der mittleren Laenge (DHeatWave) nach WMO -> $V"
#  
VARS="tasmax" #tas
PER="1971-00 2021-50 2071-00"
MONTHS="05,06,07,08,09"
NMONTHS="01,02,03,04,10,11,12"
#
VRANGE="200,350"
#
for V in ${VARS} ; do
 cd ${PFAD}/$V
#   
 set +ex
 mkdir -p ${PIPCC}/HeatWave_${V}/data
 POUT="${PIPCC}/HeatWave_${V}/data"
 set -ex
# 
 FILES=`ls *MetEir-ECEARTH_historical*.nc *MOHC-HadGEM2-ES_historical*.nc *MPI-ESM-LR_historical*.nc *MPI-M-MPI-ESM-LR_historical*.nc *MED-11_CNRM-CM5_historical*.nc`
 for F in $FILES ; do
  cdo addc,5 -timmean -yearmax -setvrange,${VRANGE} -selmon,${MONTHS} -selyear,1971/2000 $F BASE_$SGE_TASK_ID
 #
  P1=`echo $F | cut -d "_" -f1-`
  P2=${P1/_historical_/_rcp*_} 

  #spezialfall WRFII1 <= WRF1
  P2_DUMMY=${P2/-WRF1_/-WRFII1_}
  P2=${P2_DUMMY}
 
 F1=`ls ${P2}`

  for P in ${PER} ; do
#  
   if [ $P == '1971-00' ] ; then
    SP="1971/2000"
    FILES1=$F 
   elif [ $P == '2021-50' ] ; then
    SP="2021/2050"
    FILES1=$F1
   elif [ $P == '2071-00' ] ; then
    SP="2071/2100"
    FILES1=$F1
   else 
    echo "FEHLER"
    exit
   fi

   for FI in ${FILES1} ; do 

    FILE=${FI}
    PNAM="${P}_MJJAS"
    PNAM2=`echo $FILE | cut -d "_" -f2-`
#
# Haeufigkeit pro Jahr:  
#
    set +ex
     
    cdo -s sub -selyear,${SP} -setvrange,${VRANGE} $FILE BASE_$SGE_TASK_ID temp_$SGE_TASK_ID
#
# nur MONTHS-Perdiode betrachten, alles andere 0-setzten 
#
     cdo selmon,${MONTHS} temp_$SGE_TASK_ID tempa_$SGE_TASK_ID
     cdo subc,10000 -selmon,${NMONTHS} temp_$SGE_TASK_ID tempb_$SGE_TASK_ID
     cdo mergetime tempa_$SGE_TASK_ID tempb_$SGE_TASK_ID tempc_$SGE_TASK_ID
     rm temp_$SGE_TASK_ID tempa_$SGE_TASK_ID tempb_$SGE_TASK_ID
#    
     cdo -s consects -gtc,0 tempc_$SGE_TASK_ID temp1_$SGE_TASK_ID
 #
 #
 # Alles was groesser 5 Tage ist, ist Heatwave:
 #
 # Anzahl der Heatwaves:
 #  
     cdo -s setmisstoc,0 -timsum -gtc,5 temp1_$SGE_TASK_ID ${POUT}/${PNAM}_NHeatWave-WMO_${PNAM2}
 #
 # Mittlere Laenge Heatwave:
 #  
     cdo -s gtc,5 temp1_$SGE_TASK_ID HW_Mask_$SGE_TASK_ID
     cdo ifthen HW_Mask_$SGE_TASK_ID temp1_$SGE_TASK_ID Days_Heatwave_$SGE_TASK_ID
     cdo setmisstoc,0 -timmean -setvrange,4,100000 Days_Heatwave_$SGE_TASK_ID ${POUT}/${PNAM}_DHeatWave-WMO_${PNAM2}
# 
     rm tempc_$SGE_TASK_ID temp1_$SGE_TASK_ID HW_Mask_$SGE_TASK_ID Days_Heatwave_$SGE_TASK_ID
    set -ex
   done #FILES1
  done #PER
  rm BASE_$SGE_TASK_ID
 done
done 
#
fi #
#
#
# ============================================================================================================================================
#
#
# IPCC 11: Heatwave in den Monaten Mai bis September: mittlere Anzahl (NHeatWave) und mittlere Laenge (DHeatWave) -> Fuer 98 und 99 Percentil, aber nur 3 Tage, aber fuer tas und tasmax
#
#
if ${HeatWave_pcs} && [ $SGE_TASK_ID -eq 12 ] ; then
 echo " "
  echo "Berechnung der Aenderung des mittleren Auftretens von Hitzewellen (NHeatWave) und der mittleren Laenge (DHeatWave) nach WMO -> $V"
#  
PCS="98 99"
VARS="tas" #tasmax
PER="1971-00 2021-50 2071-00"
MONTHS="05,06,07,08,09"
NMONTHS="01,02,03,04,10,11,12"
#
VRANGE="200,350"
#
for V in ${VARS} ; do
 cd ${PFAD}/$V
#   
 set +ex
 mkdir -p ${PIPCC}/HeatWave_${V}/data
 POUT="${PIPCC}/HeatWave_${V}/data"
 set -ex
# 
 FILES=`ls ${VAR}*historical*.nc`
 
 for F in $FILES ; do
  for PC in ${PCS} ; do
   cdo setvrange,${VRANGE} -selmon,${MONTHS} -selyear,1971/2000 $F FIN_$SGE_TASK_ID
   cdo timmin FIN_$SGE_TASK_ID MIN_$SGE_TASK_ID
   cdo timmax FIN_$SGE_TASK_ID MAX_$SGE_TASK_ID
   cdo timpctl,${PC} FIN_$SGE_TASK_ID MIN_$SGE_TASK_ID MAX_$SGE_TASK_ID BASE_$SGE_TASK_ID
   rm MIN_$SGE_TASK_ID MAX_$SGE_TASK_ID FIN_$SGE_TASK_ID
 #
   P1=`echo $F | cut -d "_" -f1-`
   P2=${P1/_historical_/_rcp*_} 

   #spezialfall WRFII1 <= WRF1
   P2_DUMMY=${P2/-WRF1_/-WRFII1_}
   P2=${P2_DUMMY}
 

   F1=`ls ${P2}`
 #
   for P in ${PER} ; do
   
   if [ $P == '1971-00' ] ; then
    SP="1971/2000"
    FILES1=$F 
   elif [ $P == '2021-50' ] ; then
    SP="2021/2050"
    FILES1=$F1
   elif [ $P == '2071-00' ] ; then
    SP="2071/2100"
    FILES1=$F1
   else 
    echo "FEHLER"
    exit
   fi
# 
   for FI in ${FILES1} ; do 
   
   
    FILE=${FI}
    PNAM="${P}_MJJAS"
    PNAM2=`echo $FILE | cut -d "_" -f2-`
#
# Haeufigkeit pro Jahr:  
#
    set +ex
     cdo -s sub -setvrange,${VRANGE} -selyear,${SP} $FILE BASE_$SGE_TASK_ID temp_$SGE_TASK_ID
#
# nur MONTHS-Perdiode betrachten, alles andere 0-setzten 
#
     cdo selmon,${MONTHS} temp_$SGE_TASK_ID tempa_$SGE_TASK_ID
     cdo subc,10000 -selmon,${NMONTHS} temp_$SGE_TASK_ID tempb_$SGE_TASK_ID
     cdo mergetime tempa_$SGE_TASK_ID tempb_$SGE_TASK_ID tempc_$SGE_TASK_ID
     rm temp_IPCC-11 tempa_$SGE_TASK_ID tempb_$SGE_TASK_ID
#    
     cdo -s consects -gtc,0 tempc_$SGE_TASK_ID temp1_$SGE_TASK_ID
 #
 #
 # Alles was groesser 3 Tage ist, ist Heatwave:
 #
 # Anzahl der Heatwaves:
 #  
     cdo -s setmisstoc,0 -timsum -gtc,3 temp1_$SGE_TASK_ID ${POUT}/${PNAM}_NHeatWave-p${PC}th_${PNAM2}
 #
 # Mittlere Laenge Heatwave:
 #  
     cdo -s gtc,3 temp1_$SGE_TASK_ID HW_Mask_$SGE_TASK_ID
     cdo ifthen HW_Mask_$SGE_TASK_ID temp1_$SGE_TASK_ID Days_Heatwave_$SGE_TASK_ID
     cdo setmisstoc,0 -timmean -setvrange,2,100000 Days_Heatwave_$SGE_TASK_ID ${POUT}/${PNAM}_DHeatWave-p${PC}th_${PNAM2}
# 
     rm tempc_$SGE_TASK_ID temp1_$SGE_TASK_ID HW_Mask_$SGE_TASK_ID Days_Heatwave_$SGE_TASK_ID
    set -ex
    
   done #FILES1
  done #PER
  rm BASE_$SGE_TASK_ID
 done #PCS
done #FILES
done #VARS
#
fi 
#
#
# ============================================================================================================================================
#
#
# IPCC 12: DrySpell: 95th percentil aller DrySpells ( >5 Tage) in einer 30jahresperiode (p95th_dryspell), Anzahl der Dryspells pro Periode (N_dryspell) und maximaler Dryspell pro Jahr (MaxJ_dryspell) 
#
#
if ${dryspell} && [ $SGE_TASK_ID -eq 13 ] ; then
#
PER="2021-50"
#PER="1971-00 2021-50 2071-00"
#
VRANGE="-0.01,1"

cd ${PFAD}/pr
#
set +ex
 mkdir -p ${PIPCC}/dryspell/data
 POUT="${PIPCC}/dryspell/data"
set -ex
for P in ${PER} ; do

 if [ $P == '1971-00' ] ; then
  SP="1971/2000"
  FILES=`ls *historical*.nc`
 elif [ $P == '2021-50' ] ; then
  SP="2021/2050"
  FILES=`ls *MED-11_CNRM-CM5*rcp*.nc`
 elif [ $P == '2071-00' ] ; then
  SP="2071/2100"
  FILES=`ls *rcp*.nc`
 else 
  echo "FEHLER"
  exit
 fi
  
#   
 set +ex
 for F in $FILES ; do
  PNAM=${P}_"p95th_dryspell"_`echo $F | cut -d "_" -f2-`
  PNAM1=${P}_"N_dryspell"_`echo $F | cut -d "_" -f2-`
  PNAM2=${P}_"MaxJ_dryspell"_`echo $F | cut -d "_" -f2-`

  cdo selyear,${SP} -setvrange,${VRANGE} $F dFIN_$SGE_TASK_ID
  cdo consects -ltc,1 dFIN_$SGE_TASK_ID dtemp_$SGE_TASK_ID
  rm dFIN_$SGE_TASK_ID

#  cdo consects -ltc,1 -setvrange,${VRANGE} -selyear,${SP} $F dtemp_$SGE_TASK_ID

  #
  cdo gec,5 dtemp_$SGE_TASK_ID dMASK_$SGE_TASK_ID
  cdo ifthen dMASK_$SGE_TASK_ID dtemp_$SGE_TASK_ID dSPELLS_$SGE_TASK_ID
  rm dtemp_$SGE_TASK_ID
  #
  cdo timsum dMASK_$SGE_TASK_ID ${POUT}/${PNAM1}
  rm dMASK_$SGE_TASK_ID
  #
  cdo yearmax dSPELLS_$SGE_TASK_ID ${POUT}/${PNAM2}
  #
  cdo timmin dSPELLS_$SGE_TASK_ID dMIN_$SGE_TASK_ID
  cdo timmax dSPELLS_$SGE_TASK_ID dMAX_$SGE_TASK_ID  
  cdo timpctl,95 dSPELLS_$SGE_TASK_ID dMIN_$SGE_TASK_ID dMAX_$SGE_TASK_ID ${POUT}/${PNAM}
  rm dMIN_$SGE_TASK_ID dMAX_$SGE_TASK_ID dSPELLS_$SGE_TASK_ID
 done
 set -ex
done  
fi  
##
#
# ============================================================================================================================================
#
# IPCC - 13: Mittlere Temperatur - Seasonal:
#
if ${tmeanseas} && [ $SGE_TASK_ID -eq 14 ] ; then
#
PER="1971-00 2021-50 2071-00"
SEAS="YEAR DJF MAM JJA SON"
VAR=tas
#
VRANGE="270,370"
#
cd ${PFAD}/${VAR}

set +ex
 mkdir -p ${PIPCC}/tmean-seas/data
 POUT="${PIPCC}/tmean_seas/data"
set -ex
for P in ${PER} ; do

 if [ $P == '1971-00' ] ; then
  P1='1970-00'
  SP="1971/2000"
  SP1="1970/2000"
  FILES=`ls *historical*.nc`
 elif [ $P == '2021-50' ] ; then
  P1='2020-50'
  SP="2021/2050"
  SP1="2020/2050"
  FILES=`ls *rcp*.nc`
 elif [ $P == '2071-00' ] ; then
  P1='2070-00'
  SP="2071/2100"
  SP1="2070/2100"
  FILES=`ls *rcp*.nc`
 else 
  echo "FEHLER"
  exit
 fi

 for F in $FILES ; do
   set +ex
    cdo seasmean -selyear,${SP1} $F ${POUT}/${F}_seasmean_$SGE_TASK_ID
   set -ex
 done

 set +ex
 for S in ${SEAS} ; do
  if [ $S == 'YEAR' ] ; then
   for F in $FILES ; do
    echo $F
    PNAM=${P}_${S}_tmean-seas_`echo $F | cut -d "_" -f2-`
    echo $PNAM
    cdo selyear,${SP} -yearmean $F ${POUT}/${PNAM}
   done
  elif [ $S == 'DJF' ] ; then
   for F in $FILES ; do
    echo $F
    PNAM=${P}_${S}_tmean-seas_`echo $F | cut -d "_" -f2-`
    echo $PNAM
    cdo selyear,${SP} -selmon,2 ${POUT}/${F}_seasmean_$SGE_TASK_ID ${POUT}/${PNAM}
   done
  elif [ $S == 'MAM' ] ; then
   for F in $FILES ; do
    echo $F
    PNAM=${P}_${S}_tmean-seas_`echo $F | cut -d "_" -f2-`
    echo $PNAM
    cdo selyear,${SP} -selmon,5 ${POUT}/${F}_seasmean_$SGE_TASK_ID ${POUT}/${PNAM}
   done
  elif [ $S == 'JJA' ] ; then
   for F in $FILES ; do
    echo $F
    PNAM=${P}_${S}_tmean-seas_`echo $F | cut -d "_" -f2-`
    echo $PNAM
    cdo selyear,${SP} -selmon,8 ${POUT}/${F}_seasmean_$SGE_TASK_ID ${POUT}/${PNAM}
   done
  elif [ $S == 'SON' ] ; then
   for F in $FILES ; do
    echo $F
    PNAM=${P}_${S}_tmean-seas_`echo $F | cut -d "_" -f2-`
    echo $PNAM
    cdo selyear,${SP} -selmon,11 ${POUT}/${F}_seasmean_$SGE_TASK_ID ${POUT}/${PNAM}
   done
  fi
 done
 set -ex
 set +ex
  for F in $FILES ; do
   rm ${POUT}/${F}_seasmean_$SGE_TASK_ID
  done
 set -ex
 
done  
fi  
#
#
# ============================================================================================================================================
#
# IPCC - 14: Mittlerer Niederschlag - Seasonal:
#
if ${ptotseas} && [ $SGE_TASK_ID -eq 15 ] ; then
#
#PER="1971-00 2021-50 2071-00"
PER="2071-00"
SEAS="YEAR DJF MAM JJA SON"
VAR=pr
#
VRANGE="0,10000"
#
cd ${PFAD}/${VAR}

set +ex
 mkdir -p ${PIPCC}/ptot-seas/data
 POUT="${PIPCC}/ptot-seas/data"
set -ex
for P in ${PER} ; do

if [ $P == '1971-00' ] ; then
  P1='1970-00'
  SP="1971/2000"
  SP1="1970/2000"
  FILES=`ls *historical*.nc`
 elif [ $P == '2021-50' ] ; then
  P1='2020-50'
  SP="2021/2050"
  SP1="2020/2050"
  FILES=`ls *rcp*.nc`
 elif [ $P == '2071-00' ] ; then
  P1='2070-00'
  SP="2071/2100"
  SP1="2070/2100"
  FILES=`ls *rcp*.nc`
 else 
  echo "FEHLER"
  exit
 fi

 set +ex
 for F in $FILES ; do
   cdo seasmean -selyear,${SP1} $F ${POUT}/${F}_seasmean_$SGE_TASK_ID
 done

 for S in ${SEAS} ; do
  if [ $S == 'YEAR' ] ; then
   for F in $FILES ; do
    echo $F
    PNAM=${P}_${S}_ptot-seas_`echo $F | cut -d "_" -f2-`
    echo $PNAM
    cdo selyear,${SP} -yearmean $F ${POUT}/${PNAM}
   done
  elif [ $S == 'DJF' ] ; then
   for F in $FILES ; do
    echo $F
    PNAM=${P}_${S}_ptot-seas_`echo $F | cut -d "_" -f2-`
    echo $PNAM
    cdo selmon,2 -selyear,${SP} ${POUT}/${F}_seasmean_$SGE_TASK_ID ${POUT}/${PNAM}
   done
  elif [ $S == 'MAM' ] ; then
   for F in $FILES ; do
    echo $F
    PNAM=${P}_${S}_ptot-seas_`echo $F | cut -d "_" -f2-`
    echo $PNAM
    cdo selmon,5 -selyear,${SP} ${POUT}/${F}_seasmean_$SGE_TASK_ID ${POUT}/${PNAM}
   done
  elif [ $S == 'JJA' ] ; then
   for F in $FILES ; do
    echo $F
    PNAM=${P}_${S}_ptot-seas_`echo $F | cut -d "_" -f2-`
    echo $PNAM
    cdo selmon,8 -selyear,${SP} ${POUT}/${F}_seasmean_$SGE_TASK_ID ${POUT}/${PNAM}
   done
  elif [ $S == 'SON' ] ; then
   for F in $FILES ; do
    echo $F
    PNAM=${P}_${S}_ptot-seas_`echo $F | cut -d "_" -f2-`
    echo $PNAM
    cdo selmon,11 -selyear,${SP} ${POUT}/${F}_seasmean_$SGE_TASK_ID ${POUT}/${PNAM}
   done
  fi
 done

  for  F in $FILES ; do
   rm ${POUT}/${F}_seasmean_$SGE_TASK_ID
  done
 set -ex
 
 
 
done  
fi  

#
#
#
# ============================================================================================================================================
#
# IPCC - 15: Temperatur Wachstumsperiode (Anzahl Tage mit tas > 5 Grad)
#
if ${growperiod} && [ $SGE_TASK_ID -eq 16 ] ; then
#
PER="1971-00 2021-50 2071-00"
VAR=tas
#
VRANGE="220,320"
#
cd ${PFAD}/${VAR}

set +ex
 mkdir -p ${PIPCC}/growperiod/data
 POUT="${PIPCC}/growperiod/data"
set -ex
for P in ${PER} ; do

 FILES=`ls ${P}*${VAR}*.nc`
 
 for F in ${FILES} ; do
  PNAM=${P}_YEAR_growperiod_`echo $F | cut -d "_" -f2-`
  cdo timmean -yearsum -gtc,278.15 ${F} ${POUT}/${PNAM}
  done
done  
fi  


#
#
#
# ============================================================================================================================================
#
# IPCC - 16: Frostfreie Periode (Anzahl Tage mit tas > 10 Grad)
#
if ${ffp} && [ $SGE_TASK_ID -eq 17 ] ; then
#
PER="1971-00 2021-50 2071-00"
VAR=tas
#
VRANGE="220,320"
#
cd ${PFAD}/${VAR}

set +ex
 mkdir -p ${PIPCC}/ffp/data
 POUT="${PIPCC}/ffp/data"
set -ex
for P in ${PER} ; do

 FILES=`ls ${P}*${VAR}*.nc`
 
 for F in ${FILES} ; do
  PNAM=${P}_YEAR_ffp_`echo $F | cut -d "_" -f2-`
  cdo timmean -yearsum -gtc,283.15 ${F} ${POUT}/${PNAM}
  done
done  
fi  

#
#
#
# ============================================================================================================================================
#
# IPCC - 17: Temperatur Summe waehrend Temperatur Wachstumsperiode (Anzahl Tage mit tas > 5 Grad)
#
if ${growperiodsumtmean} && [ $SGE_TASK_ID -eq 18 ] ; then
#
PER="1971-00 2021-50 2071-00"
VAR=tas
#
VRANGE="220,320"
#
cd ${PFAD}/${VAR}

set +ex
 mkdir -p ${PIPCC}/growperiodsumtmean/data
 POUT="${PIPCC}/growperiodsumtmean/data"
set -ex
for P in ${PER} ; do

 FILES=`ls ${P}*${VAR}*.nc`
 
 for F in ${FILES} ; do
  PNAM=${P}_YEAR_growperiodsumtmean_`echo $F | cut -d "_" -f2-`
  cdo timmean -yearsum -gtc,278.15 ${F} ${POUT}/${PNAM}
  exit
  done
done  
fi  


#
#
# ============================================================================================================================================
#
# IPCC - 18: Anzahl von Eistagen basierend auf tasmax -> Vorsicht, nur 9 Modelle verfuegbar:
#
#
if ${icedays} && [ $SGE_TASK_ID -eq 19 ] ; then
#  
PER="1971-00 2021-50 2071-00"
SEAS="YEAR"
VAR=tasmax
#
VRANGE="220,320"
#
cd ${PFAD}/${VAR}
#   
#   
set +ex
 mkdir -p ${PIPCC}/icedays/data
 POUT="${PIPCC}/icedays/data"
set -ex

for P in ${PER} ; do

 FILES=`ls ${P}*${VAR}*.nc`
 
 for S in ${SEAS} ; do 
  for F in $FILES ; do
    PNAM=${P}_${S}_icedays_`echo $F | cut -d "_" -f2-`
    cdo timmean -yearsum -ltc,273.15 -setvrange,${VRANGE} $F ${POUT}/${PNAM}
  done
 done 
done  
fi  

#
#
# ============================================================================================================================================
#
# IPCC - 19: Einfacher Niederschlagsintensitaetsindex SDII: Durchschnittlicher Niederschlag an wet days (>= 1mm)
#
#
if ${spii} && [ $SGE_TASK_ID -eq 20 ] ; then
#  
PER="1971-00 2021-50 2071-00"
SEAS="YEAR"
VAR=pr
#
VRANGE="1,10000"
#
cd ${PFAD}/${VAR}
#   
#   
set +ex
 mkdir -p ${PIPCC}/spii/data
 POUT="${PIPCC}/spii/data"
set -ex

for P in ${PER} ; do

 FILES=`ls ${P}*${VAR}*.nc`
 
 for S in ${SEAS} ; do 
  for F in $FILES ; do
    PNAM=${P}_${S}_spii_`echo $F | cut -d "_" -f2-`
    cdo timmean -div -yearsum -setvrange,${VRANGE} $F -yearsum -gec,1 -setvrange,0,10000 $F ${POUT}/${PNAM}
  done
 done 
done  
fi  

#
#
# ============================================================================================================================================
#
# IPCC - 20: Jahresmittel Anzahl Tage mit Niederschlag >=20mm
#
#
if ${r20mm} && [ $SGE_TASK_ID -eq 21 ] ; then
#  
PER="1971-00 2021-50 2071-00"
SEAS="YEAR"
VAR=pr
#
VRANGE="0,10000"
#
cd ${PFAD}/${VAR}
#   
#   
set +ex
 mkdir -p ${PIPCC}/r20mm/data
 POUT="${PIPCC}/r20mm/data"
set -ex

for P in ${PER} ; do

 FILES=`ls ${P}*${VAR}*.nc`
 
 for S in ${SEAS} ; do 
  for F in $FILES ; do
    PNAM=${P}_${S}_r20mm_`echo $F | cut -d "_" -f2-`
    cdo timmean -eca_r20mm -setvrange,${VRANGE} $F ${POUT}/${PNAM}
  done
 done 
done  
fi  




#
#
# ============================================================================================================================================
#
date
exit
