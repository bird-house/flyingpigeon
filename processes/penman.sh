#!/bin/bash 
#


# in_path has to contain the following files:
# 
# tas_filename.nc
# sfcwind_filename.nc
# rlds_filename.nc
# rsds_filename.nc
# rlus_filename.nc
# rsus_filename.nc
# ps_filename.nc
# huss_filename.nc
# pr_filename.nc

#  program call ./WPS_evspsblpot_day.sh filename.nc 

date
echo "run WPS_evspsblpot_day.sh with filenames:" 

mkdir ../temp.$$


# get the files from arguments 

#input variables
nc_tas=$1
nc_sfcwind=$2
nc_rlds=$3
nc_rlus=$4
nc_rsds=$5
nc_rsus=$6
nc_ps=$7
nc_huss=$8
nc_pr=$9 

# output variables
nc_evspsblpot=$10

temp_dir=../temp.$$


## Selection of parameter and convertion to appropraite units 
cdo subc,273.2  $nc_tas            $temp_dir/tas.nc # 
cdo divc,1.3    $nc_sfcwind        $temp_dir/sfcwind.nc
cdo add         $nc_rlds  $nc_rlus $temp_dir/rlds_net.nc
cdo add         $nc_rsds  $nc_rsus $temp_dir/rsds_net.nc 

##  Calculation of relative humidity and dew point

# calculation of vapour pressure
# cdo  mul ../in/ps_$filename -divc,62.2 ../in/huss_$filename $temp/e_$filename
cdo -O merge $nc_ps $nc_huss  $temp_dir/merge_$name
cdo expr,'e=((ps*huss)/62.2)' $temp_dir/merge_$name $temp_dir/e_$name

# partial vapour pressure using Magnus-Formula over water 
cdo expr,'es=6.1078*exp(17.08085*(tas-273.16)/(234.175+(tas-273.16)))' $nc_tas $temp_dir/es_$name

# calculate relative humidity 

cdo setname,hurs -setreftime,1949-12-01,00:00:00,day -div $temp_dir/e_$name  $temp_dir/es_$name $temp_dir/hurs01.nc 
#ncatted -O -a units,hurs,o,c,1 $temp_dir/hurs.nc
#ncatted -O -a standard_name,hurs,o,c,"relative_humidity" $temp_dir/hurs.nc

cdo mulc,100  $temp_dir/hurs01.nc $temp_dir/hurs.nc

# dew point temperature
cdo merge $temp_dir/tas.nc $temp_dir/hurs.nc $temp_dir/hurs_tas.nc
cdo expr,'tdps=(((hurs/100)^(1/8))*(112+0.9*tas)+0.1*tas-112)' $temp_dir/hurs_tas.nc $temp_dir/tdps.nc


####Berechnen: Saettigungsdampfdruck TEMP2 #####################
##ES = 6.11*exp((17.62*T)/(243.12+T)) ###hPa/C

cdo  addc,243.12 $temp_dir/tas.nc                  $temp_dir/NT.nc
cdo  mulc,17.62  $temp_dir/tas.nc                  $temp_dir/ZT.nc
cdo  mulc,6.11 -exp -div $temp_dir/ZT.nc $temp_dir/NT.nc $temp_dir/ET.nc 

####Berechnen: Saettigungsdampfdruck DEW2 ###################### 
##ES = 6.11*exp((17.62*T)/(243.12+T)) ###hPa/C


cdo  addc,243.12 $temp_dir/tdps.nc   $temp_dir/NTD.nc
cdo  mulc,17.62  $temp_dir/tdps.nc 	 $temp_dir/ZTD.nc
cdo  mulc,6.11 -exp -div $temp_dir/ZTD.nc $temp_dir/NTD.nc   $temp_dir/ETD.nc      ###hPa/K


####Berechnung: Steigung der Saettigungsdampfdruckkurve ########
#DEDT  = ES*(4284.0/((243.12+pT)^2))

cdo mul $temp_dir/ET.nc  -mulc,4284. -reci  -sqr $temp_dir/NT.nc   $temp_dir/DEDT.nc
#rm NT.ieg

####modifizierte Psychrometerkonstante GammaX = Gamma*(1.0+0.34*V2)
# Psychrometerkonstante Gamma = 0.65 
cdo  mulc,0.65 -addc,1.  -mulc,0.34 $temp_dir/sfcwind.nc $temp_dir/GAMMAX.nc

####spezielle Verdunstungswaerme ###############################
#L = 249.8-0.242*pT
cdo  addc,249.8 -mulc,-0.242 $temp_dir/tas.nc  $temp_dir/L.nc

####Strahlungsbilanz aus Modell 1h #############################
cdo add $temp_dir/rlds_net.nc $temp_dir/rsds_net.nc $temp_dir/STRAHLBAL_W_1DM.nc

## Change unit W/m2 to Ws/(cm2 mm)
cdo mulc,8.64 $temp_dir/STRAHLBAL_W_1DM.nc $temp_dir/tmp.nc
mv $temp_dir/tmp.nc $temp_dir/STRAHLBAL_W_1DM.nc

##Verdunstungsaequivalent der Nettostrahlung in mm/d  Tageswerte
#R/L
cdo div $temp_dir/STRAHLBAL_W_1DM.nc $temp_dir/L.nc  $temp_dir/RN.nc

#################################################################
#################################################################

#####Gras-Referenzverdunstung in mm/d ###########################
#ET0 = (s*Rn)/(s+GammaX) + (90.0*Gamma)/(s+GammaX) * ES/(pT+273.0) * (1-pU/100.0)*V2

cdo div -mul $temp_dir/DEDT.nc $temp_dir/RN.nc -add $temp_dir/GAMMAX.nc $temp_dir/DEDT.nc $temp_dir/ET0_HELP1.nc

###(90.0*Gamma)=0.65*90=58.5
cdo mulc,58.5 -reci -add $temp_dir/GAMMAX.nc $temp_dir/DEDT.nc  $temp_dir/ET0_HELP2.nc
cdo addc,273 $temp_dir/tas.nc $temp_dir/TEMP2_DM.nc
cdo div $temp_dir/ET.nc $temp_dir/TEMP2_DM.nc $temp_dir/ET0_HELP3.nc
cdo mul $temp_dir/sfcwind.nc -addc,1. -divc,-100. $temp_dir/hurs.nc $temp_dir/ET0_HELP4.nc
cdo add $temp_dir/ET0_HELP1.nc -mul $temp_dir/ET0_HELP2.nc -mul $temp_dir/ET0_HELP3.nc $temp_dir/ET0_HELP4.nc  $temp_dir/ET0.nc

# climatic water balance 
# 
# cdo setname,cwb -sub $temp_dir/ET0.nc $in_path'pr_'$name $out_path'cwb_'$name
# ncatted -O -a units,cwb,o,c,'kg m-2 s-1' $out_path'cwb_'$name 
# ncatted -O -a standard_name,cwb,o,c,"climatic_water_balance" $out_path'cwb_'$name

##### rename the variables in file #########

cdo setname,evspsblpot -divc,86400 $temp_dir/ET0.nc $nc_evspsblpot 
ncatted -O -a units,evspsblpot,o,c,'kg m-2 s-1' $nc_evspsblpot 
ncatted -O -a standard_name,evspsblpot,o,c,"potential_water_evaporation_flux" $nc_evspsblpot

# cdo setname,tdps $temp_dir'tdps_'$name $out_path'tdps_'$name
# ncatted -O -a units,tdps,o,c,'kg m-2 s-1' $out_path'tdps_'$name 
# ncatted -O -a standard_name,tdps,o,c,"potential_water_evaporation_flux" $out_path'tdps_'$name

rm -r ../gret.$$

echo "end grid_evap_test.sh"
date
exit

