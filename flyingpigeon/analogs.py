import logging
logger = logging.getLogger(__name__)


def get_configfile(files, timewin=1, varname='slp', seacyc=False, cycsmooth=91, nanalog=20, seasonwin=30, 
  distfun='rms', calccor=True, silent=False, ): 
  """
  Generating the config file for fortran calculation

  :param :
  """
  from datetime import datetime as dt
  from os.path import abspath
  from tempfile import mkstemp
  
  date_stamp = dt.strftime(dt.now(), format='%Y%m%d_%H%M%S')
  logger.info('start configuraion file preparation at: %s' %(date_stamp))

  # convert True/False to Fortran syntax
  seacyc=str(seacyc)
  calccor=str(calccor)
  silent=str(silent)


  # write stuff to configuration file
  ip, config_file = mkstemp(dir='.',suffix='.txt')

  config = open(config_file, "w")
  
  config.write('!Configuration file for flyingpigeon analogs process\n')
  config.write('!Created : %s \n' % ( date_stamp ))
  config.write('!Version : 0.1 \n')
  config.write('&FILES \n')
  config.write(' my_files%archivefile = "{file}" \n'.format(file=files[0]) ) 
  config.write(' my_files%simulationfile = "{file}" \n'.format(file=files[1]) )
  config.write(' my_files%outputfile = "{file}" \n'.format(file=files[2]) )
  config.write('/ \n')
  config.write('&PARAM \n')
  config.write(' my_params%timewin = {timewin} \n'.format(timewin=timewin))
  config.write(' my_params%varname = "{varname}" \n'.format(varname=varname))
  config.write(' my_params%seacyc = .{seacyc}. \n'.format(seacyc=seacyc.upper()))
  config.write(' my_params%cycsmooth = {cycsmooth} \n'.format(cycsmooth=cycsmooth))
  config.write(' my_params%nanalog = {nanalog} \n'.format(nanalog=nanalog))
  config.write(' my_params%seasonwin = {seasonwin} \n'.format(seasonwin=seasonwin))
  config.write(' my_params%distfun = "{distfun}" \n'.format(distfun=distfun))
  config.write(' my_params%calccor = .{calccor}. \n'.format(calccor=calccor.upper()))
  config.write(' my_params%silent = .{silent}.\n'.format(silent=silent.upper()))
  config.write('/\n')
  config.close()
  return abspath(config_file)

def subset(resource=[], bbox='-80,50,22.5,70', normalize=False):
  """
  returns a subset
  :param resource: netCDF input files of one dataset
  :param bbox: bounding box
  """
  from tempfile import mkstemp
  from cdo import Cdo 
  cdo = Cdo()

  resource.sort()

  ip, nc_concat = mkstemp(dir='.',suffix='.nc')
  nc_concat = cdo.cat(input=resource, output=nc_concat)

  ip, nc_subset = mkstemp(dir='.',suffix='.nc')
  nc_subset = cdo.sellonlatbox('%s' % bbox, input=nc_concat, output=nc_subset)
  logger.info('subset done: %s ' % nc_subset)
  
  if normalize == True: 
    nc_subset = nc_subset

  return nc_subset


# start_date=`date +"%d/%m/%Y (%H:%M:%S)"`
# echo -e "\n\nStarting script at: ${start_date}\n"
# #date_stamp=`date +%Y%m%d_%H%M%S`
# #echo $date_stamp

# # set defaults for arguments
# lwin=1  # default=1 Tage nach analog
# pdomain=-80.0,50.0,22.5,70.0 
# psim=1948-01-01,2014-12-31  # simuation period
# pbase=1948-01-01,2014-12-31 # reference period
# region=NA

# # define the regions
# case $region in
#  NA) lon1_0=280.0 # north atlantic
#      lon2_0=50.0
#      lat1_0=22.5
#      lat2_0=70.0 
#      lon1_1=-80.0
#      lon2_1=50.0;;
#  NHmid) lon1_0=0.0  # nordhemisphere midlat 
#      lon2_0=360.0
#      lat1_0=20.0
#      lat2_0=70.0 
#      lon1_1=-180.0
#      lon2_1=180.0;;
#  *) echo -e "Unknown region $region selected. \n Please select one of NA, NHmid"
#     echo -e " or define new region in $0.";
#     exit 1 ;;
# esac




# basedir='/home/users/sradanov/Data/NCEP/'
# simdir='/home/users/sradanov/Data/NCEP/'
# outdir='/home/scratch01/sradanov/A2C2/'


# seacycnorm='base' # 'sim' 'None'
#    # echo "  -N<mode> define which seasonal cycle should be removed. Choose" ;
#    # echo "     base to remove cycle of the data base/archive from all data sets" ;
#    # echo "     sim to remove cycle of the simulation data set from all data sets (caution : make sure that the simulation is sufficiently long for meaningful cycle calculation)" ;
#    # echo "     own to remove its own cycle from each of the data sets" ;
#    # echo "     none to use raw data" ;
#    # echo "     (default: $seacycnorm)" ;


# wma=91 # running mean window (default=91)

# calccor=True # (default=True) make corrlation between simulated and analogs 
# distancefun=rms #'rootmeansquare' 'mahalanobis' 'opticalflow' 'displacementandamplitude' Distance Function
# seasonwin=30 # windoy to pick analogs around targed day  
# nanalog=20 # Nr. of analogs 
# varname=slp # varname in netCDF file 
# level=surface # level
# detrend=FALSE # detrend of time series 
# silent=bla # dummy 

# #please set this line to the directory containing the source.
# sourcedir=${HOME}/Code/Analogue/RSdev

# # processing arguments
# while getopts 'D:C:S:B:R:P:p:o:m:N:c:w:d:v:n:l:t:s:h:' opt ; do
#  case $opt in
#   D) pdomain=$OPTARG ;;
#   C) lwin=$OPTARG ;;
#   S) psim=$OPTARG ;;
#   B) pbase=$OPTARG ;;
#   R) region=$OPTARG ;;
#   P) basedir=$OPTARG ;;
#   p) simdir=$OPTARG ;;
#   o) outdir=$OPTARG ;;
#   m) wma=$OPTARG ;;
#   N) seacycnorm=$OPTARG ;;
#   c) calccor=$OPTARG ;;
#   w) seasonwin=$OPTARG ;;
#   d) distancefun=$OPTARG ;;
#   v) varname=$OPTARG ;;
#   n) nanalog=$OPTARG ;;
#   l) level=$OPTARG ;;
#   t) detrend=$OPTARG ;;
#   s) silent=$OPTARG ;;
#   h) echo -e "Usage: $0 [options] \n" ; 
#    echo -e "Options: \n" ;
#    echo "  -D<lonmin>,<lonmax>,<latmin>,<latmax> (predictor domain def: $pdomain )" ;
#    echo "  -C<numberofdays> (number of days to average the analogy criterion def: $lwin )" ;
#    echo "  -S<YYYY-MM-DD>,<YYYY-MM-DD> (simulation period def: $psim )" ;
#    echo "  -B<YYYY-MM-DD>,<YYYY-MM-DD> (data base/archive period def: $pbase )" ;
#    echo "  -R<region name> (known region names are NA and NHmid (def: $region)" ;
#    echo "    you can add one by adding a case in retrieve.sh" ;
#    echo "  -P<path to base data> (def: $basedir)" ;
#    echo "  -p<path to simulation data> (def: $simdir)" ;
#    echo "  -o<path to write output file> (def: $outdir)" ;
#    echo "  -N<mode> define which seasonal cycle should be removed. Choose" ;
#    echo "     base to remove cycle of the data base/archive from all data sets" ;
#    echo "     sim to remove cycle of the simulation data set from all data sets (caution : make sure that the simulation is sufficiently long for meaningful cycle calculation)" ;
#    echo "     own to remove its own cycle from each of the data sets" ;
#    echo "     none to use raw data" ;
#    echo "     (default: $seacycnorm)" ;
#    echo "  -m<numberofdays> (number of days to calculate weighted moving average for seasonal cycle smoothing" ;
#    echo "  -c<logical> TRUE if correlation should be calculated as an additional diagnostic, FALSE if not (def: TRUE)" ;
#    echo "  -w<numberofdays> (number of days +- around the target day to consider as candidates. def: $seaonwin)" ;
#    echo "  -d<distance> name of the distance to use for analogue calculation (def: $distancefun)" ;
#    echo "  -n<numberofanalogues> Number of closest analogue dates to write to output (def: $nanalog)" ;
#    echo "  -v<varname> name of the NCEP field to download" ;
#    echo "     The name has to be the same as in the filename in the NCEP database (def:$varname)" ;
#    echo "  -l<vertical level> Either 'surface' for variables like slp or pressure level in hPa, e.g. '500'" ;
#    echo "  -t<logical> TRUE if the predictor variable (see -v) should be detrended, FALSE if not (def: FALSE)" ;
#    echo "      For example for geopotential as a circulation variable in order to remove the temperature induced trend" ;
#    echo "  -silent reduced standard output" ;
#    echo " " ;
#    exit 1 ;;
#  esac
# done

# module load cdo/1.6
# ## Telechargement des reanalyses:
# if [ $silent != "ilent" ]; then echo -e "Downloading NCEP SLP data"; fi
# ./getNCEP_slp.sh ${pdomain} ${region} ${basedir} ${pbase} ${simdir} ${psim} ${sourcedir} ${varname} ${level} ${detrend} ${seacycnorm} ${date_stamp} ${lwin} ${seasonwin} ${distancefun} ${outdir} ${silent}

# case $seacycnorm in
#   none) sesync=.FALSE. ;;
#   *) sesync=.TRUE. ;;
# esac

# # # write stuff to configuration file
# # cat <<EOF >> config_${date_stamp}.txt
# # &PARAM
# #  my_params%timewin = $lwin
# #  my_params%varname = "${varname}"
# #  my_params%seacyc = $sesync
# #  my_params%cycsmooth = $wma
# #  my_params%nanalog = $nanalog
# #  my_params%seasonwin = $seasonwin
# #  my_params%distfun = "${distancefun}"
# #  my_params%calccor = .${calccor}.
# # EOF

# if [ $silent != "ilent" ]; then 
# cat <<EOF >> config_${date_stamp}.txt
#  my_params%silent = .FALSE.
# /
# EOF
# else
# cat <<EOF >> config_${date_stamp}.txt
#  my_params%silent = .TRUE.
# /
# EOF
# fi

# module load netcdf/4p

# #  start fortran program for analogue calculation
# ./analogue.out config_${date_stamp}.txt

# if [[ !$silent ]]; then
# # write finish date and time of script
# end_date=`date +"%d/%m/%Y (%H:%M:%S)"`
# echo -e "\nScript finished at: ${end_date}\n"
# fi
