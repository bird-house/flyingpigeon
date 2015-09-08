from malleefowl import wpslogging as logging
#import logging
logger = logging.getLogger(__name__)

from os import path , mkdir, listdir
import ocgis

def get_equation(culture_type='fallow', climate_type=2):
  """ 
  returns the equation as basis to calculate the segetal flora
  :param culture_type: Type of culture. possible values are
                       'fallow', 'intensiv', 'extensive' (default:'fallow')
  :param climate_type: Type of climate: number 1 to 7 or 'all' (default: 2)
  :example: eq = get_equation(culture_type= 'fallow', climate_type=2)
  """
  logger.debug('equation for %s %s '% (culture_type, climate_type))
  climate_type = str(climate_type)
  if culture_type == 'fallow':  
    if climate_type == '1': 
      equation = "sffallow1=exp(46.4619-15.4355*(%s-273.15)+0.7070*power((%s-273.15),2))" %('tas','tas')
    elif climate_type == '2': 
      equation = "sffallow2=66.463*exp(-0.1315*(%s-273.15))"  %('tas')
    elif climate_type == '3': 
      equation ="sffallow3=exp(1.837+0.2856*(%s-273.15)-0.0156*power((%s-273.15),2))" %('tas','tas')
    elif climate_type == '4': 
      equation ="sffallow4=0.0685*power((%s-273.15),3-2.9184)*(%s-273.15)^2+38.864*(%s-273.15)-80.046" %('tas','tas','tas')
    elif climate_type == '5': 
      equation ="sffallow5=1.68*exp(0.735*exp(0.1134*(%s-273.15)))" %('tas')
    elif climate_type == '6': 
      equation ="sffallow6=0.0088*power((%s-273.15),3-0.3457)*power((%s-273.15),2)+3.6656*(%s-273.15)+5.3486" %('tas','tas','tas')
    elif climate_type == '7': 
      equation ="sffallow7=3.4655*exp(0.1303*(%s-273.15))" %('tas')
    elif climate_type == 'all': 
      equation ="sffallowall=0.2787*power((%s-273.15),3)-7.5658*power((%s-273.15),2)+72.4143*(%s-273.15)-76.29" %('tas','tas','tas')
    else: 
      equation = None
      logger.exception('No equations determinated')
        
  elif culture_type == 'extensiv': 
    if climate_type == '1': 
      equation ="sfextensiv1=exp(46.0518-15.4597*%s+0.7143*power(%s,2))" %('tas','tas')
    elif climate_type == '2': 
      equation ="sfextensiv2=45.6589*exp(-0.0987*%s)"%('tas')
    elif climate_type == '3': 
      equation ="sfextensiv3=exp(1.4383+0.33*%s-0.0176*power(%s,2))"%('tas','tas')
    elif climate_type == '4': 
      equation ="sfextensiv4=-0.7587*power(%s,2)+17.8515*%s-32.8794"%('tas','tas')
    elif climate_type == '5': 
      equation ="sfextensiv5=0.0817*exp(0.4597*%s)"%('tas')
    elif climate_type == '6': 
      equation ="sfextensiv6=-0.0263*power(%s,3)+0.7119*power(%s,2)-4.878*%s+11.154" %('tas','tas','tas')
    elif climate_type == '7': 
      equation ="sfextensiv7=1.1653*exp(0.1711*%s)"%('tas')
    elif climate_type == 'all': 
      equation ="sfextensivall=0.2386*power(%s,3)-6.5351*power(%s,)2+62.475*%s-73.9023" %('tas','tas','tas')
    else: 
      equation = None
      logger.exception('No equations determinated')
  
  elif culture_type == 'intensiv': 
    if climate_type == '1': 
      equation ="sfintensiv1=exp(46.0518-15.4597*%s+0.7143*power(%s,2))" %('tas','tas')
    elif climate_type == '2': 
      equation ="sfintensiv2=31.3493*exp(-0.1108*%s)" %('tas')
    elif climate_type == '3': 
      equation ="sfintensiv3=exp(1.0791+0.3449*%s-0.0189*power(%s,2))" %('tas','tas')
    elif climate_type == '4': 
      equation ="sfintensiv4=-0.0919*power(%s,3)+2.3824*power(%s,2)-14.29*%s+38.93" %('tas','tas','tas')
    elif climate_type == '5': 
      equation ="sfintensiv5=exp(0.1663+0.0457*%s+0.0128*power(%s^2))" %('tas','tas')
    elif climate_type == '6': 
      equation ="sfintensiv6=14.1641*exp(-0.0363*%s)" %('tas')
    elif climate_type == '7': 
      equation ="sfintensiv7=1.704*exp(0.0982*%s)" %('tas')
    elif climate_type == 'all': 
      equation ="sfintensivall=43.2846*exp(0.071*%s)" %('tas')
    else: 
      equation = None
      logger.exception('No equations determinated')
  else:
    equation = None
    logger.exception('No equations determinated')
  return equation

def remove_rows(infile, outfile , indicator='1e+20'): 
  with open(infile,"r") as input:
    with open(outfile,"w") as output: 
        for line in input:
            if not indicator in line: #!="nickname_to_delete"+"\n":
                output.write(line)
  return outfile                

def get_segetalflora(resource=[], dir_output='.', culture_type='fallow', climate_type=2, region=None): 
  """productive worker for segetalflora jobs
  :param resources: list of tas netCDF files. (any time aggregation is possible)
  :param culture_type: Type of culture. possible values are
                       'fallow', 'intensiv', 'extensive' (default:'fallow')
  :param climate_type: Type of climate: number 1 to 7 or 'all' (default: 2)
  :param region: Region for subset. If 'None' (default) the values will be calculated for Europe
  """
  from flyingpigeon.subset import clipping
  from flyingpigeon.utils import calc_grouping, sort_by_filename 
  import os
  from os import remove
  from tempfile import mkstemp
  from ocgis import RequestDataset , OcgOperations
  
  from cdo import Cdo
  cdo = Cdo()
  
  os.chdir(dir_output)
  
  #outputs = []
  
  if region == None: 
    region = 'Europe'
    
  if not type(culture_type) == list:
    culture_type = list([culture_type])
  if not type(climate_type) == list:
    climate_type = list([climate_type])
    
  
  ncs = sort_by_filename(resource)
  print '%s experiments found' % (len(ncs))
  
  # generate outfolder structure: 
  
  dir_netCDF = 'netCDF'
  dir_ascii =  'ascii'
  dir_netCDF_tas = dir_netCDF+'/tas'
  dir_ascii_tas = dir_ascii+'/tas'
  
  if not os.path.exists(dir_netCDF):
    os.makedirs(dir_netCDF)
  if not os.path.exists(dir_ascii):
    os.makedirs(dir_ascii)  
  if not os.path.exists(dir_netCDF_tas):
    os.makedirs(dir_netCDF_tas)
  if not os.path.exists(dir_ascii_tas):
    os.makedirs(dir_ascii_tas)

  tas_files = []
  
  for key in ncs.keys():
    try:
      print 'process %s' % (key)
      calc =  [{'func':'mean','name':'tas'}]
      calc_group = calc_grouping('yr')
      prefix = key.replace(key.split('_')[7],'yr')
      tas_files.append(prefix)
      nc_tas = clipping(resource=ncs[key], variable='tas', calc=calc,  calc_grouping= calc_group, prefix=prefix, polygons='Europe', output_format='nc', dir_output=dir_netCDF_tas)[0]
      print 'clipping done for %s' % (key)
    except Exception as e:
      print 'clipping failed for %s: %s' % (key, e)
    try:
      f, tmp = mkstemp(suffix='.asc',  dir=dir_output)
      asc_tas = os.path.join(dir_ascii_tas,prefix + '.asc')
      cmd = 'cdo outputtab,name,date,lon,lat,value %s > %s' % (nc_tas, tmp)
      os.system(cmd)
      print ('tanslation to ascii done')
      remove_rows(tmp, asc_tas)
      remove(tmp)
      print ('rows with missing Values removed')
    except Exception as e: 
      print 'translation to ascii failed %s: %s' % (key, e)
    
  outputs = []  
  for name in tas_files:
    for cult in culture_type: 
      for climat in climate_type:
        try: 
          calc = get_equation(culture_type=cult, climate_type=climat)
          var = 'sf%s%s' %(cult, climat)
          prefix = name.replace('tas',var)
          outputs.append(prefix)
          infile = os.path.join(dir_netCDF_tas,name+'.nc')
          print infile
          dir_sf = os.path.join(dir_netCDF,var)
          if not os.path.exists(dir_sf):
            os.makedirs(dir_sf)
            
          rd = RequestDataset(infile, variable='tas')
          op = OcgOperations(dataset=rd, calc=calc, prefix=prefix, output_format='nc', dir_output=dir_sf, add_auxiliary_files=False)
          nc_sf = op.execute()
          print 'segetalflora done for %s' % (prefix)
        except Exception as e: 
          print 'netCDF segetalflora failed: %s' % (e)
        
        try:
          
          f, tmp = mkstemp(suffix='.asc',  dir=dir_output)
          
          dir_ascii_sf = os.path.join(dir_ascii,var)
          if not os.path.exists(dir_ascii_sf):
            os.makedirs(dir_ascii_sf)
          asc_sf = os.path.join(dir_ascii_sf,prefix + '.asc')
          
          cmd = 'cdo outputtab,name,date,lon,lat,value %s > %s' % (nc_tas, tmp)
          os.system(cmd)
          print ('tanslation to ascii done')
          remove_rows(tmp, asc_sf)
          remove(tmp)
          print ('rows with missing Values removed')
          
        except Exception as e: 
          print 'failed for ascii file: %s' % (e)    
  return outputs
