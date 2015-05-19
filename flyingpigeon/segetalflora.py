from malleefowl import wpslogging as logging
#import logging
logger = logging.getLogger(__name__)

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



def get_segetalflora(resources, culture_type='fallow', climate_type=2, dir_segetalflora=None, dir_tas=None, dir_fieldmean=None ):
  """ 
  returns a netCDF file containing vaulues of number of segetal flora species
  
  :param resources: dictionary of sorted tas mean input file(s)
  :param culture_type: Type of culture. possible values are
                       'fallow', 'intensiv', 'extensive' (default:'fallow')
  :param climate_type: Type of climate: number 1 to 7 or 'all' (default: 2)
  
  """
  from tempfile import mkstemp 
  from os import path , mkdir
  from flyingpigeon import timeseries as ts 
  
  ocgis.env.OVERWRITE = True
  
  logger.debug('dir_outputs created')
  
  tas_yearmean = ts.get_yearmean(resources, variable='tas', dir_output=dir_tas)
  calc = get_equation(culture_type=culture_type, climate_type=climate_type)
  
  logger.debug('tas_yearmean dictionay build')
  ocgis.env.DIR_OUTPUT = dir_segetalflora
  
  sf_files = []
  
  for nc in tas_yearmean:
    try:
      basename = path.basename(tas_yearmean[0])
      prefix = basename.replace('tas_', 'sf%s%s_'%(culture_type, climate_type)).strip('.nc') 
      rd = ocgis.RequestDataset(tas_yearmean , 'tas')
      
      geom_file = ocgis.OcgOperations(dataset=rd, calc=calc, dir_output=dir_segetalflora, prefix=prefix, add_auxiliary_files=False, output_format='nc').execute()
      sf_files.append(geom_file)
      logger.debug('segetalflora processed : %s' % (basename))
    except Exception as e:
      msg = 'segetalflora calculation failed %s : %s\n' %( nc, e) 
      logger.exception(msg)
      outlog = outlog + msg
      
  return sf_files