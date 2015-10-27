from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

from os import path , mkdir, listdir
import ocgis

def get_equation(culture_type='fallow', climate_type=2):
  """ 
  returns the equation as basis to calculate the segetal flora
  :param culture_type: Type of culture. possible values are
                       'fallow', 'intensive', 'extensive' (default:'fallow')
  :param climate_type: Type of climate: number 1 to 7 or 'all' (default: 2)
  :example: eq = get_equation(culture_type= 'fallow', climate_type=2)
  """
  logger.debug('equation for %s %s '% (culture_type, climate_type))
  climate_type = str(climate_type)
  if culture_type == 'fallow':  
    if climate_type == '1': 
      #equation = "sffallow1=exp(46.4619-15.4355*(%s-273.15)+power(0.7070*(%s-273.15),2))" %('tas','tas')
      equation = "sffallow1=exp(46.4619-15.4355*(%s-273.15)+0.7070*power((%s-273.15),2))" %('tas','tas')
    elif climate_type == '2': 
      equation = "sffallow2=66.463*exp(-0.1315*(%s-273.15))"  %('tas')
    elif climate_type == '3': 
      equation ="sffallow3=exp(1.837+0.2856*(%s-273.15)-0.0156*power((%s-273.15),2))" %('tas','tas')
    elif climate_type == '4': 
      equation ="sffallow4=0.0685*power((%s-273.15),3-2.9184)*power((%s-273.15),2)+38.864*(%s-273.15)-80.046" %('tas','tas','tas')
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
      logger.debug('No equations determinated for %s %s' %  (culture_type , climate_type ) )
        
  elif culture_type == 'extensive': 
    if climate_type == '1': 
      equation ="sfextensive1=exp(46.0518-15.4597*(%s-273.15)+0.7143*power((%s-273.15),2))" %('tas','tas')
    elif climate_type == '2': 
      equation ="sfextensive2=45.6589*exp(-0.0987*(%s-273.15))"%('tas')
    elif climate_type == '3': 
      equation ="sfextensive3=exp(1.4383+0.33*(%s-273.15)-0.0176*power((%s-273.15),2))"%('tas','tas')
    elif climate_type == '4': 
      equation ="sfextensive4=-0.7587*power((%s-273.15),2)+17.8515*(%s-273.15)-32.8794"%('tas','tas')
    elif climate_type == '5': 
      equation ="sfextensive5=0.0817*exp(0.4597*(%s-273.15))"%('tas')
    elif climate_type == '6': 
      equation ="sfextensive6=-0.0263*power((%s-273.15),3)+0.7119*power((%s-273.15),2)-4.878*(%s-273.15)+11.154" %('tas','tas','tas')
    elif climate_type == '7': 
      equation ="sfextensive7=1.1653*exp(0.1711*(%s-273.15))"%('tas')
    elif climate_type == 'all': 
      equation ="sfextensiveall=0.2386*power((%s-273.15),3)-6.5351*power((%s-273.15),2)+62.475*(%s-273.15)-73.9023" %('tas','tas','tas')
    else: 
      equation = None
      logger.debug('No equations determinated for (%s-273.15) (%s-273.15)' %  (culture_type , climate_type ) )
  
  elif culture_type == 'intensive': 
    if climate_type == '1': 
      equation ="sfintensive1=exp(46.0518-15.4597*(%s-273.15)+0.7143*power((%s-273.15),2))" %('tas','tas')
    elif climate_type == '2': 
      equation ="sfintensive2=31.3493*exp(-0.1108*(%s-273.15))" %('tas')
    elif climate_type == '3': 
      equation ="sfintensive3=exp(1.0791+0.3449*(%s-273.15)-0.0189*power((%s-273.15),2))" %('tas','tas')
    elif climate_type == '4': 
      equation ="sfintensive4=-0.0919*power((%s-273.15),3)+2.3824*power((%s-273.15),2)-14.29*(%s-273.15)+38.93" %('tas','tas','tas')
    elif climate_type == '5': 
      equation ="sfintensive5=exp(0.1663+0.0457*(%s-273.15)+0.0128*power((%s-273.15),2))" %('tas','tas')
    elif climate_type == '6': 
      equation ="sfintensive6=14.1641*exp(-0.0363*(%s-273.15))" %('tas')
    elif climate_type == '7': 
      equation ="sfintensive7=1.704*exp(0.0982*(%s-273.15))" %('tas')
    elif climate_type == 'all': 
      equation ="sfintensiveall=43.2846*exp(0.071*(%s-273.15))" %('tas')
    else: 
      equation = None
      logger.debug('No equations determinated for %s %s' %  (culture_type , climate_type ) )
  else:
    equation = None
    logger.debug('No equations determinated for %s %s' %  (culture_type , climate_type ) )
  return equation

def remove_rows(infile, outfile , indicator='1e+20'): 
  with open(infile,"r") as input:
    with open(outfile,"w") as output: 
        for line in input:
            if not indicator in line: #!="nickname_to_delete"+"\n":
                output.write(line)
  return outfile                

def plot_ascii(infile):
  ''' plot first timestep based on ascii files'''
  import matplotlib.pyplot as plt
  from os import path, listdir, makedirs
  #print infile
  try:
    p, basename = path.split(infile)
    dir_output = p.replace('ascii','pics')
    if not path.exists(dir_output): 
      makedirs(dir_output)
    
    
    f = open(infile, 'r')
    lat = []
    lon = []
    val = []
    print basename
    c = 0 
    while 1:
      lines = f.readlines(10000)
      if not lines:
        break
      for line in lines:
        line = filter(None, line.split(' '))
        if c != 0:
          year = int(line[1].split('-')[0])
          if c == 1:
            testyear = year
            variable = line[0]
          if year == testyear: 
            lat.append(float(line[2]))
            lon.append(float(line[3]))
            val.append(float(line[4]))
        c = c + 1
    if 'sf' in variable:    
      cm = plt.cm.get_cmap('BuGn')
    else: 
      cm = plt.cm.get_cmap('jet')
    #m = cm.ScalarMappable(cmap=cm.jet)
    #m.set_array(val)
    
    fig = plt.figure()
    #fig.subplots_adjust(bottom = 0)
    #fig.subplots_adjust(top = 1)
    #fig.subplots_adjust(right = 1)
    #fig.subplots_adjust(left = 0)

    plt.scatter(lat, lon, s=2, c=val, alpha=1, lw=0, cmap=cm)
    plt.colorbar()
    plt.title('variable: %s // year: %s' % (variable, testyear))
    fig.savefig(path.join(dir_output ,basename.replace('.asc','')))
    # plt.show()
    plt.close(fig)
    print ('ascii plotted')
    # fig.close()
  except Exception as e:
    print 'plotting failed for file %s : %s' % (basename , e)

def get_segetalflora(resource=[], dir_output='.', culture_type='fallow', climate_type=2, region=None, dimension_map=None): 
  """productive worker for segetalflora jobs
  :param resources: list of tas netCDF files. (any time aggregation is possible)
  :param culture_type: Type of culture. possible values are
                       'fallow', 'intensive', 'extensive' (default:'fallow')
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
  
  if not os.path.exists(dir_output):
    os.makedirs(dir_output)
  
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
  print 'keys: %s ' % (ncs.keys())
  
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
      if not os.path.exists(os.path.join(dir_netCDF_tas,prefix+'.nc')):
        nc_tas = clipping(resource=ncs[key], variable='tas', calc=calc, dimension_map=dimension_map,  
          calc_grouping= calc_group, prefix=prefix, polygons='Europe', dir_output=dir_netCDF_tas)[0]
        print 'clipping done for %s' % (key)
        if os.path.exists(os.path.join(dir_netCDF_tas,prefix+'.nc')):
          tas_files.append(prefix)
        else: 
          print 'clipping failed for %s: No output file exists' % (key)
      else :
        print 'netCDF file allready exists %s' % (key)
        nc_tas = os.path.join(dir_netCDF_tas,prefix+'.nc')
    except Exception as e:
      print 'clipping failed for %s: %s' % (key, e)
    try:
      asc_tas = os.path.join(dir_ascii_tas,prefix + '.asc')
      if not os.path.exists(asc_tas):
        f, tmp = mkstemp(dir=os.curdir, suffix='.asc')
        tmp = tmp.replace(os.path.abspath(os.curdir),'.')

        #cdo.outputtab('name,date,lon,lat,value', input = nc_tas , output = tmp)
        cmd = 'cdo outputtab,name,date,lon,lat,value %s > %s' % (nc_tas, tmp)
        print cmd
        os.system(cmd)
        print ('tanslation to ascii done')
        remove_rows(tmp, asc_tas)
        remove(tmp)
        print ('rows with missing Values removed')
      else: 
        print ('tas ascii allready exists')
      plot_ascii(asc_tas)  
    except Exception as e: 
      print 'translation to ascii failed %s: %s' % (key, e)
      if os.path.exists(tmp):
        remove(tmp)

  tas_files = [os.path.join(dir_netCDF_tas,nc) for nc in os.listdir(dir_netCDF_tas)]
  outputs = []  
  
  for name in tas_files:
    for cult in culture_type: 
      for climat in climate_type:
        try:
          calc = get_equation(culture_type=cult, climate_type=climat)
          if type(calc) != None:
            try:  
              var = 'sf%s%s' %(cult, climat)
              prefix = os.path.basename(name).replace('tas', var).strip('.nc')
              
              infile = name # os.path.join(dir_netCDF_tas,name+'.nc')
              dir_sf = os.path.join(dir_netCDF,var)
              if not os.path.exists(dir_sf):
                os.makedirs(dir_sf)
              if os.path.exists(os.path.join(dir_sf, prefix + '.nc')):
                nc_sf = os.path.join(dir_sf, prefix + '.nc')
                print 'netCDF file allready exists: %s %s ' % (dir_sf, prefix) 
              else:
                rd = RequestDataset(name, variable='tas')
                op = OcgOperations(dataset=rd, calc=calc, prefix=prefix, output_format='nc', 
                  dir_output=dir_sf,dimension_map=dimension_map,
                 add_auxiliary_files=False)
                nc_sf = op.execute()
                print 'segetalflora done for %s' % (prefix)
                outputs.append(prefix)
                
              dir_ascii_sf = os.path.join(dir_ascii,var)
              if not os.path.exists(dir_ascii_sf):
                os.makedirs(dir_ascii_sf)
              asc_sf = os.path.join(dir_ascii_sf,prefix + '.asc')
              if not os.path.exists(asc_sf):
                f, tmp = mkstemp(dir=os.curdir, suffix='.asc')
                tmp = tmp.replace(os.path.abspath(os.curdir),'.')
                #cdo.outputtab('name,date,lon,lat,value', input = nc_sf , output = tmp)
                cmd = 'cdo outputtab,name,date,lon,lat,value %s > %s' % (nc_sf, tmp)
                os.system(cmd)
                print ('tanslation to ascii done')
                remove_rows(tmp, asc_sf)
                remove(tmp)
                print ('rows with missing Values removed')
              else:
                print 'ascii file allready exists'
              plot_ascii(asc_sf)
            except Exception as e: 
              print 'failed for ascii file: %s %s ' % (name, e)
              if os.path.exists(tmp):
                remove(tmp)
          else: 
            print 'NO EQUATION found for %s %s ' % (cult, climat)
        except Exception as e: 
          print 'Segetal flora failed: %s' % (e)  
  return outputs
