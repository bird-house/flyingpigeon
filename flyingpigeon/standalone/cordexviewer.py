''' preparing netCDF files for Cordex viewer'''
from datetime import datetime, timedelta
import tempfile

import ocgis 
from ocgis.util.shp_process import ShpProcess
from ocgis.util.shp_cabinet import ShpCabinetIterator
from ocgis.util.helpers import get_sorted_uris_by_time_dimension

from cdo import *
cdo = Cdo()

def file_sorter(ncs):
  ndic = {}
  for nc in ncs:
    #logger.debug('file: %s' % nc)
    p, f = os.path.split(nc) 
    n = f.split('_')
    bn = '_'.join(n[0:-1])
    if n[3] != 'historical':
      ndic[bn] = []      
  for key in ndic:
    historical = key.replace('rcp26','historical').replace('rcp45','historical').replace('rcp85','historical')
    for n in ncs:
      if key in n or historical in n: 
        ndic[key].append(n)     
#  logger.debug('Data Experiment dictionary build: %i experiments found' % (len(ndic.keys())))
#  logger.debug('experiments: %s ' % (ndic.keys()))
  return ndic

try:
  data_basedir = '/media/nils/Iomega HDD/data/'
  normalized_dir = '/media/nils/Iomega HDD/data/normalized/'
  polygon_dir = '/media/nils/Iomega HDD/data/polygon/'
  icclim = '/media/nils/Iomega HDD/data/icclim'
  # logger.debug('starting cv_creator')
  # preparing the working directory 
  ocgis.env.OVERWRITE = True
  ocgis.env.DIR_DATA = icclim
  #ocgis.env.DIR_OUTPUT = polygons    
  output_crs = None
  
#  p_dir, p = os.path.split(os.path.abspath(__file__)) 
  SHP_DIR =  '/home/nils/birdhouse/flyingpigeon/flyingpigeon/processes/shapefiles/'
  #logger.debug('SHP_DIR: %s' % SHP_DIR )
  europa = ['AUT','BEL','BGR','CYP','CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR','GRC','HUN','HRV','IRL','ITA','LVA','LTU','LUX','MLT','NLD','POL','PRT','ROU','SVK','SVN','SWE','NOR','CHE','ISL','MKD','MNE','SRB','MDA','UKR','BIH','ALB','BLR','KOS']
  geoms = '50m_country' # 'world_countries_boundary_file_world_2002'
  ocgis.env.DIR_SHPCABINET = SHP_DIR 
  ocgis.env.OVERWRITE = True
  sc = ocgis.ShpCabinet()
  sci = ShpCabinetIterator(geoms)

  # ref_time = [datetime(1971,01,01),datetime(2000,12,31)]
  ncs = [os.path.join(icclim, f) for f in os.listdir(icclim)]
  print 'preparation Workdir for clipping done!'
  print '%i files found in DATA_DIR' % (len(ncs))
  
  exp = file_sorter(ncs)
  print 'Dictionary build with %i Experiments' % (len(exp.keys()))
#  print exp.keys()

except Exception as e:
  msg = 'Clipping preparation failed: %s ' % (e)
  print msg
  
c = 0  
for key in exp.keys():
  try:
    c = c + 1
    ncs = exp[key]
    # ncs = get_sorted_uris_by_time_dimension(nc)
    ncs.sort()
    var = key.split('_')[0]
    scenario = key.split('_')[3]
    rd = ocgis.RequestDataset(ncs, var) # 
    time_range=[datetime(1971,01,01) , datetime(2000,12,31)]
    
    print 'preperaiton for %s' % (key)
  except Exception as e:
    msg = 'sorting preparation failed for : %s %s ' % ( key , e)
    # logger.error(msg)
    # outlog = outlog + msg + '\n'
    print msg
  
  for land in europa:
    try:
      select_ugid = []
      geom_rows = []
      for row in sci:
        if row['properties']['adm0_a3'] == land:
          select_ugid.append(row['properties']['UGID'])
          geom_rows.append(row)
          
      # select_ugid.sort()
      if not os.path.exists(os.path.join(polygon_dir, var , scenario, land)):
        os.makedirs(os.path.join(polygon_dir, var , scenario, land))
      OUT_DIR = os.path.join(polygon_dir, var , scenario, land)
      
      # dir_output = tempfile.mkdtemp()
      ocgis.env.DIR_OUTPUT = OUT_DIR
      prefix = key.replace('EUR',land).strip('_mon')
      print 'done for %s ' % (prefix)
    
    except Exception as e:
      msg = 'processing failed for file  : %s ' % ( e)
      print msg 
      
    try:
      # make temofiles 
      
      if var == 'RR' or var == 'R20mm' or var == 'SU': # CDD  CSU  ID  TX  TXx
        calc = [{'func':'sum'}] 
        calc_grouping = ['year']
      elif var == 'TXx' or var == 'RX5day':
        calc = [{'func':'max'}] 
        calc_grouping = ['year']
      else :
        calc = [{'func':'mean'}] 
        calc_grouping = ['year']
              
      #temp_id1, nc = tempfile.mkstemp()
      #p, temp_nc = os.path.split(nc)
      #temp_id2, ref = tempfile.mkstemp()
      #p, temp_ref = os.path.split(ref)

      geom_nc = ocgis.OcgOperations(dataset=rd, geom=geoms, dir_output=OUT_DIR, output_format='nc', select_ugid=select_ugid, prefix=prefix, add_auxiliary_files=False ).execute()

      geom_ref = ocgis.OcgOperations(dataset=rd, geom=geoms, dir_output=OUT_DIR, output_format='nc', select_ugid=select_ugid, prefix=prefix, add_auxiliary_files=False, calc=calc, calc_grouping=calc_grouping , time_range=time_range  ).execute()
      
      #os.remove(geom_nc)
      #os.remove(geom_ref)
      
      print 'normalized fieldmean for key %s done! ' % ( key )
    except Exception as e:
      msg = 'normalized fieldmean failed for key  : %s %s ' % ( key , e)
      print msg
      
      
      #outlog = outlog + ('calculation of polygon %s with variable %s ... done \n'% (prefix , var))
      
      #if normalizer == True:
        #try: 
          #if not os.path.exists(os.path.join(os.curdir + '/normalized/', var , land)):
            #os.makedirs(os.path.join(os.curdir + '/normalized/', var , land))
          #dir_output = os.path.join(os.curdir + '/normalized/', var , land)
          
          #
          #tmp1 =  '%s/nc_temp_%s.nc' % (os.curdir, prefix )
          #tmp2 =  '%s/nc_temp2_%s.nc' % (os.curdir, prefix )
          #result =  '%s/%s.nc' % (dir_output, prefix )
          #input1 = '%s' % (geom_nc)
          #input2 = '%s' % (geom_ref)
          #input3 = ' %s %s ' % (tmp1 , tmp2)

          #cdo.fldmean (input = input1 , output = tmp1)
          #cdo.fldmean (input = input2 , output = tmp2 )
          #cdo.sub(input = input3 , output = result)
          #outlog = outlog + ('normalized fieldmean for polygon %s with variable %s ... done \n'% (prefix , var))
        #except Exception as e:
          #msg = 'normalized fieldmean failed for file  : %s %s ' % ( prefix , e)
          #logger.error(msg)
          #outlog = outlog + ('normalized fieldmean failed for file %s ! %s ... failed !!! \n'% (prefix , e))

      #logger.debug('calculation of file %s with variable %s in %s ... done'% (prefix,var, land))
      #monitor('Timeserie %i/%i for polygon: %s' % (c, len( exp.keys()), land) , (100/len( exp.keys() ) * c ))

#      logger.error(msg)
#      outlog = outlog + ('failed for polygon %s ! %s ... failed !!! \n'% (prefix , e))
    
##outlog = outlog + "Finish the Cordex Viwer preparation at : %s \n" % (datetime.strftime(datetime.now(), '%H:%M:%S %d-%m-%Y'))



  

## clipping 
    


##def cv_creator(icclim, polygons , domain, normalizer, monitor=dummy_monitor ):
  
  ##monitor('monitor: starting Cordex Viewer preparation ' , 6)
  
  ##outlog = "Starting the Cordex Viwer preparation at : %s \n" % (datetime.strftime(datetime.now(), '%H:%M:%S %d-%m-%Y'))
  ##outlog = outlog + "Domain = %s \n" % (domain)
  ##outlog = outlog + "normalizer = %s \n" % (normalizer)
  
     
  ## cdo = Cdo()

    
    ##if any("_rcp" in nc for nc in ncs):
      ##exp = fn_sorter_ch(ncs) # dictionary with experiment : files
      ##outlog = outlog + ('dictionary build with %i concatinated experiments of %i files.\n'% (len(exp.keys()), len(ncs)))
    ##else:
      ##exp = fn_sorter(ncs) # dictionary with experiment : files
      ##outlog = outlog + ('dictionary build with %i seperate experiments of %i files.\n'% (len(exp.keys()), len(ncs)))

 
  ##return outlog;