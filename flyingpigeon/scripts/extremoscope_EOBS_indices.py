#import tempfile
import os
import datetime
import ocgis 
from ocgis.util.shp_process import ShpProcess
from ocgis.util.shp_cabinet import ShpCabinetIterator
from flyingpigeon import subsetting as sb 

from cdo import *   # python version
cdo = Cdo()

# define the environment
if 'obelix' in os.uname()[1] or 'asterix' in os.uname()[1]:
    HOME = os.getenv('HOME')
    #OUT_PATH = '/home/estimr2/nhempelmann/data/extremoscop/'
    DIR_OUTPUT = '/home/estimr2/birdhouse/output/'
    CACHE = '/home/estimr2/birdhouse/cache/'
    DIR_DATA = '/home/estimr2/EUROCORDEX/EOBS_v11/'
else:
    HOME = os.getenv('HOME')
    DIR_OUTPUT = HOME+'/data/extremoscop/'
    DIR_DATA = HOME+'/data/EOBS/'


pr_files =     [os.path.join(DIR_DATA,nc) for nc in os.listdir(DIR_DATA) if 'pr_' in nc ]
tasmax_files = [os.path.join(DIR_DATA,nc) for nc in os.listdir(DIR_DATA) if 'tasmax_' in nc ]
tasmin_files = [os.path.join(DIR_DATA,nc) for nc in os.listdir(DIR_DATA) if 'tasmin_' in nc ]
tas_files =    [os.path.join(DIR_DATA,nc) for nc in os.listdir(DIR_DATA) if 'tas_' in nc ]

pr_files.sort()
tasmax_files.sort()
tasmin_files.sort()
tas_files.sort()


# define ocgis environment

ocgis.env.OVERWRITE = True
ocgis.env.USE_CFUNITS = True
SHP_DIR = os.path.join(HOME + '/birdhouse/flyingpigeon/flyingpigeon/processes/shapefiles/FRA/')
ocgis.env.DIR_SHPCABINET = SHP_DIR
ocgis.env.DIR_OUTPUT = DIR_OUTPUT
sc = ocgis.ShpCabinet()
geoms = 'regions-2016'
sci = ShpCabinetIterator(geoms)

# define ocgis environment

ocgis.env.DIR_OUTPUT = DIR_OUTPUT


#ref_19611900 = [datetime.datetime(1961,1,1),datetime.datetime(1990,12,31)]
#ref_19712000 = [datetime.datetime(1971,1,1),datetime.datetime(2000,12,31)]
#ref_19762005 = [datetime.datetime(1976,1,1),datetime.datetime(2005,12,31)]
vars = ['pr'] #'tasmax','tas','tasmin',
pr_indices = ['CDD','RR','CWD','R10mm','R20mm','RR1','RX1day','RX5day','SDII']
tasmax_indices = ['CSU','ID','SU','TX','TXx','TXn']
tas_indices = ['TG']
tasmin_indices = ['TN']


for var in vars: 
    url = 'http://opendap.knmi.nl/knmi/thredds/dodsC/e-obs_0.22rotated/%s_0.22deg_rot_v11.0.nc' % (var)
    dimension_map = sb.get_dimension_map(url)
    
    if var == 'pr': 
        indices = pr_indices
        ncs = pr_files
        var_in = 'pr'
        
    #for nc in ncs    
    for ugid in [1, 2, 3, 4, 5, 6, 7, 11,  13, 14, 15, 16, 17]: #  8, 9, 10, 12,
        calc = 'pr=rr/84600'#
        rd = ocgis.RequestDataset(url, var, dimension_map = dimension_map)
        in_file = ocgis.OcgOperations(dataset=rd, 
                                      calc=calc,
                                      geom=geoms,
                                      prefix=str('%s_%s_geom' %(var, ugid)),
                                      output_format='nc',
                                      select_ugid=[ugid], 
                                      dir_output=DIR_OUTPUT,
                                      add_auxiliary_files=False
                                      ).execute()
        print in_file
        for indice in indices:
            scenario = 'eobs'
            calc = [{'func': 'icclim_%s' % indice, 'name': indice}]
            calc_grouping = ['year'] # [[12, 1, 2], 'unique'
            # create foulder structure and define output dir:
            
            if not os.path.exists(os.path.join(DIR_OUTPUT , 'timeseries',  indice ,scenario , str(ugid))):
                os.makedirs(os.path.join(DIR_OUTPUT , 'timeseries', indice ,scenario , str(ugid)))
            DIR_TIMESERIES = os.path.join(DIR_OUTPUT , 'timeseries', indice ,scenario , str(ugid))
            
            if not os.path.exists(os.path.join(DIR_OUTPUT , 'polygons',  indice ,scenario, str(ugid))):
                os.makedirs(os.path.join(DIR_OUTPUT , 'polygons', indice ,scenario, str(ugid)))
            DIR_POLYGONS = os.path.join(DIR_OUTPUT , 'polygons', indice ,scenario, str(ugid))
            
            #output = os.path.join(DIR_OUTPUT,prefix+'.nc') # cdo output
            
                # if not os.path.exists(output):
            tmp_files = []    
            for nc in ncs: 
                try:
                    path, basename = os.path.split(nc)
                    prefix = basename.replace(var, indice).strip('.nc')
                    rd = ocgis.RequestDataset(nc, var_in , dimension_map = dimension_map)
                    # conform_units_to = "kg m-2 s-1",
            #        geom_nc = ocgis.OcgOperations(dataset=rd, geom=geoms, output_format='nc', 
             #                                 select_ugid = [ugid], prefix=prefix , 
             ##                                 add_auxiliary_files=False ,calc=calc, 
              #                                calc_grouping=calc_grouping).execute() #
               #     tmp_files.append(geom_nc)
            #        
                    print ugid, prefix
                except Exception as e:
                    print 'failed for file : %s \n %s ' % (prefix, e )
         #           
          #  try:
          #      name = '_'.join(os.path.basename(nc).replace('day', calc_grouping[0]).replace(var, indice).split('_')[:-1])+'_1950-2014.nc'
          #      cdo.mergetime(input=' '.join(tmp_files), output=os.path.join(DIR_POLYGONS,name))
          #      cdo.fldmean(input= os.path.join(DIR_POLYGONS,name), output= os.path.join(DIR_TIMESERIES,name))
          #      print '**** Succeed for %s UGID %s' %(name , ugid)
          #  except Exception as e:
          #      print 'failed for indice %s UID %s: %s' % ( indice, ugid, e )    
 