import tempfile
import os
from ocgis.util.shp_process import ShpProcess
from ocgis.util.shp_cabinet import ShpCabinetIterator
import ocgis
 
from flyingpigeon import subsetting as sb   
 
## requires commit: 
#https://github.com/NCPP/ocgis/commit/ecbecafce6b0ac2a7c207e797d3bccaf4e983b67
## processed this shapefile download (for Germany): http://imgbox.com/AooHIdDH
## unzipped the downloaded directory before proceeding
 
## path to directory containing shapefiles to add a ugid to
#SHP_DIR = '/home/local/WX/ben.koziol/Downloads/DEU_adm'
from os import environ
HOME = environ['HOME']

SHP_DIR = HOME+'/birdhouse/flyingpigeon/flyingpigeon/processes/shapefiles/'
 
## path to output directory
#OUT_DIR = tempfile.mkdtemp()
OUT_DIR = '/home/main/nils/Shapefiles/OcgisShape/'

EOBSopendub = 'http://opendap.knmi.nl/knmi/thredds/catalog/e-obs_0.22rotated/catalog.html?dataset=e-obs_0.22rotated/rr_0.22deg_rot_v11.0.nc'
 
EOBS =  HOME+'/data/EOBS/rr_0.44deg_rot_v11.0.nc'
 
#ocgis.env.DIR_SHPCABINET = '/home/main/nils/Shapefiles/DEU_adm/'
ocgis.env.DIR_SHPCABINET = SHP_DIR
ocgis.env.OVERWRITE = True  
sc = ocgis.ShpCabinet()
print sc.keys()
 
## there is only one geometry in this file...
geoms = 'country'
select_ugid = [147] 

dimension_map = sb.get_dimension_map(EOBS)
 
Mean_file = None
rd = ocgis.RequestDataset(EOBS, 'rr', dimension_map = dimension_map) # time_range=[dt1, dt2]
group = ['year']
#dir_output = tempfile.mkdtemp()
dir_output = None

calc = 'pr=rr/84600'# 
pr_file = ocgis.OcgOperations(dataset=rd, calc=calc, geom=geoms, prefix=str('pr_geom'), \
  output_format='nc', select_ugid=select_ugid, dir_output=dir_output).execute() # 

print(pr_file)

calc = [{'func' : 'icclim_R10mm', 'name' : 'R10mm'}] #[{'func':'mean','name':'mean'}]
rd = ocgis.RequestDataset(pr_file, 'pr', dimension_map = dimension_map) 

geom_file = ocgis.OcgOperations(dataset=rd, calc=calc, calc_grouping=group, geom=geoms, prefix=str('geom_geom'), \
  output_format='nc', select_ugid=select_ugid, dir_output=dir_output).execute() 

print(geom_file)
