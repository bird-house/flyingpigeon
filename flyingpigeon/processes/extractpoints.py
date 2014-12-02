from malleefowl import wpslogging as logging
from malleefowl.process import WPSProcess

# initialise
logger = logging.getLogger(__name__)

class extractpointsProcess(WPSProcess):
  def __init__(self):
    # definition of this process
    WPSProcess.__init__(self, 
      identifier = "extractpoints",
      title="Extract Coordinate Points",
      version = "0.1",
      metadata= [
              {"title": "Institut Pierre Simon Laplace", "href": "https://www.ipsl.fr/en/"}
              ],
      abstract="Extract Timeseries for specified coordinates from grid data",
      )

    self.netcdf_file = self.addComplexInput(
      identifier="netcdf_file",
      title="NetCDF File",
      abstract="NetCDF File",
      minOccurs=1,
      maxOccurs=1000,
      maxmegabites=5000,
      formats=[{"mimeType":"application/x-netcdf"}],
      )

    self.coords = self.addLiteralInput(
      identifier="coords",
      title="Coordinates",
      abstract="a comma seperated touple of WGS85 lon,lat decimal coorinate",
      default="2.356138, 48.846450",
      type=type(''),
      minOccurs=1,
      maxOccurs=100,
      )
    
    self.type_nc = self.addLiteralInput(
      identifier="type_nc",
      title="netCDF",
      abstract="Output files in netCDF format",
      type=type(False),
      default=True,
      minOccurs=1,
      maxOccurs=1,
      )
    
    self.type_csv = self.addLiteralInput(
      identifier="type_csv",
      title="CSV",
      abstract="Output  files as tables with comma seperated values",
      type=type(False),
      default=False,
      minOccurs=1,
      maxOccurs=1,
      )
  
    self.tarout = self.addComplexOutput(
      identifier="tarout",
      title="Tarfile",
      abstract="tar archive containing the value tables",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      )
          
  def execute(self):
    import ocgis
    from shapely.geometry import Point
    
    import tempfile
    import tarfile
    import os 
    import subprocess
    
    import tools
    
    import pandas as pd 
    from pandas import DataFrame, read_csv
    import numpy as np
    
    logger.debug('Initialise extractpoints ... done')

    # get the vaulues of WPS delivered arguments
    ncs = self.getInputValues(identifier='netcdf_file')
    coords = self.coords.getValue()
    os.mkdir(os.path.curdir+'/out_dir/')
    out_dir = os.path.curdir+'/out_dir/' 
    ncs_rn = tools.fn_creator(ncs)
    nc_exp = tools.fn_sorter(ncs_rn) # dictionary {experiment:[files]}
    
    logger.debug('working dir initialised  %s ' % out_dir )
    
    geom = []
    for ugid, p in enumerate(coords, start=1):
        self.show_status('processing point : %s'  % (p) , 20)
        p = p.split(',')
        self.show_status('splited x and y coord : %s'  % (p) , 20)
        point = Point(float(p[0]), float(p[1]))
        geom.append({'geom': point, 'properties': {'UGID': ugid}})
  
    ocgis.env.OVERWRITE = True
    ocgis.env.DIR_OUTPUT = out_dir
  
    (fp_tar, tarout_file) = tempfile.mkstemp(dir=".", suffix='.tar')
    tar = tarfile.open(tarout_file, "w")
    
    csvfiles = []
    
    self.show_status('coordinates : %s ' % ( coords), 7)

    for key in nc_exp:
      
      logger.debug('start calculation for %s ' % key )
      ncs = nc_exp[key]
      ncs.sort()
      
      
      var = key.split('_')[0]
      rd = ocgis.RequestDataset(ncs, var) # time_range=[dt1, dt2]
      logger.debug('calculation of experimtent %s with variable %s'% (key,var))

      if  (self.type_nc.getValue() == True ): 
        try:
          self.show_status('processing experiment: %s '  % (key) , 15)
          # (fp_csv, nc_temp) = tempfile.mkstemp(dir=".", suffix=".nc") 
          # rd = ocgis.RequestDataset(uri=nc)
          ops = ocgis.OcgOperations(dataset=rd, geom=geom, prefix=key, select_nearest=False, output_format='nc', add_auxiliary_files=False)
          ret = ops.execute()
        except Exception as e: 
          self.show_status('failed for experiment : %s  \n %s '  % (key, e ) , 15)

      if  (self.type_csv.getValue() == True ): 
        try: 
          (fp_csv, csv_temp) = tempfile.mkstemp(dir=out_dir, suffix=".csv")
          
          # csvout_file = tempfile.mktemp(suffix='.csv')
          
          self.show_status('processing files: %s, CSVfile :'  % (key) , 15)
          coordsFrame = DataFrame()
          coordsFrame.index.name = 'date'
          
          for p in coords :
            self.show_status('processing point : %s'  % (p) , 20)
            p = p.split(',')
            self.show_status('splited x and y coord : %s'  % (p) , 20)
            point = Point(float(p[0]), float(p[1]))
            
            #rd = ocgis.RequestDataset(uri=nc)
            ops = ocgis.OcgOperations(dataset=rd, geom=point, select_nearest=True, output_format='numpy')
            ret = ops.execute()
            
            self.show_status('file : %s.csv successfully ocgis procesed.'  % ( key ) , 15)
            
            # pandas conversion 
            field_dict = ret[1]
            field  = field_dict[rd.variable]
            self.show_status('values in ocgis array', 15)
            var = field.variables[rd.variable]
            
            var_value = np.squeeze(var.value.data)
            self.show_status('values in numpy array', 15)
            col_name = 'Point_%s_%s' % (point.x , point.y)
            pointFrame = DataFrame(columns = [col_name] , index = field.temporal.value_datetime )
            
            self.show_status('pandas Dataframe initialised ', 15)
            
            pointFrame[col_name] = var_value
            pointFrame.index.name = 'date'
            coordsFrame = pd.concat([coordsFrame,pointFrame], axis=1, ignore_index=False) #coordsFrame.append(pointFrame)
          coordsFrame.to_csv(csv_temp)
          os.rename(csv_temp , os.path.join(out_dir, key+'.csv')) 
          self.show_status('file : %s successfully pandas procesed: '  % (key+'.csv') , 15)
            
        except Exception as e: 
          self.show_status('failed for file : %s  \n %s '  % (key+'.csv', e ) , 15)
    
    if (len(os.listdir(out_dir)) > 0):
      tar.add(out_dir, arcname = out_dir.replace(self.working_dir, ""))
      self.show_status('ocgis folder tared with : %i '  % (len(os.listdir(out_dir))) , 15)
    else:
      self.show_status('ocgis folder contains NO files !!!')
      
    tar.close()
    
    self.tarout.setValue( tarout_file )