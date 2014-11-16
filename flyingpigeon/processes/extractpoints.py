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
            #extra_metadata={
                #'esgfilter': 'variable:tas,variable:evspsbl,variable:hurs,variable:pr',  #institute:MPI-M, ,time_frequency:day
                #'esgquery': 'variable:tas AND variable:evspsbl AND variable:hurs AND variable:pr' # institute:MPI-M AND time_frequency:day 
                #},
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
    
        #self.logout = self.addComplexOutput(
            #identifier="logout",
            #title="Logfile",
            #abstract="Logfile for user specific Information",
            #formats=[{"mimeType":"text/txt"}],
            #asReference=True,
            #)
        
        self.tarout = self.addComplexOutput(
            identifier="tarout",
            title="Tarfile",
            abstract="tar archive containing the value tables",
            formats=[{"mimeType":"application/x-tar"}],
            asReference=True,
            )
        
        #self.plotout = self.addComplexOutput(
            #identifier="tsplot",
            #title="Time Series Plot",
            #abstract="Visualisation of the extracted values",
            #formats=[{"mimeType":"application/html"}],
            #asReference=True,
            #)
            
    def execute(self):
        import ocgis
        from shapely.geometry import Point
        
        import tempfile
        import tarfile
        import os 
        import subprocess
        
        from bokeh.plotting import *
        import pandas as pd 
        from pandas import DataFrame, read_csv
        import numpy as np
        
        # define logfile 
        # logout_file = self.mktempfile(suffix='.txt')
        
        plotout_file = tempfile.mktemp(suffix='.html')
        # definfe bokeh output plot 
        output_file(plotout_file)
        save()
        hold()
        figure(x_axis_type = "datetime", tools="pan,wheel_zoom,box_zoom,reset,previewsave")
        self.show_status('output_file created: %s' % (plotout_file) , 5)
        
        logger.debug('Initialise extractpoints ... done')

        # get the vaulues of WPS delivered arguments
        ncfiles = self.getInputValues(identifier='netcdf_file')
        coords = self.coords.getValue()
        
        geom = []
        for ugid, p in enumerate(coords, start=1):
            self.show_status('processing point : %s'  % (p) , 20)
            p = p.split(',')
            self.show_status('splited x and y coord : %s'  % (p) , 20)
            point = Point(float(p[0]), float(p[1]))
            geom.append({'geom': point, 'properties': {'UGID': ugid}})
      
        ocgis.env.OVERWRITE = True
        ocgis.env.DIR_OUTPUT = self.working_dir
      
        (fp_tar, tarout_file) = tempfile.mkstemp(dir=".", suffix='.tar')
        tar = tarfile.open(tarout_file, "w")
        csvfiles = []
        
        self.show_status('ncfiles and coords : %s , %s ' % (ncfiles, coords), 7)

        for nc in ncfiles:
          if  (self.type_nc.getValue() == True ): 
            try:
              self.show_status('processing file: %s '  % (nc) , 15)
              (fp_csv, nc_temp) = tempfile.mkstemp(dir=".", suffix=".nc") 
              rd = ocgis.RequestDataset(uri=nc)
              ops = ocgis.OcgOperations(dataset=rd, geom=geom, prefix=nc_temp.strip('.nc'), select_nearest=False, output_format='nc')
              ret = ops.execute()
                  
              if os.path.isfile(nc_temp):
                tar.add(nc_temp, arcname = nc_temp.replace(self.working_dir, ""))
                self.show_status('ocgis process done for file : %s '  % (nc_temp) , 15)
              else:
                self.show_status('file : %s not found after ocgis processing'  % (nc_temp) , 15)  
            except Exception as e: 
              self.show_status('failed for file : %s  \n %s '  % (nc, e ) , 15)

          if  (self.type_csv.getValue() == True ): 
            try: 
                (fp_csv, csv_temp) = tempfile.mkstemp(dir=".", suffix=".csv") 
                # csvout_file = tempfile.mktemp(suffix='.csv')
                
                self.show_status('processing files: %s, CSVfile : %s '  % (nc, csv_temp) , 15)
                coordsFrame = DataFrame()
                coordsFrame.index.name = 'date'
                
                for p in coords :
                    self.show_status('processing point : %s'  % (p) , 20)
                    p = p.split(',')
                    self.show_status('splited x and y coord : %s'  % (p) , 20)
                    point = Point(float(p[0]), float(p[1]))
                    
                    rd = ocgis.RequestDataset(uri=nc)
                    ops = ocgis.OcgOperations(dataset=rd, geom=point, select_nearest=True, output_format='csv')
                    ret = ops.execute()
                    
                    self.show_status('file : %s successfully ocgis procesed for %s  variable'  % (csv_temp, rd.variable) , 15)
                    
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
                self.show_status('file : %s successfully pandas procesed'  % (csv_temp) , 15)
                
                if os.path.isfile(csv_temp):
                    tar.add(csv_temp, arcname = csv_temp.replace(self.working_dir, "")) # csvfiles.append( csvout_file )
                    self.show_status('file : %s successfully added to tarfile'  % (csv_temp) , 15)
                else:
                    self.show_status('file : %s not found after pandas processing'  % (csv_temp) , 15)
            except Exception as e: 
                self.show_status('failed for file : %s  \n %s '  % (nc, e ) , 15)
                
        tar.close()
        self.tarout.setValue( tarout_file )