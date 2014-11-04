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
            minOccurs=0,
            maxOccurs=100,
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
        
        tarout_file = tempfile.mktemp(suffix='.tar')
        csvfiles = []
        
        self.show_status('ncfiles and coords : %s , %s ' % (ncfiles, coords), 7)
      
        for nc in ncfiles:
            try: 
                csvout_file = tempfile.mktemp(suffix='.csv')
                
                
                self.show_status('processing files: %s, CSVfile : %s '  % (nc, csvout_file) , 15)
                coordsFrame = DataFrame()
                coordsFrame.index.name = 'date'
                
                for p in coords :
                    self.show_status('processing point : %s'  % (p) , 20)
                    p = p.split(',')
                    self.show_status('splited x and y coord : %s'  % (p) , 20)
                    point = Point(float(p[0]), float(p[1]))
                    
                    rd = ocgis.RequestDataset(uri=nc)
                    ops = ocgis.OcgOperations(dataset=rd, geom=point, select_nearest=True, output_format='numpy')
                    ret = ops.execute()
                    
                    field_dict = ret[1]
                    field  = field_dict[rd.variable]
                    var = field.variables[rd.variable]
                    var_value = np.squeeze(var.value.data)
                    
                    col_name = 'Point_%s_%s' % (point.x , point.y)
                    
                    pointFrame = DataFrame(columns = [col_name] , index = field.temporal.value_datetime )
                    pointFrame[col_name] = var_value
                    pointFrame.index.name = 'date'
                    coordsFrame = pd.concat([coordsFrame,pointFrame], axis=1, ignore_index=False) #coordsFrame.append(pointFrame)
                coordsFrame.to_csv(csvout_file)
                if os.path.isfile(csvout_file):
                    csvfiles.append( csvout_file )
            except Exception as e: 
                self.show_status('failed for file : %s  \n %s '  % (nc, e ) , 15)
          
        tar = tarfile.open(tarout_file, "w")
        for name in csvfiles:
            tar.add(name, arcname = name.replace(self.working_dir, ""))
        tar.close()
        
      #self.logout.setValue( logout_file )
        self.tarout.setValue( tarout_file )

      
      #save()
      #hold('off')
      
      #self.show_status('visualisation done', 99)
      
      #self.plotout.setValue( plotout_file )

