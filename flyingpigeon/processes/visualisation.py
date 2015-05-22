from datetime import datetime, date
import tempfile
import subprocess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

from netCDF4 import Dataset
import numpy as np

from bokeh.plotting import *
from malleefowl.process import WPSProcess

class VisualisationProcess(WPSProcess):
    def __init__(self):
        # definition of this process
        WPSProcess.__init__(self, 
            identifier = "visualisation",
            title="Visualisation of netcdf files",
            version = "0.1",
            metadata= [
                    {"title": "Climate Service Center", "href": "http://www.climate-service-center.de/"}
                    ],
            abstract="Just testing a nice script to visualise some variables",
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
            maxOccurs=100,
            maxmegabites=5000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )

        self.variableIn = self.addLiteralInput(
            identifier="variable",
            title="Variable",
            abstract="Variable to be expected in the input files",
            default="tas",
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
            )
    
        self.plotout_spagetti = self.addComplexOutput(
            identifier="plotout_spagetti",
            title="visualisation",
            abstract="Visualisation of single variables",
            formats=[{"mimeType":"application/html"}],
            asReference=True,
            )         
            
    def execute(self):
        
        from flyingpigeon import visualisation as vs
        
        ncfiles = self.getInputValues(identifier='netcdf_file')
        var = self.variableIn.getValue()

        self.show_status('Spagetti plot for %s %s files' % (len(ncfiles), var), 7)
        
        
        plotout_spagetti_file = vs.spaghetti(ncfiles , variable=var, title=var, dir_out=None)
        
        
        #import cdo 
        #cdo = cdo.Cdo()

        
        ## define bokeh 
        #plotout_spagetti_file("plotout_spagetti.html")
        #save()
        
        #self.show_status('plotout_spagetti_file created:' , 5)
        
        #hold()
        #figure(x_axis_type = "datetime", tools="pan,wheel_zoom,box_zoom,reset,previewsave")
    
        #for nc in ncfiles:
            
            #self.show_status('looping files : %s ' % (nc), 10)
            ## get timestapms
            #rawDate = cdo.showdate(input= nc) # ds.variables['time'][:]
            #strDate = [elem.strip().split('  ') for elem in rawDate]
            #dt = [datetime.strptime(elem, '%Y-%m-%d') for elem in strDate[0]]
            ## get vaules
            #ds=Dataset(nc)
            #data = np.squeeze(ds.variables[var][:])
            #meanData = np.mean(data,axis=1)
            #ts = np.mean(meanData,axis=1)
            
            ## plot into current figure
            #line( dt,ts ) # , legend= nc 
            #save()
        
        #self.show_status('timesseries lineplot done.' , 50)
        
        #legend().orientation = "bottom_left"
        #curplot().title = "Field mean of %s " % var  
        #grid().grid_line_alpha=0.3

        #window_size = 30
        #window = np.ones(window_size)/float(window_size)
        
        #save()
        #hold('off')

        #figure(x_axis_type = "datetime", tools="pan,wheel_zoom,box_zoom,reset,previewsave")
        #hold()
        #dates = set()
        
        ## get the dates
        #for nc in ncfiles:
        
            #self.show_status('looping files : %s ' % (nc), 55)
            ## get timestapms
            #rawDate = cdo.showdate(input= nc) # ds.variables['time'][:]
            #strDate = rawDate[0].split('  ')
        
            #self.show_status('len strDate : %s ' % (len(strDate)), 55)
            
            
            #dates = dates.union(strDate)
        
        ##self.show_status('dates : %s ' % len(dates), 62)
        #ldates = list(dates)
        #ldates.sort()
        #ddates = dict( (ldates[i], i) for i in range(0,len(ldates)))
        
        ##initialise matirx
        
        
        #ma = np.empty([len(ddates), len(ncfiles)])*np.nan
        ##self.show_status('ddates : %s ' % ddates, 62)
        
        ## fill matrix
        #for y in range(0,len(ncfiles)) : 
            #rawDate = cdo.showdate(input= ncfiles[y]) # ds.variables['time'][:]
            #strDate = rawDate[0].split('  ')

            #ds=Dataset(ncfiles[y])
            #data = np.squeeze(ds.variables[var][:])
            #meanData = np.mean(data,axis=1)
            #ts = np.mean(meanData,axis=1)
            #logger.debug('ts array  : %s ' % (len(ts)), 66)
            
            #for t in range(0, len(strDate)) : 
                #x = ddates.get(strDate[t],0)
                #ma[x,y] = ts[t]
            
        ## get datetimes
        #dt = [datetime.strptime(elem, '%Y-%m-%d') for elem in ldates]
        #mdat = np.ma.masked_array(ma ,np.isnan(ma))
        
        ##self.show_status('matrix masked %s ' % mdat , 80)
        ##logger.debug('matrix %s ', mdat.shape) 
        
        #ma_mean = np.mean(mdat,axis=1)
        #self.show_status('mean  %s '%  len(ma_mean) , 97 )
        #ma_min = np.min(mdat,axis=1)
        #ma_max = np.max(mdat,axis=1)
        ##ma_sub = np.subtract(ma_max, ma_min)
        ##ma_per75 = np.percentile(mdat,75, axis=0)
        ##ma_per25 = np.percentile(mdat,25, axis=0)
        #self.show_status('ma Vaules %s' % len(mdat.data) , 75)
        
        ##line(dt, ma_min , color='grey' ,line_width=1)
        ##line(dt, ma_max , color='grey' , line_width=2 )
        #line(dt, ma_mean , color='red', line_width=1)
        
        #x = []
        #y = []
        #x = np.append(dt,dt[::-1])
        #y = np.append(ma_min, ma_max[::-1])

        #patch(x,y, color='grey', alpha=0.8, line_color=None)

        #curplot().title = "Mean and Uncertainty of  %s " % var  
        #save()
        #hold('off')
        
        self.show_status('visualisation done', 99)
        self.plotout_spagetti.setValue( plotout_spagetti_file )

