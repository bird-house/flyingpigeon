from flyingpigeon.timeAggregation import aggregatTime

from pywps.Process import WPSProcess

import logging

class TimeAggregationProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(self, 
            identifier = "time_aggregation",
            title="aggregate the time frequency",
            version = "0.2",
            metadata= [ {"title": "LSCE" , "href": "http://www.lsce.ipsl.fr/"} ],
            abstract="Calculates the mean over a given time frequence for one input data experiment",
            statusSupported=True,
            storeSupported=True
            )

        # input arguments    
        self.resource = self.addComplexInput(
            identifier="resource",
            title="NetCDF Files",
            abstract="NetCDF Files",
            minOccurs=1,
            maxOccurs=100,
            maxmegabites=5000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )

        self.grouping = self.addLiteralInput(
            identifier="grouping",
            title="Grouping",
            abstract="Select an time grouping (time aggregation)",
            default='yr',
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            #allowedValues=GROUPING
            )

        self.calculation = self.addLiteralInput(
            identifier="calculation",
            title="Calculation method",
            abstract="Select an calculation method",
            default='mean',
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['mean','sum','max','min']
            )

        # output 
        self.output = self.addComplexOutput(
            identifier="output",
            title="output file",
            abstract="netCDF file containing calculated mean",
            formats=[{"mimeType":"application/netcdf"}],
            asReference=True,
            )         

    def execute(self):
        self.status.set('starting uncertainty process', 0)
            
        ncfiles = self.getInputValues(identifier='resource')
        grouping = self.grouping.getValue()
        calc = self.calculation.getValue()
        
        result = aggregatTime( resource=ncfiles, calculation=calc, grouping=grouping )
        
        self.output.setValue( result )
            
        self.status.set('time frequency reduction done', 100)       
