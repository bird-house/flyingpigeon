from malleefowl.process import WPSProcess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

class timeAggregation(WPSProcess):
    def __init__(self):
        # definition of this process
        WPSProcess.__init__(self, 
            identifier = "timeAggregation",
            title="aggregate the time frequency",
            version = "0.1",
            metadata= [ {"title": "LSCE" , "href": "http://www.lsce.ipsl.fr/"} ],
            abstract="Calculates the mean over a given time frequence for one input data experiment",
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
            allowedValues=GROUPING
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
        self.show_status('starting uncertainty process', 0)
    
        from flyingpigeon.timeAggregation import aggregatTime
        
        ncfiles = self.getInputValues(identifier='resource')
        grouping = self.getInputValues(identifier='grouping')
        calc = self.getInputValues(identifier='calculation')
        
        result = aggregatTime( resource=ncfiles, calc=calc, grouping=grouping )
        
        self.output.setValue( result )
            
        self.show_status('time frequency reduction done', 99)       
