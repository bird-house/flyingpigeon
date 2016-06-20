from pywps.Process import WPSProcess

from flyingpigeon.indices import indices, indices_description
from flyingpigeon.subset import countries, countries_longname
from flyingpigeon.utils import GROUPING

import logging

class IndicesPercentileProcess(WPSProcess):
    """This process calculates a climate indice for the given input netcdf files."""
    def __init__(self):
        WPSProcess.__init__(
            self, 
            identifier = "indices_percentile",
            title="Climate indices percentile based",
            version = "0.1",
            abstract="Calculation of climate indices based on one single input variable and based on percentils of a referece period.",
            statusSupported=True,
            storeSupported=True
            )

        self.resource = self.addComplexInput(
            identifier="resource",
            title="Resouce",
            abstract="NetCDF File",
            minOccurs=1,
            maxOccurs=100,
            maxmegabites=5000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )
    
        self.indices = self.addLiteralInput(
            identifier="indices",
            title="Indice",
            abstract='Select an indice',
            default='TG90p',
            type=type(''),
            minOccurs=1,
            maxOccurs=1, # len(indices()),
            allowedValues=['TG90p','TN90p'], # indices()
            )

        self.percentile = self.addLiteralInput(
            identifier="percentile",
            title="Percentile",
            abstract='Select an percentile',
            default=90,
            type=type('0'),
            minOccurs=1,
            maxOccurs=1, # len(indices()),
            allowedValues=range(1,100), # indices()
            )
        
        self.period = self.addLiteralInput(
            identifier="period",
            title="Reference period",
            abstract="Reference period for climate condition (all = entire timeserie)",
            default="all",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['all','1951-1980', '1961-1990', '1971-2000','1981-2010']
            )

        self.groupings = self.addLiteralInput(
            identifier="groupings",
            title="Grouping",
            abstract="Select an time grouping (time aggregation)",
            default='yr',
            type=type(''),
            minOccurs=1,
            maxOccurs=len(GROUPING),
            allowedValues=GROUPING
            )
        
        self.polygons = self.addLiteralInput(
            identifier="polygons",
            title="Country subset",
            abstract= countries_longname(), 
            default='DEU',
            type=type(''),
            minOccurs=0,
            maxOccurs=len(countries()),
            allowedValues=countries()
            )

        # complex output
        # -------------
        self.output = self.addComplexOutput(
            identifier="output",
            title="Indice",
            abstract="Calculated indice as NetCDF file",
            metadata=[],
            formats=[{"mimeType":"application/x-tar"}],
            asReference=True
            )

    def execute(self):
        from flyingpigeon.indices import calc_indice_percentile   
        import os
        import tarfile
        from tempfile import mkstemp
        from os import path
        
        ncs        = self.getInputValues(identifier='resource')
        indices    = self.indices.getValue()
        polygons   = self.polygons.getValue()
        percentile = int(self.percentile.getValue()[0])
        groupings  = self.groupings.getValue() # getInputValues(identifier='groupings')
        period     = self.period.getValue()
       
        self.status.set('starting: indices=%s, period=%s, groupings=%s, num_files=%s' % (indices, period, groupings, len(ncs)), 0)

        results = calc_indice_percentile(
            resources = ncs,
            indices = indices,
            percentile = percentile,
            polygons = polygons,
            period = period,
            groupings = groupings,
            dir_output = path.curdir,
            )
        
        self.status.set('result %s' % results, 90)
        try: 
            (fp_tarf, tarf) = mkstemp(dir=".", suffix='.tar')
            tar = tarfile.open(tarf, "w")

            for result in results:
                p , f = path.split(result) 
                tar.add( result , arcname = result.replace(p, ""))
            tar.close()

            logging.info('Tar file prepared')
        except Exception as e:
            logging.error('Tar file preparation failed %s' % e)

        self.output.setValue( tarf )
        self.status.set('done: indice=%s, num_files=%s' % (indices, len(ncs)), 100)
