import os
import tarfile
from tempfile import mkstemp
from os import path

from flyingpigeon.indices import indices, indices_description
from flyingpigeon.subset import countries, countries_longname
from flyingpigeon.utils import GROUPING

from pywps.Process import WPSProcess

import logging
logger = logging.getLogger(__name__)

class IndicesPercentileProcess(WPSProcess):
    """This process calculates climate indices for the given input netcdf files."""
    def __init__(self):
        WPSProcess.__init__(
            self, 
            identifier = "indices_percentile",
            title="Climate indices -- Percentile",
            version = "0.1",
            abstract="Climate indices based on one single input variable and the percentile of a reference period.",
            metadata = [
                {'title': 'Documentation', 'href': 'http://flyingpigeon.readthedocs.io/en/latest/descriptions/index.html#climate-indices'},
                {"title": "ICCLIM" , "href": "http://icclim.readthedocs.io/en/latest/"},
                {"title": "Percentile-based indices", "href": "http://flyingpigeon.readthedocs.io/en/latest/descriptions/indices.html#percentile-based-indices"},
                ],
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
            title="Index",
            abstract='Select an index',
            default='TG',
            type=type(''),
            minOccurs=1,
            maxOccurs=1, # len(indices()),
            allowedValues=['TG', 'TN', 'TX'], # indices()
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

        self.refperiod = self.addLiteralInput(
            identifier="refperiod",
            title="Reference Period",
            abstract="Time refperiod to retrieve the percentile level",
            default="19700101-20101231",
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
            )

        
        #self.refperiod = self.addLiteralInput(
            #identifier="refperiod",
            #title="Reference refperiod",
            #abstract="Reference refperiod for climate condition (all = entire timeserie)",
            #default=None,
            #type=type(''),
            #minOccurs=0,
            #maxOccurs=1,
            #allowedValues=['all','1951-1980', '1961-1990', '1971-2000','1981-2010']
            #)

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
        
        self.mosaic = self.addLiteralInput(
            identifier="mosaic",
            title="Mosaic",
            abstract="If Mosaic is checked, selected polygons be clipped as a mosaic for each input file",
            default=False,
            type=type(False),
            minOccurs=0,
            maxOccurs=1,
            )


        # complex output
        # -------------
        self.output = self.addComplexOutput(
            identifier="output",
            title="Index",
            abstract="Calculated index as NetCDF file",
            metadata=[],
            formats=[{"mimeType":"application/x-tar"}],
            asReference=True
            )

    def execute(self):
        ncs        = self.getInputValues(identifier='resource')
        indices    = self.indices.getValue()
        polygons   = self.polygons.getValue()
        percentile = int(self.percentile.getValue())
        groupings  = self.groupings.getValue()
        mosaic = self.mosaic.getValue()
        refperiod = self.refperiod.getValue()
       
        self.status.set('starting: indices=%s, refperiod=%s, groupings=%s, num_files=%s' % (indices, refperiod, groupings, len(ncs)), 0)
        from flyingpigeon.indices import calc_indice_percentile   
        results = calc_indice_percentile(
            resources = ncs,
            indices = indices,
            percentile = percentile,
            mosaic = mosaic,
            polygons = polygons,
            refperiod = refperiod,
            groupings = groupings,
            dir_output = path.curdir,
            )

        if not results:
            raise Exception("failed to produce results")
        
        self.status.set('num results %s' % len(results), 90)
        try: 
            (fp_tarf, tarf) = mkstemp(dir=".", suffix='.tar')
            tar = tarfile.open(tarf, "w")

            for result in results:
                tar.add( result , arcname=os.path.basename(result))
            tar.close()

            logging.info('Tar file prepared')
        except Exception as e:
            msg = "Tar file preparation failed."
            logging.exception(msg)
            raise Exception(msg)

        self.output.setValue( tarf )
        self.status.set('done: indice=%s, num_files=%s' % (indices, len(ncs)), 100)
