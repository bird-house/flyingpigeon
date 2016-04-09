import os
import tarfile

from flyingpigeon.subset import clipping
from flyingpigeon.subset import countries, countries_longname
        
from pywps.Process import WPSProcess

import logging

class ClippingProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(
            self, 
            identifier = "subset_countries",
            title="Subset netCDF files",
            version = "0.3",
            abstract="This process returns only the given polygon from input netCDF files.",
            statusSupported=True,
            storeSupported=True
            )

        self.resource = self.addComplexInput(
            identifier="resource",
            title="Resource",
            abstract="NetCDF File",
            minOccurs=1,
            maxOccurs=1000,
            maxmegabites=5000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )
       
        self.region = self.addLiteralInput(
            identifier="region",
            title="Region",
            #abstract= countries_longname(), # need to handle special non-ascii char in countries.
            default='DEU',
            type=type(''),
            minOccurs=1,
            maxOccurs=len(countries()),
            allowedValues=countries() #REGION_EUROPE #COUNTRIES # 
            )


        self.dimension_map = self.addLiteralInput(
            identifier="dimension_map",
            title="Dimension Map",
            abstract= 'if not ordered in lon/lat a dimension map has to be provided',
            type=type(''),
            minOccurs=0,
            maxOccurs=1
            )

        self.variable = self.addLiteralInput(
            identifier="variable",
            title="Variable",
            abstract="Variable to be expected in the input files (Variable will be detected if not set, )",
            default=None,
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
            )

        self.mosaik = self.addLiteralInput(
            identifier="mosaik",
            title="Mosaik",
            abstract="If Mosaik is checked, selected polygons will be merged to one Mosaik for each input file",
            default=False,
            type=type(False),
            minOccurs=0,
            maxOccurs=1,
            )

        self.output = self.addComplexOutput(
            title="Subsets",
            abstract="Tar archive containing the netCDF files",
            formats=[{"mimeType":"application/x-tar"}],
            asReference=True,
            identifier="output",
            )

    def execute(self):
        from ast import literal_eval

        urls = self.getInputValues(identifier='resource')
        mosaik = self.mosaik.getValue()
        regions = self.region.getValue()
        variable = self.variable.getValue()

        dimension_map = self.dimension_map.getValue()
        if dimension_map != None: 
            dimension_map = literal_eval(dimension_map)


        logging.debug('urls = %s', urls)
        logging.debug('regions = %s', regions)
        logging.debug('mosaik = %s', mosaik)
        logging.debug('dimension_map = %s', dimension_map)
    
        self.status.set('Arguments set for subset process', 0)

        logging.debug('starting: regions=%s, num_files=%s' % (len(regions), len(urls)))

        try:
            results = clipping(
                resource = urls,
                polygons = regions, # self.region.getValue(),
                mosaik = mosaik,
                variable = variable, 
                dir_output = os.path.abspath(os.curdir),
                dimension_map=dimension_map,
                )

        except Exception as e:
            logging.exception('clipping failed')
            self.status.set('clipping failed')
        # prepare tar file 
        try: 
            tarf = 'out.tar'
            tar = tarfile.open(tarf, "w")

            for result in results: 
                tar.add( result , arcname = result.replace(os.path.abspath(os.path.curdir), ""))
            tar.close()

            logging.info('Tar file prepared')
        except Exception as e:
            logging.exception('Tar file preparation failed')
            raise

        self.output.setValue( tarf )
        self.status.set('done', 100)
