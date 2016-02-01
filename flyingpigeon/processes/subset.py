from pywps.Process import WPSProcess

import logging

from flyingpigeon.subset import countries, countries_longname

class Clipping(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(
            self, 
            identifier = "subset_countries",
            title="Subset netCDF files",
            version = "0.1",
            metadata=[],
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
            default='FRA',
            type=type(''),
            minOccurs=1,
            maxOccurs=len(countries()),
            allowedValues=countries() #REGION_EUROPE #COUNTRIES # 
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
        from flyingpigeon.subset import clipping
        import tarfile
        from tempfile import mkstemp
        from os import path

        urls = self.getInputValues(identifier='resource')
        mosaik = self.mosaik.getValue()
        regions = self.region.getValue()

        logging.debug('urls = %s', urls)
        logging.debug('regions = %s', regions)
        logging.debug('mosaik = %s', mosaik)
    
        # self.status.set(self, 'Arguments set for subset process'  , 0)

        logging.debug('starting: regions=%s, num_files=%s' % (len(regions), len(urls)))

        results = clipping(
            resource = urls,
            polygons = regions, # self.region.getValue(),
            mosaik = mosaik,
            dir_output = path.abspath(path.curdir), #self.working_dir,
            )

        # prepare tar file 
        try: 
            (fp_tarf, tarf) = mkstemp(dir=".", suffix='.tar')
            tar = tarfile.open(tarf, "w")

            for result in results: 
                tar.add( result , arcname = result.replace(path.abspath(path.curdir), ""))
            tar.close()

            logging.info('Tar file prepared')
        except Exception as e:
            logging.exception('Tar file preparation failed %s' % e)

        self.output.setValue( tarf )
        # self.status.set(self, 'done: region=%s, num_files=%s' % (self.region.getValue(), len(urls)), 100)
