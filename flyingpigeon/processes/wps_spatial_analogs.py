"""
Processes for spatial analog calculation

Author: David Huard (huard.david@ouranos.ca),
        Nils Hempelmann (info@nilshempelmann.de)
"""
# import tarfile
# import os

from pywps.Process import WPSProcess
import logging
from os.path import basename
import numpy as np
from flyingpigeon import dist_diff as dd
# from flyingpigeon.utils import archive
import json, datetime as dt
import netCDF4 as nc
import ocgis
from ..ocgisDissimilarity import Dissimilarity

logger = logging.getLogger(__name__)

ocgis.FunctionRegistry.append(Dissimilarity)


class SpatialAnalogProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier="spatial_analog",
            title="Spatial Analog",
            version="0.9",
            # metadata= [
            #    {"title": "Documentation", "href": "http://flyingpigeon.readthedocs.io/en/latest/"},
            #   ],

            abstract="Spatial analogs based on the comparison of climate "
                     "indices. The algorithm compares the distribution of the "
                     "target indices with the distribution of spatially "
                     "distribution reference indices and returns a value  "
                     "measuring the dissimilarity between both distributions.",
            statusSupported=True,
            storeSupported=True
        )

        # Literal Input Data
        # ------------------

        self.resource = self.addComplexInput(
            identifier="resource",
            title="Reference NetCDF Dataset",
            abstract="NetCDF dataset storing the reference indices.",
            minOccurs=1,
            maxOccurs=10,
            maxmegabites=50000,
            formats=[{"mimeType": "application/x-netcdf"}],
        )

        self.target_nc = self.addComplexInput(
            identifier="target_nc",
            title="Target NetCDF dataset",
            abstract="Target NetCDF dataset storing the target indices over the target period.",
            minOccurs=1,
            maxOccurs=10,
            maxmegabites=50000,
            formats=[{"mimeType": "application/x-netcdf"}],
        )

        self.indices = self.addLiteralInput(
            identifier="indices",
            title="Indices",
            abstract="One or more climate indices to use for the comparison.",
            type=type(''),
            minOccurs=1,
            maxOccurs=10,
        )

        self.algo = self.addLiteralInput(
            identifier="algo",
            title="Algorithm",
            abstract="Name of algorithm to use to compute differences.",
            default="kldiv",
            allowedValues=['kldiv', ],
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
        )

        self.refrange = self.addLiteralInput(
            identifier="refrange",
            title="Reference period",
            abstract="Reference period (YYYY-MM-DD/YYYY-MM-DD) for climate conditions (defaults to entire timeseries).",
            default="",
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
        )

        self.targetrange = self.addLiteralInput(
            identifier="targetrange",
            title="Target period",
            abstract="Target period (YYYY-MM-DD/YYYY-MM-DD) for climate conditions (defaults to entire timeseries). Only applies to netCDF target files.",
            default="",
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
        )

        self.targetlocation = self.addLiteralInput(
            identifier="targetlocation",
            title="Target geographical coordinates",
            abstract="Geographical coordinates (lon,lat) of the target location.",
            default=""
        )

        self.archive_format = self.addLiteralInput(
            identifier="archive_format",
            title="Archive format",
            abstract="Result files will be compressed into archives. Choose an appropriate format.",
            default="tar",
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
            allowedValues=['zip', 'tar']
        )

        ###########
        ### OUTPUTS
        ###########
        # self.output = self.addComplexOutput(
        #    identifier="output_analogs",
        #    title="Spatial Analogs",
        #    abstract="Archive (tar/zip) containing calculated dissimilarity measure as netCDF files",
        #    formats=[{"mimeType":"application/x-tar"}, {"mimeType":"application/zip"}],
        #    asReference=True,
        #    )
        self.output = self.addLiteralOutput(
            identifier="output_test",
            title="Spatial Analogs",
            type=type(1.0),
        )

    def execute_ocgis(self):
        import ocgis

        urls = self.getInputValues(identifier='resource')
        target_nc = self.getInputValues(identifier='target_nc')
        indices = self.getInputValues(identifier='indices')
        algo = self.getInputValue(identifier='algo')
        refrange = self.getInputValue(identifier='refrange')
        targetrange = self.getInputValues(identifier='targetrange')
        archive_fmt = self.getInputValue(identifier='archive_format')

        logger.info('urls = {0}'.format(urls))
        logger.info('target = {0}'.format(target_nc or target_json))
        logger.info('indices = {0]'.format(indices))
        logger.info('algo = {0}'.format(algo))
        logger.info('refrange = {0}'.format(refrange))
        logger.info('targetrange = {0}'.format(targetrange))

        self.status.set('Arguments set for spatial analog process', 0)
        logger.debug('starting: num_files = {0}'.format(len(urls)))

        try:
            ref_rd = ocgis.RequestDataset(urls, variable=indices,
                                          alias='reference')
            tar_rd = ocgis.RequestDataset(target_nc, variable=indices,
                                          alias='target')
            rdc = ocgis.RequestDatasetCollection(ref_rd, tar_rd)
            results = spatial_analog_calc(
                resource=urls,
                variables=indices,
            )

        except Exception as e:
            msg = 'Spatial analog failed'
            logger.exception(msg)
            raise Exception(msg)

        if not results:
            raise Exception('no results produced')

        try:
            from flyingpigeon.utils import archive
            tarf = archive(results)
            logger.info('Tar file prepared')
        except Exception as e:
            msg = 'Tar file preparation failed'
            logger.exception(msg)
            raise Exception(msg)

        self.output.setValue(tarf)

        i = next((i for (i, x) in enumerate(results) if x), None)
        self.output_netcdf.setValue(results[i])

        self.status.set('done', 100)

    def execute_simple(self):

        self.status.set('Start process', 0)

        try:
            logger.info('reading the arguments')
            reference_nc = self.getInputValues(identifier='reference_nc')
            target_nc = self.getInputValues(identifier='target_nc')
            target_json = self.getInputValues(identifier='target_json')
            indices = self.getInputValues(identifier='indices')
            algo = self.getInputValue(identifier='algo')
            refrange = self.getInputValue(identifier='refrange')
            targetrange = self.getInputValues(identifier='targetrange')
            archive_fmt = self.getInputValue(identifier='archive_format')

        except Exception as e:
            logger.error('failed to read in the arguments %s ' % e)

        if target_nc is None or target_json is None:
            raise ValueError("Target data is missing.")

        # Load target variables
        target_data = {}
        if target_json is not None:
            for target in target_json:
                with open(target, 'r') as f:
                    data = json.load(f)
                    for var in indices:
                        target_data[var] = data[var]

        elif target_nc is not None:
            with nc.MFDataset(target_nc, 'r') as D:
                for var in indices:
                    # TODO : Implement targetrange
                    target_data[var] = D.variables[var][:]

        P = np.vstack(target_data[var] for var in indices).T

        # Load reference dataset
        ptime = lambda x: dt.datetime.strptime(x, "%Y-%m-%d")
        if refrange:
            refrange = map(ptime, refrange.split('-'))

        ref_data = {}
        with nc.MFDataset(reference_nc, 'r') as D:
            time = D.variables['time']
            if refrange:
                sl = slice(*nc.date2index(rrange, time, select='nearest'))
            else:
                sl = slice(None, None)

            for var in indices:
                assert D.variables[var].dimensions[0] == 'time'
                ref_data[var] = D.variables[var][sl]

            shape = set([ref_data[var].shape for var in indices])
            assert len(shape) == 1
            shape = shape.pop()

            out_arr = np.ones(shape[1:]) * -999
            I, J = np.indices(shape[1:])
            # TODO: parallelize
            for (i, j) in zip(I.flatten(), J.flatten()):
                Q = np.vstack([ref_data[var][:, i, j] for var in indices]).T
                if np.any(np.isnan(Q)):
                    continue

                if algo == 'kldiv':
                    out_arr[i, j] = dd.kldiv(P, Q)
                else:
                    raise NotImplementedError(algo)

            fn = 'output.nc'
            with nc.Dataset(fn, 'w') as D:
                D.setncattr('source', 'spatial_analog')
                # TODO: Copy dimensions and attributes

            D.createDimension('lat', )
            D.createVariable(algo, float, ('lat', 'lon'))

        # self.output.setValue(fn)

        self.output.setValue(ans)
        self.status.set('done', 100)