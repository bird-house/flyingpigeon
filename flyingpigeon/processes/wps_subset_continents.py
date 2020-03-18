import logging

from pywps import ComplexInput, Format, LiteralInput, Process, FORMATS
# from pywps import Process, LiteralInput
from pywps.app.Common import Metadata

from pywps.inout.outputs import MetaFile, MetaLink4
from .flyingpigeon.subset_base import output, metalink

from flyingpigeon.subset import _CONTINENTS_
from flyingpigeon.subset import clipping
from flyingpigeon.utils import extract_archive
# from flyingpigeon.utils import rename_complexinputs
from os.path import abspath, basename

LOGGER = logging.getLogger("PYWPS")


class SubsetcontinentProcess(Process):
    """
    TODO: opendap input support, additional metadata to display region names.
    """

    def __init__(self):
        inputs = [
            LiteralInput('region', 'Region',
                         data_type='string',
                         abstract="Continent name.",
                         min_occurs=1,
                         max_occurs=len(_CONTINENTS_),
                         default='Africa',
                         allowed_values=_CONTINENTS_),  # REGION_EUROPE #COUNTRIES

            LiteralInput('mosaic', 'Union of multiple regions',
                         data_type='boolean',
                         abstract="If True, selected regions will be merged"
                                  " into a single geometry.",
                         min_occurs=0,
                         max_occurs=1,
                         default=False),

            ComplexInput('resource', 'Resource',
                         abstract='NetCDF Files or archive (tar/zip) containing netCDF files.',
                         min_occurs=1,
                         max_occurs=1000,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),
        ]

        outputs = [output, metalink]

        super(SubsetcontinentProcess, self).__init__(
            self._handler,
            identifier="subset_continents",
            title="Subset Continents",
            version="0.11",
            abstract="Return the data whose grid cells intersect the selected continents for each input dataset.",
            metadata=[
                Metadata('Doc',
                         'https://flyingpigeon.readthedocs.io/en/latest/processes_des.html#subset-processes'),
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):

        # input files
        LOGGER.debug("url={}, mime_type={}".format(request.inputs['resource'][0].url,
                     request.inputs['resource'][0].data_format.mime_type))
        ncs = extract_archive(
            resources=[inpt.file for inpt in request.inputs['resource']],
            dir_output=self.workdir)
        # mime_type=request.inputs['resource'][0].data_format.mime_type)
        # mosaic option
        # TODO: fix defaults in pywps 4.x
        if 'mosaic' in request.inputs:
            mosaic = request.inputs['mosaic'][0].data
        else:
            mosaic = False
        # regions used for subsetting
        regions = [inp.data for inp in request.inputs['region']]

        LOGGER.info('ncs: {}'.format(ncs))
        LOGGER.info('regions: {}'.format(regions))
        LOGGER.info('mosaic: {}'.format(mosaic))
        LOGGER.info('flyingpigeon dir_output : {}'.format(abspath(self.workdir)))

        response.update_status("Arguments set for subset process", 0)
        LOGGER.debug('starting: regions={}, num_files={}'.format(len(regions), len(ncs)))

        try:
            ml = MetaLink4('subset', workdir=self.workdir)
            for nc in ncs:
                out = clipping(
                    resource=nc,
                    polygons=regions,
                    mosaic=mosaic,
                    spatial_wrapping='wrap',
                    # variable=variable,
                    dir_output=self.workdir,
                    # dimension_map=dimension_map,
                )
                LOGGER.info('result: {}'.format(out[0]))

                prefix = basename(nc).replace('.nc', '')
                mf = MetaFile(prefix, fmt=FORMATS.NETCDF)
                mf.file = out[0]
                ml.append(mf)

        except Exception as ex:
            msg = 'Clipping failed: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        response.outputs['output'].file = ml.files[0].file
        response.outputs['metalink'].data = ml.xml
        response.update_status("Completed", 100)

        return response
