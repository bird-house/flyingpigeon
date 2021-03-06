import logging
import tempfile
from pathlib import Path

from pywps import Process, LiteralInput, FORMATS
from pywps.inout.outputs import MetaFile, MetaLink4

from flyingpigeon.subset_base import Subsetter
from flyingpigeon.processes.wpsio import resource, variable, start, end, output, metalink
from pywps.ext_autodoc import MetadataUrl

import ocgis.exc

LOGGER = logging.getLogger("PYWPS")


class SubsetBboxProcess(Subsetter, Process):
    """Subset a NetCDF file using bounding box geometry."""

    def __init__(self):
        inputs = [resource,
                  LiteralInput('lon0',
                               'Minimum longitude',
                               abstract='Minimum longitude.',
                               data_type='float'),
                  LiteralInput('lon1',
                               'Maximum longitude',
                               abstract='Maximum longitude.',
                               data_type='float'),
                  LiteralInput('lat0',
                               'Minimum latitude',
                               abstract='Minimum latitude.',
                               data_type='float'),
                  LiteralInput('lat1',
                               'Maximum latitude',
                               abstract='Maximum latitude.',
                               data_type='float'),
                  start, end, variable]

        outputs = [output, metalink]

        super(SubsetBboxProcess, self).__init__(
            self._handler,
            identifier='subset_bbox',
            title='Subset netCDF file on bounding box',
            version='0.2',
            abstract=('Return the data for which grid cells intersect the '
                      'bounding box for each input dataset as well as'
                      'the time range selected. This implies that the centroid of'
                      'a grid cell can be outside the bounding box.'),
            metadata=[
                MetadataUrl('Doc',
                            'https://flyingpigeon.readthedocs.io/en/latest/processes_des.html#subset-processes',
                            anonymous=True),
            ],

            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):

        geom = self.parse_bbox(request)
        dr = self.parse_daterange(request)

        ml = MetaLink4('subset', workdir=self.workdir)

        response.update_status('Start processing the bbox subset', 30)

        for res in self.parse_resources(request):
            variables = self.parse_variable(request, res)
            prefix = Path(res).stem + "_bbox_subset"
            rd = ocgis.RequestDataset(res, variables)

            try:
                ops = ocgis.OcgOperations(
                    dataset=rd, geom=geom, time_range=dr,
                    output_format='nc', spatial_wrapping="wrap",
                    interpolate_spatial_bounds=True,
                    prefix=prefix, dir_output=tempfile.mkdtemp(dir=self.workdir))
                out = ops.execute()

                mf = MetaFile(prefix, fmt=FORMATS.NETCDF)
                mf.file = out
                ml.append(mf)

            except ocgis.exc.ExtentError:
                continue
        response.update_status('Finished processing the bbox subset', 90)

        response.outputs['output'].file = ml.files[0].file
        response.outputs['metalink'].data = ml.xml
        response.update_status("Completed", 100)

        return response
