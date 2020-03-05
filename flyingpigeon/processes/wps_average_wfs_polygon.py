import tempfile
from pathlib import Path

from pywps import Process, FORMATS
from pywps.inout.outputs import MetaFile, MetaLink4

from .subset_base import Subsetter, resource, variable, start, end, output, metalink, typename, \
    featureids, geoserver, mosaic

import ocgis
import ocgis.exc


class AverageWFSPolygonProcess(Process, Subsetter):
    """Subset a NetCDF file using WFS geometry."""

    def __init__(self):
        inputs = [resource, typename, featureids, geoserver, mosaic, start, end, variable]
        outputs = [output, metalink]

        super(AverageWFSPolygonProcess, self).__init__(
            self._handler,
            identifier='average-wfs-polygon',
            title='Average over polygon',
            version='0.2',
            abstract=('Return the average of the data for which grid cells intersect the '
                      'selected polygon for each input dataset as well as'
                      'the time range selected.'),
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):

        # Gather geometries, aggregate if mosaic is True.
        geoms = self.parse_feature(request)
        dr = self.parse_daterange(request)

        ml = MetaLink4('subset', workdir=self.workdir)

        for res in self.parse_resources(request):
            variables = self.parse_variable(request, res)
            rd = ocgis.RequestDataset(res, variables)
            prefix = Path(res).stem

            try:
                ops = ocgis.OcgOperations(
                    dataset=rd, geom=geoms.values(),
                    spatial_operation='clip', aggregate=True,
                    time_range=dr, output_format='nc',
                    interpolate_spatial_bounds=True,
                    prefix=prefix, dir_output=tempfile.mkdtemp(dir=self.workdir))

                out = ops.execute()

                mf = MetaFile(prefix, fmt=FORMATS.NETCDF)
                mf.file = out
                ml.append(mf)

            except ocgis.exc.ExtentError:
                continue

        response.outputs['output'].file = ml.files[0].file
        response.outputs['metalink'].data = ml.xml
        response.update_status("Completed", 100)

        return response
