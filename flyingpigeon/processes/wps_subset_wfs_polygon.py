import tempfile
from pathlib import Path

from pywps import Process, LiteralInput, FORMATS
from pywps.inout.outputs import MetaFile, MetaLink4

from .subset_base import Subsetter, resource, variable, start, end, output, metalink

import ocgis
import ocgis.exc


class SubsetWFSPolygonProcess(Process, Subsetter):
    """Subset a NetCDF file using WFS geometry."""

    def __init__(self):
        inputs = [
            resource,
            LiteralInput('typename',
                         'TypeName',
                         abstract='Name of the layer in GeoServer.',
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1),
            LiteralInput('featureids',
                         'Feature Ids',
                         abstract='fid(s) of the feature in the layer.',
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1000),
            LiteralInput('geoserver',
                         'Geoserver',
                         abstract=('Typically of the form '
                                   'http://host:port/geoserver/wfs'),
                         data_type='string',
                         min_occurs=0),
            LiteralInput('mosaic',
                         'Union of Feature Ids',
                         abstract=('If True, selected regions will be '
                                   'merged into a single geometry.'),
                         data_type='boolean',
                         min_occurs=0,
                         default=False),
            start, end, variable]

        outputs = [output, metalink]

        super(SubsetWFSPolygonProcess, self).__init__(
            self._handler,
            identifier='subset-wfs-polygon',
            title='Subset',
            version='0.2',
            abstract=('Return the data for which grid cells intersect the '
                      'selected polygon for each input dataset as well as'
                      'the time range selected.'),
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):

        geoms = self.parse_feature(request)
        dr = self.parse_daterange(request)

        ml = MetaLink4('subset', workdir=self.workdir)

        for res in self.parse_resources(request):
            variables = self.parse_variable(request, res)

            for geom in geoms:
                prefix = Path(res).stem
                if 'featuresids' in request.inputs:
                    prefix += "_feature"

                rd = ocgis.RequestDataset(res, variables)

                try:
                    ops = ocgis.OcgOperations(
                        dataset=rd, geom=geom['geom'],
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


# print(etree.tostring(etree.fromstring(s.encode()), pretty_print=True).decode())
