from pywps import LiteralInput, ComplexInput, ComplexOutput
from pywps import FORMATS


resource = ComplexInput('resource',
                        'NetCDF resource',
                        abstract='NetCDF file or OPEnDAP url pointing to netCDF file.',
                        supported_formats=[FORMATS.NETCDF, FORMATS.DODS],
                        max_occurs=1000)

start = LiteralInput('start',
                     'Initial datetime',
                     abstract='Initial datetime for temporal subsetting.',
                     data_type='dateTime',
                     min_occurs=0,
                     max_occurs=1)

end = LiteralInput('end',
                   abstract='Final datetime for temporal subsetting.',
                   data_type='dateTime',
                   min_occurs=0,
                   max_occurs=1)

variable = LiteralInput('variable',
                        'Variable',
                        abstract=('Name of the variable in the NetCDF file.'
                                  'Will be guessed if not provided.'),
                        data_type='string',
                        min_occurs=0)

output = ComplexOutput('output',
                       'NetCDF output for first resource file.',
                       as_reference=True,
                       supported_formats=[FORMATS.NETCDF])

metalink = ComplexOutput('metalink',
                         'Metalink file with links to all NetCDF outputs.',
                         as_reference=True,
                         supported_formats=[FORMATS.META4])

typename = LiteralInput('typename',
                        'TypeName',
                        abstract='Name of the layer in GeoServer.',
                        data_type='string',
                        min_occurs=1,
                        max_occurs=1)

featureids = LiteralInput('featureids',
                          'Feature Ids',
                          abstract='fid(s) of the feature in the layer.',
                          data_type='string',
                          min_occurs=0,
                          max_occurs=1000)

geoserver = LiteralInput('geoserver',
                         'Geoserver',
                         abstract=('Typically of the form '
                                   'http://host:port/geoserver/wfs'
                                   'Will default to the geoserver URL configured for this server.'),
                         data_type='string',
                         min_occurs=0)

mosaic = LiteralInput('mosaic',
                      'Union of multiple regions',
                      abstract=('If True, selected regions will be '
                                'merged into a single geometry.'),
                      data_type='boolean',
                      min_occurs=0,
                      default=False)

shape = ComplexInput('shape', 'Vector Shape',
                     abstract='An ESRI Shapefile, GML, JSON, GeoJSON, or single layer GeoPackage.'
                              ' The ESRI Shapefile must be zipped and contain the .shp, .shx, and .dbf.',
                     min_occurs=1,
                     max_occurs=1,
                     supported_formats=[FORMATS.GEOJSON, FORMATS.GML, FORMATS.JSON, FORMATS.SHP])

