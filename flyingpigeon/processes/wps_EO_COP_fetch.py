from pywps import Process
# from pywps import LiteralInput
from pywps import ComplexInput, LiteralInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata

from flyingpigeon.log import init_process_logger
from flyingpigeon.utils import rename_complexinputs

from datetime import datetime as dt
from datetime import timedelta, time
from tempfile import mkstemp

from sentinelsat import SentinelAPI, read_geojson, geojson_to_wkt

import logging
LOGGER = logging.getLogger("PYWPS")


class EO_COP_fetchProcess(Process):
    """
    TODO: like FetchProcess
    """
    def __init__(self):
        inputs = [
            LiteralInput("products", "Earth Observation Product Type",
                         abstract="Choose Earth Observation Products",
                         default='Sentinel-2',
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=['Sentinel-2']
                         ),

            LiteralInput('BBox', 'Bounding Box',
                         data_type='string',
                         abstract="Enter a bbox: min_lon, max_lon, min_lat, max_lat."
                                  " min_lon=Western longitude,"
                                  " max_lon=Eastern longitude,"
                                  " min_lat=Southern or northern latitude,"
                                  " max_lat=Northern or southern latitude."
                                  " For example: -80,50,20,70",
                         min_occurs=1,
                         max_occurs=1,
                         default='14,15,8,9',
                         ),

            LiteralInput('start', 'Start Date',
                         data_type='date',
                         abstract='First day of the period to be searched for EO data.'
                                  '(if not set, 30 days befor end of period will be selected',
                         default=(dt.now() - timedelta(days=30)).strftime('%Y-%m-%d'),
                         min_occurs=0,
                         max_occurs=1,
                         ),

            LiteralInput('end', 'End Date',
                         data_type='date',
                         abstract='Last day of the period to be searched for EO data.'
                                  '(if not set, current day is set.)',
                         default=dt.now().strftime('%Y-%m-%d'),
                         min_occurs=0,
                         max_occurs=1,
                         ),

            LiteralInput('cloud_cover', 'Cloud Cover',
                         data_type='integer',
                         abstract='Max tollerated percentage of cloud cover',
                         default="30",
                         allowed_values=[0, 10, 20, 30, 40, 50, 60, 70, 80, 100]
                         ),

            LiteralInput('username', 'User Name',
                         data_type='string',
                         abstract='Authentification user name for the COPERNICUS Sci-hub ',
                         # default='2013-12-31',
                         min_occurs=1,
                         max_occurs=1,
                         ),

            LiteralInput('password', 'Password',
                         data_type='string',
                         abstract='Authentification password for the COPERNICUS Sci-hub ',
                         min_occurs=1,
                         max_occurs=1,
                         ),
        ]

        outputs = [
            ComplexOutput("output", "Fetched Files",
                          abstract="File containing the local pathes to downloades files.",
                          supported_formats=[Format('text/plain')],
                          as_reference=True,
                          ),

            ComplexOutput("output_log", "Logging information",
                          abstract="Collected logs during process run.",
                          supported_formats=[Format("text/plain")],
                          as_reference=True,
                          )
        ]

        super(EO_COP_fetchProcess, self).__init__(
            self._handler,
            identifier="EO_COP_fetch",
            title="EO COPERNICUS Fetch Resources",
            version="0.1",
            abstract="Fetch EO Data to the local file"
                     "system of the birdhouse compute provider.",
            metadata=[
                Metadata('Documentation', 'http://flyingpigeon.readthedocs.io/en/latest/'),
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        response.update_status("start fetching resource", 10)

        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        products = [inpt.data for inpt in request.inputs['products']]

        bbox = []  # order xmin ymin xmax ymax
        bboxStr = request.inputs['BBox'][0].data
        bboxStr = bboxStr.split(',')
        bbox.append(float(bboxStr[0]))
        bbox.append(float(bboxStr[2]))
        bbox.append(float(bboxStr[1]))
        bbox.append(float(bboxStr[3]))

        if 'end' in request.inputs:
            end = request.inputs['end'][0].data
            end = dt.combine(end, time(23, 59, 59))
        else:
            end = dt.now()

        if 'start' in request.inputs:
            start = request.inputs['start'][0].data
            start = dt.combine(start, time(0, 0, 0))
        else:
            start = end - timedelta(days=30)

        if (start > end):
            start = dt.now() - timedelta(days=30)
            end = dt.now()
            LOGGER.exception("periode end befor periode start, period is set to the last 30 days from now")

        username = request.inputs['username'][0].data
        password = request.inputs['password'][0].data
        cloud_cover = request.inputs['cloud_cover'][0].data

        api = SentinelAPI(username, password)

        geom = {
          "type": "Polygon",
          "coordinates": [
                  [
                    [
                      14.00,
                      8.00
                    ],
                    [
                      16.00,
                      8.00
                    ],
                    [
                      16.00,
                      10.00
                    ],
                    [
                      14.00,
                      10.00
                    ],
                    [
                      14.00,
                      8.00
                    ]
                  ]
                ]
        }

        footprint = geojson_to_wkt(geom)

        response.update_status("start searching tiles acording query", 15)

        products = api.query(footprint,
                             date=(start, end),
                             platformname='Sentinel-2',
                             cloudcoverpercentage=(0, cloud_cover),
                             # producttype='SLC',
                             # orbitdirection='ASCENDING',
                             )

        response.update_status("write out information about files", 20)
        # api.download_all(products)
        _, filepathes = mkstemp(dir='.', suffix='.txt')
        try:
            with open(filepathes, 'w') as fp:
                fp.write('######################################################\n')
                fp.write('###     Following files are ready to download      ###\n')
                fp.write('######################################################\n')
                fp.write('\n')
                for key in products.keys():
                    size = float(products[key]['size'].split(' ')[0])
                    fp.write('%s size: %s \n' % (key, size))
            response.outputs['output'].file = filepathes
        except:
            LOGGER.exception('failed to write resources to textfile')
        # response.outputs['output'].file = filepathes

        response.update_status("done", 100)
        return response
