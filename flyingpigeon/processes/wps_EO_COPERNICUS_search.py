
from pywps import Process
# from pywps import LiteralInput
from pywps import ComplexInput, LiteralInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata

from eggshell.log import init_process_logger
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon import eodata

import logging
from datetime import datetime as dt
from datetime import timedelta, time
from tempfile import mkstemp

from pywps import Format
# from pywps import LiteralInput
from pywps import LiteralInput, ComplexOutput
from pywps import Process
from pywps.app.Common import Metadata
from sentinelsat import SentinelAPI, geojson_to_wkt

from flyingpigeon import eodata
from flyingpigeon.log import init_process_logger

LOGGER = logging.getLogger("PYWPS")


class EO_COP_searchProcess(Process):
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
            ComplexOutput("output_txt", "Files search result",
                          abstract="Files found according to the search querry",
                          supported_formats=[Format('text/plain')],
                          as_reference=True,
                          ),

            ComplexOutput("output_plot", "Extend of tiles",
                          abstract="Map showing the extends of the found EO data ",
                          supported_formats=[Format('image/png')],
                          as_reference=True,
                          ),

            ComplexOutput("output_log", "Logging information",
                          abstract="Collected logs during process run.",
                          supported_formats=[Format("text/plain")],
                          as_reference=True,
                          )
        ]

        super(EO_COP_searchProcess, self).__init__(
            self._handler,
            identifier="EO_COPERNICUS_search",
            title="EO COPERNICUS search products",
            version="0.1",
            abstract="Search for EO Data in the scihub.copernicus archive"
                     "output is a list of Product according to the querry and a graphical visualisation.",
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

        if start > end:
            start = dt.now() - timedelta(days=30)
            end = dt.now()
            LOGGER.exception('period ends before period starts; period now set to the last 30 days from now')

        username = request.inputs['username'][0].data
        password = request.inputs['password'][0].data
        cloud_cover = request.inputs['cloud_cover'][0].data

        api = SentinelAPI(username, password)

        geom = {
            "type": "Polygon",
            "coordinates": [[[bbox[0], bbox[1]],
                             [bbox[2], bbox[1]],
                             [bbox[2], bbox[3]],
                             [bbox[0], bbox[3]],
                             [bbox[0], bbox[1]]]]}

        footprint = geojson_to_wkt(geom)

        response.update_status("start searching tiles according to query", 15)

        products = api.query(footprint,
                             date=(start, end),
                             platformname='Sentinel-2',
                             cloudcoverpercentage=(0, cloud_cover),
                             # producttype='SLC',
                             # orbitdirection='ASCENDING',
                             )

        response.update_status("write out information about files", 20)
        # api.download_all(products)
        _, filepaths = mkstemp(dir='.', suffix='.txt')
        try:
            with open(filepaths, 'w') as fp:
                fp.write('######################################################\n')
                fp.write('###     Following files are ready to download      ###\n')
                fp.write('######################################################\n')
                fp.write('\n')
                for key in products.keys():
                    size = float(products[key]['size'].split(' ')[0])
                    producttype = products[key]['producttype']
                    beginposition = str(products[key]['beginposition'])
                    ID = str(products[key]['identifier'])
                    # TODO if we rewrite these algorithms in Python 3.7, use f-strings here!
                    fp.write('{} \t {} \t {} \t {} \t {} \n'.format(ID, size, producttype, beginposition, key))
            response.outputs['output_txt'].file = filepaths
        except Exception as ex:
            msg = 'failed to write resources to textfile: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)
            # response.outputs['output'].file = filepaths
        try:
            extend = [float(bboxStr[0]) - 5, float(bboxStr[1]) + 5, float(bboxStr[2]) - 5, float(bboxStr[3]) + 5]
            img = eodata.plot_products(products, extend=extend)
            response.outputs['output_plot'].file = img
        except Exception as ex:
            msg = 'Failed to plot extents of EO data: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        response.update_status('done', 100)
        return response
