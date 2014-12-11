from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)
logger.setLevel('DEBUG')

SERVICE = "http://localhost:8093/wps"
TESTDATA = {}

# GDAL_DATA environment variable is needed by ocgis!
# TODO: set GDAL_DATA in a save way
import os
from os.path import join
os.environ['GDAL_DATA'] = join(os.environ['HOME'], 'anaconda', 'share', 'gdal')
try:
    import ocgis
except:
    logging.exception('ocgis is not available!')

# load test data
from os.path import join, dirname
__testdata_filename__ = join(dirname(__file__), 'testdata.json')

try:
    from os.path import join
    import json
    with open(__testdata_filename__, 'r') as fp:
        TESTDATA = json.load(fp)
        # TODO: owslib does not like unicode
        for key in TESTDATA.keys():
            TESTDATA[key] = str(TESTDATA[key]) 
except:
    logging.exception('could not read testdata! %s', __testdata_filename__ )

