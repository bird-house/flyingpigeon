import os
import pywps
import lxml

NAMESPACES = {
    'xlink': "http://www.w3.org/1999/xlink",
    'wps': "http://www.opengis.net/wps/1.0.0",
    'ows': "http://www.opengis.net/ows/1.1",
    'gml': "http://www.opengis.net/gml",
    'xsi': "http://www.w3.org/2001/XMLSchema-instance"
}

SERVICE = "http://localhost:8093/wps"

TESTDATA = { 
    'noaa_nc_1': "http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.1955.nc",
    'noaa_catalog_1': "http://www.esrl.noaa.gov/psd/thredds/catalog/Datasets/ncep.reanalysis.dailyavgs/surface/catalog.xml?dataset=Datasets/ncep.reanalysis.dailyavgs/surface/air.sig995.1948.nc",
    'noaa_slp_1955_nc': "http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.1955.nc",
    'noaa_slp_1956_nc': "http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.1956.nc",
    'eurocordex_nc_1': "http://webportals.ipsl.jussieu.fr/thredds/fileServer/ATLAS/Flux/LandModels/yearlymean/fco2_CLM4CN_Sep2013-ext3_1980-2010_yearlymean_XYT.nc"
    }

def prepare_env():
    pywps_path = os.path.dirname(pywps.__file__)
    home_path = os.path.abspath(os.path.join(pywps_path, '..', '..', '..', '..'))
    os.environ['PATH'] = "{0}:{1}".format(os.path.join(home_path, 'bin'), os.environ['PATH'])
    os.environ['GDAL_DATA'] = os.path.join(home_path, 'share', 'gdal')
    
class WpsTestClient(object):
    def __init__(self):
        pywps_path = os.path.dirname(pywps.__file__)
        home_path = os.path.abspath(os.path.join(pywps_path, '..', '..', '..', '..'))
        os.environ['PYWPS_CFG'] = os.path.join(home_path, 'etc', 'pywps', 'flyingpigeon.cfg')
        os.environ['REQUEST_METHOD'] = pywps.METHOD_GET
        os.environ['PATH'] = "{0}:{1}".format(os.path.join(home_path, 'bin'), os.environ['PATH'])
        os.environ['GDAL_DATA'] = os.path.join(home_path, 'share', 'gdal')
        self.wps = pywps.Pywps(os.environ)
   
    def get(self, *args, **kwargs):
        query = ""
        for key,value in kwargs.iteritems():
            query += "{0}={1}&".format(key, value)
        inputs = self.wps.parseRequest(query)
        self.wps.performRequest(inputs)
        return WpsTestResponse(self.wps.response)

class WpsTestResponse(object):

    def __init__(self, data):
        self.data = data
        self.xml = lxml.etree.fromstring(data)

    def xpath(self, path):
        return self.xml.xpath(path, namespaces=NAMESPACES)

    def xpath_text(self, path):
        return ' '.join(e.text for e in self.xpath(path))


def assert_response_success(resp):
    success = resp.xpath('/wps:ExecuteResponse/wps:Status/wps:ProcessSucceeded')
    assert len(success) == 1
    


