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
    'noaa_catalog_1': "http://www.esrl.noaa.gov/psd/thredds/catalog/Datasets/ncep.reanalysis.dailyavgs/surface/catalog.xml?dataset=Datasets/ncep.reanalysis.dailyavgs/surface/air.sig995.1948.nc"
    }
    
class WpsTestClient(object):
    def __init__(self):
        pywps_path = os.path.dirname(pywps.__file__)
        os.environ['PYWPS_CFG'] = os.path.abspath(os.path.join(pywps_path, '..', '..', '..', '..', 'etc', 'pywps', 'flyingpigeon.cfg'))
        os.environ['REQUEST_METHOD'] = pywps.METHOD_GET
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
    


