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

TESTS_HOME = os.path.abspath(os.path.dirname(__file__))

TESTDATA = { 
    'noaa_nc_1': "http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.1955.nc",
    'noaa_catalog_1': "http://www.esrl.noaa.gov/psd/thredds/catalog/Datasets/ncep.reanalysis.dailyavgs/surface/catalog.xml?dataset=Datasets/ncep.reanalysis.dailyavgs/surface/air.sig995.1948.nc",
    'noaa_slp_1955_nc': "http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.1955.nc",
    'noaa_slp_1956_nc': "http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.1956.nc",
    'eurocordex_nc_1': "http://webportals.ipsl.jussieu.fr/thredds/fileServer/ATLAS/Flux/LandModels/yearlymean/fco2_CLM4CN_Sep2013-ext3_1980-2010_yearlymean_XYT.nc",
    'eurocordex_nc_2': "http://webportals.ipsl.jussieu.fr/thredds/fileServer/EUROCORDEX/extremoscope_FRA/polygons/TG/yr/rcp45/1/TG_rcp45_CNRM-CM5_CNRM-ALADIN53_1971-2100.nc",
    'cmip5_tasmax_nc': "file://{0}".format(os.path.join(TESTS_HOME, 'testdata', 'cmip5', 'tasmax_Amon_MPI-ESM-MR_rcp45_r1i1p1_200601-200612.nc')),
    'cmip5_tasmax_r1_nc': "file://{0}".format(os.path.join(TESTS_HOME, 'testdata', 'cmip5', 'tasmax_Amon_MPI-ESM-MR_rcp45_r1i1p1_200601-200612.nc')),
    'cmip5_tasmax_r2_nc': "file://{0}".format(os.path.join(TESTS_HOME, 'testdata', 'cmip5', 'tasmax_Amon_MPI-ESM-MR_rcp45_r2i1p1_200601-200612.nc')),
    'cmip5_tasmax_2006_nc': "file://{0}".format(os.path.join(TESTS_HOME, 'testdata', 'cmip5', 'tasmax_Amon_MPI-ESM-MR_rcp45_r1i1p1_200601-200612.nc')),
    'cmip5_tasmax_2007_nc': "file://{0}".format(os.path.join(TESTS_HOME, 'testdata', 'cmip5', 'tasmax_Amon_MPI-ESM-MR_rcp45_r1i1p1_200701-200712.nc')),
    'cordex_tasmax_nc': "file://{0}".format(os.path.join(TESTS_HOME, 'testdata', 'cordex', 'tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon_200602-200612.nc')),
    'cordex_tasmax_2006_nc': "file://{0}".format(os.path.join(TESTS_HOME, 'testdata', 'cordex', 'tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon_200602-200612.nc')),
    'cordex_tasmax_2007_nc': "file://{0}".format(os.path.join(TESTS_HOME, 'testdata', 'cordex', 'tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon_200602-200612.nc')),
    }

def prepare_env():
    pywps_path = os.path.dirname(pywps.__file__)
    #home_path = os.path.abspath(os.path.join(pywps_path, '..', '..', '..', '..'))
    home_path = os.path.abspath(os.path.join(os.environ['HOME'], 'birdhouse'))
    os.environ['PATH'] = "{0}:{1}".format(os.path.join(home_path, 'bin'), os.environ['PATH'])
    os.environ['GDAL_DATA'] = os.path.join(home_path, 'share', 'gdal')
    
class WpsTestClient(object):
    def __init__(self):
        pywps_path = os.path.dirname(pywps.__file__)
        #home_path = os.path.abspath(os.path.join(pywps_path, '..', '..', '..', '..'))
        home_path = os.path.abspath(os.path.join(os.environ['HOME'], 'birdhouse'))
        os.environ['PYWPS_CFG'] = os.path.join(home_path, 'etc', 'pywps', 'flyingpigeon.cfg')
        os.environ['REQUEST_METHOD'] = pywps.METHOD_GET
        os.environ['PATH'] = "{0}:{1}".format(os.path.join(home_path, 'bin'), os.environ['PATH'])
        os.environ['GDAL_DATA'] = os.path.join(home_path, 'share', 'gdal')
        self.wps = pywps.Pywps(os.environ["REQUEST_METHOD"], os.environ.get("PYWPS_CFG"))
   
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
    


