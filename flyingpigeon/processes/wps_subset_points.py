from flyingpigeon.utils import sort_by_filename    
from pywps.Process import WPSProcess

import logging
logger = logging.getLogger(__name__)


class SubsetPointsProcess(WPSProcess):
  def __init__(self):
    WPSProcess.__init__(self, 
      identifier = "subset_points",
      title="Subset Points",
      version = "0.9",
      metadata= [
                {"title": "LSCE", "href": "http://www.lsce.ipsl.fr/en/index.php"},
                {"title": "Documentation", "href": "http://flyingpigeon.readthedocs.io/en/latest/"},
                ],
      abstract="Extract Timeseries for specified coordinates from gridded datasets",
      statusSupported=True,
      storeSupported=True
      )

    self.netcdf_file = self.addComplexInput(
      identifier="netcdf_file",
      title="NetCDF File",
      abstract="NetCDF File",
      minOccurs=1,
      maxOccurs=1000,
      maxmegabites=5000,
      formats=[{"mimeType":"application/x-netcdf"}],
      )

    self.coords = self.addLiteralInput(
      identifier="coords",
      title="Coordinates",
      abstract="a comma seperated touple of WGS85 lon,lat decimal coorinate",
      default="2.356138, 48.846450",
      type=type(''),
      minOccurs=1,
      maxOccurs=100,
      )
    
    self.tarout = self.addComplexOutput(
      identifier="tarout",
      title="Tarfile",
      abstract="tar archive containing the value tables",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      )
          
  def execute(self):
    from flyingpigeon.ocgis_module import call
    from flyingpigeon.utils import sort_by_filename, archive, get_values, get_time
        
    ncs = self.getInputValues(identifier='netcdf_file')
    logger.info("ncs: %s " % ncs) 
    coords = self.getInputValues(identifier='coords')
    logger.info("coords %s", coords)
    filenames = []    
    nc_exp = sort_by_filename(ncs, historical_concatination=True)
    
    #(fp_tar, tarout_file) = tempfile.mkstemp(dir=".", suffix='.tar')
    #tar = tarfile.open(tarout_file, "w")

    from numpy import savetxt, column_stack
    from shapely.geometry import Point
    
    for key in nc_exp.keys():
      try:
        logger.info('start calculation for %s ' % key )
        ncs = nc_exp[key]
        times = get_time(ncs)
        concat_vals = ['%s-%02d-%02d_%02d:%02d:%02d' %
                       (t.year, t.month, t.day, t.hour, t.minute, t.second) for t in times]
        header = 'date_time'
        filename = '%s.csv' % key
        filenames.append(filename) 
        
        for p in coords:
          try: 
            self.status.set('processing point : {0}'.format(p), 20)
            # define the point:  
            p = p.split(',')
            point = Point(float(p[0]), float(p[1]))       
            
            # get the values
            timeseries = call(resource=ncs, geom=point, select_nearest=True)
            vals = get_values(timeseries)
            
            # concatination of values 
            header = header + ',%s-%s' % (p[0], p[1])
            concat_vals = column_stack([concat_vals, vals])
          except Exception as e: 
            logger.debug('failed for point %s %s' % (p , e))
        self.status.set('*** all points processed for {0} ****'.format(key), 50)
        savetxt(filename, concat_vals, fmt='%s', delimiter=',', header=header)
      except Exception as e: 
        logger.debug('failed for %s %s' % (key, e))

    ### set the outputs
    self.status.set('*** creating output tar archive ****',90) 
    tarout_file = archive(filenames)
    self.tarout.setValue( tarout_file )