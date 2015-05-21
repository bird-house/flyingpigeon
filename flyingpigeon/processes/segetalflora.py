from malleefowl.process import WPSProcess
import subprocess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)


class segetalflora(WPSProcess):
  """This process calculates the relative humidity"""
  def __init__(self):
    WPSProcess.__init__(self, 
      identifier = "segetalflora",
      title="Segetal Flora",
      version = "0.1",
      metadata=[{"title": "Institut Pierre Simon Laplace", "href": "https://www.ipsl.fr/en/"}],
      abstract="Species biodiversity of segetal flora",
      )

    self.netcdf_file = self.addComplexInput(
      identifier="netcdf_file",
      title="NetCDF Files",
      abstract="NetCDF File",
      minOccurs=1,
      maxOccurs=1000,
      maxmegabites=500000,
      formats=[{"mimeType":"application/x-netcdf"}],
      )

    self.climate_type = self.addLiteralInput(
      identifier="climate_type",
      title="Climate type",
      abstract="Select climate type",
      default='3',
      type=type(''),
      minOccurs=1,
      maxOccurs=8,
      allowedValues=["1", "2", "3", "4", "5", "6", "7", "all"] # sem
      )

    self.culture_type = self.addLiteralInput(
      identifier="culture_type",
      title="Culture type",
      abstract="Select culture type",
      default='fallow',
      type=type(''),
      minOccurs=1,
      maxOccurs=8,
      allowedValues=["fallow", "intensiv", "extensiv"] # sem
      )

    #complex output
    #-------------
    #self.logout = self.addComplexOutput(
      #identifier="logout",
      #title="Indice log-file",
      #abstract="logfile for segetalflora process",
      #metadata=[],
      #formats=[{"mimeType":"text/plain"}],
      #asReference=True,
      #)
    
    self.out_tas = self.addComplexOutput(
      title="tas_EUR",
      abstract="Tar archive containing the netCDF EUR tas mean files",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="out_tas",
      )

    self.out_segetalflora = self.addComplexOutput(
      title="polygons",
      abstract="Tar archive containing the netCDF EU-countries polygons segetalflora ",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="out_segetalflora",
      )

    self.out_fieldmeans = self.addComplexOutput(
      title="fieldmeans",
      abstract="Tar archive containing the netCDF EU-countries fieldmeans segetalflora ",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="out_fieldmeans",
      )

    self.out_plots = self.addComplexOutput(
      title="plots",
      abstract="Tar archive containing the bokeh plots html files for segetalflora ",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="out_plots",
      )

# calculation of number of segetal flora species
  def execute(self):
    
    from os import mkdir, path, listdir 
    import tarfile
    from tempfile import  mkstemp #, mkdtemp
    from datetime import datetime
    
    from flyingpigeon import segetalflora as sf
    
    logger.debug('starting segetalflora process execution')
    self.show_status('starting calcualtion segetalflora', 5)
    
    ## prepare environment
    # create the tar files
    
    try: 
      (fp_tarf_tas, tarf_tas) = mkstemp(dir=".", suffix='.tar')
      (fp_tarf_segetalflora, tarf_segetalflora) = mkstemp(dir=".", suffix='.tar')
      (fp_tarf_fieldmeans, tarf_fieldmeans) = mkstemp(dir=".", suffix='.tar')
      (fp_tarf_plots, tarf_plots) = mkstemp(dir=".", suffix='.tar')
      tar_tas = tarfile.open(tarf_tas, "w")
      tar_segetalflora = tarfile.open(tarf_segetalflora, "w")
      tar_fieldmeans = tarfile.open(tarf_fieldmeans, "w")
      tar_plots = tarfile.open(tarf_plots, "w")
      
      logger.debug('tar files initialized')
      
      # create output folders
      dir_tas = path.abspath(path.curdir+'/dir_tas/')
      dir_segetalflora = path.abspath(path.curdir+'/dir_segetalflora/')
      dir_fieldmean = path.abspath(path.curdir+'/dir_fieldmean/')
      dir_plots = path.abspath(path.curdir+'/dir_plots/')
      
      mkdir(dir_tas)
      mkdir(dir_segetalflora)
      mkdir(dir_fieldmean)
      mkdir(dir_plots)
      logger.debug('out directories created')
    except  Exception as e:
      msg = 'tar file or mkdir failed!: %s ' % (e)
      logger.error(msg)
      
    countries = ['AUT','BEL','BGR','CYP','CZE','DEU','DNK','ESP',
                 'EST','FIN','FRA','GBR','GRC','HUN','HRV','IRL',
                 'ITA','LVA','LTU','LUX','MLT','NLD','POL','PRT',
                 'ROU','SVK','SVN','SWE','NOR','CHE','ISL','MKD',
                 'MNE','SRB','MDA','UKR','BIH','ALB','BLR','KOS']

    
    # read argments to variables
    #try:  
    ncs = self.getInputValues(identifier='netcdf_file')
    climate_type = self.climate_type.getValue()
    culture_type = self.culture_type.getValue()
    
    if type(climate_type) != list:
      climate_type = list([climate_type])
    if type(culture_type) != list:
      culture_type = list([culture_type])
      
    logger.debug('urls for %s ncs found' % (len(ncs)))
    logger.debug('culture type: %s ' % (culture_type))


# === main call for segetalflora processing    
    stepps = len(culture_type) * len(climate_type)
    
    for a, cult in enumerate(culture_type): 
      for b, clim in enumerate(climate_type):
        start = (a + 1) * (b + 1) 
        per = (start / stepps) * 95
        self.show_status('%s/%s processing for %s climate type: %s' %(start, stepps, culture_type, climate_type), per)
        try:
          sf_files =  sf.get_segetalflora(ncs, culture_type=cult,
                                          climate_type=clim,
                                          countries=countries, 
                                          dir_tas=dir_tas,
                                          dir_segetalflora=dir_segetalflora)
          self.show_status("processing of %s segetalflora files done " % (len(sf_files)) , 95)
        except Exception as e:
          msg = 'segetalflora calculation failed %s %s : %s\n' %( climate_type, culture_type, e) 
          logger.exception(msg)
        
# === fieldmeans         
    from flyingpigeon import timeseries as ts
    self.show_status('processing fieldmeans' , 97)
    try:
      ncs = [path.join(dir_segetalflora,nc) for nc in listdir(dir_segetalflora)]
      ncs_fld = ts.fldmean(ncs, dir_output=dir_fieldmean)
      logger.debug('%s fieldmeans processed' % (len(ncs_fld)))
    except Exception as e:
      logger.exception('fieldmeans failed: %s\n' % (e))
    

# === visualisation 
    from flyingpigeon import visualisation as vs
    self.show_status('processing visualisation' , 98)
    
    # sort files for plotting
    try:
      ncs = listdir(dir_segetalflora)
      set_var = set()
      set_contry = set()
      for nc in ncs: 
        set_var = set_var.union([nc.split('_')[0]])
        set_contry = set_contry.union([nc.split('_')[1]])
      logger.debug('%s files to plots sorted' % (len(ncs)))
    except Exception as e:
      logger.exception('files sorting failed: %s\n' % (e))
      
    # plot sorted files 
    try:
      plots = []
      for v in set_var: 
        for c in set_contry: 
          ncs = [path.join(dir_segetalflora,nc) for nc in listdir(dir_segetalflora) if v in nc and c in nc ]
          p = vs.spaghetti(ncs, variable=v, title='Segetalflora %s in %s' % (v, c), dir_out=dir_plots)
          plots.append(p)
          logger.debug('plot created for %s %s' % (v, c )) 
    except Exception as e:
      logger.exception('ploting failed: %s\n' % (e))

# === tar file archiving 
    self.show_status('files to tar archives', 99)
    tar_tas.add(dir_tas, arcname = dir_tas.replace(path.abspath(path.curdir), ""))
    tar_segetalflora.add(dir_segetalflora, arcname = dir_segetalflora.replace(path.abspath(path.curdir), ""))
    tar_fieldmeans.add(dir_fieldmean, arcname = dir_fieldmean.replace(path.abspath(path.curdir), ""))
    tar_plots.add(dir_plots, arcname = dir_plots.replace(path.abspath(path.curdir), ""))
    
    tar_tas.close()
    tar_fieldmeans.close()
    tar_segetalflora.close()
    tar_plots.close()
    logger.debug('tar ncfiles closed')
    

# === set output parameter   
    self.out_fieldmeans.setValue( tarf_fieldmeans )
    self.out_segetalflora.setValue( tarf_segetalflora )
    self.out_tas.setValue( tarf_tas )
    self.out_plots.setValue( tarf_plots )
    self.show_status("processing done", 100)