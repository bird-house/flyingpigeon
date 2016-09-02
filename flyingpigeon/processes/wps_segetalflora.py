from pywps.Process import WPSProcess

from flyingpigeon.subset import countries #REGION_EUROPE

import logging

class SegetalfloraProcess(WPSProcess):
  """This process calculates the relative humidity"""
  def __init__(self):
    WPSProcess.__init__(self, 
      identifier = "segetalflora",
      title="Segetal Flora",
      version = "0.9",
      metadata=[{"title": "Institut Pierre Simon Laplace", "href": "https://www.ipsl.fr/en/"}],
      abstract="Species biodiversity of segetal flora. Imput files: variable:tas , domain: EUR-11 or EUR-44",
      statusSupported=True,
      storeSupported=True
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
      allowedValues=["fallow", "intensive", "extensive"] # sem
      )
    
    #self.region = self.addLiteralInput(
      #identifier="region",
      #title="Region",
      #abstract="European Regions ...",
      #default='FRA',
      #type=type(''),
      #minOccurs=0,
      #maxOccurs=25,
      #allowedValues=countries() 
      #)


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
    
    logging.debug('starting segetalflora process execution')
    self.status.set('starting calcualtion segetalflora', 5)
    
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
      
      logging.debug('tar files initialized')
      
      # create output folders
      dir_tas = path.abspath(path.curdir+'/dir_tas/')
      dir_segetalflora = path.abspath(path.curdir+'/dir_segetalflora/')
      dir_fieldmean = path.abspath(path.curdir+'/dir_fieldmean/')
      dir_plots = path.abspath(path.curdir+'/dir_plots/')
      
      mkdir(dir_tas)
      mkdir(dir_segetalflora)
      mkdir(dir_fieldmean)
      mkdir(dir_plots)
      logging.debug('out directories created')
    except  Exception as e:
      logging.exception('tar file or mkdir failed!')
      raise
      
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
    #countries = self.getInputValues(identifier='region')
    
    if type(climate_type) != list:
      climate_type = list([climate_type])
    if type(culture_type) != list:
      culture_type = list([culture_type])
    #if type(countries) != list:
      #countries = list([countries])
      
    logging.debug('urls for %s ncs found' % (len(ncs)))
    logging.debug('culture type: %s ' % (culture_type))

# === main call for segetalflora processing    
    stepps = len(culture_type) * len(climate_type)
    
    for a, cult in enumerate(culture_type): 
      for b, clim in enumerate(climate_type):
        start = (a + 1) * (b + 1) 
        per = (start / stepps) * 95
        self.status.set('%s/%s processing for %s climate type: %s' %(start, stepps, culture_type, climate_type), per)
        try:
          sf_files =  sf.get_segetalflora(resource=ncs, dir_output=dir_tas, 
                                          culture_type=cult, climate_type=clim,
                                          region=None, dimension_map=None)
            
          self.status.set("processing of %s segetalflora files done " % (len(sf_files)) , 95)
        except Exception as e:
          logging.exception('segetalflora calculation failed %s %s' % ( climate_type, culture_type))
          raise
        
# === fieldmeans         
    from flyingpigeon import timeseries as ts
    self.status.set('processing fieldmeans' , 97)
    try:
      ncs = [path.join(dir_segetalflora,nc) for nc in listdir(dir_segetalflora)]
      ncs_fld = ts.fldmean(ncs, dir_output=dir_fieldmean)
      logging.debug('%s fieldmeans processed' % (len(ncs_fld)))
    except Exception as e:
      logging.exception('fieldmeans failed')
      raise
    

# === visualisation 
    from flyingpigeon import visualisation as vs
    from os import rename
    
    self.status.set('processing visualisation' , 98)
    
    # sort files for plotting
    try:
      ncs = listdir(dir_segetalflora)
      set_var = set()
      set_contry = set()
      for nc in ncs: 
        set_var = set_var.union([nc.split('_')[0]])
        set_contry = set_contry.union([nc.split('_')[1]])
      logging.debug('%s files to plots sorted' % (len(ncs)))
    except Exception as e:
      logging.exception('files sorting failed')
      raise
      
    # plot sorted files 
    try:
      plots = []
      for v in set_var: 
        for c in set_contry: 
          ncs = [path.join(dir_segetalflora,nc) for nc in listdir(dir_segetalflora) if v in nc and c in nc ]
          p = vs.spaghetti(ncs,
                           variable=v,
                           title='Segetalflora %s in %s' % (v, c),
                           dir_out=dir_plots)
          newname = path.dirname(p)+'/%s_%s_birdhouse_output.html' %(v,c)
          rename(p,newname)
          plots.append(newname)
          logging.debug('plot created and renamed for %s %s' % (v, c )) 
    except Exception as e:
      logging.exception('ploting failed')
      raise

# === tar file archiving 
    self.status.set('files to tar archives', 99)
    tar_tas.add(dir_tas, arcname = dir_tas.replace(path.abspath(path.curdir), ""))
    tar_segetalflora.add(dir_segetalflora, arcname = dir_segetalflora.replace(path.abspath(path.curdir), ""))
    tar_fieldmeans.add(dir_fieldmean, arcname = dir_fieldmean.replace(path.abspath(path.curdir), ""))
    tar_plots.add(dir_plots, arcname = dir_plots.replace(path.abspath(path.curdir), ""))
    
    tar_tas.close()
    tar_fieldmeans.close()
    tar_segetalflora.close()
    tar_plots.close()
    logging.debug('tar ncfiles closed')
    

# === set output parameter   
    self.out_fieldmeans.setValue( tarf_fieldmeans )
    self.out_segetalflora.setValue( tarf_segetalflora )
    self.out_tas.setValue( tarf_tas )
    self.out_plots.setValue( tarf_plots )
    self.status.set("processing done", 100)
