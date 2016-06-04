import ocgis
from shapely.geometry import Point
import tempfile
import tarfile
import os
import pandas as pd 
from pandas import DataFrame, read_csv
import numpy as np
    
from flyingpigeon.utils import sort_by_filename
    
from pywps.Process import WPSProcess

import logging

class ExtractPointsProcess(WPSProcess):
  def __init__(self):
    WPSProcess.__init__(self, 
      identifier = "extractpoints",
      title="Extract Coordinate Points",
      version = "0.3",
      metadata= [
              {"title": "Institut Pierre Simon Laplace", "href": "https://www.ipsl.fr/en/"}
              ],
      abstract="Extract Timeseries for specified coordinates from grid data",
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
    from flyingpigeon.utils import get_time, get_variable, sort_by_filename
    
    from datetime import datetime as dt
    from netCDF4 import Dataset
    from numpy import savetxt, column_stack, squeeze
    
    ncs = self.getInputValues(identifier='netcdf_file')
    logging.info("ncs: %s " % ncs) 
    coords = self.getInputValues(identifier='coords')
    logging.info("coords %s", coords)

 
    nc_exp = sort_by_filename(ncs) # dictionary {experiment:[files]}
    filenames = []
    
    (fp_tar, tarout_file) = tempfile.mkstemp(dir=".", suffix='.tar')
    tar = tarfile.open(tarout_file, "w")
    
    for key in nc_exp.keys():
      logging.info('start calculation for %s ' % key )
      ncs = nc_exp[key]
      nc = ncs[0]
      
      times = get_time(nc)
      var = get_variable(nc)
      
      concat_vals = [dt.strftime(t, format='%Y-%d-%m_%H:%M:%S') for t in times]
      header = 'date_time'
      filename = '%s.csv' % key
      filenames.append(filename) 
      
      for ugid, p in enumerate(coords, start=1):
        self.status.set('processing point : {0}'.format(p), 20)
        p = p.split(',')
        self.status.set('splited x and y coord : {0}'.format(p), 20)
        point = Point(float(p[0]), float(p[1]))
        
        #get the timeseries at gridpoint
        timeseries = call(resource=ncs, geom=point, select_nearest=True)
        
        ds = Dataset(timeseries)
        vals = squeeze(ds.variables[var])
        header = header + ',%s_%s' % (p[0], p[1])
        concat_vals = column_stack([concat_vals, vals])

      savetxt(filename, concat_vals, fmt='%s', delimiter=',', header=header)
      tar.add( filename )
      
    tar.close()
    self.tarout.setValue( tarout_file )