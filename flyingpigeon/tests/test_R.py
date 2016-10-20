import pytest

from rpy2.robjects.packages import importr

def test_R_rpy2():
  import rpy2.robjects 
  import rpy2.robjects.numpy2ri
  rpy2.robjects.numpy2ri.activate()

def test_R_fields():
  fields = importr("fields")
  
def test_R_stats():  
  stats = importr("stats")

def test_R_ncdf4():  
  ncdf4 = importr("ncdf4")

def test_R_mclust():  
  mclust = importr("mclust")

def test_R_maps():
  maps = importr("maps")

def test_pdf_grDevices():
  from os import remove
  grDevices = importr("grDevices")
  grDevices.pdf(file='Rplot.pdf')
  grDevices.dev_off()
  remove('Rplot.pdf')  
  png = grDevices.png(filename='Rplot.png', type='cairo')
  grDevices.dev_off()
  grDevices.postscript(filename='Rplot.eps')
  grDevices.dev_off()
  remove('Rplot.eps')