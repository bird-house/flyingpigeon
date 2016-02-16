import logging
logger = logging.getLogger(__name__)


dic_basis = {
             'activity': 'birdhouse project',
             'contact': "Nils und Joerg",
                      'software': 'flyingpigeon v 0.1',
                      'software_platform': 'Python 2.7 on x86_64 GNU/Linux',
                      'software_project': 'birdhouse'}


def set_metadata_segetalflora(resource):
  """
  :param resources: imput files 
  """
  from netCDF4 import Dataset
  
  dic_segetalflora = dic_basis
  dic_segetalflora['keywords'] = 'Segetalflora'
  ds = Dataset(resource, mode='a')
  ds.setncatts(dic_segetalflora)
  ds.close()
 
  return resource


  

def set_metadata(resources):
  
  if type(resources) == str:
    resources = [resources]
    
  for resource in resources:
    ds = Dateset
    ds.attncatts(dic_segetalflora)
    ds.close()
    
  return resources