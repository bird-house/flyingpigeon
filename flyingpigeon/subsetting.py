# import ocgis

from .exceptions import CalculationException
from .utils import drs_filename, calc_grouping

from malleefowl import wpslogging as logging
#import logging

logger = logging.getLogger(__name__)

from os.path import dirname, join
DIR_MASKS = join(dirname(__file__), 'processes', 'masks')

def masking(resource, mask, prefix=None, dir_output=None):
  """
  Returns a list of masked netCDF file(s) path(es).  
  :param resource: string path to netCDF resource
  :param mask: predifined mask ('EUR-11', 'EUR-44')
  :param prefix:  prefix for filename. If prefix is not set, a filename will be created
  :param dir_output: directory for output file. If dir_output is not set, a tempdir will be created
  """
  from cdo import Cdo
  cdo = Cdo()
  from tempfile import mkstemp
  from os import system, path 
  
  if dir_output == None: 
    dir_output = path.curdir
  nc_mask = path.join(DIR_MASKS, mask + '.nc')
  
  if prefix == None: 
    p1 , resource_masked = mkstemp(dir = dir_output, suffix='.nc')
  else: 
    resource_masked = path.join(dir_output, prefix + '.nc')
# try:
  call = "cdo div '%s' '%s' '%s'" % ( resource , nc_mask , resource_masked)
  system(call)
  
  return resource_masked


def get_dimension_map(resource): 
  """ returns the dimension map for a file, required for ocgis processing. 
  file must have a DRS conform filename (see: utils.drs_filename())
  
  :param resource: str input file path
  """

  from os.path import basename
  file_name = basename(resource)
  
  dim_map1 = {'X': {'variable': 'lon', 'dimension': 'x', 'pos': 2},
              'Y': {'variable': 'lat', 'dimension': 'y', 'pos': 1},
              'T': {'variable': 'time', 'dimension': 'time', 'pos': 0}}
  
  #dim_map2 = {'X': {'variable': 'lon', 'dimension': 'x', 'pos': 2},
              #'Y': {'variable': 'lat', 'dimension': 'y', 'pos': 1},
              #'T': {'variable': 'time', 'dimension': 'time', 'pos': 0, 'bounds': 'time_bnds'}}
  
  dim_map3 = {'X': {'variable': 'rlon', 'dimension': 'x', 'pos': 2},
              'Y': {'variable': 'rlat', 'dimension': 'y', 'pos': 1},
              'T': {'variable': 'time', 'dimension': 'time', 'pos': 0 }}

  dim_map4 = {'X': {'variable': 'Actual_longitude', 'dimension': 'x', 'pos': 2},
              'Y': {'variable': 'Actual_latitude', 'dimension': 'y', 'pos': 1},
              'T': {'variable': 'time', 'dimension': 'time', 'pos': 0 }}

  dim_map5 = {'X': {'variable': 'x', 'dimension': 'x', 'pos': 2},
              'Y': {'variable': 'y', 'dimension': 'y', 'pos': 1},
              'T': {'variable': 'time', 'dimension': 'time', 'pos': 0 }}

  
  if 'CM5A-MR_WRF331F' in file_name: 
    dimension_map = dim_map1
  elif 'CNRM-CM5_CNRM-ALADIN53' in file_name: 
    dimension_map = dim_map1
  elif 'MPI-ESM-LR_REMO019' in file_name: 
    dimension_map = dim_map1
  elif 'CLMcom-CCLM4-8-17' in file_name:
    dimension_map = dim_map1
  elif '_v11.0' in file_name: # EOBS Data
    dimension_map = dim_map4
  #elif 'EOBS' in file_name:   
    #dimension_map = dim_map5
  else:     
    dimension_map = None
    
  return dimension_map

# === Functions for Clipping: 

# === Available Polygons
POLYGONS = ['AUT','BEL','BGR','CYP','CZE','DEU','DNK','ESP','EST','FIN','FRA', 'GBR','GRC','HUN','HRV','IRL','ITA','LVA','LTU','LUX','NLD',
                 'POL','PRT','ROU','SVK','SVN','SWE','NOR','CHE','ISL','MKD','MNE',
                 'SRB','MDA','UKR','BIH','ALB','BLR','KOS'] #'MLT',


_COUNTRIES_ =  dict(
  ABW=dict(longname='Aruba'),
  AFG=dict(longname='Afghanistan'),
  AGO=dict(longname='Angola'),
  AIA=dict(longname='Anguilla'),
  ALB=dict(longname='Albania'),
  ALD=dict(longname='Aland'),
  AND=dict(longname='Andorra'),
  ARE=dict(longname='United Arab Emirates'),
  ARG=dict(longname='Argentina'),
  ARM=dict(longname='Armenia'),
  ASM=dict(longname='American Samoa'),
  ATA=dict(longname='Antarctica'),
  ATC=dict(longname='Ashmore and Cartier Is.'),
  ATF=dict(longname='Fr. S. and Antarctic Lands'),
  ATG=dict(longname='Antigua and Barb.'),
  AUS=dict(longname='Australia'),
  AUT=dict(longname='Austria'),
  AZE=dict(longname='Azerbaijan'),
  BDI=dict(longname='Burundi'),
  BEL=dict(longname='Belgium'),
  BEN=dict(longname='Benin'),
  BFA=dict(longname='Burkina Faso'),
  BGD=dict(longname='Bangladesh'),
  BGR=dict(longname='Bulgaria'),
  BHR=dict(longname='Bahrain'),
  BHS=dict(longname='Bahamas'),
  BIH=dict(longname='Bosnia and Herz.'),
  BLM=dict(longname='St-Barth\xe9lemy'),
  BLR=dict(longname='Belarus'),
  BLZ=dict(longname='Belize'),
  BMU=dict(longname='Bermuda'),
  BOL=dict(longname='Bolivia'),
  BRA=dict(longname='Brazil'),
  BRB=dict(longname='Barbados'),
  BRN=dict(longname='Brunei'),
  BTN=dict(longname='Bhutan'),
  BWA=dict(longname='Botswana'),
  CAF=dict(longname='Central African Rep.'),
  CAN=dict(longname='Canada'),
  CHE=dict(longname='Switzerland'),
  CHL=dict(longname='Chile'),
  CHN=dict(longname='China'),
  CIV=dict(longname='Cote d\'Ivoire'),
  CMR=dict(longname='Cameroon'),
  COD=dict(longname='Democratic Republic of the Congo'),
  COG=dict(longname='Republic of Congo'),
  COK=dict(longname='Cook Is.'),
  COL=dict(longname='Colombia'),
  COM=dict(longname='Comoros'),
  CPV=dict(longname='Cape Verde'),
  CRI=dict(longname='Costa Rica'),
  CUB=dict(longname='Cuba'),
  CUW=dict(longname='Curacao'),
  CYM=dict(longname='Cayman Is.'),
  B20=dict(longname='N. Cyprus'),
  CYP=dict(longname='Cyprus'),
  CZE=dict(longname='Czech Rep.'),
  DEU=dict(longname='Germany'),
  DJI=dict(longname='Djibouti'),
  DMA=dict(longname='Dominica'),
  DNK=dict(longname='Denmark'),
  DOM=dict(longname='Dominican Rep.'),
  DZA=dict(longname='Algeria'),
  ECU=dict(longname='Ecuador'),
  EGY=dict(longname='Egypt'),
  ERI=dict(longname='Eritrea'),
  ESP=dict(longname='Spain'),
  EST=dict(longname='Estonia'),
  ETH=dict(longname='Ethiopia'),
  FIN=dict(longname='Finland'),
  FJI=dict(longname='Fiji'),
  B12=dict(longname='Falkland Is.'),
  FRA=dict(longname='France'),
  FRO=dict(longname='Faeroe Islands'),
  FSM=dict(longname='Micronesia'),
  GAB=dict(longname='Gabon'),
  GBR=dict(longname='United Kingdom'),
  GEO=dict(longname='Georgia'),
  GGY=dict(longname='Guernsey'),
  GHA=dict(longname='Ghana'),
  GIN=dict(longname='Guinea'),
  GMB=dict(longname='Gambia'),
  GNB=dict(longname='Guinea-Bissau'),
  GNQ=dict(longname='Eq. Guinea'),
  GRC=dict(longname='Greece'),
  GRD=dict(longname='Grenada'),
  GRL=dict(longname='Greenland'),
  GTM=dict(longname='Guatemala'),
  GUM=dict(longname='Guam'),
  GUY=dict(longname='Guyana'),
  HKG=dict(longname='Hong Kong'),
  HMD=dict(longname='Heard I. and McDonald Is.'),
  HND=dict(longname='Honduras'),
  HRV=dict(longname='Croatia'),
  HTI=dict(longname='Haiti'),
  HUN=dict(longname='Hungary'),
  IDN=dict(longname='Indonesia'),
  IMN=dict(longname='Isle of Man'),
  IND=dict(longname='India'),
  IOA=dict(longname='Indian Ocean Ter.'),
  B69=dict(longname='Br. Indian Ocean Ter.'),
  IRL=dict(longname='Ireland'),
  IRN=dict(longname='Iran'),
  IRQ=dict(longname='Iraq'),
  ISL=dict(longname='Iceland'),
  ISR=dict(longname='Israel'),
  ITA=dict(longname='Italy'),
  JAM=dict(longname='Jamaica'),
  JEY=dict(longname='Jersey'),
  JOR=dict(longname='Jordan'),
  JPN=dict(longname='Japan'),
  B45=dict(longname='Siachen Glacier'),
  KAZ=dict(longname='Kazakhstan'),
  KEN=dict(longname='Kenya'),
  KGZ=dict(longname='Kyrgyzstan'),
  KHM=dict(longname='Cambodia'),
  KIR=dict(longname='Kiribati'),
  KNA=dict(longname='Saint Kitts and Nevis'),
  KOR=dict(longname='Republic of Korea'),
  B57=dict(longname='Kosovo'),
  KWT=dict(longname='Kuwait'),
  LAO=dict(longname='Laos'),
  LBN=dict(longname='Lebanon'),
  LBR=dict(longname='Liberia'),
  LBY=dict(longname='Libya'),
  LCA=dict(longname='Saint Lucia'),
  LIE=dict(longname='Liechtenstein'),
  LKA=dict(longname='Sri Lanka'),
  LSO=dict(longname='Lesotho'),
  LTU=dict(longname='Lithuania'),
  LUX=dict(longname='Luxembourg'),
  LVA=dict(longname='Latvia'),
  MAC=dict(longname='Macao'),
  MAF=dict(longname='Saint-Martin'),
  MAR=dict(longname='Morocco'),
  MCO=dict(longname='Monaco'),
  MDA=dict(longname='Moldova'),
  MDG=dict(longname='Madagascar'),
  MDV=dict(longname='Maldives'),
  MEX=dict(longname='Mexico'),
  MHL=dict(longname='Marshall Is.'),
  MKD=dict(longname='Macedonia'),
  MLI=dict(longname='Mali'),
  MLT=dict(longname='Malta'),
  MMR=dict(longname='Myanmar'),
  MNE=dict(longname='Montenegro'),
  MNG=dict(longname='Mongolia'),
  MNP=dict(longname='N. Mariana Is.'),
  MOZ=dict(longname='Mozambique'),
  MRT=dict(longname='Mauritania'),
  MSR=dict(longname='Montserrat'),
  MUS=dict(longname='Mauritius'),
  MWI=dict(longname='Malawi'),
  MYS=dict(longname='Malaysia'),
  NAM=dict(longname='Namibia'),
  NCL=dict(longname='New Caledonia'),
  NER=dict(longname='Niger'),
  NFK=dict(longname='Norfolk Island'),
  NGA=dict(longname='Nigeria'),
  NIC=dict(longname='Nicaragua'),
  NIU=dict(longname='Niue'),
  NLD=dict(longname='Netherlands'),
  NOR=dict(longname='Norway'),
  NPL=dict(longname='Nepal'),
  NRU=dict(longname='Nauru'),
  NZL=dict(longname='New Zealand'),
  OMN=dict(longname='Oman'),
  PAK=dict(longname='Pakistan'),
  PAN=dict(longname='Panama'),
  PCN=dict(longname='Pitcairn Is.'),
  PER=dict(longname='Peru'),
  PHL=dict(longname='Philippines'),
  PLW=dict(longname='Palau'),
  PN1=dict(longname='Papua New Guinea'),
  POL=dict(longname='Poland'),
  PRI=dict(longname='Puerto Rico'),
  PRK=dict(longname='Dem. Rep. Korea'),
  PR1=dict(longname='Portugal'),
  PRY=dict(longname='Paraguay'),
  PSX=dict(longname='Palestine'),
  PYF=dict(longname='Fr. Polynesia'),
  QAT=dict(longname='Qatar'),
  ROU=dict(longname='Romania'),
  RUS=dict(longname='Russia'),
  RWA=dict(longname='Rwanda'),
  B28=dict(longname='W. Sahara'),
  SAU=dict(longname='Saudi Arabia'),
  SDN=dict(longname='Sudan'),
  SDS=dict(longname='S. Sudan'),
  SEN=dict(longname='Senegal'),
  SGP=dict(longname='Singapore'),
  SGS=dict(longname='S. Geo. and S. Sandw. Is.'),
  SHN=dict(longname='Saint Helena'),
  SLB=dict(longname='Solomon Is.'),
  SLE=dict(longname='Sierra Leone'),
  SLV=dict(longname='El Salvador'),
  SMR=dict(longname='San Marino'),
  B30=dict(longname='Somaliland'),
  SOM=dict(longname='Somalia'),
  SPM=dict(longname='St. Pierre and Miquelon'),
  SRB=dict(longname='Serbia'),
  STP=dict(longname='Sao Tome and Principe'),
  SUR=dict(longname='Suriname'),
  SVK=dict(longname='Slovakia'),
  SVN=dict(longname='Slovenia'),
  SWE=dict(longname='Sweden'),
  SWZ=dict(longname='Swaziland'),
  SXM=dict(longname='Sint Maarten'),
  SYC=dict(longname='Seychelles'),
  SYR=dict(longname='Syria'),
  TCA=dict(longname='Turks and Caicos Is.'),
  TCD=dict(longname='Chad'),
  TGO=dict(longname='Togo'),
  THA=dict(longname='Thailand'),
  TJK=dict(longname='Tajikistan'),
  TKM=dict(longname='Turkmenistan'),
  TLS=dict(longname='Timor-Leste'),
  TON=dict(longname='Tonga'),
  TTO=dict(longname='Trinidad and Tobago'),
  TUN=dict(longname='Tunisia'),
  TUR=dict(longname='Turkey'),
  B77=dict(longname='Taiwan'),
  TZA=dict(longname='Tanzania'),
  UGA=dict(longname='Uganda'),
  UKR=dict(longname='Ukraine'),
  URY=dict(longname='Uruguay'),
  USA=dict(longname='United States'),
  UZB=dict(longname='Uzbekistan'),
  VAT=dict(longname='Vatican'),
  VCT=dict(longname='St. Vin. and Gren.'),
  VEN=dict(longname='Venezuela'),
  VGB=dict(longname='British Virgin Is.'),
  VIR=dict(longname='U.S. Virgin Is.'),
  VNM=dict(longname='Vietnam'),
  VUT=dict(longname='Vanuatu'),
  WLF=dict(longname='Wallis and Futuna Islands'),
  WSM=dict(longname='Samoa'),
  YEM=dict(longname='Yemen'),
  ZAF=dict(longname='South Africa'),
  ZMB=dict(longname='Zambia'),
  ZWE=dict(longname='Zimbabwe')
)

def countries():
    """
    :return: a list of all countries codes.
    """
    countries = _COUNTRIES_.keys()
    countries.sort()
    return countries

def countries_longname():
    """
    :return: a list of all countries long names.
    """
    longname = ''
    for country in countries(): 
      longname = longname + "%s : %s \n" % (country, _COUNTRIES_[country]['longname'])
    return longname


COUNTRIES_EU = ['AUT','BEL','BGR','CYP','CZE','DEU','DNK','ESP','EST','FIN','FRA', 'GBR','GRC','HUN','HRV','IRL','ITA','LVA','LTU','LUX','NLD',
                 'POL','PRT','ROU','SVK','SVN','SWE','NOR','CHE','ISL','MKD','MNE',
                 'SRB','MDA','UKR','BIH','ALB','BLR','KOS'] #'MLT',
