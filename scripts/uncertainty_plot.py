from os import listdir
from os.path import join
from flyingpigeon import utils
from flyingpigeon import metadata as md
from pandas import DataFrame
from flyingpigeon import calculation as cal

p = '/home/nils/data/AFR-44/tas/'
ncs = [join(p, nc) for nc in listdir(p) if not 'tas_AFR-44_MOHC-HadGEM2-ES_historical_r1i1p1_KNMI-RACMO22T_v2_day' in nc ]
ncs_dic = utils.sort_by_filename(ncs)

ts = utils.get_time(ncs_dic[ncs_dic.keys()[0]])
data = cal.fieldmean(ncs_dic[ncs_dic.keys()[0]])
