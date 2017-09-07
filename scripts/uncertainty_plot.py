from os import listdir
from os.path import join
from flyingpigeon import utils
from flyingpigeon import metadata as md
from pandas import DataFrame
from flyingpigeon import calculation as cal

p = '/home/nils/data/AFR-44/tas/'
ncs = [join(p, nc) for nc in listdir(p)]
ncs_dic = utils.sort_by_filename(ncs)

keys = ncs_dic.keys()

ts = utils.get_time(ncs_dic[keys[0]])
vals = cal.fieldmean(ncs_dic[keys[0]])

sd = Series(data=vals, index=ts)
sd_year = sd.resample('12M').mean()


df = DataFrame(data=vals, index=ts)
df_mean = df.rolling(31, center=True).mean()
