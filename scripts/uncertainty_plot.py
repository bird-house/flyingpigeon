from os import listdir
from os.path import join
from flyingpigeon import utils
from flyingpigeon import metadata as md
from pandas import DataFrame
from flyingpigeon import calculation as cal


df = DataFrame(data=mean, index=ts)

df_mean = df.rolling(31, center=True).mean()
