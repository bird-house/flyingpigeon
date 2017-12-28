
# David Trethewey 01-06-2016
#
# Sentinel2 Bands Stacker
# Uses visible, NIR, SWIR bands
#
# Assumptions:
#
# the .jp2 files are in the current directory
# that this script is being run from
#
# there is only one Sentinel2 scene in the directory
# and no other jp2 files
#
# Converts jp2 files of each band to single stacked file
#
# imports
import rsgislib
import rsgislib.imageutils
import os.path
import sys

# image list
# find all *.jp2 files in the current directory
directory = os.getcwd()
dirFileList = os.listdir(directory)
# print dirFileList

jp2FileList = [f for f in dirFileList if (f[-4:].lower()=='.jp2')]

bands = ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '8A']
# bands that are already at 10m resolution
bands_10m = ['02', '03', '04', '08']

# resample other bands to resolution of blue image (B02)
bands_toberesam = [b for b in bands if b not in bands_10m]

# identify the band number by counting backwards from the end in the filename
Bands_VIS_NIR_SWIR_FileList = [f for f in jp2FileList if (f[-6:-4] in bands)and(f[-7]=='B')]

# list of bands to be resampled
Bands_resam_FileList = [f for f in jp2FileList if (f[-6:-4] in bands_toberesam)and(f[-7]=='B')]

# find the filename of the blue image
blue_image = [f for f in jp2FileList if (f[-6:-4] == '02')and(f[-7]=='B')][0]

# bands to be resampled 20m --> 10m
# 05, 06, 07, 8b, 11, 12
# bands to be resampled 60m --> 10m
# 01, 09, 10

for b in Bands_resam_FileList:
    print("resampling band {q} to 10m".format(q=b[-6:-4]))
    outFile = b[:-4]+'_10m.kea'
    rsgislib.imageutils.resampleImage2Match(blue_image, b, outFile, 'KEA', 'cubic')
    Bands_VIS_NIR_SWIR_FileList.remove(b)
    Bands_VIS_NIR_SWIR_FileList.append(outFile)

Bands_VIS_NIR_SWIR_FileList = sorted(Bands_VIS_NIR_SWIR_FileList)

fileNameBase = blue_image[:-7]

# Sentinel2 bands
bandNamesList = ["B1Coastal443nm", "B2Blue490nm", "B3Green560nm", "B4Red665nm", "B5NIR705nm", "B6NIR740nm",
                 "B7NIR783nm", "B8NIR_broad842nm", "B9NIR940nm", "B10_1375nm", "B11_SWIR1610nm",
                 "B12_SWIR2190nm", "B8A_NIR865nm"]

#output file name
outputImage = fileNameBase + 'B'+''.join(bands)+'_stack.kea'

#output format (GDAL code)
outFormat = 'KEA'
outType = rsgislib.TYPE_32UINT

# stack bands using rsgislib
rsgislib.imageutils.stackImageBands(Bands_VIS_NIR_SWIR_FileList, bandNamesList, outputImage, None, 0, outFormat, outType)
# stats and pyramids
rsgislib.imageutils.popImageStats(outputImage,True,0.,True)

# remove individual resampled 10m files
print("removing intermediate resampled files")
for b in Bands_resam_FileList:
    outFile = b[:-4]+'_10m.kea'
    os.remove(outFile)
