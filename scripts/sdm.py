from flyingpigeon import sdm 

ncs_inidces = ['/home/nils/data/sdm/RX1day_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_JJA_19960101-20000101.nc', '/home/nils/data/sdm/TNn_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_Jan_19960101-20000101.nc','/home/nils/data/sdm/TG_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_JJA_19960101-20000101.nc']

ncs_references = ['/home/nils/data/sdm/RX1day_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_JJA_ref-1998-2000.nc','/home/nils/data/sdm/TG_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_JJA_ref-1998-2000.nc','/home/nils/data/sdm/TNn_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_Jan_ref-1998-2000.nc']

csv = '/home/nils/Downloads/0013848-160118175350007.csv'

latlon = sdm.get_latlon(csv)
PAmask = sdm.get_PAmask(latlon)

gam_model = sdm.get_gam(ncs_references,PAmask)

