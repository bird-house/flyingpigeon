from flyingpigeon import sdm 
from datetime import datetime as dt

tic = dt.now()

ncs_indices = ['/home/nils/data/sdm/RX1day_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_JJA_19960101-20000101.nc', '/home/nils/data/sdm/TNn_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_Jan_19960101-20000101.nc','/home/nils/data/sdm/TG_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_JJA_19960101-20000101.nc']

ncs_references = ['/home/nils/data/sdm/RX1day_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_JJA_ref-1998-2000.nc','/home/nils/data/sdm/TG_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_JJA_ref-1998-2000.nc','/home/nils/data/sdm/TNn_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_Jan_ref-1998-2000.nc']

csv = '/home/nils/Downloads/0013848-160118175350007.csv'
latlon = sdm.get_latlon(csv)

PApoints = sdm.get_PApoints(coordinates=latlon)


gam_model, predict_ref, gam_info = sdm.get_gam(ncs_references,PApoints)
prediction = sdm.get_prediction(gam_model, ncs_indices)

from numpy import invert, isnan, nan, broadcast_arrays, array, zeros, linspace, meshgrid
mask = invert(isnan(PApoints))
mask = broadcast_arrays(prediction, mask)[1]
prediction[mask==False] = nan

species_file = sdm.write_to_file(ncs_indices[0], prediction)

tac = dt.now()

print 'prediction finished in %s minutes' % (tac - tic)
