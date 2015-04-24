import ocgis
 
nc = '/homel/nhempel/anaconda/var/cache/pywps/pr_EUR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20110101-20151231.nc'
 
calc = [{'func': 'icclim_R20mm', 'name': 'R20mm'}]
calc_grouping = [[12, 1, 2], 'unique']
 
Mean_file = None
rd = ocgis.RequestDataset(nc, 'pr') # time_range=[dt1, dt2]
ocgis.env.OVERWRITE=True

#dir_output = tempfile.mkdtemp()
dir_output = '/homel/nhempel/data/cviewer/extremoscope/' # None

geom_file = ocgis.OcgOperations(dataset=rd, calc=calc, calc_grouping=calc_grouping, prefix=str('output_icclim'), 
                                output_format='nc',  dir_output=dir_output).execute()
print(geom_file)