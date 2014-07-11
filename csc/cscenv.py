##
import ocgis
#import icclim
from ocgis.interface.base.crs import CFWGS84

from netCDF4 import Dataset
import os
import time 

import subprocess
from malleefowl.process import WorkerProcess
from malleefowl.utils import dupname

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)


def indices( outdir, ncfile, TG, TX, TN, RR, TG_5to9, TG_6to8, RR_5to9, RR_6to8, SU ): # 

    outlog = "Starting the indice calculation at: \n"
    
    logger.debug('starting ECA indices ... done')
            
    logger.debug('outdir ... : %s' % ( outdir ))
    
    ocgis.env.OVERWRITE = True
    ocgis.env.DIR_OUTPUT = outdir    
    output_crs = None
        
    #self.show_status('Set ocgis outdir ...', 5)
    logger.debug('set ocgis outdir ... done')

    outlog = outlog + "Set the output dir \n"
    
    # simple precesses realized by cdo commands:
    
    for nc in ncfile:
    #self.show_status('in the nc loop ...', 15)
        
        filename = nc
        try : 
            logger.debug('in the nc loop ... done')
            outlog = outlog + "Starting to process file:  " + nc + " \n"
        
            ds = Dataset(nc)
            
            if (str(ds.project_id) == 'CMIP5'):
            #day_MPI-ESM-LR_historical_r1i1p1
                #frq = str(ds.frequency)
                gmodel = str(ds.model_id)
                exp = str(ds.experiment_id)
                ens = str(ds.parent_experiment_rip)
                filename = str( gmodel + '_' + exp + '_' + ens )
                
            elif (str(ds.project_id) == 'CORDEX'):
            #EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_DMI-HIRHAM5_v1_day
                dom = str(ds.CORDEX_domain)
                gmodel = str(ds.driving_model_id)
                exp = str(ds.experiment_id)
                ens = str(ds.driving_model_ensemble_member)
                rmodel = str(ds.model_id)
                ver = str(ds.rcm_version_id)
                #frq = str(ds.frequency)
                filename = str(dom + '_' + gmodel + '_' + exp + '_' + ens + '_' + rmodel + '_' + ver )
            
            #self.show_status('filename created ...:'+ filename , 15)
            logger.debug('filename created ...:'+ filename)

            outlog = outlog + "Create filename:  " + filename + " \n"
        except Exception as e: 
            msg = 'Could not define file name for file : %s ' % ( filename )
            logger.error(msg)
            outlog = outlog + msg + '\n'

            
            
        try :
            
            if TG == True :
                if "tas" in ds.variables.keys():
                    TG_file = None
                    rd = ocgis.RequestDataset(nc, 'tas') # time_range=[dt1, dt2]
                    group = ['year']
                    calc_icclim = [{'func':'icclim_TG','name':'TG'}]
                    TG_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim,
                                                  calc_grouping=group,
                                                  prefix=dupname(outdir, 'TG_' + filename),
                                                  output_crs=output_crs,
                                                  output_format='nc',
                                                  add_auxiliary_files=False).execute()

                    logger.debug('TG calculated ...:%s' % ( filename))
                    outlog = outlog + "TG indice processed sucessfully  \n"
                
            if TX == True :
                if "tasmax" in ds.variables.keys():
                    TX_file = None
                    rd = ocgis.RequestDataset(nc, 'tasmax') # time_range=[dt1, dt2]
                    group = ['year']
                    calc_icclim = [{'func':'icclim_TX','name':'TX'}]
                    TX_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group,
                                                  prefix=dupname(outdir, 'TX_' + filename),
                                                  output_crs=output_crs,
                                                  output_format='nc',
                                                  add_auxiliary_files=False).execute()

                    logger.debug('TX calculated ...:%s' % ( filename))
                    outlog = outlog + "TX indice processed sucessfully  \n"
                    
                
            if TN == True :
                if "tasmin" in ds.variables.keys():
                    TN_file = None
                    rd = ocgis.RequestDataset(nc, 'tasmin') # time_range=[dt1, dt2]
                    group = ['year']
                    calc_icclim = [{'func':'icclim_TN','name':'TN'}]
                    TN_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group,
                                                  prefix=dupname(outdir, 'TN_' + filename),
                                                  output_crs=output_crs,
                                                  output_format='nc',
                                                  add_auxiliary_files=False).execute()

                logger.debug('TN calculated ...:%s' % ( filename))
                outlog = outlog + "TN indice processed sucessfully  \n"
                
            if RR == True :
                if "pr" in ds.variables.keys():
                    RR_file = None
                    rd = ocgis.RequestDataset(nc, 'pr') # time_range=[dt1, dt2]
                    group = ['year']
                    calc_icclim = [{'func':'sum','name':'RR'}]
                    RR_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group,
                                                  prefix=dupname(outdir, 'RR_' + filename),
                                                  output_crs=output_crs,
                                                  output_format='nc',
                                                  add_auxiliary_files=False).execute()

                    logger.debug('RR calculated ...:%s' % ( filename))
                    outlog = outlog + "RR indice processed sucessfully  \n"
                
            if TG_5to9 == True :
                if "tas" in ds.variables.keys():
                    TG_5to9_file = None
                    rd = ocgis.RequestDataset(nc, 'tas', time_region={'month':[5,6,7,8,9]}) # time_range=[dt1, dt2]
                    group = ['year']
                    calc_icclim = [{'func':'icclim_TG','name':'TG_5to9'}]
                    TG_5to9_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group,
                                                       prefix=dupname(outdir, 'TG_5to9_' + filename),
                                                       output_crs=output_crs,
                                                       output_format='nc',
                                                       add_auxiliary_files=False).execute()

                    logger.debug('TG_5to9 calculated ...:%s' % ( filename))
                    outlog = outlog + "TG_5to9 indice processed sucessfully  \n"
            
            if TG_6to8 == True :
                if "tas" in ds.variables.keys():
                    TG_6to8_file = None
                    rd = ocgis.RequestDataset(nc, 'tas', time_region={'month':[6,7,8]}) # time_range=[dt1, dt2]
                    group = ['year']
                    calc_icclim = [{'func':'icclim_TG','name':'TG_6to8'}]
                    TG_6to8_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group,
                                                       prefix=dupname(outdir, 'TG_6to8_' + filename),
                                                       output_crs=output_crs,
                                                       output_format='nc',
                                                       add_auxiliary_files=False).execute()

                    logger.debug('TG_6to8 calculated ...:%s' % ( filename))
                    outlog = outlog + "TG_6to8 indice processed sucessfully  \n"
                
            if RR_5to9 == True :
                if "pr" in ds.variables.keys():
                    RR_5to9_file = None
                    rd = ocgis.RequestDataset(nc, 'pr', time_region={'month':[5,6,7,8,9]}) # time_range=[dt1, dt2]
                    group = ['year']
                    calc_icclim = [{'func':'sum','name':'RR_5to9'}]
                    RR_5to9_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group,
                                                       prefix=dupname(outdir, 'RR_5to9_' + filename),
                                                       output_crs=output_crs,
                                                       output_format='nc',
                                                       add_auxiliary_files=False).execute()

                #RR_5to9_nc = outdir + 'RR_5to9'+ filename + ''
                    #cdo.setname('RR_5to9',input = "-yearsum -selmon,5,6,7,8,9 "+ nc , output = RR_5to9_nc, output_crs=output_crs, options =  '-f nc')  

                    logger.debug('RR_5to9 calculated ...:%s' % ( filename))
                    outlog = outlog + "RR_5to9 indice processed sucessfully  \n"            
            
            if RR_6to8 == True :
                if "pr" in ds.variables.keys():
                    RR_6to8_file = None
                    rd = ocgis.RequestDataset(nc, 'pr', time_region={'month':[6,7,8]}) # time_range=[dt1, dt2]
                    group = ['year']
                    calc_icclim = [{'func':'sum','name':'RR_6to8'}]
                    RR_6to8_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group,
                                                       prefix=dupname(outdir, 'RR_6to8_' + filename),
                                                       output_crs=output_crs,
                                                       output_format='nc',
                                                       add_auxiliary_files=False).execute()

                    #RR_6to8_nc = outdir + 'RR_6to8'+ filename + ''
                    #cdo.setname('RR_6to8',input = "-yearsum -selmon,6,7,8 "+ nc , output = RR_6to8_nc, output_crs=output_crs, options =  '-f nc')  
                    logger.debug('RR_6to8 calculated ...:%s' % ( filename))
                    outlog = outlog + "RR_6to8 indice processed sucessfully  \n"
                
            if SU == True and "tasmax" in ds.variables.keys():               
                logger.debug('In the SU loop')
                SU_file = None
                rd = ocgis.RequestDataset(nc, 'tasmax') # time_range=[dt1, dt2]
                logger.debug('ocgis.RequestDataset ... done')
                group = ['year']
                calc_icclim = [{'func':'icclim_SU','name':'SU'}]
                try :
                    SU_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group,
                                                  prefix=dupname(outdir, 'SU_' + filename),
                                                  output_format='nc',
                                                  add_auxiliary_files=False).execute()
                    #self.show_status('SU calculated ...:'+ filename , 15)
                    logger.debug('SU calculated ...:%s' % ( filename))
                    outlog = outlog + "SU indice processed sucessfully  \n"
                    
                except Exception as e: 
                    logger.error('calc_icclim SU faild %s' , e.message)
                    outlog = outlog + "SU indice processed faild for %s \n" % (filename)
                        
            #if ETR == True :
                #ETR_file = None
                
                #variables = ['tasmin', 'tasmax']
                #request_datasets = [ocgis.RequestDataset(uri,variable) for uri,variable in zip(uris,variables)]
                #rdc = ocgis.RequestDatasetCollection(request_datasets)
                #group = ['year']
                #calc_icclim = [{'func':'icclim_ETR','name':'ETR','kwds':{'tasmin':'tasmin','tasmax':'tasmax'}}]
                #ETR_file = ocgis.OcgOperations(dataset=rdc, calc=calc_icclim, calc_grouping=group, prefix=str('ETR_'), output_format='nc', add_auxiliary_files=False).execute()

            #if HI == True :
                #HI_file = None
                
                #variables = ['tasmin', 'tasmax']
                #request_datasets = [ocgis.RequestDataset(uri,variable) for uri,variable in zip(uris,variables)]
                #rdc = ocgis.RequestDatasetCollection(request_datasets)
                #group = ['year']
                #calc_icclim = [{'func':'icclim_ETR','name':'ETR','kwds':{'tasmin':'tasmin','tasmax':'tasmax'}}]
                #ETR_file = ocgis.OcgOperations(dataset=rdc, calc=calc_icclim, calc_grouping=group, prefix=str('ETR_'), output_format='nc', add_auxiliary_files=False).execute()
        except Exception as e:
            msg = 'processing failed for file  : %s' % ( filename)
            logger.error(msg)
            outlog = outlog + msg + '\n'

        logger.debug('processing done')    
        outlog = outlog + "Processing icclim worker done \n"
    
    return outlog ;
    
