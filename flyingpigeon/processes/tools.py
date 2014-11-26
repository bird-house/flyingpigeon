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

def indices( idic  ): # 
    # read the idic 
    outdir = idic['outdir'] if idir.has_key('outdir') else None
    ncs = idic['ncs'] if idir.has_key('ncs') else  None
    TG = idic['TG'] if idir.has_key('TG') else  None
    TX = idic['TX'] if idir.has_key('TX') else  None
    SU = idic['SU'] if idir.has_key('SU') else  None
    RR = idic['RR'] if idir.has_key('RR') else  None
    
    outlog = "Starting the indice calculation at: \n"
    logger.debug('starting icclim indices ... done')
    logger.debug('outdir ... : %s' % ( outdir ))
    
    ocgis.env.OVERWRITE = True
    ocgis.env.DIR_OUTPUT = cdir    
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
                        
        except Exception as e:
            msg = 'processing failed for file  : %s' % ( filename)
            logger.error(msg)
            outlog = outlog + msg + '\n'

        logger.debug('processing done')    
        outlog = outlog + "Processing icclim worker done \n"
    
    return outlog ;
    
