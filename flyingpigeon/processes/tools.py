##
import ocgis
#import icclim
from ocgis.interface.base.crs import CFWGS84

from netCDF4 import Dataset
import os 
from datetime import datetime, timedelta

import subprocess
#from malleefowl.process import WorkerProcess
#from malleefowl.utils import dupname
from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)


def fn_creator( ncs ):
  newnames = []
  for nc in ncs:
    
    fp ,fn = os.path.split(nc)
    logger.debug('fn_gerator for: %s' % fn)
    ds = Dataset(nc)
    rd = []
    rd = ocgis.RequestDataset(nc)
    ts = ds.variables['time']
    reftime = reftime = datetime.strptime('1949-12-01', '%Y-%m-%d')
    st = datetime.strftime(reftime + timedelta(days=ts[0]), '%Y%m%d') 
    en = datetime.strftime(reftime + timedelta(days=ts[-1]), '%Y%m%d') 
    
    if (str(ds.project_id) == 'CMIP5'):
    #day_MPI-ESM-LR_historical_r1i1p1
      var ==str(rd.variable)
      frq = str(ds.frequency)
      gmodel = str(ds.model_id)
      exp = str(ds.experiment_id)
      ens = str(ds.parent_experiment_rip)
      filename = var + '_' + str( gmodel + '_' + exp + '_' + ens + '_' + str(int(st)) + '-' + str(int(en)) + '.nc')
        
    elif (str(ds.project_id) == 'CORDEX'):
    #EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_DMI-HIRHAM5_v1_day
      var ==str(rd.variable)
      dom = str(ds.CORDEX_domain)
      gmodel = str(ds.driving_model_id)
      exp = str(ds.experiment_id)
      ens = str(ds.driving_model_ensemble_member)
      rmodel = str(ds.model_id)
      ver = str(ds.rcm_version_id)
      frq = str(ds.frequency)
      filename = var + '_'+ str(dom + '_' + gmodel + '_' + exp + '_' + ens + '_' + rmodel + '_' + ver + \
        '_' + frq + '_' + str(int(st)) + '-' + str(int(en)) + '.nc' )
    else:
      filename = fn 
      logger.debug('WPS name forwarded :%s' % ( filename))
      
    ##except Exception as e: 
      #msg = 'Could not define file name for file : %s %s' % ( nc , e )
      #logger.error(msg)
      #outlog = outlog + msg + '\n'
    os.rename(nc, os.path.join(fp, filename ))
    newnames.append(os.path.join(fp, filename))
    logger.debug('file name generated and renamed :%s' % ( filename))
  
  return newnames

def fn_sorter(ncs): 
  ndic = {}
  for nc in ncs:
    #logger.debug('file: %s' % nc)
    p, f = os.path.split(nc) 
    n = f.split('_')
    bn = '_'.join(n[0:-1])
    ndic[bn] = []
  for key in ndic: 
    for n in ncs:
      if key in n: 
        ndic[key].append(n)  
  logger.debug('Data Experiment dictionary build: %i experiments found' % (len(ndic.keys())))
  return ndic

def fn_sorter_ch(ncs):
    # concatinates szenarios and appropriate historical runs
  ndic = {}
  for nc in ncs:
    p, f = os.path.split(nc) 
    n = f.split('_')
    bn = '_'.join(n[0:-1])
    if n[3] != 'historical':
      ndic[bn] = []
  for key in ndic:
    historical = key.replace('rcp26','historical').replace('rcp45','historical').replace('rcp85','historical')
    for n in ncs:
      if key in n or historical in n: 
        ndic[key].append(n)
  logger.debug('Data Experiment dictionary build: %i experiments found' % (len(ndic.keys())))      
  return ndic
  
def indices( idic  ):
  # 
  # read the idic 
  outdir = idic['outdir'] if idic.has_key('outdir') else None
  ncs = idic['ncs'] if idic.has_key('ncs') else  None
  TG = idic['TG'] if idic.has_key('TG') else  None
  TX = idic['TX'] if idic.has_key('TX') else  None
  TN = idic['TN'] if idic.has_key('TN') else  None
  TXn = idic['TXn'] if idic.has_key('TXn') else  None
  TXx = idic['TXx'] if idic.has_key('TXx') else  None
  TNn = idic['TNn'] if idic.has_key('TNn') else  None
  TNx = idic['TNx'] if idic.has_key('TNx') else  None
  SU = idic['SU'] if idic.has_key('SU') else  None
  CSU = idic['CSU'] if idic.has_key('CSU') else  None
  FD = idic['FD'] if idic.has_key('FD') else  None
  CFD = idic['CFD'] if idic.has_key('CFD') else  None
  TR = idic['TR'] if idic.has_key('TR') else  None
  ID = idic['ID'] if idic.has_key('ID') else  None
  HD17 = idic['HD17'] if idic.has_key('HD17') else  None
  GD4 = idic['GD4'] if idic.has_key('GD4') else  None
  RR = idic['RR'] if idic.has_key('RR') else  None
  RR1 = idic['RR1'] if idic.has_key('RR1') else  None
  CWD = idic['CWD'] if idic.has_key('CWD') else  None
  SDII = idic['SDII'] if idic.has_key('SDII') else  None
  R10mm = idic['R10mm'] if idic.has_key('R10mm') else  None
  R20mm = idic['R20mm'] if idic.has_key('R20mm') else  None
  RX1day = idic['RX1day'] if idic.has_key('RX1day') else  None
  RX5day = idic['RX5day'] if idic.has_key('RX5day') else  None
  SD = idic['SD'] if idic.has_key('SD') else  None
  SD1 = idic['SD1'] if idic.has_key('SD1') else  None
  SD5cm = idic['SD5cm'] if idic.has_key('SD5cm') else  None
  SD50cm = idic['SD50cm'] if idic.has_key('SD50cm') else  None
  CDD = idic['CDD'] if idic.has_key('CDD') else  None
  group = idic['group'] if idic.has_key('group') else  ['year']
  logger.debug('gcalc_roup set to : %s' % ( group ))
  
  outlog = "Starting the indice calculation at: \n"
  logger.debug('starting icclim indices ... done')
  logger.debug('outdir ... : %s' % ( outdir ))
  
  ocgis.env.OVERWRITE = True
  ocgis.env.DIR_DATA = os.path.curdir
  ocgis.env.DIR_OUTPUT = outdir    
  output_crs = None
  
  logger.debug('settings for ocgis done')
  outlog = outlog + "settings for ocgis done \n"
  
  # simple precesses realized by cdo commands:
  #rd_tas = ocgis.util.helpers.get_sorted_uris_by_time_dimension(ocgis.RequestDataset(ncs, 'tas'))
  #rd_tasmin = ocgis.util.helpers.get_sorted_uris_by_time_dimension(ocgis.RequestDataset(ncs, 'tasmin'))
  #rd_tasmax = ocgis.util.helpers.get_sorted_uris_by_time_dimension(ocgis.RequestDataset(ncs, 'tasmax'))
  #rd_pr = ocgis.util.helpers.get_sorted_uris_by_time_dimension(ocgis.RequestDataset(ncs, 'pr'))
  
  #self.show_status('Set ocgis outdir ...', 5)
  
  exp = fn_sorter(ncs) # dictionary with experiment : files
  
  for key in exp.keys():
    ncs = exp[key]
    basename = key
    var = key.split('_')[0]
    ncs.sort()
    rd = ocgis.RequestDataset(ncs, var) # time_range=[dt1, dt2]
    logger.debug('calculation of experimtent %s with variable %s'% (key,var))
    
  #for nc in ncs:
    #fp, fn = os.path.split(nc)
    #basename = os.path.splitext(fn)[0]
    #ds = Dataset(nc)
    try:
      if TG == True and var == 'tas': # ds.variables.keys():
        logger.debug('calculation for TG started ')
        TG_file = None
        calc_icclim = [{'func':'icclim_TG','name':'TG'}]
        rds = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix= (basename.replace('tas_','TG_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False)
        TG_file= rds.execute()
        logger.debug('TG calculated ' )
        outlog = outlog + "TG indice processed sucessfully  \n"
        #TG_file = fn_creator( TG_file )
      
      if TX == True and  var =="tasmax" :
        logger.debug('calculation for TX started ')
        TX_file = None
        rd = ocgis.RequestDataset(nc, 'tasmax') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_TX','name':'TX'}]
        TX_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('tasmax_','TG_')) , output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('TX calculated ' )
        outlog = outlog + "TX indice processed sucessfully  \n"
              
      if TN == True and var =="tasmin" :
        logger.debug('calculation for TN started ')
        TN_file = None
        rd = ocgis.RequestDataset(nc, 'tasmin') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_TN','name':'TN'}]
        TN_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('tasmin_','TN_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('TN calculated ' )
        outlog = outlog + "TN indice processed sucessfully  \n"

      if TXx == True and  var =="tasmax" :
        logger.debug('calculation for TXx started ')
        TXx_file = None
        rd = ocgis.RequestDataset(nc, 'tasmax') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_TXx','name':'TXx'}]
        TXx_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('tasmax_','TXx_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('TXx calculated ' )
        outlog = outlog + "TXx indice processed sucessfully  \n"

      if TNx == True and var =="tasmin" :
        logger.debug('calculation for TNx started ')
        TNx_file = None
        rd = ocgis.RequestDataset(nc, 'tasmin') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_TNx','name':'TNx'}]
        TN_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('tasmin_','TNx_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('TNx calculated ' )
        outlog = outlog + "TNx indice processed sucessfully  \n"

      if TNn == True and var =="tasmin" :
        logger.debug('calculation for TNn started ')
        TNn_file = None
        rd = ocgis.RequestDataset(nc, 'tasmin') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_TNn','name':'TNn'}]
        TN_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('tasmin_','TNn_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('TNn calculated ' )
        outlog = outlog + "TNn indice processed sucessfully  \n"
          
      if SU == True and  var =="tasmax" :
        logger.debug('calculation for SU started ')
        SU_file = None
        rd = ocgis.RequestDataset(nc, 'tasmax') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_SU','name':'SU'}]
        SU_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('tasmax_','SU_')), output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('SU calculated ' )
        outlog = outlog + "SU indice processed sucessfully  \n"

      if CSU == True and  var =="tasmax" :
        logger.debug('calculation for CSU started ')
        SU_file = None
        rd = ocgis.RequestDataset(nc, 'tasmax') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_CSU','name':'CSU'}]
        CSU_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('tasmax_','CSU_')) ,
        output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('CSU calculated ' )
        outlog = outlog + "CSU indice processed sucessfully  \n"

      if FD == True and var =="tasmin" :
        logger.debug('calculation for FD started ')
        FD_file = None
        rd = ocgis.RequestDataset(nc, 'tasmin') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_FD','name':'FD'}]
        FD_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('tasmax_','FD_')) ,
        output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('FD calculated ' )
        outlog = outlog + "FD indice processed sucessfully  \n"
          
      if CFD == True and var =="tasmin" :
        logger.debug('calculation for CFD started ')
        CFD_file = None
        rd = ocgis.RequestDataset(nc, 'tasmin') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_CFD','name':'CFD'}]
        CFD_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('tasmax_','CFD_')) ,
        output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('CFD calculated ' )
        outlog = outlog + "CFD indice processed sucessfully  \n"
          
      if TR == True and var =="tasmin" :
        logger.debug('calculation for TR started ')
        TR_file = None
        rd = ocgis.RequestDataset(nc, 'tasmin') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_TR','name':'TR'}]
        TR_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('tasmax_','TR_')) ,
        output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('TR calculated ' )
        outlog = outlog + "TR indice processed sucessfully  \n"


      if ID == True and  var =="tasmax" :
        logger.debug('calculation for ID started ')
        ID_file = None
        rd = ocgis.RequestDataset(nc, 'tasmax') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_ID','name':'ID'}]
        IR_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('tasmax_','ID_')) ,
        output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('ID calculated ' )
        outlog = outlog + "ID indice processed sucessfully  \n"

      if HD17 == True and var =="tas" :
        logger.debug('calculation for HD17 started ')
        HD17_file = None
        rd = ocgis.RequestDataset(nc, 'tas') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_HD17','name':'HD17'}]
        IR_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('tas_','HD17_')) ,
        output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('HD17 calculated ' )
        outlog = outlog + "HD17 indice processed sucessfully  \n"

      if GD4 == True and "tas" in ds.variables.keys():
        logger.debug('calculation for GD4 started ')
        GD4_file = None
        rd = ocgis.RequestDataset(nc, 'tas') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_GD4','name':'GD4'}]
        IR_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('tas_','GD4_')) , output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('GD4 calculated ' )
        outlog = outlog + "GD4 indice processed sucessfully  \n"
          
      if RR == True and var == "pr" :
        logger.debug('calculation for RR started ')
        RR_file = None
        rd = ocgis.RequestDataset(nc, 'pr') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_RR','name':'RR'}]
        RR_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('pr_','RR_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('RR calculated ' )
        outlog = outlog + "RR indice processed sucessfully  \n"
                  
      if RR1 == True and var == "pr" :
        logger.debug('calculation for RR1 started ')
        RR1_file = None
        rd = ocgis.RequestDataset(nc, 'pr') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_RR1','name':'RR'}]
        RR_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('pr_','RR1_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('RR1 calculated ' )
        outlog = outlog + "RR1 indice processed sucessfully  \n"           
                  
      if CWD == True and var == "pr" :
        logger.debug('calculation for CWD started ')
        CWD_file = None
        rd = ocgis.RequestDataset(nc, 'pr') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_CWD','name':'CWD'}]
        RR_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('pr_','CWD_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('CWD calculated ' )
        outlog = outlog + "CWD indice processed sucessfully  \n"           
                  
      if SDII == True and var == "pr" :
        logger.debug('calculation for SDII started ')
        SDII_file = None
        rd = ocgis.RequestDataset(nc, 'pr') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_SDII','name':'SDII'}]
        SDII_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('pr_','SDII_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('SDII calculated ' )
        outlog = outlog + "SDII indice processed sucessfully  \n"           
                  
      if R10mm == True and var == "pr" :
        logger.debug('calculation for R10mm started ')
        R10mm_file = None
        rd = ocgis.RequestDataset(nc, 'pr') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_R10mm','name':'R10mm'}]
        R10mm_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('pr_','R10mm_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('R10mm calculated ' )
        outlog = outlog + "R10mm indice processed sucessfully  \n"           
                  
      if R20mm == True and var == "pr" :
        logger.debug('calculation for R20mm started ')
        R20mm_file = None
        rd = ocgis.RequestDataset(nc, 'pr') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_R20mm','name':'R20mm'}]
        R20mm_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('pr_','R20mm_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('R20mm calculated ' )
        outlog = outlog + "R20mm indice processed sucessfully  \n"           
                  
      if RX1day == True and var == "pr" :
        logger.debug('calculation for RX1day started ')
        RX1day_file = None
        rd = ocgis.RequestDataset(nc, 'pr') # time_range=[dt1, dt2]
        group = ['year']
        calc_icclim = [{'func':'icclim_RX1day','name':'RX1day'}]
        RX1day_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('pr_','RX1day_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('RX1day calculated ' )
        outlog = outlog + "RX1day indice processed sucessfully  \n"           
                                    
      if RX5day == True and var == "pr" :
        logger.debug('calculation for RX5day started ')
        RX5day_file = None
        rd = ocgis.RequestDataset(nc, 'pr') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_RX5day','name':'RX5day'}]
        RX5day_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('pr_','RX5day_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('RX5day calculated ' )
        outlog = outlog + "RX5day indice processed sucessfully  \n" 
          
      if SD == True and var == "prsn" ::
        logger.debug('calculation for SD started ')
        SD_file = None
        rd = ocgis.RequestDataset(nc, 'prsn') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_SD','name':'SD'}]
        SD_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('prsn_','SD_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('SD calculated ' )
        outlog = outlog + "SD indice processed sucessfully  \n"
          
      if SD1 == True and var == "prsn" ::
        logger.debug('calculation for SD1 started ')
        SD1_file = None
        rd = ocgis.RequestDataset(nc, 'prsn') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_SD1','name':'SD1'}]
        SD1_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('prsn_','SD1_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('SD1 calculated ' )
        outlog = outlog + "SD1 indice processed sucessfully  \n"
          
      if SD5cm == True and var == "prsn" ::
        logger.debug('calculation for SD5cm started ')
        SD5cm_file = None
        rd = ocgis.RequestDataset(nc, 'prsn') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_SD5cm','name':'SD5cm'}]
        SD5cm_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('prsn_','SD5cm_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('SD5cm calculated ' )
        outlog = outlog + "SD5cm indice processed sucessfully  \n"          
          
                  
      if SD50cm == True and var == "prsn" ::
        logger.debug('calculation for SD50cm started ')
        SD50cm_file = None
        rd = ocgis.RequestDataset(nc, 'prsn') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_SD50cm','name':'SD50cm'}]
        SD5cm_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('prsn_','SD50cm_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('SD50cm calculated ' )
        outlog = outlog + "SD50cm indice processed sucessfully  \n"          

      if CDD == True and var == "prsn" ::
        logger.debug('calculation for CDD started ')
        CDD_file = None
        rd = ocgis.RequestDataset(nc, 'prsn') # time_range=[dt1, dt2]
        calc_icclim = [{'func':'icclim_CDD','name':'CDD'}]
        SD5cm_file = ocgis.OcgOperations(dataset=rd, calc=calc_icclim, calc_grouping=group, prefix=(basename.replace('prsn_','CDD_')), output_crs=output_crs, output_format='nc', add_auxiliary_files=False).execute()
        logger.debug('CDD calculated ' )
        outlog = outlog + "CDD indice processed sucessfully  \n"            
    except Exception as e:
      msg = 'processing failed for file  : %s %s ' % ( basename , e)
      logger.error(msg)
      outlog = outlog + msg + '\n'
    logger.debug('processing done for experiment :  %s ' % key  )    
    outlog = outlog + 'processing done for experiment:  %s \n ' % key 
  
  return outlog;
              