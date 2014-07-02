"""
Processes for Anopheles Gambiae population dynamics 
Author: Nils Hempelmann (nils.hempelmann@hzg)
"""
from datetime import datetime, date
import tempfile
import subprocess

#from malleefowl.process import WorkerProcess
import malleefowl.process 
from malleefowl import tokenmgr, utils
from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

class AnophelesProcess(malleefowl.process.WorkerProcess):
    """This process calculates the evapotranspiration following the Pennan Monteith equation"""

    def __init__(self):
        # definition of this process
        malleefowl.process.WorkerProcess.__init__(self, 
            identifier = "de.csc.vbd",
            title="Vector born diseases",
            version = "0.1",
            metadata= [
                       {"title": "Climate Service Center", "href": "http://www.climate-service-center.de/"}
                      ],
            abstract="Collection of models to calculate variables related to vector born diseases",
            extra_metadata={
                  'esgfilter': 'variable:tas, variable:pr, variable:huss, variable:ps,variable:evspsblpot, domain:AFR-44, domain:AFR-44i',  
                  'esgquery': ' time_frequency:day' 
                  },
            )
            
        # Literal Input Data
        # ------------------
        
                           
        self.token = self.addLiteralInput(
            identifier = "token",
            title = "Token",
            abstract = "Your unique token to recieve data",
            minOccurs = 1,
            maxOccurs = 1,
            type = type('')
            )

        
        self.tommymodel = self.addLiteralInput(
            identifier="tommymodel",
            title="Tommy Model",
            abstract="Population dynamics model for Anopheles Gambiae select tas, huss, pr, evspsblpot and ps as input data",
            default=True,
            type=type(False),
            minOccurs=0,
            maxOccurs=1,
            )
        
        #self.kamilmodel = self.addLiteralInput(
            #identifier="kamilmodel",
            #title="Kamil Model",
            #abstract="nothing is implemented here so far ... ",
            #default=False,
            #type=type(False),
            #minOccurs=0,
            #maxOccurs=1,
            #)
        
        
        #self.land_sea_mask = self.addComplexInput(
            #identifier="land_sea_mask",
            #title="land sea mask",
            #abstract="Load land Sea mask from other",
            #metadata=[],
            #minOccurs=1,
            #maxOccurs=1,
            #formats=[{"mimeType":"application/x-netcdf"}],
            #maxmegabites=100
            #)

        self.output = self.addComplexOutput(
            identifier="output",
            title="anopheles",
            abstract="Calculated population dynamics of adult Anopheles Gambiae ",
            formats=[{"mimeType":"application/netcdf"}],
            asReference=True,
            )         
            
    def execute(self):
        from netCDF4 import Dataset
        from os import curdir, path
        import numpy as np
        from cdo import *
        import datetime 
        from math import *
        from ocgis.util.shp_process import ShpProcess
        from ocgis.util.shp_cabinet import ShpCabinetIterator
        import ocgis

        self.show_status('starting anopholes ...', 5)
   
        token = self.token.getValue()
        userid = tokenmgr.get_userid(tokenmgr.sys_token(), token)
        outdir = os.path.join(self.files_path, userid)
        utils.mkdir(outdir)

        self.show_status('got token and outdir : %s ...'% (outdir),  10)
   
        ocgis.env.DIR_SHPCABINET = path.join(path.dirname(__file__),'shapefiles')
        ocgis.env.DIR_OUTPUT = outdir
        ocgis.env.OVERWRITE = True  
        sc = ocgis.ShpCabinet()
        geoms = 'continent'
        select_ugid = [1] # UGID for Africa
        
        self.show_status('got ShapeCabinet selected ugid : %s ...'% (select_ugid),  12)
        

        # guess var names of files
        nc_files = self.get_nc_files()
        for nc_file in nc_files: 
            ds = Dataset(nc_file)
            if "tas" in ds.variables.keys():
                file_tas = nc_file
            elif "huss" in ds.variables.keys():
                file_huss = nc_file
            elif "ps" in ds.variables.keys():
                file_ps = nc_file
            elif "pr" in ds.variables.keys():
                file_pr = nc_file
            elif "evspsblpot" in ds.variables.keys():
                file_evspsblpot = nc_file                          # Dataset(nc_file , 'r')   
            else:
                raise Exception("input netcdf file has not variable tas|hurs|pr|evspsbl")

        logger.debug('get files of var names ... done')
        
        #file_land_sea_mask = self.land_sea_mask.getValue()
        #logger.debug('get landseamask ... done')
        
        # build the n4 out variable based on pr
        rd = ocgis.RequestDataset(file_pr, 'pr') # time_range=[dt1, dt2]
        
        file_n4 = None
        
        try : 
            file_n4 = ocgis.OcgOperations(dataset=rd,  geom=geoms, prefix=str('n4_'), output_format='nc',select_ugid=select_ugid).execute()
            self.show_status('created N4 outfile : %s ...'% (file_n4),  15)
        except Exception as e: 
            logger.exception("Something awful happened! Africa polygon subset failed")
            

        #file_n4 = path.join(path.abspath(curdir), "n4.nc")       
        #cdo.setname('n4', input=file_pr, output=file_n4)
        
        
        nc_tas = Dataset(file_tas,'r')
        nc_pr = Dataset(file_pr,'r')
        nc_ps = Dataset(file_ps,'r')
        nc_huss = Dataset(file_huss,'r')
        nc_evspsblpot = Dataset(file_evspsblpot,'r')
        nc_n4 = Dataset(file_n4,'a')
        
        #nc_land_sea_mask = Dataset(file_land_sea_mask,'r')
        
        logger.debug('open all files ... done')
        
        #change attributes und variable name here 
        # att.put.nc(nc_n4, "n4", "units", "NC_FLOAT", -9e+33)
        ## read in values 

        tas = np.squeeze(nc_tas.variables["tas"])
        pr = np.squeeze(nc_pr.variables["pr"])
        ps = np.squeeze(nc_ps.variables["ps"])
        huss = np.squeeze(nc_huss.variables["huss"])
        evspsblpot = np.squeeze(nc_evspsblpot.variables["evspsblpot"])
        
        
        logger.debug('read in all variables ... done')
        
        var_n4 = nc_n4.variables["n4"]
        n4 = np.zeros(pr.shape, dtype='f')
        
        # define some constatnts:
        Increase_Ta = 0
        #Evaporation (> -8)
        Increase_Et = 0
        #Rainfall (> -150)
        Increase_Rt = 0
        #Relative Humidity in (-97,+39)
        Increase_RH = 0
        ## Text
        deltaT = 6.08
        h0 = 97
        AT = 1.79*10**6
        lamb = 1.5
        m = 1000
        De = 37.1
        Te = 7.7
        Nep = 120
        alpha1 = 280.486
        alpha2 = 0.025616
            
        #if (abs(deltaT)<4):
            #b = 0.89
        #else:
        b = 0.88

        logger.debug('start main loop ...')

        for x in range(0,tas.shape[1],1): #tas.shape[1]
            for y in range(0,tas.shape[2],1): #tas.shape[2]
                try:
                    logger.debug('working on x=%s, y=%s' % (x, y))
                    if (var_n4[0,x,y] >= 0):
                        ## get the appropriate values 
                        #RH = hurs[:,x,y] * 100
                        Ta = tas[:,x,y] -273.15
                        Rt = pr[:,x,y] * 86400.     
                        Et = np.fabs(evspsblpot[:,x,y] * 86400.) # some times evspsblpot ist stored as negaitve value  
                        
                        # calculation of rel. humidity 
                        e_ = ((ps[:,x,y] * huss[:,x,y])/62.2)
                        es = 6.1078*10.**(7.5*(tas[:,x,y]-273.16)/(237.3+(tas[:,x,y]-273.16)))
                        RH = (e_ / es) * 100.
                        
                        #calulation of water temperature
                        Tw = Ta + deltaT
                        
                        ## Check for Values out of range
                        Rt[Rt + Increase_Rt < 0] = 0 
                        Et[Rt + Increase_Rt < 0] = 0
                        RH[RH + Increase_RH < 0] = 0
                        RH[RH + Increase_RH > 100] = 100
                        
                        # create appropriate variabels 
                        D = np.zeros(Ta.size)
                        Vt = np.zeros(Ta.size)
                        p4 = np.zeros(Ta.size)
                        ft = np.zeros(Ta.size)
                        Gc_Ta = np.zeros(Ta.size)
                        F4 = np.zeros(Ta.size)
                        N23 = np.zeros(Ta.size)
                        p_DD = np.zeros(Ta.size)
                        p_Tw = np.zeros([Ta.size,3])
                        p_Rt = np.zeros([Ta.size,3])
                        p_D = np.zeros([Ta.size,3])
                        G = np.zeros([Ta.size,3])
                        P = np.zeros([Ta.size,4])
                        p = np.zeros([Ta.size,4])
                        d = np.zeros([Ta.size,4]) 
                        N = np.zeros([Ta.size,4])

                        # initialize the model
                        Vt[0] = 1000.
                        N[0,0] = N[0,1] = N[0,2] = N[0,3] = 100.

                        # pdb.set_trace()

                        for t in range(0, (Ta.size -1) ,1):
                            #print x, y, t
                            if (Vt[t] == 0) & (Rt[t] == 0):
                                Vt[t+1] = 0
                            else:
                                Vt[t+1] = (Vt[t] + AT*Rt[t]/1000.)*(1 - 3.*Et[t]/h0* (Vt[0]/(Vt[t]+AT*Rt[t]/1000))**(1./3.))
                            if((Vt[t] == 0) & (Rt[t] == 0)):
                                Vt[t+1] = 0
                            else:
                                Vt[t+1] = (Vt[t] + AT*Rt[t]/1000.)*(1 - 3.*Et[t]/h0*(Vt[0]/(Vt[t]+AT*Rt[t]/1000))**(1./3.))
                            
                            if(Vt[t+1] <= 0):
                                Vt[t+1] = 0
                            if (Vt[t+1] == 0):
                                D[t+1] = D[t] + 1
                            else:
                                D[t+1] = 0
                                
                        beta2 = 4*10**(-6)*RH**2 - 1.09*10**(-3)*RH - 0.0255
                        beta1 = -2.32 * 10.**(-4.)* RH**2. + 0.0515*RH + 1.06
                        beta0 = 1.13*10**(-3)*RH**2 - 0.158*RH - 6.61

                        p4 = np.exp(-1/(beta2*Ta**2. + beta1*Ta + beta0))

                        d[:,0] = np.where(Vt != 0, 1.011 + 20.212*(1 + (Tw/12.096)**4.839)**(-1), 1.011 + 20.212*(1 + (Ta/12.096)**4.839)**(-1))
                        d[:,1] = np.where(Vt != 0, 8.130 + 13.794*(1 + (Tw/20.742)**8.946)**(-1) - d[:,0], 8.130 + 13.794*(1 + (Ta/20.742)**8.946)**(-1) - d[:,0])
                        d[:,2] = np.where(Vt != 0, 8.560 + 20.654*(1 + (Tw/19.759)**6.827)**(-1) - d[:,1] - d[:,0] , 8.560 + 20.654*(1 + (Ta/19.759)**6.827)**(-1) - d[:,1] - d[:,0])
                        d[:,3] = -1/np.log(p4)

                        p_Tw[:,0] = np.where(Vt != 0,np.where((Ta >= 14) & (Ta <= 40),np.exp(-1/d[:,0]),0),np.where((Ta >= 25) & (Ta <= 35),np.exp(-1./d[:,0]),0))
                        p_Tw[:,1] = np.where(Vt != 0,np.where((Tw >= 18) & (Tw <= 32),np.exp(-1/d[:,1]),0),np.where((Tw >= 18) & (Tw <= 32),np.exp(-1/d[:,1]),0))
                        p_Tw[:,2] = np.where(Vt != 0,np.where((Tw >= 18) & (Tw <= 32),np.exp(-1/d[:,2]),0),np.where((Tw >= 18) & (Tw <= 32),np.exp(-1/d[:,2]),0))

                        p_Rt[:,0] = np.exp(-0.0242*Rt)
                        p_Rt[:,1] = np.exp(-0.0127*Rt)
                        p_Rt[:,2] = np.exp(-0.00618*Rt)

                        p_D[:,0] = 2*np.exp(-0.405*D)/(1 + np.exp(-0.405*D))
                        p_D[:,1] = 2*np.exp(-0.855*D)/(1 + np.exp(-0.855*D))
                        p_D[:,2] = 2*np.exp(-0.602*D)/(1 + np.exp(-0.602*D))

                        for t in range(0,Rt.size -1,1): #tas.shape[0]
                            if(Vt[t] != 0):
                                p_DD[t] = (b*m/(1000*(N[t,1]+N[t,2])/Vt[t])) * (1 - (lamb**lamb/(lamb +(1000*(N[t,1]+N[t,2])/Vt[t])/m)**lamb))        
                            else:
                                p_DD[t] = 1
                            
                            p[t,0]= p_Tw[t,0]*p_Rt[t,0]*p_D[t,0]
                            p[t,1]= p_Tw[t,1]*p_Rt[t,1]*p_D[t,1]*p_DD[t]
                            p[t,2]= p_Tw[t,2]*p_Rt[t,2]*p_D[t,2]*p_DD[t] 
                            p[t,3]= p4[t]
                            for j in range(0,4,1):
                                P[t,j] = (p[t,j] - p[t,j]**(d[t,j]))/(1. - p[t,j]**d[t,j])
                            for j in range(0,3,1):
                                G[t,j] = (1. - p[t,j])/(1. - p[t,j]**d[t,j])*p[t,j]**d[t,j]

                            ft[t] = 0.518*np.exp(-6.*(N[t,1]/Vt[t] - 0.317)**2.) + 0.192
                            Gc_Ta[t] = 1. + De/(Ta[t] - Te)
                            F4[t] = ft[t]*Nep/Gc_Ta[t]
                            
                            N[t+1,0] = (P[t,0] * N[t,0] + (alpha1 * F4[t]) * N[t,3])
                            N[t+1,1] = (P[t,1] * N[t,1] + G[t,0] * N[t,0])
                            N[t+1,2] = (P[t,2] * N[t,2] + G[t,1] * N[t,1])
                            N[t+1,3] = (P[t,3] * N[t,3] + G[t,2] * N[t,2])

                        N[np.isnan(N)] = 0
                        n4[:,x,y] =  N[:,3] #p4[t] # p_D[t,2] #N[t,3]
                    else:
                        n4[:,x,y] =  float('NaN')
                        
                except Exception as e:
                    logger.warn('Gridbox not calculated. Error=%s' %(e.message))

        # write values into file
        var_n4.assignValue(n4)

        self.show_status("anopheles done", 90)
        self.output.setValue( file_n4 )
        
