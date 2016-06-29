# computing weather regimes for NCEP in a reference Period (1970-2010)
# by Pascal Yiou & Carmen Alvarez-Castro
rm(list=ls())
ptm <- proc.time()# starting time script
library(ncdf4)
library(mclust)
library(maps)
NCEPdir="/home/estimr2/calvarez/birdhouse/"
Results=NCEPdir
source(paste(NCEPdir,"classnorm.R",sep=""))
varname="slp"
modelname="NCEP"

seas="JJA"

#reference period
y1=1970
y2=2010

#open netcdf4
nc_anom= '/home/estimr2/nhempelmann/ea4e5ea8-3df9-11e6-b034-0756a0266937.nc'
nc = nc_open(nc_anom)
data=ncvar_get(nc,varname)
lon=ncvar_get(nc,'lon')
lat=ncvar_get(nc,'lat')
#time=ncvar_get(nc,'time')
nx=dim(data)[2];ny=dim(data)[1]
nt=dim(data)[3]
# reshape order lat-lon-time
dat = data*NA; dim(dat) <- c(nt,ny,nx)
for (i in 1:nt) dat[i,,] <- t(as.matrix(data[,,i]))
# two dimentions
dim(dat)=c(nt,nx*ny)
nc_close(nc)

dat.m=dat
#Normalization by latitude by latitute

pond.slp=1/sqrt(cos(lat*pi/180))
scale.slp=rep(pond.slp,length(lon))

# Calculating PCs
pc.dat=prcomp(dat.m,scale.=scale.slp)

## Saving the first 10 EOFs/PCs/variance
# filout=paste(Results,varname,"_PC_",seas,"_clim.dat",sep="")
npc=10
# write.table(file=filout,cbind(time[ISEAS],pc.dat$x[,1:npc]),quote=FALSE,
#             col.names=FALSE,row.names=FALSE)
# filout=paste(Results,varname,"_vap_",seas,"_clim.dat",sep="")
# cat(file=filout,pc.dat$sdev^2)
filout=paste(Results,varname,"_EOF_",seas,"_clim.dat",sep="")
write.table(file=filout,pc.dat$rotation[,1:npc],quote=FALSE,
            col.names=FALSE,row.names=FALSE)

## Classification using k-means approach
nreg=4
dat.class=classnorm(pc.dat,nreg=nreg,npc=10,lat=lat,lon=lon)
## RMS related to the centroids
dat.rms=c()
for(i in 1:nrow(dat.m)){
  diff=dat.m[i,]-dat.class$reg.var[,dat.class$kmeans$cluster[i]]
  rms=sqrt(sum(diff^2)/ncol(dat.m))/100
  dat.rms=c(dat.rms,rms)
}
## Spatial Correlation to the centroids
dat.cor=c()
for(i in 1:nrow(dat.m)){
  cor.r=cor(dat.m[i,],dat.class$reg.var[,dat.class$kmeans$cluster[i]],
            method="spearman")
  dat.cor=c(dat.cor,cor.r)
}

# Plotting Weather regimes
fname=paste(Results,"NCEP_regimes_py",y1,"-",y2,"_",seas,".pdf",sep="")
pdf(file=fname)
layout(matrix(1:(2*ceiling(nreg/2)),2,ceiling(nreg/2)))
par(mar=c(4,6,2,2))
for(i in 1:nreg){ 
   champ=dat.class$reg.var[,i]/100                        
    zlev=pretty(champ,20)
    colplot=rainbow(length(zlev)-1,start=3/6,end=1)
    par( mar=c(2.5,2,2,1))
    
    dum=t(matrix(champ,length(lat),length(lon)))
    #dum=matrix(champ,length(lon),length(lat)) #if transpose
    lat.sort=sort(lat,index.return=TRUE)
    titleplot=paste(modelname,"(",y1,"-",y2,") Reg.",i,"(",
                           format(dat.class$perc.r[i],digits=3),"%)")
    contour(lon,sort(lat),dum[,lat.sort$ix],
            xlab="Longitude",ylab="Latitude",main=titleplot,col=colplot,add=FALSE,nlevels=length(zlev),
            levels=zlev,lty=1)
    library(maps)
    map(add=TRUE)
}#end for i
dev.off()


## Saving the classification of Weather Regimes that we will use for projections
timeout=datNCEP$time[ISEAS]
fname=paste(Results,"NCEP_regimes_",y1,"-",y2,"_",seas,".Rdat",sep="")
save(file=fname,dat.class,lon,lat,timeout,nreg,dat.climatol,dat.rms,dat.cor,mean.clim.ref)
proc.time() - ptm #ending time script
#end