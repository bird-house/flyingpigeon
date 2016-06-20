# computing weather regimes for NCEP in a reference Period (1970-2010)
# by Pascal Yiou & Carmen Alvarez-Castro
rm(list=ls())
ptm <- proc.time()# starting time script
library(ncdf4)
library(mclust)
library(maps)
NCEPdir="/home/estimr2/calvarez/birdhouse/libraryregimes.R"
Results=NCEPdir
source(paste(NCEPdir,"libraryregimes.R",sep=""))
varname="slp"
modelname="NCEP"
yr1=1948
yr2=2014
seas="JJA"

#reference period
y1=1970
y2=2010

#open netcdf4
fname = paste(NCEPdir,"slp.",yr1,"-",yr2,"_NA.nc",sep="")
nc = nc_open(fname)
datNCEP=lirevarnc(nc,varname)
dat.NCEP.dum=sousseasmean(datNCEP$dat,datNCEP$conv.time,l.year=c(1970:1999))
datNCEP$anom=dat.NCEP.dum$anom
datNCEP$seascyc=dat.NCEP.dum$seascyc
conv.time=datNCEP$conv.time
nc_close(nc)

# Define months and seasons
l.seas=list(JJA=6:8,SON=9:11,DJF=c(12,1,2),SONDJF=c(9:12,1,2),MAM=c(3,4,5),all=c(1:12),
            JJAS=6:9,DJFM=c(1:3,12),MAMJ=3:6,FMA=c(2,3,4),DJFM=c(12,1,2,3),MAMJ=c(3:6),
            JJAS=c(6:9),SOND=c(9:12))
ISEAS=which(datNCEP$conv.time$month %in% l.seas[[seas]] &
              datNCEP$conv.time$year %in% c(y1:y2))
dat.m=datNCEP$anom[ISEAS,]
print(paste("Classification for",seas))

## SLP Climatology 
dat.climatol=apply(datNCEP$dat[ISEAS,]/100,2,mean,na.rm=TRUE)
mean.clim.ref=mean(dat.climatol)

#Normalization by latitude by latitute
lon=datNCEP$lon
lat=datNCEP$lat
time=datNCEP$time
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
#iplot=TRUE for pre-visualization before save the plot
nreg=4
dat.class=classnorm(pc.dat,nreg=nreg,npc=10,lat=lat,lon=lon,iplot=TRUE)
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

## Plotting Weather regimes
# fname=paste(Results,"NCEP_regimes_",y1,"-",y2,"_",seas,".pdf",sep="")
# pdf(file=fname)
# layout(matrix(1:(2*ceiling(nreg/2)),2,ceiling(nreg/2)))
# par(mar=c(4,6,2,2))
# for(i in 1:nreg){ 
#   image.cont.mc(lon,lat,dat.class$reg.var[,i]/100,
#                xlab="",ylab="",mar=c(2.5,2,2,1),paquet="maps",
#                titre=paste(modelname,"(",y1,"-",y2,") Reg.",i,"(",
#                            format(dat.class$perc.r[i],digits=3),"%)"))
# }#end for i
# dev.off()

## Saving the classification of Weather Regimes that we will use for projections
timeout=datNCEP$time[ISEAS]
fname=paste(Results,"NCEP_regimes_",y1,"-",y2,"_",seas,".Rdat",sep="")
save(file=fname,dat.class,lon,lat,timeout,nreg,dat.climatol,dat.rms,dat.cor,mean.clim.ref)
proc.time() - ptm #ending time script
#end

