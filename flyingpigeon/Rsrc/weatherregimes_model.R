# calcultion of weather regimes for a single model dataset
#
#
########################################################################
#                            R Version 3.1.2
########################################################################

rm(list = ls(all = TRUE))
# rm(list=ls())
ptm <- proc.time()# starting time script
library(ncdf4)
library(mclust)
library(maps)

# fetching the arguments
args <- commandArgs(trailingOnly = TRUE) 

rworkspace <- args[1]
Rsrc <- args[2]
infile <- args[3]
variable <- args[4]
#y1 <- as.numeric(args[5]) #1948
#y2 <- as.numeric(args[6]) #2014
output_graphics <- args[5]
file_pca <- args[6]
file_classification <- args[7]
season <- args[8]

print(' *** Here starts the R execution ***')

print( rworkspace )
print( Rsrc )
print( infile)
print( variable )
#print( yr1 )
#print( yr2 )
print( output_graphics )

source(paste(Rsrc,"libraryregimes.R",sep=""))
varname=variable
#yr1=yr1
#yr2=yr2
seas=season

#reference period
# y1=
# y2=2010

#open netcdf4
# fname = paste(dirname,"slp.1948-2014_NA.nc",sep="")

# nc = nc_open(infile)
# datNCEP=lirevarnc(nc,variable)
# dat.NCEP.dum=sousseasmean(datNCEP$dat,datNCEP$conv.time,l.year=c(y1:y2))
# datNCEP$anom=dat.NCEP.dum$anom
# datNCEP$seascyc=dat.NCEP.dum$seascyc
# conv.time=datNCEP$conv.time
# nc_close(nc)

# # Define months and seasons
# l.seas=list(JJA=6:8,SON=9:11,DJF=c(12,1,2),SONDJF=c(9:12,1,2),MAM=c(3,4,5),all=c(1:12),
#             JJAS=6:9,DJFM=c(1:3,12),MAMJ=3:6,FMA=c(2,3,4),DJFM=c(12,1,2,3),MAMJ=c(3:6),
#             JJAS=c(6:9),SOND=c(9:12))
# ISEAS=which(datNCEP$conv.time$month %in% l.seas[[seas]] &
#               datNCEP$conv.time$year %in% c(y1:y2))
# dat.m=datNCEP$anom[ISEAS,]
# print(paste("Classification for",seas))

nc = nc_open(infile)
datNCEP=lirevarnc(nc,varname)
nc_close(nc)

## SLP Climatology 
dat.climatol=apply(datNCEP$dat/100,2,mean,na.rm=TRUE)
mean.clim.ref=mean(dat.climatol)

#Normalization by latitude by latitute
lon=datNCEP$lon
lat=datNCEP$lat
time=datNCEP$time
pond.slp=1/sqrt(cos(lat*pi/180))
scale.slp=rep(pond.slp,length(lon))

# Calculating PCs
pc.dat=prcomp(dat.m,scale.=scale.slp)

npc=10
# write.table(file=filout,cbind(time[ISEAS],pc.dat$x[,1:npc]),quote=FALSE,
#             col.names=FALSE,row.names=FALSE)
# filout=paste(Results,varname,"_vap_",seas,"_clim.dat",sep="")
# cat(file=filout,pc.dat$sdev^2)
#filout='/home/nils/birdhouse/flyingpigeon/textfile.txt'

write.table(file=file_pca,pc.dat$rotation[,1:npc],quote=FALSE,
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


############################################################### plots
##### plot EOFs

pdf(output_graphics)

## Plotting Weather regimes
layout(matrix(1:(2*ceiling(nreg/2)),2,ceiling(nreg/2)))
par(mar=c(4,6,2,2))
for(i in 1:nreg){ 
  image.cont.mc(lon,lat,dat.class$reg.var[,i],
                xlab="",ylab="",mar=c(2.5,2,2,1),paquet="maps",
                titre=paste("Reg.",i,"(",
                            format(dat.class$perc.r[i],digits=2),"%)"))
}#end for i
dev.off()

## Saving the classification of Weather Regimes that we will use for projections
timeout=datNCEP$time[ISEAS]
#fname=paste(Results,"NCEP_regimes_",y1,"-",y2,"_",seas,".Rdat",sep="")

save(file=file_classification,dat.class,lon,lat,timeout,nreg,dat.climatol,dat.rms,dat.cor,mean.clim.ref)
proc.time() - ptm #ending time script

