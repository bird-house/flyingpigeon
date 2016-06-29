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
infile <- args[3] # '/home/estimr2/nhempelmann/ea4e5ea8-3df9-11e6-b034-0756a0266937.nc' #args[3]
variable <- args[4]
output_graphics <- args[5]
file_pca <- args[6]
file_classification <- args[7]
season <- args[8]

print(' *** Here starts the R execution ***')

print( rworkspace )
print( Rsrc )
print( infile)
print( variable )
print( output_graphics )

source(paste(Rsrc,"classnorm.R",sep=""))
varname=variable
seas=season

nc = nc_open(infile)

data=ncvar_get(nc,varname)
lon=ncvar_get(nc,'lon')
lat=ncvar_get(nc,'lat')
time=ncvar_get(nc,'time')
nx=dim(data)[2];ny=dim(data)[1]
nt=dim(data)[3]
# reshape order lat-lon-time
dat = data*NA; dim(dat) <- c(nt,ny,nx)
for (i in 1:nt) dat[i,,] <- t(as.matrix(data[,,i]))
# two dimentions
dim(dat)=c(nt,nx*ny)
nc_close(nc)

dat.m=dat

print( 'data sucessfully loaded' )

## SLP Climatology 
dat.climatol=apply(data/100,2,mean,na.rm=TRUE)
mean.clim.ref=mean(dat.climatol)

#Normalization by latitude by latitute

pond.slp=1/sqrt(cos(lat*pi/180))
scale.slp=rep(pond.slp,length(lon))

print( 'ponderation calculated' )

# Calculating PCs
pc.dat=prcomp(dat.m, scale.=scale.slp)

print( 'principal components calculated' )
npc=10

# write.table(file=filout,cbind(time[ISEAS],pc.dat$x[,1:npc]),quote=FALSE,
#             col.names=FALSE,row.names=FALSE)
# filout=paste(Results,varname,"_vap_",seas,"_clim.dat",sep="")
# cat(file=filout,pc.dat$sdev^2)
#filout='/home/nils/birdhouse/flyingpigeon/textfile.txt'

write.table(file=file_pca,pc.dat$rotation[,1:npc],quote=FALSE,
            col.names=FALSE,row.names=FALSE)

print( 'data table written' )

## Classification using k-means approach
#iplot=TRUE for pre-visualization before save the plot
nreg=4
dat.class=classnorm(pc.dat,nreg=nreg,npc=10,lat=lat,lon=lon)

print( 'classification done' )
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
# timeout=datNCEP$time[ISEAS]
#fname=paste(Results,"NCEP_regimes_",y1,"-",y2,"_",seas,".Rdat",sep="")

save(file=file_classification,dat.class,lon,lat,time,nreg,dat.climatol,dat.rms,dat.cor,mean.clim.ref)
proc.time() - ptm #ending time script
