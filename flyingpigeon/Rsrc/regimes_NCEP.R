# weather regimes NCEP
########################################################################
#                            R Version 3.1.2
########################################################################

rm(list = ls(all = TRUE))
# rm(list=ls())
library(ncdf4)

# fetching the arguments
args <- commandArgs(trailingOnly = TRUE) # pass --args modelname (match to filename)

rworkspace <- args[1]
Rsrc <- args[2]
infile <- args[3]
variable <- args[4]
modelname <- args[5]
yr1 <- as.numeric(args[6]) #1948
yr2 <- as.numeric(args[7]) #2014
output_grphics <- args[8]

print(' *** Here starts the R execution ***')

print( rworkspace )
print( Rsrc )
print( infile)
print( variable )
print( modelname )
print( yr1 )
print( yr2 )
print( output_grphics )

source(paste(Rsrc,"imagecont.R",sep=""))
source(paste(Rsrc,"readextranc.R",sep=""))
source(paste(Rsrc,"classif.R",sep=""))
source(paste(Rsrc,"compu_regimes.R",sep=""))

#open netcdf4
# fname = paste(dirname,"slp.1948-2014_NA.nc",sep="")
nc = nc_open(infile)
datNCEP=lirevarnc(nc,variable)
dat.NCEP.dum=sousseasmean(datNCEP$dat,datNCEP$conv.time,l.year=c(1970:1999))
datNCEP$anom=dat.NCEP.dum$anom
datNCEP$seascyc=dat.NCEP.dum$seascyc
conv.time=datNCEP$conv.time
nc_close(nc)
#ponderate
pond.slp=1/sqrt(cos(datNCEP$lat*pi/180))
scale.slp=rep(pond.slp,length(datNCEP$lon))
lon=datNCEP$lon
lat=datNCEP$lat
# Define months and seasons
l.seas=list(JJA=6:8,SON=9:11,DJF=c(12,1,2),SONDJF=c(9:12,1,2),MAM=3:5,JJAS=6:9,DJFM=c(1:3,12))
seas="JJA"
days=30+31+31
ISEAS=which(datNCEP$conv.time$month %in% l.seas[[seas]])
dat.m=datNCEP$anom[ISEAS,]
data=datNCEP$time[ISEAS]

# Compute PCs
pc.dat=prcomp(dat.m,scale.=scale.slp)
if(class(pc.dat)=="princomp"){
  pc.dum=list(x=pc.dat$scores,rotation=pc.dat$loadings,sdev=pc.dat$sdev)
  pc.dat=pc.dum
  rm(pc.dum)
}

# Classification
library(mclust)
npc=10
if(!exists("nreg")) nreg=4
dat.class=classnorm(pc.dat,nreg=nreg,simuname=paste("NCEP",seas,sep="_"),yr1=1970,yr2=2010,
                    lon=lon,lat=lat,isave=FALSE,iplot=FALSE)
# Frequency of weather regimes 
perc.r=c()
for(i in 1:nreg){
  perc.r=c(perc.r,length(which(dat.class$kmeans$cluster==i))/
             length(dat.class$kmeans$cluster)*100)
}
perc.r.sort=sort(perc.r,index.return=TRUE,decreasing=TRUE)

# frequecy of weather regimes per year in a particular season
# days=92 days
freq.reg=c()
for(yr in yr1:(yr2)){
  count.reg=c()
  for(i in 1:nreg){
    count.reg.i=
      length(which(dat.class$kmeans$cluster[conv.time$year[ISEAS]==yr]==i))
    count.reg=c(count.reg,count.reg.i)
  }
  freq.reg=rbind(freq.reg,count.reg/days)
  
}

# save results
# #setwd(Rdata)
# prefname="slp"
# filout=paste(modelname,"_",seas,"_",nreg,"freq_regimes_",yr1,"-",yr2,"_",prefname,".Rdata",sep="")
# save(file=filout,lon,lat,dat.class,perc.r.sort,nreg,freq.reg)
# timeout=data
# varout=cbind(timeout,dat.class$kmeans$cluster)
# fname=paste(modelname,"_regimes_",yr1,"-",year.end,"_",seas,".dat",sep="")
# write.table(file=fname,varout,col.names=FALSE,
#             row.names=FALSE,quote=FALSE)
# save(file=fname,dat.class,
#      lon,lat,timeout)

############################################################### plots

##### plot EOFs

# setwd(NCEPdir)
# file=paste(modelname,"_",seas,"_",yr1,"-",yr2,"_EOF.pdf",sep="")

pdf(output_grphics)
layout(matrix(1:4,2,2))
for(i in 1:4){
  pourcent=floor(100*pc.dat$sdev[i]^2/sum(pc.dat$sdev^2))
  image.cont(lon,lat,pc.dat$rotation[,i],
             xlab="",ylab="",mar=c(4,4,4,8))
  title(main=paste(modelname,"_EOF",i," ",seas," [",pourcent,"%]",sep=""))
}

# dev.off()
# plot weather regimes

# fname=paste(modelname,"_regimes_",yr1,"-",yr2,"_",seas,".pdf",sep="")
# pdf(file=fname)

layout(matrix(1:(2*ceiling(nreg/2)),2,ceiling(nreg/2)))
par(mar=c(4,6,2,2))
for(i in 1:nreg){ 
  image.cont.mc(lon,lat,dat.class$reg.var[,i],
                xlab="",ylab="",mar=c(2.5,2,2,1),paquet="maps",
                titre=paste("Reg.",i,"(",
                            format(dat.class$perc.r[i],digits=2),"%)"))
}#end for i
dev.off()