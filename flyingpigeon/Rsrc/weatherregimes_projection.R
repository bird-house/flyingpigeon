#Computing weather regimes in a dataset (here NCEP) projecting NCEP-EOFs during a reference period (1970-2010) 
#by Pascal Yiou & Carmen Alvarez-Castro

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
varname <- args[4]
output_graphics <- args[5]
file_pca <- args[6]
file_Rdat <- args[7]
output_pca <- args[8]
output_Rdat <- args[9]
seas <- args[10]
y1 <- args[11]
y2 <- args[12]
model_var <- args[13]

source(paste(Rsrc,"classnorm.R",sep=""))

#birdhouse_output = '~/.conda/envs/birdhouse/var/lib/pywps/outputs/flyingpigeon/'
#Rdat=paste(birdhouse_output,'output_classification-9041c716-3f83-11e6-ae14-4f6097e9069b.Rdat',sep="")
#print(paste("Reading",Rdat,"for reference WR"))

load(file_Rdat)

## load EOFs for the reference period (NCEP 1970-2010)
#dat = paste(birdhouse_output,'output_pca-9041c716-3f83-11e6-ae14-4f6097e9069b.dat',sep="")
# fname=paste(NCEPdir,varname,"_EOF_",seas,"_clim.dat",sep="")
#print(paste("Reading",dat,"for EOFs"))

EOF.r=read.table(file=file_pca)
n.eof=ncol(EOF.r)

# #open netcdf4
# fname = paste(NCEPdir,"slp.",yr1,"-",yr2,"_NA.nc",sep="")
# nc = nc_open(fname)
# datNCEP=lirevarnc(nc,varname)
# dat.NCEP.dum=sousseasmean(datNCEP$dat,datNCEP$conv.time,l.year=c(1970:1999))
# datNCEP$anom=dat.NCEP.dum$anom
# datNCEP$seascyc=dat.NCEP.dum$seascyc
# conv.time=datNCEP$conv.time
# nc_close(nc)
# 
# #Define months and seasons
# #seas=JJA
# l.seas=list(JJA=6:8,SON=9:11,DJF=c(12,1,2),SONDJF=c(9:12,1,2),MAM=c(3,4,5),all=c(1:12),
#             JJAS=6:9,DJFM=c(1:3,12),MAMJ=3:6,FMA=c(2,3,4),DJFM=c(12,1,2,3),MAMJ=c(3:6),JJAS=c(6:9),SOND=c(9:12))
# ISEAS=which(datNCEP$conv.time$month %in% l.seas[[seas]])
# dat.m=datNCEP$anom[ISEAS,]

#open netcdf4
#infile = '~/birdhouse/flyingpigeon/notebooks/6f412034-3fa1-11e6-8fa8-25c1f77d80b4.nc'
#varname = 'psl'

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



# print(paste("Classification for",seas))
# 
# ## SLP Climatology and ponderation by latitude
# dat.climatol=apply(datNCEP$dat[ISEAS,]/100,2,mean,na.rm=TRUE)
# mean.clim=mean(dat.climatol)

#correction:removing spatial average. To be used with models or different datasets
#mean.clim.ref=1013.96389304258  #NCEP(1970-2010)
# dif.mean=mean.clim.ref-mean.clim
# dat.m=dat.m+dif.mean

## Calculate projections for EOFs(PC empirics)
## Normalization by latitude
# lon=datNCEP$lon
# lat=datNCEP$lat
pond.slp=1/sqrt(cos(lat*pi/180))
scale.slp=rep(pond.slp,length(lon))
dat.scale=scale(dat.m,scale=scale.slp)
## Projection of EOFs (1970-2010)
PC.e = dat.scale %*% as.matrix(EOF.r)

# compute distance, correlation and best WR
rms.reg=c()
cor.reg=c()
for(i in 1:nreg){
  diffi=t(t(PC.e) - dat.class$kmeans$centers[i,])
    diffi=diffi^2
    rms=apply(diffi,1,sum,na.rm=TRUE)
    dum=cor(t(dat.m),dat.class$reg.var[,i])
    rms.reg=cbind(rms.reg,rms)
    cor.reg=cbind(cor.reg,dum)
}
best.reg=apply(rms.reg,1,which.min)
dist.reg=sqrt(apply(rms.reg,1,min)/nrow(dat.m))

# Save classification in Results
timeout=time # datNCEP$time[ISEAS]
varout=cbind(timeout,best.reg,dist.reg,cor.reg)

fname= output_pca # paste(Results,"NCEP_SLP_",seas,"_",yr1,"-",yr2,"_classif.dat",sep="")
header=c("Time","Best.reg","rms","Cor1","Cor2","Cor3","Cor4")
write.table(file=fname,varout,quote=FALSE,row.names=FALSE,col.names=header)

#Frequencies of Weather regimes for each "seas"
R=read.table(fname,header=TRUE)
yyyy=floor(R$Time/10000)
mm=floor(R$Time/100) %% 100
yyyy[mm==12]=yyyy[mm==12]+1
uyyyy=sort(unique(yyyy))
yyyymm=yyyy*100+mm
dum=rep(1,length=length(yyyymm))
reg.freq=c()
for(i in 1:nreg){
  tdum=tapply(dum[R$Best.reg==i],yyyy[R$Best.reg==i],length)
  ldum=tapply(dum,yyyy,length)
  rdum=rep(0,length=length(uyyyy))
  rdum[uyyyy %in% as.numeric(names(tdum))]=tdum
  rdum=rdum/ldum
  reg.freq=cbind(reg.freq,rdum)
}
WR.freq <- as.data.frame(reg.freq*100)
WR.freq$year <- uyyyy
#imposing names. Attention!! check names!
name.reg=c("NAO-","AL","BLO","AR")
#name.reg=c("Reg.1","Reg.2","Reg.3","Reg.4")
names(WR.freq) <- c(name.reg,"year")

## Save frequencies of Weather Regimes per seas in Results
fname=output_Rdat  # paste(Results,"frec_percent_WR_",modelname,"_SLP_",seas,"_",yr1,"-",yr2,".dat",sep="")
write.table(file=fname,WR.freq,quote=FALSE,row.names=FALSE)

## Plotting Weather regimes

fname=output_graphics # paste(Results,"projcted_regimes_",yr1,"-",yr2,"_",seas,".pdf",sep="")
pdf(file=fname)
layout(matrix(1:(2*ceiling(nreg/2)),2,ceiling(nreg/2)))
par(mar=c(4,6,2,2))
for(i in 1:nreg){ 
  image.cont.mc(lon,lat,dat.class$reg.var[,i]/100,
                xlab="",ylab="",mar=c(2.5,2,2,1),paquet="maps",
                titre=paste(modelname,"(",seas," ",y1,"-",y2,")",name.reg[i],"(",
                            format(dat.class$perc.r[i],digits=3),"%)"))
}#end for i
dev.off()
proc.time() - ptm #ending time script
#end

