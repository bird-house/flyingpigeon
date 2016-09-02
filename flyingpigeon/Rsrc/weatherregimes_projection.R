#Computing weather regimes in a dataset (here NCEP) projecting NCEP-EOFs during a reference period (1970-2010) 
#by Pascal Yiou & Carmen Alvarez-Castro

rm(list = ls(all = TRUE))
# rm(list=ls())
ptm <- proc.time()# starting time script
library(ncdf4)
library(mclust)
library(maps)
library(fields)

# fetching the arguments
args <- commandArgs(trailingOnly = TRUE) 

rworkspace <- args[1]
Rsrc <- args[2]
infile <- args[3] 
varname <- args[4]
time <- args[5]
#output_graphics <- args[5]
file_pca <- args[6]
file_Rdat <- args[7]
output_pca <- args[8]
output_Rdat <- args[9]
output_frec_percent <- args[10]
seas <- args[11]
y1 <- as.numeric(args[12])
y2 <- as.numeric(args[13])
modelname <- args[14]

# Convert variables 
yr1 = as.numeric(y1)
yr2 = as.numeric(y2)
timeout  <- as.integer(unlist(strsplit(time,",")))

source(paste(Rsrc,"classnorm.R",sep=""))
source(paste(Rsrc,"libraryregimes.R",sep=""))

print(' Arguments functions loaded ')
print('Time from netCDF:')
print(timeout)

## load EOFs for thereference period 
load(file_Rdat)
EOF.r=read.table(file=file_pca)
n.eof=ncol(EOF.r)

# open prepared netCDF file for projection
nc = nc_open(infile)
data=ncvar_get(nc,varname)
lon=ncvar_get(nc,'lon')
lat=ncvar_get(nc,'lat')
# time=ncvar_get(nc,'time')
# print('Time from netCDF R:')
# print(time)

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

# #reproduce the time
# ical=366 
# 
# if(ical==366){
#   year=c()
#   month=c()
#   day=c()
#   monthdum=c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),
#              rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
#   monthdumB=c(rep(1,31),rep(2,29),rep(3,31),rep(4,30),rep(5,31),rep(6,30),
#               rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
#   daydum=c((1:31),(1:28),(1:31),(1:30),(1:31),(1:30),(1:31),(1:31),(1:30),(1:31),(1:30),(1:31))
#   daydumB=c((1:31),(1:29),(1:31),(1:30),(1:31),(1:30),(1:31),(1:31),(1:30),(1:31),(1:30),(1:31))
#   yr=c(yr1:yr2)
#   bi=c(1:(yr2-yr1+1))
#   for(i in 1:(yr2-yr1+1)){
#     if ((yr[i]/4)==trunc(yr[i]/4)){
#       bi[i]=yr[i]
#       year=c(year,rep(yr[i],length=length(monthdumB)))
#       month=c(month,monthdumB)
#       day=c(day,daydumB)}
#     
#     else{
#       bi[i]=NaN
#       year=c(year,rep(yr[i],length=length(monthdum)))
#       month=c(month,monthdum)
#       day=c(day,daydum)}
#   }
#   month=array(month[1:(length(month))])
#   day=array(day[1:(length(day))])
#   year=array(year[1:(length(year))])
#   conv.time=list(month=month,day=day,year=year)
#   time=array((year)*10000+month*100+day)
# }
# if(ical==365){
#   year=c()
#   month=c()
#   day=c()
#   monthdum=c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),
#              rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
#   daydum=c((1:31),(1:28),(1:31),(1:30),(1:31),(1:30),(1:31),(1:31),(1:30),(1:31),(1:30),(1:31))
#   yr=c(yr1:yr2)
#   for(i in 1:(yr2-yr1+1)){
#     year=c(year,rep(yr[i],length=length(monthdum)))
#       month=c(month,monthdum)
#       day=c(day,daydum)}
#   month=array(month[1:(length(month))])
#   day=array(day[1:(length(day))])
#   year=array(year[1:(length(year))])
#   conv.time=list(month=month,day=day,year=year)
#   time=array((year)*10000+month*100+day)
# }
# if (ical==360){
#   ndyear=360
#   nmyear=12
#   lmo=1:12
#   nyear=yr2-yr1+1
#   day=rep(1:30,times=nmyear*nyear)
#   month=rep(rep(lmo,each=30),times=nyear)
#   year=rep(y1:y2,each=ndyear)
#   conv.time=list(year=year,month=month,day=day)
#   time=array((year)*10000+month*100+day)
# }

#select season
# seas="JJA"

# l.seas=list(JJA=6:8,SON=9:11,MAM=3:5,DJF=c(12,1,2))
# ISEAS=which(conv.time$month %in% l.seas[[seas]] &
#               conv.time$year %in% c(yr1:yr2))
# time during the selected period and season
# timeout=time[ISEAS]

## Calculate projections for EOFs(PC empirics)
## Normalization by latitude
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

#Frequencies of Weather regimes for each "seas"
# timeout=time[ISEAS]

yyyy=floor(timeout/10000)
mm=floor(timeout/100) %% 100
yyyy[mm==12]=yyyy[mm==12]+1
uyyyy=sort(unique(yyyy))
yyyymm=yyyy*100+mm
dum=rep(1,length=length(yyyymm))
reg.freq=c()
reg.freq.total=c()
for(i in 1:nreg){
  tdum=tapply(dum[best.reg==i],yyyy[best.reg==i],length)
  ldum=tapply(dum,yyyy,length)
  rdum=rep(0,length=length(uyyyy))
  rdum[uyyyy %in% as.numeric(names(tdum))]=tdum
  rdum=rdum/ldum
  reg.freq=cbind(reg.freq,rdum)
  reg.freq.total[i]=(sum(reg.freq[,i])/length(uyyyy))*100
}

WR.freq <- as.data.frame(reg.freq*100)
WR.freq$year <- uyyyy

name.reg=c("Reg.1","Reg.2","Reg.3","Reg.4")
names(WR.freq) <- c(name.reg,"year")
total=nrow(WR.freq)+1
WR.freq[total,] <- c(reg.freq.total,"total")

## Save frequencies of Weather Regimes per seas in Results
write.table(file=output_frec_percent,WR.freq,quote=FALSE,row.names=FALSE)

## Saving the workspace 
save.image(file=output_Rdat) #lon,lat,time,

proc.time() - ptm

# print ('frequencies saved to file')
# ## Plotting Weather regimes of the model
# # fname=paste(Results,"projNCEP_regimes_",modelname,"_",yr1,"-",yr2,"_",seas,".pdf",sep="")
# pdf(file=output_graphics, height=8, width=11)
# layout(matrix(1:(2*ceiling(nreg/2)),2,ceiling(nreg/2)))
# for(i in 1:nreg){ 
#   champ=dat.class$reg.var[,i]/100                        
  
#   zlev=pretty(champ,20)
#   colplot=rainbow(length(zlev)-1,start=3/6,end=1)
#   par( mar=c(2.5,2,2,1.5))
  
#   dum=t(matrix(champ,length(lat),length(lon)))
#   #dum=matrix(champ,length(lon),length(lat)) #if transpose
#   lat.sort=sort(lat,index.return=TRUE)
#   titleplot=paste("NCEP(",y1,"-",y2,")",seas,"",name.reg[i],"(",
#                   format(dat.class$perc.r[i],digits=3),"%)")
  
#   contour(lon,sort(lat), dum[,lat.sort$ix],
#           xlab="Longitude",ylab="Latitude",main=titleplot,col=colplot,
#           add=FALSE,nlevels=length(zlev),
#           levels=zlev,lty=1, cex.main=0.95)
#   library(fields)
#   world(xlim=range(lon),ylim=range(lat),add=TRUE)
# }
# pdf(file='test.pdf')
# library(gridExtra)
# pdf("trade.pdf", height=11, width=8.5)
# library(grid)
# library(gridExtra)
# grid.newpage()
# grid.table(WR.freq)
# dev.off()

# proc.time() - ptm 






