## Calcul de la climatologie d'un ensemble de simulations WAH
SI=Sys.info()
if(SI[[1]] == "Darwin"){
  Rsource="/Users/yiou/programmes/RStat/"
  DATdir="/Users/yiou/data/EUCLEIA/"
  OUTdir="/Users/yiou/data/EUCLEIA/"
}
if(SI[[1]] == "Linux"){
  Rsource="/home/users/yiou/RStat/"
  DATdir="/home/estimr1/yiou/estimr1/EUCLEIA/"
  NCEPdir="/home/estimr1/yiou/estimr1/NCEP/"
  OUTdir="/home/estimr1/yiou/estimr1/EUCLEIA/"
}

library(ncdf)
source(paste(Rsource,"readextranc.R",sep=""))
suff="a"

setwd(paste(DATdir,"WAH_",suff,sep=""))
commd=paste("ls lores_psl_",suff,".*.nc",sep="")
lname=system(commd,intern=TRUE)

datX.mean=c()
varname="field8_1"
for(fname in lname){
    print(paste("Processing",fname))
  nc = open.ncdf(fname)
  lon = get.var.ncdf(nc,"lon")  ## lon
  lat = get.var.ncdf(nc,"lat")  ## lat
  tt = get.var.ncdf(nc,"time")
  datEUCLEIA=lirevarnc(nc,varname)
  close.ncdf(nc)
  datEUCLEIA$dat=datEUCLEIA$dat/100
  datEUCLEIA$dat[datEUCLEIA$dat<900 | datEUCLEIA$dat>1100]=NA
  dum=apply(datEUCLEIA$dat,2,mean,na.rm=TRUE)
  datX.mean=rbind(datX.mean,dum)
}

meanX=apply(datX.mean,2,mean,na.rm=TRUE)
