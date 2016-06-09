## Classification des donnees de SLP d'EUCLEIA, selon les regimes d'une
## simulation EUCLEIA (WAH_a) ou NCEP
## Pascal Yiou (LSCE) Mai 2014, Novembre 2014
## Demande de faire: module load R/2.15.3 sur obelix
## Se lance apres processing_WAH_20141103.sh
## Fait la classif pour 1 fichier netcdf
## Se lance par:
## R CMD BATCH "--args ${fname} ${refname} ${name.climatol}" /home/users/yiou/RStat/EUCLEIA/classif_WAH_20141106.R
SI=Sys.info()

if(SI[[1]] == "Linux"){
  Rsource="~/sandbox/weatherregime/"
  REFdir="/home/estimr1/EUCLEIA/"
  NCEPdir="/home/estimr1/yiou/estimr1/NCEP/"
  DATdir="/home/estimr1/EUCLEIA/WIN2014/"
  OUTdir="/home/estimr1/EUCLEIA/CLASSIF/"
}

##library(parallel)
library(ncdf)
source(paste(Rsource,"readextranc.R",sep=""))

## Lecture des arguments d'entree
args=(commandArgs(TRUE))
print(args)
if(length(args)>0){
##  filin=args[1]
  fname=args[1]         # repertoire de fichiers a analyser
  refname=args[2]       # regimes de reference
  name.climatol=args[3] # Nom du fichier de climatologie a retirer
}else{
  suff="a"
  refname=paste(NCEPdir,"NCEP_regimes_1948-2014_DJFM.Rdat",sep="")
}

if(!exists("suff")) suff="a"

setwd(REFdir)
## Regimes de reference
##fname="WAH_a/lores_psl_a.1_regimes.Rdata"
## Regimes NCEP
##fname=paste(NCEPdir,"NCEP_regimes_1948-2014_DJFM.Rdat",sep="")
load(refname)
dat.class$reg.var=dat.class$reg.var/100
regime.ref=dat.class$reg.var #+dat.climatol

## Fichiers a classifier
setwd(DATdir)
##commd="ls *.nc"
##lname=system(commd,intern=TRUE)

## Wrapper pour la classif
"wrap.class" = function(fname)
  {
    require(ncdf)
    source(paste(Rsource,"readextranc.R",sep=""))
    varname="field8_1"
    nc = open.ncdf(fname)
    lon = get.var.ncdf(nc,"lon")  ## lon
    lat = get.var.ncdf(nc,"lat")  ## lat
    tt = get.var.ncdf(nc,"time")
    datEUCLEIA=lirevarnc(nc,varname)
    close.ncdf(nc)
    datEUCLEIA$dat=datEUCLEIA$dat/100
    datEUCLEIA$dat[datEUCLEIA$dat<900 | datEUCLEIA$dat>1100]=NA
    if(length(args)==3){
      datEUCLEIA.climatol=scan(file=name.climatol)
    }else{
      datEUCLEIA.climatol=apply(datEUCLEIA$dat,2,mean,na.rm=TRUE)
    }

    datEUCLEIA.anom=t(t(datEUCLEIA$dat)-datEUCLEIA.climatol)

    rms.reg=c()
    for(i in 1:nreg){
##      diffi=t(t(datEUCLEIA$dat)-regime.ref[,i])
      diffi=t(t(datEUCLEIA.anom)-regime.ref[,i])
      diffi=diffi^2
      rms=apply(diffi,1,sum,na.rm=TRUE)
      rms.reg=cbind(rms.reg,rms)
    }
    best.reg=apply(rms.reg,1,which.min)
    dist.reg=sqrt(apply(rms.reg,1,min)/nrow(regime.ref))

    varout=cbind(tt,best.reg,dist.reg)
    namdum=strsplit(fname,".nc")[[1]]
    namdum=rev(strsplit(namdum,"/")[[1]])[1]
    fout=paste(OUTdir,namdum,"_classif-ref.dat",sep="")
    write.table(file=paste(fout,sep=""),varout,col.names=FALSE,
                row.names=FALSE,quote=FALSE)
  }# fin du wrapper



tm=proc.time()
wrap.class(fname)
proc.time()-tm


q("no")
