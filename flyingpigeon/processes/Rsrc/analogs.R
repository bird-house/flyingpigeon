## Calcul d'analogues de circulation sur la SLP sur les
## reanalyses NCEP pour les USA et l'Atlantique Nord
## 
## Pascal Yiou (LSCE), Decembre 2012, modifications Janvier 2013, Juillet 2013
## Octobre 2013 (analogues a fenetres), Avril 2014, Septembre 2014
## Utilise le package "parallel" ditribue en standard dans les distributions
## de R
## Version 1.0
## Se lance en BATCH par:
## qsub -q mediump -l nodes=1:ppn=12 /home/users/yiou/RStat/A2C2/analogs_slp-genericpar.sh

print ('start test skript')

library(ncdf) # Pour lire les fichiers netcdf d'entree
library(parallel) # Pour la parallelisation

#clean the working dir:
rm(list = ls(all = TRUE))

# get the arguments
# Rcmd = 'R --vanilla --args %s %s %s %i %i %s %s <  %s > %s ' %  (ret, dateSt.date(), dateEn.date(), refSt.year, refEn.year, Rsource, curdir, Rskript , Rlog )
args <- commandArgs(trailingOnly = TRUE)

print(args)

fname  <- args[1]
dateSt <- args[2]
dateEn <- args[3]
refSt  <- as.integer(args[4])
refEn  <- as.integer(args[5])
Rsource <- args[6]
wdir <- args[7]

setwd(wdir)
SI=Sys.info()
print (SI)

# if(SI[['nodename']] == "lsce3199.extra.cea.fr"){
Rsource=Rsource # "/home/nils/birdhouse/flyingpigeon/flyingpigeon/processes/"
DATdir= wdir
OUTdir= dir.create(file.path(DATdir, 'RoutDir'))
sink('./RoutDir/Rlog.log', split = TRUE)

# }

print ('got the sources directories')
print (Rsource)
print (OUTdir)

source(paste(Rsource,"readextranc.R",sep="")) #netcdf file functions
source(paste(Rsource,"analogfun.R",sep="")) #Analogue functions

print ('load the sources')

## INITIALISATIONS des parametres d'analyse
## Lecture des arguments d'entree
#args=(commandArgs(TRUE))
#print(args)
#suffana=""
#if(length(args)>10){
#  region=args[1]
#  nfen=as.integer(args[2])
#  if(length(args)>2){
#    lyear.ref=scan(file=args[3])
#    suffana="hiSST"
#  }
#}else{
region="NA"
nfen=1
#}

#dirname=paste(DATdir,"NCEP/",sep="")

ipara=TRUE # Parallelisation
#nproc=12 # Nombre de processeurs

yrstart= 1948
year.start=1948

yr.now=as.integer(format(Sys.time(), "%Y")) # Annee en cours
print ( yr.now )

year.end = yr.now
## PARAMETRES CHANGEABLES
prefname = "slp"
varname = "slp"
## Faut-il soustraire le cycle saisonnier?
seacycnorm=TRUE
## Taille de la demi-fenetre de calcul d'analogues (par defaut 30 jours)
nwindow=30
## Nombre d'analogues (par defaut 20)
nanalog=20
## Type de correlation (par defaut "corrnk"=par rang, "spearman"=lineaire,
## "euclid"= -distance euclidienne)
## Par defaut, on calcule tous les analogues possibles
##l.method=c("spearman","pearson","euclid")
##l.method=c("corrnk","rms","esv","mahalanobis2")
l.method=c("rms","corrnk")
method="rms"
print(paste("Calculating analogues:",year.start,"-",yr.now,
            "nwindow=",nfen,"region=",region))

## FIN DES PARAMETRES CHANGEABLES
## Nom du fichier d'entree
#fname = paste(dirname,prefname,".",year.start,"-",year.end,"_",region,
#  ".nc",sep="")
print(paste("Processing",fname))

nc = open.ncdf(fname)
varnc=nc$var[[varname]]
varsize=varnc$varsize
ndims=varnc$ndims
## Extraction des longitudes, latitudes et du temps
nclon=nc$dim[['lon']]
lon=nclon$vals
nclat=nc$dim[['lat']]
lat=nclat$vals
## liste des longitudes et latitude de chaque point de grille
list.lat=rep(lat,length(lon))
list.lon=rep(lon,each=length(lat))
## Traitement du temps
nctime = nc$dim[['time']]
time=nctime$vals
conv.time=caldat(time/24+julday(1,1,1))
## Au cas ou on ne filtre pas les annees
if(!exists("lyear.ref")) lyear.ref=unique(conv.time$year)
## Choix de la saison
l.seas=list(all=1:12,SONDJF=c(8,9,10,11,12,1,2),DJF=c(1,2,12),SON=c(9:11))
seas="all"
l.mon=l.seas[[seas]]
## Lecture du fichier netcdf
yrmax=2014
## On prend les donnees a partir de 1948
ISEAS=which(conv.time$month %in% l.mon & conv.time$year<=yrmax & 
            conv.time$year>=yrstart)
dat=extractnc(nc,varnc,NULL,ndims,varsize)
close.ncdf(nc)

if(seacycnorm){
## Soustraction de la moyenne saisonniere jour a jour pour chaque
## point de grille
  dat.mcyc=sousseasmean(dat,conv.time)
  dat.m=dat.mcyc$anom[ISEAS,]
  zsuff="ano"
}else{
  dat.m=dat[ISEAS,]
  zsuff="raw"
}

print ('success fully season syncronised')

ndat=nrow(dat.mcyc$anom)
ngri=ncol(dat.m)

yst = refSt
yen = refEn

##---------------------------------------------------------------------------
## Wrapper pour parallelisation
"wrap.para" = function(ijour)
  {
    jdiffi= which(conv.time$day[ISEAS]==conv.time$day[ISEAS[ijour]] &
      conv.time$month[ISEAS]==conv.time$month[ISEAS[ijour]] &
      conv.time$year[ISEAS]!=conv.time$year[ISEAS[ijour]] &
      conv.time$year[ISEAS] %in% c(yst:yen)) # lyear.ref)
    kdiff=c()
## On choisit des jours calendaires proches de nwindow du jour courant    
    for(j in jdiffi) kdiff=c(kdiff,which(abs(ISEAS-j)<=nwindow))

    cor.dum=winscore(dat.m[c(ijour:(ijour+nfen-1)),],dat.m[kdiff,],
      method=method)

## Calcul de la correlation spatiale avec les meilleurs analogues    
## Correlation spatiale avec les meilleurs analogues    
    cor.test=cor(dat.m[ijour,],t(dat.m[kdiff[cor.dum$ix[1:nanalog]],]),
      method="spearman")
    
    l.iday=kdiff[cor.dum$ix[1:nanalog]]
    l.cormax=cor.dum$x[1:nanalog]
    ddijour=conv.time$day[ISEAS[ijour]]+100*conv.time$month[ISEAS[ijour]]+
      10000*conv.time$year[ISEAS[ijour]]
    ddana=conv.time$day[ISEAS[l.iday]]+
      100*conv.time$month[ISEAS[l.iday]]+
        10000*conv.time$year[ISEAS[l.iday]]
    return(c(ddijour,ddana,l.cormax,cor.test))
  }
## Fin du wrapper
##---------------------------------------------------------------------------

#rm(dat,dat.m,pc.dat)
## Calcul des analogues 
## les analogues sont choisis parmi toutes les annees sauf l'annee courante
ncpus =  detectCores() # as.numeric(Sys.getenv(c("NCPU")))
# ncpus = min(nproc,ncpus)
print(paste("Calcul sur",ncpus,"CPUs"))

for(method in l.method){
  print(paste("Analogues with",method))
## Calcul de la matrice de covariance pour la distance de Mahalanobis
  ## ATTENTION! IL Y A DES MANIERES PLUS OPTIMALES DE CALCULER CETTE DISTANCE
  ## IL VAUT MIEUX EVITER CETTE OPTION POUR GARDER DES TEMPS DE CALCULS
  ## RAISONNABLES!
  if(method=="mahalanobis2"){
    S.dat=var(dat.mcyc$anom)
    S.dat=solve(S.dat)
  }
  tm=proc.time()
  Xanatestdum=mclapply(seq(1,(length(ISEAS)-nfen+1),by=1),wrap.para,
    mc.cores=ncpus) # 24000 = 15.Sep 2014
  print(proc.time()-tm)
  Xanatest=t(matrix(unlist(Xanatestdum),nrow=(1+3*nanalog)))

## Sauvegarde en ASCII	 
  filout=paste(OUTdir,"NCEP/slp",zsuff,"-",region,"_ref_",yst,"-",yen,".analog",
    nwindow,method,".",
    nfen,"d.",
    seas,yrstart,suffana,".dat",sep="")
  nam=c("date",paste("date.an",1:nanalog,sep=""),
    paste("dis",1:nanalog,sep=""),
    paste("cor",1:nanalog,sep="")
    )
  write.table(file=filout,Xanatest,row.names=FALSE,col.names=nam,quote=FALSE)

}#end for method
q(save="no")
