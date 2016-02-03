# Routines de lecture de fichiers ncdf
# Pascal Yiou (LSCE)
# Lecture d'un fichier 2D netcdf
"lirevarnc" = function(nc,varname,n.lon="lon",n.lat="lat",n.time="time",
  scaling=c(1,0))
  {
#    library(clim.pact)
    varnc=nc$var[[varname]]
    varsize=varnc$varsize
    ndims=varnc$ndims
# Extraction des longitudes, latitudes et du temps
    nclon=nc$dim[[n.lon]]
    lonNCEP=nclon$vals
    nclat=nc$dim[[n.lat]]
    latNCEP=nclat$vals
 # Traitement du temps
    nctime = nc$dim[[n.time]]
    time=nctime$vals
    conv.timeNCEP=caldat(time/24+julday(1,1,1))
    timeNCEP=conv.timeNCEP$year*10000+conv.timeNCEP$month*100+
      conv.timeNCEP$day
# Lecture du fichier netcdf
    datNCEP=extractnc(nc,varnc)
# Scaling des donnees    
    datNCEP=datNCEP/scaling[1]-scaling[2]
    datout=list(dat=datNCEP,lon=lonNCEP,lat=latNCEP,
      time=timeNCEP,conv.time=conv.timeNCEP)
    rm(datNCEP)
    invisible(datout)
  }

# Extraction d'une matrice a partir d'un fichier netcdf
"extractnc" <- function(nc,varnc,ISEAS=NULL,ndims,varsize=NULL)
{
#start=rep(1,ndims)
#start[ndims]=ISEAS[1]
#count=varsize
#count[ndims]=length(ISEAS)
# Lecture des donnees de la saison ISEAS pour l'annee year
#data3=get.var.ncdf(nc,varnc,start=start,count=count)
dat.all=get.var.ncdf(nc,varnc)
if(is.null(ISEAS)) ISEAS=1:length(dat.all[1,1,])
data3=dat.all[,,ISEAS]
if(is.null(varsize)){
  nx=dim(dat.all)[2];ny=dim(dat.all)[1]
  }else
  {nx=varsize[1];ny=varsize[2];}
rm(dat.all)
nt=length(ISEAS)
# Remise dans l'ordre lat-lon-temps
dat = data3*NA; dim(dat) <- c(nt,ny,nx)
for (i in 1:nt) dat[i,,] <- t(as.matrix(data3[,,i]))
# On prefere les tableaux a deux dimensions
dim(dat)=c(nt,nx*ny)
invisible(dat)
}

## Routine de lecture d'un fichier ncdf provenant du modele de l'IPSL
## creation d'un calendrier de 360 jours
"readipslnc"=function(varname="t2m",fname,yr.range,ical=360)
  {
    nc = open.ncdf(fname)
    varnc=nc$var[[varname]]
    varsize=varnc$varsize
    ndims=varnc$ndims
# Extraction des longitudes, latitudes et du temps
    nclon=nc$dim[['lon']]
    lon=nclon$vals
    nclat=nc$dim[['lat']]
    lat=nclat$vals
#Traitement du temps
    itime=nc$dim[["time_counter"]]$vals
    if(ical==360){
      ndyear=360
      nmyear=12
      lmo=1:12

      nyear=yr.range[2]-yr.range[1]+1
      day=rep(1:30,times=nmyear*nyear)
      month=rep(rep(lmo,each=30),times=nyear)
      year=rep(yr.range[1]:yr.range[2],each=ndyear)

    }
    if(ical==365){
      year=c()
      month=c()
      day=c()
      monthdum=c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),
        rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
      daydum=c((1:31),(1:28),(1:31),(1:30),(1:31),(1:30),(1:31),(1:31),(1:30),(1:31),(1:30),(1:31))
      for(yr in yr.range[1]:yr.range[2]){
        year=c(year,rep(yr,length=length(monthdum)))
        month=c(month,monthdum)
        day=c(day,daydum)
      }
    }
    conv.time=list(year=year,month=month,day=day)

    dat=extractnc(nc,varnc,NULL,ndims,varsize)
    close.ncdf(nc)
    data.nc=list(lon=lon,lat=lat,dat=dat,time=conv.time)
    invisible(data.nc)
  }
## end function


## Routine de lecture d'un fichier ncdf provenant du modele du CNRM
## creation d'un calendrier de 365 jours
"readcnrmnc"=function(varname="t2m",fname,yr.range)
  {
    nc = open.ncdf(fname)
    varnc=nc$var[[varname]]
    varsize=varnc$varsize
    ndims=varnc$ndims
# Extraction des longitudes, latitudes et du temps
    nclon=nc$dim[['AT42_LONN27_22']]
    lon=nclon$vals
    nclat=nc$dim[['AT42_LAT40_58']]
    lat=nclat$vals
#Traitement du temps
    itime=nc$dim[["TIME"]]$vals
    ndyear=360
    nmyear=12
    lmo=1:12

    nyear=yr.range[2]-yr.range[1]+1
    day=rep(1:30,times=nmyear*nyear)
    month=rep(rep(lmo,each=30),times=nyear)
    year=rep(yr.range[1]:yr.range[2],each=ndyear)
# On attribue le mois de decembre a l'annee suivante
#year[month==12]=year[month==12]+1 

    conv.time=list(year=year,month=month,day=day)

    dat=extractnc(nc,varnc,NULL,ndims,varsize)
    close.ncdf(nc)
    data.nc=list(lon=lon,lat=lat,dat=dat,time=conv.time)
    invisible(data.nc)
  }
## end function

## Soustraction de la moyenne saisonniere jour a jour pour une matrice
## Le cycle saisonnier peut etre estime sur un sous ensemble d'annees
"sousseasmean"<-function(dat,conv.time,l.mon=1:12,
                         l.year=unique(conv.time$year),rprint=FALSE)
{
  dat.m=dat
  seas.cyc=c()
  time.cyc=c()
  ## Calcul du cycle saisonnier pour une liste d'annees dans l.year
  if(rprint) print("Seasonal cycle")
  for(mon in l.mon){
    r.day=range(conv.time$day[conv.time$month==mon])
    for(day in r.day[1]:r.day[2]){
      m.day=apply(dat[conv.time$day==day & conv.time$month==mon &
        conv.time$year %in% l.year,],2,mean,na.rm=TRUE)
      seas.cyc=rbind(seas.cyc,m.day)
      time.cyc$month=c(time.cyc$month,mon)
      time.cyc$day=c(time.cyc$day,day)
    }
  }
  ## Lissage par spline du cycle saisonnier avec preservation des
  ## derivees aux bords
  if(rprint) print("Seasonal cycle smoothing")
  seas.cyc.spl=rbind(seas.cyc,seas.cyc,seas.cyc)
  for(i in 1:ncol(seas.cyc)){
    seas.cyc.spl[,i]=smooth.spline(seas.cyc.spl[,i],spar=0.8)$y
  }
  seas.cyc.spl=seas.cyc.spl[(nrow(seas.cyc)+1):(2*nrow(seas.cyc)),]

  ## Soustraction du cycle saisonnier
  if(rprint) print("Seasonal anomalies")
  for(t in 1:nrow(dat)){
    mon=conv.time$month[t]
    day=conv.time$day[t]
    ii= which(time.cyc$month %in% mon & time.cyc$day %in% day)
    dat.m[t,]=dat[t,]-seas.cyc.spl[ii,]
  }
  datsub=list(anom=dat.m,seascyc=list(seascyc=seas.cyc.spl,timecyc=time.cyc))
}## fin de la definition de fonction



# Moyennes avec valeurs manquantes
"mean.na" <- function(x){
mean(x,na.rm=TRUE)
}

# Calcule des moyennes mensuelles d'un champ X assorti d'une liste de dates
"monmean" <- function(X,ctime,r.year=NULL)
{
  ryear=range(ctime$year)
  if(is.null(r.year)) r.year=ryear
  rmon=range(ctime$mon)
  l.mX=c()
  for(yr in max(ryear[1],r.year[1]):min(r.year[2],ryear[2])){
    for(mon in rmon[1]:rmon[2]){
      mX=apply(X[ctime$year==yr & ctime$month==mon,],2,'mean.na')
      l.mX=rbind(l.mX,mX)
    }
  }
l.mX
}

# Calcul de moyennes sur une liste de mois
"seamean" <-function(X,ctime,seas=c(3,4,5))
{
  ryear=range(ctime$year)
  Iseas=ctime$month %in% seas
  l.mX=c()
  for(yr in ryear[1]:ryear[2]){
    Iyr=which(ctime$year==yr & ctime$month==seas[1] & ctime$day==1)
    if(length(Iyr)>0)  l.mX=c(l.mX,mean(X[Iyr:(Iyr+length(seas)*30)],na.rm=TRUE))
    else l.mX=c(l.mX,NaN)
  }
l.mX
}

# Calcul des moyennes mensuelles d'une liste X assorti d'une liste de dates
"monmeanl" <- function(X,ctime,r.year=NULL)
{
  ryear=range(ctime$year)
  if(is.null(r.year)) r.year=ryear
  rmon=range(ctime$mon)
  l.mX=c()
  for(yr in max(ryear[1],r.year[1]):min(r.year[2],ryear[2])){
    for(mon in rmon[1]:rmon[2]){
      mX=mean(X[ctime$year==yr & ctime$month==mon],na.rm=TRUE)
      l.mX=c(l.mX,mX)
    }
  }
l.mX
}

# Calcul des quantiles mensuels d'une liste X assorti d'une liste de dates
"monquant" <- function(X,ctime,r.year=NULL,quantile=0.5)
{
  ryear=range(ctime$year)
  if(is.null(r.year)) r.year=ryear
  rmon=range(ctime$mon)
  l.mX=c()
  for(yr in max(ryear[1],r.year[1]):min(r.year[2],ryear[2])){
    for(mon in rmon[1]:rmon[2]){
      mX=quantile(X[ctime$year==yr & ctime$month==mon],na.rm=TRUE,probs=quantile)
      l.mX=c(l.mX,mX)
    }
  }
l.mX
}

# Calcul des nombres de depassement "sous" un seuil mensuels d'une liste X 
# assorti d'une liste de dates
"monthresh" <- function(X,ctime,r.year=NULL,threshold=5)
{
  ryear=range(ctime$year)
  if(is.null(r.year)) r.year=ryear
  rmon=range(ctime$mon)
  l.mX=c()
  for(yr in max(ryear[1],r.year[1]):min(r.year[2],ryear[2])){
    for(mon in rmon[1]:rmon[2]){
      mX=length(which(X[ctime$year==yr & ctime$month==mon]<=threshold))
      l.mX=c(l.mX,mX)
    }
  }
l.mX
}

# Calcul de la liste des dates apres une moyenne mensuelle
"mean.dates" <- function(ctime,r.year=NULL)
{
mctime=c()
if(is.null(r.year)) r.year=c(min(ctime$year),max(ctime$year))
for(yr in max(r.year[1],min(ctime$year)):min(r.year[2],max(ctime$year)))
  for(mon in 1:12){         
    mctime$year=c(mctime$year,yr)
    mctime$month=c(mctime$month,mon)
    mctime$day=c(mctime$day,1)
}
mctime
}

# Calcul du nombre de jours consecutifs max ayant une propriete dans le
# vecteur binaire (TRUE FALSE) Xbin
"conseq.prop.max" <- function(Xbin)
{
nX=length(Xbin)
l.max=c()
for(i in 1:nX){
  XXn=Xbin[1:(nX-i)] & Xbin[(i+1):nX]
  lXXn=length(which(XXn))
  l.max=c(l.max,lXXn)
}
l.max
}

## The function computes month, day, and year from Julian days. 
"caldat" = function (julian) 
{
  igreg = 2299161
  julian <- trunc(julian)
  jalpha <- julian * 0
  ja <- julian * 0
  im <- (julian >= igreg)
  if (sum(im) > 0) {
    jalpha[im] <- trunc(((julian - 1867216) - 0.25)/36524.25)
    ja[im] <- julian + 1 + jalpha - trunc(0.25 * jalpha)
  }
  im <- (julian < igreg)
  if (sum(im) > 0) 
    ja[im] <- julian[im]
  jb <- ja + 1524
  jc <- trunc(6680 + ((jb - 2439870) - 122.1)/365.25)
  jd <- 365 * jc + trunc(0.25 * jc)
  je <- trunc((jb - jd)/30.6001)
  id <- jb - jd - trunc(30.6001 * je)
  mm <- je - 1
  im <- (mm > 12)
  if (sum(im) > 0) 
    mm[im] <- mm[im] - 12
  iyyy <- jc - 4715
  im <- (mm > 2)
  if (sum(im) > 0) 
    iyyy[im] <- iyyy[im] - 1
  im <- (iyyy <= 0)
  if (sum(im) > 0) 
    iyyy <- iyyy - 1
  caldat <- list(month = mm, day = id, year = iyyy)
  invisible(caldat)
}

## The function computes Julian days from month, day, and year. 
"julday" = function (mm, id, iyyy)
{
  igreg <- 588829
  mm <- trunc(mm)
  id <- trunc(id)
  iyyy <- trunc(iyyy)
  im <- (iyyy == 0)
  if (sum(im, na.rm = TRUE) > 0)
    return("There is no year zero!")
  if ((length(mm) != length(id)) | (length(mm) != length(iyyy)) |
      (length(iyyy) != length(id)))
    return("The vectors must have same length!")
  im <- (iyyy < 0)
  if (sum(im) > 0)
    iyyy[im] <- iyyy[im] + 1
  jy <- mm * 0
  jm <- mm * 0
  ja <- mm * 0
  im <- (mm > 2)
  if (sum(im) > 0) {
    jy[im] <- iyyy[im]
    jm[im] <- mm[im] + 1
  }
  im <- (mm <= 2)
  if (sum(im) > 0) {
    jy[im] <- iyyy[im] - 1
    jm[im] <- mm[im] + 13
  }
  jul <- trunc(365.25 * jy) + trunc(30.6001 * jm) + id + 1720995
  im <- (id + 31 * (mm + 12 * iyyy) >= igreg)
  if (sum(im) > 0) {
    ja[im] <- trunc(0.01 * jy)
    jul[im] <- jul + 2 - ja[im] + trunc(0.25 * ja[im])
  }
  julday <- jul
  invisible(julday)
} 
