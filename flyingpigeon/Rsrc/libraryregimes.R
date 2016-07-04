## Routines de lecture de fichiers ncdf
# by Pascal Yiou (LSCE)

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
  conv.timeNCEP=caldat(time/24+julday(1,1,1800))
  timeNCEP=conv.timeNCEP$year*10000+conv.timeNCEP$month*100+
    conv.timeNCEP$day
  # Lecture du fichier netcdf
  datNCEP=extractnc(nc,varnc)
  # Scaling des donnees    
  datNCEP=datNCEP/scaling[1]-scaling[2]
  datout=list(dat=datNCEP,lon=lonNCEP,lat=latNCEP,
              time=timeNCEP,conv.time=conv.timeNCEP,timenc=time)
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
  dat.all=ncvar_get(nc,varnc)
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
"julday" <- function (mm, id, iyyy)
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


# Soustraction de la moyenne saisonniere jour a jour pour une matrice
# Le cycle saisonnier peut etre estime sur un sous ensemble d'annees
"sousseasmean"<-function(dat,conv.time,l.mon=1:12,
                         l.year=unique(conv.time$year),rprint=FALSE)
{
  dat.m=dat
  seas.cyc=c()
  time.cyc=c()
  # Calcul du cycle saisonnier pour une liste d'annees dans l.year
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
  # Lissage par spline du cycle saisonnier avec preservation des derivees aux bords
  if(rprint) print("Seasonal cycle smoothing")
  seas.cyc.spl=rbind(seas.cyc,seas.cyc,seas.cyc)
  for(i in 1:ncol(seas.cyc)){
    seas.cyc.spl[,i]=smooth.spline(seas.cyc.spl[,i],spar=0.8)$y
  }
  seas.cyc.spl=seas.cyc.spl[(nrow(seas.cyc)+1):(2*nrow(seas.cyc)),]
  
  # Soustraction du cycle saisonnier
  if(rprint) print("Seasonal anomalies")
  for(t in 1:nrow(dat)){
    mon=conv.time$month[t]
    day=conv.time$day[t]
    ii= which(time.cyc$month %in% mon & time.cyc$day %in% day)
    dat.m[t,]=dat[t,]-seas.cyc.spl[ii,]
  }
  datsub=list(anom=dat.m,seascyc=list(seascyc=seas.cyc.spl,timecyc=time.cyc))
}# fin de la definition de fonction



# Routine de classification par kmeans avec Monte-Carlo et
# classification des classifications
"classnorm"=function(pc.dat,nreg=4,npc=10,nsim=100,lon="",lat="",iplot=TRUE,varname="slp")
  {
# Classification par kmeans
# On effectue nsim=100 classifications et on recupere les centroides
    require(mclust)
    if(class(pc.dat)=="princomp"){
      kmeans.dat=kmeans(pc.dat$scores[,1:npc],nreg)
    }else{
      kmeans.dat=kmeans(pc.dat$x[,1:npc],nreg)
    }
    ndat=ifelse(class(pc.dat)=="princomp",nrow(pc.dat$scores),nrow(pc.dat$x))
    dum=kmeans.dat$centers
    for(i in 2:nsim){
      kmeans.dat=kmeans(pc.dat$x[sample(1:ndat,ndat),1:npc],nreg)
      dum=rbind(dum,kmeans.dat$centers)
    }
# Classification des nreg*nsim centroides par mixture modeling
    dum.mclust=Mclust(dum)
# Je mets la classif par groupes de nreg
    dum.mcl.cla=t(matrix(dum.mclust$classification,nreg,nsim))
# J'ordonne la classification
    dum.str=c()
    for(i in 1:nsim){
      s.a=paste(sort(dum.mcl.cla[i,]),sep="",collapse="")
      dum.str=c(dum.str,s.a)
    }
    dum.levels=levels(factor(dum.str))
# Je determine la classe de classif la plus probable
    dum.class=c()
    for(lev in dum.levels) dum.class=c(dum.class,length(which(dum.str==lev)))
    class.max=which.max(dum.class)
# Determination des manips qui conduisent a cette classif
    I.max=which(dum.str==dum.levels[class.max])
    II=(rep(I.max,each=nreg)-1)*nreg+c(1:nreg)
    dum.II=dum[II,]
    kmeans.dat=kmeans(pc.dat$x[,1:npc],dum[II[1:nreg],])

# Calcul des regimes
    reg.var.kmeans=pc.dat$rotation[,1:npc] %*% t(kmeans.dat$centers[1:nreg,])
# Frequence de chaque regime
    perc.r=c()
    for(i in 1:nreg) perc.r=c(perc.r,length(which(kmeans.dat$cluster==i))/
      length(kmeans.dat$cluster)*100)
# Trace des resultats
    if(iplot){
      layout(matrix(1:(2*ceiling(nreg/2)),2,ceiling(nreg/2)))
      par(mar=c(4,6,2,2))
      for(i in 1:nreg){ 
        image.cont.mc(lon,lat,reg.var.kmeans[,i],
                     xlab="",ylab="",mar=c(2.5,2,2,1),
                     titre=paste("Reg.",i,"(",format(perc.r[i],digits=2),"%)"))
      }#end for i
    }#end if iplot
    classif.out=list(kmeans=kmeans.dat,reg.var=reg.var.kmeans,
      perc.r=perc.r,lon,lat)
    detach(package:mclust)
    invisible(classif.out)
  }#end function


#Trace une carte "longitude-latitude" d'un champ, avec la ligne de continents
# qui correspond
"image.cont" <-
  function(lonF,latF,champ,titre="",Ichange=numeric(0),
           zlev=seq(min(champ,na.rm=TRUE),max(champ,na.rm=TRUE),length=11),
           transpose=T,mar=c(3,3,3,4),legend=TRUE,xlab="Longitude",
           ylab="Latitude",satur=FALSE,paquet="fields",add=FALSE,
           lonrange=range(lonF),latrange=range(latF))
  {
    #library(paquet)
    #zlev=seq(-0.85,0.6,length=11)
    #col10=rainbow(length(zlev)-1,start=0,end=2/6)
    col10=rainbow(length(zlev)-1,start=5/6,end=3/6)
    #col10=rainbow(length(zlev)-1,start=6/6,end=4/6)
    #par( mar=c(10,5,5,5))
    par( mar=mar)
    if(satur){
      champ[champ<=min(zlev)]=min(zlev)
      champ[champ>=max(zlev)]=max(zlev)
    }
    if(length(Ichange)>0) champ[Ichange]=1
    if (transpose)
      dum=t(matrix(champ,length(latF),length(lonF)))
    else
      dum=matrix(champ,length(lonF),length(latF))
    latF.sort=sort(latF,index.return=TRUE)
    lonF.sort=sort(lonF,index.return=TRUE)
    plot(lonrange,latrange,type="n",xlab=xlab,ylab=ylab,xlim=lonrange,
         ylim=latrange)
    image(sort(lonF),sort(latF),dum[lonF.sort$ix,latF.sort$ix],
          col=col10[length(col10):1],
          xlab=xlab,ylab=ylab,main=titre,breaks=zlev,add=TRUE)
    if(paquet=="fields"){
      library(fields)
      world(add=TRUE)
      #  world(xlim=range(lonF),ylim=range(latF),add=TRUE)
    }
    if(paquet=="maps"){
      library(maps)
      map(add=TRUE)
    }
    if(legend) image.plot(dum[,length(latF):1],col=col10[length(col10):1],
                          legend.only=TRUE,zlim=range(zlev))
  }

#Trace une carte "longitude-latitude" de contours d'un champ, avec la ligne 
#de continents qui correspond. contour rainbow
"image.cont.mc" <-
  function(lonF,latF,champ,titre="",Ichange=numeric(0),
           zlev=pretty(champ,20),
           #         zlev=seq(min(champ,na.rm=TRUE),max(champ,na.rm=TRUE),length=11),
           transpose=TRUE,mar=c(5,5,5,6),xlab="Longitude",
           ylab="Latitude",add=FALSE,paquet="fields",lty=1)
  {
    #zlev=seq(-0.85,0.6,length=11)
    col10=rainbow(length(zlev)-1,start=3/6,end=1)
    #par( mar=c(10,5,5,5))
    par( mar=mar)
    if(length(Ichange)>0) champ[Ichange]=1
    if (transpose)
      dum=t(matrix(champ,length(latF),length(lonF)))
    else
      dum=matrix(champ,length(lonF),length(latF))
    latF.sort=sort(latF,index.return=TRUE)
    nlev=length(zlev)
    contour(lonF,sort(latF),dum[,latF.sort$ix],
            xlab=xlab,ylab=ylab,main=titre,col=col10,add=add,nlevels=nlev,
            levels=zlev,lty=lty)
    if(paquet=="fields"){
      library(fields)
      world(xlim=range(lonF),ylim=range(latF),add=TRUE)}
    if(paquet=="maps"){
      library(maps)
      map(add=TRUE)
    }
    #world(xlim=range(lonF),ylim=range(latF),add=TRUE)
  }
#Trace une carte "longitude-latitude" de contours d'un champ, avec la ligne 
#de continents qui correspond. contour blue
"image.cont.c" <-
  function(lonF,latF,champ,titre="",Ichange=numeric(0),
           zlev=pretty(champ,20),
           #         zlev=seq(min(champ,na.rm=TRUE),max(champ,na.rm=TRUE),length=11),
           transpose=TRUE,mar=c(5,5,5,6),xlab="Longitude",
           ylab="Latitude",col="blue",add=FALSE,paquet="fields",lty=1)
  {
    #zlev=seq(-0.85,0.6,length=11)
    col10=rainbow(length(zlev)-1,start=2/6,end=0)
    #par( mar=c(10,5,5,5))
    par( mar=mar)
    if(length(Ichange)>0) champ[Ichange]=1
    if (transpose)
      dum=t(matrix(champ,length(latF),length(lonF)))
    else
      dum=matrix(champ,length(lonF),length(latF))
    latF.sort=sort(latF,index.return=TRUE)
    nlev=length(zlev)
    contour(lonF,sort(latF),dum[,latF.sort$ix],
            xlab=xlab,ylab=ylab,main=titre,col=col,add=add,nlevels=nlev,
            levels=zlev,lty=lty)
    if(paquet=="fields"){
      library(fields)
      world(xlim=range(lonF),ylim=range(latF),add=TRUE)}
    if(paquet=="maps"){
      library(maps)
      map(add=TRUE)
    }
    #world(xlim=range(lonF),ylim=range(latF),add=TRUE)
  }

