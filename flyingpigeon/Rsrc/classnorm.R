
# Routine classification for kmeans with Monte-Carlo 
# classification of classifications
"classnorm"=function(pc.dat,nreg=4,npc=10,nsim=100,lon="",lat="",varname="slp")
{
  # Classification of kmeans
  # nsim=100 classifications and we get the centroids
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
  # Classification of nreg*nsim centroids per mixture modeling
  dum.mclust=Mclust(dum)
  # grouping classification in clusters of nreg
  dum.mcl.cla=t(matrix(dum.mclust$classification,nreg,nsim))
  # order of classification
  dum.str=c()
  for(i in 1:nsim){
    s.a=paste(sort(dum.mcl.cla[i,]),sep="",collapse="")
    dum.str=c(dum.str,s.a)
  }
  dum.levels=levels(factor(dum.str))
  # determining the most probably class of classification 
  dum.class=c()
  for(lev in dum.levels) dum.class=c(dum.class,length(which(dum.str==lev)))
  class.max=which.max(dum.class)
  # Determination des manips qui conduisent a cette classif
  I.max=which(dum.str==dum.levels[class.max])
  II=(rep(I.max,each=nreg)-1)*nreg+c(1:nreg)
  dum.II=dum[II,]
  kmeans.dat=kmeans(pc.dat$x[,1:npc],dum[II[1:nreg],])
  
  # Calculating regimes
  reg.var.kmeans=pc.dat$rotation[,1:npc] %*% t(kmeans.dat$centers[1:nreg,])
  # Frequence of regimes
  perc.r=c()
  for(i in 1:nreg) perc.r=c(perc.r,length(which(kmeans.dat$cluster==i))/
                              length(kmeans.dat$cluster)*100)

  classif.out=list(kmeans=kmeans.dat,reg.var=reg.var.kmeans,
                   perc.r=perc.r,lon,lat)
  detach(package:mclust)
  invisible(classif.out)
}#end function


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
