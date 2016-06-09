# Routine de classification par kmeans avec Monte-Carlo et
# classification des classifications
"classnorm"=function(pc.dat,nreg=4,npc=10,nsim=200,simuname="",isave=TRUE,
  iplot=TRUE,varname="z500",yr1="",yr2="",lon="",lat="")
  {
# Classification par kmeans
# On effectue nsim=100 classifications et on recupere les centroides
    require(mclust)
#    nsim=200
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
# Sauvegarde
    if(isave){
      foutname=paste(simuname,"_",varname,"_",yr1,"-",yr2,"-diags_nreg",
        nreg,".Rdata",sep="")
      save(file=foutname,reg.var.kmeans,nreg,npc,lon,lat,kmeans.dat,perc.r)
    }#end if isave
# Trace des resultats
    if(iplot){
      layout(matrix(1:(2*ceiling(nreg/2)),2,ceiling(nreg/2)))
      par(mar=c(4,6,2,2))
      for(i in 1:nreg){ 
        image.cont.c(lon,lat,reg.var.kmeans[,i],
                     xlab="",ylab="",mar=c(2.5,2,2,1),
                     titre=paste("Reg.",i,"(",format(perc.r[i],digits=2),"%)"))
      }#end for i
    }#end if iplot
    classif.out=list(kmeans=kmeans.dat,reg.var=reg.var.kmeans,
      perc.r=perc.r,lon,lat)
    detach(package:mclust)
    invisible(classif.out)
  }#end function
