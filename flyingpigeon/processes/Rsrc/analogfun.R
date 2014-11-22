# An R package to compute flow analogues
# by Pascal Yiou (LSCE), 2012

# Definitions of standard score functions to be maximized
# Xtest is an input vector of length M, Yref is a reference matrix of size
# N x M
# RMS (root mean square) or Euclidean distance
"rms" = function(Xtest,Yref)
  {
    dis=Xtest-t(Yref)
    dum=-sqrt(apply(dis^2,2,sum,na.rm=TRUE)/length(Xtest))
    invisible(dum)
  }

# ESV is a weighting of the RMS by the correlation
"esv" = function(Xtest,Yref)
  {
     dis=Xtest-t(Yref)
     dum=1-apply(dis,2,var,na.rm=TRUE)/var(Xtest,na.rm=TRUE)
     invisible(dum)
  }

# Linear correlation (Pearson)
"corlin" = function(Xtest,Yref)
{
  dum=cor(Xtest,t(Yref),method="pearson",use="pairwise.complete")
  invisible(dum)
}

# Rank correlation (Spearman)
"corrnk" = function(Xtest,Yref)
{
  dum=cor(Xtest,t(Yref),method="spearman",use="pairwise.complete")
  invisible(dum)
}

# Mahalanobis distance
# Requires the computation of S.dat=inverted covariance matrix
# Allows (some) missing data
"mahalanobis2" = function(Xtest,Yref)
  {
#    Xdiff=sweep(Yref,2,Xtest)
    if(!exists("S.dat")){
      S.dat=var(Xtest)
      S.dat=solve(S.dat)
    }
    dum=mahalanobis(Yref,Xtest,S.dat,inverted=TRUE)
    dum=-sqrt(dum)
#    dum=mahalanobis(Xdiff,rep(0,length(Xtest)),S.dat,inverted=TRUE)
    invisible(dum)
  }

# Computes windowed scores
# Xtest is a matrix of size NX x M: NX is the size of the window for X
# Yref is a matrix of size NY x M: NY is the number of observations for Y
"winscore" = function(Xtest,Yref,method="rms")
  {
    FUN=match.fun(method)
    if(is.vector(Xtest)){
      Xtest.m=matrix(Xtest,1,length(Xtest))
      lwin=1
    }else{
      Xtest.m=Xtest
      lwin=nrow(Xtest)
    }
    score=c()
    for(dd in 1:lwin){
      Xdum=Xtest.m[dd,]
      scoredum=FUN(Xdum,Yref)
      score=rbind(score,scoredum)
    }
    rm(Xtest.m)
    ddum=c() #offset in the windowing
    for(dd in 1:lwin){
      ddum=rbind(ddum,score[dd,dd:(ncol(score)-lwin+dd)])
    }
    clwin=apply(ddum,2,sum) # mean score over lwin days
    clwin.sort=sort(clwin,index.return=TRUE,decreasing=TRUE)
    return(clwin.sort)
  }

# Computes best iana windowed analogues
# Xtest is a matrix of size lwin x M
# Ytest is a matrix of size N x M
"Analowin" = function(Xtest,Yref,iana=10,method="rms")
  {
    score=winscore(Xtest,Yref,method=method)
    analogs=list(index=score$ix[1:iana],score=score$x[1:iana])
    return(analogs)
  }

# Remove seasonal cycle in a matrix
# Seasonal cycle is estimated on a subset of input data
# input:
# dat: data matrix (T x M). lines indicate time, columns indicate space
# conv.time: list of dates list(year, month, day), with the same
# row number as dat.
# l.year: list of years on which the seasonal cycle is computed
"rm.seascyc"<-function(dat,conv.time,l.year=unique(conv.time$year),
                       rprint=FALSE)
{
  dat.m=dat
  l.mon=1:12
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
# Lissage par spline du cycle saisonnier avec preservation
# des derivees aux bords
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
  invisible(datsub)
}# end rm.seascyc
