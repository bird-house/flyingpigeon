
 library(sp)
# library(raster)
# library(ncdf)
# library(PresenceAbsence)
 library(mgcv)

c_files <- vector()
c_kappa <- vector()
c_names <- vector()

x1 = sample(0:40,1000, replace=TRUE)
x2 = x1/2 + (sample(-4:4,1000, replace=TRUE))

PA = x1

PA[x1 < 20 ] = 0
PA[x1 > 30 ] = 0   
PA[PA > 0 ] = 1

x1 = x1 + (sample(-1:1,1000, replace=TRUE))

PA_Clima<-cbind(PA,x1,x2)


# ###########################################
# calculate the GAM
# ###########################################

Trans <- function(x){exp(x)/(1+exp(x))}

# set constants 
set.seed(123)
#PValspecies <- length(PAS_Clima$Fsylv[PAS_Clima$Fsylv==1])/length(PAS_Clima$Fsylv)

df <- as.data.frame(PA_Clima[,c("PA","x1","x2")])

temp <- 'PA ~ s(x1 , k = 3 ) + s(x2 , k = 3 )'

# calculate the modelprameter
gam_model <- gam(eval(parse(text=(temp))),data=df ,family=binomial(),scale=-1)

# Responsekruven erstellen
Summ <- summary(gam_model)
NamesSmoothCovMod <- rownames(Summ$s.table)
NamesSmoothCovMod <- substr(NamesSmoothCovMod,3,nchar(NamesSmoothCovMod)-1)

# plot responsecurves
# par(mfrow=c(1,1))
for (i in 1:length(NamesSmoothCovMod))
{
 plot.gam(gam_model,shade=T,col='black',select=i,trans=Trans,ylab='Predicted Probability',rug=FALSE, cex.lab = 1.4, cex.axis = 1.4, ylim=c(-6,6))  # ?? ylim=c(0,1)
 points(y=c(rep(1,21)),as.numeric(quantile(PA_Clima[PA_Clima$PA==1,which(names(PAS_Clima)==NamesSmoothCovMod[i])],probs = seq(0, 1, 0.05), na.rm = TRUE)),pch='|',cex=1)
# points(y=c(rep(0,21)),as.numeric(quantile(PA_Clima[PA_Clima$PA==0,which(names(PAS_Clima)==NamesSmoothCovMod[i])],probs = seq(0, 1, 0.05), na.rm = TRUE)),pch='|',cex=1)
}
  title("Response Curves", outer=T, line=-2)
 
# ###########################################
# spatial calculation 
# ###########################################

# Fsylv <- predict(object=rstack, model=GamFsylv, filename="/home/main/sandbox/climdaps/parts/files/Fsylv.nc", progress="text", 
# 		na.rm=TRUE , overwrite=TRUE, format="CDF", type="response") # 


x1 = sample(0:40,1000, replace=TRUE)
x2 = x1/2 + (sample(-4:4,1000, replace=TRUE))
x1 = sample(0:40,200*200, replace=TRUE)
x2 = x1/2 + (sample(-4:4,200*200, replace=TRUE))
x1 = matrix(x1,ncol=200,nrow=200, dimnames=c(x,y))
x2 = matrix(x2,ncol=200,nrow=200,dimnames=c(x,y))
d = list(x1, x2)
rstack = do.call(rbind,d)

#bdg <- gam(V4~s(V1,V2,V3), data=bdp)

#           1 
# 85431440244

#species <- predict_gam(object=rstack, model=gam_model, progress="text", na.rm=TRUE, type="response") # 

save.image(file=args[1])  

# Umwandlung der Wahrscheinlichkeitsoberfläche in Favourabilities nach Real et al. 2006 (Prävalenz = 0.5)
# Real, R, Barbosa, AM, Vargas, JM 2006 Obtaining environmental favourability functions from logistic regression Environ Ecol Stat 13: 237-245.

predfun <- function(species) {species/(1-species)/((PValspecies/(1-PValspecies)+ species/(1-species)))}
speciesFav <- calc(species, fun=predfun)

# #######################
# Gütebewertung 
# #######################

# Favourabilities and PAS lat long anfügen
  xy$fav <- extract(species, sp)
# Datensatz erstellen, wie von Paket benötigt (Key, PA, Preds/Favs...)
  guete <- cbind(data[,c("KEY","Fsylv")],xy[,c("fav")])
  names(guete) <- c("KEY","Fsylv", "FAV")
# wieder die NAs entfernen (wobei die Funktion presence.absence.accuray() ein eigenes na.rm-Argument hätte)
  guete <- guete[is.na(guete$FAV)==FALSE,]

# bei Favs ist die Prävalenz auf 0.5 transformiert, sonst Prävalenz ausrechnen: PValFsylv <- dim(data[data$Fsylv==1,])[1]/dim(data)[1]
df.guete <- presence.absence.accuracy(guete, threshold=0.5,find.auc=TRUE, st.dev=TRUE,which.model=(1:(ncol(guete)-2)), na.rm=TRUE )
# which.model=(1:(ncol(guete)-2)) etwas umständlich für die dritte Spalte, die das Modell enthält. 
# Ist sinnvoll, wenn viele unterschiedliche Modellvorhersagen an die ersten zwei Spalten gehängt werden (Fav_MPI, Fav_CRU etc.)

par(mfrow=c(3,1))

save.image(file=args[1])  

# textplot(summary(guete))
# textplot(df.guete)

#  model threshold       PCC sensitivity specificity     Kappa       AUC
#1   FAV       0.5 0.7580872   0.5682782   0.8522727 0.4352433 0.8128604
#  Modellvergleich auch ohne st.dev mgl., es wird meist nur auf PCC, sens und spec sowie Kappa und AUC geachtet
# für alle Gütemaße gilt: je höher desto besser;-)

par(mfrow=c(1,1))
calibration.plot(guete,which.model=1, na.rm=FALSE, alpha=0.05, N.bins=10, xlab="Predicted Favourability", ylab="Amount of Observed Presences")
boxplot(guete$FAV~guete$Fsylv, names=c("Absences","Presences"), main="Favourabilities Species")

par(mfrow=c(1,1))
 Lab.palette <- colorRampPalette(c("Dark Green", "chartreuse4", "green4",  "green1", "yellow", "orange", "red","dark red", "PeachPuff3", "grey" ), space = "Lab")
 brks <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) 
 nb <- length(brks)-1
plot(speciesFav, breaks=brks, col=rev(Lab.palette(nb)), lab.breaks=brks, main="Favourability Species (reference)",
     sub='Demo Species', xlab="Longitude", ylab="Latitude")

 # close the open pdf. 
save.image(file=args[1])  
