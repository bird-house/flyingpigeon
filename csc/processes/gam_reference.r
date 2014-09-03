# #########################################
#
# Statistical Approach with a generalized additive Model (GAM)
# 
# Climate Service Center (CSC)
#
# Author: Dr. Nils Hempelmann 
# 	      Wolfgang Falk (LWF)
# 
# History: 30.08.2012
# 	     : initial preparation for WPS 27.08.2013
# #########################################

# ########################################## 
# Prepare the Workspace
# ########################################## 

# setwd("$C3GRID_WORKFLOWS/GAM/1.0/bin/")

rm(list = ls(all = TRUE))
#  library(sp, lib="./../lib" )
#  library(raster, lib="./../lib" )
#  library(ncdf, lib="./../lib")
#  library(PresenceAbsence, lib="./../lib")
#  library(mgcv)

 library(sp)
 library(raster)
 library(ncdf)
 library(PresenceAbsence)
 library(mgcv)

args <- commandArgs(trailingOnly = TRUE) # pass --args modelname (match to filename)

c_files <- vector()
c_kappa <- vector()
c_names <- vector()

print(args)

rworkspace <- args[1]
PA  <- args[2]
c_nr <- as.integer(args[3])
for (i in c(1:c_nr)) {
  c_names[i] <- args[(i+3)]
  c_files[i] <- args[3+c_nr+i]
  c_kappa[i] <- args[3+(2*c_nr)+i]
  }

print(PA)
print(c_nr)
print(c_files)
print(c_kappa)
print(c_names)

save.image(file=args[1])  

# 
# var <- c("c1",  # ID - 1 
#          "c3",  # ID - 3
#          "c5"  # ID - 5
# )
# c_files <- c("/home/main/sandbox/climdaps/parts/files/C1_GAM-pywpsInputXZvvTt.nc","/home/main/sandbox/climdaps/parts/files/C3_GAM-pywpsInputxCsLT9.nc","/home/main/sandbox/climdaps/parts/files/C5_GAM-pywpsInputgGH1Ce.nc")         
# c_kappa <- c("3","3","3")  # lapply(c(strsplit(args[4], ",")), 

c1.nc = open.ncdf(c_files[1])
c1_tmly = get.var.ncdf(c1.nc, "tas")
c1_lon = get.var.ncdf(c1.nc,varid="lon")
c1_lat = get.var.ncdf(c1.nc,varid="lat")
c1_mean = apply(c1_tmly,c(1,2),mean)
c1_mean = raster(c1_mean)
c1_mean = flip(c1_mean,"x")
c1_mean = t(c1_mean)
extent(c1_mean) <- c(min(c1_lon),max(c1_lon),min(c1_lat),max(c1_lat))

c3.nc = open.ncdf(c_files[2])
c3_tmly = get.var.ncdf(c3.nc, "pr")
c3_lon = get.var.ncdf(c3.nc,varid="lon")
c3_lat = get.var.ncdf(c3.nc,varid="lat")
c3_mean = apply(c3_tmly,c(1,2),mean)
c3_mean = raster(c3_mean)
c3_mean = flip(c3_mean,"x")
c3_mean = t(c3_mean)
extent(c3_mean) <- c(min(c3_lon),max(c3_lon),min(c3_lat),max(c3_lat))

c5.nc = open.ncdf(c_files[3])
c5_tmly = get.var.ncdf(c5.nc, "tas")
c5_lon = get.var.ncdf(c5.nc,varid="lon")
c5_lat = get.var.ncdf(c5.nc,varid="lat")
c5_mean = apply(c5_tmly,c(1,2),mean)
c5_mean = raster(c5_mean)
c5_mean = flip(c5_mean,"x")
c5_mean = t(c5_mean)
extent(c5_mean) <- c(min(c5_lon),max(c5_lon),min(c5_lat),max(c5_lat))

save.image(file=args[1])  


# ########################################## 
# prepare list of used variables and read values 
# ########################################## 

c1_mean[c1_mean>500] = NA 
c3_mean[c3_mean>500] = NA 
c5_mean[c5_mean>500] = NA 

rstack <- stack(c1_mean, c3_mean , c5_mean)
names(rstack) <- c_names
plot(rstack)

save.image(file=args[1])  

# ########################################## 
# get the ICP-Data and Climate-Proxies and combine them
# ########################################## 

data <- read.table(PA, header=TRUE, sep=";", dec=".")
data <- data[,c(2,4,7:8)] # Fsylv, KEY, LONG, LAT

# ###########################################
# get the climate data for each ICP-Point
# ###########################################

xy <- data[,c("LONG","LAT")]
sp <- SpatialPoints(xy)
xyClima <- extract(rstack, sp )
PAS_Clima<-cbind(data,xyClima)
head(sp)
summary(xyClima)
head(PAS_Clima)

save.image(file=args[1])  

# ###########################################
# calculate the GAM
# ###########################################

Trans <- function(x){exp(x)/(1+exp(x))}

# set constants 
set.seed(123)
PValspecies <- length(PAS_Clima$Fsylv[PAS_Clima$Fsylv==1])/length(PAS_Clima$Fsylv)

df <- as.data.frame(PAS_Clima[,c("Fsylv","KEY")])

save.image(file=args[1])  

######################
# prepare the input variables 

temp <- paste("s(",c_names[1],", k = ",c_kappa[1],")",sep="")
for (i in 2:length(c_names))
{
  temp <- paste(temp," + s(",c_names[i],", k = ", c_kappa[i],")",sep="")
}
temp <- paste("Fsylv ~ ",temp, sep="" )

# calculate the modelprameter
GamFsylv <- gam(eval(parse(text=(temp))),data=PAS_Clima,family=binomial(),scale=-1)

save.image(file=args[1])  

# Responsekruven erstellen
 Summ <- summary(GamFsylv)
 NamesSmoothCovMod <- rownames(Summ$s.table)
 NamesSmoothCovMod <- substr(NamesSmoothCovMod,3,nchar(NamesSmoothCovMod)-1)

# plot responsecurves
# par(mfrow=c(1,1))
  for (i in 1:length(NamesSmoothCovMod))
{
 plot.gam(GamFsylv,shade=T,col='black',select=i,trans=Trans,ylab='Predicted Probability',rug=FALSE, cex.lab = 1.4, cex.axis = 1.4, ylim=c(-6,6))  # ?? ylim=c(0,1)
 points(y=c(rep(1,21)),as.numeric(quantile(PAS_Clima[PAS_Clima$Fsylv==1,which(names(PAS_Clima)==NamesSmoothCovMod[i])],probs = seq(0, 1, 0.05), na.rm = TRUE)),pch='|',cex=1)
 points(y=c(rep(0,21)),as.numeric(quantile(PAS_Clima[PAS_Clima$Fsylv==0,which(names(PAS_Clima)==NamesSmoothCovMod[i])],probs = seq(0, 1, 0.05), na.rm = TRUE)),pch='|',cex=1)
}
  title("Response Curves", outer=T, line=-2)

  
  
# ###########################################
# spatial calculation 
# ###########################################

# Fsylv <- predict(object=rstack, model=GamFsylv, filename="/home/main/sandbox/climdaps/parts/files/Fsylv.nc", progress="text", 
# 		na.rm=TRUE , overwrite=TRUE, format="CDF", type="response") # 

species <- predict(object=rstack, model=GamFsylv, progress="text", na.rm=TRUE, type="response") # 


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
