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

 library(sp)
 library(raster)
 library(ncdf)
 library(PresenceAbsence)
 library(mgcv)

args_proj <- commandArgs(trailingOnly = TRUE)  # pass --args modelname (match to filename)
# (summary, self.R_in.getValue(), str(len(c_files)), string.join(c_names," "), string.join(c_files," "), 
# string.join(c_kappa," ")))

pdf <- args_proj[1]
# 
pdf(paste(pdf))

load(file=args_proj[2])

c_files_proj <- vector()
c_kappa_proj <- vector()
c_names_proj <- vector()

print(args_proj)
c_nr <- as.integer(args_proj[3])
for (i in c(1:c_nr)) {
  c_names_proj[i] <- args_proj[(i+3)]
  c_files_proj[i] <- args_proj[3+c_nr+i]
  c_kappa_proj[i] <- args_proj[3+(2*c_nr)+i]
}

print(c_files)
print(c_kappa)
print(c_names)
print(c_files_proj)
print(c_kappa_proj)
print(c_names_proj)

c1_proj.nc = open.ncdf(c_files_proj[1])
c1_proj_tmly = get.var.ncdf(c1_proj.nc, "tas")
c1_proj_lon = get.var.ncdf(c1_proj.nc,varid="lon")
c1_proj_lat = get.var.ncdf(c1_proj.nc,varid="lat")
c1_proj_mean = apply(c1_proj_tmly,c(1,2),mean)
c1_proj_mean = raster(c1_proj_mean)
c1_proj_mean = flip(c1_proj_mean,"x")
c1_proj_mean = t(c1_proj_mean)
extent(c1_proj_mean) <- c(min(c1_proj_lon),max(c1_proj_lon),min(c1_proj_lat),max(c1_proj_lat))

c3_proj.nc = open.ncdf(c_files_proj[2])
c3_proj_tmly = get.var.ncdf(c3_proj.nc, "pr")
c3_proj_lon = get.var.ncdf(c3_proj.nc,varid="lon")
c3_proj_lat = get.var.ncdf(c3_proj.nc,varid="lat")
c3_proj_mean = apply(c3_proj_tmly,c(1,2),mean)
c3_proj_mean = raster(c3_proj_mean)
c3_proj_mean = flip(c3_proj_mean,"x")
c3_proj_mean = t(c3_proj_mean)
extent(c3_proj_mean) <- c(min(c3_proj_lon),max(c3_proj_lon),min(c3_proj_lat),max(c3_proj_lat))

c5_proj.nc = open.ncdf(c_files_proj[1])
c5_proj_tmly = get.var.ncdf(c5_proj.nc, "tas")
c5_proj_lon = get.var.ncdf(c5_proj.nc,varid="lon")
c5_proj_lat = get.var.ncdf(c5_proj.nc,varid="lat")
c5_proj_mean = apply(c5_proj_tmly,c(1,2),mean)
c5_proj_mean = raster(c5_proj_mean)
c5_proj_mean = flip(c5_proj_mean,"x")
c5_proj_mean = t(c5_proj_mean)
extent(c5_proj_mean) <- c(min(c5_proj_lon),max(c5_proj_lon),min(c5_proj_lat),max(c5_proj_lat))

# ########################################## 
# prepare list of used variables and read values 
# ########################################## 

c1_proj_mean[c1_proj_mean>500] = NA 
c3_proj_mean[c3_proj_mean>500] = NA 
c5_proj_mean[c5_proj_mean>500] = NA 

rstack_proj <- stack(c1_proj_mean, c3_proj_mean , c5_proj_mean)
names(rstack_proj) <- c_names_proj
plot(rstack)
plot(rstack_proj)

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

species <- predict(object=rstack, model=GamFsylv, progress="text", na.rm=TRUE, type="response") # 

# Umwandlung der Wahrscheinlichkeitsoberfläche in Favourabilities nach Real et al. 2006 (Prävalenz = 0.5)
# Real, R, Barbosa, AM, Vargas, JM 2006 Obtaining environmental favourability functions from logistic regression Environ Ecol Stat 13: 237-245.

predfun <- function(species) {species/(1-species)/((PValspecies/(1-PValspecies)+ species/(1-species)))}
speciesFav <- calc(species, fun=predfun)

# speciesFav <- calc(species, fun=predfun)

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

# textplot(summary(guete))
# textplot(df.guete)
#  model threshold       PCC sensitivity specificity     Kappa       AUC
#1   FAV       0.5 0.7580872   0.5682782   0.8522727 0.4352433 0.8128604
#  Modellvergleich auch ohne st.dev mgl., es wird meist nur auf PCC, sens und spec sowie Kappa und AUC geachtet
# für alle Gütemaße gilt: je höher desto besser;-)

par(mfrow=c(1,1))
calibration.plot(guete,which.model=1, na.rm=FALSE, alpha=0.05, N.bins=10, xlab="Projected Favourability", ylab="Amount of Observed Presences")
boxplot(guete$FAV~guete$Fsylv, names=c("Absences","Presences"), main="Favourabilities species")

par(mfrow=c(1,1))
 Lab.palette <- colorRampPalette(c("Dark Green", "chartreuse4", "green4",  "green1", "yellow", "orange", "red","dark red", "PeachPuff3", "grey" ), space = "Lab")
 brks <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) 
 nb <- length(brks)-1
plot(speciesFav, breaks=brks, col=rev(Lab.palette(nb)), lab.breaks=brks, main="Favourability (reference)",
     sub=c_files[1], xlab="Longitude", ylab="Latitude")


# # ###########################################
# # spatial calculation for predicted  Szenario
# # ###########################################
# 
species_proj <- predict(object=rstack_proj, model=GamFsylv, progress="text", na.rm=TRUE,  type="response")

# # Umwandlung der Wahrscheinlichkeitsoberfläche in Favourabilities nach Real et al. 2006 (Prävalenz = 0.5)
# # Real, R, Barbosa, AM, Vargas, JM 2006 Obtaining environmental favourability functions from logistic regression Environ Ecol Stat 13: 237-245.
predfun <- function(species_proj) {species_proj/(1-species_proj)/((PValspecies/(1-PValspecies)+ species_proj/(1-species_proj)))}
speciesFav_proj <- calc(species_proj, fun=predfun)
# 
par(mfrow=c(1,1))
plot(speciesFav_proj, breaks=brks, col=rev(Lab.palette(nb)), lab.breaks=brks, main="Favourability (projection)",
     sub=c_files[1], xlab="Longitude", ylab="Latitude")

dev.off() # close the open pdf.

# save.image() 

