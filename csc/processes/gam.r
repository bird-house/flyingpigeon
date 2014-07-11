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

args <- commandArgs(trailingOnly = TRUE)                   # pass --args modelname (match to filename)

print(args)

# content of possible variables
var <- c("c1",  # ID - 1
	     "c2",  # ID - 2
	     "c3",  # ID - 3 
	     "c4",  # ID - 4
	     "c5",  # ID - 5
	     "c6"
         )

         
c_files <- c("/home/main/sandbox/climdaps/parts/files/C1_GAM-pywpsInputXZvvTt.nc","
/home/main/sandbox/climdaps/parts/files/C3_GAM-pywpsInputHWnJUD.nc","
/home/main/sandbox/climdaps/parts/files/C5_GAM-pywpsInputgGH1Ce.nc
")         
c_kappa <- c(3,3,3)  # lapply(c(strsplit(args[4], ",")), 
PA  <- "/home/main/sandbox/climdaps/parts/files/ICP_DATA_Fsylv_Pabie_2.csv" # args[3];  # PA data 
pdf     <- "testout.pdf"    #    args[5];  # pdf file 





print(file1)
print(file2)
print(PA)
print(ID)
print(pdf)

pdf(pdf) # all Graphics are stored one pdf.file
# file <- "/home/nhempelm/testdata/C4IRCA3_A2_ECHAM5_DM_25km_1971-2000_05_out.nc"
# file2 <- "/home/nhempelm/testdata/C4IRCA3_A2_ECHAM5_DM_25km_2021-2050_05_out.nc"

# (paste("./../out/",args[1],"_2021-2050_10_out.nc",sep = ""))
#file3 <- (paste("./../out/",args[1],"_2071-2100_10_out.nc",sep = ""))

# ########################################## 
# prepare list of used variables and read values 
# ########################################## 

tmp_list <- cbind(c_kappa, as.numeric(ID)) 
c_kappa <-  subset(tmp_list, ID > 0)

for (i in 1:length(c_kappa[,1]))
{
assign(c_kappa[i,1], raster(file1, varname=c_kappa[i,1])); 
# qnames[i] <- list(print(var[i,1], quote = FALSE)); 
}

names <- 0 # inintialisieren 

for (i in 1:length(var[,1]))
names[i] <- list(get(var[i,1]))

# is.na(prtgsd) = prtgsd > 2000 

rstack <- stack(names)
plot(rstack)

# ########################################## 
# get the ICP-Data and Climate-Proxies and combine them
# ########################################## 

data <- read.table(PA, header=TRUE, sep=";", dec=",")
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

# ###########################################
# calculate the GAM
# ###########################################

 Trans <- function(x){exp(x)/(1+exp(x))}
 Europa <- PAS_Clima
 dim(Europa)#[1]

# Header an Rasterdaten anpassen
names(Europa) <- c("Fsylv","KEY","LONG","LAT")
for (i in 1:length(var[,1]))
names(Europa)[i+4] <- var[i,1]

write.csv2(Europa, file=("./../out/Test_-2000_Fsylv.csv"))

# set constants 
 set.seed(123)
 PValFsylv <- length(Europa$Fsylv[Europa$Fsylv==1])/length(Europa$Fsylv)
 df <- as.data.frame(Europa[,c("Fsylv","KEY")])

######################
# prepare the input variables 

temp <- paste("s(",var[1,1],", k = ",var[1,2],")",sep="")
for (i in 2:length(var[,1]))
{
  temp <- paste(temp," + s(",var[i,1],", k = ",var[i,2],")",sep="")
}
temp <- paste("Fsylv ~ ",temp, sep="" )

# calculate the modelprameter
GamFsylv <- gam(eval(parse(text=(temp)))
                ,data=Europa,family=binomial(),scale=-1)

# Responsekruven erstellen
 Summ <- summary(GamFsylv)
 NamesSmoothCovMod <- rownames(Summ$s.table)
 NamesSmoothCovMod <- substr(NamesSmoothCovMod,3,nchar(NamesSmoothCovMod)-1)

# plot responsecurves
# par(mfrow=c(1,1))
  for (i in 1:length(NamesSmoothCovMod))
{
 plot.gam(GamFsylv,shade=T,col='black',select=i,trans=Trans,ylab='Predicted Probability',rug=FALSE, cex.lab = 1.4, cex.axis = 1.4, ylim=c(-6,6))  # ?? ylim=c(0,1)
 points(y=c(rep(1,21)),as.numeric(quantile(Europa[Europa$Fsylv==1,which(names(Europa)==NamesSmoothCovMod[i])],probs = seq(0, 1, 0.05), na.rm = TRUE)),pch='|',cex=1)
 points(y=c(rep(0,21)),as.numeric(quantile(Europa[Europa$Fsylv==0,which(names(Europa)==NamesSmoothCovMod[i])],probs = seq(0, 1, 0.05), na.rm = TRUE)),pch='|',cex=1)
}
  title("Response Curves", outer=T, line=-2)

# ###########################################
# spatial calculation 
# ###########################################

Fsylv <- predict(object=rstack, model=GamFsylv, filename="./../out/Test_Fsylv_-2000.nc", progress="text", 
		na.rm=TRUE , overwrite=TRUE, format="CDF", type="response") # 

# Umwandlung der Wahrscheinlichkeitsoberfläche in Favourabilities nach Real et al. 2006 (Prävalenz = 0.5)
# Real, R, Barbosa, AM, Vargas, JM 2006 Obtaining environmental favourability functions from logistic regression Environ Ecol Stat 13: 237-245.

predfun <- function(Fsylv) {Fsylv/(1-Fsylv)/((PValFsylv/(1-PValFsylv)+ Fsylv/(1-Fsylv)))}
FsylvFav <- calc(Fsylv, fun=predfun, filename="./../out/Test_FsylvFav_1971-2000.nc", overwrite=TRUE)

# #######################
# Gütebewertung 
# #######################

# Favourabilities and PAS lat long anfügen
  xy$fav <- extract(Fsylv, sp)
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
calibration.plot(guete,which.model=1, na.rm=FALSE, alpha=0.05, N.bins=10, xlab="Predicted Favourability", ylab="Amount of Observed Presences")
boxplot(guete$FAV~guete$Fsylv, names=c("Absences","Presences"), main="Favourabilities Fagus sylvatica")

par(mfrow=c(1,1))
 Lab.palette <- colorRampPalette(c("Dark Green", "chartreuse4", "green4",  "green1", "yellow", "orange", "red","dark red", "PeachPuff3", "grey" ), space = "Lab")
 brks <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) 
 nb <- length(brks)-1
plot(FsylvFav, breaks=brks, col=rev(Lab.palette(nb)), lab.breaks=brks, main="Favourability European Beech (1971-2000)",
     sub=file1, xlab="Longitude", ylab="Latitude")


# ###########################################
# spatial calculation for predicted  Szenario
# ###########################################

for (i in 1:length(var[,1]))
{
assign(var[i,1], raster(file2, varname=var[i,1])); 
# qnames[i] <- list(print(var[i,1], quote = FALSE)); 
}

names <- 0 # inintialisieren 

for (i in 1:length(var[,1]))
names[i] <- list(get(var[i,1]))

# is.na(prtgsd) = prtgsd > 2000 

rstack <- stack(names)
plot(rstack)


Fsylv <- predict(object=rstack, model=GamFsylv, filename="./../out/Test_Fsylv__2010.nc", progress="text", 
 		na.rm=TRUE, format = "CDF", overwrite=TRUE,  type="response")

# Umwandlung der Wahrscheinlichkeitsoberfläche in Favourabilities nach Real et al. 2006 (Prävalenz = 0.5)
# Real, R, Barbosa, AM, Vargas, JM 2006 Obtaining environmental favourability functions from logistic regression Environ Ecol Stat 13: 237-245.

predfun <- function(Fsylv) {Fsylv/(1-Fsylv)/((PValFsylv/(1-PValFsylv)+ Fsylv/(1-Fsylv)))}
FsylvFav <- calc(Fsylv, fun=predfun, filename="./../out/Test_FsylvFav_2021-2050.nc", overwrite=TRUE)

par(mfrow=c(1,1))
plot(FsylvFav, breaks=brks, col=rev(Lab.palette(nb)), lab.breaks=brks, main="Favourability European Beech (2010)",
     sub=file2, xlab="Longitude", ylab="Latitude")


dev.off() # close the open pdf. 
