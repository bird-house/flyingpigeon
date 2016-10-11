.. _weatherregimes:

Weather Regimes
---------------

Calculation of weather regimes based on pressure patterns (k-means clustering method). The processes performs a pattern classification for observations data (NCEP) as well as model data. Both results are compared.
 
Method:
.......

* fetching observation data 
* fetching model data
* subset the selected geographical region 
* selection of month to be analyzed
* unit conversion to hPa (if necessary)
* regridding (bilinear) to the grid of the observation data (if necessary)
* computing of principal components for dimension reduction

Process identifiers:
....................

 * **weatherregimes_reanalyses**
     to perform weather regime cluster analyses on pre-defined datasets of weather regimes
 * **weatherregimes_models**
     to perform weather regime cluster analyses on climate model data
 * **weatherregimes_projection**
     project trained weather regime clusters on a second dataset 
    

Input Parameter:
................

**resources**
  (links to netCDF sea surface pressure data) or search with phoenix

**dataset** 
  NCEP slp data (automatic fetch)
  
**Region**
  Region for weather regimes classification specified by coordinate bounding box 
  
**Nr. of clusters**
  defines the number of weather regimes to be detected
  

Outputs: 
........

**pressure pattern graphic**
**R workspace**
**PCA data**
**Frequency**

Post-processing:
................

The weather regime process provides a map of the detected pressure patterns according to the centroid. This is a default graphic and might not satisfy the needs of the user. But the process also provides the entire workspace containing all required values to generate an individual plot according to the needs of the user. 
Download the R-Workspace (output of the weather regime process) to your local machine by clicking the download button or run e.g.::

  wget  http://birdhouse-lsce.extra.cea.fr:8090/wpsoutputs/flyingpigeon/output_classification-e49f2d78-8563-11e6-bf14-fbeae168c26e.Rdat

This can be read with R syntax. Here is an example to generate an individual plot:

       
.. code-block:: R

   library(maps)
   load("output_classification-e49f2d78-8563-11e6-bf14-fbeae168c26e.Rdat")
   #save as pdf
   pdf(file="output_graphics.pdf", width=14,height=7)
   #save as eps
   #postscript(file="output_graphics.eps",width=1400,height=700)
   # colorscale 
   bluered=colorRampPalette(c("darkblue","blue","lightblue", "white","white","pink","red","darkred"))
   zcol=c(round(min(dat.class$reg.var/100)),round(max(dat.class$reg.var/100)))
   zlev=seq(zcol[1],zcol[2])
   for(i in 1:nreg){ 
     champ=dat.class$reg.var[,i]/100
     par( mar=c(3,3,2,1.5))
     dum=t(matrix(champ,length(lat),length(lon)))
     #dum=matrix(champ,length(lon),length(lat)) #if transpose
     lat.sort=sort(lat,index.return=TRUE)
     titleplot=paste("WR: ",i,"(",format(dat.class$perc.r[i],digits=3),"%)")
     filled.contour(lon,sort(lat),dum[,lat.sort$ix],color.palette =bluered,
                 asp = 0,nlevels=length(zlev),levels=zlev,
                 plot.title = title(main = titleplot, xlab = "lon", ylab = "lat"),
                 plot.axes={axis(1); axis(2); 
                   contour(lon,sort(lat),dum[,lat.sort$ix],add=T, nlevels=6,lwd=2);
                   library(fields)
                   world(xlim=range(lon),ylim=range(lat),interior=FALSE,add=TRUE)
                 })
   }# end i
   dev.off() #eps or pdf


Examples: 
.........

.. toctree::
   :maxdepth: 2

   /tutorials/weatherregimes
