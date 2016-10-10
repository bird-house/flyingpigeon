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

Postorcessing:
..............

The weather regime process provides a map of the detected pressure patterns according to the centriode. This is a default graphic and might not satisfy the needs of the user. But the process is also providing the intire workspace containing all required values to generate a individual plot according to the need of the user. 
Download the R-Workspace (output of weather regime process) to your local machine by klicking the download button or run e.g.::

  wget  http://birdhouse-lsce.extra.cea.fr:8090/wpsoutputs/flyingpigeon/output_classification-e49f2d78-8563-11e6-bf14-fbeae168c26e.Rdat

to fetch the R-workspace to your local machine. This can be read with R syntax. An example to generate an idividual plot can:

       
.. code-block:: R

   library(maps)
   load("output_classification-e49f2d78-8563-11e6-bf14-fbeae168c26e.Rdat")

   pdf(file="output_graphics.pdf")

   add_legend <- function(...) {
    opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
                mar=c(0, 0, 0, 0), new=TRUE)
    on.exit(par(opar))
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    legend(...)
   }
   layout(matrix(1:(2*ceiling(nreg/2)),2,ceiling(nreg/2)))
   par(mar=c(4,5,2,2.5))
   for(i in 1:nreg){ 
    champ=dat.class$reg.var[,i]/100
    zcol=c(round(min(dat.class$reg.var/100)),round(max(dat.class$reg.var/100)))
    zlev=seq(zcol[1],zcol[2])
    colplot=rainbow(length(zlev)-1,start=3/6,end=1)
    par( mar=c(2.5,2,2,1.5))
    
    dum=t(matrix(champ,length(lat),length(lon)))
    #dum=matrix(champ,length(lon),length(lat)) #if transpose
    lat.sort=sort(lat,index.return=TRUE)
    titleplot=paste("WR: ",i,"(",format(dat.class$perc.r[i],digits=3),"%)")
    contour(lon,sort(lat),dum[,lat.sort$ix],
            xlab="Longitude",ylab="Latitude",main=titleplot,col=colplot,add=FALSE,nlevels=length(zlev),
            levels=zlev,lty=1, cex.main=0.95)
    library(fields)
    world(xlim=range(lon),ylim=range(lat),interior=FALSE,add=TRUE)
   }#end for i

   add_legend("right", legend=zlev[1:length(zlev)-1],pch=20, 
             col=colplot,
             horiz=FALSE, bty='n', cex=0.8)

   dev.off()


Examples: 
.........

.. toctree::
   :maxdepth: 2

   /tutorials/weatherregimes
