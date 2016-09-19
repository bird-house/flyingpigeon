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


Examples: 
.........

.. toctree::
   :maxdepth: 2

   /tutorials/weatherregimes
