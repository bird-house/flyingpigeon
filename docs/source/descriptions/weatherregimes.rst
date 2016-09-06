.. _weatherregimes:

Weather Regimes
---------------

Calculation of weatherregimes based on pressure patterns (kmean method). The processes is performing a pattern clusterfication for observations data ( NCEP ) as well as to model data. both results are compared
 
Method:
.......

* fetching observation data 
* fetching model data
* subset the selected geographical region 
* selection of month to be analyzed
* unit conversion to hPa (if necessary)
* regridding (biliniar) to the grid of observation (if necessary)
* comuting of pricipal componets for dimension reduction

Process identifiers:
....................

 * **weatherregimes_reanalyses**
     to perform weatherregime cluster analyses on predifined datasets of weatherregimes
 * **weatherregimes_models**
     to perform weatherregime cluster analyses on climate model data
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
  defining the number of Weatherregies to be detected
  

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
