Subsets 
-------

Generates a polygon subset of input netCDF files. Based on an ocgis call, several pre-defined polygons (e.g. world countries) can be used to generate an appropriate subset of input netCDF files. 

Method:
.......

Integrated ocgis performs the subsetting. 


Process idetifiers:
...................

  * **subset_continents**
      subsetting continents
  * **subset_countries**
      subsetting countries
  * **subset_europeanregions**
      subsetting European regions
  * **subset_points**    
      extracting of timeseries for given coordinate subset_points

Input parameter: 
................

**Polygons**
  Abbreviation of the appropriate polygon
  
**Mosaic**
  The option 'MOSAIC' as a checkbox allows you to decide, in the case of multiple polygon selection, if the polygons should be stitched together into one polygon (e.g. shape of Germany and France as one polygon) or calculated as separate output files. 


Shapefile optimisation:   
.......................

For optimisation of the subset process, the appropriate shapefiles are prepared as follows: 

.. toctree::
   :maxdepth: 1

   shapefilepreparation
   