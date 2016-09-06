Subsets 
-------

Generates a polygon subset of input netCDF files. Based on an ocgis call, several predfined polygons ( world counties ) can be used to generate an appropriate subset of input netCDF files. 

Method:
.......

Integrated ocgis performing the subsetting. 


Process idetifiers:
...................

  * **subset_countinents**
      subsetting continets
  * **subset_countries**
      subsetting counties
  * **subset_europeanregions**
      subsetting European regions
  * **subset_points**    
      extracting of timeseries for given coordinate subset_points

Input parameter: 
................

**Polygons**
  Abreviation of the appropriate polygon
  
**Mosaic**
  The option 'MOSAIK' as a checkbox allows you to decide, in the case of multiple polygon selection, if the polygons should be stitched together into one polygon (e.g. shape of Germany and France as one polygon) or calculated as separate output files. 


Shapefile optimisation:   
.......................

For optimisation of the subset process, the appropriate shapefiles are prepared as follows: 

.. toctree::
   :maxdepth: 1

   shapefilepreparation
   