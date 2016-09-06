Processes
*********

Flyingpigeon provides processes for climate model data analysis, climate impact studies and investigations of extremes. 
A combination of processes will be called 'workflow'.


Analogues of circulation
------------------------

Analogues of circulation provide a versatile tool to investigate the relation between climate variables (such as temperature or precipitation) and large-scale atmospheric circulation patterns (SLP or Zg (h)). The depoyed software in the analogues processes are the circulation analogue simulation tool (`CASTf90 <https://github.com/sradanov/castf90>`_). 

Methode: 
........

Here should come a bit of a method description. 


Processes:
..........

  * **Analogues_reanalyses:** 
      Includes a preselection of reanalyses pressure data ( sea surface or geopotential hight)
      This process fetches the data based on the selected reanalyses dataset and variable, so no input files has to be provided by the user. 
  * **Analogues_model:** 
      Is designed to analyse a climte model data set. The location of the input files has to be provided by the user (using the ESGF serach interface is possible). 
  * **Analogues_compare:** 
      To search analogue days in a climte model dataset for given days in a reanalyses dataset.
  * **Analogues_viewer:** 
      Analougs data output as an txt file can be visualized in a html page with interactive graphics.  
 

Input parameter: 
................

**Data experiment:** 
 Input Data

**Region** 
 Region to be analysed

**Start data of anayse period** 
 Starting date of the period to be analysed

**End date of analyse period** 
 End date of the period to be analyzed

**Start reference period** 
 Start day of period where analogues days will be picked

**End reference period** 
 End day of period where analogues days will be picked

**Nr of analogues** 
 Number of analogues to be detected and written out as results. They are ordered with increasing distance to the original pressure pattern.

**Seasonal window** 
 Number of day for a calendar proximity. 
 It defines the time window around the given date in all years except the year of the given day.

**normalisation** 
 Pressure values can be normalized (substraction of average value over the whole period). possible options are:

 * None 
    No normalisation 
 * based 
    normalisation based on reference period
 * sim 
    normalisation based on analyse period
 * own 
    normalisation of reference and analyse data values by there own average
   
**Distance** 
 Method to calculate the distance

 * euclidean  
 * mahalanobis 
 * cosine 
 * of
 
**output file format**
  * netCDF 
     output values will be provided as a netCDF file 
  * ascii  
     output values will be provided as an ascii file
  
**Time window** 
  Values of analyse period can be smoothed by averaging with the values of the following days. 'time window' is giving the number of days averaging.  
  (default=1)


Outputs: 
........

**Config File**
  Configuration file use by the CASTf90 software
  
**Analogs File**
  list of analogues days
  
**Target netCDF**
  pressure values as input for CASTf90 for analogues days to be picked
  
**prepared netCDF**
  pressure values as input for CASTf90 for analogues days to be searched format
  
**html viewer**
  output of the analogues_viewer process for data visualisation

**modified analogues txt file**
  output of the analogues_viewer process. input data for viewer generation.


Climate indices
---------------

Climate indices are values that describe the state the climate system for a certain parameter. Climate indices as timeseries can be used to describe or estimate the change in climte over time.

The climate indices processes in flyingpigeon are based on the `python package icclim <http://icclim.readthedocs.io/en/latest/>`_
They are subclassed to:

.. toctree::
   :maxdepth: 1
   
   indices


Climate indices have to be calculated for a time aggregation:

+-------------+-------------+---------+
| Time        |Description  |values   |
| aggregation |             |per year | 
+-------------+-------------+---------+
| mon         | monthly     | 12      |
+-------------+-------------+---------+
| sem         | seasonal    | 4       |          
+-------------+-------------+---------+
| yr          | yearly      | 1       |
+-------------+-------------+---------+
| ONDJFM      | winter half | 1       |
+-------------+-------------+---------+ 
| AMJJAS      | summer half | 1       |
+-------------+-------------+---------+
| DJF         | winter      | 1       |                
+-------------+-------------+---------+ 
| MAM         | Spring      | 1       |
+-------------+-------------+---------+ 
| JJA         | Summer      | 1       |
+-------------+-------------+---------+ 
| SON         | Autumn      | 1       |
+-------------+-------------+---------+ 
| Jan         | Januar      | 1       |
+-------------+-------------+---------+
| Feb         | Februar     | 1       |
+-------------+-------------+---------+
| Mar         | March       | 1       |
+-------------+-------------+---------+
| April       | April       | 1       |
+-------------+-------------+---------+
| May         | May         | 1       |
+-------------+-------------+---------+
| Jun         | June        | 1       |
+-------------+-------------+---------+
| Jul         | July        | 1       |
+-------------+-------------+---------+
| Aug         | August      | 1       |
+-------------+-------------+---------+
| Sep         | September   | 1       |
+-------------+-------------+---------+
| Oct         | October     | 1       |
+-------------+-------------+---------+
| Nov         | November    | 1       |
+-------------+-------------+---------+
| Dec         | December    | 1       |
+-------------+-------------+---------+

   

Download Resources
------------------

Downloads resources (limited to 50GB) to the local file system of the birdhouse computer provider.


EOBS to CORDEX
--------------

converts EOBS data files into the CORDEX convention. (variable names, attributes, etc ... ).


extract Timeseries
------------------

Extract 1D Timeseries for specified coordinates from gridded data.


Robustness of an ensemble
-------------------------

Calculates the robustness as the ratio of noise to signal in an ensemble of timeseries.
Good luck 


Return times
------------

Calculation of return time Values for 1D timeseries. 

Segetal flora
------------
Species biodiversity of segetal flora. Imput files: variable:tas , domain: EUR-11 or EUR-44.


Species Distribution Model
--------------------------

Prediction of growth favourability for tree species. 

Further reading: 

`Species Favourability Shift in Europe due to Climate Change: A Case Study for Fagus sylvatica L. and Picea abies (L.) Karst. Based on an Ensemble of Climate Models <http://www.hindawi.com/journals/jcli/2013/787250/>`_.


.. toctree::
   :maxdepth: 1
   
   sdm   
   /tutorials/sdm


   
.. _subset_countries: 

Subset Countries 
----------------

Generates a polygon subset of input netCDF files.


Based on an ocgis call, several predfined polygons ( world counties ) can be used to generate an appropriate subset of input netCDF files. 
The option 'MOSAIK' as a checkbox allows you to decide, in the case of multiple polygon selection, if the polygons should be stitched together into one polygon (e.g. shape of Germany and France as one polygon) or calculated as separate output files. 

For optimisation of the subset process, the appropriate shapefiles are prepared as follows: 


.. toctree::
   :maxdepth: 1

   shapefilepreparation
   
.. _visualisation: 

Visualisation
-------------

Time series visualisation of netCDF files. 
Creates a spaghetti plot and an uncertainty plot.


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

Process Arguments:
..................

* resources (links to netCDF sea surface pressure data) 
* or search with phoenix



Inputs:
.......

* NCEP slp data (automatic fetch)
* any kind of surface pressure data (netCDF files in cf convention). Multiple Datasets slized in seperate files possible

Outputs: 
........

* scatter plot showing the centoides of the clusters and the appropriate centroids of each timestep
* maps for each weather regime of all input datasets. including comparison statistics with observation pattern
* tar archive containing text files with date time , weatherregime table


