.. _processes:

Processes in Flyingpigeon
*************************

Flyingpigeon provides processes for climate model data analysis, climate impact studies and investigations of extremes. 



.. _analogs:

Analog pressure pattern
-----------------------


CASTf90 first downloads fields from NCEP reanalysis (sea level pressure, slp, as default) and then searches in a given simulation period for the most similar cases within a given data base period according to a given distance measure. Finally, it writes the N most similar days, including their calculated distances from the reference case, to an output file.


.. _extract_coordinates

extract 1D Timeseries from coordinate points
--------------------------------------------

Extract Timeseries for specified coordinates from gridded data.

.. _extremevalues: 

Extremevalues
-------------

Calculation of return time Values for 1D timeseries. 


.. _getEOBS_inCORDEXformat: 

get EOBS Data in CORDEX format
-------------------------------

converts EOBS data files into the CORDEX convention. (variable names, attributes, etc ... ).

.. _fetch: 


Download Resources
------------------

Downloads resources (limited to 50GB) to the local file system of the birdhouse computer provider.

.. _indices:

Climate indices
---------------

Climate indices are values that describe the state the climate system for a certain parameter. Climate indices as timeseries can be used to describe or estimate the change in climte over time. 
The climate indices processes in flyingpigeon are based on the python package 'Link icclim <http://icclim.readthedocs.org/en/latest/>'.
they are subclassed to 

* index simple: 


+--------+----------------+--------------------------------------------------------------------------------+
| Index  | Input Variable | Definition                                                                     |
+========+================+================================================================================+
| TG     |        tas     | Mean of mean temperature                                                       |
+--------+----------------+--------------------------------------------------------------------------------+
| TX     |     tasmax     | Mean of max temperature                                                        |
+--------+----------------+--------------------------------------------------------------------------------+
| TN     |     tasmin     | Mean of daily min temperature                                                  |
+--------+----------------+--------------------------------------------------------------------------------+
| TXn    |     tasmax     | Min of daily min temperature                                                   |
+--------+----------------+--------------------------------------------------------------------------------+
| TXx    |     tasmax     | Max of daily max temperature                                                   |
+--------+----------------+--------------------------------------------------------------------------------+
| TNn    |     tasmin     | Min of daily min temperature                                                   |
+--------+----------------+--------------------------------------------------------------------------------+
| TNx    |     tasmin     | Max of daily min temperature                                                   |
+--------+----------------+--------------------------------------------------------------------------------+
| SU     |     tasmax     | Nr of summer days  [tasmax > = 25°C]                                           |
+--------+----------------+--------------------------------------------------------------------------------+
| CSU    |     tasmax     | Nr of consecutive summer days                                                  |
+--------+----------------+--------------------------------------------------------------------------------+
| FD     |     tasmin     | Nr of frost days  [tasmin < 0°C]                                               |
+--------+----------------+--------------------------------------------------------------------------------+
| CFD    |     tasmin     | Nr of consecutive frost days                                                   |
+--------+----------------+--------------------------------------------------------------------------------+
| TR     |      tasmin    | Tropical nights [tasmin >= 20°C]                                               |
+--------+----------------+--------------------------------------------------------------------------------+
| ID     |     tasmax     | Nr of Ice days [tasmax < 0°C]                                                  |
+--------+----------------+--------------------------------------------------------------------------------+
| HD17   |        tas     | Heating degree days [sum of 17 degrees - mean temperature]                     |
+--------+----------------+--------------------------------------------------------------------------------+
| GD4    |        tas     | Growing degree days [sum of TG >= 4 degrees]                                   |
+--------+----------------+--------------------------------------------------------------------------------+
| PRCPTOT|         pr     | Precipitation total amount [sum]                                               |
+--------+----------------+--------------------------------------------------------------------------------+
| RR1    |         pr     | Nr of days with precipitation > 1 mm                                           |
+--------+----------------+--------------------------------------------------------------------------------+
| CWD    |         pr     | Consecutive wet days                                                           |
+--------+----------------+--------------------------------------------------------------------------------+
| CDD    |         pr     | Consecutive dry days                                                           |
+--------+----------------+--------------------------------------------------------------------------------+
| SDII   |         pr     | Simple daily intensity index for wet days [mm/wet day]                         |
+--------+----------------+--------------------------------------------------------------------------------+
| R10mm  |         pr     | Nr of days > 10mm                                                              |
+--------+----------------+--------------------------------------------------------------------------------+
| R20mm  |         pr     | Nr of days with precipitation >= 20 mm                                         |
+--------+----------------+--------------------------------------------------------------------------------+
| RX1day |         pr     | Highest 1-day precipitation amount                                             |
+--------+----------------+--------------------------------------------------------------------------------+
| RX5day |         pr     | Highest 5-day precipitation amount                                             |
+--------+----------------+--------------------------------------------------------------------------------+
| SD     |       prsn     | Nr of snow days                                                                |
+--------+----------------+--------------------------------------------------------------------------------+
| SD1    |       prsn     | Nr of days with snow >= 1cm                                                    |
+--------+----------------+--------------------------------------------------------------------------------+
| SD5cm  |       prsn     | Nr of days with snow >= 5cm                                                    |
+--------+----------------+--------------------------------------------------------------------------------+
| SD50cm |       prsn     | Nr of days with snow >= 50 cm                                                  |
+--------+----------------+--------------------------------------------------------------------------------+




* index percentile based

_INDICESper_ = dict(
    TG10p=dict(variable='tas', description='Days with TG < 10th percentile of daily mean temperature (cold days) (days)'),
    TX10p=dict(variable='tasmax', description='Days with TX < 10th percentile of daily maximum temperature (cold day-times) (days)'),
    TN10p=dict(variable='tasmin', description='Days with TN < 10th percentile of daily minimum temperature (cold nights) (days)'),
    TG90p=dict(variable='tas', description='Days with TG > 90th percentile of daily mean temperature (warm days) (days)'),
    TX90p=dict(variable='tasmax', description='Days with TX > 90th percentile of daily maximum temperature (warm day-times) (days)'),
    TN90p=dict(variable='tasmin', description='Days with TN > 90th percentile of daily minimum temperature (warm nights) (days)'),
    WSDI=dict(variable='tasmax', description='Warm-spell duration index (days)'),
    CSDI=dict(variable='tasmin', description='Cold-spell duration index (days)'),
    R75p=dict(variable='pr', description= 'Days with PRCPTOT > 75th percentile of daily amounts (moderate wet days) (days)'),
    R75pTOT=dict(variable='pr', description= 'Precipitation fraction due to moderate wet days (>75th percentile) (%)'),
    R95p=dict(variable='pr', description= 'Days with PRCPTOT > 95th percentile of daily amounts (very wet days) (days)'),
    R95pTOT=dict(variable='pr', description= 'Precipitation fraction due to very wet days (>95th percentile) (%)'),
    R99p=dict(variable='pr', description= 'Days with PRCPTOT > 99th percentile of daily amounts (extremely wet days)(days)'),
    R99pTOT=dict(variable='pr', description= 'recipitation fraction due to extremely wet days (>99th percentile)(%)'),
    )  
    

.. _ensemble_Robustness:

Robustness of an ensemble
-------------------------

Calculates the robustness as the ratio of noise to signal in an ensemble of timeseries.


.. _segetalflora: 

Segetalflora
------------
Species biodiversity of segetal flora. Imput files: variable:tas , domain: EUR-11 or EUR-44.

.. _sdm: 

Species Distribution Model
--------------------------

Statistical approach to calculate the spatial favorability of climate-sensitive species.

The appraoch is performed in two steps:

* Statistical training with species presents absence data and historical climate data
* future projection based on the statistical training

.. _subset_countries: 

Subset Counties 
---------------

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

Calculation of weather regimes based on pressure patterns (k-means clustering method). The processes clusters data into a predefined number of clusters. Performed on observation data ( NCEP ) and model data and the results of the clustering are compared.
 
processing stepps: 

* fetch observation data 
* fetch model data
 
