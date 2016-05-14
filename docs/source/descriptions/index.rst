Processes:
**********

Flyingpigeon provides processes (also named as workflows ) for climate model data analysis, climate impact studies and investigations of extremes. 


Analog pressure pattern
-----------------------


CASTf90 first downloads fields from NCEP reanalysis (sea level pressure, slp, as default) and then searches in a given simulation period for the most similar cases within a given data base period according to a given distance measure. Finally, it writes the N most similar days, including their calculated distances from the reference case, to an output file.


Climate indices
---------------


Climate indices are values that describe the state the climate system for a certain parameter. Climate indices as timeseries can be used to describe or estimate the change in climte over time.
Climate indices have to be calculated for a given time aggregation: 


+-------------+-------------+---------+
|             | Time        |values   |
|             | aggregation |per year | 
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
| JAN         | Januar      | 1       |
+-------------+-------------+---------+

The climate indices processes in flyingpigeon are based on the python package 'Link icclim <http://icclim.readthedocs.org/en/latest/>'.
they are subclassed to 

Simple indices:
===============

Simple indices are based on a single input variable, and with and an simple calculation algorythem.

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


Percentile based indices:
=========================

Percentile based indices are calculated based on an given percentile of a reference periode. 
The calculation of percentile based indices are done in two stepps:

* Calculation of the appropriate percentile value for a given revenence period
* Counting of days beyond the threshold, the sum of days beyond the treshold within the time aggregation is taken as the result 

+------------+----------------+--------------------------------------------------------------------------------+
| Indice     | Input Variable | Definition                                                                     |
+============+================+================================================================================+
|  TG10p     |    tas         |    Days with TG < 10th percentile of daily mean temperature (cold days)        |
+------------+----------------+--------------------------------------------------------------------------------+
|  TX10p     |    tasmax      |    Days with TX < 10th percentile of daily maximum temperature (cold day-times)|
+------------+----------------+--------------------------------------------------------------------------------+
|  TN10p     |    tasmin      |    Days with TN < 10th percentile of daily minimum temperature (cold nights)   |
+------------+----------------+--------------------------------------------------------------------------------+
|  TG90p     |    tas         |    Days with TG > 90th percentile of daily mean temperature (warm days)        |
+------------+----------------+--------------------------------------------------------------------------------+
|  TX90p     |    tasmax      |    Days with TX > 90th percentile of daily maximum temperature (warm day-times)|
+------------+----------------+--------------------------------------------------------------------------------+
|  TN90p     |    tasmin      |    Days with TN > 90th percentile of daily minimum temperature (warm nights)   |
+------------+----------------+--------------------------------------------------------------------------------+
|  WSDI      |    tasmax      |    Warm-spell duration index                                                   |
+------------+----------------+--------------------------------------------------------------------------------+
|  CSDI      |    tasmin      |    Cold-spell duration index                                                   |
+------------+----------------+--------------------------------------------------------------------------------+
|  R75p      |    pr          |    Days with PRCPTOT > 75th percentile of daily amounts (moderate wet days)    |
+------------+----------------+--------------------------------------------------------------------------------+
|  R75pTOT   |    pr          |    Precipitation fraction due to moderate wet days (>75th percentile)          |
+------------+----------------+--------------------------------------------------------------------------------+
|  R95p      |    pr          |    Days with PRCPTOT > 95th percentile of daily amounts (very wet days)        |
+------------+----------------+--------------------------------------------------------------------------------+
|  R95pTOT   |    pr          |    Precipitation fraction due to very wet days (>95th percentile) (%)          |
+------------+----------------+--------------------------------------------------------------------------------+
|  R99p      |    pr          |    Days with PRCPTOT > 99th percentile of daily amounts (extremely wet days)   |
+------------+----------------+--------------------------------------------------------------------------------+
|  R99pTOT   |    pr          |    Precipitation fraction due to extremely wet days (>99th percentile)(%)      |
+------------+----------------+--------------------------------------------------------------------------------+


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


Returntimes
-----------

Calculation of return time Values for 1D timeseries. 

Segetalflora
------------
Species biodiversity of segetal flora. Imput files: variable:tas , domain: EUR-11 or EUR-44.


Species Distribution Model
--------------------------

The process is performing the following steps:

* fetching selected data (GBIF zip file) and climate model data. 
* extraction of GBIF Data
* based on GBIF Data coordinates a presents/absence mask is generated
* calculation of selected climate indices in appropriate time aggregations
* calculation of mean values of climate indices for the given reference period
* statistical training (GAM) based on presents / absence maks and climate indices of reference period
* prediction of favourability based on traind GAM and projected as yearly timeseries for the whole proided time series
* plotting information and storing netCDF files in archive files (tar or zip)

Further reading: 

`Species Favourability Shift in Europe due to Climate Change: A Case Study for Fagus sylvatica L. and Picea abies (L.) Karst. Based on an Ensemble of Climate Models <http://www.hindawi.com/journals/jcli/2013/787250/>`_.


Example with Phoenix (GUI):
...........................


1. login to a Phoenix GUI (e.g. `Compute provider mouflon at DKRZ <https://mouflon.dkrz.de/>`_) 
2. Menu option: Wizard
3. Choose a Favorite: No Favourite 
4. Choose a Web Processing Service: Flyingpigeon 
5. Choose WPS Process of Flyingpigeon: Species distribution model - Species distribution model (SDM) 
6. Literal inputs of Species distribution model :

.. image:: ../pics/sdm_literalinputs.png

7. Choose Input Parameter of Species distribution model: netCDF
8. Choose Data Source : Earth System Grid (ESGF)
            
The next window is the Data search interface to the available data of the ESGF archive. The input files are used to calculate the climate conditions. Dipending on the selection of indices appropriate variables has to be provided (an indice based on pricipitation needs 'pr' as input variable ). multiple selection can be done while pressing the Cntr button. With other search options the dataselection shoule look like this example: 

.. image:: ../pics/sdm_esgfsearch.png

10. You can optional check or uncheck  'Save as Favourite' with an appropriate name;  and submit the job.  
11. Done!!!

You can follow the log file of your process in the monitor (klick on the job ID e.g. a4aa98de-ffde-11e5-b50a-bb0d01b14483). Manual reload of your browser site is necessary.


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

Example PYTHON call:
....................

`Asyncron Link creation <https://github.com/bird-house/flyingpigeon/blob/master/notebooks/WPS_weatherregimes.ipynb>`


Example with Phoenix (GUI):
...........................

- login to a Phoenix GUI (e.g. `Compute provider mouflon at DKRZ <https://mouflon.dkrz.de/>`_) 

- Menu option: Wizard
- Choose a Favorite: No Favourite 
- Choose a Web Processing Service: Flyingpigeon 
- Choose WPS Process of Flyingpigeon: Weather Regimes
- Literal inputs of Weather Regimes: given is a default, feel free to change ;-) 
- Choose Input Parameter of Weather Regimes: netCDF
- Choose Data Source : Earth System Grid (ESGF) 
            
The next window is the Data search interface to the available data of the ESGF archive. Weather regimes are computed based on sea surface pressure values. The appropriate variable for CMIP5 data are 'psl'. With other search options the dataselection shoule look like this example: 

.. image:: ../pics/esgf_search_psl.png

You can save your settings as favourite. And submit the job.  
Done!!!

You follow the log file of your in the monitor (klick on the job ID e.g. a4aa98de-ffde-11e5-b50a-bb0d01b14483). manual reload of your browser site is necessary.

.. image:: ../pics/monitor_log_weatherregimes.png

