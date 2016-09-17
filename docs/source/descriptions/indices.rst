Climate indices
---------------

Climate indices are values that describe the state the climate system for a certain parameter. Climate indices as timeseries can be used to describe or estimate the change in climate over time.

The climate indices processes in flyingpigeon are based on the `python package icclim <http://icclim.readthedocs.io/en/latest/>`_
They are subclassed to:


Process identifiers: 
....................

  * **Simple indices**
    Simple indices are based on a single input variable, and with and a simple calculation algorithm.

  * **Percentile indices**
    Percentile-based indices are calculated based on an given percentile of a reference period. 
    The calculation of percentile-based indices is done in two steps:

    Calculation of the percentile value for a given reference period
    Cumulative sum of the number of days beyond the threshold 
    
  * **Multivariable indices**
  * **User defined indices**
  

Input Parameter:
................    
    
*for simple indices*

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
| CWD    |         pr     | Longest period of consecutive wet days                                         |
+--------+----------------+--------------------------------------------------------------------------------+
| CDD    |         pr     | Longest period of consecutive dry days                                         |
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


*Percentile-based indices*


+------------+----------------+--------------------------------------------------------------------------------+
| Index      | Input Variable | Definition                                                                     |
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


*for Multivariable Indices*


+------------+----------------+--------------------------------------------------------------------------------+
| Index      | Input Variable | Definition                                                                     |
+============+================+================================================================================+
| CD         | tas , pr       | Days with TG < 25th percentile of daily mean temperature and                   |
|            |                | PRCPTOT < 25th percentile of daily precipitation sum (cold/dry days)           |
+------------+----------------+--------------------------------------------------------------------------------+
| CW         | tas , pr       | Days with TG < 25th percentile of daily mean temperature and                   |
|            |                | PRCPTOT > 75th percentile of daily precipitation sum (cold/wet days)           |
+------------+----------------+--------------------------------------------------------------------------------+
| WD         | tas , pr       | days with TG > 75th percentile of daily mean temperature and                   |
|            |                | PRCPTOT < 25th percentile of daily precipitation sum (warm/dry days)           |
+------------+----------------+--------------------------------------------------------------------------------+
| WW         | tas, pr        | Days with TG > 75th percentile of daily mean temperature and                   |
|            |                | PRCPTOT > 75th percentile of daily precipitation sum (warm/wet days)           |
+------------+----------------+--------------------------------------------------------------------------------+

*Climate indices have to be calculated for a time aggregation*

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


**Mosaic**

To be checked if multiple polygons should be merged into one polygon.


Output: 
.......

**tar archive**
  Tar archive containing all netCDF files. Subsetting is performed for each input dataset. 

**netCDF example**
  One netCDF file is picked out as an example file to be displayed on the web mapping service. 
