Species Distribution Model
--------------------------

Prediction of growth favourability for tree species. 


Method:
.......


  * fetching selected data (GBIF zip file) and climate model data. 
  * extraction of GBIF Data
  * based on GBIF data coordinates, a presence/absence mask is generated
  * calculation of selected climate indices in appropriate time aggregations
  * calculation of mean values of climate indices for the given reference period
  * statistical training (GAM) based on presence/absence mask and climate indices of reference period
  * prediction of favourability based on trained GAM and projected as yearly timeseries for the provided time series
  * plotting information and storing netCDF files in archive files (tar or zip)

  
Further reading: `Species Favourability Shift in Europe due to Climate Change: A Case Study for Fagus sylvatica L. and Picea abies (L.) Karst. Based on an Ensemble of Climate Models <http://www.hindawi.com/journals/jcli/2013/787250/>`_.


Processes identifiers:
.....................

  * **SDM_csv**
    species distribution coordinates are provided by a csv table to be uploaded
  * **SDM_gbif**
    species distribution coordinates are fetched from GBIF database based on a scientific name 

Input Paramter: 
...............

**Indices**

+----------------+----------------+--------------------------------------------------------------------------------+
| Index          | Input Variable | Definition                                                                     |
+================+================+================================================================================+
| TG_yr          | tas            | Mean of mean temperature per year                                              |
+----------------+----------------+--------------------------------------------------------------------------------+
| TG_AMJJAS      | tas            | Mean of mean temperature April to September                                    |
+----------------+----------------+--------------------------------------------------------------------------------+
| TG_ONDJFM      | tas            | Mean of mean temperature October to March                                      |
+----------------+----------------+--------------------------------------------------------------------------------+
| TG_JJA         | tas            | Mean of mean temperature June to August                                        |
+----------------+----------------+--------------------------------------------------------------------------------+
| TNn_yr         | tasmin         | Minimum of minimum temperature per year                                        |
+----------------+----------------+--------------------------------------------------------------------------------+
| TNn_AMJJAS     | tasmin         | Minimum of minimum temperature April to September                              |
+----------------+----------------+--------------------------------------------------------------------------------+
| TNn_ONDJFM     | tasmin         | Minimum of minimum temperature October to March                                |
+----------------+----------------+--------------------------------------------------------------------------------+
| TNn_Jan        | tasmin         | Minimum of minimum temperature in January                                      |
+----------------+----------------+--------------------------------------------------------------------------------+
| SU_yr          | tasmax         | Summer days [tasmax > = 25°C] per year                                         |
+----------------+----------------+--------------------------------------------------------------------------------+
| FD_ONDJFM      | tasmin         | Nr of frost days  [tasmin < 0°C] in October to March                           |
+----------------+----------------+--------------------------------------------------------------------------------+
| FD_April       | tasmin         | Nr of frost days  [tasmin < 0°C] in April                                      |
+----------------+----------------+--------------------------------------------------------------------------------+
| FD_May         | tasmin         | Nr of frost days  [tasmin < 0°C] in May                                        |
+----------------+----------------+--------------------------------------------------------------------------------+
| FD_June        | tasmin         | Nr of frost days  [tasmin < 0°C] in June                                       |
+----------------+----------------+--------------------------------------------------------------------------------+
| CFD_ONDJFM     | tasmin         | Longest period of consecutive frost days in October to March                   |
+----------------+----------------+--------------------------------------------------------------------------------+
| ID_yr          | tasmax         |  Nr of ice days [tasmax < 0°C] per year                                        |
+----------------+----------------+--------------------------------------------------------------------------------+
| GD4_yr         | tas            | Growing degree days [sum of TG >= 4 degrees] per year                          |
+----------------+----------------+--------------------------------------------------------------------------------+
| PRCPTOT_yr     | pr             | Precipitation total amount [sum] per year                                      |
+----------------+----------------+--------------------------------------------------------------------------------+
| PRCPTOT_ONDJFM | pr             | Precipitation total amount [sum] in winter half                                |
+----------------+----------------+--------------------------------------------------------------------------------+
| PRCPTOT_AMJJAS | pr             | Precipitation total amount [sum] in summer half                                |
+----------------+----------------+--------------------------------------------------------------------------------+
| PRCPTOT_MAM    | pr             | Precipitation total amount [sum] in spring (March to May)                      |
+----------------+----------------+--------------------------------------------------------------------------------+
| PRCPTOT_JJA    | pr             | Precipitation total amount [sum] in summer (June to August)                    |
+----------------+----------------+--------------------------------------------------------------------------------+
| RR1_yr         | pr             | Nr of days with precipitation > 1 mm per year                                  |
+----------------+----------------+--------------------------------------------------------------------------------+
| CDD_AMJJAS     | pr             | Consecutive dry days precipitation < 1 mm in summer half                       |
+----------------+----------------+--------------------------------------------------------------------------------+

Outputs: 

to be checked

Examples:
..........


.. toctree::
   :maxdepth: 1
   
   /tutorials/sdm
