Species Distribution Model
--------------------------

The processes related to species distribution models (SDM) produce data showing the growth favourability for single tree species
related to climate conditions. Generally, the input data consist of tree observations (coordinate points of tree occurences) and climate indices based on temperature at surface and precipitation. With statistical methods (general additive models [GAM]), the spatial favourability is calculated for each year of the input timeseries.
The SDM processes are able to handle ensemble datasets and output a favourability dataset for each ensemble member.


Processing steps in an SDM experiment
.....................................

  * fetching tree occurences of a specific tree species from the GBIF database
  * calculation of climate indices timeseries based on climate model raw data
  * generation of a presence/absence mask based on the GBIF data coordinates and the grid resolution of the climate indices data
  * calculation of time mean values for the climate indices data for the given reference period
  * statistical training (GAM) based on presence/absence mask and climate indices of a reference period
  * calculation of favourability as yearly timeseries for each dataset based on the statistically-trained GAM


SDM-related processes
......................

Besides a big 'all-in-one' process which contains all the analysis steps from fetching of raw data to the final output, the processes can also be run as parts.
The following processes are available:


+----------------+------------------------------------------------------------------------------------------------+
| Indentifier    | Description                                                                                    |
+================+================================================================================================+
| sdm_getgbif    | Only fetching GBIF tree occurence data                                                         |
+----------------+------------------------------------------------------------------------------------------------+
| sdm_getindices | Only calculation of climate indices                                                            |
+----------------+----------------+-------------------------------------------------------------------------------+
| sdm_csvindices | output of sdm_getgbif and sdm_getindices as input to run an SDM experiment                     |
+----------------+----------------+-------------------------------------------------------------------------------+
| sdm_csv        | output of sdm_getgbif and raw climate model data as input to run an SDM experiment             |
+----------------+------------------------------------------------------------------------------------------------+
| sdm_allinone   | All required steps are performed in this process to run an SDM process (!! time consuming !!)  |
+----------------+------------------------------------------------------------------------------------------------+

Further reading: `Species Favourability Shift in Europe due to Climate Change:
A Case Study for Fagus sylvatica L. and Picea abies (L.) Karst. Based on an Ensemble of Climate Models
<http://www.hindawi.com/journals/jcli/2013/787250/>`_.


.. _sdmindices:

Climate Indices for SDM:
........................

+----------------+----------------+--------------------------------------------------------------------------------+
| Index          | Input Variable | Definition                                                                     |
+================+================+================================================================================+
| TG_yr          | tas            | Mean of mean temperature per year                                              |
+----------------+----------------+--------------------------------------------------------------------------------+
| TG_AMJJAS      | tas            | Mean of mean temperature from April to September                               |
+----------------+----------------+--------------------------------------------------------------------------------+
| TG_ONDJFM      | tas            | Mean of mean temperature from October to March                                 |
+----------------+----------------+--------------------------------------------------------------------------------+
| TG_JJA         | tas            | Mean of mean temperature from June to August                                   |
+----------------+----------------+--------------------------------------------------------------------------------+
| GD4_yr         | tas            | Growing degree days [sum of TG >= 4 degrees] per year                          |
+----------------+----------------+--------------------------------------------------------------------------------+
| TNn_yr         | tasmin         | Minimum of minimum temperature per year                                        |
+----------------+----------------+--------------------------------------------------------------------------------+
| TNn_AMJJAS     | tasmin         | Minimum of minimum temperature from April to September                         |
+----------------+----------------+--------------------------------------------------------------------------------+
| TNn_ONDJFM     | tasmin         | Minimum of minimum temperature from October to March                           |
+----------------+----------------+--------------------------------------------------------------------------------+
| TNn_Jan        | tasmin         | Minimum of minimum temperature in January                                      |
+----------------+----------------+--------------------------------------------------------------------------------+
| FD_ONDJFM      | tasmin         | Nr of frost days  [tasmin < 0°C] from October to March                         |
+----------------+----------------+--------------------------------------------------------------------------------+
| FD_April       | tasmin         | Nr of frost days  [tasmin < 0°C] in April                                      |
+----------------+----------------+--------------------------------------------------------------------------------+
| FD_May         | tasmin         | Nr of frost days  [tasmin < 0°C] in May                                        |
+----------------+----------------+--------------------------------------------------------------------------------+
| FD_June        | tasmin         | Nr of frost days  [tasmin < 0°C] in June                                       |
+----------------+----------------+--------------------------------------------------------------------------------+
| CFD_ONDJFM     | tasmin         | Longest period of consecutive frost days from October to March                 |
+----------------+----------------+--------------------------------------------------------------------------------+
| ID_yr          | tasmax         |  Nr of ice days [tasmax < 0°C] per year                                        |
+----------------+----------------+--------------------------------------------------------------------------------+
| SU_yr          | tasmax         | Summer days [tasmax > = 25°C] per year                                         |
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


Examples
........

.. toctree::
   :maxdepth: 1

   /tutorials/sdm
