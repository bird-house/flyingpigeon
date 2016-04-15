.. _processes:

Processes in Flyingpigeon
*************************

Flyingpigeon provides processes for climate model data analytis, climate impact studies and extrem values investigatins.

.. _analogs:

Analog pressure pattern
-----------------------

CASTf90 first downloads fields from NCEP reanalysis (sea level pressure, slp, as default) and then searches for a given simulation period the most similar cases within a given data base period according to a given distance measure. Finally it writes the N most similar days including the calculated distances for them to an output file


.. _extract_coordinates

extract 1D Timeseries from coordinate points
--------------------------------------------

Extract Timeseries for specified coordinates from grided data


.. _extremvalues: 

Extremvalues
-------------

Calculation of retun time Values for 1D time series. 


.. _getEOBS_inCORDEXformat: 

get EOBS Data in CORDEX format
-------------------------------

converts EOBS data files into the CORDEX convetion. (variable names, attributes etc ... )

.. _fetch: 


Download Resources
------------------

Downloads resources (limited to 50GB) to the local file system of the birdhouse compute provider

.. _indices:

Climate indice
--------------

Climate indice are values to describe the state the climate system for a certain parameter. Climate indice as timeseries can be used to describe or estimate the climte change over time. 
The climate indices processes in flyingpigeon are based on the python package `icclim <http://icclim.readthedocs.org/en/latest/>`_.
they are subcassed to 

.. toctree::
  indice simple
  -------------
  
  indice percentile based
  -----------------------
  
  indice multi varaibales 
  -----------------------
  
  indices custom
  --------------
  

.. _ensemble_Robustness:

Robustness of an ensemble
-------------------------

Calculates the robustness as the ratio of noise to signal in an ensemle of timeseries


.. _segetalflora: 

Segetalflora
------------
Species biodiversity of segetal flora. Imput files: variable:tas , domain: EUR-11 or EUR-44

.. _sdm: 

Species Distribution Model
--------------------------

Statistical approach to calculate the spatial favorability of climate sensitive species.

The appraoch is to be performed in two steps:

* Statistical training with species presents absense data and historical climate data
* future projection based on the statistical training

The algorithm is described in the `Journal of Climatology <http://www.hindawi.com/journals/jcli/2013/787250/>`_.

.. _subset_countries: 

Subset Counties 
---------------

generates a polygon subset of input netCDF files 


Based on an ocgis call, several predfined polygons ( world counties ) can be used to generate an appropriate subset of input netCDF files. 
The option 'MOSAIK' as an checkbox allows you to decide in case of multiple polygon selection, if the polygons are stiched together to one polygon (e.g. shape of Germany and France as one polygon) or calculated as seperte output files. 

For optimisation of processing the subset, the appropriate shapefile are prepared with the following stepps: 


.. toctree::
   :maxdepth: 1

   shapefilepreparation
   
.. _visualisation: 

Visualisation
-------------

Time series visualisation of netCDF files. 
Creating a spagetti plot and an uncertainty plot.


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
* 

Process Arguments:
..................

* resources (links to netCDF sea surface pressure data) 
|_ or search with phoenix 




Inputs:
.......

* NCEP slp data (automatic fetch)
* any kind of surface pressure data (netCDF files in cf convention). Multiple Datasets slized in seperate files possible

Outputs: 
.......

* scatter plot showing the centoides of the clusters and the appropriate centroids of each timestep
* maps for each weather regime of all input datasets. including comparison statistics with observation pattern
* tar archive containing text files with date time , weatherregime table

Example PYTHON call: 

`Asyncron Link creation <https://github.com/bird-house/flyingpigeon/blob/master/notebooks/WPS_weatherregimes.ipynb>`


Example with Phoenix (GUI):
...........................

* login to a Phoenix GUI (e.g. `Compute provider DKRZ` <https://mouflon.dkrz.de>/_) 

|_ Menu option: Wizard
  |_ Choose a Favorite: No Favourite 
    |_ Choose a Web Processing Service: Flyingpigeon 
      |_ Choose WPS Process of Flyingpigeon: Weather Regimes
        |_ Literal inputs of Weather Regimes: given is a default, feel free to change ;-) 
          |_ Choose Input Parameter of Weather Regimes: netCDF
            |_ Choose Data Source : Earth System Grid (ESGF) 
            
The next window is the Data search interface to the available data of the ESGF archive. Weather regimes are computed based on sea surface pressure values. The appropriate variable for CMIP5 data are 'psl'. With other search options the dataselection shoule look like this example: 

.. image:: pics/esgf_search_psl.png

You can save your settings as favourite. And submit the job.  
Done!!!

You follow the log file of your in the monitor (klick on the job ID e.g. a4aa98de-ffde-11e5-b50a-bb0d01b14483). manual reload of your browser site is necessary.

.. image:: ../pics/monitor_log_weatherregimes.png
           