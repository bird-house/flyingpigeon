.. _processes:

Processes in Flyingpigeon
*************************

Flyingpigeon provides processes for climate model data analytis, climate impact studies and extrem values investigatins. 


.. _analogs:

CASTf90 first downloads fields from NCEP reanalysis (sea level pressure, slp, as default) and then searches for a given simulation period the most similar cases within a given data base period according to a given distance measure. Finally it writes the N most similar days including the calculated distances for them to an output file


.. _extremvalues: 

Extremvalues
-------------

Calculation of retun time Values for 1D time series. 


.. _getEOBS_inCORDEXformat: 

get EOBS Data in CORDEX format
-------------------------------

converts EOBS data files into the CORDEX convetion. (variable names, attributes etc ... )

.. _indices:

ECA simple indices
-------------------

Indices based on one input variable. The indices are executed by an ocgis call of icclim

.. _segetalflora: 

Segetalflora
------------
Calculation of numbers of segetal flora species in Europe. 

.. _sdm: 

Species Distribution Model
--------------------------

Statistical approach to calculate the spatial favorability of climate sensitive species.

The appraoch is to be performed in two steps:

* Statistical training with species presents absense data and historical climate data
* future projection based on the statistical training


.. _subset: 

Subset
------

generates a polygon subset of input netCDF files 


Based on an ocgis call, several predfined polygons ( world counties ) can be used to generate an appropriate subset of input netCDF files. 
The option 'MOSAIK' as an checkbox allows you to decide in case of multiple polygon selection, if the polygons are stiched together to one polygon (e.g. shape of Germany and France as one polygon) or calculated as seperte output files. 

For optimisation of processing the subset, the appropriate shapefile are prepared with the following stepps: 


.. toctree::
   :maxdepth: 1

   shapefilepreparation




.. _vbd: 

Vector borne diseases
---------------------

The livecycle of disease transmitting vectors (mosquitos, ticks etc. ) are stronly depending on climatic conditions.

These models are integrated:

* Thommy Model
* Kamil Model

.. _visualisation: 

Visualisation
-------------

Line plots using `Bokeh <http://bokeh.pydata.org/en/latest/>`_ to compare variables of NetCDF files from CORDEX and CMIP5. [..]

