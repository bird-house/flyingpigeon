.. _processes_description:

Process Descriptions
====================

.. contents::
    :local:
    :depth: 1

Following is a detailed description of processes in Flyingpigeon. As Flyingpigeon is currently dedicated to be the Testbed for Process development, existing processes might migrate to other birds (WPS services in birdhouse) in upcoming version.

Migrated Processes:
-------------------
Here comes a list of Processes already beeing migrated and where you can find them:

+-------------------------------+---------------------+----------------------------+
| Process group                 | migrated to:        | brief description          |
+===============================+=====================+============================+
| Analogs of atmospheric flow   |  BLACKSWAN          | Extreme Weather Analytics  |
+-------------------------------+---------------------+----------------------------+
| Weather Regimes               |  BLACKSWAN          | Extreme Weather Analytics  |
+-------------------------------+---------------------+----------------------------+
| Climate Indices               |  FINCH              | Climate Monitoring         |
+-------------------------------+---------------------+----------------------------+
| Species Distribution Models   |  Disabled           | Climate Impact             |
+-------------------------------+---------------------+----------------------------+
| Segetal Flora                 |  Disabled           | Climate Impact             |
+-------------------------------+---------------------+----------------------------+


Subset Processes
----------------

Generates a polygon subset of input netCDF files. Based on an ocgis call, several pre-defined polygons (e.g. world countries) can be used to generate an appropriate subset of input netCDF files. Spatial subsetting are methods of deriving a new set of data from another set of data using interpolation techniques to generate different spatial or temporal resolutions.

The User can make the principal decisions to define the area or areas to be subsetted and in case of multiple areas wether they should stay in separate files or be merged to an unit.

Point-inspection can be seen as a special form of subsetting. On defined coordinates a 1D time-series will be generated for each coordinate point.

.. NOTE:: See the :ref:`subset_processes_api` for detailed options and data-IO.

In case of polygon subsetting used to subset the shape of e.g. countries or continents, **flyingpigeon contains prepared shapefiles**. To increase the performance the shapefiles had been optimized with the following steps:

.. toctree::
   :maxdepth: 1

   preparation

Data Visualisation:
-------------------

They are various ways of data visualization. In flyingpigeon are realized the basic ones of creating an ordinary graphic file. It helps to have a quick understanding of your data.

Time series visualization of netCDF files. Creates a spaghetti plot and an uncertainty plot.

Plots are generated based on matplotlib. Appropriate functions are located in eggshell.

.. NOTE:: See the :ref:`plot_processes_api` for detailed options and data-IO.

.. TODO:: include an example
