.. _user_guide:

User Guide
==========

.. contents::
    :local:
    :depth: 2

placeholder for some Tutorials how to use the processes of Flyingpigeon.


command line
-------------

with birdy
(reference to birdy)


.. toctree::
   :maxdepth: 2

   flyingpigeon_tutorial


Phoenix gui
------------

Screenshot with Phoenix


Process Descriptions
====================

Following is a detailed description of processes in Flyingpigeon

Subset Processes
----------------

Generates a polygon subset of input netCDF files. Based on an ocgis call, several pre-defined polygons (e.g. world countries) can be used to generate an appropriate subset of input netCDF files.

Method:
.......

Integrated ocgis performs the sub-setting.

Process identifiers:
...................

  * **subset_continents**
      subsets continents
  * **subset_countries**
      subsets countries
  * **point-inspection**
      extracts time-series for given coordinate subset_points

Input parameter:
................

**Polygons**
  Abbreviation of the appropriate polygon.

**Mosaic**
  The option 'MOSAIC' as a checkbox allows you to decide, in the case of multiple polygon selection, if the polygons should be stitched together into one polygon (e.g. shape of Germany and France as one polygon) or calculated as separate output files.


Shapefile optimization:
.......................

For optimization of the subset process, the appropriate shapefiles are prepared as follows:

.. toctree::
   :maxdepth: 1

   preparation


Visualization:
---------------

Time series visualization of netCDF files.
Creates a spaghetti plot and an uncertainty plot.
