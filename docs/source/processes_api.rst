.. _processes_api:

Processes API
=============

.. contents::
    :local:
    :depth: 1


.. _subset_processes_api:

Subset Processes API
--------------------

Sub-setting is performed with ocgis. Appropriate functions are located in eggshell.

|
|

.. autoprocess:: flyingpigeon.processes.wps_pointinspection.PointinspectionProcess
   :docstring:
   :skiplines: 1

|
|

.. autoprocess:: flyingpigeon.processes.wps_subset_wfs_polygon.SubsetWFSPolygonProcess
   :docstring:
   :skiplines: 1

|
|

.. autoprocess:: flyingpigeon.processes.wps_subset_continents.SubsetcontinentProcess
   :docstring:
   :skiplines: 1

|
|

.. autoprocess:: flyingpigeon.processes.wps_subset_countries.SubsetcountryProcess
   :docstring:
   :skiplines: 1

|
|

.. autoprocess:: flyingpigeon.processes.wps_subset_bbox.SubsetBboxProcess
   :docstring:
   :skiplines: 1

|
|

.. _plot_processes_api:

Plot Timeseries API
-------------------

.. autoprocess:: flyingpigeon.processes.wps_plot_timeseries.PlottimeseriesProcess
  :docstring:
  :skiplines: 1

|
|

.. _analog_processes_api:

Spatial Analogs API
-------------------

.. autoprocess:: flyingpigeon.processes.wps_spatial_analog.SpatialAnalogProcess
  :docstring:
  :skiplines: 1

.. autoprocess:: flyingpigeon.processes.wps_plot_spatial_analog.PlotSpatialAnalogProcess
  :docstring:
  :skiplines: 1
