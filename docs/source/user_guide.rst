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


Python syntax:
--------------

.. code:: ipython3

    """Python WPS execute"""

    from owslib.wps import WebProcessingService, monitorExecution
    from os import system


.. code:: ipython3

    wps = WebProcessingService(url="http://localhost:8093/wps", verbose=False)
    print("Service '{}' is running".format(wps.identification.title))


.. parsed-literal::

    Service 'Flyingpigeon' is running


.. code:: ipython3

    for process in wps.processes:
        print( '{} : \t {}'.format(process.identifier, process.abstract))


.. parsed-literal::

    subset : 	 Return the data for which grid cells intersect the selected polygon for each input dataset as well asthe time range selected.
    subset_bbox : 	 Return the data for which grid cells intersect the bounding box for each input dataset as well asthe time range selected.
    subset_continents : 	 Return the data whose grid cells intersect the selected continents for each input dataset.
    subset_countries : 	 Return the data whose grid cells intersect the selected countries for each input dataset.
    pointinspection : 	 Extract the timeseries at the given coordinates.
    subset_WFS : 	 Return the data for which grid cells intersect the selected polygon for each input dataset.
    plot_timeseries : 	 Outputs some timeseries of the file field means. Spaghetti and uncertainty plot


.. code:: ipython3

    # define some data urls


    url1 = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.2000.nc'
    url2 = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.2001.nc'
    url3 = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.2002.nc'
    url4 = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.2003.nc'

.. code:: ipython3

    execute = wps.execute(
        identifier="plot_timeseries", #indices_clipping",
        inputs=[
           ("resource",url1),
           ("resource",url2),
           ("resource",url3),
           ("resource",url4),
           # ("variable" , "slp"),
           ])

    monitorExecution(execute, sleepSecs=5)
    print(execute.getStatus())

    for o in execute.processOutputs:
        print(o.reference)


.. parsed-literal::

     owslib.wps.WPSException : {'code': 'NoApplicableCode', 'locator': 'None', 'text': 'Process failed, please check server error log'}
    ProcessFailed


.. code:: ipython3

    from flyingpigeon.nc_utils import get_coordinates
