.. tutorial

Tutorials
=========


Subset Processes of Flyingpigeon
--------------------------------


The WPS flyingpigeon provides several processes to perform spatial
subsetts of netCDF files. This are:

-  **bounding box:** reduces the input netCDF files to a given latitude
   longitude bounding box.

-  **country or continent subset:** to reduce netCDF files to only the
   intersection of selected polygons. Countries and Continents are
   predifined.

-  **WFS subset:** reduce netCDF files to only the intersection of given
   polygons available on a given WFS server.

.. code:: ipython3

    # import the WPS client and connet to the server
    from birdy import WPSClient
    import birdy

    fp_server = 'http://localhost:8093/wps'   # finch

    # simple connection (not recommended for larger processing)
    fp = WPSClient(fp_server)

    # asyncron connection with progess status requests
    fp_i = WPSClient(url=fp_server, progress=True)

Explore the available processes:

.. code:: ipython3

    # fp? for general exploration on processes provided by flyingpigeon
    fp?

    # of check out a process in detail:
    help(fp.subset_continents) # or type: fp.subset_countries?


.. parsed-literal::

    Help on method subset_continents in module birdy.client.base:

    subset_continents(resource=None, region='Africa', mosaic=False) method of birdy.client.base.WPSClient instance
        Return the data whose grid cells intersect the selected continents for each input dataset.

        Parameters
        ----------
        region : {'Africa', 'Asia', 'Australia', 'North America', 'Oceania', 'South America', 'Antarctica', 'Europe'}string
            Continent name.
        mosaic : boolean
            If True, selected regions will be merged into a single geometry.
        resource : ComplexData:mimetype:`application/x-netcdf`, :mimetype:`application/x-tar`, :mimetype:`application/zip`
            NetCDF Files or archive (tar/zip) containing netCDF files.

        Returns
        -------
        output : ComplexData:mimetype:`application/x-netcdf`
            NetCDF output for first resource file.
        metalink : ComplexData:mimetype:`application/metalink+xml; version=4.0`
            Metalink file with links to all NetCDF outputs.



.. code:: ipython3

    # point out some input files:

    url1 = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.2000.nc'
    url2 = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.2001.nc'
    url3 = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.2002.nc'
    url4 = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.2003.nc'

Call a continent subset process
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

All netCDF input files will be subsetted seperatly and depending on
mosic=True or Fales the selected polygons are given as seperated files
or one output file per input file including one intersection of all
selected polygons.

**subset_countries** is working in the same principe

.. code:: ipython3

    # run the process
    out = fp_i.subset_continents(resource=[url1, url2, url3, url4], region=['Europe', 'Africa'], mosaic=True)

    # You need to wait until the processing is done!



.. parsed-literal::

    HBox(children=(IntProgress(value=0, bar_style='info', description='Processing:'), Button(button_style='danger'…


There are two outputs: \* a netCDF file to have a quick test to check if
the process went according to the users needs \* a metalink file with
the list of all output files

.. code:: ipython3

    # check the output files:
    out.get()




.. parsed-literal::

    subset_continentsResponse(
        output='http://127.0.0.1:8093/outputs/f232a4ba-67a0-11ea-a160-9cb6d08a53e7/slp.2000_EuropeAfrica.nc',
        metalink='http://127.0.0.1:8093/outputs/f232a4ba-67a0-11ea-a160-9cb6d08a53e7/input.meta4'
    )



.. code:: ipython3

    # plot the test file with the flyingpigeon plot function
    out = fp_i.plot_map_timemean(resource=out.get()[0])



.. parsed-literal::

    HBox(children=(IntProgress(value=0, bar_style='info', description='Processing:'), Button(button_style='danger'…


.. code:: ipython3

    # the plot process returnes one graphic file
    out.get()




.. parsed-literal::

    plot_map_timemeanResponse(
        plotout_map='http://127.0.0.1:8093/outputs/bdbf0876-67a1-11ea-9e91-9cb6d08a53e7/tmp_5ahujnj.png'
    )



.. code:: ipython3

    from IPython.display import Image
    from IPython.core.display import HTML
    Image(url= out.get()[0], width=400)




.. raw:: html

    <img src="http://127.0.0.1:8093/outputs/bdbf0876-67a1-11ea-9e91-9cb6d08a53e7/tmp_5ahujnj.png" width="400"/>
