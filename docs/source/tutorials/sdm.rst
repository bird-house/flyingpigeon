GBIF Data fetch
...............

1. Login to a Phoenix GUI (e.g. `Compute provider mouflon at DKRZ <https://mouflon.dkrz.de/>`_)
2. Menu option: Processes
3. Choose a Web Processing Service: Flyingpigeon
4. Choose WPS Process of Flyingpigeon: SDM - Get GBIF only
5. Set the taxonomic name of the tree species of your choice and optionally reduce the region to fetch data (recommended! to reduce the process time)



Calculate climate indices
.........................

1. Login to a Phoenix GUI (e.g. `Compute provider mouflon at DKRZ <https://mouflon.dkrz.de/>`_)
2. Menu option: Wizard
3. Choose a Favorite: No Favorite
4. Choose WPS Process of Flyingpigeon: SDM - Get indices only
5. Choose the climate indices to be taken as input for the SDM experiment (optionally change the archive format)
6. Choose Input Parameter of species distribution model: netCDF
7. Choose Data Source: Earth System Grid Federation (ESGF)

The next window displays the data search interface to the data available in the ESGF archive. The input files are used to calculate the climate conditions.
Depending on the selection of indices, appropriate variables have to be provided (e.g. an index based on precipitation needs 'pr' as an input variable ).
Multiple selection can be done while pressing the Ctrl button. With other search options, the data selection looks like this example:

.. image:: ../pics/sdm_esgfsearch.png

8. Follow the 'next' buttons and submit the job with 'Done'.


Running an SDM experiment
........................

With the outputs of the first two examples (fetching GBIF data and climate indices calculation), it is possible to run an SDM experiment.

1. Login to a Phoenix GUI (e.g. `Compute provider mouflon at DKRZ <https://mouflon.dkrz.de/>`_)
2. Menu option: Processes
3. Choose a Web Processing Service: Flyingpigeon
4. Choose WPS Process of Flyingpigeon: SDM - csvindices
5. Resources is an archive file containing the precalculated climate indices, and GBIF data is the csv table output by the GBIF data fetch process.
   You can use the basket buttons to put/take the appropriate urls for the data an easy way.
6. Execute to submit the job and follow the process in the monitor.
