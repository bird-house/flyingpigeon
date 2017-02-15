Tutorial: Ensemble Robustness
.............................


Processing files of the local (compute providers) disc system with solr - search :

Note for administrators:
To index files run:
birdfeeder from-walker --start-dir /path/to/local/folder/

* Login to a Phoenix GUI (e.g. `Compute provider DKRZ <https://mouflon.dkrz.de/>`_)
  (`Detailed descritions for login options <http://pyramid-phoenix.readthedocs.io/en/latest/user_guide.html#login>`_ )

- Menu option: Wizard
- Choose a Favorite: No Favorite
- Choose a Web Processing Service: Flyingpigeon
- Choose WPS Process of Flyingpigeon: Weather Regimes
- Literal inputs of Weather Regimes: defaults given
- Choose Input Parameter of Weather Regimes: netCDF
- Choose Data Source : Birdhouse Solr Search

The open window is the data search interface to the available local data of the compute provider. Weather regimes are computed based on sea surface pressure values. Appropriate variables are 'psl' or 'slp'. In this example, search for 'psl':

.. image:: pics/solr_search_psl.png

You can save your settings as a favorite. And submit the job.
Done!!!

You can follow the log file in the monitor (click on the job ID e.g. a4aa98de-ffde-11e5-b50a-bb0d01b14483). Manual reload of your browser site is necessary.

.. image:: pics/monitor_log_weatherregimes.png
