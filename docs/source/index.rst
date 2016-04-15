.. Flyingpigeon documentation master file, created by
   sphinx-quickstart on Fri Feb 27 13:30:34 2015.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _introduction:

Introduction
============

Flying Pigeon (the bird)
  *The pigeon finds its way home over extremely long distances. [..].* (` Wikipedia <https://en.wikipedia.org/wiki/Pigeon_flying>`_).

Flying Pigeon (the bike)
  *Flying Pigeon is a Chinese bicycle company [..]. The Flying Pigeon is the most popular vehicle ever.* (`Wikipedia <https://en.wikipedia.org/wiki/Flying_Pigeon>`_)

Flyingpigeon is a Python package with a collection of algorithms for the climate community available as a Web Processing Service (WPS) as a compartment of the `birdhouse <http://birdhouse.readthedocs.org/en/latest/index.html>`_ 

Contents:

.. toctree::
   :maxdepth: 1

   installation
   configuration
   descriptions/index

   
Examples: 
=========

Processing files of the local (compute providers) disc system with solr - search 
................................................................................

Note for administrators: 
To index files run: 
birdfeeder from-walker --start-dir /path/to/local/folder/


* login to a Phoenix GUI (e.g. `Compute provider DKRZ` <https://mouflon.dkrz.de>/_) 
 
|_ Menu option: Wizard
  |_ Choose a Favorite: No Favourite 
    |_ Choose a Web Processing Service: Flyingpigeon 
      |_ Choose WPS Process of Flyingpigeon: Weather Regimes
        |_ Literal inputs of Weather Regimes: given is a default, feel free to change ;-) 
          |_ Choose Input Parameter of Weather Regimes: netCDF
            |_ Choose Data Source : Birdhouse Solr Search
            
The open window is the Data search interface to the available local data of the compute provider. Weather regimes are computed based on sea surface pressure values. Appropriable variables are 'psl' or 'slp'. In case of this example, search for 'psl': 
.. image:: ../pics/solr_search_psl.png


You can save your settings as favourite. And submit the job.  
Done!!!

You follow the log file of your in the monitor (klick on the job ID e.g. a4aa98de-ffde-11e5-b50a-bb0d01b14483). manual reload of your browser site is necessary.

.. image:: ../pics/monitor_log_weatherregimes.png
 





