GUI for Weatherregimes
......................

Weatherregimes comparison NCEP to CMIP5

- login to a Phoenix GUI (e.g. `Compute provider mouflon at DKRZ <https://mouflon.dkrz.de/>`_) 

- Menu option: Wizard
- Choose a Favorite: No Favourite 
- Choose a Web Processing Service: Flyingpigeon 
- Choose WPS Process of Flyingpigeon: Weather Regimes
- Literal inputs of Weather Regimes: given is a default, feel free to change ;-) 
- Choose Input Parameter of Weather Regimes: netCDF
- Choose Data Source : Earth System Grid (ESGF) 
            
The next window is the Data search interface to the available data of the ESGF archive. Weather regimes are computed based on sea surface pressure values. The appropriate variable for CMIP5 data are 'psl'. With other search options the dataselection shoule look like this example: 

.. image:: ../pics/esgf_search_psl.png

You can save your settings as favourite. And submit the job.  
Done!!!

You follow the log file of your in the monitor (click on the job ID e.g. a4aa98de-ffde-11e5-b50a-bb0d01b14483). manual reload of your browser site is necessary.

.. image:: ../pics/monitor_log_weatherregimes.png

