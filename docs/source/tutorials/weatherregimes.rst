GUI for Weather regimes
......................

Weather regimes comparison between NCEP and CMIP5

- Login to a Phoenix GUI (e.g. `Compute provider mouflon at DKRZ <https://mouflon.dkrz.de/>`_) 

- Menu option: Wizard
- Choose a Favorite: No Favorite 
- Choose a Web Processing Service: Flyingpigeon 
- Choose WPS Process of Flyingpigeon: Weather Regimes
- Literal inputs of Weather Regimes: defaults given, feel free to change ;-) 
- Choose Input Parameter of Weather Regimes: netCDF
- Choose Data Source: Earth System Grid Federation (ESGF) 
            
The next window is the data search interface to the data available in the ESGF archive. Weather regimes are computed based on sea surface pressure values. The appropriate variable for CMIP5 data is 'psl'. With other search options, the datas election look like this example: 

.. image:: ../pics/esgf_search_psl.png

You can save your settings as a favorite. And submit the job.  
Done!!!

You can follow the log file in the monitor (click on the job ID e.g. a4aa98de-ffde-11e5-b50a-bb0d01b14483). Manual reload of your browser site is necessary.

.. image:: ../pics/monitor_log_weatherregimes.png

