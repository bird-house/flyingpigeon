Example with Phoenix (GUI):
...........................


1. login to a Phoenix GUI (e.g. `Compute provider mouflon at DKRZ <https://mouflon.dkrz.de/>`_) 
2. Menu option: Wizard
3. Choose a Favorite: No Favourite 
4. Choose a Web Processing Service: Flyingpigeon 
5. Choose WPS Process of Flyingpigeon: Species distribution model - Species distribution model (SDM) 
6. Literal inputs of Species distribution model :

.. image:: ../pics/sdm_literalinputs.png

7. Choose Input Parameter of Species distribution model: netCDF
8. Choose Data Source : Earth System Grid (ESGF)
            
The next window is the Data search interface to the available data of the ESGF archive. The input files are used to calculate the climate conditions. Dipending on the selection of indices appropriate variables has to be provided (an indice based on pricipitation needs 'pr' as input variable ). multiple selection can be done while pressing the Cntr button. With other search options the dataselection shoule look like this example: 

.. image:: ../pics/sdm_esgfsearch.png

10. You can optional check or uncheck  'Save as Favourite' with an appropriate name;  and submit the job.  
11. Done!!!

You can follow the log file of your process in the monitor (klick on the job ID e.g. a4aa98de-ffde-11e5-b50a-bb0d01b14483). Manual reload of your browser site is necessary.
