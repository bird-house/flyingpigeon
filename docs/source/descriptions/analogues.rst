Analogues of circulation
------------------------

Analogues of circulation provide a versatile tool to investigate the relation between climate variables (such as temperature or precipitation) and large-scale atmospheric circulation patterns (SLP or Zg (h)). The depoyed software in the analogues processes are the circulation analogue simulation tool (`CASTf90 <https://github.com/sradanov/castf90>`_). 

Methode: 
........

Here should come a bit of a method description. 


Processes:
..........

  * **Analogues_reanalyses:** 
      Includes a preselection of reanalyses pressure data ( sea surface or geopotential hight)
      This process fetches the data based on the selected reanalyses dataset and variable, so no input files has to be provided by the user. 
  * **Analogues_model:** 
      Is designed to analyse a climte model data set. The location of the input files has to be provided by the user (using the ESGF serach interface is possible). 
  * **Analogues_compare:** 
      To search analogue days in a climte model dataset for given days in a reanalyses dataset.
  * **Analogues_viewer:** 
      Analougs data output as an txt file can be visualized in a html page with interactive graphics.  
 

Input parameter: 
................

**Data experiment:** 
 Input Data

**Region** 
 Region to be analysed

**Start data of anayse period** 
 Starting date of the period to be analysed

**End date of analyse period** 
 End date of the period to be analyzed

**Start reference period** 
 Start day of period where analogues days will be picked

**End reference period** 
 End day of period where analogues days will be picked

**Nr of analogues** 
 Number of analogues to be detected and written out as results. They are ordered with increasing distance to the original pressure pattern.

**Seasonal window** 
 Number of day for a calendar proximity. 
 It defines the time window around the given date in all years except the year of the given day.

**normalisation** 
 Pressure values can be normalized (substraction of average value over the whole period). possible options are:

 * None 
    No normalisation 
 * based 
    normalisation based on reference period
 * sim 
    normalisation based on analyse period
 * own 
    normalisation of reference and analyse data values by there own average
   
**Distance** 
 Method to calculate the distance

 * euclidean  
 * mahalanobis 
 * cosine 
 * of
 
**output file format**
  * netCDF 
     output values will be provided as a netCDF file 
  * ascii  
     output values will be provided as an ascii file
  
**Time window** 
  Values of analyse period can be smoothed by averaging with the values of the following days. 'time window' is giving the number of days averaging.  
  (default=1)


Outputs: 
........

**Config File**
  Configuration file use by the CASTf90 software
  
**Analogs File**
  list of analogues days
  
**Target netCDF**
  pressure values as input for CASTf90 for analogues days to be picked
  
**prepared netCDF**
  pressure values as input for CASTf90 for analogues days to be searched format
  
**html viewer**
  output of the analogues_viewer process for data visualisation

**modified analogues txt file**
  output of the analogues_viewer process. input data for viewer generation.

Examples: 
.........

.. toctree::
   :maxdepth: 1
   
   /tutorials/analogues