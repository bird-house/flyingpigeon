fAnalogues of circulation
------------------------

Analogues of circulation provide a versatile tool to investigate the relation between climate variables (such as temperature or precipitation) and large-scale atmospheric circulation patterns (SLP or Zg (h)). The deployed software in the analogues processes are the circulation analogue simulation tool (`CASTf90 <https://github.com/sradanov/castf90>`_). 

Method: 
........

Here should come a bit of a method description. 


Process identifiers:
.....................

  * **Analogues_reanalyses:** 
      Includes a preselection of reanalyses pressure data (sea surface or geopotential height).
      This process fetches the data based on the selected reanalyses dataset and variable, so no input file has to be provided by the user. 
  * **Analogues_model:** 
      Designed to analyse a climate model data set. The location of the input files has to be provided by the user (using the ESGF search interface is possible). 
  * **Analogues_compare:** 
      To search analogue days in a climate model dataset for given days in a reanalyses dataset.
  * **Analogues_viewer:** 
      Analogues data output as a text file can be visualized in an html page with interactive graphics.  
 

Input parameter: 
................

**Data experiment:** 
 Input Data

**Region** 
 Region to be analysed

**Start data of analysis period** 
 Starting date of the period to be analysed

**End date of analysis period** 
 End date of the period to be analyzed

**Start reference period** 
 Start date of period where analogue days will be picked

**End reference period** 
 End date of period where analogue days will be picked

**Nr of analogues** 
 Number of analogues to be detected and written out as results. They are ordered with increasing distance from the original pressure pattern.

**Seasonal window** 
 Number of days defining the calendar proximity, i.e., the time window around the given date in all years except the year of the given day.

**normalisation** 
 Pressure values can be normalized (subtraction of average value over the whole period). Possible options are:

 * None 
    No normalisation 
 * based 
    normalisation based on reference period
 * sim 
    normalisation based on analysis period
 * own 
    normalisation of reference and analysis data values by their own average
   
**Distance** 
 Methods to calculate the distance:

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
  Values of the analysis period can be smoothed by averaging with the values of the following days given in 'time window'  
  (default = 1).


Outputs: 
........

**Config File**
  configuration file used by the CASTf90 software
  
**Analogs File**
  list of analogue days
  
**Target netCDF**
  pressure values as input for CASTf90 for analogue days to be picked
  
**prepared netCDF**
  pressure values as input for CASTf90 for analogue days to be searched
  
**html viewer**
  output of the analogues_viewer process for data visualisation

**modified analogues txt file**
  output analogue days formatted for the analogues_viewer process

Examples: 
.........

.. toctree::
   :maxdepth: 1
   
   /tutorials/analogues