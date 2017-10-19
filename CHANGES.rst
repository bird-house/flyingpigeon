Releasenotes
************

1.0.0 (2017-10-30)
==================

* complet code adapted to pywps4
* ocgis v2 depoyed
* Tests for components
* `Version published in Computer & Geosciences <http://www.sciencedirect.com/science/article/pii/S0098300416302801>`_
* Set of processes:
|  * Base processes:
|    Fetch resources
|    Fetch GBIF Species Coordination
|    Subset Polygons
|    Point Inspection
|    Timeseries visualisation
|    Climate Indices Calculation (base process??)
  * Climate Impact
|    Species Distribution Model (à can be in more detail depending on the fixes the next days)
|    Segetal Flora Calculation
  * Extreme Weather Events Assessment
|    Analogs of Circulation for reanalyzes Datasets
|    Analogs of Circulation Comparison between reanalyzes and climate model data (à If fixed until 30.Okt )
|    Analogs output data visualisation
|    Weather regime Determination for reanalyzes Datasets
|    Weather regime Determination for model datasets
|    Weather regime projections  (based on previous analyses)


0.11.0 (2017-07-11)
===================

converted processes to pywps-4 from next:

* subsetting countries, continents and european regions
* climate indices (daily percentiles, single variable)
* species distribution model
* land-sea mask
* point inspection
* fetch resources

0.10.1 (2017-07-11)
===================

* disabled bbox parameter ... needs to be fixed in OWSLib.
* updated titles of analogs processes.
* updated version in docs.
* disabled wps_gbiffetch test ... was stalled.

0.10.0 (2017-07-10)
===================

* Translate code pywps4 conform
* Climate indices dailypercentile
* Climate Fact sheet Generator
* R plot for SDM response cuvres running under CentOS
* Species distribution model Processes modularized in five processes
* Direction switch for analogs comparison process

0.9.1 (2016-11-16)
==================

* modularisation of segetalflora process
* docker update

0.9.0 (2016-09-08)
==================

* Subset points
* Subset European regions
* Subset world countries
* Subset continents
* Analogues for reanalyses datasets
* Analogues for model datasets
* Analogues for comparison model to reanalyses datasets
* Species Distribution Model based on GBIF CSV file
* Species Distribution Model with GBIF search included
* Weather regimes for reanalyses datasets
* Weather regimes for model datasets
* Weather regimes for model datasets with centroids trained on reanalyses datasets
* Segetalflora
* Initial spatial analogues process
* Climate indices (simple)
* Climate indices (percentile-based)
* Download resources
* Initial ensembles robustness
* Plots for time series

0.2.0 (2016-07-15)
==================

* analogs detection and viewer.
* timeseries plot.
* indices calculation with icclim.
* subsetting for countries and regions.
* weather regimes.
* SDM: species distribution model for tree species based on GBIF.
* species biodiversity of segetal flora.

0.1.0 (2014-09-04)
==================

Paris Release

* moved code to github.
* Initial Release.
