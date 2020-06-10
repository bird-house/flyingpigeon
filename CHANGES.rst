Changes
*******

1.6 (2020-06-10)
================
* Setup cruft for cookiecutter refresh
* remove dependency eggshell
* notebook test integration
* improved plot processes
* remove mosaic option for subset processes
* loop over multiple files of one dataset
* multiple outputs given as Metalink standard
* update pywps to 4.2.3

1.5.1 (2019-11-11)
==================
* Add Postgres library to docker image.
* Pin PyWPS 4.2.3.

1.5 (2019-10-01)
==================
* Update from cookiecutter template.
* Pin PyWPS 4.2.2.

1.4.2 (2019-09-18)
==================
* Fixed the logic of the nc resource handler in subset (#288).
* Various documentation fixes.

1.4.1 (2019-05-20)
==================

* Subset processes enabled (#274).
* Point-inspection process enabled (#271).
* Spatial-analog process enabled (#280).
* Fixed docs build on RTD (#279).

1.4.0 (2018-12-03)
==================

New FlyingPigeon without buildout deployment (#265).

1.3.0 (2018-12-03)
==================

Release with merged processes from PAVICS projects.

* Merged processes from Ouranos/PAVICS fork (#262).
* Multiple fixes.

**warning**:
This is not a functional release due to unresolved issues and dependency conflicts.
The release is kept as reference for the available functionality.

1.2.1 (2018-09-14)
==================

Bug-fix release:

* disabled many processes due to conda dependency conflicts (#261).
* simplified `buildout.cfg` (#245).
* tests for `subset_countries` added (#237).
* numerous others fixes.

1.2.0 (2018-04-04)
==================

Issues:

* Fixed abstract for CSV files output in pointinspection process: #216
* snappy installation is optional: #229
* Disabled sphinx buildout configuration: #227
* Fixed test failures: #210 and #224
* Fixed codacy report: #211
* Fixed readthedocs build: #207

1.1.0 (2017-12-22)
==================

* disabled analogs processes (using castf90) ... moved to black-swan.
* added new spatial analogs process.
* added initial version of satellite processes using scihub.coperniucs data.
* updated weatherregimes processes.

1.0.3 (2017-12-21)
==================

* fixed sphinx build.

1.0.2 (2017-12-20)
==================

* updated conda environment.
* fixed pytest configuration.
* updated travis link in Readme.

1.0.1 (2017-11-14)
==================

* fixed version number
* fixed changes formatting
* display version number in service title

1.0.0 (2017-11-01)
==================

* code adapted to pywps4
* ocgis v2 depoyed
* Tests for components
* `Version published in Computers & Geosciences <http://www.sciencedirect.com/science/article/pii/S0098300416302801>`_

Set of processes
################

Base processes:

* Fetch resources
* Fetch GBIF Species Coordination
* Subset Polygons
* Point Inspection
* Timeseries visualisation
* Climate Indices Calculation

Climate Impact:

* Species Distribution Model
* Segetal Flora Calculation

Extreme Weather Events Assessment:

* Analogs of Circulation for reanalyzes Datasets
* Analogs of Circulation for model data
* Analogs of Circulation Comparison between reanalyzes and climate model data
* Analogs output data visualisation
* Weather regime Determination for reanalyzes Datasets
* Weather regime Determination for model datasets
* Weather regime projections  (based on previous analyses)


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
