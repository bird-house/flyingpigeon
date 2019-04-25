Spatial Analogues
-----------------

Spatial analogues are maps showing which areas have a climate today that is
*analogous* to the future climate of a given place. This type of map can be
useful for climate adaptation to see which places are coping today with specific climate
conditions. For example, officials from a city located in a temperate region could learn from
the experience of another city where heatwaves are a common occurrence and
intervention plans have been improved over time.

Spatial analogues are estimated by comparing the distribution of climate indices
computed at the target location over the future period with the distribution of
the same climate indices computed over a reference period for multiple candidate regions.
A number of methodological choices thus enter the computation:

  * the climate indices,
  * the metric measuring the difference between both distributions,
  * the reference data from which to compute the indices,
  * the future climate scenario to compute the target indices.

The climate indices chosen to compute the spatial analogs are usually annual values
of indices relevant to the intended audience of these maps. For example, if the maps are
made for the grape industry, then the climate indices used could
be the length of the frost-free season, growing degree-days, annual winter minimum
temperature andand annual number of very cold days [Roy2017]_.

The :class:`flyingpigeon.processes.SpatialAnalogProcess` offers six
distance metrics: standard euclidean distance, nearest neighbor,
Zech-Aslan energy distance, Kolmogorov-Smirnov statistic,Friedman-Rafsky runs
statistics and the Kullback-Leibler divergence. A description and reference for
each distance metric is given in :mod:`flyingpigeon.dissimilarity` and based
on [Grenier2013]_.

The reference data set should cover the target site in order to perform
validation tests, and a large area around it. Global or continental scale datasets
are generally used, but the spatial resolution should be high enough for users to be
able to *recognize* climate features they are familiar with.

Different future climate scenarios from climate models can be used to compute the
target distribution over the future period. Usually the raw model outputs are
bias-corrected with the observation dataset. This is done to avoid discrepancies
that would be introduced by systematic model errors. One way to validate the results
is to compute the spatial analog using the simulation over the historical period. The
best analog region should thus cover the target site.

The WPS process automatically extracts the target series from a netCDF file using
geographical coordinates and the names of the climate indices (the name of the
climate indices should be the same for both netCDF files). It also allows users
to specify the period over which the distributions should be compared, for both
the target and candidate datasets.

An accompanying process :class:`flyingpigeon.processes.MapSpatialAnalogProcess`
can then be called to create a graphic displaying the dissimilarity value.
An example of such graphic is shown below, with the target location indicated
by a white marker.

.. figure:: ./pics/spatial_analog_example.*
   :alt: Example of spatial analog graphic.

   A map of the dissimilarity metric computed from mean annual precipitation and
   temperature values in Montreal over the period 1970-1990.

References
~~~~~~~~~~

.. [Roy2017] Roy, P., Grenier, P., Barriault, E. et al. Climatic Change (2017) 143: 43. `<doi:10.1007/s10584-017-1960-x>`_
.. [Grenier2013]  Grenier, P., A.-C. Parent, D. Huard, F. Anctil, and D. Chaumont, 2013: An assessment of six dissimilarity metrics for climate analogs. J. Appl. Meteor. Climatol., 52, 733–752, `<doi:10.1175/JAMC-D-12-0170.1>`_
