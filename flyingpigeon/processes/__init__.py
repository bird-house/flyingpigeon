from .wps_pointinspection import PointinspectionProcess
from .wps_subset_bbox import SubsetBboxProcess
from .wps_subset_polygon import SubsetpolygonProcess
from .wps_subset_continents import SubsetcontinentProcess
from .wps_subset_countries import SubsetcountryProcess
from .wps_subset_WFS import SubsetWFSProcess
from .wps_spatial_analog import SpatialAnalogProcess
from .wps_plot_spatial_analog import PlotSpatialAnalogProcess
from .wps_plot_timeseries import PlottimeseriesProcess


processes = [
    SubsetBboxProcess(),
    SubsetpolygonProcess(),
    SubsetcontinentProcess(),
    SubsetcountryProcess(),
    PointinspectionProcess(),
    SubsetWFSProcess(),
    SpatialAnalogProcess(),
    PlottimeseriesProcess(),
    PlotSpatialAnalogProcess()
]
