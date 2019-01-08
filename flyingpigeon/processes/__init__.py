from .wps_subset import SubsetProcess
from .wps_subset_bbox import SubsetBboxProcess
from .wps_subset_continents import SubsetcontinentProcess
from .wps_subset_countries import SubsetcountryProcess
from .wps_pointinspection import PointinspectionProcess
from .wps_subset_WFS import SubsetWFSProcess
from .wps_plot_timeseries import PlottimeseriesProcess
from .wps_spatial_analog import SpatialAnalogProcess
from .wps_map_spatial_analog import MapSpatialAnalogProcess

processes = [
    SubsetProcess(),
    SubsetBboxProcess(),
    SubsetcontinentProcess(),
    SubsetcountryProcess(),
    PointinspectionProcess(),
    SubsetWFSProcess(),
    PlottimeseriesProcess(),
    SpatialAnalogProcess(),
    MapSpatialAnalogProcess()
]
