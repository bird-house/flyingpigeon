
# from .wps_say_hello import SayHello
from .wps_subset_wfs_polygon import SubsetWFSPolygonProcess
from .wps_subset_bbox import SubsetBboxProcess
from .wps_subset_continents import SubsetcontinentProcess
from .wps_subset_countries import SubsetcountryProcess
from .wps_pointinspection import PointinspectionProcess
from .wps_spatial_analog import SpatialAnalogProcess
from .wps_robustness_statistic import RobustnesstatisticProcess
from .wps_climatechange_signal import ClimatechangesignalProcess
from .wps_plot_spatial_analog import PlotSpatialAnalogProcess
# from .wps_plot_timeseries import PlottimeseriesProcess
from .wps_plot_spaghetti import PlotspaghettiProcess
from .wps_plot_uncertainty import PlotuncertaintyProcess
from .wps_plot_uncertaintyRcp import PlotuncertaintyRcpProcess
from .wps_plot_maptimemean import PlotmaptimemeanProcess


processes = [
    # SayHello(),
    SubsetWFSPolygonProcess(),
    SubsetBboxProcess(),
    SubsetcontinentProcess(),
    SubsetcountryProcess(),
    PointinspectionProcess(),
    RobustnesstatisticProcess(),
    ClimatechangesignalProcess(),
    SpatialAnalogProcess(),
    PlotSpatialAnalogProcess(),
    PlotspaghettiProcess(),
    PlotuncertaintyProcess(),
    PlotuncertaintyRcpProcess(),
    PlotmaptimemeanProcess(),
    # PlottimeseriesProcess(),
]
