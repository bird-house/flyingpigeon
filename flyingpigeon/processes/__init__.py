# from .wps_say_hello import SayHello
from .wps_subset_polygon import SubsetWFSPolygonProcess
from .wps_subset_bbox import SubsetBboxProcess
from .wps_subset_continents import SubsetcontinentProcess
from .wps_subset_countries import SubsetcountryProcess
from .wps_pointinspection import PointinspectionProcess
from .wps_plot_timeseries import PlottimeseriesProcess

processes = [
    # SayHello(),
    SubsetWFSPolygonProcess(),
    SubsetBboxProcess(),
    SubsetcontinentProcess(),
    SubsetcountryProcess(),
    PointinspectionProcess(),
    PlottimeseriesProcess(),
]
