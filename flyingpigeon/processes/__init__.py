from .wps_say_hello import SayHello
from .wps_subset import SubsetProcess
# from .wps_subset_WFS import SubsetWFSProcess
# from .wps_subset_bbox import SubsetBboxProcess
# from .wps_subset_continents import ClipcontinentProcess
# from .wps_subset_countries import ClippingProcess
# from .wps_subset_regionseurope import ClipregionseuropeProcess
# from .wps_ocgis_func import OCGIS_INDEX_PROCESSES  # This stores all the processes defined in the module.


# from .wps_averager import AveragerProcess
# from .wps_averager_WFS import AveragerWFSProcess
# from .wps_averager_bbox import AveragerBboxProcess
# from .wps_indices_percentiledays import IndicespercentiledaysProcess
# from .wps_indices_single import IndicessingleProcess
# from .wps_kddm_bc import KDDM_BC_Process
# from .wps_landseamask import LandseamaskProcess
# from .wps_map_spatial_analog import MapSpatialAnalogProcess
# from .wps_ncmerge import NCMergeProcess
# from .wps_ouranos_pub_indicators import OuranosPublicIndicatorProcess
# from .wps_plot_timeseries import PlottimeseriesProcess
# from .wps_pointinspection import PointinspectionProcess
# from .wps_regrid import ESMFRegridProcess



processes = [
    SayHello(),
    SubsetProcess(),
    # SubsetWFSProcess(),
    # SubsetBboxProcess(),
    # AveragerProcess(),
    # AveragerWFSProcess(),
    # AveragerBboxProcess(),
    # OuranosPublicIndicatorProcess(),
    # NCMergeProcess(),

]# + [c() for c in OCGIS_INDEX_PROCESSES]
