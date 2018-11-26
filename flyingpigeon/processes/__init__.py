from .wps_EO_COPERNICUS_fetch import EO_COP_fetchProcess
from .wps_EO_COPERNICUS_search import EO_COP_searchProcess
# from .wps_EO_COPERNICUS_indices import EO_COP_indicesProcess
# from .wps_EO_COPERNICUS_rgb import EO_COP_rgbProcess
# from .wps_EO_fetch import FetcheodataProcess
# from .wps_EO_ndvi import NdviProcess
# from .wps_EO_merge import MergeProcess
# from .wps_climatefactsheet import FactsheetProcess
# from .wps_fetch import FetchProcess
# from .wps_indices_percentiledays import IndicespercentiledaysProcess
# from .wps_indices_single import IndicessingleProcess
# from .wps_landseamask import LandseamaskProcess
# from .wps_map_spatial_analog import MapSpatialAnalogProcess
from .wps_ocgis_func import OCGIS_INDEX_PROCESSES
# from .wps_robustness import RobustnessProcess
# from .wps_plot_timeseries import PlottimeseriesProcess
from .wps_pointinspection import PointinspectionProcess
# from .wps_regrid import ESMFRegridProcess
# from .wps_sdm_allinone import SDMallinoneProcess
# from .wps_sdm_csv import SDMcsvProcess
# from .wps_sdm_csvindices import SDMcsvindicesProcess
# from .wps_sdm_gbiffetch import GBIFfetchProcess
# from .wps_sdm_getindices import SDMgetindicesProcess
# from .wps_segetalflora import SegetalfloraProcess
# from .wps_spatial_analog import SpatialAnalogProcess
from .wps_subset_continents import ClipcontinentProcess
from .wps_subset_countries import ClippingProcess
# from .wps_subset_regionseurope import ClipregionseuropeProcess

processes = [
    ClippingProcess(),
    ClipcontinentProcess(),
    # ClipregionseuropeProcess(),
    PointinspectionProcess(),
    # LandseamaskProcess(),
    # FactsheetProcess(),
    # FetchProcess(),
    # IndicespercentiledaysProcess(),
    # IndicessingleProcess(),
    # GBIFfetchProcess(),
    # SDMgetindicesProcess(),
    # SDMcsvindicesProcess(),
    # SDMcsvProcess(),
    # SDMallinoneProcess(),
    # RobustnessProcess(),
    # PlottimeseriesProcess(),
    # SegetalfloraProcess(),
    # SpatialAnalogProcess(),
    # MapSpatialAnalogProcess(),
    # FetcheodataProcess(),
    EO_COP_searchProcess(),
    EO_COP_fetchProcess(),
    # ESMFRegridProcess(),
    # EO_COP_rgbProcess(),
    # EO_COP_indicesProcess(),
    # MergeProcess(),
    # NdviProcess(),
] + [c() for c in OCGIS_INDEX_PROCESSES]

"""
pywps3 processes:

# climate for impact processes
"wps_c4i_simple_indice",

# TODO: c4i processes with multiple input sources
# "wps_c4i_multivar_indice",
# "wps_c4i_percentile_indice",
# "wps_c4i_compound_indice",
"""
