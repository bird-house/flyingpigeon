from .wps_subset_countries import ClippingProcess
from .wps_subset_continents import ClipcontinentProcess
from .wps_subset_regionseurope import ClipregionseuropeProcess
from .wps_pointinspection import PointinspectionProcess
# from .wps_landseamask import LandseamaskProcess
# from .wps_climatefactsheet import FactsheetProcess
from .wps_fetch import FetchProcess
from .wps_indices_percentiledays import IndicespercentiledaysProcess
from .wps_indices_single import IndicessingleProcess
from .wps_sdm_gbiffetch import GBIFfetchProcess
from .wps_sdm_getindices import SDMgetindicesProcess
from .wps_sdm_csv import SDMcsvProcess
from .wps_sdm_csvindices import SDMcsvindicesProcess
from .wps_sdm_allinone import SDMallinoneProcess
from .wps_weatherregimes_reanalyse import WeatherregimesreanalyseProcess
from .wps_weatherregimes_projection import WeatherregimesprojectionProcess
from .wps_weatherregimes_model import WeatherregimesmodelProcess
from .wps_analogs_reanalyse import AnalogsreanalyseProcess
from .wps_analogs_model import AnalogsmodelProcess
from .wps_analogs_compare import AnalogscompareProcess
from .wps_analogs_viewer import AnalogsviewerProcess
# from .wps_robustness import RobustnessProcess
from .wps_plot_timeseries import PlottimeseriesProcess
from .wps_segetalflora import SegetalfloraProcess
# from .wps_spatial_analog import SpatialAnalogProcess
# from .wps_map_spatial_analog import MapSpatialAnalogProcess#  LandseamaskProcess(),

processes = [
    ClippingProcess(),
    ClipcontinentProcess(),
    ClipregionseuropeProcess(),
    PointinspectionProcess(),
    LandseamaskProcess(),
    #    FactsheetProcess(),
    FetchProcess(),
    IndicespercentiledaysProcess(),
    IndicessingleProcess(),
    GBIFfetchProcess(),
    SDMgetindicesProcess(),
    #   SDMcsvProcess(),
    #   SDMcsvindicesProcess(),
    #   SSDMallinoneProcess(),
    WeatherregimesreanalyseProcess(),
    WeatherregimesprojectionProcess(),
    WeatherregimesmodelProcess(),
    AnalogsreanalyseProcess(),
    AnalogsmodelProcess(),
    #    AnalogscompareProcess(),
    AnalogsviewerProcess(),
    #   RobustnessProcess(),
    PlottimeseriesProcess(),
    SegetalfloraProcess(),
    #   SpatialAnalogProcess(),
    #   MapSpatialAnalogProcess(),
]

"""
pywps3 processes:

# climate for impact processes
"wps_c4i_simple_indice",
# processes under development
# "wps_spatial_analog",
# "wps_eobs2cordex",

# TODO: c4i processes with multiple input sources
# "wps_c4i_multivar_indice",
# "wps_c4i_percentile_indice",
# "wps_c4i_compound_indice",
"""
