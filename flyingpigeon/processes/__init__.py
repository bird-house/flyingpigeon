from .wps_subset_countries import ClippingProcess
from .wps_subset_continents import ClipcontinentProcess
from .wps_subset_regionseurope import ClipregionseuropeProcess
from .wps_pointinspection import PointinspectionProcess
from .wps_landseamask import LandseamaskProcess
from .wps_climatefactsheet import FactsheetProcess
from .wps_fetch import FetchProcess
from .wps_indices_percentiledays import IndicespercentiledaysProcess
from .wps_indices_single import IndicessingleProcess
from .wps_robustness import RobustnessProcess
from .wps_plot_timeseries import PlottimeseriesProcess
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
from .wps_segetalflora import SegetalfloraProcess
from .wps_spatial_analog import SpatialAnalogProcess

processes = [
    ClippingProcess(),
    ClipcontinentProcess(),
    ClipregionseuropeProcess(),
    PointinspectionProcess(),
    #    FactsheetProcess(),
    FetchProcess(),
    LandseamaskProcess(),
    IndicespercentiledaysProcess(),
    IndicessingleProcess(),
    GBIFfetchProcess(),
    SDMgetindicesProcess(),
    #    SDMcsvProcess(),
    #    SDMcsvindicesProcess(),
    #    SDMallinoneProcess(),
    WeatherregimesreanalyseProcess(),
    WeatherregimesprojectionProcess(),
    WeatherregimesmodelProcess(),
    AnalogsreanalyseProcess(),
    #    AnalogsmodelProcess(),
    #    AnalogscompareProcess(),
    AnalogsviewerProcess(),
    #    RobustnessProcess(),
    #    PlottimeseriesProcess(),
    SegetalfloraProcess(),
    SpatialAnalogProcess()
]

"""
pywps3 processes:

"wps_subset_continents",                # adopted to pywps4
"wps_subset_countries",                 # adopted to pywps4
"wps_subset_regionseurope",             # adopted to pywps4
"wps_subset_points",                    # adopted to pywps4
"wps_landseamask",                      # adopted to pywps4
"wps_indices_simple",                   # adopted to pywps4
"wps_indices_percentile",               # adopted to pywps4
"wps_weatherregimes_reanalyse",         # adopted to pywps4
"wps_weatherregimes_model",             # adopted to pywps4
"wps_weatherregimes_projection",        # adopted to pywps4
"wps_analogs_detection",                # adopted to pywps4
"wps_analogs_model",                    # adopted to pywps4
"wps_analogs_compare",                  # adopted to pywps4
"wps_analogs_viewer",                   # adopted to pywps4
"wps_segetalflora",                     # adopted to pywps4
"wps_sdm_gbiffetch",                    # adopted to pywps4
"wps_sdm_getindices",                   # adopted to pywps4
"wps_sdm_csvindices",                   # adopted to pywps4
"wps_sdm_csv",                          # adopted to pywps4
"wps_sdm_allinone",                     # adopted to pywps4
"wps_robustness",                       # adopted to pywps4
"wps_plot_timeseries",                  # adopted to pywps4
"wps_climatefactsheet",                 # adopted to pywps4
"wps_fetch",                            # adopted to pywps4

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
