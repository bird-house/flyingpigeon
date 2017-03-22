from .wps_subset_countries import ClippingProcess
from .wps_subset_continents import ClipcontinentProcess
from .wps_subset_regionseurope import ClipregionseuropeProcess
from .wps_subset_points import SubsetpointsProcess
from .wps_climatefactsheet import FactsheetProcess
from .wps_fetch import FetchProcess
from .wps_landseamask import LandseamaskProcess
from .wps_indices_percentile import IndicespercentileProcess
from .wps_indices_single import IndicessingleProcess
from .wps_sdm_gbiffetch import GBIFfetchProcess
from .wps_robustness import RobustnessProcess
from .wps_plot_timeseries import PlottimeseriesProcess

processes = [
    ClippingProcess(),
    ClipcontinentProcess(),
    ClipregionseuropeProcess(),
    SubsetpointsProcess(),
    FactsheetProcess(),
    FetchProcess(),
    LandseamaskProcess(),
    IndicespercentileProcess(),
    IndicessingleProcess(),
    GBIFfetchProcess(),
    RobustnessProcess(),
    PlottimeseriesProcess()
]

"""
__all__ = [
        "wps_subset_continents",                # adopted to pywps4
        "wps_subset_countries",                 # adopted to pywps4
        "wps_subset_regionseurope",             # adopted to pywps4
        "wps_subset_points",                    # adopted to pywps4
        "wps_landseamask",                      # adopted to pywps4
        "wps_indices_simple",                   # adopted to pywps4
        "wps_indices_percentile",               # adopted to pywps4
        "wps_weatherregimes_reanalyse",
        "wps_weatherregimes_model",
        "wps_weatherregimes_projection",
        "wps_analogs_detection",
        "wps_analogs_model",
        "wps_analogs_compare",
        "wps_analogs_viewer",
        "wps_segetalflora",
        "wps_sdm_gbiffetch",                    # adopted to pywps4
        "wps_sdm_getindices",
        "wps_sdm_csvindices",
        "wps_sdm_csv",
        "wps_sdm_allinone",
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
        ]
"""
