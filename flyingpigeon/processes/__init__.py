# from .wps_say_hello import SayHello
from .wps_subset import SubsetProcess
from .wps_subset_bbox import SubsetBboxProcess
from .wps_subset_continents import SubsetcontinentProcess


processes = [
    # SayHello(),
    SubsetProcess(),
    SubsetBboxProcess(),
    SubsetcontinentProcess(),
]
