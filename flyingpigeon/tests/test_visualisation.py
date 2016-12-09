import tempfile

from flyingpigeon import visualisation


def test_map_gbifoccurrences():
    from numpy import zeros
    latlon = zeros((3, 2))
    latlon[0] = [10, 20]
    latlon[1] = [11, 21]
    latlon[2] = [12, 22]
    visualisation.map_gbifoccurrences(latlon=latlon, dir=tempfile.gettempdir())
