from pywps import Process
from .subset_base import Subsetter, resource, variable, start, end, output, metalink, shape, mosaic


class AveragePolygonProcess(Process, Subsetter):
    """Subset a NetCDF file using WFS geometry."""

    def __init__(self):
        inputs = [resource, shape, mosaic, start, end, variable]
        outputs = [output, metalink]

        super().__init__(
            self._handler,
            identifier='average-polygon',
            title='Average over polygon',
            version='0.1',
            abstract=('Return the average of the data for which grid cells intersect the '
                      'selected polygon for each input dataset as well as'
                      'the time range selected.'),
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )
