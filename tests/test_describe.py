from pywps import Service
from .common import client_for
from flyingpigeon.processes import processes
from owslib.wps import WebProcessingService


def test_describe():
    """Check that owslib can parse the processes' description."""
    # Get the description of all processes
    client = client_for(Service(processes=processes))
    resp = client.get(service='wps', request='describeprocess', identifier='all', version='1.0.0')

    # Parse the description with owslib. Dummy URL.
    wps = WebProcessingService("http://localhost", skip_caps=True)
    wps.describeprocess("all", xml=resp.data)
