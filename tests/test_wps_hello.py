import pytest

from pywps import Service
from pywps.tests import client_for, assert_response_success

from .common import get_output
from flyingpigeon.processes.wps_say_hello import SayHello


def test_wps_hello():
    client = client_for(Service(processes=[SayHello()]))
    datainputs = "name=Flying Pigeon"
    resp = client.get(
        "?service=WPS&request=Execute&version=1.0.0&identifier=hello&datainputs={}".format(
            datainputs))
    assert_response_success(resp)
    assert get_output(resp.xml) == {'output': "Hello Flying Pigeon"}
