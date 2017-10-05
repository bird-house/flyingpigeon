"""
===============
WPS Tests Utils
===============

Functions:

 * :func:`get_wps_xlink` - read a document from an url.
 * :func:`config_is_available` - skip tests when config is unavailable.

"""

import unittest
try:
    from urllib.request import urlopen, Request
except ImportError:
    from urllib2 import urlopen, Request

from owslib.wps import WebProcessingService, WPSReader, WPSExecution


def get_capabilities(wps_host=None, wps_client=None, version='1.0.0'):
    if wps_host:
        return WebProcessingService(wps_host, version)
    else:
        response = wps_client.get(
            '?service=WPS&request=GetCapabilities&version={0}'.format(version))
        wps_reader = WPSReader()
        element = wps_reader.readFromString(response.get_data())
        wps = WebProcessingService(None, version, skip_caps=True)
        wps._parseCapabilitiesMetadata(element)
        return wps


def describe_process(identifier, wps_host=None, wps_client=None,
                     version='1.0.0'):
    if wps_host:
        wps = WebProcessingService(wps_host, version)
        return wps.describeprocess(identifier)
    else:
        response = wps_client.get(
            ('?service=WPS&request=DescribeProcess&version={0}&'
             'identifier={1}').format(version, identifier))
        wps_reader = WPSReader()
        element = wps_reader.readFromString(response.get_data())
        wps = WebProcessingService(None, version, skip_caps=True)
        return wps._parseProcessMetadata(element)


def execute(identifier, inputs=[], wps_host=None, wps_client=None,
            version='1.0.0'):
    if wps_host:
        wps = WebProcessingService(wps_host, version)
        return wps.execute(identifier, inputs=inputs)
    else:
        y = ''
        for data_input in inputs:
            y += '{0}={1};'.format(data_input[0], data_input[1])
        y = y[:-1]
        response = wps_client.get(
            ('?service=WPS&request=execute&version={0}&'
             'identifier={1}&DataInputs={2}').format(version, identifier, y))
        wps_reader = WPSReader()
        element = wps_reader.readFromString(response.get_data())
        execution = WPSExecution()
        execution._parseExecuteResponse(element)
        return execution


def get_wps_xlink(xlink):
    url_request = Request(url=xlink)
    url_response = urlopen(url_request)
    return url_response.read()


def config_is_available(config_section, config_names, config_read,
                        set_wps_host=False):
    """Check if a config section & parameters are available for tests.

    Parameters
    ----------
    config_section : string
        section of a cfg file.
    config_names : list of string
        name of parameters to check.
    config_read : result from read method of ConfigParser.RawConfigParser
    set_wps_host : bool
        whether to set a default wps_host in the output config dictionary,
        if there is already one, it is not overwritten.

    Returns
    -------
    out : dict
        dictionary of parameter:value for all parameters of the given section

    """

    if not hasattr(config_names, '__iter__'):
        config_names = [config_names]
    if config_section not in config_read.sections():
        raise unittest.SkipTest(
            "{0} section not defined in config.".format(config_section))
    section = config_read.items(config_section)
    section_d = {}
    for item in section:
        section_d[item[0]] = item[1]
    for config_name in config_names:
        if (config_name not in section_d) or (not section_d[config_name]):
            raise unittest.SkipTest(
                "{0} not defined in config.".format(config_name))

    if set_wps_host:
        if 'wps_host' in section_d:
            # wps_host might be set, but empty. If that's the case, set to None
            if not section_d['wps_host']:
                section_d['wps_host'] = None
        else:
            section_d['wps_host'] = None

    return section_d
