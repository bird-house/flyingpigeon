"""
===============
WPS Tests Utils
===============

Functions:

 * :func:`xml_children_as_dict` - create dictionary from xml element children.
 * :func:`xml_attrib_nsmap` - replace nsmap values with their key.
 * :func:`parse_getcapabilities` - parse wps GetCapabilities response.
 * :func:`parse_describeprocess` - parse wps DescribeProcess response.
 * :func:`parse_execute_response` - parse wps execute response.
 * :func:`wps_response` - get xml document response from a WPS.
 * :func:`get_wps_xlink` - read a document from an url.
 * :func:`config_is_available` - skip tests when config is unavailable.

"""

import unittest
from lxml import etree
try:
    from urllib.request import urlopen, Request
except ImportError:
    from urllib2 import urlopen, Request


def xml_children_as_dict(element):
    """Create dictionary from xml element children.

    Parameters
    ----------
    element : lxml.etree._Element

    Returns
    -------
    out : dict

    Notes
    -----
    An additional replacement of the nsmap values with their key is done
    on the tag of each child.

    """

    d = {}
    for child in element:
        child_tag = child.tag
        for key, nsvalue in child.nsmap.items():
            child_tag = child_tag.replace('{' + nsvalue + '}', key + ':')
        if child_tag in d:
            d[child_tag].append(child)
        else:
            d[child_tag] = [child]
    return d


def xml_attrib_nsmap(element):
    """Replace nsmap values with their key in element attributes.

    Parameters
    ----------
    element : lxml.etree._Element

    Returns
    -------
    out : dict
        element.attrib with the replaced nsmap.

    """

    d = {}
    for key, value in element.attrib.items():
        new_key = key
        for nskey, nsvalue in element.nsmap.items():
            new_key = new_key.replace('{' + nsvalue + '}', nskey + ':')
        d[new_key] = value
    return d


def parse_getcapabilities(html_response):
    """Parse WPS GetCapabilities response.

    Parameters
    ----------
    html_response : string
        xml document from a GetCapabilities WPS request.

    Returns
    -------
    out : list of string
        wps:ProcessOfferings -> wps:Process -> ows:Identifier

    """

    # XML structure:
    # wps:Capabilities
    #     ows:ServiceIdentification
    #         [...]
    #     ows:ServiceProvider
    #         [...]
    #     ows:OperationsMetadata
    #         [...]
    #     wps:ProcessOfferings
    #         wps:Process (list)
    #             ows:Identifier (text)
    #             ows:Title (text)
    #     wps:Languages
    #         [...]
    processes = []
    capabilities = xml_children_as_dict(etree.fromstring(html_response))
    process_offerings = xml_children_as_dict(
        capabilities['wps:ProcessOfferings'][0])
    for process_element in process_offerings['wps:Process']:
        process = xml_children_as_dict(process_element)
        processes.append(process['ows:Identifier'][0].text)
    return sorted(processes)


def parse_describeprocess(html_response):
    """Parse WPS DescribeProcess response.

    Parameters
    ----------
    html_response : string
        xml document from a DescribeProcess WPS request.

    Returns
    -------
    out : list of dict
        'identifier' : ProcessDescription -> ows:Identifier
        'inputs' : ProcessDescription -> DataInputs -> Input -> ows:Identifier
        'ouputs' : ProcessDescription -> ProcessOutputs -> Output ->
                   ows:Identifier

    """

    # XML structure:
    # wps:ProcessDescriptions
    #     ProcessDescription (list)
    #         ows:Identifier (text)
    #         ows:Title (text)
    #         DataInputs (optional)
    #             Input (list)
    #                 ows:Identifier (text)
    #                 ows:Title (text)
    #                 LiteralData (xor)
    #                     ows:DataType
    #                     [...]
    #         ProcessOutputs
    #             Output (list)
    #                 ows:Identifier (text)
    #                 ows:Title (text)
    #                 LiteralOutput (xor)
    #                     ows:DataType
    #                     [...]
    processes = []
    process_descriptions = xml_children_as_dict(
        etree.fromstring(html_response))
    for process_description_el in process_descriptions['ProcessDescription']:
        d = {'inputs': [], 'outputs': []}
        process_description = xml_children_as_dict(process_description_el)
        d['identifier'] = process_description['ows:Identifier'][0].text
        if 'DataInputs' in process_description:
            data_inputs = xml_children_as_dict(
                process_description['DataInputs'][0])
            for input_element in data_inputs['Input']:
                input1 = xml_children_as_dict(input_element)
                d['inputs'].append(input1['ows:Identifier'][0].text)
        process_outputs = xml_children_as_dict(
            process_description['ProcessOutputs'][0])
        for output_element in process_outputs['Output']:
            output1 = xml_children_as_dict(output_element)
            d['outputs'].append(output1['ows:Identifier'][0].text)
        processes.append(d)
    return processes


def parse_execute_response(html_response):
    """Parse WPS execute response.

    Parameters
    ----------
    html_response : string
        xml document from an execute WPS request.

    Returns
    -------
    out : dict
        'identifier' : wps:Process -> ows:Identifier
        'status' : wps:Status -> {tag of the status, without nsmap}
        'ouputs' : wps:ProcessOutputs -> wps:Output ->
                   {ows:Identifier : wps:Data}

    """

    # XML structure:
    # wps:ExecuteResponse
    #     wps:Process
    #         ows:Identifier (text)
    #         ows:Title (text)
    #     wps:Status
    #         creationTime (attrib)
    #         wps:ProcessSucceeded (xor, text)
    #         wps:ProcessFailed (xor, text)
    #         wps:ProcessAccepted (xor, text)
    #         wps:ProcessStarted (xor, text)
    #             percentCompleted (attrib)
    #     wps:ProcessOutputs
    #         wps:Output (list)
    #             ows:Identifier (text)
    #             ows:Title (text)
    #             wps:Data (xor)
    #                 wps:LiteralData (xor)
    #                 [...]
    #             wps:Reference (xor)
    #                 xlink:href (attrib)
    #                 mimeType (attrib)
    d = {'outputs': {}}
    execute_response_element = etree.fromstring(html_response)
    execute_response_attrib = xml_attrib_nsmap(execute_response_element)
    if 'statusLocation' in execute_response_attrib:
        d['statusLocation'] = execute_response_attrib['statusLocation']
    execute_response = xml_children_as_dict(execute_response_element)

    process = xml_children_as_dict(execute_response['wps:Process'][0])
    d['identifier'] = process['ows:Identifier'][0].text

    status_element = execute_response['wps:Status'][0]
    status_attrib = xml_attrib_nsmap(status_element)
    status = xml_children_as_dict(status_element)
    d['creationTime'] = status_attrib['creationTime']
    if 'wps:ProcessSucceeded' in status:
        d['status'] = 'ProcessSucceeded'
    elif 'wps:ProcessFailed' in status:
        d['status'] = 'ProcessFailed'
        return d
    elif 'wps:ProcessAccepted' in status:
        d['status'] = 'ProcessAccepted'
        return d
    elif 'wps:ProcessStarted' in status:
        process_started_element = status['wps:ProcessStarted'][0]
        process_started_attrib = xml_attrib_nsmap(process_started_element)
        d['status'] = 'ProcessStarted'
        d['percentCompleted'] = \
            float(process_started_attrib['percentCompleted'])
        return d
    else:
        raise NotImplementedError()

    process_outputs = xml_children_as_dict(
        execute_response['wps:ProcessOutputs'][0])
    for output_element in process_outputs['wps:Output']:
        output1 = xml_children_as_dict(output_element)
        identifier = output1['ows:Identifier'][0].text
        if 'wps:Data' in output1:
            data1 = xml_children_as_dict(output1['wps:Data'][0])
            if 'wps:LiteralData' in data1:
                d['outputs'][identifier] = data1['wps:LiteralData'][0].text
            else:
                raise NotImplementedError()
        elif 'wps:Reference' in output1:
            reference_element = output1['wps:Reference'][0]
            reference_attrib = xml_attrib_nsmap(reference_element)
            d['outputs'][identifier] = reference_attrib['xlink:href']
        else:
            raise NotImplementedError()
    return d


def wps_response(wps_host, pywps_request, wps_client=None):
    """Get xml document response from a WPS.

    Parameters
    ----------
    wps_host : string or None
        url without the http:// prefix (e.g. 'localhost:8009/pywps').
        If set to None, will use the wps_client provided (required).
    pywps_request : string
        wps request starting with '?service=WPS&request=[...]'
    wps_client : pywps.tests.WpsClient or None
        If wps_host is None this will be used to listen to wps requests.

    Returns
    -------
    out : string
        response from the server, which is an xml document if the request
        is valid and the server is correctly setup.

    """

    if wps_host:
        url_request = Request(
            url='http://{0}{1}'.format(wps_host, pywps_request))
        url_response = urlopen(url_request)
        return url_response.read()
    else:
        resp = wps_client.get(pywps_request)
        return resp.get_data()


def get_wps_xlink(xlink):
    url_request = Request(url=xlink)
    url_response = urlopen(url_request)
    return url_response.read()


def config_is_available(config_section, config_names, config_dict):
    if not hasattr(config_names, '__iter__'):
        config_names = [config_names]
    if config_section not in config_dict.sections():
        raise unittest.SkipTest(
            "{0} section not defined in config.".format(config_section))
    section = config_dict.items(config_section)
    section_d = {}
    for item in section:
        section_d[item[0]] = item[1]
    for config_name in config_names:
        if (config_name not in section_d) or (not section_d[config_name]):
            raise unittest.SkipTest(
                "{0} not defined in config.".format(config_name))
    return section_d


def set_wps_host(config_dict):
    wps_host = config_dict.get('wps_host', None)
    # wps_host might be set, but empty...
    if not wps_host:
        wps_host = None
    return wps_host
