from tempfile import mkstemp
import os

from eggshell.nc.ocg_utils import call, get_variable
from eggshell.nc.nc_utils import sort_by_filename
from ocgis import env, ShpCabinetIterator, ShpCabinet


from eggshell.config import Paths
import flyingpigeon as fp

import logging
LOGGER = logging.getLogger("PYWPS")
paths = Paths(fp)

env.DIR_SHPCABINET = paths.shapefiles


def countries():
    """
    :return: a list of all country codes.
    """
    countries = _COUNTRIES_.keys()
    # countries = ['DEU', 'FRA', 'GBR', 'ESP', 'ITA']
    # countries.sort()
    return countries


def countries_longname():
    """
    :return: the long name of all countries.
    """
    longname = ''
    for country in countries():
        longname = longname + "%s : %s \n" % (country, _COUNTRIES_[country]['longname'])
    return longname


# def masking(resource, sftlf, threshold=50, land_area=True, prefix=None):
#     """
#     Set land/sea areas to nan.
#
#     :param resource: string path to netCDF resource
#     :param sftlf: land_area fraction netCDF file
#     :param threshold: Percentage of land area
#     :param land_area: If True (default), sea areas will set to nan
#     :param prefix:  prefix for filename. If prefix is not set, a filename will be created
#
#     :returns str: path to netCDF file
#     """
#     cdo = Cdo()
#     #######################
#     # TODO check sftlf unit
#     #######################
#     th = threshold / 100.0
#
#     ###################
#     # generate the mask
#     if land_area is True:
#         mask = cdo.gtc(th, input=sftlf, output='mask.nc')
#     else:
#         # TODO: check the operator ltc/stc
#         mask = cdo.ltc(th, input=sftlf, output='mask.nc')
#
#     # generate output filename
#     if prefix is not None:
#         nc_masked = prefix + '.nc'
#     else:
#         _, nc_masked = mkstemp(dir='.', suffix='.nc')
#
#     #################
#     # divide by mask
#
#     _ = cdo.div(input=[resource, mask], output=nc_masked)
#
#     return nc_masked


def clipping(resource=[], variable=None, dimension_map=None, calc=None, output_format='nc',
             calc_grouping=None, time_range=None, time_region=None,
             historical_concatination=True, prefix=None,
             spatial_wrapping='wrap', polygons=None, mosaic=False,
             dir_output=None, memory_limit=None):
    """ returns list of clipped netCDF files

    :param resource: list of input netCDF files
    :param variable: variable (string) to be used in netCDF
    :param dimesion_map: specify a dimension map if input netCDF has unconventional dimension
    :param calc: ocgis calculation argument
    :param calc_grouping: ocgis calculation grouping
    :param historical_concatination: concat files of RCPs with appropriate historical runs into one timeseries
    :param prefix: prefix for output file name
    :param polygons: list of polygons to be used. If more than 1 in the list, an appropriate mosaic will be clipped
    :param mosaic: Whether the polygons are aggregated into a single geometry (True) or individual files
                   are created for each geometry (False).
    :param output_format: output_format (default='nc')
    :param dir_output: specify an output location
    :param time_range: [start, end] of time subset
    :param time_region: year, months or days to be extracted in the timeseries

    :returns list: path to clipped files
    """

    if type(resource) != list:
        resource = list([resource])
    if type(polygons) != list:
        polygons = list([polygons])
    if prefix is not None:
        if type(prefix) != list:
            prefix = list([prefix])

    geoms = set()
    ncs = sort_by_filename(resource, historical_concatination=historical_concatination)  # historical_concatenation=True
    geom_files = []
    if mosaic is True:
        try:
            nameadd = '_'
            for polygon in polygons:
                geoms.add(get_geom(polygon))
                nameadd = nameadd + polygon.replace(' ', '')
            if len(geoms) > 1:
                LOGGER.error('polygons belong to different shapefiles! mosaic option is not possible %s', geoms)
            else:
                geom = geoms.pop()
            ugids = get_ugid(polygons=polygons, geom=geom)
        except Exception as ex:
            LOGGER.exception('geom identification failed {}'.format(str(ex)))
        for i, key in enumerate(ncs.keys()):
            try:
                # if variable is None:
                variable = get_variable(ncs[key])
                LOGGER.info('variable %s detected in resource' % (variable))
                if prefix is None:
                    name = key + nameadd
                else:
                    name = prefix[i]
                geom_file = call(resource=ncs[key], variable=variable, calc=calc, calc_grouping=calc_grouping,
                                 output_format=output_format, prefix=name,
                                 geom=geom, select_ugid=ugids, time_range=time_range,
                                 time_region=time_region,
                                 spatial_wrapping=spatial_wrapping, memory_limit=memory_limit,
                                 dir_output=dir_output, dimension_map=dimension_map)
                geom_files.append(geom_file)
                LOGGER.info('ocgis mosaik clipping done for %s' % (key))
            except Exception as ex:
                msg = 'ocgis mosaik clipping failed for %s, %s ' % (key, ex)
                LOGGER.exception(msg)
    else:
        for i, polygon in enumerate(polygons):
            try:
                geom = get_geom(polygon)
                ugid = get_ugid(polygons=polygon, geom=geom)
                for key in ncs.keys():
                    try:
                        # if variable is None:
                        variable = get_variable(ncs[key])
                        LOGGER.info('variable %s detected in resource' % (variable))
                        if prefix is None:
                            name = key + '_' + polygon.replace(' ', '')
                        else:
                            name = prefix[i]
                        geom_file = call(resource=ncs[key], variable=variable, calc=calc, calc_grouping=calc_grouping,
                                         output_format=output_format,
                                         prefix=name, geom=geom, select_ugid=ugid, dir_output=dir_output,
                                         dimension_map=dimension_map, spatial_wrapping=spatial_wrapping,
                                         memory_limit=memory_limit, time_range=time_range, time_region=time_region,
                                         )
                        geom_files.append(geom_file)
                        LOGGER.info('ocgis clipping done for %s' % (key))
                    except Exception as ex:
                        msg = 'ocgis clipping failed for %s: %s ' % (key, ex)
                        LOGGER.exception(msg)
            except Exception as ex:
                LOGGER.exception('geom identification failed {}'.format(str(ex)))
    return geom_files


def get_dimension_map(resource):
    """ returns the dimension map for a file, required for ocgis processing.
    file must have a DRS-conformant filename (see: utils.drs_filename())

    OBSOLETE!

    :param resource: str input file path

    :returns dic: dimension map dictionary
    """
    pass
#
# file_name = os.path.basename(resource)
#
# dim_map1 = {'X': {'variable': 'lon', 'dimension': 'x', 'pos': 2},
#             'Y': {'variable': 'lat', 'dimension': 'y', 'pos': 1},
#             'T': {'variable': 'time', 'dimension': 'time', 'pos': 0}}
#
# #dim_map2 = {'X': {'variable': 'lon', 'dimension': 'x', 'pos': 2},
#             #'Y': {'variable': 'lat', 'dimension': 'y', 'pos': 1},
#             #'T': {'variable': 'time', 'dimension': 'time', 'pos': 0, 'bounds': 'time_bnds'}}
#
# dim_map3 = {'X': {'variable': 'rlon', 'dimension': 'x', 'pos': 2},
#             'Y': {'variable': 'rlat', 'dimension': 'y', 'pos': 1},
#             'T': {'variable': 'time', 'dimension': 'time', 'pos': 0 }}
#
# dim_map4 = {'X': {'variable': 'Actual_longitude', 'dimension': 'x', 'pos': 2},
#             'Y': {'variable': 'Actual_latitude', 'dimension': 'y', 'pos': 1},
#             'T': {'variable': 'time', 'dimension': 'time', 'pos': 0 }}
#
# dim_map5 = {'X': {'variable': 'x', 'dimension': 'x', 'pos': 2},
#             'Y': {'variable': 'y', 'dimension': 'y', 'pos': 1},
#             'T': {'variable': 'time', 'dimension': 'time', 'pos': 0 }}
#
# if 'CM5A-MR_WRF331F' in file_name:
#   dimension_map = dim_map1
# elif 'CNRM-CM5_CNRM-ALADIN53' in file_name:
#   dimension_map = dim_map1
# elif 'MPI-ESM-LR_REMO019' in file_name:
#   dimension_map = dim_map1
# elif 'CLMcom-CCLM4-8-17' in file_name:
#   dimension_map = dim_map1
# elif '_v11.0' in file_name: # EOBS Data
#   dimension_map = dim_map4
# #elif 'EOBS' in file_name:
#   #dimension_map = dim_map5
# else:
#   dimension_map = None
#
# return dimension_map


def get_shp_column_values(geom, columnname):
    """ returns a list of all entries the shapefile column name

    :param geom: name of the shapefile
    :param columnname: Column name to be intepreted

    returns list: column names
    """

    # import ocgis

    # env.DIR_SHPCABINET = shapefiles_path()
    sci = ShpCabinetIterator(geom)

    vals = []
    for row in sci:
        vals.append(row['properties'][columnname])
    return vals


def get_ugid(polygons=None, geom=None):
    """
    returns geometry id of given polygon in a given shapefile.

    :param polygons: string or list of the region polygons
    :param geom: available shapefile. Possible entries: '50m_country', 'NUTS2'

    :returns list: ugids used by ocgis
    """

    if polygons is None:
        result = None
    else:
        if type(polygons) != list:
            polygons = list([polygons])

        # env.DIR_SHPCABINET = shapefiles_path()
        sc_iter = ShpCabinetIterator(geom)
        result = []

        if geom == 'countries':
            for row in sc_iter:
                for polygon in polygons:
                    if row['properties']['ADM0_A3'] == polygon:
                        result.append(row['properties']['UGID'])

        # elif geom == 'extremoscope':
        #     for row in sc_iter:
        #         for polygon in polygons:
        #             if row['properties']['HASC_1'] == polygon:
        #                 result.append(row['properties']['UGID'])

        elif geom == 'continents':
            for row in sc_iter:
                for polygon in polygons:
                    if row['properties']['CONTINENT'] == polygon:
                        result.append(row['properties']['UGID'])
        else:
            sc = ShpCabinet(paths.shapefiles)
            LOGGER.debug('geom: %s not found in shape cabinet. Available geoms are: %s ', geom, sc)
    return result


def get_geom(polygon=None):
    """ returns the appropriate shapefile (geom) for a given polygon abbreviation

    :param polygon: polygon short name

    returns str: name of shapefile (geom)
    """

    if polygon is None:
        geom = None
    else:
        if polygon in _COUNTRIES_:  # (polygon) == 3:
            geom = 'countries'
        # elif polygon in _POLYGONS_EXTREMOSCOPE_:  # len(polygon) == 5 and polygon[2] == '.':
        #     geom = 'extremoscope'
        # elif polygon in _EUREGIONS_:
        #     geom = 'extremoscope'
        elif polygon in _CONTINENTS_:
            geom = 'continents'
        else:
            LOGGER.debug('polygon: %s not found in geoms' % polygon)
    return geom


# === Available Polygons
_CONTINENTS_ = get_shp_column_values(geom='continents', columnname='CONTINENT')

_COUNTRIES_ = {}
# _COUNTRIES_Europe_ = {}

# === populate polygon dictionaries
ADM0_A3 = get_shp_column_values(geom='countries', columnname='ADM0_A3')
NAMELONG = get_shp_column_values(geom='countries', columnname='NAME_LONG')
CONTINENT = get_shp_column_values(geom='countries', columnname='CONTINENT')

for c, key in enumerate(ADM0_A3):
    _COUNTRIES_[key] = dict(longname=NAMELONG[c])

# for c, key in enumerate(ADM0_A3):
#     if CONTINENT[c] == 'Europe':
#         _COUNTRIES_Europe_[key] = dict(longname=NAMELONG[c])

# _EUREGIONS_ = {}
#
# HASC_1 = get_shp_column_values(geom='extremoscope', columnname='HASC_1')
# NAME_1 = get_shp_column_values(geom='extremoscope', columnname='NAME_1')

# _POLYGONS_EXTREMOSCOPE_ = HASC_1

# for c, key in enumerate(HASC_1):
#     _EUREGIONS_[key] = dict(longname=NAME_1[c])
