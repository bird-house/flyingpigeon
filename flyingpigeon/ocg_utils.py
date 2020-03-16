from os.path import abspath, curdir
# import flyingpigeon.config
import logging
from ocgis import RequestDataset

LOGGER = logging.getLogger("PYWPS")

# This should replace calc_grouping, as it provides direct access to keys and makes inspection easier.
temp_groups = {'AMJJAS': [[4, 5, 6, 7, 8, 9], 'unique'],
               'Apr': [[4], 'unique'],
               'Aug': [[8], 'unique'],
               'DJF': [[12, 1, 2], 'unique'],
               'Dec': [[12], 'unique'],
               'Feb': [[2], 'unique'],
               'JJA': [[6, 7, 8], 'unique'],
               'Jan': [[1], 'unique'],
               'Jul': [[7], 'unique'],
               'Jun': [[6], 'unique'],
               'MAM': [[3, 4, 5], 'unique'],
               'Mar': [[3], 'unique'],
               'May': [[5], 'unique'],
               'Nov': [[11], 'unique'],
               'ONDJFM': [[10, 11, 12, 1, 2, 3], 'unique'],
               'Oct': [[10], 'unique'],
               'SON': [[9, 10, 11], 'unique'],
               'Sep': [[9], 'unique'],
               'day': ['year', 'month', 'day'],
               'mon': ['year', 'month'],
               'sem': [[12, 1, 2], [3, 4, 5], [6, 7, 8], [9, 10, 11], 'unique'],
               'yr': ['year']}
# # TODO: include regridding with ocgis


def call(resource=[], variable=None, dimension_map=None, agg_selection=True,
         calc=None, calc_grouping=None, conform_units_to=None, crs=None,
         memory_limit=None, prefix=None,
         regrid_destination=None, regrid_options='bil', level_range=None,  # cdover='python',
         geom=None, output_format_options=None, search_radius_mult=2.,
         select_nearest=False, select_ugid=None, spatial_wrapping=None,
         t_calendar=None, time_region=None,
         time_range=None, dir_output=None, output_format='nc'):
    """
    Call OCGIS operation.

    :param resource: Input netCDF file.
    :param variable: variable in the input file to be picked
    :param dimension_map: dimension map in case of unconventional storage of data
    :param agg_selection: For aggregation of in case of mulitple polygons geoms
    :param calc: ocgis calc syntax for calculation partion
    :param calc_grouping: time aggregate grouping
    :param cdover: OUTDATED use py-cdo ('python', by default) or cdo from the system ('system')
    :param conform_units_to:
    :param crs: coordinate reference system
    :param memory_limit: limit the amount of data to be loaded into the memory at once \
        if None (default) free memory is detected by birdhouse
    :param level_range: subset of given levels
    :param prefix: string for the file base name
    :param regrid_destination: file path with netCDF file with grid for output file
    :param geom: name of shapefile stored in birdhouse shape cabinet
    :param output_format_options: output options for netCDF e.g compression level()
    :param regrid_destination: file containing the targed grid (griddes.txt or netCDF file)
    :param regrid_options: methods for regridding:
                          'bil' = Bilinear interpolation
                          'bic' = Bicubic interpolation
                          'dis' = Distance-weighted average remapping
                          'nn' = nearest neighbour
                          'con' = First-order conservative remapping
                          'laf' = largest area fraction reamapping
    :param search_radius_mult: search radius for point geometries. All included gridboxes will be returned
    :param select_nearest: nearest neighbour selection for point geometries
    :param select_ugid: ugid for appropriate polygons
    :param spatial_wrapping: how to handle coordinates in case of subsets, options: None (default), 'wrap', 'unwrap'
    :param time_region: select single month
    :param time_range: sequence of two datetime.datetime objects to mark start and end point
    :param dir_output: path to folder to store ouput files  (default= curdir)
    :param output_format: format in which results will be returned.
    :return: output file path
    """
    LOGGER.info('Start ocgis module call function')
    from ocgis import OcgOperations, RequestDataset, env
    from ocgis.util.large_array import compute
    from datetime import datetime as dt
    from datetime import date as dd
    # from datetime import time as dt_time
    import uuid

    # prepare the environment
    env.OVERWRITE = True

    if dir_output is None:
        dir_output = abspath(curdir)

    # check time_range format:

    if time_range is not None:
        try:
            LOGGER.debug('time_range type= %s , %s ' % (type(time_range[0]), type(time_range[1])))
            LOGGER.debug('time_range= %s , %s ' % (time_range[0], time_range[1]))
            # if type(time_range[0] is 'datetime.date'):
            if (isinstance(time_range[0], dd) and not isinstance(time_range[0], dt)):
                time_range = [dt.combine(time_range[0], dt.min.time()),
                              dt.combine(time_range[1], dt.min.time())]
                # time_range = [dt.combine(time_range[0], dt_time(12,0)),
                #               dt.combine(time_range[1], dt_time(12,0))]
            LOGGER.debug('time_range changed to type= %s , %s ' % (type(time_range[0]), type(time_range[1])))
            LOGGER.debug('time_range changed to= %s , %s ' % (time_range[0], time_range[1]))
        except Exception as ex:
            LOGGER.exception('failed to convert data to datetime {}'.format(ex))

    if spatial_wrapping == 'wrap':
        spatial_reorder = True
    else:
        spatial_reorder = False
    LOGGER.debug('spatial_reorder: %s and spatial_wrapping: %s ' % (spatial_reorder, spatial_wrapping))

    if prefix is None:
        prefix = str(uuid.uuid1())
        env.PREFIX = prefix
    #
    # if output_format_options is False:
    #     output_format_options = None
    # elif output_format_options is True:
    #     output_format_options = {'data_model': 'NETCDF4',  # NETCDF4_CLASSIC
    #                              'variable_kwargs': {'zlib': True, 'complevel': 9}}
    # else:
    if output_format_options is not None:
        LOGGER.info('output_format_options are set to %s ' % (output_format_options))

    if type(resource) != list:
        resource = list([resource])
    # execute ocgis
    LOGGER.info('Execute ocgis module call function')

    try:
        LOGGER.debug('call module dir_output = %s ' % abspath(dir_output))
        rd = RequestDataset(resource,
                            variable=variable,
                            level_range=level_range,
                            dimension_map=dimension_map,
                            conform_units_to=conform_units_to,
                            time_region=time_region,
                            t_calendar=t_calendar,
                            time_range=time_range)

        from ocgis.constants import DimensionMapKey
        rd.dimension_map.set_bounds(DimensionMapKey.TIME, None)

        ops = OcgOperations(dataset=rd,
                            output_format_options=output_format_options,
                            dir_output=dir_output,
                            spatial_wrapping=spatial_wrapping,
                            spatial_reorder=spatial_reorder,
                            # regrid_destination=rd_regrid,
                            # options=options,
                            calc=calc,
                            calc_grouping=calc_grouping,
                            geom=geom,
                            agg_selection=agg_selection,
                            output_format=output_format,
                            prefix=prefix,
                            search_radius_mult=search_radius_mult,
                            select_nearest=select_nearest,
                            select_ugid=select_ugid,
                            add_auxiliary_files=False)
        LOGGER.info('OcgOperations set')
    except Exception as ex:
        LOGGER.exception('failed to setup OcgOperations: {}'.format(ex))
        return None

    # TODO include comaprison dataload to available memory
    dataload = 1
    available_memory = 2

    try:
        if dataload < available_memory:  # compare dataload to free_memory
            LOGGER.info('ocgis module call as ops.execute()')
            geom_file = ops.execute()
        else:
            # LOGGER.info('ocgis module call as compute(ops)')
            # TODO: estimate right tile_dimensionS
            tile_dimension = 10  # default
            LOGGER.info('Not enough memory for data load, ocgis module call compute in chunks')
            geom_file = compute(ops, tile_dimension=tile_dimension, verbose=True)

    except Exception as ex:
        LOGGER.exception('failed to execute ocgis operation : {}'.format(ex))
        return None
    return geom_file

    # try:
    #     from numpy import sqrt
    #     from flyingpigeon.utils import FreeMemory
    #
    #     if memory_limit is None:
    #         f = FreeMemory()
    #         mem_kb = f.user_free
    #         mem_mb = mem_kb / 1024.
    #         mem_limit = mem_mb / 2.  # set limit to half of the free memory
    #     else:
    #         mem_limit = memory_limit
    #
    #     if mem_limit >= 1024. * 4:
    #         mem_limit = 1024. * 4
    #         # 475.0 MB for openDAP
    #
    #     LOGGER.info('memory_limit = %s Mb' % (mem_limit))
    #
    #     data_kb = ops.get_base_request_size()['total']
    #     data_mb = data_kb / 1024.
    #
    #     # data_kb = size['total']/reduce(lambda x,y: x*y,size['variables'][variable]['value']['shape'])
    #     LOGGER.info('data_mb  = %s Mb' % (data_mb))
    #
    #     if data_mb <= mem_limit:  # input is smaler than the half of free memory size
    #         try:
    #             LOGGER.info('ocgis module call as ops.execute()')
    #             geom_file = ops.execute()
    #         except Exception as e:
    #             LOGGER.debug('failed to execute ocgis operation')
    #             raise
    #             return None
    #
    #     else:
    #         ##########################
    #         # calcultion of chunk size
    #         ##########################
    #         try:
    #             size = ops.get_base_request_size()
    #             nb_time_coordinates_rd = size['variables'][variable]['temporal']['shape'][0]
    #             element_in_kb = size['total']/reduce(lambda x, y: x*y, size['variables'][variable]['value']['shape'])
    #             element_in_mb = element_in_kb / 1024.
    #             tile_dim = sqrt(mem_limit/(element_in_mb*nb_time_coordinates_rd))  # maximum chunk size
    #
    #             LOGGER.info('ocgis module call compute with chunks')
    #             if calc is None:
    #                 calc = '%s=%s*1' % (variable, variable)
    #                 LOGGER.info('calc set to = %s ' % calc)
    #             ops = OcgOperations(dataset=rd,
    #                                 output_format_options=output_format_options,
    #                                 dir_output=dir_output,
    #                                 spatial_wrapping=spatial_wrapping,
    #                                 spatial_reorder=spatial_reorder,
    #                                 # regrid_destination=rd_regrid,
    #                                 # options=options,
    #                                 calc=calc,
    #                                 calc_grouping=calc_grouping,
    #                                 geom=geom,
    #                                 output_format=output_format,
    #                                 prefix=prefix,
    #                                 search_radius_mult=search_radius_mult,
    #                                 select_nearest=select_nearest,
    #                                 select_ugid=select_ugid,
    #                                 add_auxiliary_files=False)
    #             geom_file = compute(ops, tile_dimension=int(tile_dim), verbose=True)
    #             print 'ocgis calculated'
    #         except Exception as e:
    #             LOGGER.debug('failed to compute ocgis with chunks')
    #             raise
    #             return None
    #     LOGGER.info('Succeeded with ocgis module call function')
    # except:
    #     LOGGER.exception('failed to compare dataload with free memory, calling as execute instead')

    # ############################################
    # # remapping according to regrid informations
    # ############################################
    # if regrid_destination is not None:
    #     try:
    #         if (cdover=='system'):
    #             from os import system
    #             remap = 'remap%s' % regrid_options
    #             output = '%s.nc' % uuid.uuid1()
    #             output = abspath(curdir)+'/'+output
    #             comcdo = 'cdo -O %s,%s %s %s' % (remap, regrid_destination, geom_file, output)
    #             system(comcdo)
    #
    #             if(isfile(output)==False):
    #                 comcdo = '/usr/bin/cdo -O %s,%s %s %s' % (remap, regrid_destination, geom_file, output)
    #                 system(comcdo)
    #
    #             if(isfile(output)==False): cdover='python'
    #
    #             # need to substitute by subprocess call
    #             # TODO: If system failed - py-cdo used insted
    #             # what if py-cdo failed, with option 'python'
    #             # need to call 'system' in this case - need to write function
    #
    #         if (cdover=='python'):
    #             from tempfile import mkstemp
    #             from cdo import Cdo
    #             cdo = Cdo()
    #             output = '%s.nc' % uuid.uuid1()
    #             remap = 'remap%s' % regrid_options
    #             call = [op for op in dir(cdo) if remap in op]
    #             cmd = "output = cdo.%s('%s',input='%s', output='%s')" \
    #                   % (str(call[0]), regrid_destination, geom_file, output)
    #             exec(cmd)
    #     except Exception as e:
    #         LOGGER.debug('failed to remap')
    #         raise
    #         return None
    # else:
    #     output = geom_file

    # try:
    #     from flyingpigeon.utils import unrotate_pole
    #     lat, lon = unrotate_pole(output)
    # except:
    #     LOGGER.exception('failed to unrotate pole')
    # output


def calc_grouping(grouping):
    """
    translate time grouping abbreviation (e.g 'JJA') into the appropriate ocgis calc_grouping syntax

    :param grouping: time group abbreviation allowed values: "yr", "mon", "sem",
                     "ONDJFM", "AMJJAS", "DJF", "MAM", "JJA", "SON"

    :returns list: calc_grouping conformant to ocgis syntax
    """
    calc_grouping = ['year']  # default year
    if grouping == 'yr':
        calc_grouping = ['year']
    elif grouping == 'sem':
        calc_grouping = [[12, 1, 2], [3, 4, 5], [6, 7, 8], [9, 10, 11], 'unique']
    elif grouping == 'ONDJFM':
        calc_grouping = [[10, 11, 12, 1, 2, 3], 'unique']
    elif grouping == 'AMJJAS':
        calc_grouping = [[4, 5, 6, 7, 8, 9], 'unique']
    elif grouping == 'DJF':
        calc_grouping = [[12, 1, 2], 'unique']
    elif grouping == 'MAM':
        calc_grouping = [[3, 4, 5], 'unique']
    elif grouping == 'JJA':
        calc_grouping = [[6, 7, 8], 'unique']
    elif grouping == 'SON':
        calc_grouping = [[9, 10, 11], 'unique']
    elif grouping == 'day':
        calc_grouping = ['year', 'month', 'day']
    elif grouping == 'mon':
        calc_grouping = ['year', 'month']
    elif grouping == 'Jan':
        calc_grouping = [[1], 'unique']
    elif grouping == 'Feb':
        calc_grouping = [[2], 'unique']
    elif grouping == 'Mar':
        calc_grouping = [[3], 'unique']
    elif grouping == 'Apr':
        calc_grouping = [[4], 'unique']
    elif grouping == 'May':
        calc_grouping = [[5], 'unique']
    elif grouping == 'Jun':
        calc_grouping = [[6], 'unique']
    elif grouping == 'Jul':
        calc_grouping = [[7], 'unique']
    elif grouping == 'Aug':
        calc_grouping = [[8], 'unique']
    elif grouping == 'Sep':
        calc_grouping = [[9], 'unique']
    elif grouping == 'Oct':
        calc_grouping = [[10], 'unique']
    elif grouping == 'Nov':
        calc_grouping = [[11], 'unique']
    elif grouping == 'Dec':
        calc_grouping = [[12], 'unique']
    elif grouping in ['year', 'month']:
        calc_grouping = [grouping]
    else:
        msg = 'Unknown calculation grouping: %s' % grouping
        LOGGER.debug(msg)
        raise Exception(msg)
    return calc_grouping


def get_variable(resource):
    """
    detects processable variable name in netCDF file
    based on ocgis (compare guess_main_variables)

    :param resource: filepath sting or sorted list for netcdf file(s)

    :returns str: variable name
    """
    rds = RequestDataset(resource)
    return rds.variable


def has_variable(resource, variable):
    success = False
    try:
        rd = RequestDataset(uri=resource)
        success = rd.variable == variable
    except Exception:
        LOGGER.exception('has_variable failed.')
        raise
    return success
