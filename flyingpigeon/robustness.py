# from flyingpigeon.visualisation import map_robustness
from flyingpigeon.utils import get_variable, sort_by_filename, get_timerange, get_calendar

import logging
LOGGER = logging.getLogger("PYWPS")


def signal_noise_ratio(resource=[], start=None, end=None, timeslice=20,
                       variable=None, title=None, cmap='seismic'):
    """returns the result

    :param resource: list of paths to netCDF files
    :param start: beginning of reference period (if None (default),
                  the first year of the consistent ensemble will be detected)
    :param end: end of comparison period (if None (default), the last year of the consistent ensemble will be detected)
    :param timeslice: period length for mean calculation of reference and comparison period
    :param variable: OBSOLETE
    :param title: str to be used as title for the signal mal
    :param cmap: define the color scheme for signal map plotting

    :return: signal.nc, low_agreement_mask.nc, high_agreement_mask.nc, text.txt,  #  graphic.png,
    """
    from os.path import split
    from tempfile import mkstemp
    from cdo import Cdo
    cdo = Cdo()
    cdo.forceOutput = True

    # preparing the resource
    try:
        file_dic = sort_by_filename(resource, historical_concatination=True)
        LOGGER.info('file names sorted experimets: %s' % len(file_dic.keys()))
    except:
        msg = 'failed to sort the input files'
        LOGGER.exception(msg)

    # check that all datasets contains the same variable

    try:
        var_name = set()
        for key in file_dic.keys():
            var_name = var_name.union([get_variable(file_dic[key])])
        LOGGER.debug(var_name)
    except:
        LOGGER.exception('failed to get the variable in common')

    if len(var_name) == 1:
        variable = [str(n) for n in var_name][0]
        LOGGER.info('varible %s detected in all members of the ensemble' % variable)
    else:
        raise Exception('none or more than one variables are found in the ensemble members')

    # TODO: drop missfitting grids

    # timemerge for seperate datasets
    try:
        mergefiles = []
        for key in file_dic.keys():
            # if variable is None:
            #     variable = get_variable(file_dic[key])
            #     LOGGER.info('variable detected %s ' % variable)
            try:
                if type(file_dic[key]) == list and len(file_dic[key]) > 1:
                    _, nc_merge = mkstemp(dir='.', suffix='.nc')
                    mergefiles.append(cdo.mergetime(input=file_dic[key], output=nc_merge))
                else:
                    mergefiles.extend(file_dic[key])
            except:
                LOGGER.exception('failed to merge files for %s ' % key)
        LOGGER.info('datasets merged %s ' % mergefiles)
    except:
        msg = 'seltime and mergetime failed'
        LOGGER.exception(msg)

    # verify the calendar
    # find the most common calendar
    cals = []
    n = 0
    for nc in mergefiles:
        cal, util = get_calendar(nc)
        cals.append(cal)
    for cal in cals:
        m = cals.count(cal)
        if m > n:
            calendar = cal

    for c, nc in enumerate(mergefiles):
        cal, unit = get_calendar(nc)
        print 'calendar detected: %s most common: %s' % (cal, calendar)
        if cal != calendar:
            print 'calendar changed for %s to %s' % (cal, calendar)
            _, nc_cal = mkstemp(dir='.', suffix='.nc')
            nc_out = cdo.setcalendar('{0}'.format(calendar), input=nc, output=nc_cal)
            mergefiles[c] = nc_cal
            LOGGER.debug('calendar changed for %s' % nc)
        else:
            LOGGER.debug('calendar was %s' % cal)

    # dataset documentation
    try:
        text_src = open('infiles.txt', 'a')
        for key in file_dic.keys():
            text_src.write(key + '\n')
        text_src.close()
    except:
        msg = 'failed to write source textfile'
        LOGGER.exception(msg)
        _, text_src = mkstemp(dir='.', suffix='.txt')

    # evaluation
    # configure reference and compare period
    st = set()
    en = set()

    for key in file_dic.keys():
        #  TODO: convert 360day calendar

        s, e = get_timerange(file_dic[key])
        st.update([s])
        en.update([e])

    if start is None:
        start = list(st)[-1]
    else:
        if start < list(st)[-1]:
            start = list(st)[-1]
            LOGGER.debug('start was befor the first common timestep, set start to the first common timestep')

    if end is None:
        end = list(en)[0]
    else:
        if end > list(en)[0]:
            end = list(en)[0]
            LOGGER.debug('end was after the last common timestepp, set end to last common timestep ')

    from datetime import datetime as dt
    from datetime import timedelta

    start = dt.strptime(start, '%Y%M%d')
    end = dt.strptime(end, '%Y%M%d')
    length = end - start

    # set the periodes:
    try:
        if timeslice is None:
            td = lenth / 3
        else:
            td = timedelta(days=timeslice)
            if td > length:
                td = lenth / 3
                LOGGER.debug('timeslice is larger as whole timeseries! set timeslice to third of timeseries')

        start_td = start + td
        end_td = end - td
        LOGGER.info('timeslice and periodes set')
    except:
        msg = 'failed to set the periodes'
        LOGGER.exception(msg)

    try:
        files = []
        for i, mf in enumerate(mergefiles):
            files.append(cdo.selyear('{0}/{1}'.format(start.year, end.year), input=[mf.replace(' ', '\ ')],
                         output='file_{0}_.nc'.format(i)))  # python version
        LOGGER.info('timeseries selected from defined start to end year')
    except:
        msg = 'seltime and mergetime failed'
        LOGGER.exception(msg)

    try:
        # ensemble mean
        nc_ensmean = cdo.ensmean(input=files, output='nc_ensmean.nc')
        LOGGER.info('ensemble mean calculation done')
    except:
        msg = 'ensemble mean failed'
        LOGGER.exception(msg)

    try:
        # ensemble std
        nc_ensstd = cdo.ensstd(input=files, output='nc_ensstd.nc')
        LOGGER.info('ensemble std and calculation done')
    except:
        msg = 'ensemble std or failed'
        LOGGER.exception(msg)

    # get the get the signal as difference from the beginning (first years) and end period (last years), :
    try:
        selyearstart = cdo.selyear('%s/%s' % (start.year, start_td.year), input=nc_ensmean, output='selyearstart.nc')
        selyearend = cdo.selyear('%s/%s' % (end_td.year, end.year), input=nc_ensmean, output='selyearend.nc')
        meanyearst = cdo.timmean(input=selyearstart, output='meanyearst.nc')
        meanyearend = cdo.timmean(input=selyearend, output='meanyearend.nc')
        signal = cdo.sub(input=[meanyearend, meanyearst], output='signal.nc')
        LOGGER.info('Signal calculation done')
    except:
        msg = 'calculation of signal failed'
        LOGGER.exception(msg)
        _, signal = mkstemp(dir='.', suffix='.nc')

    # get the intermodel standard deviation (mean over whole period)
    try:
        # std_selyear = cdo.selyear('%s/%s' % (end1,end2), input=nc_ensstd, output='std_selyear.nc')
        # std = cdo.timmean(input = std_selyear, output = 'std.nc')

        std = cdo.timmean(input=nc_ensstd, output='std.nc')
        std2 = cdo.mulc('2', input=std, output='std2.nc')
        LOGGER.info('calculation of internal model std for time period done')
    except:
        msg = 'calculation of internal model std failed'
        LOGGER.exception(msg)
    try:
        # absolut = cdo.abs(input=signal, output='absolut_signal.nc')  # don't get the sence of this step :-)

        high_agreement_mask = cdo.gt(input=[signal, std2],  output='signal_larger_than_noise.nc')
        low_agreement_mask = cdo.lt(input=[signal, std], output='signal_smaller_than_noise.nc')
        LOGGER.info('high and low mask done')
    except:
        msg = 'calculation of robustness mask failed'
        LOGGER.exception(msg)
        _, high_agreement_mask = mkstemp(dir='.', suffix='.nc')
        _, low_agreement_mask = mkstemp(dir='.', suffix='.nc')

    return signal, low_agreement_mask, high_agreement_mask, text_src
    #   nc_ensmean, nc_ensstd, selyearstart, selyearend
    #   # graphic,
