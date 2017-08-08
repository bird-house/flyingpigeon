import os
from tempfile import mkstemp

from flyingpigeon import config

import logging
LOGGER = logging.getLogger("PYWPS")


def get_configfile(files,
                   seasoncyc_base=None,
                   seasoncyc_sim=None,
                   timewin=1,
                   varname='slp',
                   seacyc=False,
                   cycsmooth=91,
                   nanalog=20,
                   seasonwin=30,
                   distfun='rms',
                   outformat='.txt',
                   period=["1973-01-01", "2012-12-31"],
                   bbox="-180.0,-90.0,180,90.0",
                   calccor=True,
                   silent=False, ):
    """
    Generates the configuration file for the CASTf90 calculation.

    :param files: input files (reference period and period for analyses)
    :param timewin: number of days the distance is averaged
    :param varname: variable name in input files
    :param seacyc: remove the smoothed seasonal cycle from the input fields (True/False)
    :param cycsmooth: smoothing window for the seasonal cycle in days (should be an odd integer)
    :param nanalog: Number of analogs to detect
    :param distfun: Name of the distance function used to calculate the analogs.
     (Supported values: 'rms' 'mahalanobis', 'S1' (Teweles and wobus), 'cosine' (correlation)
     and - still experimental - 'of' (displacement and amplitude score based on optical flow image distortion)
    :param outformat: file format for output ('txt' or 'nc' (default))
    :param analysis_period: dates for which analogs are desired
    :param period: reference period in which analogs are picked (for netcdf output attributes)
    :param bbox: coordinates for the region to be analysed
    :param calccor: calculate rank correlation for analog fields (True/False)
    :param silent: handling of log file output

    :returns: configuration file
    """
    from datetime import datetime as dt
    from os.path import relpath

    date_stamp = dt.strftime(dt.now(), format='%Y%m%d_%H%M%S')
    LOGGER.info('start configuration file preparation at: %s' % (date_stamp))

    # convert True/False to Fortran syntax
    seacyc = str(seacyc)
    calccor = str(calccor)
    silent = str(silent)

    # write stuff to configuration file
    # NB: if order or format or number changes, need to edit wps_analogs_viewer.py
    # and template_analogviewer.html where these scripts read in the config
    # params
    ip, config_file = mkstemp(dir='.', suffix='.txt')

    config = open(config_file, "w")

    config.write(
        '!Configuration file for CASTf90 analogs processes deployed in flyingpigeon\n')
    config.write('!Created : %s \n' % (date_stamp))
    config.write('!Version : 0.1.5 \n')
    config.write('&FILES \n')
    config.write(' my_files%archivefile = "{file}" \n'.format(
        file=relpath(files[0])))
    config.write(' my_files%simulationfile = "{file}" \n'.format(
        file=relpath(files[1])))
    config.write(' my_files%outputfile = "{file}" \n'.format(
        file=relpath(files[2])))
    config.write('  my_files%seacycfilebase = "{file}" \n'.format(
        file=relpath(seasoncyc_base)))
    config.write(' my_files%seacycfilesim = "{file}" \n'.format(
        file=relpath(seasoncyc_sim)))
    config.write('/ \n')
    config.write('&PARAM \n')
    config.write(' my_params%timewin = {timewin} \n'.format(timewin=timewin))
    config.write(' my_params%varname = "{varname}" \n'.format(varname=varname))
    config.write(' my_params%seacyc = .{seacyc}. \n'.format(
        seacyc=seacyc.upper()))
    config.write(' my_params%cycsmooth = {cycsmooth} \n'.format(
        cycsmooth=cycsmooth))
    config.write(' my_params%nanalog = {nanalog} \n'.format(nanalog=nanalog))
    config.write(' my_params%seasonwin = {seasonwin} \n'.format(
        seasonwin=seasonwin))
    config.write(' my_params%distfun = "{distfun}" \n'.format(distfun=distfun))
    config.write(' my_params%calccor = .{calccor}. \n'.format(
        calccor=calccor.upper()))
    config.write(' my_params%oformat = "{outformat}" \n'.format(
        outformat=outformat))  # ".txt" # ! if equals ".nc"
    config.write(' my_params%silent = .{silent}.\n'.format(
        silent=silent.upper()))
    config.write('/\n')
    config.write('&ATTS\n')
    config.write(' my_atts%simsource = "NCEP" \n')  # model name
    config.write(
        ' my_atts%predictorvar = "{varname}" \n'.format(varname=varname))
    config.write(' my_atts%archisource = "NCEP" \n')
    config.write(' my_atts%archiperiod = "{start},{end}" \n'.format(
        start=period[0], end=period[1]))
    config.write(' my_atts%predictordom = "{bbox}" \n'.format(bbox=bbox))
    config.write('/\n')

    config.close()
    return relpath(config_file)

# def subset(resource=[], bbox='-80,50,22.5,70'):
#   """
#    OBSOLETE
#   Returns a subset.

#   :param resource: netCDF input files of one dataset
#   :param bbox: bounding box

#   :return: subset netCDF file
#   """
#   from tempfile import mkstemp
#   from cdo import Cdo
#   cdo = Cdo()
#   resource.sort()

#   ip, nc_concat = mkstemp(dir='.',suffix='.nc')
#   nc_concat = cdo.cat(input=resource, output=nc_concat)

#   ip, nc_subset = mkstemp(dir='.',suffix='.nc')
#   nc_subset = cdo.sellonlatbox('%s' % bbox, input=nc_concat, output=nc_subset)
#   LOGGER.info('subset done: %s ' % nc_subset)

#   return nc_subset


def seacyc(archive, simulation, method='base'):
    """
    Subtracts the seasonal cycle.

    :param archive: netCDF file containing the reference period
    :param simulation: netCDF file containing the period to be analysed
    :param method: method to generate the seasonal cycle files
                   base = seasonal cycle generated from reference period
                   sim = seasonal cycle generated from period to be analysed
                   own = seasonal cycle generated for both time windows

    :return [str,str]: two netCDF filenames for analysis and reference period (located in working directory)
    """
    try:
        LOGGER.debug('seacyc started with method: %s' % method)

        from shutil import copy
        from flyingpigeon.ocgis_module import call
        from flyingpigeon.utils import get_variable
        from cdo import Cdo
        cdo = Cdo()

        if method == 'base':
            seasoncyc_base = cdo.ydaymean(
                input=archive, output='seasoncyc_base.nc')
            variable = get_variable(archive)
            # seasoncyc_base = call(resource=archive,
            # variable=variable,
            # prefix='seasoncyc_base',
            # calc=[{'func': 'mean', 'name': variable}],
            # calc_grouping=['day','month'] )

            LOGGER.debug('seasoncyc_base calculated : %s' % seasoncyc_base)
            cdo.ydaymean(input=archive, output='seasoncyc_base.nc')
            seasoncyc_sim = 'seasoncyc_sim.nc'
            copy(seasoncyc_base, seasoncyc_sim)
        elif method == 'sim':
            # seasoncyc_sim  = call(resource=archive,
            # variable=variable,
            # prefix='seasoncyc_sim',
            # calc=[{'func': 'mean', 'name': variable}],
            # calc_grouping=['day','month'] )
            cdo.ydaymean(input=simulation, output='seasoncyc_sim.nc')
            seasoncyc_base = 'seasoncyc_base.nc'
            copy(seasoncyc_sim, seasoncyc_base)
        elif method == 'own':
            # seasoncyc_base = call(resource=archive,
            # variable=variable,
            # prefix='seasoncyc_base',
            # calc=[{'func': 'mean', 'name': variable}],
            # calc_grouping=['day','month'] )
            seasoncyc_base = cdo.ydaymean(
                input=archive, output='seasoncyc_base.nc')
            # seasoncyc_sim  = call(resource=archive,
            # variable=variable,
            # prefix='seasoncyc_sim',
            # calc=[{'func': 'mean', 'name': variable}],
            # calc_grouping=['day','month'] )
            seasoncyc_sim = cdo.ydaymean(
                input=simulation, output='seasoncyc_sim.nc')
        else:
            raise Exception('normalisation method not found')

    except Exception:
        msg = 'seacyc function failed:'
        LOGGER.exception(msg)
        raise Exception(msg)

    return seasoncyc_base, seasoncyc_sim


def config_edits(configfile):
    """
    Edits the CASTf90 configuration file. Removes filepaths.

    :param configfile: configfile name with its path

    :return str: modified_configfile name
    """
    try:

        # Read in the file
        filedata = None
        with open(configfile, 'r') as file:
            filedata = file.read()

        # Replace the target string
        filedata = filedata.replace(
            '/home/scratch01/sradanov/A2C2/NCEP/', '').replace('/home/estimr2/sradanov/Operational/', '')

        # Write the file out again
        with open(configfile, 'w') as file:
            file.write(filedata)

        LOGGER.info('configfile modified')
    except Exception:
        LOGGER.exeption('Failed to modify configfile:')

    return configfile


def reformat_analogs(analogs):
    """
    Reformats analogs results file for analogues viewer code.

    :param analogs: output from analog_detection process

    :return str: reformatted analogs file for analogues viewer
    """
    import numpy as np
    import pandas as pd

    try:
        num_cols = 3  # dateAnlg, Dis, Corr

        # Create dataframe and read in output csv file of analogs process
        dfS = pd.DataFrame()
        dfS = pd.read_csv(analogs, delimiter=r"\s+", index_col=0)

        # Find number of analogues
        num_analogues = (dfS.shape[1]) / 3
        # LOGGER.debug('num_analogues: %s', num_analogues)

        # Define temporary df
        df_anlg = dfS.iloc[:, 0:num_analogues]  # store only anlg dates
        df_dis = dfS.iloc[:, num_analogues:2 * num_analogues]  # store only dis
        df_corr = dfS.iloc[:, 2 * num_analogues:3 *
                           num_analogues]  # store only corr

        # remove index name before stacking
        df_anlg.index.name = ""
        df_dis.index.name = ""
        df_corr.index.name = ""

        dateStack = df_anlg.stack()
        disStack = df_dis.stack().abs()  # raw values < 0 so take abs
        corrStack = df_corr.stack()

        # Create df of correct dimensions (n x num_cols) using dfS
        df_all = dfS.iloc[:, 0:num_cols]  # NB data are placeholders
        # Rename cols
        df_all.columns = ['dateAnlg', 'Dis', 'Corr']
        # Replicate each row 20 times (for dcjs format)
        df_all = df_all.loc[np.repeat(df_all.index.values, num_analogues)]
        # Replace data placeholders with correct values
        df_all['dateAnlg'] = list(dateStack)
        df_all['Dis'] = list(disStack)
        df_all['Corr'] = list(corrStack)
        # Name index col
        df_all.index.name = 'dateRef'

        # save to tsv file
        ip, analogs_mod = mkstemp(
            suffix='.tsv', prefix='modified-analogfile', text=False)
        df_all.to_csv(analogs_mod, sep='\t')
        LOGGER.info('successfully reformatted analog file')
    except Exception:
        msg = 'failed to reformat analog file'
        LOGGER.exception(msg)
    return analogs_mod


def get_viewer_configfile(analogs):
    """
    finds or generates configuration file for an analogs file
    to be used by the analogs viewer. The configuration file
    will be copied into the output folder.

    :param analogs: text file containing the analogs values
    :return str: configuration file path/name.txt
    """
    import requests
    from shutil import copyfile

    try:
        # Config file with path (server URL address)
        configfile = analogs.replace('analogs-', 'config-')
        LOGGER.info('config filename generated %s', configfile)

        configfile_with_path = os.path.join(config.output_url(), configfile)
        LOGGER.debug('configfile_with_path: %s', configfile_with_path)

        # Check if config file exists
        r = requests.get(configfile_with_path)
        if r.status_code != 404:
            LOGGER.debug('Config file exists on server URL address.')

        else:
            LOGGER.debug(
                'Config file does not exist on server address. Check local disk.')

            # Make config file name and get its path on local disk
            configfile = 'config_' + analogs
            LOGGER.debug('local disk configfile: %s', configfile)

            p, name = os.path.split(path.realpath(analogs))
            configfile_localAddress = os.path.join(p, configfile)
            LOGGER.debug('local disk configfile_localAddress: %s',
                         configfile_localAddress)

            # Check if config file exists
            if os.path.isfile(configfile_localAddress):
                LOGGER.debug('Config file exists on local disk.')

                # Copy config file to output_path
                # (~/birdhouse/var/lib/pywps/outputs/flyingpigeon)
                configfile_outputlocation = os.path.join(config.output_path(), configfile)

                copyfile(configfile_localAddress, configfile_outputlocation)
                LOGGER.info(' time for coffee ')

                configfile_outputlocation_edited = config_edits(
                    configfile_outputlocation)
                LOGGER.info('outputlocation_edited: %s',
                            configfile_outputlocation_edited)

                configfile = os.path.basename(configfile_outputlocation_edited)
                LOGGER.info('  configfile %s  ', configfile)

            else:
                LOGGER.debug(
                    'There is no config file on local disk. Generating a default one.')

                # Insert analogs filename into config file.
                # The rest of the params are unknown.
                configfile_wkdir = get_configfile(
                    files=['dummyconfig', 'dummyconfig', analogs],
                    nanalog='DUMMY!!!',
                    varname='DUMMY!!!',
                    seacyc='DUMMY!!!',
                    cycsmooth='DUMMY!!!',
                    timewin='DUMMY!!!',
                    seasonwin='DUMMY!!!',
                    distfun='DUMMY!!!',
                    calccor='DUMMY!!!',
                    outformat='DUMMY!!!',
                    silent='DUMMY!!!',
                    period=['dummy', 'dummy'],
                    bbox='DUMMY!!!'
                )

                configfile = os.path.basename(configfile_wkdir)  # just file name
                # Add server path to file name
                configfile_inplace = os.path.join(config.output_path(), configfile)
                # Copy out of local working dir to output_path
                copyfile(configfile_wkdir, configfile_inplace)

    except Exception:
        msg = 'failed to read number of analogues from config file'
        LOGGER.exception(msg)
    return configfile


def get_viewer(configfile, datafile, filename=None):
    """
    Generate an analogs viewer based on a template.

    :param datafile: modified analogs file (output of reformat_analogs)
    :param configfile: configuration file

    return html: analog viewer html page
    """
    from flyingpigeon import templating

    filename = filename or 'analogviewer.html'
    with open(filename, 'w') as fp:
        import os
        # Insert reformatted analogue file and config file into analogviewer template
        fp.write(
            templating.render_template(
                'analogviewer.html',
                configfile=configfile,
                datafile=datafile,
                home_url=filename,
                static_url=config.output_url() + '/static'))
        destination = os.path.join(config.output_path(), 'static')
        if not os.path.exists(destination):
            os.symlink(config.static_path(), destination)
    return filename
