import logging
logger = logging.getLogger(__name__)
from tempfile import mkstemp


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
    logger.info('start configuration file preparation at: %s' % (date_stamp))

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
#   logger.info('subset done: %s ' % nc_subset)

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
        logger.debug('seacyc started with method: %s' % method)

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
            #calc=[{'func': 'mean', 'name': variable}],
            # calc_grouping=['day','month'] )

            logger.debug('seasoncyc_base calculated : %s' % seasoncyc_base)
            cdo.ydaymean(input=archive, output='seasoncyc_base.nc')
            seasoncyc_sim = 'seasoncyc_sim.nc'
            copy(seasoncyc_base, seasoncyc_sim)
        elif method == 'sim':
            # seasoncyc_sim  = call(resource=archive,
              # variable=variable,
              # prefix='seasoncyc_sim',
              #calc=[{'func': 'mean', 'name': variable}],
              # calc_grouping=['day','month'] )
            cdo.ydaymean(input=simulation, output='seasoncyc_sim.nc')
            seasoncyc_base = 'seasoncyc_base.nc'
            copy(seasoncyc_sim, seasoncyc_base)
        elif method == 'own':
            # seasoncyc_base = call(resource=archive,
              # variable=variable,
              # prefix='seasoncyc_base',
              #calc=[{'func': 'mean', 'name': variable}],
              # calc_grouping=['day','month'] )
            seasoncyc_base = cdo.ydaymean(
                input=archive, output='seasoncyc_base.nc')
            # seasoncyc_sim  = call(resource=archive,
            # variable=variable,
            # prefix='seasoncyc_sim',
            #calc=[{'func': 'mean', 'name': variable}],
            # calc_grouping=['day','month'] )
            seasoncyc_sim = cdo.ydaymean(
                input=simulation, output='seasoncyc_sim.nc')
        else:
            raise Exception('normalisation method not found')

    except Exception as e:
        msg = 'seacyc function failed : %s ' % e
        logger.debug(msg)
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

        logger.info('configfile modified')
    except Exception as e:
        logger.debug('Failed to modify configfile: %s ' % e)

    return configfile


def refomat_analogs(analogs):
    """
    Reformats analogs results file for analogues viewer code.

    :param analogs: output from analog_detection process

    :return str: reformatted analogs file for analogues viewer
    """

    from flyingpigeon import config
    import numpy as np
    import pandas as pd

    try:

        num_cols = 3  # dateAnlg, Dis, Corr

        # Create dataframe and read in output csv file of analogs process
        dfS = pd.DataFrame()
        dfS = pd.read_csv(analogs, delimiter=r"\s+", index_col=0)

        # Find number of analogues
        num_analogues = (dfS.shape[1]) / 3
        logger.debug('num_analogues: %s ' % num_analogues)

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
        output_path = config.output_path()
        ip, analogs_mod = mkstemp(
            suffix='.tsv', prefix='modified-analogfile', dir=output_path, text=False)
        df_all.to_csv(analogs_mod, sep='\t')
        logger.info('successfully reformatted analog file')

    except Exception as e:
        msg = 'failed to reformat analog file %s ' % e
        logger.debug(msg)

    return analogs_mod


def get_viewer_configfile(analogs):
    """
    finds or generates configuration file for an analogs file 
    to be used by the analogs viewer. The configuration file
    will be copied into the output folder.

    :param analogs: text file containing the analogs values
    :return str: configuration file path/name.txt
    """
    from flyingpigeon import config
    from flyingpigeon.config import www_url

    import requests
    from shutil import copyfile
    from tempfile import mkstemp
    from os import path
    from os.path import basename

    try:
        outputUrl_path = config.outputUrl_path()
        output_path = config.output_path()

        # Config file with path (server URL address)
        configfile = analogs.replace('analogs-', 'config-')
        logger.info('config filename generated %s' % configfile)

        configfile_with_path = path.join(outputUrl_path, configfile)
        logger.debug('configfile_with_path: %s' % configfile_with_path)

        # Check if config file exists
        r = requests.get(configfile_with_path)
        if r.status_code != 404:
            logger.debug('Config file exists on server URL address.')

        else:
            logger.debug(
                'Config file does not exist on server address. Check local disk.')

            # Make config file name and get its path on local disk
            configfile = 'config_' + analogs
            logger.debug('local disk configfile: %s' % configfile)

            p, name = path.split(path.realpath(analogs))
            configfile_localAddress = path.join(p, configfile)
            logger.debug('local disk configfile_localAddress: %s' %
                         configfile_localAddress)

            # Check if config file exists
            if path.isfile(configfile_localAddress):
                logger.debug('Config file exists on local disk.')

                # Copy config file to output_path
                # (~/birdhouse/var/lib/pywps/outputs/flyingpigeon)
                configfile_outputlocation = path.join(output_path, configfile)

                copyfile(configfile_localAddress, configfile_outputlocation)
                logger.info(' time for coffee ')

                configfile_outputlocation_edited = config_edits(
                    configfile_outputlocation)
                logger.info('outputlocation_edited: %s' %
                            configfile_outputlocation_edited)

                configfile = path.basename(configfile_outputlocation_edited)
                logger.info('  configfile %s  ' % configfile)

            else:
                logger.debug(
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

                configfile = path.basename(configfile_wkdir)  # just file name
                # Add server path to file name
                configfile_inplace = path.join(output_path, configfile)
                # Copy out of local working dir to output_path
                copyfile(configfile_wkdir, configfile_inplace)

    except Exception as e:
        msg = 'failed to read number of analogues from config file %s ' % e
        logger.debug(msg)
    return configfile


def copy_configfile(configfile):
    """
    copy configuration file into output folder

    :param configfile: configuration file (path/to/file.txt) in working dir

    :return str,str: output_path, output_url 
    """
    from flyingpigeon import config

    from shutil import copyfile
    from os import path

    outputUrl_path = config.outputUrl_path()
    output_path = config.output_path()

    # configfile = path.basename(configfile) #just file name
    # Add server path to file name
    config_output_path = path.join(output_path, path.basename(configfile))
    # Copy out of local working dir to output_path
    copyfile(configfile, config_output_path)
    config_output_url = path.join(outputUrl_path, configfile)

    return config_output_path, config_output_url


def get_viewer(analogs_mod, configfile):
    """
    Generate an analogs viewer based on a template.

    :param analogs_mod: modified analogs file (output of refomat_analogs)
    :param configfile: configuration file 

    return html: analog viewer html page
    """

    from os.path import basename
    from flyingpigeon.config import JSsrc_dir
    tmpl = JSsrc_dir() + '/template_analogviewer.html'

    ip, output_av = mkstemp(
        suffix='.html', prefix='analogviewer', dir='.', text=False)

    tmpl_file = open(tmpl).read()

    viewer = open(output_av, 'w')

    # Insert reformatted analogue file and config file into placeholders in
    # the js script
    tmpl_file = tmpl_file.replace(
        'analogues_placeholder.json', basename(analogs_mod))
    tmpl_file = tmpl_file.replace(
        'analogues_config_placeholder.txt', configfile)
    viewer.write(tmpl_file)
    viewer.close()

    return viewer