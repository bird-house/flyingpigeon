import os

from matplotlib import use
use('Agg')
from matplotlib import pyplot as plt
from matplotlib.cm import get_cmap

import cartopy.crs as ccrs

import numpy as np

#from eggshell.ocg.utils import call
from eggshell.nc.utils import get_variable, get_time
from eggshell.viz.visualisation import pdfmerge

from flyingpigeon import templating
from flyingpigeon.utils import prepare_static_folder

import logging
LOGGER = logging.getLogger("PYWPS")

def get_time_nc(nc_file, tv='time'):
    """
    returns all timestamps of given netcdf file as datetime list.

    :param nc_file: NetCDF file(s)
    :param tv: name of temporal dimension
    :return format: netcdftime._datetime.datetime
    """
    from netCDF4 import MFDataset,num2date

    ds = MFDataset(nc_file)
    try:
        time = ds.variables[tv]
    except:
        tv='time_counter'
    ds.close()

    try:
        ds = MFDataset(nc_file)
        time = ds.variables[tv]
        if (hasattr(time , 'units') and hasattr(time , 'calendar')) == True:
            timestamps = num2date(time[:], time.units , time.calendar)
        elif hasattr(time , 'units'):
            timestamps = num2date(time[:], time.units)
        else:
            timestamps = num2date(time[:])
        ds.close()
    except Exception as e:
        raise Exception
    return timestamps

def pdf_from_analog(lon, lat, data, vmin, vmax, Nlin=30, domain=[-80,50,20,70], output='ana_map.pdf', title='Analogs'):
    fig = plt.figure()
    fig.set_size_inches(18.5, 10.5, forward=True)

    ax = plt.axes(projection=ccrs.PlateCarree())
    ax.set_extent(domain, crs=ccrs.PlateCarree())
    ax.coastlines(linewidth=0.8)
    ax.gridlines()

    levels = np.linspace(vmin, vmax, Nlin)

    cmap=get_cmap("RdBu_r")

    data_map = ax.contourf(lon, lat, data, levels=levels, extend='both', cmap=cmap, projection=ccrs.PlateCarree())
    data_cbar = plt.colorbar(data_map, extend='both', shrink=0.6)
    data_cont = ax.contour(lon, lat, data ,levels=levels, linewidths=0.8, colors="white", linestyles = 'dashed', projection=ccrs.PlateCarree())

    plt.clabel(data_cont, inline=1, fmt='%1.0f')
    plt.title(title)
    plt.tight_layout()

    pdffilename = output
    plt.savefig(pdffilename)
    fig.clf()
    plt.close(fig)
    return pdffilename

def get_configfile(files,
                   seasoncyc_base=None,
                   seasoncyc_sim=None,
                   base_id='NCEP',
                   sim_id='NCEP',
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
                   silent=False):
    """
    Generates the configuration file for the CASTf90 calculation.

    TODO: use jjinja template

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
    config_file = "config.txt"

    config = open(config_file, "w")

    config.write(
        '!Configuration file for CASTf90 analogs processes deployed in flyingpigeon\n')
    config.write('!Created : %s \n' % (date_stamp))
    config.write('!Version : 0.1.5 \n')
    config.write('&FILES \n')
    config.write(' my_files%archivefile = "{file}" \n'.format(
        file=os.path.relpath(files[0])))
    config.write(' my_files%simulationfile = "{file}" \n'.format(
        file=os.path.relpath(files[1])))
    config.write(' my_files%outputfile = "{file}" \n'.format(
        file=os.path.relpath(files[2])))

    if seacyc is not 'False':
        config.write(' my_files%seacycfilebase = "{file}" \n'.format(
            file=os.path.relpath(seasoncyc_base)))
        config.write(' my_files%seacycfilesim = "{file}" \n'.format(
            file=os.path.relpath(seasoncyc_sim)))

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
    config.write(' my_atts%simsource = "{sim_id}" \n'.format(sim_id=sim_id))  # model name
    config.write(
        ' my_atts%predictorvar = "{varname}" \n'.format(varname=varname))
    config.write(' my_atts%archisource = "{base_id}" \n'.format(base_id=base_id))
    config.write(' my_atts%archiperiod = "{start},{end}" \n'.format(
        start=period[0], end=period[1]))
    config.write(' my_atts%predictordom = "{bbox}" \n'.format(bbox=bbox))
    config.write('/\n')

    config.close()
    return config_file

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


def reformat_analogs(analogs, prefix = 'modified-analogfile.tsv'):
    """
    Reformats analogs results file for analogues viewer code.

    :param analogs: output from analog_detection process

    :return str: reformatted analogs file for analogues viewer
    """
    # import numpy as np
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
        analogs_mod = prefix
        df_all.to_csv(analogs_mod, sep='\t')
        LOGGER.info('successfully reformatted analog file')
    except Exception:
        msg = 'failed to reformat analog file'
        LOGGER.exception(msg)
        raise Exception(msg)
    return analogs_mod


def render_viewer(configfile, datafile):
    """
    Generate an analogs viewer HTML page based on a template.

    :param datafile: modified analogs file (output of reformat_analogs)
    :param configfile: configuration file

    return html: analog viewer html page
    """
    try:
        page = 'analogviewer.html'
        with open(page, 'w') as fp:
            fp.write(templating.render_template(
                page,
                configfile=configfile,
                datafile=datafile,
                # static_url=config.output_url() + '/static'))
                static_url='../static'))
            prepare_static_folder()
    except Exception:
        msg = "Failed to render analogviewer."
        LOGGER.exception(msg)
        raise Exception(msg)
    else:
        return page

def plot_analogs(configfile='config.txt', simday='all', **kwargs):
    """
    """

    from datetime import datetime as dt
    from netCDF4 import Dataset

    simoutpdf='Analogs.pdf'

    if (os.path.isfile(configfile) == True):
        curdir, confile = os.path.split(os.path.abspath(configfile))

        lines=[line.rstrip('\n') for line in open(configfile)]
        for i in lines:
            if 'archivefile' in i: arcfile = i.split('"')[1]
            if 'simulationfile' in i: simfile = i.split('"')[1]
            if 'outputfile' in i: analogfile = i.split('"')[1]
            if 'nanalog' in i : nanalog = int(i.split(' =')[1])
            if 'varname' in i: varname = i.split('"')[1]
            if 'predictordom' in i: domain = i.split('"')[1]

        analogfile = curdir +'/' + analogfile
        arcfile = curdir + '/' + arcfile
        simfile = curdir + '/' + simfile

        try:
            arc_times = get_time(arcfile)
            sim_times = get_time(simfile)
        except:
            arc_times = get_time_nc(arcfile)
            sim_times = get_time_nc(simfile)

        sim_dataset = Dataset(simfile)
        simvar = sim_dataset.variables[varname][:]
        # TODO: check other names for lat/lon
        lon = sim_dataset.variables['lon'][:]
        lat = sim_dataset.variables['lat'][:]
        sim_dataset.close()

        domain = domain.split(",")

        try:
            domain = [float(i) for i in domain]
        except:
            domain = [lon[0],lon[-1],lat[-1],lat[0]]

        outlist = []
        total_simmin = np.min(simvar)
        total_simmax = np.max(simvar)

        cont=[line.rstrip('\n') for line in open(analogfile)]

        Nlin = 30

        for idx, item in enumerate(cont[1:]):
            ana = item.split()
            sim_date = dt.strptime(ana[0],'%Y%m%d')
            an_dates = []
            for dat in ana[1:1+nanalog]: an_dates.append(dt.strptime(dat,'%Y%m%d'))

            c_dists = ana[1+nanalog:1+2*nanalog]
            c_cors = ana[1+2*nanalog:]
            dists = np.zeros((nanalog),dtype=float)
            cors = np.zeros((nanalog),dtype=float)

            for i in range(0,nanalog):
                dists[i] = float(c_dists[i])
                cors[i] = float(c_cors[i])
            min_dist = np.min(dists)
            max_corr = np.max(cors)
            w_dist = min_dist/dists
            w_corr = cors/max_corr

            sim_index = idx # day by day

            tmp_i = []
            for i in arc_times:
                tmp_z = '%s-%s-%s' % (i.year, i.month, i.day)
                tmp_i.append(tmp_z)

            arc_index = []

            for arc in an_dates:
                arc_date_temp = '%s-%s-%s' % (arc.year, arc.month, arc.day)
                arc_index.append(tmp_i.index(arc_date_temp))

            simmin = np.min(simvar[sim_index,:,:])
            simmax = np.max(simvar[sim_index,:,:])

            # PLOT SIM ====================================
            sim_title = 'Simulation Day: ' + ana[0]
            output_file_name = 'sim_' + ana[0] + '.pdf'
            output_file = pdf_from_analog(lon=lon, lat=lat, data=simvar[sim_index,:,:],
                                  vmin=simmin, vmax=simmax, Nlin=Nlin, domain=domain,
                                  output=output_file_name, title=sim_title)
            outlist.append(str(output_file))


            # PLOT Mean analogs ====================================

            arc_dataset = Dataset(arcfile)
            arcvar = arc_dataset.variables[varname][:]
            arc_dataset.close()

            mean_ana = np.zeros((len(arcvar[0,:,0]),len(arcvar[0,0,:])),dtype=float)
            for ida, art in enumerate(arc_index): mean_ana = mean_ana + arcvar[art,:,:]
            mean_ana = mean_ana / nanalog

            output_an_file_name = 'ana_' + ana[0] + '.pdf'
            an_title = 'Mean analogs for sim Day: ' + ana[0]
            an_output_file = pdf_from_analog(lon=lon, lat=lat, data=mean_ana,
                                          vmin=simmin, vmax=simmax, Nlin=Nlin, domain=domain,
                                          output=output_an_file_name, title=an_title)
            outlist.append(str(an_output_file))

            # PLOT BEST (first) analog

            output_ban_file_name = 'bana_' + ana[0] + '.pdf' #PDF!!
            ban_title = 'BEST analog for sim Day ' + ana[0] + ' is: ' + ana[1]
            ban_output_file = pdf_from_analog(lon=lon, lat=lat, data=arcvar[arc_index[0]],
                                          vmin=simmin, vmax=simmax, Nlin=Nlin, domain=domain,
                                          output=output_ban_file_name, title=ban_title)
            outlist.append(str(ban_output_file))


            # PLOT WORST (last) analog

            output_wan_file_name = 'wana_' + ana[0] + '.pdf' #PDF!!
            wan_title = 'LAST analog for sim Day ' + ana[0] + ' is: ' + ana[nanalog]
            wan_output_file = pdf_from_analog(lon=lon, lat=lat, data=arcvar[arc_index[-1]],
                                          vmin=simmin, vmax=simmax, Nlin=Nlin, domain=domain,
                                          output=output_wan_file_name, title=wan_title)
            outlist.append(str(wan_output_file))

            # PLOT Max and Min correl analog

            min_c_index = np.argmin(cors)
            max_c_index = np.argmax(cors)

            output_bcan_file_name = 'bcana_' + ana[0] + '.pdf' #PDF!!
            bcan_title = 'Analog with max corr for sim Day ' + ana[0] + ' is: ' + ana[1+max_c_index]
            bcan_output_file = pdf_from_analog(lon=lon, lat=lat, data=arcvar[arc_index[max_c_index]],
                                          vmin=simmin, vmax=simmax, Nlin=Nlin, domain=domain,
                                          output=output_bcan_file_name, title=bcan_title)
            outlist.append(str(bcan_output_file))

            output_wcan_file_name = 'wcana_' + ana[0] + '.pdf' #PDF!!
            wcan_title = 'Analog with min corr for sim Day ' + ana[0] + ' is: ' + ana[1+min_c_index]
            wcan_output_file = pdf_from_analog(lon=lon, lat=lat, data=arcvar[arc_index[min_c_index]],
                                          vmin=simmin, vmax=simmax, Nlin=Nlin, domain=domain,
                                          output=output_wcan_file_name, title=wcan_title)
            outlist.append(str(wcan_output_file))

            """    
            # PLOT analogs dist weighted ====================================
            mean_ana = np.zeros((len(arcvar[0,:,0]),len(arcvar[0,0,:])),dtype=float)
            for ida, art in enumerate(arc_index):
                mean_ana=mean_ana+arcvar[art,:,:]*w_dist[ida]
    
            mean_ana = mean_ana/sum(w_dist)
    
            # PLOT analogs corr weighted ====================================
            mean_ana = np.zeros((len(arcvar[0,:,0]),len(arcvar[0,0,:])),dtype=float)
            for ida, art in enumerate(arc_index):
                mean_ana=mean_ana+arcvar[art,:,:]*w_corr[ida]
    
            mean_ana = mean_ana/sum(w_corr)
   
            """
        simoutpdf = pdfmerge(outlist)
    # get the information from config file:
    # netcdf files, period, Nanalogs, ouput analogs

    # get the information from config file:
    # netcdf files, period, Nanalogs, ouput analogs
    else:
        simoutpdf = 'Analogs.pdf'
        # TODO: call this func with analogfile = '..',
        # arguments came from kwargs
        # need to prescribe all input info - to use with external analogs results.
        # check kwargs: ncfiles, N analogs (?), periods, etc
    return simoutpdf
