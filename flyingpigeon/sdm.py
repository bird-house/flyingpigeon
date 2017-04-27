from tempfile import mkstemp

import logging
LOGGER = logging.getLogger("PYWPS")

_SDMINDICES_ = [
    'CDD_AMJJAS',
    'CFD_ONDJFM',
    'FD_ONDJFM', 'FD_Apr', 'FD_May', 'FD_Jun',
    'GD4_yr',
    'ID_yr',
    'PRCPTOT_yr', 'PRCPTOT_ONDJFM', 'PRCPTOT_AMJJAS',
    'PRCPTOT_JJA', 'PRCPTOT_MAM', 'PRCPTOT_JJA',
    'RR1_yr',
    'SU_yr',
    'TG_yr', 'TG_AMJJAS', 'TG_ONDJFM', 'TG_JJA',
    'TNn_yr', 'TNn_AMJJAS', 'TG_ONDJFM', 'TNn_Jan',
]


def get_csv(zip_file_url):
    import requests
    import zipfile
    import StringIO
    r = requests.get(zip_file_url, stream=True)
    z = zipfile.ZipFile(StringIO.StringIO(r.content))
    z.extractall()
    csv = z.namelist()[0]
    return csv


def latlon_gbifdic(gbifdic):
    """
    extracts the coordinates from the fetched gbif dictionay

    :param gbifdic: pygbif dictionary (output from get_gbif data fetch function)

    :return numpy.ndarray: [[lats],[lons]]
    """
    from numpy import empty
    try:
        coords = [{k: v for k, v in w.items() if k.startswith('decimal')} for w in gbifdic]
        latlon = empty([len(coords), 2], dtype=float, order='C')

        for i, coord in enumerate(coords):
            latlon[i][0] = coord['decimalLatitude']
            latlon[i][1] = coord['decimalLongitude']

        LOGGER.info('read in PA coordinates for %s rows ', len(coords))
    except:
        msg = 'failed search GBIF data.'
        LOGGER.exception(msg)
        raise
    return latlon


def latlon_gbifcsv(csvfile):
    """
    extracts the coordinates from a given csv table containing GBIF data

    :param csvfile: path to csv file

    :return list: [[lats],[lons]]
    """
    try:
        import csv
        from collections import defaultdict
        from numpy import empty

        columns = defaultdict(list)

        with open(csvfile, 'rb') as f:
            reader = csv.DictReader(f, delimiter=',')
            for row in reader:
                for (k, v) in row.items():
                    columns[k].append(v)

        dl = len(columns['decimalLongitude'])
        # print 'length of decimalLongitude: %s' % dl
        ll = empty([dl, 2], dtype=float, order='C')
        c = 0
        for i in range(0, dl):
            try:
                ll[i][0] = float(columns['decimalLatitude'][i])
                ll[i][1] = float(columns['decimalLongitude'][i])
            except:
                c = c + 1

        nz = (ll == 0).sum(1)
        latlon = ll[nz == 0, :]
        LOGGER.info('read in PA coordinates for %s rows ' % len(latlon[:, 0]))
        LOGGER.warn('failed to read in PA coordinates for %s rows ' % c)
    except:
        LOGGER.exception('failed to get lat/lon coordinates from csv table')
    return latlon


def get_gbif(taxon_name='Fagus sylvatica', bbox=[-10, -10, 10, 10]):
    """
    fetching species data from GBIF database ( pageing over polygons in Europe )

    :param taxon_name: Taxon name of the species to be searched
                     default='Fagus sylvatica'
    :param bbox: extention of georaphical region to fetch data e.g bbox=[-180,-90,180,90]
    :returns dic: Dictionay of species occurences
    """
    from numpy import arange  # nan, empty,
    from pygbif import occurrences as occ
    from pygbif import species

    LOGGER.info('libs loaded in get_gbif function')

    try:
        nm = species.name_backbone(taxon_name)['usageKey']
        LOGGER.info('taxon name set')
        print('taxon name set')
        # generate polygons with gridwidth 10_degree
        # x_len = (bbox[2] - bbox[0] ) / 10
        # y_len = (bbox[3] - bbox[1] ) / 10
        # LOGGER.info('length = %s , %s ' % (x_len, y_len))
        polys = []
        gridlen = 10

        for x in arange(bbox[0], bbox[2], gridlen):
            for y in arange(bbox[1], bbox[3], gridlen):
                print 'processing %s , %s' % (x, y)
                poly = "POLYGON ((%s %s,%s %s,%s %s,%s %s,%s %s))" % \
                    (x, y, x, y + gridlen, x + gridlen, y + gridlen, x + gridlen, y, x, y)
                polys.extend([poly])
        print(polys)

        LOGGER.info('%s polygons created' % len(polys))
        gbifdic = []

        for i in polys:
            LOGGER.info('processing polyon')
            res = []
            x = occ.search(taxonKey=nm, geometry=i)
            res.append(x['results'])
            while not x['endOfRecords']:
                x = occ.search(taxonKey=nm, geometry=i, offset=sum([len(x) for x in res]))
                res.append(x['results'])
            gbifdic.append([w for z in res for w in z])
            LOGGER.info('polyon fetched')

        results = [w for z in gbifdic for w in z]
    except:
        msg = 'failed search GBIF data.'
        LOGGER.exception(msg)
        raise
    return results

#
#    coords = [ { k: v for k, v in w.items() if k.startswith('decimal') } for w in allres ]#
#
#    latlon = empty([len(coords),2], dtype=float, order='C')
#    for i , coord in enumerate(coords):
#      latlon[i][0] = Latitude
#      latlon[i][1] = Longitude
#    nz = (latlon == 0).sum(1)
#    ll = latlon[nz == 0, :]
#    LOGGER.info('read in PA coordinates for %s rows ' % len(ll[:,0]))


def gbifdic2csv(gbifdic):
    """
    creates a csv file based on gbif a dictionay .

    :param gbifdic: gbif dictionay, returned by get_gbif

    :return csv file: path to csvfile
    """
    from pandas import DataFrame
    from tempfile import mkstemp

    ip, gbif_csv = mkstemp(dir='.', suffix='.csv')
    DataFrame.from_records(gbifdic).to_csv(gbif_csv, mode='a', sep=',', encoding='utf-8')
    return gbif_csv


def get_PAmask(coordinates=[], nc=None):
    """
    generates a matrix with 1/0 values over land areas. (NaN for water regions)

    :param coordinates: 2D array with lat lon coordinates representing tree observation
    :param domain: region (default='EUR-11')

    :return : PAmask
    """
    from scipy import spatial
    import numpy as np
    import numpy.ma as ma

#    from netCDF4 import Dataset
#    from flyingpigeon import config
#    DIR_MASKS = config.masks_dir()

    from flyingpigeon.utils import get_variable
    from flyingpigeon.utils import get_coordinates, get_values  # unrotate_pole,

    lats, lons = np.array(get_coordinates(nc))  # unrotate_pole(nc, write_to_file=False))
    sftlf = get_values(nc)[0, :, :]
    #
    # sftlf[sftlf.mask is True] = 0
    # sftlf[sftlf.mask is False] = np.nan

    domain = sftlf.shape

    lats1D = np.array(lats).ravel()
    lons1D = np.array(lons).ravel()
    tree = spatial.KDTree(zip(lats1D, lons1D))
    l, i = tree.query(coordinates)

    PA = np.zeros(len(lats1D))
    PA[i] = 1
    #
    PAmask = PA.reshape(domain)
    PAmask[sftlf.mask] = np.nan

    return PAmask


def get_indices(resources, indices):
    """
    calculating indices (netCDF files) defined in _SDMINDICES_

    :param resources:
    :param indices: indices defined in _SDMINDICES_

    :return list: list of filepathes to netCDF files
    """

    from flyingpigeon.utils import sort_by_filename, calc_grouping, drs_filename, unrotate_pole
    # from flyingpigeon.ocgis_module import call
    from flyingpigeon.indices import indice_variable, calc_indice_simple

    # names = [drs_filename(nc, skip_timestamp=False, skip_format=False,
    #               variable=None, rename_file=True, add_file_path=True) for nc in resources]

    ncs = sort_by_filename(resources, historical_concatination=True)
    ncs_indices = []
    LOGGER.info('resources sorted found %s datasets' % len(ncs.keys()))
    for key in ncs.keys():
        for indice in indices:
            try:
                name, month = indice.split('_')
                variable = key.split('_')[0]
                # print name, month , variable
                if variable == indice_variable(name):
                    LOGGER.info('calculating indice %s ' % indice)
                    prefix = key.replace(variable, name).replace('_day_', '_%s_' % month)
                    nc = calc_indice_simple(resource=ncs[key],
                                            variable=variable,
                                            polygons=['Europe', 'Africa', 'Asia', 'North America', 'Oceania',
                                                      'South America', 'Antarctica'],
                                            mosaic=True,
                                            prefix=prefix, indices=name, groupings=month)
                    if nc is not None:
                        coords = unrotate_pole(nc[0], write_to_file=True)
                        ncs_indices.append(nc[0])
                        LOGGER.info('Successful calculated indice %s %s' % (key, indice))
                    else:
                        msg = 'failed to calculate indice %s %s' % (key, indice)
                        LOGGER.exception(msg)
            except:
                msg = 'failed to calculate indice %s %s' % (key, indice)
                LOGGER.exception(msg)
                raise
    return ncs_indices


def sort_indices(ncs_indices):
    """
    groups the defining growing conditions indices per dataset
    :param ncs_indices: list of climate indices
    :return dictionary: {'dataset' = [filepathlist]}
    """
    from os.path import basename
    indices_dic = {}

    for indice in ncs_indices:
        f = basename(indice).strip('.nc')
        name = '_'.join(f.split('_')[1:-2])
        indices_dic[name] = []

    for key in indices_dic.keys():
        for path in ncs_indices:
            if key in path:
                indices_dic[key].append(path)
    return indices_dic


def get_reference(ncs_indices, period='all'):
    """
    calculates the netCDF files containing the mean climatology for statistical GAM training

    :param ncs_indices: list of climate indices defining the growing conditions of tree species
    :param refperiod: time period for statistic training

    :return present: present conditions
    """
    from datetime import datetime as dt
    from flyingpigeon.ocgis_module import call
    from flyingpigeon.utils import get_variable
    from os.path import basename

    if not period == 'all':
        s, e = period.split('-')
        start = dt.strptime(s + '-01-01', '%Y-%m-%d')
        end = dt.strptime(e + '-12-31', '%Y-%m-%d')
        time_range = [start, end]
    else:
        time_range = None

    ref_indices = []
    for nc_indice in ncs_indices:
        variable = get_variable(nc_indice)
        f = basename(nc_indice).strip('.nc')
        prefix = '%s_ref-%s' % ('_'.join(f.split('_')[0:-1]), period)

        ref_indices.append(call(resource=nc_indice, variable=variable, prefix=prefix,
                                calc=[{'func': 'mean', 'name': variable}],
                                calc_grouping=['all'], time_range=time_range))

    return ref_indices


def get_gam(ncs_reference, PAmask, modelname=None):
    """
    GAM statistical training based on presence/absence mask and indices

    :param ncs_reference: list of netCDF files containing the indices
    :param PAmask: presence/absence mask as output from get_PAmask
    :param modelname: modelname to be used for potting

    :return gam_model, prediction, infos_concat: Rstatisics,
                                                 occurence predicion based on ncs_reference files,
                                                 graphical visualisation of regression curves
    """

    try:
        from netCDF4 import Dataset
        from os.path import basename
        from numpy import squeeze, ravel, isnan, nan, array, reshape

        from flyingpigeon.utils import get_variable

        from rpy2.robjects.packages import importr
        import rpy2.robjects as ro
        import rpy2.robjects.numpy2ri

        rpy2.robjects.numpy2ri.activate()

        base = importr("base")
        stats = importr("stats")
        mgcv = importr("mgcv")
        LOGGER.info('rpy2 modules imported')
    except:
        msg = 'failed to import rpy2 modules'
        LOGGER.exception(msg)
        raise

    try:
        data = {'PA': ro.FloatVector(ravel(PAmask))}
        domain = PAmask.shape
        LOGGER.info('mask data converted to R float vector')
    except:
        msg = 'failed to convert mask to R vector'
        LOGGER.exception(msg)
        raise Exception

    try:
        form = 'PA ~ '
        ncs_reference.sort()
        for i, nc in enumerate(ncs_reference):
            var = get_variable(nc)
            agg = basename(nc).split('_')[-2]
            ds = Dataset(nc)
            vals = squeeze(ds.variables[var])
            # vals[vals > 1000] = 0
            vals[isnan(PAmask)] = nan
            indice = '%s_%s' % (var, agg)
            data[str(indice)] = ro.FloatVector(ravel(vals))
            if i == 0:
                form = form + 's(%s, k=3)' % indice
            else:
                form = form + ' + s(%s, k=3)' % indice
        LOGGER.info('form string generated for gam model')
    except:
        msg = 'form string generation for gam failed'
        LOGGER.exception(msg)
        # raise Exception

    try:
        dataf = ro.DataFrame(data)
        eq = ro.Formula(str(form))
        gam_model = mgcv.gam(base.eval(eq),
                             data=dataf,
                             family=stats.binomial(),
                             scale=-1,
                             na_action=stats.na_exclude)
        LOGGER.info('GAM model trained')
    except:
        msg = 'failed to train the GAM model'
        LOGGER.exception(msg)

    # ####################
    # plot response curves
    # ####################

    try:
        try:
            from tempfile import mkstemp
            grdevices = importr('grDevices')
            ip, statinfos = mkstemp(dir='.', suffix='.pdf')
            grdevices.pdf(file=statinfos)
            for i in range(1, len(ncs_reference) + 1):
                try:
                    trans = ro.r('function(x){exp(x)/(1+exp(x))}')
                    _ = mgcv.plot_gam(gam_model, trans=trans, shade='T',
                                      col='black', select=i, ylab='Predicted Probability',
                                      main=modelname,
                                      rug=False, cex_lab=1.4, cex_axis=4.2)
                    LOGGER.info('plot GAM curves for %s.', i)
                except:
                    LOGGER.exception('failed to plot GAM curves for %s.', i)
            _ = grdevices.dev_off()
        except:
            LOGGER.exception('GAM plot failedin SDM process')

        try:
            predict_gam = mgcv.predict_gam(gam_model, type="response",
                                           progress="text", na_action=stats.na_pass)
            prediction = array(predict_gam).reshape(domain)
            LOGGER.info('SDM prediction for reference period processed')
        except:
            LOGGER.exception('failed to process SDM prediction')
            prediction = None
    except:
        LOGGER.exception('failed to plot GAM curves')
        _, infos_concat = mkstemp(dir='.', suffix='.pdf')

    return gam_model, prediction, statinfos


def get_prediction(gam_model, ncs_indices):  # mask=None
    """
    predict the probabillity based on the gam_model and the given climate index datasets

    :param gam_model: fitted gam (output from sdm.get_gam)
    :pram nsc_indices: list of netCDF files containing climate indices of one dataset
    :param mask: 2D array of True/False to exclude areas (e.g ocean) for prediction

    :return array: 3D array with prediction values
    """
    from netCDF4 import Dataset
    from os.path import basename
    from numpy import squeeze, ravel, array, reshape  # , zeros, broadcast_arrays, nan

    from flyingpigeon.utils import get_variable

    from rpy2.robjects.packages import importr
    import rpy2.robjects as ro

    import rpy2.robjects.numpy2ri
    rpy2.robjects.numpy2ri.activate()
    mgcv = importr("mgcv")
    stats = importr("stats")

    ncs_indices.sort()

    data = {}

    for i, nc in enumerate(ncs_indices):
        var = get_variable(nc)
        agg = basename(nc).split('_')[-2]
        ds = Dataset(nc)
        vals = squeeze(ds.variables[var])
        if i == 0:
            dims = vals.shape
        # if mask != None:
            # mask = broadcast_arrays(vals, mask)[1]
            # vals[mask==False] = nan
        indice = '%s_%s' % (var, agg)
        data[str(indice)] = ro.FloatVector(ravel(vals))

    dataf = ro.DataFrame(data)
    predict_gam = mgcv.predict_gam(gam_model, newdata=dataf,
                                   type="response", progress="text",
                                   newdata_guaranteed=True, na_action=stats.na_pass)
    prediction = array(predict_gam).reshape(dims)
    return prediction


def write_to_file(nc_indice, data):
    """
    repaces the values in an indice file with given data

    :param nc_indice: base netCDF file (indice file)
    :param data: data to be filled into the netCDF file

    :returns str: path to netCDF file
    """
    try:
        from netCDF4 import Dataset
        from shutil import copy
        from os.path import split, join
        from flyingpigeon.utils import get_variable
        from flyingpigeon.metadata import get_frequency
        from numpy import nan

        # path, nc_indice = split(indice_file)

        var = get_variable(nc_indice)
        fq = get_frequency(nc_indice)
        agg = nc_indice.split('_')[-2]

        nc = nc_indice.replace(var, 'tree').replace(agg, fq)
        copy(nc_indice, nc)

        ds = Dataset(nc, mode='a')
        vals = ds.variables[var]

        ds.renameVariable(var, 'tree')

        vals[:, :, :] = data[:, :, :]
        vals.long_name = 'Favourabilliy for tree species'
        vals.standard_name = 'tree'
        vals.units = '0-1'
        vals.missing_value = nan
        ds.close()
    except:
        msg = 'failed to fill data to netCDF file'
        LOGGER.exception(msg)
    return nc
