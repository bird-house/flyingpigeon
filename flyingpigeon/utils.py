# -*- coding: utf-8 -*-

"""Utitility functions."""

import os
import tempfile
import tarfile
import requests
import shutil

from re import search
from urllib.parse import urlparse
from zipfile import ZipFile

import flyingpigeon as fp
from .config import Paths
import logging

LOGGER = logging.getLogger("PYWPS")
paths = Paths(fp)


def archive(resources, format='tar', dir_output=None, mode=None):
    """
    Compresses a list of files into an archive.

    :param resources: list of files to be stored in archive
    :param format: archive format. Options: tar (default), zip
    :param dir_output: path to output folder (default: tempory folder)
    :param mode: for format='tar':
                  'w' or 'w:'  open for writing without compression
                  'w:gz'       open for writing with gzip compression
                  'w:bz2'      open for writing with bzip2 compression
                  'w|'         open an uncompressed stream for writing
                  'w|gz'       open a gzip compressed stream for writing
                  'w|bz2'      open a bzip2 compressed stream for writing

                  for foramt='zip':
                  read "r", write "w" or append "a"

    :return str: archive path/filname.ext
    """
    dir_output = dir_output or tempfile.gettempdir()
    mode = mode or 'w'

    if format not in ['tar', 'zip']:
        raise Exception('archive format {} not supported (only zip and tar)'.format(format))

    LOGGER.info('compressing files to archive, format={}'.format(format))

    # convert to list if necessary
    if not isinstance(resources, list):
        resources = list([resources])
    resources = [x for x in resources if x is not None]

    _, arch = tempfile.mkstemp(dir=dir_output, suffix='.{}'.format(format))

    try:
        if format == 'tar':
            with tarfile.open(arch, mode) as tar:
                for f in resources:
                    tar.add(f, arcname=os.path.basename(f))
        elif format == 'zip':
            with ZipFile(arch, mode=mode) as zf:
                for f in resources:
                    zf.write(f, os.path.basename(f))
    except Exception as e:
        raise Exception('failed to create {} archive: {}'.format(format, e))
    return arch


def download_file(url, out=None, verify=False):
    if out:
        local_filename = out
    else:
        local_filename = url.split('/')[-1]
    r = requests.get(url, stream=True, verify=verify)
    with open(local_filename, 'wb') as fp:
        shutil.copyfileobj(r.raw, fp)
    return local_filename


def local_path(url):
    url_parts = urlparse(url)
    return url_parts.path


def download(url, cache=False):
    """
    Downloads URL using the Python requests module to the current directory.

    :param cache: if True then files will be downloaded to a cache directory.
    :param url: url adress of the target file location

    :return str: filename
    """
    filename = ''
    try:
        if cache:
            parsed_url = urlparse(url)
            filename = os.path.join(paths.cache, parsed_url.netloc, parsed_url.path.strip('/'))
            if os.path.exists(filename):
                LOGGER.info('file already in cache: %s', os.path.basename(filename))
                # if check_creationtime(filename, url):
                #     msg = 'file in cache older than archive file, downloading: {}'.format(os.path.basename(filename))
                #     LOGGER.info(msg)
                #     os.remove(filename)
                #     filename = download_file(url, out=filename)
                # TODO: enable creation time check
            else:
                if not os.path.exists(os.path.dirname(filename)):
                    os.makedirs(os.path.dirname(filename))
                LOGGER.info('downloading: {}'.format(url))
                filename = download_file(url, out=filename)
                # make softlink to current dir
                # os.symlink(filename, os.path.basename(filename))
                # filename = os.path.basename(filename)
        else:
            filename = download_file(url)
    except Exception as e:
        msg = 'failed to download data: {}'.format(e)
        LOGGER.exception(msg)
    return filename


def extract_archive(resources, dir_output=None):
    """
    extracts archives (tar/zip)

    :param resources: list of archive files (if netCDF files are in list,
                     they are passed and returnd as well in the return).
    :param dir_output: define a directory to store the results (default: tempory folder).

    :return list: [list of extracted files]
    """
    dir_output = dir_output or tempfile.gettempdir()

    if not isinstance(resources, list):
        resources = list([resources])
    files = []

    for arch in resources:
        try:
            LOGGER.debug("archive=%s", arch)
            ext = os.path.basename(arch).split('.')[-1]

            if ext == 'nc':
                files.append(os.path.join(dir_output, arch))
            elif ext == 'tar':
                with tarfile.open(arch, mode='r') as tar:
                    tar.extractall()
                    files.extend([os.path.join(dir_output, f) for f in tar.getnames()])
            elif ext == 'zip':
                with ZipFile(arch, mode='r') as zf:
                    zf.extractall()
                    files.extend([os.path.join(dir_output, f) for f in zf.filelist])
            else:
                LOGGER.warning('file extention {} unknown'.format(ext))
        except Exception as e:
            LOGGER.error('failed to extract sub archive {}: {}'.format(arch, e))
    return files


# def get_coordinates(resource, variable=None, unrotate=False):
#     """
#     reads out the coordinates of a variable
#
#     :param resource: netCDF resource file
#     :param variable: variable name
#     :param unrotate: If True the coordinates will be returned for unrotated pole
#
#     :returns list, list: latitudes , longitudes
#     """
#     if type(resource) != list:
#         resource = [resource]
#
#     if variable is None:
#         variable = get_variable(resource)
#
#     if unrotate is False:
#         try:
#             if len(resource) > 1:
#                 ds = MFDataset(resource)
#             else:
#                 ds = Dataset(resource[0])
#
#             var = ds.variables[variable]
#             dims = list(var.dimensions)
#             if 'time' in dims:
#                 dims.remove('time')
#             # TODO: find position of lat and long in list and replace dims[0] dims[1]
#             lats = ds.variables[dims[0]][:]
#             lons = ds.variables[dims[1]][:]
#             ds.close()
#             LOGGER.info('got coordinates without pole rotation')
#         except Exception as e:
#             msg = 'failed to extract coordinates: {}'.format(e)
#             raise Exception(msg)
#     else:
#         lats, lons = unrotate_pole(resource)
#         LOGGER.info('got coordinates with pole rotation')
#     return lats, lons


def rename_complexinputs(complexinputs):
    """
    TODO: this method is just a dirty workaround to rename input files according to the url name.
    """
    resources = []
    for inpt in complexinputs:
        new_name = inpt.url.split('/')[-1]
        os.rename(inpt.file, new_name)
        resources.append(os.path.abspath(new_name))
    return resources


def address_append(address):
    """
    Formats a URL/URI to be more easily read with libraries such as "rasterstats"

    :param address: URL/URI to a potential zip or tar file
    :return: URL/URI prefixed for archive type
    """
    zipped = search(r'(\.zip)', address)
    tarred = search(r'(\.tar)', address)

    try:
        if zipped:
            return 'zip://{}'.format(address)
        elif tarred:
            return 'tar://{}'.format(address)
        else:
            return address
    except Exception as e:
        msg = 'Failed to prefix or parse URL: {}'.format(e)
        raise Exception(msg)
