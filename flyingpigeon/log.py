"""
Logging
-------

Progress and errors in WPS processes are logged by the server. The initialization of the log file for each process
is done using the :func:`init_process_logger`.

"""

import logging


def init_process_logger(filename=None):
    """Connect and initialize the logging mechanism to a given file.

    :param str filename: Logging file name. Defaults to log.txt
    """
    filename = filename or 'log.txt'
    # create console handler and set level to debug
    ch = logging.FileHandler(filename=filename, mode="a", delay=False)
    ch.setLevel(logging.DEBUG)

    # create formatter
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')

    # add formatter to ch
    ch.setFormatter(formatter)

    # add ch to root logger
    logger = logging.getLogger()
    logger.addHandler(ch)
