import logging


def init_process_logger(filename=None):
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

