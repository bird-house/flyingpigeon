import os
from pywps.app.Service import Service

from flyingpigeon.processes import processes


def application(environ, start_response):
    app = create_app()
    return app(environ, start_response)


def create_app(cfg_files=None):
    config_files = [os.path.join(os.path.dirname(__file__), 'default.cfg')]
    if cfg_files:
        config_files.extend(cfg_files)
    if 'PYWPS_CFG' in os.environ:
        config_files.append(os.environ['PYWPS_CFG'])
    service = Service(processes, config_files)
    return service
