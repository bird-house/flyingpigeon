import os
from pywps.app.Service import Service

from flyingpigeon.processes import processes


def application(environ, start_response):
    app = create_app()
    return app(environ, start_response)


def create_app():
    service = Service(processes, [
        os.path.join(os.path.dirname(__file__), 'default.cfg'),
        os.environ.get('PYWPS_CFG', '')])
    return service
