import os
from pywps.app.Service import Service
from werkzeug.wsgi import SharedDataMiddleware

from flyingpigeon.processes import processes

__version__ = "0.11.0"


def application(environ, start_response):
    # see werkzeug example:
    # https://github.com/pallets/werkzeug/blob/master/examples/shortly/shortly.py
    wps_app = Service(processes, [os.environ.get('PYWPS_CFG')])
    app = SharedDataMiddleware(
        wps_app,
        {
            # '/static': os.path.join(os.path.dirname(__file__), 'static')
            '/wps/static': ('flyingpigeon', 'static')
        })
    return app(environ, start_response)


if __name__ == '__main__':
    from werkzeug.serving import run_simple
    run_simple('127.0.0.1', 5000, application, use_debugger=True, use_reloader=True)
