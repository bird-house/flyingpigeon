import argparse

from werkzeug.wsgi import SharedDataMiddleware
from werkzeug.serving import run_simple

from pywps import configuration

from flyingpigeon import wsgi


def create_app(with_shared=True):
    app = wsgi.application
    if with_shared:
        app = SharedDataMiddleware(
            app,
            {
                '/static': ('flyingpigeon', 'static'),
                '/outputs': configuration.get_config_value('server', 'outputpath')
            })
    return app


def main():
    # see werkzeug example:
    # https://github.com/pallets/werkzeug/blob/master/examples/shortly/shortly.py

    parser = argparse.ArgumentParser(
        description="""Script for starting a demo Flyingpigeon WPS
                       instance with processes for the climate impact community.
                       This service is by default available at http://localhost:5000/wps""",
        epilog="""Do not use this service in a production environment.
         It's intended to be running in test environment only!
         For more documentation, visit http://flyingpigeon.readthedocs.io/en/latest/
        """
    )
    parser.add_argument('-d', '--daemon',
                        action='store_true', help="run in daemon mode")
    parser.add_argument('-a', '--all-addresses',
                        action='store_true', help="run flask using IPv4 0.0.0.0 (all network interfaces), "
                        "otherwise bind to 127.0.0.1 (localhost). This maybe necessary in systems that only run Flask")
    args = parser.parse_args()

    if args.all_addresses:
        bind_host = '0.0.0.0'
    else:
        bind_host = '127.0.0.1'

    app = create_app()

    if args.daemon:
        pass
    else:
        run_simple(bind_host, 5000, app, use_debugger=True, use_reloader=True)


if __name__ == '__main__':
    main()
