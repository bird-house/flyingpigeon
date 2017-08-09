import os

from pywps import configuration
from flyingpigeon._compat import urlparse

import logging
LOGGER = logging.getLogger('demo')


def get_host():
    url = configuration.get_config_value('server', 'url')
    url = url or 'http://localhost:5000/wps'

    LOGGER.warn("starting WPS service on %s", url)

    parsed_url = urlparse(url)
    if ':' in parsed_url.netloc:
        bind_host, port = parsed_url.netloc.split(':')
        port = int(port)
    else:
        bind_host = parsed_url.netloc
        port = 80
    return bind_host, port


def main():
    import argparse
    from werkzeug.serving import run_simple
    from flyingpigeon import wsgi

    parser = argparse.ArgumentParser(
        description="""Script for starting a demo Flyingpigeon WPS
                       instance with processes for the climate impact community.
                       This service is by default available at http://localhost:5000/wps""",
        epilog="""Do not use this service in a production environment.
         It's intended to be running in test environment only!
         For more documentation, visit http://flyingpigeon.readthedocs.io/en/latest/
        """
    )
    parser.add_argument('--debug',
                        action="store_true", help="enable debug logging mode")
    parser.add_argument('-c', '--config',
                        help="path to pywps configuration file")
    parser.add_argument('-d', '--daemon',
                        action='store_true', help="run in daemon mode")
    args = parser.parse_args()
    cfg_files = []
    if args.config:
        cfg_files.append(args.config)
        LOGGER.warn('using pywps configuration: %s', args.config)
    if args.debug:
        cfg_files.append(os.path.join(os.path.dirname(__file__), 'debug.cfg'))
    app = wsgi.create_app(cfg_files)
    static_files = {
        # '/static': ('flyingpigeon', 'static'),
        '/outputs': configuration.get_config_value('server', 'outputpath')
    }

    # call this *after* app is initialized ... needs pywps config.
    bind_host, port = get_host()

    run_simple(
        hostname=bind_host,
        port=port,
        application=app,
        use_debugger=True,
        use_reloader=True,
        static_files=static_files)


if __name__ == '__main__':
    main()
