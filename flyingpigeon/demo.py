def main():
    import argparse
    from werkzeug.serving import run_simple
    from pywps import configuration
    from flyingpigeon import wsgi
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
    parser.add_argument('-c', '--config',
                        help="path to pywps configuration file.")
    parser.add_argument('-d', '--daemon',
                        action='store_true', help="run in daemon mode.")
    args = parser.parse_args()

    bind_host = '127.0.0.1'

    static_files = {
        '/static': ('flyingpigeon', 'static'),
        '/outputs': configuration.get_config_value('server', 'outputpath')
    }

    if args.daemon:
        pass
    else:
        run_simple(
            hostname='127.0.0.1',
            port=5000,
            application=wsgi.application,
            use_debugger=True,
            use_reloader=True,
            static_files=static_files)


if __name__ == '__main__':
    main()
