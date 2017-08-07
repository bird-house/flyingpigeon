# based on pywps-flask demo: https://github.com/geopython/pywps-flask

import os
from pywps.app.Service import Service
from pywps import configuration
import flask

from flyingpigeon.processes import processes

app = flask.Flask('flyingpigeon')
wps_app = Service(processes, [os.path.join(os.path.dirname(__file__), 'default.cfg')])


process_descriptor = {}
for process in processes:
    abstract = process.abstract
    identifier = process.identifier
    process_descriptor[identifier] = abstract


@app.route("/")
def hello():
    server_url = configuration.get_config_value("server", "url")
    request_url = flask.request.url
    return flask.render_template('home.html', request_url=request_url,
                                 server_url=server_url,
                                 process_descriptor=process_descriptor)


@app.route('/wps', methods=['GET', 'POST'])
def wps():
    return wps_app


@app.route('/outputs/<path:path>')
def outputfile(path):
    print path
    return flask.send_from_directory(configuration.get_config_value('server', 'outputpath'), path)


def main():
    import argparse

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
                        "otherwise bind to 127.0.0.1 (localhost).  This maybe necessary in systems that only run Flask")
    args = parser.parse_args()

    if args.all_addresses:
        bind_host = '0.0.0.0'
    else:
        bind_host = '127.0.0.1'

    if args.daemon:
        pass
    else:
        app.run(host=bind_host, port=5000, debug=True, threaded=False)


if __name__ == '__main__':
    main()
