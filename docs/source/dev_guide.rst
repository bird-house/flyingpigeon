.. _devguide:

Developer Guide
===============

.. contents::
    :local:
    :depth: 2


.. _wps_test_env:

Running WPS service in test environment
---------------------------------------

For development purposes you can run the WPS service without nginx and supervisor.
Use the following instructions:

.. code-block:: sh

    # get the source code
    $ git clone https://github.com/bird-house/flyingpigeon.git
    $ cd flyingpigeon

    # create conda environment
    $ conda env create -f environment.yml

    # activate conda environment
    $ source activate flyingpigeon

    # install flyingpigeon code into conda environment
    $ python setup.py develop

    # start the WPS service
    $ flyingpigeon

    # open your browser on the default service url
    $ firefox http://localhost:5000/wps

    # ... and service capabilities url
    $ firefox http://localhost:5000/wps?service=WPS&request=GetCapabilities

The ``flyingpigeon`` service command-line has more options:

.. code-block:: sh

    $ flyingpigeon -h

For example you can start the WPS with enabled debug logging mode:

.. code-block:: sh

    $ flyingpigeon --debug

Or you can overwrite the default `PyWPS`_ configuration by providing your own
PyWPS configuration file (just modifiy the options you want to change):

.. code-block:: sh

    # edit your local pywps configuration file
    $ cat mydev.cfg
    [logging]
    level = WARN
    file = /tmp/mydev.log

    # start the service with this configuration
    $ flyingpigeon -c mydev.cfg

.. _PyWPS: http://pywps.org/
