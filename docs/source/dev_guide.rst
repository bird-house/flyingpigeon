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
Use the following instructions::

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

The ``flyingpigeon`` service command-line has more options::

    $ flyingpigeon -h

For example you can start the WPS with enabled debug logging mode::

    $ flyingpigeon --debug

Or you can overwrite the default `PyWPS`_ configuration by providing your own
PyWPS configuration file (just modifiy the options you want to change)::

    $ flyingpigeon -c mywps.cfg

.. _PyWPS: http://pywps.org/
