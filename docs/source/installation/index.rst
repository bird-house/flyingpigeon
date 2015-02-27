.. _installation:
Installation
************

Check out code from the flyingpigeon git repo. Then do the following::

   $ git clone https://github.com/bird-house/flyingpigeon.git
   $ cd flyingpigeon
   $ make

For other install options run ``make help`` and read the documention for the `Makefile <https://github.com/bird-house/birdhousebuilder.bootstrap/blob/master/README.rst>`_.

After successful installation you need to start the services. Flyingpigeon is using `Anaconda <http://www.continuum.io/>`_ Python distribution system. All installed files (config etc ...) are below the Anaconda root folder which is by default in your home directory ``~/anaconda``. Now, start the services::

   $ make start    # starts supervisor services
   $ make status   # shows supervisor status

The depolyed WPS service is available on http://localhost:8093/wps?service=WPS&version=1.0.0&request=GetCapabilities.

Check the log files for errors::

   $ tail -f  ~/anaconda/var/log/pywps/malleefowl.log
   $ tail -f  ~/anaconda/var/log/pywps/malleefowl_trace.log




