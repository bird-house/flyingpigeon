Installation
============

The installation uses the Python distribution system `Anaconda <https://www.continuum.io/>`_ to maintain software dependencies.
If Anaconda is not available, then a minimal Anaconda will be installed during the installation processes in your home directory ``~/anaconda``.

The installation process setups a conda environment named ``flyingpigeon``. All additional packages and configuration files are put into this conda environment. The location is ``~/.conda/envs/birdhouse``.

Now, check out the Flying Pigeon code from github and start the installation::

   $ git clone https://github.com/bird-house/flyingpigeon.git
   $ cd flyingpigeon
   $ make clean install

After successful installation, you need to start the services. All installed files (config etc ...) are below the conda environment ``birdhouse`` which is by default in your home directory ``~/.conda/envs/birdhouse``. Now, start the services::

   $ make start  # starts supervisor services
   $ make status # shows supervisor status

The depolyed WPS service is available on http://localhost:8093/wps?service=WPS&version=1.0.0&request=GetCapabilities.

Check the log files for errors::

   $ tail -f ~/birdhouse/var/log/pywps/flyingpigeon.log
   $ tail -f ~/birdhouse/var/log/supervisor/flyingpigeon.log

For other install options, run ``make help`` and read the documention for the `Makefile <Bootstrap_>`_.

Using docker-compose
====================

Start flyingpigeon with docker-compose (docker-compose version > 1.7):

.. code-block:: sh

   $ docker-compose up

By default the WPS is available on port 8080: http://localhost:8080/wps?service=WPS&version=1.0.0&request=GetCapabilities.

You can change the ports and hostname with environment variables:

.. code-block:: sh

  $ HOSTNAME=flyingpigeon HTTP_PORT=8093 SUPERVISOR_PORT=48093 docker-compose up

Now the WPS is available on port 8093: http://flyingpigeon:8093/wps?service=WPS&version=1.0.0&request=GetCapabilities.
