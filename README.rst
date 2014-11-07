Flying Pigeon
=============

Flying Pigeon (the bird)
  *The Pigeon find its way home over extremely long distances. [..].* (`Wikipedia <https://en.wikipedia.org/wiki/Pigeon_flying>`_).

Flying Pigeon (the bike)
  *Flying Pigeon is a Chinese bicycle company [..]. The Flying Pigeon is the most popular vehicle ever.* (`Wikiepedia <https://en.wikipedia.org/wiki/Flying_Pigeon>`_)

Pidgeon is a Python package with a collection of algorithms for the climate community available on a Web Processing Service (WPS).

Installation
------------

Check out code from the flyingpigeon git repo. Then do the following::

   $ git clone https://github.com/bird-house/flyingpigeon.git
   $ cd flyingpigeon
   $ make

For other install options run ``make help`` and read the documention for the `Makefile <https://github.com/bird-house/birdhousebuilder.bootstrap/blob/master/README.rst>`_.

After successful installation you need to start the services. Flyingpigeon is using `Anaconda <http://www.continuum.io/>`_ Python distribution system. All installed files (config etc ...) are below the Anaconda root folder which is by default in your home directory ``~/anaconda``. Now, start the services::

   $ cd ~/anaconda
   $ etc/init.d/supervisor start
   $ etc/init.d/nginx start

The depolyed WPS service is available on http://localhost:8093/wps?service=WPS&version=1.0.0&request=GetCapabilities.

Check the log files for errors::

   $ tail -f  ~/anaconda/var/log/pywps/malleefowl.log
   $ tail -f  ~/anaconda/var/log/pywps/malleefowl_trace.log

Configuration
-------------

If you want to run on a different hostname or port then change the default values in ``custom.cfg``::

   $ cd malleefowl
   $ vim custom.cfg
   $ cat custom.cfg
   [settings]
   hostname = localhost
   http-port = 8091

After any change to your ``custom.cfg`` you **need** to run ``make install`` again and restart the ``supervisor`` service::

  $ make install
  $  ~/anaconda/etc/init.d/supervisor restart



