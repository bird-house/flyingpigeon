Flying Pigeon
=============

Flying Pigeon (the bird)
  *The Pigeon find its way home over extremely long distances. [..].* (`Wikipedia https://en.wikipedia.org/wiki/Pigeon_flying`_).

Flying Pigeon (the bike)
  *Flying Pigeon is a Chinese bicycle company [..]. The Flying Pigeon is the most popular vehicle ever.* (`Wikiepedia https://en.wikipedia.org/wiki/Flying_Pigeon`_)

Pidgeon is a Python package with a collection of algorithms for the climate community available on a Web Processing Service (WPS).

Installation
------------

Check out code from the pigeon git repo (will be available on github). Then do the following::

   $ cd pigeon
   $ ./requirements.sh
   $ ./install.sh


After successful installation you need to start the services. Pigeon is using `Anaconda http://www.continuum.io/`_ Python distribution system. All installed files (config etc ...) are below the Anaconda root folder which is by default in your home directory ``~/anaconda``. Now, start the services::

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

After any change to your ``custom.cfg`` you **need** to run ``install.sh`` again and restart the ``supervisor`` service::

  $ ./install.sh
  $  ~/anaconda/etc/init.d/supervisor restart


Update
------

When updating your installation you may run ``clean.sh`` to remove outdated Python dependencies::

   $ cd malleefowl
   $ git pull
   $ ./clean.sh
   $ ./requirement.sh
   $ ./install.sh

And then restart the ``supervisor`` and ``nginx`` service.


Authors
-------

* `DKRZ http://www.dkrz.de`_
* `Climate Service Center http://www.climate-service-center.de/`_
* `IPSL http://www.ipsl.fr/`_



