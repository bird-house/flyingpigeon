# vim:set ft=dockerfile:
FROM birdhouse/bird-base:latest
MAINTAINER https://github.com/bird-house

LABEL Description="Flyingpigeon WPS" Vendor="Birdhouse"

# Configure hostname and ports for services
ENV HTTP_PORT 5000
ENV OUTPUT_PORT 8000
ENV HOSTNAME localhost

# Set current home
ENV HOME /root

# Copy application sources
COPY . /opt/birdhouse/src/flyingpigeon

# cd into application
WORKDIR /opt/birdhouse/src/flyingpigeon

# Provide custom.cfg with settings for docker image
COPY .docker.cfg custom.cfg

# Install system dependencies
RUN bash bootstrap.sh -i && bash requirements.sh

# Set conda enviroment
ENV ANACONDA_HOME /opt/conda
ENV CONDA_ENVS_DIR /opt/conda/envs

# Run install and fix permissions
RUN make clean install && chmod 755 /opt/birdhouse/etc && chmod 755 /opt/birdhouse/var/run

# Install PAVICS version of pywps
RUN /opt/conda/envs/flyingpigeon/bin/pip install --upgrade git+https://github.com/Ouranosinc/pywps.git@pavics

# Volume for data, cache, logfiles, ...
VOLUME /opt/birdhouse/var/lib
VOLUME /opt/birdhouse/var/log
# Volume for configs
VOLUME /opt/birdhouse/etc

# Ports used in birdhouse
EXPOSE $HTTP_PORT $OUTPUT_PORT

# Start supervisor in foreground
ENV DAEMON_OPTS --nodaemon

# Start service ...
CMD ["make", "update-config", "start"]
