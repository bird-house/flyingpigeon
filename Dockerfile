# vim:set ft=dockerfile:
FROM birdhouse/bird-base:latest
MAINTAINER https://github.com/bird-house

LABEL Description="Flyingpigeon WPS Application" Vendor="Birdhouse" Version="0.2.0"

# Configure hostname and user for services 
ENV HOSTNAME localhost
ENV USER www-data
ENV OUTPUT_PORT 8090
ENV PHOENIX_PASSWORD "sha256:10761810a2f2:8535bf8468e0045ec2d33bd4d2f513d669bd31b79794614f23632c3b2cadc51c"
ENV WPS_URL http://malleefowl:8094/wps


# Set current home
ENV HOME /root

# Add application sources
ADD . /opt/birdhouse

# cd into application
WORKDIR /opt/birdhouse

# Install system dependencies
RUN bash bootstrap.sh -i && bash requirements.sh

# Set conda enviroment
ENV ANACONDA_HOME /opt/conda
ENV CONDA_ENVS_DIR /opt/conda/envs

# Run install
RUN make clean install 

# Volume for data, cache, logfiles, ...
RUN chown -R $USER $CONDA_ENVS_DIR/birdhouse
RUN mv $CONDA_ENVS_DIR/birdhouse/var /data && ln -s /data $CONDA_ENVS_DIR/birdhouse/var
VOLUME /data

# Ports used in birdhouse
EXPOSE $OUTPUT_PORT 9001 8093 28093

# Start supervisor in foreground
ENV DAEMON_OPTS --nodaemon --user $USER

# Update config and start supervisor ...
CMD ["make", "update-config", "start"]

