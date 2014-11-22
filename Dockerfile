FROM ubuntu:14.04
MAINTAINER Flyingpigeon WPS Application

# Add user phoenix
RUN useradd -d /home/phoenix -m phoenix

# Add bootstrap and application requirements
ADD ./bootstrap.sh /tmp/bootstrap.sh
ADD ./requirements.sh /tmp/requirements.sh

WORKDIR /tmp

# Install system dependencies
RUN bash bootstrap.sh -i && bash requirements.sh

# Add application sources
ADD . /home/phoenix/src

# Change permissions for user phoenix
RUN chown -R phoenix /home/phoenix/src

# cd into application
WORKDIR /home/phoenix/src

# Remaining tasks run as user phoenix
USER phoenix

# Update makefile and run install
RUN bash bootstrap.sh -u && make clean build

# cd into anaconda
WORKDIR /home/phoenix/anaconda

# all currently used ports in birdhouse
EXPOSE 8080 8081 8082 8090 8091 8092 8093 8094 9001

#CMD bin/supervisord -n -c etc/supervisor/supervisord.conf && bin/nginx -c etc/nginx/nginx.conf -g 'daemon off;
CMD etc/init.d/supervisord start && bin/nginx -c etc/nginx/nginx.conf -g 'daemon off;'

