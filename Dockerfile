# vim:set ft=dockerfile:
FROM continuumio/miniconda3
MAINTAINER https://github.com/bird-house/flyingpigeon
LABEL Description="Flyingpigeon WPS" Vendor="Birdhouse" Version="1.3.1"

# Update Debian system
RUN apt-get update && apt-get install -y \
 build-essential \
&& rm -rf /var/lib/apt/lists/*

# Update conda
RUN conda update -n base conda

# Copy WPS project
COPY . /opt/wps

WORKDIR /opt/wps

# Create conda environment
RUN conda env create -n wps -f environment.yml

# Install WPS
RUN ["/bin/bash", "-c", "source activate wps && python setup.py develop"]

# Start WPS service on port 8093 on 0.0.0.0
EXPOSE 8093
ENTRYPOINT ["/bin/bash", "-c"]
CMD ["source activate wps && exec emu start -b 0.0.0.0 -config /opt/wps/etc/demo.cfg"]

# docker build -t bird-house/flyingpigeon .
# docker run -p 8093:8093 bird-house/flyingpigeon
# http://localhost:8093/wps?request=GetCapabilities&service=WPS
# http://localhost:8093/wps?request=DescribeProcess&service=WPS&identifier=all&version=1.0.0
