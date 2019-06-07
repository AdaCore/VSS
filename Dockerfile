FROM ubuntu:18.04

RUN apt-get -qq update
RUN apt-get install -y make gnat gprbuild

RUN mkdir -p /src/
WORKDIR /src/

COPY . /src/

RUN make all check
