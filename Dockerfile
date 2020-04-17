FROM registry.fedoraproject.org/fedora-minimal:32
ARG CODECOV_TOKEN
RUN microdnf install \
  make \
  rpmdevtools \
  gcc-gnat \
  gprbuild \
  git \
  openssh-server \
  tar \
  gzip \
  bash \
  curl \
  ca-certificates && \
 microdnf clean all

RUN mkdir -p /src/
WORKDIR /src/

COPY . /src/

RUN make all check coverage && \
  curl -s https://codecov.io/bash -o /tmp/codecov.sh && \
  /bin/bash /tmp/codecov.sh
