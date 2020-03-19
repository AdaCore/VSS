FROM registry.fedoraproject.org/fedora-minimal:32
RUN microdnf install \
  make \
  rpmdevtools \
  gcc-gnat \
  gprbuild \
  git \
  openssh-server \
  tar \
  gzip \
  ca-certificates && \
 microdnf clean all

RUN mkdir -p /src/
WORKDIR /src/

COPY . /src/

RUN make all check
