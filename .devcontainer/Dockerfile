FROM debian:13
RUN set -eux;\
    apt-get update;\
    env DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends\
  ca-certificates\
  curl\
  g++\
  gcc\
  git\
  iverilog\
  libgmp-dev\
  make\
  patch\
  pkg-config\
  python3\
  unzip\
  verilator\
  xz-utils\
;\
    apt-get clean -y; rm -rf /var/lib/apt/lists/*;\
    echo DONE
