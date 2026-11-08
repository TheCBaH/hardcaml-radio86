FROM debian:13
RUN set -eux;\
    apt-get update;\
    env DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends\
  ca-certificates\
  curl\
  gcc\
  git\
  libgmp-dev\
  make\
  patch\
  pkg-config\
  python3\
  unzip\
  xz-utils\
;\
    apt-get clean -y; rm -rf /var/lib/apt/lists/*;\
    echo DONE
