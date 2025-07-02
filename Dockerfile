# Pin the base image to a specific hash for maximum reproducibility.
# It will probably still work on newer images, though, unless an update
# changes some compiler optimisations (unlikely).
# bookworm-slim taken from https://hub.docker.com/_/debian/tags?page=1&name=bookworm-slim
FROM debian@sha256:d365f4920711a9074c4bcd178e8f457ee59250426441ab2a5f8106ed8fe948eb
# install remove default packages repository
RUN rm /etc/apt/sources.list.d/debian.sources
# and set the package source to a specific release too
# taken from https://snapshot.debian.org/archive/debian
RUN printf "deb [check-valid-until=no] http://snapshot.debian.org/archive/debian/20250103T213531Z bookworm main\n" > /etc/apt/sources.list
# taken from https://snapshot.debian.org/archive/debian-security/
RUN printf "deb [check-valid-until=no] http://snapshot.debian.org/archive/debian-security/20250101T021323Z bookworm-security main\n" >> /etc/apt/sources.list

RUN apt update && apt install --no-install-recommends --no-install-suggests -y wget ca-certificates git patch unzip bzip2 xz-utils make gcc g++ libc-dev
RUN wget -O /usr/bin/opam https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-i686-linux && chmod 755 /usr/bin/opam
# taken from https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh
RUN test `sha512sum /usr/bin/opam | cut -d' ' -f1` = \
"4c0e8771889a36bad4d5f964e2e662d5b611e6f112777d3d4eea3eea919d109cd17826beba38e6cfa1ad9553a0a989d9268f911ea5485968da04b1e08efc7de2" || exit

ENV OPAMROOT=/tmp
ENV OPAMCONFIRMLEVEL=unsafe-yes
# Pin last known-good version for reproducible builds.
# Remove this line (and the base image pin above) if you want to test with the
# latest versions.
# taken from https://github.com/ocaml/opam-repository
RUN opam init --disable-sandboxing -a --bare https://github.com/ocaml/opam-repository.git#70ed6dc3809eac914120062fdc7a82492f5c3ef9
RUN opam switch create myswitch 4.14.2
RUN opam exec -- opam install -y mirage opam-monorepo ocaml-solo5
RUN opam pin add -yn https://github.com/robur-coop/miragevpn.git#440768ae2fefc9da57e4a9f15bcbe6d217550d6c
RUN mkdir /tmp/orb-build
ADD config.ml /tmp/orb-build/config.ml
WORKDIR /tmp/orb-build
CMD opam exec -- sh -exc 'mirage configure -t xen --extra-repos=\
opam-overlays:https://github.com/dune-universe/opam-overlays.git#e031bb64e33bf93be963e9a38b28962e6e14381f,\
mirage-overlays:https://github.com/dune-universe/mirage-opam-overlays.git#797cb363df3ff763c43c8fbec5cd44de2878757e \
&& make depend && make build'
