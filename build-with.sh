#!/bin/sh
set -eu

if [[ $# -ne 1 ]] ; then
	echo "Usage: build-with.sh { docker | podman }"
	exit 1
fi

builder=$1
case $builder in
	docker|podman)
	;;
	*)
	echo "You should use either docker or podman for building"
	exit 2
esac

echo Building $builder image with dependencies..
$builder build -t qubes-miragevpn .
echo Building MirageVPN...
$builder run --rm -i -v `pwd`:/tmp/orb-build:Z qubes-miragevpn
echo "SHA2 of build:   $(sha256sum ./dist/qubes-miragevpn.xen)"
echo "SHA2 last known: $(cat qubes-miragevpn.sha256)"
echo "(hashes should match for released versions)"
