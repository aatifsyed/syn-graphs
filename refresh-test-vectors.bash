#!/usr/bin/env bash
set -euxo pipefail

root=$PWD/test-vectors
rm -rf "$root"
mkdir -- "$root"


graphviz_gallery=$root/graphviz-gallery
mkdir -- "$graphviz_gallery"

commit=$(git ls-remote https://gitlab.com/graphviz/graphviz.gitlab.io | rg HEAD | awk '{ print $1 }')
test -n "$commit"
tarball_url=https://gitlab.com/graphviz/graphviz.gitlab.io/-/archive/$commit/graphviz.gitlab.io-main.tar.gz

pushd -- "$graphviz_gallery"
curl --silent --show-error --location "$tarball_url" \
    | tar --extract --verbose --ungzip --wildcards '*/content/en/Gallery/*/*.gv.txt' --strip-components=5
popd

echo "$commit" > "$root/graphviz-gallery.commit"
