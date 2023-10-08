#!/usr/bin/env bash
set -euxo pipefail

root=$PWD/extern
rm -rf "$root"
mkdir -- "$root"

tag=0.2.14
tarball_url=https://github.com/dtolnay/prettyplease/archive/refs/tags/$tag.tar.gz

curl --silent --show-error --location "$tarball_url" \
    | tar --extract --verbose --ungzip --directory "$root" 

cp "$root/prettyplease-$tag/src/algorithm.rs" "src/prettyplease"
cp "$root/prettyplease-$tag/src/convenience.rs" "src/prettyplease"
cp "$root/prettyplease-$tag/src/iter.rs" "src/prettyplease"
cp "$root/prettyplease-$tag/src/lit.rs" "src/prettyplease"
cp "$root/prettyplease-$tag/src/ring.rs" "src/prettyplease"
cp "$root/prettyplease-$tag/src/token.rs" "src/prettyplease"
