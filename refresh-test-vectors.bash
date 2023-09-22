#!/usr/bin/env bash
set -euxo pipefail

temp=$PWD/target/test-vector-sources
rm -rf "$temp"
mkdir -- "$temp"

test_vectors_root=$PWD/test-vectors
test_vectors=$test_vectors_root/dot

rm -rf "$test_vectors_root"
mkdir --parents -- "$test_vectors"

for repo in graphviz graphviz.gitlab.io; do
    repo_url=https://gitlab.com/graphviz/$repo
    commit=$(git ls-remote "$repo_url" | rg HEAD | awk '{ print $1 }')
    test -n "$commit"
    tarball_url=https://gitlab.com/graphviz/$repo/-/archive/$commit/$repo-$commit.tar.gz

    # tar: --touch because some files may be from the future upstream
    curl --silent --show-error --location "$tarball_url" \
        | tar --extract --verbose --ungzip --touch --directory "$temp"
    
    echo "$commit" > "$test_vectors_root/$repo.commit"
done

for suffix in '*.gv' '*.gv.txt'; do
    # sort for determinism
    find "$temp" -name "$suffix" -and -type f -print0  \
        | sort --zero-terminated \
        | xargs --null cp --backup=numbered --verbose --target-directory="$test_vectors"
done

