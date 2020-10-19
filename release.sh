#!/usr/bin/env bash

set -euxo pipefail

name=$(grep name: < package.yaml | awk '{print $2}')
version=$(grep version: < package.yaml | awk '{print $2}')
bundle="$name-$version.tar.gz"

# check changelog contains an entry for this version
grep "^# $version$" < CHANGELOG.md

hpack
cabal sdist -o - > "$bundle"
cabal upload --publish "$bundle"
cabal upload -d --publish
