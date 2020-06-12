#!/bin/sh

# usage: pass the git tag.

set -e

if [ -z "$1" ]; then
   exit
fi

# ================================================

tag=$1
download_url="https://codeload.github.com/rosado/reframe.nim/zip/$tag"
file="$tag.zip"
formula_file="$HOME/dev/homebrew-rosado/Formula/reframe-tool.rb"

if [ ! -f $file ]; then
   curl $download_url -o $file
fi

file_sha=`shasum -a 256 $file | awk '{ print $1} '`
echo $file_sha

sed -i '' "s/sha256 \".*\"/sha256 \"$file_sha\"/" "$formula_file"
sed -i '' "s+url \".*\"+url \"$download_url\"+" "$formula_file"
