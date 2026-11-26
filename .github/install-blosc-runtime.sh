#!/usr/bin/env bash
set -eo pipefail

# Cached openvdbr binaries can link to the system copy of Blosc.
if ldconfig -p | grep -F 'libblosc.so.1 ' > /dev/null; then
  exit 0
fi
elevate=()
if [[ $(id -u) != 0 ]]; then elevate=(sudo); fi
if command -v apt-get > /dev/null; then
  "${elevate[@]}" apt-get update -qq
  "${elevate[@]}" apt-get install -y --no-install-recommends libblosc1
elif command -v dnf > /dev/null; then
  "${elevate[@]}" dnf install -y blosc
fi
