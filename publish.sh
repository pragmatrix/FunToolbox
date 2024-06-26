#!/bin/bash
# before publishing bump the version number in FunToolbox/FunToolbox.fsproj
set -e

if [ -z "$MYGETAPIKEY" ]; then
  echo "MYGETAPIKEEY is not set or empty."
  exit 1
fi

mkdir -p tmp
rm -f tmp/*.nupkg
(cd FunToolbox && dotnet clean -c Release)
(cd FunToolbox && rm -rf obj bin && dotnet pack -c Release -o ../tmp)
dotnet paket push --url https://www.myget.org/F/pragmatrix/api/v2/package --api-key $MYGETAPIKEY tmp/*.nupkg
