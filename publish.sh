#!/bin/bash
# before publishing bump the version number in FunToolbox/AssemblyInfo.fs
set -e
MSBuild.exe FunToolbox.sln -t:Clean -p:Configuration=Release
MSBuild.exe FunToolbox.sln -t:FunToolbox -p:Configuration=Release
mkdir -p tmp
rm -f tmp/*.nupkg
(cd FunToolbox && ../.paket/paket pack ../tmp)
.paket/paket push --url https://www.myget.org/F/pragmatrix/api/v2/package --api-key $MYGETAPIKEY tmp/*.nupkg
