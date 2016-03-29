#!/bin/sh

kraken="kraken"

if [ -s "$kraken" ]
then
    #echo "$kraken exists, calling"
    echo "$kraken exists!"
else
    echo "gotta make $kraken, testing for compilers to do so"
    if ! [ -s "${kraken}_bac" ]
    then
        if ! [ -s "${kraken}_deprecated" ]
        then
            echo "no ${kraken}_deprecated, using Cephelpod"
            cp -r stdlib deprecated_compiler
            cp krakenGrammer.kgm deprecated_compiler
            cp kraken.krak deprecated_compiler
            pushd deprecated_compiler
            mkdir build
            pushd build
            cmake ..
            make
            popd
            mkdir build_kraken
            mv kraken.krak build_kraken
            pushd build_kraken
            ../build/kraken kraken.krak
            popd
            popd
            cp deprecated_compiler/build_kraken/kraken/kraken ./kraken_deprecated
        else
            echo "${kraken}_deprecated exists, calling"
        fi
        ./kraken_deprecated kraken.krak kraken_bac
    else
        echo "${kraken}_bac exists, calling"
    fi
    ./${kraken}_bac ${kraken}.krak
fi

#./${kraken} $@


