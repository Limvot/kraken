#!/bin/bash

kraken="kraken"

if [[ $1 == "clean" ]]
then
    rm ${kraken}
    rm ${kraken}_bac
    rm ${kraken}_deprecated
    rm -r deprecated_compiler/stdlib
    rm deprecated_compiler/krakenGrammer.kgm.comp
    rm deprecated_compiler/krakenGrammer.kgm
    rm -r deprecated_compiler/build
    rm -r deprecated_compiler/build_kraken
else
    if [[ $1 == "backup" ]]
    then
        rm ${kraken}
    fi
    if [[ $1 == "rebuild" ]]
    then
        rm ${kraken}
        rm ${kraken}_bac
        rm ${kraken}_deprecated
    fi

    if [ -s "$kraken" ]
    then
        #echo "$kraken exists, calling"
        ./${kraken} ${kraken}.krak ${kraken}
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
                cp deprecated_compiler/build_kraken/kraken/kraken ./${kraken}_deprecated
            else
                echo "${kraken}_deprecated exists, calling"
            fi
            ./${kraken}_deprecated kraken.krak ${kraken}_bac
        else
            echo "${kraken}_bac exists, calling"
        fi
        ./${kraken}_bac kraken.krak ${kraken}
    fi
fi

#./${kraken} $@


