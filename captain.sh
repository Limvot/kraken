#!/bin/bash

kraken="kraken"
bootstrap_commits=(cf46fb13afe66ba475db9725e9269c9c1cd3bbc3)

if [[ $1 == "clean" ]]
then
    rm ${kraken}
    rm ${kraken}_bac
    rm ${kraken}_deprecated
    rm -rf bootstrap_kalypso
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
        rm ${kraken}_bootstrap
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
                echo "no ${kraken}_deprecated, bootstrapping using Cephelpod and a chain of old Kalypsos"
                git clone . bootstrap_kalypso
                pushd bootstrap_kalypso
                git checkout ${bootstrap_commits[0]}
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
                cp deprecated_compiler/build_kraken/kraken/kraken ../${kraken}_bootstrap
                popd
                # Now make 
                mv ./krakenGrammer.kgm krakenGrammer.kgm_new
                cp bootstrap_kalypso/krakenGrammer.kgm ./
                ./${kraken}_bootstrap kraken.krak ${kraken}_deprecated
                mv ./krakenGrammer.kgm_new krakenGrammer.kgm
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


