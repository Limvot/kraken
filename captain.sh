#!/bin/bash

kraken="kraken"
bootstrap_commits=(cf46fb13afe66ba475db9725e9269c9c1cd3bbc3 2cd43e5a217318c70097334b3598d2924f64b362 2051f54b559ac5edf67277d4f1134aca2cb9215d ecbbcb4eda56e2467efb0a04e7d668b95856aa4b d126cbf24ba8b26e3814e2260d555ecaee86508c 947384cced5397a517a71963edc8f47e668d734f cfcaff7887a804fe77dadaf2ebb0251d6e8ae8e2 12dfa837e31bf09adb1335219473b9a7e6db9eac acb0e48324f353d30d148eb11d1bf2843d83b51a 29eff2a23e5c8afc59dc71a9ecd74cedbd5663c3)


# Echo version string to a file included by kraken.krak
# There is a default version string in the file in case kraken is not built with captain
echo "var version_string = \"Self-hosted Kraken compiler \\\"Kalypso\\\" - revision $(git rev-list HEAD | wc -l), commit: $(git rev-parse HEAD)\";" > compiler_version.krak

if ! [ -s "cached_builds" ]
then
    mkdir cached_builds
fi

if [[ $1 == "clean" ]]
then
    rm ${kraken}
    rm ${kraken}_bac
    rm ${kraken}_deprecated
    rm ${kraken}_bootstrap
    rm -rf bootstrap_kalypso
else
    if [[ $1 == "backup" ]]
    then
        rm ${kraken}
    fi
    if [[ $1 == "from_old" ]]
    then
        rm ${kraken}
        rm ${kraken}_bac
        rm ${kraken}_deprecated
    fi
    if [[ $1 == "rebuild" ]]
    then
        rm ${kraken}
        rm ${kraken}_bac
        rm ${kraken}_deprecated
        rm ${kraken}_bootstrap
        rm -rf bootstrap_kalypso
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
                echo "no ${kraken}_deprecated, bootstrapping using kraken_bootstrap"
                if ! [ -s "${kraken}_bootstrap" ]
                then
                    # Check to see if we have a chached version
                    cached_index=0
                    for ((i=1; i < ${#bootstrap_commits[@]}; i++))
                    do
                        echo "checking for cached kalypso part $i"
                        echo "commit hash: ${bootstrap_commits[$i]}"
                        if [ -s "cached_builds/${bootstrap_commits[i]}" ]
                        then
                            cached_index=$i
                            echo "have cached: ${bootstrap_commits[$i]}"
                        else
                            echo "do not have cached: ${bootstrap_commits[$i]}"
                        fi
                    done

                    git clone . bootstrap_kalypso
                    pushd bootstrap_kalypso
                    if [[ $cached_index == "0" ]]
                    then
                        echo "no ${kraken}_bootstrap, bootstrapping using Cephelpod and a chain of old Kalypsos"
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
                        pushd deprecated_compiler/build_kraken/kraken
                        sh kraken.sh
                        popd
                        cp deprecated_compiler/build_kraken/kraken/kraken ./${kraken}_bootstrap
                    else
                        echo "no ${kraken}_bootstrap, bootstrapping using starting from cached version"
                        git checkout ${bootstrap_commits[$cached_index]}
                        cp "../cached_builds/${bootstrap_commits[$cached_index]}/kraken.krak.c" "./"
                        cc kraken.krak.c -lm -lpthread -O3 -o kraken_bootstrap
                    fi

                    # loop through the chain
                    for ((i=$cached_index+1; i < ${#bootstrap_commits[@]}; i++))
                    do
                        echo "building kalypso bootstrap part $i"
                        echo "commit hash: ${bootstrap_commits[$i]}"
                        mv ./krakenGrammer.kgm krakenGrammer.kgm_old
                        git checkout ${bootstrap_commits[$i]}
                        echo "var version_string = \"Self-hosted Kraken compiler \\\"Kalypso\\\" - revision $(git rev-list HEAD | wc -l), commit: $(git rev-parse HEAD)\";" > compiler_version.krak
                        mv ./krakenGrammer.kgm krakenGrammer.kgm_new
                        mv ./krakenGrammer.kgm_old krakenGrammer.kgm
                        # Quick fix - I made a commit that actually depends on it's own grammer to be built
                        if [[ ${bootstrap_commits[$i]} == "12dfa837e31bf09adb1335219473b9a7e6db9eac" ]]
                        then
                            echo "Hot fixing mistake - using new grammer instead of old"
                            mv ./krakenGrammer.kgm_new krakenGrammer.kgm
                        fi
                        ./${kraken}_bootstrap kraken.krak ${kraken}_bootstrap
                        mkdir "../cached_builds/${bootstrap_commits[$i]}"
                        cp "./kraken.krak.c" "../cached_builds/${bootstrap_commits[$i]}/"
                        mv ./krakenGrammer.kgm_new krakenGrammer.kgm
                    done
                    popd # out of bootstrap
                fi
                echo "making kraken_deprecated - the first current Kraken version, but built with an old compiler"
            
                # Now make real
                mv ./krakenGrammer.kgm krakenGrammer.kgm_new
                mv ./krakenGrammer.kgm.comp_new krakenGrammer.kgm.comp_new_new
                cp bootstrap_kalypso/krakenGrammer.kgm ./
                cp bootstrap_kalypso/krakenGrammer.kgm.comp_new ./
                cp bootstrap_kalypso/${kraken}_bootstrap ./${kraken}_bootstrap
                ./${kraken}_bootstrap kraken.krak ${kraken}_deprecated
                mv ./krakenGrammer.kgm_new krakenGrammer.kgm
                mv ./krakenGrammer.kgm.comp_new_new krakenGrammer.kgm.comp_new
            else
                echo "${kraken}_deprecated exists, calling"
            fi
            echo "making kraken_bac, a current compiler built with kraken_deprecated"
            ./${kraken}_deprecated kraken.krak ${kraken}_bac
        else
            echo "${kraken}_bac exists, calling"
        fi
        echo "making kraken, the real current compiler built with kraken_bac"
        ./${kraken}_bac kraken.krak ${kraken}
    fi
fi

#./${kraken} $@


