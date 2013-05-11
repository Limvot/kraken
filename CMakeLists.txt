cmake_minimum_required (VERSION 2.6)

project(Kraken)

set( MY_INCLUDES ${PROJECT_SOURCE_DIR}/include)

set( MY_SOURCES main.cpp src/NodeTree.cpp )

include_directories( ${MY_INCLUDES} )

add_executable(kraken ${MY_SOURCES})

