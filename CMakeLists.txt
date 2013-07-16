cmake_minimum_required (VERSION 2.6)

project(Kraken)

set( MY_INCLUDES ${PROJECT_SOURCE_DIR}/include)

set( MY_SOURCES main.cpp src/Parser.cpp src/ParseAction.cpp src/ParseRule.cpp src/Symbol.cpp src/StringReader.cpp src/NodeTree.cpp src/State.cpp src/util.cpp src/Lexer.cpp src/RegEx.cpp src/RegExState.cpp src/Table.cpp )

include_directories( ${MY_INCLUDES} )

add_executable(kraken ${MY_SOURCES})

