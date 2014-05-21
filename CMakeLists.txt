cmake_minimum_required (VERSION 2.6)

project(Kraken)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set( MY_INCLUDES ${PROJECT_SOURCE_DIR}/include)

set( MY_SOURCES main.cpp src/Parser.cpp src/LALRParser.cpp src/GraphStructuredStack.cpp src/RNGLRParser.cpp src/ParseAction.cpp src/ParseRule.cpp src/Symbol.cpp src/StringReader.cpp src/State.cpp src/util.cpp src/Lexer.cpp src/RegEx.cpp src/RegExState.cpp src/Table.cpp src/ASTData.cpp src/ASTTransformation.cpp src/CGenerator.cpp src/Type.cpp src/Importer.cpp src/Tester.cpp )

add_custom_target(STDLibCopy ALL)
add_custom_command(TARGET STDLibCopy POST_BUILD
		COMMAND ${CMAKE_COMMAND} -E copy_directory
		"${PROJECT_SOURCE_DIR}/stdlib"
		"${PROJECT_BINARY_DIR}/stdlib")

include_directories( ${MY_INCLUDES} )

add_executable(kraken ${MY_SOURCES})



