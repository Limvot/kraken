#include <string>
#include <iostream>
#include <fstream>
#include <vector>

#include <cstring>

#include "NodeTree.h"
#include "Symbol.h"
#include "Lexer.h"
#include "RNGLRParser.h"

#include "Importer.h"
#include "ASTData.h"
#include "CGenerator.h"
#include "Poset.h"

#include "util.h"
#include "Tester.h"

int main(int argc, char* argv[]) {
	std::vector<std::string> includePaths;
	includePaths.push_back(""); //Local

    if (argc <= 1) {
        std::cerr << "Kraken invocation: kraken sourceFile.krak" << std::endl;
        std::cerr << "Kraken invocation: kraken sourceFile.krak outputName" << std::endl;
        std::cerr << "Kraken invocation: kraken grammerFile.kgm sourceFile.krak outputName" << std::endl;
        std::cerr << "Or for testing do: kraken --test [optional list of names of file (.krak .expected_results) without extentions to run]" << std::endl;
        return 0;
    }
	std::string grammerFileString = "../krakenGrammer.kgm";

	if (argc >= 2 && std::string(argv[1]) == "--test") {
		StringReader::test();
		RegEx::test();
		Lexer::test();
		//std::cout << strSlice("123", 0, -1) << std::endl;
		Poset<int>::test();

		if (argc >= 3) {
			std::string testResults, line;
			int passed = 0, failed = 0;
			Tester test(argv[0], grammerFileString);
            // find the max length so we can pad the string and align the results
            unsigned int maxLineLength = 0;
			for (int i = 2; i < argc; i++) {
                int strLen = std::string(argv[i]).length();
                maxLineLength = maxLineLength < strLen ? strLen : maxLineLength;
            }
			for (int i = 2; i < argc; i++) {
				bool result = test.run(argv[i]);
				if (result)
					line =  padWithSpaces(std::string(argv[i]), maxLineLength) + "\t\tpassed!\n", passed++;
				else
					line = padWithSpaces(std::string(argv[i]), maxLineLength) + "\t\tFAILED!!!!\n", failed++;
				std::cout << line << std::endl;
				testResults += line;
			}
			std::cout << "===========Done Testing===========" << std::endl;
			std::cout << testResults << std::endl;
			std::cout << "Test results: " << passed << "/" << passed+failed << std::endl;
		}
		return 0;
	}
	std::string krakenDir = argv[0];
	krakenDir = strSlice(krakenDir, 0, -(std::string("kraken").length()+1));
	includePaths.push_back(krakenDir + "stdlib/"); //Add the stdlib directory that exists in the same directory as the kraken executable to the path

	std::string programName;
	std::string outputName;
    bool parse_only = false;
    //std::cout << "argv[1] == " << argv[1] << std::endl;
    if (std::string(argv[1]) == "--parse-only") {
        parse_only = true;
        grammerFileString = argv[2];
        programName = argv[3];
        //outputName = argv[3];
    } else if (argc > 3) {
        grammerFileString = argv[1];
        programName = argv[2];
        outputName = argv[3];
    } else if (argc == 3) {
        programName = argv[1];
        outputName = argv[2];
    } else {
        programName = argv[1];
        outputName = join(slice(split(programName, '.'), 0, -2), "."); // without extension
    }

	std::ifstream grammerInFile, compiledGrammerInFile;
	std::ofstream compiledGrammerOutFile;

	grammerInFile.open(grammerFileString);
	if (!grammerInFile.is_open()) {
		std::cerr << "Problem opening grammerInFile " << grammerFileString << "\n";
		return(1);
	}

	compiledGrammerInFile.open(grammerFileString + ".comp", std::ios::binary | std::ios::ate);
	if (!compiledGrammerInFile.is_open())
		std::cerr << "Problem opening compiledGrammerInFile " << grammerFileString + ".comp" << "\n";

	//Read the input file into a string
	std::string grammerInputFileString;
	std::string line;
	while(grammerInFile.good()) {
		getline(grammerInFile, line);
		grammerInputFileString.append(line+"\n");
	}
	grammerInFile.close();

	RNGLRParser parser;
	parser.loadGrammer(grammerInputFileString);

	//Start binary stuff
	bool compGramGood = false;
	if (compiledGrammerInFile.is_open()) {
		//std::cout << "Compiled grammer file exists, reading it in" << std::endl;
		std::streampos compGramSize = compiledGrammerInFile.tellg();
		char* binaryTablePointer = new char [compGramSize];
		compiledGrammerInFile.seekg(0, std::ios::beg);
		compiledGrammerInFile.read(binaryTablePointer, compGramSize);
		compiledGrammerInFile.close();
		//Check magic number
		if (binaryTablePointer[0] == 'K' && binaryTablePointer[1] == 'R' && binaryTablePointer[2] == 'A' && binaryTablePointer[3] == 'K') {
			//std::cout << "Valid Kraken Compiled Grammer File" << std::endl;
			int gramStringLength = *((int*)(binaryTablePointer+4));
			//std::cout << "The grammer string is stored to be " << gramStringLength << " characters long, gramString is "
			//<< grammerInputFileString.length() << " long. Remember 1 extra for null terminator!" << std::endl;
			if (grammerInputFileString.length() != gramStringLength-1 ||
				(strncmp(grammerInputFileString.c_str(), (binaryTablePointer+4+sizeof(int)), gramStringLength) != 0)) {
				//(one less for null terminator that is stored)
				std::cout << "The Grammer has been changed, will re-create" << std::endl;
			} else {
				compGramGood = true;
				//std::cout << "Grammer file is up to date." << std::endl;
				parser.importTable(binaryTablePointer + 4 + sizeof(int) + gramStringLength); //Load table starting at the table section
			}
		} else {
			std::cerr << grammerFileString << ".comp is NOT A Valid Kraken Compiled Grammer File, aborting" << std::endl;
			return -1;
		}
		delete [] binaryTablePointer;
	}

	if (!compGramGood) {
		//The load failed because either the file does not exist or it is not up-to-date.
		std::cout << "Compiled grammer file does not exist or is not up-to-date, generating table and writing it out" << std::endl;
		compiledGrammerOutFile.open(grammerFileString + ".comp", std::ios::binary);
		if (!compiledGrammerOutFile.is_open())
			std::cerr << "Could not open compiled file to write either!" << std::endl;
		compiledGrammerOutFile.write("KRAK", sizeof(char)*4); 	//Let us know when we load it that this is a kraken grammer file, but don't write out
		compiledGrammerOutFile.flush();							// the grammer txt until we create the set, so that if we fail creating it it won't look valid
		parser.createStateSet();
		int* intBuffer = new int;
		*intBuffer = grammerInputFileString.length()+1;
		compiledGrammerOutFile.write((char*)intBuffer, sizeof(int));
		delete intBuffer;
		compiledGrammerOutFile.write(grammerInputFileString.c_str(), grammerInputFileString.length()+1); //Don't forget null terminator

		parser.exportTable(compiledGrammerOutFile);
		compiledGrammerOutFile.close();
	}
	//End binary stuff

    //std::cout << "\nParsing" << std::endl;
	//std::cout << "\toutput name: " << outputName << std::endl;
	//std::cout << "\tprogram name: " << programName << std::endl;
	Importer importer(&parser, includePaths, outputName, parse_only); // Output name for directory to put stuff in

	//for (auto i : includePaths)
		//std::cout << i << std::endl;

	importer.import(programName);
	std::map<std::string, NodeTree<ASTData>*> ASTs = importer.getASTMap();
    if (parse_only)
        return 0;

	//Do optimization, etc. here.
	//None at this time, instead going straight to C in this first (more naive) version

	//Code generation
	//For right now, just C

	CGenerator().generateCompSet(ASTs, outputName);
	return(0);
}

