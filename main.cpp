#include <string>
#include <iostream>
#include <fstream>
#include <vector>

#include <cstring>

#include "NodeTree.h"
#include "Symbol.h"
#include "Lexer.h"
#include "LALRParser.h"
#include "RNGLRParser.h"

#include "NodeTransformation.h"
#include "RemovalTransformation.h"
#include "CollapseTransformation.h"
#include "ASTTransformation.h"
#include "ASTData.h"
#include "CGenerator.h"

#include "util.h"

int main(int argc, char* argv[]) {
	if (argc == 2 && std::string(argv[1]) == "--test") {
		StringReader::test();
		RegEx::test();
		Lexer::test();
		//std::cout << strSlice("123", 0, -1) << std::endl;
		return 0;
	}

	std::ifstream programInFile, grammerInFile, compiledGrammerInFile;
	std::ofstream outFile, outFileTransformed, outFileAST, outFileC, compiledGrammerOutFile;

	programInFile.open(argv[1]);
	if (!programInFile.is_open()) {
		std::cout << "Problem opening programInFile " << argv[1] << "\n";
		return(1);
	}

	std::string grammerFileString = argv[2];

	grammerInFile.open(grammerFileString);
	if (!grammerInFile.is_open()) {
		std::cout << "Problem opening grammerInFile " << grammerFileString << "\n";
		return(1);
	}

	compiledGrammerInFile.open(grammerFileString + ".comp", std::ios::binary | std::ios::ate);
	if (!compiledGrammerInFile.is_open()) {
		std::cout << "Problem opening compiledGrammerInFile " << grammerFileString + ".comp" << "\n";
		//return(1);
	}

	outFile.open(argv[3]);
	if (!outFile.is_open()) {
		std::cout << "Probelm opening output file " << argv[3] << "\n";
		return(1);
	}

	outFileTransformed.open((std::string(argv[3]) + ".transformed.dot").c_str());
	if (!outFileTransformed.is_open()) {
		std::cout << "Probelm opening second output file " << std::string(argv[3]) + ".transformed.dot" << "\n";
		return(1);
	}

	outFileAST.open((std::string(argv[3]) + ".AST.dot").c_str());
	if (!outFileAST.is_open()) {
		std::cout << "Probelm opening second output file " << std::string(argv[3]) + ".AST.dot" << "\n";
		return(1);
	}

	outFileC.open((std::string(argv[3]) + ".c").c_str());
	if (!outFileC.is_open()) {
		std::cout << "Probelm opening third output file " << std::string(argv[3]) + ".c" << "\n";
		return(1);
	}
	//Read the input file into a string
	std::string programInputFileString, grammerInputFileString;
	std::string line;
	while(grammerInFile.good()) {
		getline(grammerInFile, line);
		grammerInputFileString.append(line+"\n");
	}
	grammerInFile.close();

	while(programInFile.good()) {
		getline(programInFile, line);
		programInputFileString.append(line+"\n");
	}
	programInFile.close();

	//LALRParser parser;
	RNGLRParser parser;
	parser.loadGrammer(grammerInputFileString);
	//std::cout << "Creating State Set from Main" << std::endl;
	std::cout << "\nState Set" << std::endl;

	//Start binary stuff
	bool compGramGood = false;
	if (compiledGrammerInFile.is_open()) {
		std::cout << "Compiled grammer file exists, reading it in" << std::endl;
		std::streampos compGramSize = compiledGrammerInFile.tellg();
		char* binaryTablePointer = new char [compGramSize];
		compiledGrammerInFile.seekg(0, std::ios::beg);
		compiledGrammerInFile.read(binaryTablePointer, compGramSize);
		compiledGrammerInFile.close();
		if (binaryTablePointer[0] == 'K' && binaryTablePointer[1] == 'R' && binaryTablePointer[2] == 'A' && binaryTablePointer[3] == 'K') {
			std::cout << "Valid Kraken Compiled Grammer File" << std::endl;
			int gramStringLength = *((int*)(binaryTablePointer+4));
			std::cout << "The grammer string is stored to be " << gramStringLength << " characters long, gramString is "
			<< grammerInputFileString.length() << " long. Remember 1 extra for null terminator!" << std::endl;
			if (grammerInputFileString.length() != gramStringLength-1 ||
				(strncmp(grammerInputFileString.c_str(), (binaryTablePointer+4+sizeof(int)), gramStringLength) != 0)) {
				//(one less for null terminator that is stored)
				std::cout << "The Grammer has been changed, will re-create" << std::endl;
			} else {
				compGramGood = true;
				std::cout << "grammer file good" << std::endl;
				//int tableLength = *((int*)(binaryTablePointer + 4 + sizeof(int) + gramStringLength));
				parser.importTable(binaryTablePointer + 4 + sizeof(int) + gramStringLength); //Load table starting at the table section
			}
		} else {
			std::cout << grammerFileString << ".comp is NOT A Valid Kraken Compiled Grammer File, aborting" << std::endl;
			return -1;
		}
		delete binaryTablePointer;
	}

	if (!compGramGood) {
		//The load failed because either the file does not exist or it is not up-to-date.
		std::cout << "Compiled grammer file does not exist or is not up-to-date, generating table and writing it out" << std::endl;
		compiledGrammerOutFile.open(grammerFileString + ".comp", std::ios::binary);
		if (!compiledGrammerOutFile.is_open())
			std::cout << "Could not open compiled file to write either!" << std::endl;
		compiledGrammerOutFile.write("KRAK", sizeof(char)*4);
		int* intBuffer = new int;
		*intBuffer = grammerInputFileString.length()+1;
		compiledGrammerOutFile.write((char*)intBuffer, sizeof(int));
		delete intBuffer;
		compiledGrammerOutFile.write(grammerInputFileString.c_str(), grammerInputFileString.length()+1); //Don't forget null terminator

		parser.createStateSet();
		parser.exportTable(compiledGrammerOutFile);
		compiledGrammerOutFile.close();
	}
	//End binary stuff

	//std::cout << "finished State Set from Main" << std::endl;
	//std::cout << "Doing stateSetToString from Main" << std::endl;
	// std::cout << "\n\n\n\n\n\n\n\n\n\nState Set toString" << std::endl;
	// std::cout << parser.stateSetToString() << std::endl;
 	// std::cout << "finished stateSetToString from Main" << std::endl;
	// std::cout << "\n\n\n\n\n\n\n\n\n\nTable" << std::endl;
	// std::cout << parser.tableToString() << std::endl;
	// std::cout << "\n\n\n\n\n\n\n\n\n\nGrammer Input File" << std::endl;
	// std::cout << grammerInputFileString << std::endl;
	// std::cout << "\n\n\n\n\n\n\n\n\n\nGrammer toString" << std::endl;
	// std::cout << parser.grammerToString() << std::endl;
	//std::cout << parser.grammerToDOT() << std::endl;

	//outFile << parser.grammerToDOT() << std::endl;
	std::cout << "\nParsing" << std::endl;

	std::cout << programInputFileString << std::endl;
	NodeTree<Symbol>* parseTree = parser.parseInput(programInputFileString);

	if (parseTree) {
		//std::cout << parseTree->DOTGraphString() << std::endl;
		outFile << parseTree->DOTGraphString() << std::endl;
	} else {
		std::cout << "ParseTree returned from parser is NULL!" << std::endl;
	}
	outFile.close();

	//Remove Transformations
	std::vector<Symbol> removeSymbols;
	removeSymbols.push_back(Symbol("WS", false));
	removeSymbols.push_back(Symbol("\\(", true));
	removeSymbols.push_back(Symbol("\\)", true));
	removeSymbols.push_back(Symbol("::", true));
	removeSymbols.push_back(Symbol(";", true));
	removeSymbols.push_back(Symbol("{", true));
	removeSymbols.push_back(Symbol("}", true));
	removeSymbols.push_back(Symbol("(", true));
	removeSymbols.push_back(Symbol(")", true));
	removeSymbols.push_back(Symbol("import", true)); //Don't need the actual text of the symbol
	removeSymbols.push_back(Symbol("interpreter_directive", false));
	removeSymbols.push_back(Symbol("if", true));
	removeSymbols.push_back(Symbol("while", true));
	removeSymbols.push_back(Symbol("__if_comp__", true));
	removeSymbols.push_back(Symbol("comp_simple_passthrough", true));

	for (int i = 0; i < removeSymbols.size(); i++)
		parseTree = RemovalTransformation<Symbol>(removeSymbols[i]).transform(parseTree);

	//Collapse Transformations
	std::vector<Symbol> collapseSymbols;

	collapseSymbols.push_back(Symbol("opt_typed_parameter_list", false));
	collapseSymbols.push_back(Symbol("opt_parameter_list", false));
	collapseSymbols.push_back(Symbol("opt_import_list", false));
	collapseSymbols.push_back(Symbol("import_list", false));
	collapseSymbols.push_back(Symbol("statement_list", false));
	collapseSymbols.push_back(Symbol("parameter_list", false));
	collapseSymbols.push_back(Symbol("typed_parameter_list", false));
	collapseSymbols.push_back(Symbol("unorderd_list_part", false));
	collapseSymbols.push_back(Symbol("if_comp_pred", false));

	for (int i = 0; i < collapseSymbols.size(); i++)
		parseTree = CollapseTransformation<Symbol>(collapseSymbols[i]).transform(parseTree);

	if (parseTree) {
		outFileTransformed << parseTree->DOTGraphString() << std::endl;
	} else {
		std::cout << "Tree returned from transformation is NULL!" << std::endl;
	}
	outFileTransformed.close();

	NodeTree<ASTData>* AST = ASTTransformation().transform(parseTree);
	if (AST) {
		outFileAST << AST->DOTGraphString() << std::endl;
	} else {
		std::cout << "Tree returned from ASTTransformation is NULL!" << std::endl;
	}
	outFileAST.close();

	//Do type checking, scope creation, etc. here.
	//None at this time, instead going straight to C in this first (more naive) version

	//Code generation
	//For right now, just C
	std::string c_code = CGenerator().generate(AST);
	outFileC << c_code << std::endl;
	outFileC.close();

	return(0);
}
 
