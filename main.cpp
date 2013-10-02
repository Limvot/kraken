#include <string>
#include <iostream>
#include <fstream>
#include <vector>

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


int main(int argc, char* argv[]) {
	
	std::ifstream programInFile, grammerInFile;
	std::ofstream outFile, outFileTransformed, outFileAST;

	programInFile.open(argv[1]);
	if (!programInFile.is_open()) {
		std::cout << "Problem opening programInFile " << argv[1] << "\n";
		return(1);
	}

	grammerInFile.open(argv[2]);
	if (!grammerInFile.is_open()) {
		std::cout << "Problem opening grammerInFile " << argv[2] << "\n";
		return(1);
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

	//Read the input file into a string
	std::string programInputFileString, grammerInputFileString;
	std::string line;
	while(grammerInFile.good()) {
		getline(grammerInFile, line);
		grammerInputFileString.append(line+"\n");
	}

	while(programInFile.good()) {
		getline(programInFile, line);
		programInputFileString.append(line+"\n");
	}

	//LALRParser parser;
	RNGLRParser parser;
	parser.loadGrammer(grammerInputFileString);
	//std::cout << "Creating State Set from Main" << std::endl;
	std::cout << "\nState Set" << std::endl;
	parser.createStateSet();
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

	//Pre AST Transformations
	std::vector<NodeTransformation<Symbol, Symbol>*> preASTTransforms;
	//Remove Transformations
	preASTTransforms.push_back(new RemovalTransformation<Symbol>(Symbol("WS", false)));
	preASTTransforms.push_back(new RemovalTransformation<Symbol>(Symbol("\\(", true)));
	preASTTransforms.push_back(new RemovalTransformation<Symbol>(Symbol("\\)", true)));
	//preASTTransforms.push_back(new RemovalTransformation<Symbol>(Symbol("/", true)));
	preASTTransforms.push_back(new RemovalTransformation<Symbol>(Symbol("::", true)));
	preASTTransforms.push_back(new RemovalTransformation<Symbol>(Symbol(";", true)));
	preASTTransforms.push_back(new RemovalTransformation<Symbol>(Symbol("{", true)));
	preASTTransforms.push_back(new RemovalTransformation<Symbol>(Symbol("}", true)));
	//Collapse Transformations
	preASTTransforms.push_back(new CollapseTransformation<Symbol>(Symbol("opt_typed_parameter_list", false)));
	preASTTransforms.push_back(new CollapseTransformation<Symbol>(Symbol("opt_parameter_list", false)));
	for (int i = 0; i < preASTTransforms.size(); i++) {
		parseTree = preASTTransforms[i]->transform(parseTree);
	}
	preASTTransforms.erase(preASTTransforms.begin(), preASTTransforms.end());

	NodeTree<ASTData>* AST = ASTTransformation().transform(parseTree);
	//NodeTree<ASTData>* AST = (new ASTTransformation())->transform(parseTree);

	if (parseTree) {
		outFileTransformed << parseTree->DOTGraphString() << std::endl;
	} else {
		std::cout << "Tree returned from transformation is NULL!" << std::endl;
	}


	if (AST) {
		outFileTransformed << AST->DOTGraphString() << std::endl;
	} else {
		std::cout << "Tree returned from ASTTransformation is NULL!" << std::endl;
	}

	programInFile.close();
	grammerInFile.close();
	outFile.close();
	outFileTransformed.close();
	outFileAST.close();

	return(0);
}
 