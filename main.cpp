#include "NodeTree.h"
#include "Lexer.h"
#include "Parser.h"
#include <string>
#include <iostream>
#include <fstream>


int main(int argc, char* argv[]) {
	
	std::ifstream programInFile, grammerInFile;
	std::ofstream outFile;

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

	NodeTree root;
	root.setName("Root");
	root.addChild(new NodeTree("SomeChild"));
	root.addChild(new NodeTree("SomeOtherChild"));
	root.get(0)->addChild(new NodeTree("Grandchildren"));

	//outFile << root.DOTGraphString() << std::endl;


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

	Parser parser;
	parser.loadGrammer(grammerInputFileString);
	//std::cout << "Creating State Set from Main" << std::endl;
	parser.createStateSet();
	//std::cout << "finished State Set from Main" << std::endl;
	//std::cout << "Doing stateSetToString from Main" << std::endl;
	std::cout << parser.stateSetToString() << std::endl;
	//std::cout << "finished stateSetToString from Main" << std::endl;
	std::cout << parser.tableToString() << std::endl;

	std::cout << grammerInputFileString << std::endl;
	std::cout << parser.grammerToString() << std::endl;
	//std::cout << parser.grammerToDOT() << std::endl;

	//outFile << parser.grammerToDOT() << std::endl;

	std::cout << programInputFileString << std::endl;
	NodeTree* parseTree = parser.parseInput(new Lexer(programInputFileString));

	if (parseTree) {
		std::cout << parseTree->DOTGraphString() << std::endl;
		outFile << parseTree->DOTGraphString() << std::endl;
	}

	programInFile.close();
	grammerInFile.close();
	outFile.close();

	return(0);
}
 