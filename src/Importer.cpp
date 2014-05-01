#include "Importer.h"

Importer::Importer(Parser* parserIn, std::vector<std::string> includePaths) {
	//constructor
	parser = parserIn;
	this->includePaths = includePaths;

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
	removeSymbols.push_back(Symbol("typedef", true));

	collapseSymbols.push_back(Symbol("opt_typed_parameter_list", false));
	collapseSymbols.push_back(Symbol("opt_parameter_list", false));
	collapseSymbols.push_back(Symbol("opt_import_list", false));
	collapseSymbols.push_back(Symbol("import_list", false));
	collapseSymbols.push_back(Symbol("statement_list", false));
	collapseSymbols.push_back(Symbol("parameter_list", false));
	collapseSymbols.push_back(Symbol("typed_parameter_list", false));
	collapseSymbols.push_back(Symbol("unorderd_list_part", false));
	collapseSymbols.push_back(Symbol("if_comp_pred", false));
	collapseSymbols.push_back(Symbol("declaration_block", false));
}

Importer::~Importer() {
	//destructor
}

NodeTree<ASTData>* Importer::import(std::string fileName) {
	//Check to see if we've already done it
	if (imported.find(fileName) != imported.end())
		return imported[fileName];

	std::ifstream programInFile;
	std::ofstream outFile, outFileTransformed, outFileAST;

	std::string outputName = fileName + "out";
	
	for (auto i : includePaths) {
		programInFile.open(i+fileName);
		if (programInFile.is_open())
			break;
	}
	if (!programInFile.is_open()) {
		std::cout << "Problem opening programInFile " << fileName << "\n";
		return NULL;
	}

	outFile.open(outputName);
	if (!outFile.is_open()) {
		std::cout << "Probelm opening output file " << outputName << "\n";
		return NULL;
	}

	outFileTransformed.open((outputName + ".transformed.dot").c_str());
	if (!outFileTransformed.is_open()) {
		std::cout << "Probelm opening second output file " << outputName + ".transformed.dot" << "\n";
		return NULL;
	}

	outFileAST.open((outputName + ".AST.dot").c_str());
	if (!outFileAST.is_open()) {
		std::cout << "Probelm opening second output file " << outputName + ".AST.dot" << "\n";
		return NULL;
	}

	std::string programInputFileString, line;
	while(programInFile.good()) {
		getline(programInFile, line);
		programInputFileString.append(line+"\n");
	}
	programInFile.close();

	//std::cout << programInputFileString << std::endl;
	NodeTree<Symbol>* parseTree = parser->parseInput(programInputFileString);

	if (parseTree) {
		//std::cout << parseTree->DOTGraphString() << std::endl;
		outFile << parseTree->DOTGraphString() << std::endl;
	} else {
		std::cout << "ParseTree returned from parser is NULL!" << std::endl;
	}
	outFile.close();

	//Remove Transformations

	for (int i = 0; i < removeSymbols.size(); i++)
		parseTree = RemovalTransformation<Symbol>(removeSymbols[i]).transform(parseTree);

	//Collapse Transformations

	for (int i = 0; i < collapseSymbols.size(); i++)
		parseTree = CollapseTransformation<Symbol>(collapseSymbols[i]).transform(parseTree);

	if (parseTree) {
		outFileTransformed << parseTree->DOTGraphString() << std::endl;
	} else {
		std::cout << "Tree returned from transformation is NULL!" << std::endl;
	}
	outFileTransformed.close();

	//Call with ourself to allow the transformation to call us to import files that it needs
	NodeTree<ASTData>* AST = ASTTransformation(this).transform(parseTree);

	if (AST) {
		outFileAST << AST->DOTGraphString() << std::endl;
	} else {
		std::cout << "Tree returned from ASTTransformation is NULL!" << std::endl;
	}
	outFileAST.close();

	imported[fileName] = AST;

	return AST;
}

std::map<std::string, NodeTree<ASTData>*> Importer::getASTMap() {
	return imported;
}
