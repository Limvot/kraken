#include "Importer.h"

#ifdef _WIN32
	#include <unistd.h>
	#define mkdir( A, B ) mkdir(A)
#endif
Importer::Importer(Parser* parserIn, std::vector<std::string> includePaths, std::string outputNameIn, bool only_parseIn) {
    only_parse = only_parseIn;
	//constructor
    outputName = outputNameIn;

    if (!only_parse) {
        if (mkdir(("./" + outputName).c_str(), 0755)) {
            //std::cerr << "\n\n =====IMPORTER===== \n\n" << std::endl;
            //std::cerr << "Could not make directory " << outputName << std::endl;
        }
    }

	parser = parserIn;
	this->includePaths = includePaths;
	ASTTransformer = new ASTTransformation(this);

	removeSymbols.push_back(Symbol("$NULL$", true));
	removeSymbols.push_back(Symbol("WS", false));
	removeSymbols.push_back(Symbol("\\(", true));
	removeSymbols.push_back(Symbol("\\)", true));
	removeSymbols.push_back(Symbol("var", true));
	removeSymbols.push_back(Symbol("fun", true));

	removeSymbols.push_back(Symbol(";", true));
	removeSymbols.push_back(Symbol("line_end", false));
	removeSymbols.push_back(Symbol("{", true));
	removeSymbols.push_back(Symbol("}", true));
	removeSymbols.push_back(Symbol("(", true));
	removeSymbols.push_back(Symbol(")", true));
	//removeSymbols.push_back(Symbol("import", true));
	removeSymbols.push_back(Symbol("if", true));
	removeSymbols.push_back(Symbol("while", true));
	removeSymbols.push_back(Symbol("__if_comp__", true));
	//removeSymbols.push_back(Symbol("simple_passthrough", true));
	removeSymbols.push_back(Symbol("comp_simple_passthrough", true));
	removeSymbols.push_back(Symbol("def_nonterm", false));
	removeSymbols.push_back(Symbol("obj_nonterm", false));
	removeSymbols.push_back(Symbol("adt_nonterm", false));
	removeSymbols.push_back(Symbol("template", true));
	removeSymbols.push_back(Symbol("\\|", true));
	//removeSymbols.push_back(Symbol("match", true));
    collapseSymbols.push_back(Symbol("case_statement_list", false));
    collapseSymbols.push_back(Symbol("opt_param_assign_list", false));
	collapseSymbols.push_back(Symbol("param_assign_list", false));
	collapseSymbols.push_back(Symbol("opt_typed_parameter_list", false));
	collapseSymbols.push_back(Symbol("opt_parameter_list", false));
	collapseSymbols.push_back(Symbol("identifier_list", false));
	collapseSymbols.push_back(Symbol("adt_option_list", false));
	collapseSymbols.push_back(Symbol("statement_list", false));
	collapseSymbols.push_back(Symbol("parameter_list", false));
	collapseSymbols.push_back(Symbol("typed_parameter_list", false));
	collapseSymbols.push_back(Symbol("unorderd_list_part", false));
	collapseSymbols.push_back(Symbol("if_comp_pred", false));
	collapseSymbols.push_back(Symbol("declaration_block", false));
	collapseSymbols.push_back(Symbol("type_list", false));
	collapseSymbols.push_back(Symbol("opt_type_list", false));
	collapseSymbols.push_back(Symbol("template_param_list", false));
	collapseSymbols.push_back(Symbol("trait_list", false));
	collapseSymbols.push_back(Symbol("dec_type", false));
	//collapseSymbols.push_back(Symbol("pre_reffed", false));
}

Importer::~Importer() {
	//destructor
	delete ASTTransformer;
}

void Importer::registerAST(std::string name, NodeTree<ASTData>* ast, NodeTree<Symbol>* syntaxTree) {
	imported[name] = ast;
	importedTrips.push_back({name, ast, syntaxTree});
	std::cout << "REGISTERD " << name << std::endl;
}

NodeTree<ASTData>* Importer::getUnit(std::string fileName) {
	//std::cout << "\n\nImporting " << fileName << " ";
	//Check to see if we've already done it
	if (imported.find(fileName) != imported.end()) {
		//std::cout << "Already Imported!" << std::endl;
		return imported[fileName];
	}
	//std::cout << "Not yet imported" << std::endl;

	return NULL;
}

NodeTree<ASTData>* Importer::importFirstPass(std::string fileName) {
	NodeTree<ASTData>* ast = getUnit(fileName);
	if (ast == NULL) {
		NodeTree<Symbol>* parseTree = parseAndTrim(fileName);
        if (!parseTree)
            return NULL;
		//Call with ourself to allow the transformation to call us to import files that it needs
        if (!only_parse)
            ast = ASTTransformer->firstPass(fileName, parseTree); //This firstPass will register itself
	}
	return ast;
}

void Importer::import(std::string fileName) {

	//Start the ball rolling by importing and running the first pass on the first file.
	//This will import, first pass and register all the other files too.

	//std::cout << "\n\n =====FIRST PASS===== \n\n" << std::endl;
	importFirstPass(fileName);	//First pass defines all objects
    if (only_parse)
        return;

	std::cout << "\n\n =====SECOND PASS===== \n\n" << std::endl;
	for (importTriplet i : importedTrips)					//Second pass defines data inside objects, outside declaration statements,
		std::cout << "\n\nSecond pass for: " << i.name << std::endl, ASTTransformer->secondPass(i.ast, i.syntaxTree);		//function prototypes, and identifiers (as we now have all type defs)

	std::cout << "\n\n =====THIRD PASS===== \n\n" << std::endl;
	for (importTriplet i : importedTrips)					//Third pass does all function bodies
		std::cout << "\n\nThird pass for: " << i.name << std::endl, ASTTransformer->thirdPass(i.ast, i.syntaxTree);

	std::cout << "\n\n =====FOURTH PASS===== \n\n" << std::endl;
    bool changed = true;
    while (changed) {
        changed = false;
        for (importTriplet i : importedTrips) {					//Fourth pass finishes up by doing all template classes
            std::cout << "\n\nFourth pass for: " << i.name << std::endl;
            changed = changed ? changed : ASTTransformer->fourthPass(i.ast, i.syntaxTree);
        }
    }

	//Note that class template instantiation can happen in the second or third passes and that function template instantion
	//can happen in the third pass.

	std::ofstream outFileAST;
	for (importTriplet i : importedTrips) {
		std::string outputFileName = outputName + "/" + i.name + "out";
		outFileAST.open((outputFileName + ".AST.dot").c_str());
		if (!outFileAST.is_open()) {
			std::cout << "Problem opening second output file " << outputFileName + ".AST.dot" << "\n";
			return;
		}
		if (i.ast) {
			//outFileAST << i.ast->DOTGraphString() << std::endl;
		} else {
			std::cout << "Tree returned from ASTTransformation for " << fileName << " is NULL!" << std::endl;
		}
		outFileAST.close();
	}
}

NodeTree<Symbol>* Importer::parseAndTrim(std::string fileName) {

	std::ifstream programInFile;
	//std::ofstream outFile, outFileTransformed;

    //std::cout << "outputName " << outputName << std::endl;
    //std::cout << "fileName " << fileName << std::endl;

    auto pathPieces = split(fileName, '/');
    std::string outputFileName = outputName + "/" + pathPieces[pathPieces.size()-1] + "out";
    //std::cout << "outputFileName " << outputFileName << std::endl;

    std::string inputFileName;
	for (auto i : includePaths) {
		programInFile.open(i+fileName);
		if (programInFile.is_open()) {
            inputFileName = i+fileName;
			break;
        } else {
			std::cout << i+fileName << " is no good" << std::endl;
        }
	}
	if (!programInFile.is_open()) {
		std::cout << "Problem opening programInFile " << fileName << "\n";
		return NULL;
	}

	//outFile.open(outputFileName);
	//if (!outFile.is_open()) {
		//std::cout << "Probelm opening output file " << outputFileName << "\n";
		//return NULL;
	//}

	//outFileTransformed.open((outputFileName + ".transformed.dot").c_str());
	//if (!outFileTransformed.is_open()) {
		//std::cout << "Probelm opening second output file " << outputFileName + ".transformed.dot" << "\n";
		//return NULL;
	//}

	std::string programInputFileString, line;
	while(programInFile.good()) {
		getline(programInFile, line);
		programInputFileString.append(line+"\n");
	}
	programInFile.close();

	//std::cout << programInputFileString << std::endl;
	NodeTree<Symbol>* parseTree = parser->parseInput(programInputFileString, inputFileName);

	if (parseTree) {
        //std::cout << parseTree->DOTGraphString() << std::endl;
		//outFile << parseTree->DOTGraphString() << std::endl;
	} else {
		std::cout << "ParseTree returned from parser for " << fileName << " is NULL!" << std::endl;
		//outFile.close(); outFileTransformed.close();
        throw "unexceptablblllll";
        return NULL;
    }
	//outFile.close();

	//Remove Transformations

	for (int i = 0; i < removeSymbols.size(); i++)
		parseTree = RemovalTransformation<Symbol>(removeSymbols[i]).transform(parseTree);

	//Collapse Transformations

	for (int i = 0; i < collapseSymbols.size(); i++)
		parseTree = CollapseTransformation<Symbol>(collapseSymbols[i]).transform(parseTree);

	if (parseTree) {
		//outFileTransformed << parseTree->DOTGraphString() << std::endl;
	} else {
		std::cout << "Tree returned from transformation is NULL!" << std::endl;
	}
	//outFileTransformed.close();

    std::cout << "Returning parse tree" << std::endl;
	return parseTree;
}

std::map<std::string, NodeTree<ASTData>*> Importer::getASTMap() {
	return imported;
}
