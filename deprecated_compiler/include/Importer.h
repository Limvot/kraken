#ifndef __IMPORTER__H_
#define __IMPORTER__H_

#include <string>
#include <vector>
#include <iostream>
#include <fstream>

#include <sys/stat.h>

#include "Parser.h"
#include "NodeTree.h"
#include "ASTData.h"
#include "Symbol.h"
#include "RemovalTransformation.h"
#include "CollapseTransformation.h"
#include "ASTTransformation.h"

class ASTTransformation;

class Importer {
	public:
		Importer(Parser* parserIn, std::vector<std::string> includePaths, std::string outputName, bool only_parseIn = false);
		~Importer();
		void import(std::string fileName);
		NodeTree<ASTData>* getUnit(std::string fileName);
		NodeTree<ASTData>* importFirstPass(std::string fileName);
		NodeTree<Symbol>* parseAndTrim(std::string fileName);
		void registerAST(std::string name, NodeTree<ASTData>* ast, NodeTree<Symbol>* syntaxTree);
		std::map<std::string, NodeTree<ASTData>*> getASTMap();
	private:
        std::string outputName;
		ASTTransformation *ASTTransformer;
		struct importTriplet {
			std::string name;
			NodeTree<ASTData>* ast;
			NodeTree<Symbol>* syntaxTree;
		};
        bool only_parse;
		std::vector<importTriplet> importedTrips;
		std::vector<std::string> includePaths;
		Parser* parser;
		std::vector<Symbol> removeSymbols;
		std::vector<Symbol> collapseSymbols;
		std::map<std::string, NodeTree<ASTData>*> imported;
};

#endif
