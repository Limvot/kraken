#ifndef __IMPORTER__H_
#define __IMPORTER__H_

#include <string>
#include <vector>
#include <iostream>
#include <fstream>

#include "Parser.h"
#include "NodeTree.h"
#include "ASTData.h"
#include "Symbol.h"
#include "RemovalTransformation.h"
#include "CollapseTransformation.h"
#include "ASTTransformation.h"

class Importer {
	public:
		Importer(Parser* parserIn, std::vector<std::string> includePaths);
		~Importer();
		NodeTree<ASTData>* import(std::string fileName);
		std::map<std::string, NodeTree<ASTData>*> getASTMap();
	private:
		std::vector<std::string> includePaths;
		Parser* parser;
		std::vector<Symbol> removeSymbols;
		std::vector<Symbol> collapseSymbols;
		std::map<std::string, NodeTree<ASTData>*> imported;
};

#endif