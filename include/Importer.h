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
		Importer(Parser* parserIn);
		~Importer();
		NodeTree<ASTData>* import(std::string fileName);
	private:
		Parser* parser;
		std::vector<Symbol> removeSymbols;
		std::vector<Symbol> collapseSymbols;
};

#endif