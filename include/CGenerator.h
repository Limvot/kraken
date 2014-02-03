#ifndef CGENERATOR_H
#define CGENERATOR_H

#include <string>
#include <iostream>
#include <fstream>

#include "NodeTree.h"
#include "ASTData.h"
#include "Type.h"

#include "util.h"


class CGenerator {
	public:
		CGenerator();
		~CGenerator();
		void generateCompSet(std::map<std::string, NodeTree<ASTData>*> ASTs, std::string outputName);
		std::string generate(NodeTree<ASTData>* from, NodeTree<ASTData>* enclosingObject = NULL);
		static std::string ValueTypeToCType(Type *type);
		std::string generateObjectMethod(NodeTree<ASTData>* enclosingObject, NodeTree<ASTData>* from);

		std::string generatorString;
	private:
		std::string tabs();
		int tabLevel;
};
#endif