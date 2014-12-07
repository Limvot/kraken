#ifndef CGENERATOR_H
#define CGENERATOR_H

#include <string>
#include <iostream>
#include <fstream>

#include "NodeTree.h"
#include "ASTData.h"
#include "Type.h"

#include "util.h"
#include "Poset.h"


class CGenerator {
	public:
		CGenerator();
		~CGenerator();
		void generateCompSet(std::map<std::string, NodeTree<ASTData>*> ASTs, std::string outputName);
        std::string generateClassStruct(NodeTree<ASTData>* from);
		std::string generate(NodeTree<ASTData>* from, NodeTree<ASTData>* enclosingObject = NULL);
        std::string generateAliasChains(NodeTree<ASTData>* scopeNode, NodeTree<ASTData>* definition);
		static std::string ValueTypeToCType(Type *type);
		static std::string ValueTypeToCTypeDecoration(Type *type);
		static std::string CifyName(std::string name);
		std::string generateObjectMethod(NodeTree<ASTData>* enclosingObject, NodeTree<ASTData>* from, std::string *functionPrototype);
        NodeTree<ASTData>* getMethodsObjectType(NodeTree<ASTData>* scope, std::string functionName);
		std::string generatorString;
	private:
		std::string tabs();
		int tabLevel;
};
#endif
