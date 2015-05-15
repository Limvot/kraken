#ifndef CGENERATOR_H
#define CGENERATOR_H

#include <string>
#include <iostream>
#include <fstream>
#include <utility>
#include <stack>
#include <sys/stat.h>

#include "NodeTree.h"
#include "ASTData.h"
#include "Type.h"

#include "util.h"
#include "Poset.h"

// Note the use of std::pair to hold two strings - the running string for the header file and the running string for  the c file.

class CGenerator {
	public:
		CGenerator();
		~CGenerator();
		void generateCompSet(std::map<std::string, NodeTree<ASTData>*> ASTs, std::string outputName);
        std::string generateClassStruct(NodeTree<ASTData>* from);
        bool isUnderTranslationUnit(NodeTree<ASTData>* from, NodeTree<ASTData>* typeDefinition);
        NodeTree<ASTData>* highestScope(NodeTree<ASTData>* node);
        std::pair<std::string, std::string> generateTranslationUnit(std::string name, std::map<std::string, NodeTree<ASTData>*> ASTs);
		std::string generate(NodeTree<ASTData>* from, NodeTree<ASTData>* enclosingObject = NULL);
        std::string generateAliasChains(std::map<std::string, NodeTree<ASTData>*> ASTs, NodeTree<ASTData>* definition);
		static std::string ValueTypeToCType(Type *type);
		static std::string ValueTypeToCTypeDecoration(Type *type);
        static std::string ValueTypeToCTypeThingHelper(Type *type, std::string ptrStr);
		static std::string CifyName(std::string name);
		static std::string scopePrefix(NodeTree<ASTData>* from);
        std::string generateObjectMethod(NodeTree<ASTData>* enclosingObject, NodeTree<ASTData>* from, std::string *functionPrototype);
        NodeTree<ASTData>* getMethodsObjectType(NodeTree<ASTData>* scope, std::string functionName);
		std::string tabs();

		int tabLevel;
		std::string generatorString;
        std::string linkerString;
        std::vector<std::vector<NodeTree<ASTData>*>> deferDoubleStack;
        std::stack<int> loopDeferStackDepth;
	private:
};
#endif
