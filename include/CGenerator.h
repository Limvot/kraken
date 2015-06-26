#ifndef CGENERATOR_H
#define CGENERATOR_H

#include <string>
#include <iostream>
#include <fstream>
#include <utility>
#include <stack>
#include <sys/stat.h>

#include "CCodeTriple.h"
#include "NodeTree.h"
#include "ASTData.h"
#include "Type.h"
// for mapNodesToTypes
#include "ASTTransformation.h"

#include "util.h"
#include "Poset.h"

// Note the use of std::pair to hold two strings - the running string for the header file and the running string for  the c file.

enum ClosureTypeSpecialType { ClosureTypeRegularNone, ClosureFunctionPointerTypeWithClosedParam };

class CGenerator {
	public:
		CGenerator();
		~CGenerator();
		void generateCompSet(std::map<std::string, NodeTree<ASTData>*> ASTs, std::string outputName);
        std::string generateClassStruct(NodeTree<ASTData>* from);
        bool isUnderTranslationUnit(NodeTree<ASTData>* from, NodeTree<ASTData>* typeDefinition);
        NodeTree<ASTData>* highestScope(NodeTree<ASTData>* node);
        std::pair<std::string, std::string> generateTranslationUnit(std::string name, std::map<std::string, NodeTree<ASTData>*> ASTs);
		CCodeTriple generate(NodeTree<ASTData>* from, NodeTree<ASTData>* enclosingObject = NULL, bool justFuncName = false, NodeTree<ASTData>* enclosingFunction = NULL);
        std::string generateAliasChains(std::map<std::string, NodeTree<ASTData>*> ASTs, NodeTree<ASTData>* definition);

        std::string closureStructType(std::set<NodeTree<ASTData>*> closedVariables);
		std::string ValueTypeToCType(Type *type, std::string, ClosureTypeSpecialType closureSpecial = ClosureTypeRegularNone);
		std::string ValueTypeToCTypeDecoration(Type *type, ClosureTypeSpecialType closureSpecial = ClosureTypeRegularNone);
        std::string ValueTypeToCTypeThingHelper(Type *type, std::string ptrStr, ClosureTypeSpecialType closureSpecial);
		static std::string CifyName(std::string name);
		static std::string scopePrefix(NodeTree<ASTData>* from);
        std::string generateObjectMethod(NodeTree<ASTData>* enclosingObject, NodeTree<ASTData>* from, std::string *functionPrototype);
        NodeTree<ASTData>* getMethodsObjectType(NodeTree<ASTData>* scope, std::string functionName);
        NodeTree<ASTData>* getMethod(Type* type, std::string method, std::vector<Type> types);
        bool methodExists(Type* type, std::string method, std::vector<Type> types);
        std::string generateMethodIfExists(Type* type, std::string method, std::string parameter, std::vector<Type> methodTypes);
        std::string emitDestructors(std::vector<NodeTree<ASTData>*> possibleDeclarations, NodeTree<ASTData>* enclosingObject);
		std::string tabs();
		std::string getID();

		int tabLevel;
		int id;
        std::string function_header;
		std::string generatorString;
        std::string linkerString;
        std::string functionTypedefString;
        std::map<Type, std::pair<std::string, std::string>> functionTypedefMap;
        std::map<std::set<NodeTree<ASTData>*>, std::string> closureStructMap;
        std::vector<std::vector<NodeTree<ASTData>*>> distructDoubleStack;
        std::stack<int> loopDistructStackDepth;
        std::vector<std::vector<NodeTree<ASTData>*>> deferDoubleStack;
        std::stack<int> loopDeferStackDepth;
	private:
};
#endif
