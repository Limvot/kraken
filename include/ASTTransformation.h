#ifndef ASTTRANSFORMATION_H
#define ASTTRANSFORMATION_H

#include <set>
#include <map>

#include <iterator>
#include <algorithm>

#include "Type.h"
#include "ASTData.h"
#include "NodeTransformation.h"
#include "Importer.h"

class Importer;

class ASTTransformation: public NodeTransformation<Symbol,ASTData> {
	public:
		ASTTransformation(Importer* importerIn);
		~ASTTransformation();

        NodeTree<Symbol>* getNode(std::string lookup, std::vector<NodeTree<Symbol>*> nodes);
        NodeTree<Symbol>* getNode(std::string lookup, NodeTree<Symbol>* parent);
        std::vector<NodeTree<Symbol>*> getNodes(std::string lookup, std::vector<NodeTree<Symbol>*> nodes);
        std::vector<NodeTree<Symbol>*> getNodes(std::string lookup, NodeTree<Symbol>* parent);

		//First pass defines all type_defs (objects and ailises)
		NodeTree<ASTData>* firstPass(std::string fileName, NodeTree<Symbol>* parseTree);
        std::set<std::string> parseTraits(NodeTree<Symbol>* traitsNode);

		//Second pass defines data inside objects, outside declaration statements, and function prototpyes (since we have type_defs now)
		void secondPass(NodeTree<ASTData>* ast, NodeTree<Symbol>* parseTree);
        void secondPassDoClassInsides(NodeTree<ASTData>* typeDef, std::vector<NodeTree<Symbol>*> typedefChildren, std::map<std::string, Type*> templateTypeReplacements);
		NodeTree<ASTData>* secondPassDeclaration(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::map<std::string, Type*> templateTypeReplacements);
		NodeTree<ASTData>* secondPassFunction(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::map<std::string, Type*> templateTypeReplacements);

		//The third pass finishes up by doing all function bodies
		void thirdPass(NodeTree<ASTData>* ast, NodeTree<Symbol>* parseTree);
		NodeTree<ASTData>* searchScopeForFunctionDef(NodeTree<ASTData>* scope, NodeTree<Symbol>* parseTree, std::map<std::string, Type*> templateTypeReplacements);
		void thirdPassFunction(NodeTree<Symbol>* from, NodeTree<ASTData>* functionDef, std::map<std::string, Type*> templateTypeReplacements);

		virtual NodeTree<ASTData>* transform(NodeTree<Symbol>* from);
		NodeTree<ASTData>* transform(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::vector<Type> types, bool limitToFunction, std::map<std::string, Type*> templateTypeReplacements);
		std::vector<NodeTree<ASTData>*> transformChildren(std::vector<NodeTree<Symbol>*> children, std::set<int> skipChildren, NodeTree<ASTData>* scope, std::vector<Type> types, bool limitToFunction, std::map<std::string, Type*> templateTypeReplacements);
		std::vector<Type> mapNodesToTypes(std::vector<NodeTree<ASTData>*> nodes);
		std::vector<Type*> mapNodesToTypePointers(std::vector<NodeTree<ASTData>*> nodes);
		std::string concatSymbolTree(NodeTree<Symbol>* root);
		NodeTree<ASTData>* doFunction(NodeTree<ASTData>* scope, std::string lookup, std::vector<NodeTree<ASTData>*> nodes, std::map<std::string, Type*> templateTypeReplacements);

        std::set<NodeTree<ASTData>*> findVariablesToClose(NodeTree<ASTData>* func, NodeTree<ASTData>* stat);
        bool inScopeChain(NodeTree<ASTData>* node, NodeTree<ASTData>* scope);
        NodeTree<ASTData>* functionLookup(NodeTree<ASTData>* scope, std::string lookup, std::vector<Type> types);
        NodeTree<ASTData>* templateFunctionLookup(NodeTree<ASTData>* scope, std::string lookup, std::vector<Type*>* templateInstantiationTypes, std::vector<Type> types, std::map<std::string, Type*> scopeTypeMap);
        std::vector<NodeTree<ASTData>*> scopeLookup(NodeTree<ASTData>* scope, std::string lookup, bool includeModules = false);
        std::vector<NodeTree<ASTData>*> scopeLookup(NodeTree<ASTData>* scope, std::string lookup, bool includeModules, std::set<NodeTree<ASTData>*> visited);

        NodeTree<ASTData>* getUpperTranslationUnit(NodeTree<ASTData>* node);
        NodeTree<ASTData>* addToScope(std::string name, NodeTree<ASTData>* toAdd, NodeTree<ASTData>* addTo);
        Type* typeFromTypeNode(NodeTree<Symbol>* typeNode, NodeTree<ASTData>* scope, std::map<std::string, Type*> templateTypeReplacements);
        NodeTree<ASTData>* templateClassLookup(NodeTree<ASTData>* scope, std::string name, std::vector<Type*> templateInstantiationTypes);
        void unifyType(NodeTree<Symbol> *syntaxType, Type type, std::map<std::string, Type>* templateTypeMap, std::map<std::string, Type*> typeMap);
        void unifyTemplateFunction(NodeTree<ASTData>* templateFunction, std::vector<Type> types, std::vector<Type*>* templateInstantiationTypes, std::map<std::string, Type*> typeMap);
		NodeTree<ASTData>* findOrInstantiateFunctionTemplate(std::vector<NodeTree<Symbol>*> children, NodeTree<ASTData>* scope, std::vector<Type> types, std::map<std::string, Type*> templateTypeReplacements);
        std::map<std::string, Type*> makeTemplateFunctionTypeMap(NodeTree<Symbol>* templateNode, std::vector<Type*> types, std::map<std::string, Type*> scopeTypeMap);
        std::vector<std::pair<std::string, std::set<std::string>>> makeTemplateNameTraitPairs(NodeTree<Symbol>* templateNode);

	private:
		Importer * importer;
		NodeTree<ASTData>* builtin_trans_unit; // the top scope for language level stuff
		std::map<std::string, std::vector<NodeTree<ASTData>*>> languageLevelReservedWords;
		std::map<std::string, std::vector<NodeTree<ASTData>*>> languageLevelOperators;
		NodeTree<ASTData>* topScope; //maintained for templates that need to add themselves to the top scope no matter where they are instantiated
        int lambdaID = 0;
};

#endif
