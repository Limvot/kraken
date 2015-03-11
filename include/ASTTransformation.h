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

		//First pass defines all type_defs (objects and ailises)
		NodeTree<ASTData>* firstPass(std::string fileName, NodeTree<Symbol>* parseTree);
        std::set<std::string> parseTraits(NodeTree<Symbol>* traitsNode);

		//Second pass defines data inside objects, outside declaration statements, and function prototpyes (since we have type_defs now)
		void secondPass(NodeTree<ASTData>* ast, NodeTree<Symbol>* parseTree);
        void secondPassDoClassInsides(NodeTree<ASTData>* typeDef, std::vector<NodeTree<Symbol>*> typedefChildren, std::map<std::string, Type*> templateTypeReplacements);
		NodeTree<ASTData>* secondPassDeclaration(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::map<std::string, Type*> templateTypeReplacements);
		NodeTree<ASTData>* secondPassFunction(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::map<std::string, Type*> templateTypeReplacements);

		//Third pass redoes all imports to import the new function prototypes and identifiers
		void thirdPass(NodeTree<ASTData>* ast);

		//The fourth pass finishes up by doing all function bodies
		void fourthPass(NodeTree<ASTData>* ast, NodeTree<Symbol>* parseTree);
		NodeTree<ASTData>* searchScopeForFunctionDef(NodeTree<ASTData>* scope, NodeTree<Symbol>* parseTree, std::map<std::string, Type*> templateTypeReplacements);
		void fourthPassFunction(NodeTree<Symbol>* from, NodeTree<ASTData>* functionDef, std::map<std::string, Type*> templateTypeReplacements);

		virtual NodeTree<ASTData>* transform(NodeTree<Symbol>* from);
		NodeTree<ASTData>* transform(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::vector<Type> types, std::map<std::string, Type*> templateTypeReplacements);
		std::vector<NodeTree<ASTData>*> transformChildren(std::vector<NodeTree<Symbol>*> children, std::set<int> skipChildren, NodeTree<ASTData>* scope, std::vector<Type> types, std::map<std::string, Type*> templateTypeReplacements);
		std::vector<Type> mapNodesToTypes(std::vector<NodeTree<ASTData>*> nodes);
		std::string concatSymbolTree(NodeTree<Symbol>* root);
		NodeTree<ASTData>* doFunction(NodeTree<ASTData>* scope, std::string lookup, std::vector<NodeTree<ASTData>*> nodes, std::map<std::string, Type*> templateTypeReplacements);

        NodeTree<ASTData>* functionLookup(NodeTree<ASTData>* scope, std::string lookup, std::vector<Type> types);
        NodeTree<ASTData>* templateFunctionLookup(NodeTree<ASTData>* scope, std::string lookup, std::vector<Type*> templateInstantiationTypes, std::vector<Type> types);
        std::vector<NodeTree<ASTData>*> scopeLookup(NodeTree<ASTData>* scope, std::string lookup, bool includeModules = false);
        std::vector<NodeTree<ASTData>*> scopeLookup(NodeTree<ASTData>* scope, std::string lookup, bool includeModules, std::vector<NodeTree<ASTData>*> visited);

        NodeTree<ASTData>* getUpperTranslationUnit(NodeTree<ASTData>* node);
        Type* typeFromTypeNode(NodeTree<Symbol>* typeNode, NodeTree<ASTData>* scope, std::map<std::string, Type*> templateTypeReplacements);
        NodeTree<ASTData>* templateClassLookup(NodeTree<ASTData>* scope, std::string name, std::vector<Type*> templateInstantiationTypes);
		NodeTree<ASTData>* findOrInstantiateFunctionTemplate(std::vector<NodeTree<Symbol>*> children, NodeTree<ASTData>* scope, std::vector<Type> types, std::map<std::string, Type*> templateTypeReplacements);
        std::map<std::string, Type*> makeTemplateFunctionTypeMap(NodeTree<Symbol>* templateNode, std::vector<Type*> types);
        std::vector<std::pair<std::string, std::set<std::string>>> makeTemplateNameTraitPairs(NodeTree<Symbol>* templateNode);
	private:
		Importer * importer;
		std::map<std::string, std::vector<NodeTree<ASTData>*>> languageLevelReservedWords;
		std::map<std::string, std::vector<NodeTree<ASTData>*>> languageLevelOperators;
		NodeTree<ASTData>* topScope; //maintained for templates that need to add themselves to the top scope no matter where they are instantiated
};

#endif
