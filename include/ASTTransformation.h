#ifndef ASTTRANSFORMATION_H
#define ASTTRANSFORMATION_H

#include <set>
#include <map>

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

		//Second pass defines data inside objects, outside declaration statements, and function prototpyes (since we have type_defs now)
		void secondPass(NodeTree<ASTData>* ast, NodeTree<Symbol>* parseTree);
        void secondPassDoClassInsides(NodeTree<ASTData>* typeDef, std::vector<NodeTree<Symbol>*> typedefChildren);
		NodeTree<ASTData>* secondPassDeclaration(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::map<std::string, Type*> templateTypeReplacements);
		NodeTree<ASTData>* secondPassFunction(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::map<std::string, Type*> templateTypeReplacements);

		//Third pass redoes all imports to import the new function prototypes and identifiers
		void thirdPass(NodeTree<ASTData>* ast);

		//The fourth pass finishes up by doing all function bodies
		void fourthPass(NodeTree<ASTData>* ast, NodeTree<Symbol>* parseTree);
		NodeTree<ASTData>* seachScopeForFunctionDef(NodeTree<ASTData>* scope, NodeTree<Symbol>* parseTree, std::map<std::string, Type*> templateTypeReplacements);
		void fourthPassFunction(NodeTree<Symbol>* from, NodeTree<ASTData>* functionDef, std::map<std::string, Type*> templateTypeReplacements);

		virtual NodeTree<ASTData>* transform(NodeTree<Symbol>* from);
		NodeTree<ASTData>* transform(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::vector<Type> types, std::map<std::string, Type*> templateTypeReplacements, bool instantiateTemplates);
		std::vector<NodeTree<ASTData>*> transformChildren(std::vector<NodeTree<Symbol>*> children, std::set<int> skipChildren, NodeTree<ASTData>* scope, std::vector<Type> types, std::map<std::string, Type*> templateTypeReplacements, bool instantiateTemplates);
		std::vector<Type> mapNodesToTypes(std::vector<NodeTree<ASTData>*> nodes);
		std::string concatSymbolTree(NodeTree<Symbol>* root);
		NodeTree<ASTData>* doFunction(NodeTree<ASTData>* scope, std::string lookup, std::vector<NodeTree<ASTData>*> nodes, std::map<std::string, Type*> templateTypeReplacements);
		NodeTree<ASTData>* scopeLookup(NodeTree<ASTData>* scope, std::string lookup, std::vector<Type> types = std::vector<Type>());

        Type* typeFromTypeNode(NodeTree<Symbol>* typeNode, NodeTree<ASTData>* scope, std::map<std::string, Type*> templateTypeReplacements, bool instantiateTemplates);
		NodeTree<ASTData>* findOrInstantiateFunctionTemplate(std::vector<NodeTree<Symbol>*> children, NodeTree<ASTData>* scope, std::vector<Type> types, std::map<std::string, Type*> templateTypeReplacements);
	private:
		Importer * importer;
		std::map<std::string, std::vector<NodeTree<ASTData>*>> languageLevelScope;
		NodeTree<ASTData>* topScope; //maintained for templates that need to add themselves to the top scope no matter where they are instantiated
};

#endif
