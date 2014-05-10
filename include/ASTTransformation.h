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
		virtual NodeTree<ASTData>* transform(NodeTree<Symbol>* from);
		NodeTree<ASTData>* transform(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::vector<Type> types, std::map<std::string, Type*> templateTypeReplacements);
		std::vector<NodeTree<ASTData>*> transformChildren(std::vector<NodeTree<Symbol>*> children, std::set<int> skipChildren, NodeTree<ASTData>* scope, std::vector<Type> types, std::map<std::string, Type*> templateTypeReplacements);
		std::vector<Type> mapNodesToTypes(std::vector<NodeTree<ASTData>*> nodes);
		std::string concatSymbolTree(NodeTree<Symbol>* root);
		NodeTree<ASTData>* doFunction(NodeTree<ASTData>* scope, std::string lookup, std::vector<NodeTree<ASTData>*> nodes, std::map<std::string, Type*> templateTypeReplacements);
		NodeTree<ASTData>* scopeLookup(NodeTree<ASTData>* scope, std::string lookup, std::vector<Type> types = std::vector<Type>());
		Type* typeFromTypeNode(NodeTree<Symbol>* typeNode, NodeTree<ASTData>* scope, std::map<std::string, Type*> templateTypeReplacements);
	private:
		Importer * importer;
		std::map<std::string, std::vector<NodeTree<ASTData>*>> languageLevelScope;
		NodeTree<ASTData>* topScope; //maintained for templates that need to add themselves to the top scope no matter where they are instantiated
};

#endif
