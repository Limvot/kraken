#ifndef ASTTRANSFORMATION_H
#define ASTTRANSFORMATION_H

#include <set>
#include <map>

#include "ASTData.h"
#include "NodeTransformation.h"

class ASTTransformation: public NodeTransformation<Symbol,ASTData> {
	public:
		ASTTransformation();
		~ASTTransformation();
		virtual NodeTree<ASTData>* transform(NodeTree<Symbol>* from);
		NodeTree<ASTData>* transform(NodeTree<Symbol>* from, NodeTree<ASTData>* scope);
		std::string concatSymbolTree(NodeTree<Symbol>* root);
		NodeTree<ASTData>* scopeLookup(NodeTree<ASTData>* scope, std::string lookup);
	private:
		//Nothing
};
#endif
