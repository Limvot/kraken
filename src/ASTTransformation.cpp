#include "ASTTransformation.h"

ASTTransformation::ASTTransformation() {
	//
}

ASTTransformation::~ASTTransformation() {
	//
}

NodeTree<ASTData>* ASTTransformation::transform(NodeTree<Symbol>* from) {
	Symbol current = from->getData();
	std::string name = current.getName();
	NodeTree<ASTData>* newNode;
	std::vector<NodeTree<Symbol>*> children = from->getChildren();

	if (name == "translation_unit") {
		newNode = new NodeTree<ASTData>(name, ASTData(translation_unit));
	} else if (name == "import" && !current.isTerminal()) {
		newNode = new NodeTree<ASTData>(name, ASTData(import, Symbol(concatSymbolTree(children[0]), true)));
		return newNode; // Don't need children of import
	} else if (name == "function") {
		newNode = new NodeTree<ASTData>(name, ASTData(function, Symbol(concatSymbolTree(children[1]), true), ASTData::strToType(concatSymbolTree(children[0]))));
	} else {
		return new NodeTree<ASTData>();
	}

	// In general, iterate through children and do them. Might not do this for all children.
	for (int i = 0; i < children.size(); i++) {
		newNode->addChild(transform(children[i]));
	}

	return newNode;
}

std::string ASTTransformation::concatSymbolTree(NodeTree<Symbol>* root) {
	std::string concatString;
	std::string ourValue = root->getData().getValue();
	if (ourValue != "NoValue")
		concatString += ourValue;
	std::vector<NodeTree<Symbol>*> children = root->getChildren();
	for (int i = 0; i < children.size(); i++) {
		concatString = concatSymbolTree(children[i]);	
	}
	return concatString;
}
