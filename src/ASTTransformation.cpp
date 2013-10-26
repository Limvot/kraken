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
	std::set<int> skipChildren;

	if (name == "translation_unit") {
		newNode = new NodeTree<ASTData>(name, ASTData(translation_unit));
	} else if (name == "interpreter_directive") {
		newNode = new NodeTree<ASTData>(name, ASTData(interpreter_directive));
	} else if (name == "import" && !current.isTerminal()) {
		newNode = new NodeTree<ASTData>(name, ASTData(import, Symbol(concatSymbolTree(children[0]), true)));
		return newNode; // Don't need children of import
	} else if (name == "identifier") {
		newNode = new NodeTree<ASTData>(name, ASTData(identifier, Symbol(concatSymbolTree(children[0]), true)));
	} else if (name == "function") {
		newNode = new NodeTree<ASTData>(name, ASTData(function, Symbol(concatSymbolTree(children[1]), true), ASTData::strToType(concatSymbolTree(children[0]))));
		skipChildren.insert(0);
		skipChildren.insert(1);
	} else if (name == "code_block") {
		newNode = new NodeTree<ASTData>(name, ASTData(code_block));
	} else if (name == "typed_parameter") {
		newNode = new NodeTree<ASTData>(name, ASTData(typed_parameter));
	} else if (name == "expression") {
		//If this is an actual part of an expression, not just a premoted term
		if (children.size() > 1) {
			std::string functionCallName = concatSymbolTree(children[1]);
			newNode = new NodeTree<ASTData>(functionCallName, ASTData(function_call, Symbol(functionCallName, true)));
			skipChildren.insert(1);
		} else {
			return transform(children[0]); //Just a promoted term, so do child
		}
	} else if (name == "term") {
		//If this is an actual part of an expression, not just a premoted factor
		if (children.size() > 1) {
			std::string functionCallName = concatSymbolTree(children[1]);
			newNode = new NodeTree<ASTData>(functionCallName, ASTData(function_call, Symbol(functionCallName, true)));
			skipChildren.insert(1);
		} else {
			return transform(children[0]); //Just a promoted factor, so do child
		}
	} else if (name == "factor") {
		return transform(children[0]); //Just a premoted number or function call or something, so use it instead
	} else if (name == "boolean_expression") {
		newNode = new NodeTree<ASTData>(name, ASTData(boolean_expression));
	} else if (name == "statement") {
		newNode = new NodeTree<ASTData>(name, ASTData(statement));
	} else if (name == "if_statement") {
		newNode = new NodeTree<ASTData>(name, ASTData(if_statement));
	} else if (name == "return_statement") {
		newNode = new NodeTree<ASTData>(name, ASTData(return_statement));
	} else if (name == "assignment_statement") {
		newNode = new NodeTree<ASTData>(name, ASTData(assignment_statement));
	} else if (name == "function_call") {
		//children[0] is scope
		std::string functionCallName = concatSymbolTree(children[1]);
		newNode = new NodeTree<ASTData>(functionCallName, ASTData(function_call, Symbol(functionCallName, true)));
		skipChildren.insert(1);
	} else if (name == "parameter") {
		return transform(children[0]); //Don't need a parameter node, just the value
	} else if (name == "bool") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(children[0]), true), boolean));
	} else if (name == "number") {
		return transform(children[0]);
	} else if (name == "integer") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(from), true), integer));
	} else if (name == "float") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(from), true), floating));
	} else if (name == "double") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(from), true), double_percision));
	} else if (name == "string") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(children[0]), true), char_string));
	} else {
		return new NodeTree<ASTData>();
	}

	// In general, iterate through children and do them. Might not do this for all children.
	for (int i = 0; i < children.size(); i++) {
		if (skipChildren.find(i) == skipChildren.end()) {
			NodeTree<ASTData>* transChild = transform(children[i]);
			if (transChild->getData().type)
				newNode->addChild(transChild);
			else
				delete transChild;
		}
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
