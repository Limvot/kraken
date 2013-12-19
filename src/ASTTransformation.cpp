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
		newNode = transform(children[1]); //Transform to get the identifier
		newNode->getDataRef()->valueType = ASTData::strToType(concatSymbolTree(children[0])); //Get the type (left child) and set our new identifer to be that type
		return newNode;
	} else if (name == "boolean_expression") {
		//If this is an actual part of an expression, not just a premoted term
		if (children.size() > 1) {
			std::string functionCallName = concatSymbolTree(children[1]);
			newNode = new NodeTree<ASTData>(functionCallName, ASTData(function_call, Symbol(functionCallName, true)));
			skipChildren.insert(1);
		} else {
			return transform(children[0]); //Just a promoted term, so do child
		}
	} else if (name == "and_boolean_expression") {
		//If this is an actual part of an expression, not just a premoted bool_exp
		if (children.size() > 1) {
			std::string functionCallName = concatSymbolTree(children[1]);
			newNode = new NodeTree<ASTData>(functionCallName, ASTData(function_call, Symbol(functionCallName, true)));
			skipChildren.insert(1);
		} else {
			return transform(children[0]); //Just a promoted bool_exp, so do child
		}
	} else if (name == "bool_exp") {
		//If this is an actual part of an expression, not just a premoted bool_exp.
		if (children.size() > 1) {
			std::string functionCallName = concatSymbolTree(children[1]);
			newNode = new NodeTree<ASTData>(functionCallName, ASTData(function_call, Symbol(functionCallName, true)));
			skipChildren.insert(1);
		} else {
			return transform(children[0]); //Just a promoted bool_exp, so do child
		}
	//Here's the order of ops stuff
	} else if (name == "expression" || name == "shiftand" || name == "term" || name == "factor" || name == "unarad") {
		//If this is an actual part of an expression, not just a premoted child
		if (children.size() > 1) {
			std::string functionCallName = concatSymbolTree(children[1]);
			std::cout << functionCallName << std::endl;
			newNode = new NodeTree<ASTData>(functionCallName, ASTData(function_call, Symbol(functionCallName, true)));
			skipChildren.insert(1);
		} else {
			return transform(children[0]); //Just a promoted child, so do it instead
		}
	} else if (name == "statement") {
		newNode = new NodeTree<ASTData>(name, ASTData(statement));
	} else if (name == "if_statement") {
		newNode = new NodeTree<ASTData>(name, ASTData(if_statement));
	} else if (name == "while_loop") {
		newNode = new NodeTree<ASTData>(name, ASTData(while_loop));
	} else if (name == "for_loop") {
		newNode = new NodeTree<ASTData>(name, ASTData(for_loop));
	} else if (name == "return_statement") {
		newNode = new NodeTree<ASTData>(name, ASTData(return_statement));
	} else if (name == "assignment_statement") {
		newNode = new NodeTree<ASTData>(name, ASTData(assignment_statement));
		std::string assignFuncName = concatSymbolTree(children[1]);
		if (assignFuncName == "=") {
			newNode->addChild(transform(children[0]));
			newNode->addChild(transform(children[2]));
		} else {
			//For assignments like += or *=, expand the syntatic sugar.
			NodeTree<ASTData>* lhs = transform(children[0]);
			NodeTree<ASTData>* childCall = new NodeTree<ASTData>(assignFuncName.substr(0,1), ASTData(function_call, Symbol(assignFuncName.substr(0,1), true)));
			childCall->addChild(lhs);
			childCall->addChild(transform(children[2]));
			newNode->addChild(lhs);
			newNode->addChild(childCall);
		}
		return newNode;
	} else if (name == "declaration_statement") {
		newNode = new NodeTree<ASTData>(name, ASTData(declaration_statement));
		NodeTree<ASTData>* newIdentifier = transform(children[1]); //Transform the identifier
		newIdentifier->getDataRef()->valueType = ASTData::strToType(concatSymbolTree(children[0]));//set the type of the identifier
		newNode->addChild(newIdentifier);
		skipChildren.insert(0); //These, the type and the identifier, have been taken care of.
		skipChildren.insert(1);
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
			if (transChild->getData().type) //Only add the children that have a real ASTData::ASTType, that is, legit ASTData.
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
