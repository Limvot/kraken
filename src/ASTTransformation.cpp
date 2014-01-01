#include "ASTTransformation.h"

ASTTransformation::ASTTransformation(Importer *importerIn) {
	//
	importer = importerIn;
}

ASTTransformation::~ASTTransformation() {
	//
}

NodeTree<ASTData>* ASTTransformation::transform(NodeTree<Symbol>* from) {
	//Set up top scope
	return transform(from, NULL);
}

NodeTree<ASTData>* ASTTransformation::transform(NodeTree<Symbol>* from, NodeTree<ASTData>* scope) {
	Symbol current = from->getData();
	std::string name = current.getName();
	NodeTree<ASTData>* newNode;
	std::vector<NodeTree<Symbol>*> children = from->getChildren();
	std::set<int> skipChildren;

	if (name == "translation_unit") {
		newNode = new NodeTree<ASTData>(name, ASTData(translation_unit));
		scope = newNode;
		//Temporary scope fix
		scope->getDataRef()->scope["+"] = new NodeTree<ASTData>();
		scope->getDataRef()->scope["-"] = new NodeTree<ASTData>();
		scope->getDataRef()->scope["*"] = new NodeTree<ASTData>();
		scope->getDataRef()->scope["&"] = new NodeTree<ASTData>();
		scope->getDataRef()->scope["--"] = new NodeTree<ASTData>();
		scope->getDataRef()->scope["++"] = new NodeTree<ASTData>();
		scope->getDataRef()->scope["=="] = new NodeTree<ASTData>();
		scope->getDataRef()->scope["<="] = new NodeTree<ASTData>();
		scope->getDataRef()->scope[">="] = new NodeTree<ASTData>();
		scope->getDataRef()->scope["<"] = new NodeTree<ASTData>();
		scope->getDataRef()->scope[">"] = new NodeTree<ASTData>();
		scope->getDataRef()->scope["&&"] = new NodeTree<ASTData>();
		scope->getDataRef()->scope["||"] = new NodeTree<ASTData>();
		scope->getDataRef()->scope["!"] = new NodeTree<ASTData>();
		scope->getDataRef()->scope["*="] = new NodeTree<ASTData>();
		scope->getDataRef()->scope["+="] = new NodeTree<ASTData>();
		scope->getDataRef()->scope["-="] = new NodeTree<ASTData>();

	} else if (name == "interpreter_directive") {
		newNode = new NodeTree<ASTData>(name, ASTData(interpreter_directive));
	} else if (name == "import" && !current.isTerminal()) {
		std::string toImport = concatSymbolTree(children[0]);
		newNode = new NodeTree<ASTData>(name, ASTData(import, Symbol(toImport, true)));
		//Do the imported file too
		NodeTree<ASTData>* outsideTranslationUnit = importer->import(toImport + ".krak");
		scope->getDataRef()->scope[toImport] = outsideTranslationUnit; //Put this transation_unit in the scope as it's files name
		//Now add it to scope
		for (auto i = outsideTranslationUnit->getDataRef()->scope.begin(); i != outsideTranslationUnit->getDataRef()->scope.end(); i++)
			scope->getDataRef()->scope[i->first] = i->second;
		return newNode; // Don't need children of import
	} else if (name == "identifier") {
		std::string lookupName = concatSymbolTree(children[0]);
		//std::cout << "scope lookup from identifier" << std::endl;
		newNode = scopeLookup(scope, lookupName);
		if (newNode == NULL) {
			std::cout << "scope lookup error! Could not find " << lookupName << std::endl;
			throw "LOOKUP ERROR: " + lookupName; 
		}
		//newNode = new NodeTree<ASTData>(name, ASTData(identifier, Symbol(concatSymbolTree(children[0]), true)));
	} else if (name == "function") {
		std::string functionName = concatSymbolTree(children[1]);
		newNode = new NodeTree<ASTData>(name, ASTData(function, Symbol(functionName, true), Type(concatSymbolTree(children[0]))));
		skipChildren.insert(0);
		skipChildren.insert(1);
		scope->getDataRef()->scope[functionName] = newNode;
		newNode->getDataRef()->scope["~enclosing_scope"] = scope;
		scope = newNode;
	} else if (name == "code_block") {
		newNode = new NodeTree<ASTData>(name, ASTData(code_block));
		newNode->getDataRef()->scope["~enclosing_scope"] = scope;
		scope = newNode;
	} else if (name == "typed_parameter") {
		//newNode = transform(children[1]); //Transform to get the identifier
		std::string parameterName = concatSymbolTree(children[1]);
		std::string typeString = concatSymbolTree(children[0]);//Get the type (left child) and set our new identifer to be that type
		newNode = new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(parameterName, true), Type(typeString)));
		scope->getDataRef()->scope[parameterName] = newNode;
		return newNode;
	} else if (name == "boolean_expression" || name == "and_boolean_expression" || name == "bool_exp") {
		//If this is an actual part of an expression, not just a premoted term
		if (children.size() > 1) {
			std::string functionCallName = concatSymbolTree(children[1]);
			//std::cout << "scope lookup from boolen_expression or similar" << std::endl;
			NodeTree<ASTData>* function = scopeLookup(scope, functionCallName);
			if (function == NULL) {
				std::cout << "scope lookup error! Could not find " << functionCallName << std::endl;
				throw "LOOKUP ERROR: " + functionCallName; 
			}
			newNode = new NodeTree<ASTData>(functionCallName, ASTData(function_call, Symbol(functionCallName, true)));
			newNode->addChild(function); // First child of function call is a link to the function definition
			skipChildren.insert(1);
		} else {
			//std::cout << children.size() << std::endl;
			if (children.size() == 0)
				return new NodeTree<ASTData>();
			return transform(children[0], scope); //Just a promoted term, so do child
		}
	//Here's the order of ops stuff
	} else if (name == "expression" || name == "shiftand" || name == "term" || name == "unarad") { //unarad can ride through, it should always just be a promoted child
		//If this is an actual part of an expression, not just a premoted child
		if (children.size() > 2) {
			std::string functionCallName = concatSymbolTree(children[1]);
			//std::cout << "scope lookup from expression or similar" << std::endl;
			NodeTree<ASTData>* function = scopeLookup(scope, functionCallName);
			if (function == NULL) {
				std::cout << "scope lookup error! Could not find " << functionCallName << std::endl;
				throw "LOOKUP ERROR: " + functionCallName; 
			}
			newNode = new NodeTree<ASTData>(functionCallName, ASTData(function_call, Symbol(functionCallName, true)));
			newNode->addChild(function); // First child of function call is a link to the function definition
			skipChildren.insert(1);
		} else {
			return transform(children[0], scope); //Just a promoted child, so do it instead
		}
	} else if (name == "factor") { //Do factor here, as it has all the weird unary operators
				//If this is an actual part of an expression, not just a premoted child
		//NO SUPPORT FOR CASTING YET
		if (children.size() == 2) {
			std::string funcName = concatSymbolTree(children[0]);
			int funcNum;
			if (funcName == "*" || funcName == "&" || funcName == "++" || funcName == "--" || funcName == "-" || funcName == "!" || funcName == "~")
				funcNum = 0;
			else
				funcName = concatSymbolTree(children[1]), funcNum = 1;

			//std::cout << "scope lookup from factor" << std::endl;
			NodeTree<ASTData>* function = scopeLookup(scope, funcName);
			if (function == NULL) {
				std::cout << "scope lookup error! Could not find " << funcName << std::endl;
				throw "LOOKUP ERROR: " + funcName; 
			}
			newNode = new NodeTree<ASTData>(funcName, ASTData(function_call, Symbol(funcName, true)));
			newNode->addChild(function);
			skipChildren.insert(funcNum);
		} else {
			return transform(children[0], scope); //Just a promoted child, so do it instead
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
			newNode->addChild(transform(children[0], scope));
			newNode->addChild(transform(children[2], scope));
		} else {
			//For assignments like += or *=, expand the syntatic sugar.
			NodeTree<ASTData>* lhs = transform(children[0], scope);
			std::string functionName = assignFuncName.substr(0,1);
			NodeTree<ASTData>* childCall = new NodeTree<ASTData>(functionName, ASTData(function_call, Symbol(functionName, true)));
			NodeTree<ASTData>* functionDef = scopeLookup(scope, functionName);
			if (functionDef == NULL) {
				std::cout << "scope lookup error! Could not find " << functionName << std::endl;
				throw "LOOKUP ERROR: " + functionName; 
			}
			childCall->addChild(functionDef); //First child of function call is definition of the function
			childCall->addChild(lhs);
			childCall->addChild(transform(children[2], scope));
			newNode->addChild(lhs);
			newNode->addChild(childCall);
		}
		return newNode;
	} else if (name == "declaration_statement") {
		newNode = new NodeTree<ASTData>(name, ASTData(declaration_statement));

		// NodeTree<ASTData>* newIdentifier = transform(children[1], scope); //Transform the identifier
		// newIdentifier->getDataRef()->valueType = Type(concatSymbolTree(children[0]));//set the type of the identifier
		
		std::string newIdentifierStr = concatSymbolTree(children[1]);
		std::string typeString = concatSymbolTree(children[0]);//Get the type (left child) and set our new identifer to be that type
		NodeTree<ASTData>* newIdentifier = new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(newIdentifierStr, true), Type(typeString)));
		scope->getDataRef()->scope[newIdentifierStr] = newIdentifier;
		
		newNode->addChild(newIdentifier);
		skipChildren.insert(0); //These, the type and the identifier, have been taken care of.
		skipChildren.insert(1);
	} else if (name == "if_comp") {
		newNode = new NodeTree<ASTData>(name, ASTData(if_comp));
		newNode->addChild(new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(concatSymbolTree(children[0]),true))));
		skipChildren.insert(0); //Don't do the identifier. The identifier lookup will fail. That's why we do it here.
	} else if (name == "simple_passthrough") {
		newNode = new NodeTree<ASTData>(name, ASTData(simple_passthrough));
	} else if (name == "function_call") {
		//children[0] is scope
		std::string functionCallName = concatSymbolTree(children[1]);
		newNode = new NodeTree<ASTData>(functionCallName, ASTData(function_call, Symbol(functionCallName, true)));
		//std::cout << "scope lookup from function_call" << std::endl;
		NodeTree<ASTData>* function = scopeLookup(scope, functionCallName);
		if (function == NULL) {
			std::cout << "scope lookup error! Could not find " << functionCallName << std::endl;
			throw "LOOKUP ERROR: " + functionCallName; 
		}
		newNode->addChild(function);
		skipChildren.insert(1);
	} else if (name == "parameter") {
		return transform(children[0], scope); //Don't need a parameter node, just the value
	} else if (name == "parameter") {
		return transform(children[0], scope); //Don't need a parameter node, just the value
	} else if (name == "type") {
		std::string theConcat = concatSymbolTree(from); //We have no symbol, so this will concat our children
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(theConcat, true), Type(theConcat)));
	} else if (name == "number") {
		return transform(children[0], scope);
	} else if (name == "integer") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(from), true), Type(integer)));
	} else if (name == "float") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(from), true), Type(floating)));
	} else if (name == "double") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(from), true), Type(double_percision)));
	} else if (name == "char") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(children[0]), true), Type(character, 1))); //Indirection of 1 for array
	} else if (name == "string" || name == "triple_quoted_string") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(children[0]), true), Type(character, 1))); //Indirection of 1 for array
	} else {
		return new NodeTree<ASTData>();
	}

	// In general, iterate through children and do them. Might not do this for all children.
	for (int i = 0; i < children.size(); i++) {
		if (skipChildren.find(i) == skipChildren.end()) {
			NodeTree<ASTData>* transChild = transform(children[i], scope);
			if (transChild->getDataRef()->type) //Only add the children that have a real ASTData::ASTType, that is, legit ASTData.
				newNode->addChild(transChild);
			else
				delete transChild;
		}
	}

	return newNode;
}

std::string ASTTransformation::concatSymbolTree(NodeTree<Symbol>* root) {
	std::string concatString;
	std::string ourValue = root->getDataRef()->getValue();
	if (ourValue != "NoValue")
		concatString += ourValue;
	std::vector<NodeTree<Symbol>*> children = root->getChildren();
	for (int i = 0; i < children.size(); i++) {
		concatString += concatSymbolTree(children[i]);	
	}
	return concatString;
}

NodeTree<ASTData>* ASTTransformation::scopeLookup(NodeTree<ASTData>* scope, std::string lookup) {
	//Seach the map
	auto scopeMap = scope->getDataRef()->scope;
	//std::cout << "scope size: " << scopeMap.size() << ", scope from " << scope->getName() << std::endl;
	// for (auto i = scopeMap.begin(); i != scopeMap.end(); i++)
	// 	std::cout << i->first << " : " << i-> second << " - " << i->second->getName() << std::endl;

	auto elementIterator = scopeMap.find(lookup);
	if (elementIterator != scopeMap.end()) {
	//	std::cout << "lookup of " << lookup << " succeded in first scope!" << std::endl;
		return elementIterator->second;
	}
	//std::cout << "lookup of " << lookup << " failed in first scope, checking for upper scope" << std::endl;
	//if it doesn't exist, try the enclosing scope if it exists.
	auto enclosingIterator = scopeMap.find("~enclosing_scope");
	if (enclosingIterator != scopeMap.end()) {
	//	std::cout << "upper scope exists, searching it for " << lookup << std::endl;
		return scopeLookup(enclosingIterator->second, lookup);
	}
	//std::cout << "upper scope does not exist" << std::endl;
	return NULL;
}
