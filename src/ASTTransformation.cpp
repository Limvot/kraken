#include "ASTTransformation.h"

ASTTransformation::ASTTransformation(Importer *importerIn) {
	importer = importerIn;
	//Set up language level special scope. (the final scope checked)
	//Note the NULL type
	languageLevelScope["+"].push_back( new NodeTree<ASTData>("function", ASTData(function, Symbol("+", true), NULL)));
	languageLevelScope["-"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("-", true), NULL)));
	languageLevelScope["*"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("*", true), NULL)));
	languageLevelScope["&"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("&", true), NULL)));
	languageLevelScope["--"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("--", true), NULL)));
	languageLevelScope["++"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("++", true), NULL)));
	languageLevelScope["=="].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("==", true), NULL)));
	languageLevelScope["<="].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("<=", true), NULL)));
	languageLevelScope[">="].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol(">=", true), NULL)));
	languageLevelScope["<"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("<", true), NULL)));
	languageLevelScope[">"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol(">", true), NULL)));
	languageLevelScope["&&"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("&&", true), NULL)));
	languageLevelScope["||"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("||", true), NULL)));
	languageLevelScope["!"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("!", true), NULL)));
	languageLevelScope["*="].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("*=", true), NULL)));
	languageLevelScope["+="].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("+=", true), NULL)));
	languageLevelScope["-="].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("-=", true), NULL)));
	languageLevelScope["."].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol(".", true), NULL)));
	languageLevelScope["->"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("->", true), NULL)));
}

ASTTransformation::~ASTTransformation() {
	//
}

NodeTree<ASTData>* ASTTransformation::transform(NodeTree<Symbol>* from) {
	//Set up top scope
	return transform(from, NULL, std::vector<Type>());
}

NodeTree<ASTData>* ASTTransformation::transform(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::vector<Type> types) {
	Symbol current = from->getData();
	std::string name = current.getName();
	NodeTree<ASTData>* newNode = NULL;
	std::vector<NodeTree<Symbol>*> children = from->getChildren();
	std::set<int> skipChildren;

	if (name == "translation_unit") {
		newNode = new NodeTree<ASTData>(name, ASTData(translation_unit));
		scope = newNode;
	} else if (name == "interpreter_directive") {
		newNode = new NodeTree<ASTData>(name, ASTData(interpreter_directive));
	} else if (name == "import" && !current.isTerminal()) {
		std::string toImport = concatSymbolTree(children[0]);
		newNode = new NodeTree<ASTData>(name, ASTData(import, Symbol(toImport, true)));
		//Do the imported file too
		NodeTree<ASTData>* outsideTranslationUnit = importer->import(toImport + ".krak");
		scope->getDataRef()->scope[toImport].push_back(outsideTranslationUnit); //Put this transation_unit in the scope as it's files name
		//Now add it to scope
		for (auto i = outsideTranslationUnit->getDataRef()->scope.begin(); i != outsideTranslationUnit->getDataRef()->scope.end(); i++)
			for (auto j : i->second)
				scope->getDataRef()->scope[i->first].push_back(j);
		return newNode; // Don't need children of import
	} else if (name == "identifier") {
		//Make sure we get the entire name
		std::string lookupName = concatSymbolTree(from);
		std::cout << "Looking up: " << lookupName << std::endl;
		newNode = scopeLookup(scope, lookupName, types);
		if (newNode == NULL) {
			std::cout << "scope lookup error! Could not find " << lookupName << " in identifier " << std::endl;
			throw "LOOKUP ERROR: " + lookupName; 
		} else if (newNode->getDataRef()->symbol.getName() !=lookupName) {
			//This happens when the lookup name denotes a member of an object, i.e. obj.foo
			//The newNode points to obj, not foo.
		}
		//newNode = new NodeTree<ASTData>(name, ASTData(identifier, Symbol(concatSymbolTree(children[0]), true)));
	} else if (name == "type_def") {
		std::string typeAlias = concatSymbolTree(children[0]);
		//If it is an alisis of a type
		if (children[1]->getData().getName() == "type") {
			newNode = new NodeTree<ASTData>(name, ASTData(type_def, Symbol(typeAlias, true, typeAlias), typeFromString(concatSymbolTree(children[1]), scope)));
			skipChildren.insert(1); //Don't want any children, it's unnecessary for ailising
		} else { //Is a struct or class
			newNode = new NodeTree<ASTData>(name, ASTData(type_def, Symbol(typeAlias, true, typeAlias)));
			newNode->getDataRef()->valueType = new Type(newNode); //Type is self-referential since this is the definition
		}
		scope->getDataRef()->scope[typeAlias].push_back(newNode);
		newNode->getDataRef()->scope["~enclosing_scope"].push_back(scope);
		scope = newNode;
		skipChildren.insert(0); //Identifier lookup will be ourselves, as we just added ourselves to the scope
		//return newNode;
	} else if (name == "function") {
		std::string functionName = concatSymbolTree(children[1]);
		newNode = new NodeTree<ASTData>(name, ASTData(function, Symbol(functionName, true), typeFromString(concatSymbolTree(children[0]), scope)));
		skipChildren.insert(0);
		skipChildren.insert(1);
		scope->getDataRef()->scope[functionName].push_back(newNode);
		newNode->getDataRef()->scope["~enclosing_scope"].push_back(scope);
		scope = newNode;
		
		// auto transChildren = transformChildren(children, skipChildren, scope, types);
		// std::cout << functionName << " ";
		// for (auto i : transChildren)
		// 	std::cout << "||" << i->getDataRef()->toString() << "|| ";
		// std::cout << "??||" << std::endl;
		// newNode->addChildren(transChildren);
		// return newNode;
		
		std::cout << "finished function " << functionName << std::endl;
	} else if (name == "code_block") {
		newNode = new NodeTree<ASTData>(name, ASTData(code_block));
		newNode->getDataRef()->scope["~enclosing_scope"].push_back(scope);
		scope = newNode;
	} else if (name == "typed_parameter") {
		//newNode = transform(children[1]); //Transform to get the identifier
		std::string parameterName = concatSymbolTree(children[1]);
		std::string typeString = concatSymbolTree(children[0]);//Get the type (left child) and set our new identifer to be that type
		newNode = new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(parameterName, true), typeFromString(typeString, scope)));
		scope->getDataRef()->scope[parameterName].push_back(newNode);
		newNode->getDataRef()->scope["~enclosing_scope"].push_back(scope);
		return newNode;
	} else if (name == "boolean_expression" || name == "and_boolean_expression" || name == "bool_exp") {
		//If this is an actual part of an expression, not just a premoted term
		if (children.size() > 1) {
			//We do children first so we can do appropriate scope searching with types (yay operator overloading!)
			skipChildren.insert(1);
			std::vector<NodeTree<ASTData>*> transformedChildren = transformChildren(children, skipChildren, scope, types);
			std::string functionCallString = concatSymbolTree(children[1]);
			NodeTree<ASTData>* function = doFunction(scope, functionCallString, transformedChildren);
			if (function == NULL) {
				std::cout << "scope lookup error! Could not find " << functionCallString << " in boolean stuff " << std::endl;
				throw "LOOKUP ERROR: " + functionCallString; 
			}
			newNode = function;
			// newNode = new NodeTree<ASTData>(functionCallString, ASTData(function_call, function->getDataRef()->valueType));
			// newNode->addChild(function); // First child of function call is a link to the function
			// newNode->addChildren(transformedChildren);
		} else {
			//std::cout << children.size() << std::endl;
			if (children.size() == 0)
				return new NodeTree<ASTData>();
			return transform(children[0], scope, types); //Just a promoted term, so do child
		}
	//Here's the order of ops stuff
	} else if (name == "expression" || name == "shiftand" || name == "term" || name == "unarad" || name == "access_operation") { //unarad can ride through, it should always just be a promoted child
		//If this is an actual part of an expression, not just a premoted child
		if (children.size() > 2) {
			NodeTree<ASTData>* lhs = transform(children[0], scope); //LHS does not inherit types
			NodeTree<ASTData>* rhs;
			if (name == "access_operation") 
				rhs = transform(children[2], lhs->getDataRef()->valueType->typeDefinition, types); //If an access operation, then the right side will be in the lhs's type's scope
			else
				rhs = transform(children[2], scope, types);

			std::string functionCallName = concatSymbolTree(children[1]);
			//std::cout << "scope lookup from expression or similar" << std::endl;
			std::vector<NodeTree<ASTData>*> transformedChildren; transformedChildren.push_back(lhs); transformedChildren.push_back(rhs);
			NodeTree<ASTData>* function = doFunction(scope, functionCallName, transformedChildren);
			if (function == NULL) {
				std::cout << "scope lookup error! Could not find " << functionCallName << " in expression " << std::endl;
				throw "LOOKUP ERROR: " + functionCallName; 
			}
			newNode = function;
			// newNode = new NodeTree<ASTData>(functionCallName, ASTData(function_call, Symbol(functionCallName, true)));
			// newNode->addChild(function); // First child of function call is a link to the function definition
			// newNode->addChild(lhs);
			// newNode->addChild(rhs);

			// if (name == "access_operation")
			// 	std::cout << "Access Operation: " << lhs->getDataRef()->symbol.getName() << " : " << rhs->getDataRef()->symbol.getName() << std::endl;
			// std::cout << functionCallName << " - " << function->getName() << " has value type " << function->getDataRef()->valueType  << " and rhs " << rhs->getDataRef()->valueType << std::endl;
			// //Set the value of this function call
			if (function->getDataRef()->valueType)
				newNode->getDataRef()->valueType = function->getDataRef()->valueType;
			else if (rhs->getDataRef()->valueType)
				newNode->getDataRef()->valueType = rhs->getDataRef()->valueType;
			else
				newNode->getDataRef()->valueType = NULL;
			std::cout << "function call to " << functionCallName << " - " << function->getName() << " is now " << newNode->getDataRef()->valueType  << std::endl;
			return newNode;
			//skipChildren.insert(1);
		} else {
			return transform(children[0], scope, types); //Just a promoted child, so do it instead
		}
	} else if (name == "factor") { //Do factor here, as it has all the weird unary operators
		//If this is an actual part of an expression, not just a premoted child
		//NO SUPPORT FOR CASTING YET
		if (children.size() == 2) {
			std::string funcName = concatSymbolTree(children[0]);
			NodeTree<ASTData>* param;
			if (funcName == "*" || funcName == "&" || funcName == "++" || funcName == "--" || funcName == "-" || funcName == "!" || funcName == "~")
				param = transform(children[1], scope, types);
			else
				funcName = concatSymbolTree(children[1]), param = transform(children[0], scope, types);

			//std::cout << "scope lookup from factor" << std::endl;
			std::vector<NodeTree<ASTData>*> transformedChildren; transformedChildren.push_back(param);
			NodeTree<ASTData>* function = doFunction(scope, funcName, transformedChildren);
			if (function == NULL) {
				std::cout << "scope lookup error! Could not find " << funcName  << " in factor " << std::endl;
				throw "LOOKUP ERROR: " + funcName; 
			}
			newNode = function;
			// newNode = new NodeTree<ASTData>(funcName, ASTData(function_call, Symbol(funcName, true)));
			// newNode->addChild(function);
			// newNode->addChild(param);
			// if (function->getDataRef()->valueType)
			// 	newNode->getDataRef()->valueType = function->getDataRef()->valueType;
			// else
			// 	newNode->getDataRef()->valueType = param->getDataRef()->valueType;

			return newNode;
		} else {
			return transform(children[0], scope, types); //Just a promoted child, so do it instead
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
			newNode->addChild(transform(children[0], scope, types));
			newNode->addChild(transform(children[2], scope, types));
		} else {
			//For assignments like += or *=, expand the syntatic sugar.
			NodeTree<ASTData>* lhs = transform(children[0], scope, types);
			NodeTree<ASTData>* rhs = transform(children[2], scope, types);
			std::vector<NodeTree<ASTData>*> transformedChildren; transformedChildren.push_back(lhs); transformedChildren.push_back(rhs);
			std::string functionName = assignFuncName.substr(0,1);
			NodeTree<ASTData>* operatorCall = doFunction(scope, functionName, transformedChildren);
			if (operatorCall == NULL) {
				std::cout << "scope lookup error! Could not find " << functionName  << " in assignment_statement " << std::endl;
				throw "LOOKUP ERROR: " + functionName; 
			}
			newNode->addChild(lhs);
			newNode->addChild(operatorCall);
		}
		return newNode;
	} else if (name == "declaration_statement") {
		newNode = new NodeTree<ASTData>(name, ASTData(declaration_statement));

		// NodeTree<ASTData>* newIdentifier = transform(children[1], scope); //Transform the identifier
		// newIdentifier->getDataRef()->valueType = Type(concatSymbolTree(children[0]));//set the type of the identifier
		std::string newIdentifierStr = concatSymbolTree(children[1]);
		std::string typeString = concatSymbolTree(children[0]);//Get the type (left child) and set our new identifer to be that type
		Type* identifierType = typeFromString(typeString, scope);
		NodeTree<ASTData>* newIdentifier = new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(newIdentifierStr, true), identifierType));
		scope->getDataRef()->scope[newIdentifierStr].push_back(newIdentifier);
		newNode->getDataRef()->scope["~enclosing_scope"].push_back(scope);
		//Now we don't do this thing
		// if (identifierType->typeDefinition) {
		// 	//Is a custom type. Populate this declaration's scope with it's inner declarations
		// 	std::vector<NodeTree<ASTData>*> definitions = identifierType->typeDefinition->getChildren();
		// 	for (auto i : definitions) {
		// 		//Point to the identifier. May need to change so it points to the declaration or something, with new declarations.....
		// 		newIdentifier->getDataRef()->scope[i->get(0)->getDataRef()->symbol.getName()] = i->get(0); //make each declaration's name point to it's definition, like above
		// 	}
		// }
		
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
		std::string functionCallName = concatSymbolTree(children[0]);
		newNode = new NodeTree<ASTData>(functionCallName, ASTData(function_call, Symbol(functionCallName, true)));
		// if (function == NULL) {
		// 	std::cout << "scope lookup error! Could not find " << functionCallName << " in function_call " << std::endl;
		// 	throw "LOOKUP ERROR: " + functionCallName; 
		// }
		skipChildren.insert(0);
		std::vector<NodeTree<ASTData>*> transformedChildren = transformChildren(children, skipChildren, scope, types);
		std::cout << "scope lookup from function_call: " << functionCallName << std::endl;
		for (auto i : children)
			std::cout << i << " : " << i->getName()  << " : " << i->getDataRef()->getName() << std::endl;

		NodeTree<ASTData>* function = transform(children[0], scope, mapNodesToTypes(transformedChildren));
		std::cout << "The thing: " << function << " : " << function->getName() << std::endl;
		for (auto i : function->getChildren())
			std::cout << i->getName() << " ";
		std::cout << std::endl;
		newNode->addChild(function);
		newNode->getDataRef()->valueType = function->getDataRef()->valueType;
		newNode->addChildren(transformedChildren);
		return newNode;
	} else if (name == "parameter") {
		return transform(children[0], scope, types); //Don't need a parameter node, just the value
	} else if (name == "type") {
		std::string theConcat = concatSymbolTree(from); //We have no symbol, so this will concat our children
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(theConcat, true), typeFromString(theConcat, scope)));
	} else if (name == "number") {
		return transform(children[0], scope, types);
	} else if (name == "integer") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(from), true), new Type(integer)));
	} else if (name == "float") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(from), true), new Type(floating)));
	} else if (name == "double") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(from), true), new Type(double_percision)));
	} else if (name == "char") { //Is this correct? This might be a useless old thing
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(children[0]), true), new Type(character, 1))); //Indirection of 1 for array
	} else if (name == "string" || name == "triple_quoted_string") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(children[0]), true), new Type(character, 1))); //Indirection of 1 for array
	}else if (name == "character") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(children[0]), true), new Type(character, 0))); //Indirection of 0 for character
	} else {
		return new NodeTree<ASTData>();
	}

	//Do all children but the ones we skip
	for (int i = 0; i < children.size(); i++) {
		if (skipChildren.find(i) == skipChildren.end()) {
			NodeTree<ASTData>* transChild = transform(children[i], scope, types);
			if (transChild->getDataRef()->type) //Only add the children that have a real ASTData::ASTType, that is, legit ASTData.
				newNode->addChild(transChild);
			else
				delete transChild;
		}
	}
	return newNode;
}

//We use this functionality a lot at different places
std::vector<NodeTree<ASTData>*> ASTTransformation::transformChildren(std::vector<NodeTree<Symbol>*> children, std::set<int> skipChildren, NodeTree<ASTData>* scope, std::vector<Type> types) {
	std::vector<NodeTree<ASTData>*> transformedChildren;
	// In general, iterate through children and do them. Might not do this for all children.
	for (int i = 0; i < children.size(); i++) {
		if (skipChildren.find(i) == skipChildren.end()) {
			NodeTree<ASTData>* transChild = transform(children[i], scope, types);
			if (transChild->getDataRef()->type) //Only add the children that have a real ASTData::ASTType, that is, legit ASTData.
				transformedChildren.push_back(transChild);
			else
				delete transChild;
		}
	}
	return transformedChildren;
}

std::vector<Type> ASTTransformation::mapNodesToTypes(std::vector<NodeTree<ASTData>*> nodes) {
	std::vector<Type> types;
	for (auto i : nodes)
		types.push_back(*(i->getDataRef()->valueType));
	return types;
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

//Overloaded with the actual children to allow us to handle operator methods
NodeTree<ASTData>* ASTTransformation::doFunction(NodeTree<ASTData>* scope, std::string lookup, std::vector<NodeTree<ASTData>*> nodes) {
	//
	auto LLElementIterator = languageLevelScope.find(lookup);
	NodeTree<ASTData>* newNode;
	if (LLElementIterator != languageLevelScope.end()) {
		std::cout << "Checking for early method level operator overload" << std::endl;
		std::string lookupOp = "operator" + lookup;
		for (auto i : nodes)
			std::cout << i->getDataRef()->toString() << " ";
		std::cout << std::endl;
		NodeTree<ASTData>* operatorMethod = NULL;
		if (nodes[0]->getDataRef()->valueType && nodes[0]->getDataRef()->valueType->typeDefinition)
			operatorMethod = scopeLookup(nodes[0]->getDataRef()->valueType->typeDefinition, lookupOp, mapNodesToTypes(slice(nodes,1,-1)));
		if (operatorMethod) {
			//Ok, so we construct 
			std::cout << "Early method level operator was found" << std::endl;
			//return operatorMethod;
			NodeTree<ASTData>* newNode = new NodeTree<ASTData>(lookupOp, ASTData(function_call, Symbol(lookupOp, true)));
			NodeTree<ASTData>* dotFunctionCall = new NodeTree<ASTData>(".", ASTData(function_call, Symbol(".", true)));
			dotFunctionCall->addChild(languageLevelScope["."][0]); //function definition
			dotFunctionCall->addChild(nodes[0]); // The object whose method we're calling
			dotFunctionCall->addChild(operatorMethod); //The method we're calling
			newNode->addChild(dotFunctionCall); // First child of function call is a link to the function definition
			newNode->addChildren(slice(nodes, 1, -1)); //The rest of the parameters to the operator


			//Set the value of this function call
			newNode->getDataRef()->valueType = operatorMethod->getDataRef()->valueType;
			return newNode;
		}
		std::cout << "Early method level operator was NOT found" << std::endl;
	}
	
	newNode = new NodeTree<ASTData>(lookup, ASTData(function_call, Symbol(lookup, true)));
	NodeTree<ASTData>* function = scopeLookup(scope, lookup, mapNodesToTypes(nodes));
	newNode->addChild(function);
	newNode->addChildren(nodes);
	newNode->getDataRef()->valueType = function->getDataRef()->valueType;

	return newNode;
}

NodeTree<ASTData>* ASTTransformation::scopeLookup(NodeTree<ASTData>* scope, std::string lookup, std::vector<Type> types) {
	//We first search the languageLevelScope to see if it's an operator. If so, we modifiy the lookup with a preceding "operator"
	auto LLElementIterator = languageLevelScope.find(lookup);
	if (LLElementIterator != languageLevelScope.end())
		lookup = "operator" + lookup;
	//Search the map
	auto scopeMap = scope->getDataRef()->scope;
	auto elementIterator = scopeMap.find(lookup);
	for (auto i : scopeMap)
		std::cout << i.first << " ";
	std::cout << std::endl;
	//
	if (elementIterator != scopeMap.end()) {
		for (auto i = elementIterator->second.begin(); i != elementIterator->second.end(); i++) {
			//Types and functions cannot have the same name, and types very apparently do not have parameter types, so check and short-circuit
			if ((*i)->getDataRef()->type == type_def)
				return *i;
			//return *i;
			std::vector<NodeTree<ASTData>*> children = (*i)->getChildren();
			if (types.size() != ((children.size() > 0) ? children.size()-1 : 0)) {
				std::cout << "Type sizes do not match between two " << lookup << "(" << types.size() << "," << ((children.size() > 0) ? children.size()-1 : 0) << "), types are: ";
				for (auto j : types)
					std::cout << j.toString() << " ";
				std::cout << std::endl;
				continue;
			}
			bool typesMatch = true;
			for (int j = 0; j < types.size(); j++) {
				if (types[j] != *(children[j]->getDataRef()->valueType)) {
					typesMatch = false;
					std::cout << "Types do not match between two " << lookup << std::endl;
					break;
				}
			}
			if (typesMatch)
				return *i;
		}
	}

	//if it doesn't exist, try the enclosing scope if it exists.
	auto enclosingIterator = scopeMap.find("~enclosing_scope");
	if (enclosingIterator != scopeMap.end()) {
	//	std::cout << "upper scope exists, searching it for " << lookup << std::endl;
		NodeTree<ASTData>* upperResult = scopeLookup(enclosingIterator->second[0], lookup, types);
		if (upperResult)
			return upperResult;
	}
	//std::cout << "upper scope does not exist" << std::endl;
	std::cout << "could not find " << lookup << " in standard scope, checking for operator" << std::endl;
	//Note that we don't check for types. At some point we should, as we don't know how to add objects/structs without overloaded operators, etc
	//Also, we've already searched for the element because this is also how we keep track of operator overloading
	if (LLElementIterator != languageLevelScope.end()) {
		std::cout << "found it at language level as operator." << std::endl;
		return LLElementIterator->second[0];
	}
	std::cout << "Did not find, returning NULL" << std::endl;
	return NULL;
}

Type* ASTTransformation::typeFromString(std::string typeIn, NodeTree<ASTData>* scope) {
	int indirection = 0;
	ValueType baseType;
	NodeTree<ASTData>* typeDefinition = NULL;
	while (typeIn[typeIn.size() - indirection - 1] == '*') indirection++;
	std::string edited = strSlice(typeIn, 0, -(indirection + 1));
	if (edited == "void")
		baseType = void_type;
	else if (edited == "bool")
		baseType = boolean;
	else if (edited == "int")
		baseType = integer;
	else if (edited == "float")
		baseType = floating
;	else if (edited == "double")
		baseType = double_percision;
	else if (edited == "char")
		baseType = character;
	else {
		baseType = none;
		typeDefinition = scopeLookup(scope, edited);
		//std::cout << "scopeLookup of type " << edited << " returned " << typeDefinition << std::endl;
	}
	return new Type(baseType, typeDefinition, indirection);
}
