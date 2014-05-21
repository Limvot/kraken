#include "ASTTransformation.h"

ASTTransformation::ASTTransformation(Importer *importerIn) {
	importer = importerIn;
	topScope = NULL;
	//Set up language level special scope. (the final scope checked)
	//Note the NULL type
	languageLevelScope["+"].push_back( new NodeTree<ASTData>("function", ASTData(function, Symbol("+", true), NULL)));
	languageLevelScope["-"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("-", true), NULL)));
	languageLevelScope["*"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("*", true), NULL)));
	languageLevelScope["/"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("/", true), NULL)));
	languageLevelScope["%"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("%", true), NULL)));
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
	languageLevelScope["[]"].push_back(new NodeTree<ASTData>("function", ASTData(function, Symbol("[]", true), NULL)));
}

ASTTransformation::~ASTTransformation() {
	//
}

NodeTree<ASTData>* ASTTransformation::transform(NodeTree<Symbol>* from) {
	//Set up top scope
	return transform(from, NULL, std::vector<Type>(), std::map<std::string, Type*>());
}

NodeTree<ASTData>* ASTTransformation::transform(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::vector<Type> types, std::map<std::string, Type*> templateTypeReplacements) {
	Symbol current = from->getData();
	std::string name = current.getName();
	NodeTree<ASTData>* newNode = NULL;
	std::vector<NodeTree<Symbol>*> children = from->getChildren();
	std::set<int> skipChildren;

	if (name == "translation_unit") {
		newNode = new NodeTree<ASTData>(name, ASTData(translation_unit));
		scope = newNode;
		topScope = newNode; //Top scope is maintained for templates, which need to add themselves to the top scope from where ever they are instantiated
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
		}
	} else if (name == "type_def") {
		//If it is an alisis of a type
		std::string typeAlias;
		if (children[1]->getData().getName() == "type") {
			typeAlias = concatSymbolTree(children[0]);
			newNode = new NodeTree<ASTData>(name, ASTData(type_def, Symbol(typeAlias, true, typeAlias), typeFromTypeNode(children[1], scope, templateTypeReplacements)));
			skipChildren.insert(0); //Don't want any children, it's unnecessary for ailising
			skipChildren.insert(1);
		} else { //Is a struct or class
			Type* objectType = NULL;
			if (children[0]->getData().getName() == "template_dec") {
				typeAlias = concatSymbolTree(children[1]);
				std::cout << "Template Type!"<<std::endl;

				newNode = new NodeTree<ASTData>(name, ASTData(type_def, Symbol(typeAlias, true, typeAlias)));
				//So we give this typedef its name without any template types and make its type template_type, and point to this from node.
				//Then, when this template is instantiated, it will run transform on from with the types filled in.
				objectType = new Type(template_type, from);
			} else {
				typeAlias = concatSymbolTree(children[0]);
				newNode = new NodeTree<ASTData>(name, ASTData(type_def, Symbol(typeAlias, true, typeAlias)));
				objectType = new Type(newNode);
				skipChildren.insert(0); //Identifier lookup will be ourselves, as we just added ourselves to the scope
			}
			newNode->getDataRef()->valueType = objectType; //Type is self-referential since this is the definition
		}
		scope->getDataRef()->scope[typeAlias].push_back(newNode);
		newNode->getDataRef()->scope["~enclosing_scope"].push_back(scope);

		//Templates are done here. No need to go farther
		if (children[0]->getData().getName() == "template_dec")
			return newNode;
		scope = newNode;
	} else if (name == "function") {
		std::string functionName;
		//If this is a function template
		if (children[0]->getData().getName() == "template_dec") {
			functionName = concatSymbolTree(children[2]);
			newNode = new NodeTree<ASTData>(name, ASTData(function, Symbol(functionName, true), new Type(template_type, from)));
			scope->getDataRef()->scope[functionName].push_back(newNode);
			newNode->getDataRef()->scope["~enclosing_scope"].push_back(scope);
			std::map<std::string, Type*> yetToBeInstantiatedTemplateTypes; //So that template types (like T) that have not been placed yet are found and given
																			//a special Type() - baseType = template_type_type
			yetToBeInstantiatedTemplateTypes[concatSymbolTree(children[0]->getChildren()[1])] = new Type(template_type_type); //This may have to be combined with templateTypeReplacements if we do templated member functions inside of templated classes

			auto transChildren = transformChildren(slice(children,3,-2), std::set<int>(), newNode, types, yetToBeInstantiatedTemplateTypes);
			std::cout << "Template function " << functionName << " has these parameters: ";
			for (auto i : transChildren)
				std::cout << "||" << i->getDataRef()->toString() << "|| ";
			std::cout << "??||" << std::endl;
			newNode->addChildren(transChildren);

			std::cout << "Finished Non-Instantiated Template function " << functionName << std::endl;
			return newNode;
		}
		functionName = concatSymbolTree(children[1]);
		newNode = new NodeTree<ASTData>(name, ASTData(function, Symbol(functionName, true), typeFromTypeNode(children[0], scope, templateTypeReplacements)));
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
		
		std::cout << "finished function (kinda, not children) " << functionName << std::endl;
	} else if (name == "code_block") {
		newNode = new NodeTree<ASTData>(name, ASTData(code_block));
		newNode->getDataRef()->scope["~enclosing_scope"].push_back(scope);
		scope = newNode;
	} else if (name == "typed_parameter") {
		//newNode = transform(children[1]); //Transform to get the identifier
		std::string parameterName = concatSymbolTree(children[1]);
		std::cout << "Doing typed parameter " << parameterName << std::endl;
		//std::string typeString = concatSymbolTree(children[0]);//Get the type (left child) and set our new identifer to be that type
		newNode = new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(parameterName, true), typeFromTypeNode(children[0], scope, templateTypeReplacements)));
		scope->getDataRef()->scope[parameterName].push_back(newNode);
		newNode->getDataRef()->scope["~enclosing_scope"].push_back(scope);
		std::cout << "Done doing typed_parameter " << parameterName << std::endl;
		return newNode;
	} else if (name == "boolean_expression" || name == "and_boolean_expression" || name == "bool_exp") {
		//If this is an actual part of an expression, not just a premoted term
		if (children.size() > 1) {
			//We do children first so we can do appropriate scope searching with types (yay operator overloading!)
			skipChildren.insert(1);
			std::vector<NodeTree<ASTData>*> transformedChildren = transformChildren(children, skipChildren, scope, types, templateTypeReplacements);
			std::string functionCallString = concatSymbolTree(children[1]);
			NodeTree<ASTData>* function = doFunction(scope, functionCallString, transformedChildren, templateTypeReplacements);
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
			return transform(children[0], scope, types, templateTypeReplacements); //Just a promoted term, so do child
		}
	//Here's the order of ops stuff
	} else if (name == "expression" || name == "shiftand" || name == "term" || name == "unarad" || name == "access_operation") { //unarad can ride through, it should always just be a promoted child
		//If this is an actual part of an expression, not just a premoted child
		if (children.size() > 2) {
			NodeTree<ASTData>* lhs = transform(children[0], scope, std::vector<Type>(), templateTypeReplacements); //LHS does not inherit types
			NodeTree<ASTData>* rhs;
			if (name == "access_operation") {
				std::cout << "lhs is: " << lhs->getDataRef()->toString() << std::endl;
				rhs = transform(children[2], lhs->getDataRef()->valueType->typeDefinition, types, templateTypeReplacements); //If an access operation, then the right side will be in the lhs's type's scope
			}
			else
				rhs = transform(children[2], scope, types, templateTypeReplacements);

			std::string functionCallName = concatSymbolTree(children[1]);
			//std::cout << "scope lookup from expression or similar" << std::endl;
			std::vector<NodeTree<ASTData>*> transformedChildren; transformedChildren.push_back(lhs); transformedChildren.push_back(rhs);
			newNode = doFunction(scope, functionCallName, transformedChildren, templateTypeReplacements);
			if (newNode == NULL) {
				std::cout << "scope lookup error! Could not find " << functionCallName << " in expression " << std::endl;
				throw "LOOKUP ERROR: " + functionCallName; 
			}

			// //Set the value of this function call
			if (newNode->getDataRef()->valueType == NULL && rhs->getDataRef()->valueType)
				newNode->getDataRef()->valueType = rhs->getDataRef()->valueType;
			else
				newNode->getDataRef()->valueType = NULL;
			std::cout << "function call to " << functionCallName << " - " << newNode->getName() << " is now " << newNode->getDataRef()->valueType  << std::endl;
			return newNode;
			//skipChildren.insert(1);
		} else if (children.size() == 2) {
			//Is template instantiation
			return findOrInstantiateFunctionTemplate(children, scope, types, templateTypeReplacements);
		} else {
			return transform(children[0], scope, types, templateTypeReplacements); //Just a promoted child, so do it instead
		}
	} else if (name == "factor") { //Do factor here, as it has all the weird unary operators
		//If this is an actual part of an expression, not just a premoted child
		//NO SUPPORT FOR CASTING YET
		std::string funcName;
		if (children.size() == 2) {
			funcName = concatSymbolTree(children[0]);
			NodeTree<ASTData>* param;
			if (funcName == "*" || funcName == "&" || funcName == "++" || funcName == "--" || funcName == "-" || funcName == "!" || funcName == "~")
				param = transform(children[1], scope, types, templateTypeReplacements);
			else
				funcName = concatSymbolTree(children[1]), param = transform(children[0], scope, types, templateTypeReplacements);

			//std::cout << "scope lookup from factor" << std::endl;
			std::vector<NodeTree<ASTData>*> transformedChildren; transformedChildren.push_back(param);
			NodeTree<ASTData>* function = doFunction(scope, funcName, transformedChildren, templateTypeReplacements);
			if (function == NULL) {
				std::cout << "scope lookup error! Could not find " << funcName  << " in factor " << std::endl;
				throw "LOOKUP ERROR: " + funcName; 
			}

			return function;
		} else if (children.size() >= 4) { //Array brackets []
			funcName = "[]";
			std::vector<NodeTree<ASTData>*> transformedChildren;
			transformedChildren.push_back(transform(children[0], scope, types, templateTypeReplacements));
			transformedChildren.push_back(transform(children[2], scope, types, templateTypeReplacements));
			NodeTree<ASTData>* function = doFunction(scope, funcName, transformedChildren, templateTypeReplacements);
			if (function == NULL) {
				std::cout << "scope lookup error! Could not find " << funcName  << " in factor " << std::endl;
				throw "LOOKUP ERROR: " + funcName; 
			}
			return function;
		} else {
			return transform(children[0], scope, types, templateTypeReplacements); //Just a promoted child, so do it instead
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
			newNode->addChild(transform(children[0], scope, types, templateTypeReplacements));
			newNode->addChild(transform(children[2], scope, types, templateTypeReplacements));
		} else {
			//For assignments like += or *=, expand the syntatic sugar.
			NodeTree<ASTData>* lhs = transform(children[0], scope, types, templateTypeReplacements);
			NodeTree<ASTData>* rhs = transform(children[2], scope, types, templateTypeReplacements);
			std::vector<NodeTree<ASTData>*> transformedChildren; transformedChildren.push_back(lhs); transformedChildren.push_back(rhs);
			std::string functionName = assignFuncName.substr(0,1);
			NodeTree<ASTData>* operatorCall = doFunction(scope, functionName, transformedChildren, templateTypeReplacements);
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
		Type* identifierType = typeFromTypeNode(children[0], scope, templateTypeReplacements);
		std::cout << "Declaring an identifier " << newIdentifierStr << " to be of type " << identifierType->toString() << std::endl;
		NodeTree<ASTData>* newIdentifier = new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(newIdentifierStr, true), identifierType));
		scope->getDataRef()->scope[newIdentifierStr].push_back(newIdentifier);
		newNode->getDataRef()->scope["~enclosing_scope"].push_back(scope);
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

		skipChildren.insert(0);
		std::vector<NodeTree<ASTData>*> transformedChildren = transformChildren(children, skipChildren, scope, types, templateTypeReplacements);
		std::cout << "scope lookup from function_call: " << functionCallName << std::endl;
		for (auto i : children)
			std::cout << i << " : " << i->getName()  << " : " << i->getDataRef()->getName() << std::endl;

		NodeTree<ASTData>* function = transform(children[0], scope, mapNodesToTypes(transformedChildren), templateTypeReplacements);
		std::cout << "The thing: " << function << " : " << function->getName() << std::endl;
		for (auto i : function->getChildren())
			std::cout << i->getName() << " ";
		std::cout << std::endl;
		newNode->addChild(function);
		newNode->getDataRef()->valueType = function->getDataRef()->valueType;
		newNode->addChildren(transformedChildren);
		return newNode;
	} else if (name == "parameter") {
		return transform(children[0], scope, types, templateTypeReplacements); //Don't need a parameter node, just the value
	} else if (name == "type") {
		std::string theConcat = concatSymbolTree(from); //We have no symbol, so this will concat our children
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(theConcat, true), typeFromTypeNode(from, scope, templateTypeReplacements)));
	} else if (name == "number") {
		return transform(children[0], scope, types, templateTypeReplacements);
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
			NodeTree<ASTData>* transChild = transform(children[i], scope, types, templateTypeReplacements);
			if (transChild->getDataRef()->type) //Only add the children that have a real ASTData::ASTType, that is, legit ASTData.
				newNode->addChild(transChild);
			else
				delete transChild;
		}
	}
	return newNode;
}

//We use this functionality a lot at different places
std::vector<NodeTree<ASTData>*> ASTTransformation::transformChildren(std::vector<NodeTree<Symbol>*> children, std::set<int> skipChildren, NodeTree<ASTData>* scope, std::vector<Type> types, std::map<std::string, Type*> templateTypeReplacements) {
	std::vector<NodeTree<ASTData>*> transformedChildren;
	// In general, iterate through children and do them. Might not do this for all children.
	for (int i = 0; i < children.size(); i++) {
		if (skipChildren.find(i) == skipChildren.end()) {
			NodeTree<ASTData>* transChild = transform(children[i], scope, types, templateTypeReplacements);
			if (transChild->getDataRef()->type) //Only add the children that have a real ASTData::ASTType, that is, legit ASTData.
				transformedChildren.push_back(transChild);
			else
				delete transChild;
		}
	}
	return transformedChildren;
}

//Extract types from already transformed nodes
std::vector<Type> ASTTransformation::mapNodesToTypes(std::vector<NodeTree<ASTData>*> nodes) {
	std::vector<Type> types;
	for (auto i : nodes) {
		std::cout << i->getDataRef()->toString() << std::endl;
		types.push_back(*(i->getDataRef()->valueType));
	}
	return types;
}

//Simple way to extract strings from syntax trees. Used often for identifiers, strings, types
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

//We pass in the actual children (parameters) to allow us to handle overloaded operator methods (where a parameter is actually the scope of the method)
NodeTree<ASTData>* ASTTransformation::doFunction(NodeTree<ASTData>* scope, std::string lookup, std::vector<NodeTree<ASTData>*> nodes, std::map<std::string, Type*> templateTypeReplacements) {
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

	//Specially handle dereference and address of to assign the correct type
	//We need some significant other type corrections here, maybe to the point of being their own function. (int + float, etc.)
	for (auto i : nodes)
		std::cout << i->getDataRef()->toString() << " ";
	std::cout<<std::endl;

	std::vector<Type> oldTypes = mapNodesToTypes(nodes);
	if ((nodes.size() != 2 && lookup == "*") || lookup == "&" || lookup == "[]") {
		Type* newType = oldTypes[0].clone();
		if (lookup == "*" || lookup == "[]")
			newType->decreaseIndirection();
		else
			newType->increaseIndirection();

		newNode->getDataRef()->valueType = newType, std::cout << "Operator " + lookup << " is altering indirection "<< std::endl;
	} else {
		newNode->getDataRef()->valueType = function->getDataRef()->valueType, std::cout << "Some other ||" << lookup << "||" << std::endl;
	}
	return newNode;
}

//Search recursively through levels of scope (each ASTData, that is, every node, has its own scope)
//We pass in types so that if we're searching for a function we can find the right overloaded one
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

			std::vector<NodeTree<ASTData>*> children = (*i)->getChildren();
			//We subtract one from the children to get the type size only if there is at least one child AND
			// the last node is actually a body node, as it may not have been generated yet if we're in the body
			//and this function is recursive or if this is a non-instantiated template function
			if (types.size() != ((children.size() > 0 && children[children.size()-1]->getDataRef()->type == code_block) ? children.size()-1 : children.size())) {
				std::cout << "Type sizes do not match between two " << lookup << "(" << types.size() << "," << ((children.size() > 0 && children[children.size()-1]->getDataRef()->type == code_block) ? children.size()-1 : children.size()) << "), types are: ";
				for (auto j : types)
					std::cout << j.toString() << " ";
				std::cout << std::endl;
				continue;
			}
			bool typesMatch = true;
			for (int j = 0; j < types.size(); j++) {
				Type* tmpType = children[j]->getDataRef()->valueType;
				//Don't worry if types don't match if it's a template type
				if (types[j] != *tmpType && tmpType->baseType != template_type_type) {
					typesMatch = false;
					std::cout << "Types do not match between two " << lookup << " " << types[j].toString();
					std::cout << " vs " << children[j]->getDataRef()->valueType->toString() << std::endl;
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

//Create a type from a syntax tree. This can get complicated with templates
Type* ASTTransformation::typeFromTypeNode(NodeTree<Symbol>* typeNode, NodeTree<ASTData>* scope, std::map<std::string, Type*> templateTypeReplacements) {
	std::string typeIn;
	typeIn = concatSymbolTree(typeNode);

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
		baseType = floating;
	else if (edited == "double")
		baseType = double_percision;
	else if (edited == "char")
		baseType = character;
	else {
		baseType = none;
		typeDefinition = scopeLookup(scope, edited);
		//So, if this is a template class type and it has already been instantiated, then the above scope lookup will take care of it.
		//So either this is an uninstatiated template class type, or this is literally a template type T, and we should get it from our
		//templateTypeReplacements map. We try this first
		if (templateTypeReplacements.find(edited) != templateTypeReplacements.end()) {
			std::cout << "Template type! (" << edited << ")" << std::endl;
			Type* templateTypeReplacement = templateTypeReplacements[edited]->clone();
			templateTypeReplacement->modifyIndirection(indirection);
			return templateTypeReplacement;
		} else {
			std::cout << edited << " was not found in templateTypeReplacements" << std::endl;
			std::cout << "templateTypeReplacements consists of : ";
			for (auto i : templateTypeReplacements)
				std::cout << i.first << " ";
			std::cout << std::endl;
		}
		//If not, we better instantiate it and then add it to the highest (not current) scope
		if (typeDefinition == NULL && typeNode->getChildren().size() > 1 && typeNode->getChildren()[1]->getData().getName() == "template_inst") {
		 	std::cout << "Template type: " << edited << " not yet instantiated" << std::endl;
		 	//Look up this template's plain definition. It's type has the syntax tree that we need to parse
		 	NodeTree<ASTData>* templateDefinition = scopeLookup(scope,concatSymbolTree(typeNode->getChildren()[0]));
		 	if (templateDefinition == NULL)
		 		std::cout << "Template definition is null!" << std::endl;
		 	else
		 		std::cout << "Template definition is not null!" << std::endl;

		 	NodeTree<Symbol>* templateSyntaxTree = templateDefinition->getDataRef()->valueType->templateDefinition;
		 	//Create a new map of template type names to actual types.
		 	std::map<std::string, Type*> newTemplateTypeReplacement;
		 	std::string templateParameterName = concatSymbolTree(templateSyntaxTree->getChildren()[0]->getChildren()[1]);
		 	Type* replacementType = typeFromTypeNode(typeNode->getChildren()[1]->getChildren()[1], scope, templateTypeReplacements);
		 	newTemplateTypeReplacement[templateParameterName] = replacementType;
		 	
		 	std::string classNameWithoutTemplate = concatSymbolTree(typeNode->getChildren()[0]);
		 	std::string fullyInstantiatedName = classNameWithoutTemplate + "<" + replacementType->toString() + ">";

			typeDefinition = new NodeTree<ASTData>("type_def", ASTData(type_def, Symbol(fullyInstantiatedName, true, fullyInstantiatedName)));
			typeDefinition->getDataRef()->valueType = new Type(typeDefinition);; //Type is self-referential since this is the definition

			//Note that we're adding to the current top scope. This makes it more efficient by preventing multiple instantiation and should not cause any problems
			//It also makes sure it gets generated in the right place
			topScope->getDataRef()->scope[fullyInstantiatedName].push_back(typeDefinition);
			topScope->addChild(typeDefinition); //Add this object the the highest scope's 


			//Note that the instantiated template's scope is the template's definition.
			typeDefinition->getDataRef()->scope["~enclosing_scope"].push_back(templateDefinition);

			std::set<int> skipChildren;
			skipChildren.insert(0); //Don't do the template part
			skipChildren.insert(1); //Identifier lookup will be ourselves, as we just added ourselves to the scope
		 	typeDefinition->addChildren(transformChildren(templateSyntaxTree->getChildren(), skipChildren, typeDefinition, std::vector<Type>(), newTemplateTypeReplacement));
			std::cout << "Done instantating " << fullyInstantiatedName << " that had template parameter " << templateParameterName << " with " << replacementType->toString() << std::endl;
		} else if (typeDefinition == NULL) {
			std::cout << "Could not find type " << edited << ", returning NULL" << std::endl;
			return NULL;
		} else {
			std::cout << "Type: " << edited << " already instantiated with " << typeDefinition << ", will be " << Type(baseType, typeDefinition, indirection).toString() << std::endl;
		}
	}
	Type* toReturn = new Type(baseType, typeDefinition, indirection);
	std::cout << "Returning type " << toReturn->toString() << std::endl;
	return toReturn;
}

NodeTree<ASTData>* ASTTransformation::findOrInstantiateFunctionTemplate(std::vector<NodeTree<Symbol>*> children, NodeTree<ASTData>* scope, std::vector<Type> types, std::map<std::string, Type*> templateTypeReplacements) {
	//First look to see if we can find this already instantiated
	std::cout << "Finding or instantiating templated function" << std::endl;
	std::string functionName = concatSymbolTree(children[0]);
	Type* templateActualType = typeFromTypeNode(children[1]->getChildren()[1], scope, templateTypeReplacements);
	std::string fullyInstantiatedName = functionName + "<" + templateActualType->toString() + ">";
	std::cout << "Looking for " << fullyInstantiatedName << std::endl;

	std::cout << "Types are : ";
	for (auto i : types)
		std::cout << " " << i.toString();
	std::cout << std::endl;

	NodeTree<ASTData>* instantiatedFunction = scopeLookup(scope, fullyInstantiatedName,types);
	//If it already exists, return it
	if (instantiatedFunction) {
		std::cout << fullyInstantiatedName << " already exists! Returning" << std::endl;
		return instantiatedFunction;
	} else {
		std::cout << fullyInstantiatedName << " does NOT exist" << std::endl;
	}

	//Otherwise, we're going to instantiate it
	//Find the template definition
	NodeTree<ASTData>* templateDefinition = scopeLookup(scope,functionName,types);
	if (templateDefinition == NULL) {
		std::cout << functionName << " search turned up null, returing null" << std::endl;
		return NULL;
	}

	NodeTree<Symbol>* templateSyntaxTree = templateDefinition->getDataRef()->valueType->templateDefinition;
	std::string templateParameterName = concatSymbolTree(templateSyntaxTree->getChildren()[0]->getChildren()[1]);

	std::map<std::string, Type*> newTemplateTypeReplacement;
	newTemplateTypeReplacement[templateParameterName] = templateActualType;

	std::vector<NodeTree<Symbol>*> templateChildren = templateSyntaxTree->getChildren();
	for (int i = 0; i < templateChildren.size(); i++)
		std::cout << ", " << i << " : " << templateChildren[i]->getDataRef()->getName();
	std::cout << std::endl;

	instantiatedFunction = new NodeTree<ASTData>("function", ASTData(function, Symbol(fullyInstantiatedName, true), typeFromTypeNode(templateChildren[1], scope, newTemplateTypeReplacement)));
	std::set<int> skipChildren;
	skipChildren.insert(0);
	skipChildren.insert(1);
	skipChildren.insert(2);
	scope->getDataRef()->scope[fullyInstantiatedName].push_back(instantiatedFunction);
	instantiatedFunction->getDataRef()->scope["~enclosing_scope"].push_back(templateDefinition->getDataRef()->scope["~enclosing_scope"][0]); //Instantiated Template Function's scope is it's template's definition's scope
	std::cout << "About to do children of " << functionName << " to " << fullyInstantiatedName << std::endl;
	instantiatedFunction->addChildren(transformChildren(templateSyntaxTree->getChildren(), skipChildren, instantiatedFunction, std::vector<Type>(), newTemplateTypeReplacement));

	topScope->getDataRef()->scope[fullyInstantiatedName].push_back(instantiatedFunction);
	topScope->addChild(instantiatedFunction); //Add this object the the highest scope's

	std::cout << "Fully Instantiated function " << functionName << " to " << fullyInstantiatedName << std::endl;

	return instantiatedFunction;
}

