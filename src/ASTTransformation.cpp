#include "ASTTransformation.h"

ASTTransformation::ASTTransformation(Importer *importerIn) {
	importer = importerIn;
	topScope = NULL;

	builtin_trans_unit = new NodeTree<ASTData>("translation_unit", ASTData(translation_unit, Symbol("builtin", false)));

    //Set up language level reserved identifier scope (only this, right now)
    languageLevelReservedWords["this"].push_back(new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol("this", true), NULL)));

    //Set up language level special scope. (the final scope checked)
	//Note the NULL type
	languageLevelOperators["+"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("+", true), NULL))));
	languageLevelOperators["-"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("-", true), NULL))));
	languageLevelOperators["*"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("*", true), NULL))));
	languageLevelOperators["/"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("/", true), NULL))));
	languageLevelOperators["%"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("%", true), NULL))));
	languageLevelOperators["&"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("&", true), NULL))));
	languageLevelOperators["--"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("--", true), NULL))));
	languageLevelOperators["++"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("++", true), NULL))));
	languageLevelOperators["=="].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("==", true), new Type(boolean)))));
	languageLevelOperators["!="].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("!=", true), new Type(boolean)))));
	languageLevelOperators["<="].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("<=", true), new Type(boolean)))));
	languageLevelOperators[">="].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol(">=", true), new Type(boolean)))));
	languageLevelOperators["<"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("<", true), new Type(boolean)))));
	languageLevelOperators[">"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol(">", true), new Type(boolean)))));
	languageLevelOperators["&&"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("&&", true), new Type(boolean)))));
	languageLevelOperators["||"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("||", true), new Type(boolean)))));
	languageLevelOperators["!"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("!", true), new Type(boolean)))));
	languageLevelOperators["*="].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("*=", true), NULL))));
	languageLevelOperators["+="].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("+=", true), NULL))));
	languageLevelOperators["-="].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("-=", true), NULL))));
	languageLevelOperators["."].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol(".", true), NULL))));
	languageLevelOperators["->"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("->", true), NULL))));
	languageLevelOperators["[]"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("[]", true), NULL))));
}

ASTTransformation::~ASTTransformation() {
	//
}

//First pass defines all type_defs (objects and ailises), and if_comp/simple_passthrough
NodeTree<ASTData>* ASTTransformation::firstPass(std::string fileName, NodeTree<Symbol>* parseTree) {
	NodeTree<ASTData>* translationUnit = new NodeTree<ASTData>("translation_unit", ASTData(translation_unit, Symbol(fileName, false)));
	std::vector<NodeTree<Symbol>*> children = parseTree->getChildren();
	importer->registerAST(fileName, translationUnit, parseTree); 	//Register ourselves with the importer.
	                												//This puts us in the scope and the list of ASTs that go through all the passes

	//Go through and define all types (type_defs whether they are classes or ailises, as well as including non-instantiated templates)
	for (NodeTree<Symbol>* i : children) {
		if (i->getDataRef()->getName() == "type_def") {
			std::string name;
			if (i->getChildren()[0]->getData().getName() == "template_dec") // It's a template
				name = concatSymbolTree(i->getChildren()[1]);
			else 															//It's not
				name = concatSymbolTree(i->getChildren()[0]);
			NodeTree<ASTData>* firstDec = addToScope("~enclosing_scope", translationUnit, new NodeTree<ASTData>("type_def", ASTData(type_def, Symbol(name, true, name))));
            addToScope(name, firstDec, translationUnit);
			translationUnit->addChild(firstDec);
			//If this is a template, go ahead and set it up. Pass 2 needs templates set up so it can (partially) instantiate them.
				//So we give this typedef its name without any template types and make its type template_type, and point to this from node.
				//Then, when this template is instantiated, it will run transform on from with the types filled in.
			auto typedefChildren = i->getChildren();
            if (typedefChildren[0]->getData().getName() == "template_dec") {
				if (typedefChildren.size() > 2 && typedefChildren[2]->getData().getName() == "traits")
                    firstDec->getDataRef()->valueType = new Type(template_type, i, parseTraits(i->getChildren()[2]));
                else
                    firstDec->getDataRef()->valueType = new Type(template_type, i);
            }
            else if (typedefChildren.size() > 1 && typedefChildren[1]->getData().getName() == "traits")
                firstDec->getDataRef()->valueType = new Type(firstDec, parseTraits(i->getChildren()[1]));
            else if (typedefChildren.size() == 1 || typedefChildren[1]->getData().getName() != "type") //We don't make the type for alises, because the second pass will assign it the type it points to
                firstDec->getDataRef()->valueType = new Type(firstDec);

		}  else if (i->getDataRef()->getName() == "if_comp") {
            std::cout << "IF COMP" << std::endl;
            NodeTree<ASTData>* newNode = addToScope("~enclosing_scope", translationUnit, new NodeTree<ASTData>(i->getDataRef()->getName(), ASTData(if_comp)));
            newNode->addChild(addToScope("~enclosing_scope", newNode, new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(concatSymbolTree(i->getChildren()[0]),true)))));
            std::set<int> skipChildren;
            skipChildren.insert(0); //Don't do the identifier. The identifier lookup will fail. That's why we do it here.
            newNode->addChildren(transformChildren(i->getChildren(), skipChildren, translationUnit, std::vector<Type>(), std::map<std::string, Type*>()));
			translationUnit->addChild(newNode);
        }
	}

	//Now go through and do all imports
	//We do this second so that if an import also imports us, all of our stuff has already been defined
	for (NodeTree<Symbol>* i : children) {
		if (i->getDataRef()->getName() == "import") {
            auto importChildren = i->getChildren();
			std::string toImport = concatSymbolTree(importChildren[0]);
            auto importNode = addToScope("~enclosing_scope", translationUnit, new NodeTree<ASTData>("import", ASTData(import, Symbol(toImport, true))));
			translationUnit->addChild(importNode);
			//Do the imported file too
			NodeTree<ASTData>* outsideTranslationUnit = importer->importFirstPass(toImport + ".krak");
			translationUnit->getDataRef()->scope[toImport].push_back(outsideTranslationUnit); //Put this transation_unit in the scope as it's files name
			// Now go through and handle anything like import asdf: a; or import asdf: a,b; or import asdf: *;
            // We do this by looping through the children and adding links to them as the scope in the import node. If it's *, we add the entire translationUnit link.
            // Note that import affects scope in two ways:
            //              (1) The other file's translationUnit is added to our translationUnit's scope under it's name
            //              (2) The import node's scope contains the nodes indicated by the qualifiers after the import (i.e. the import a:b; or import a:*;)
            for (auto importQualifer : slice(importChildren, 1, -1)) {                        // Not the first child, that's the name of the file
                auto name = concatSymbolTree(importQualifer);
                if (name == "*") {
                    std::vector<NodeTree<ASTData>*> tmp;
                    tmp.push_back(outsideTranslationUnit);
                    importNode->getDataRef()->scope["*"] = tmp;
                } else {
                    bool found = false;
                    for (auto outsideScopeEntry : outsideTranslationUnit->getDataRef()->scope) {
                        if (name == outsideScopeEntry.first) {
                            importNode->getDataRef()->scope[outsideScopeEntry.first] = outsideScopeEntry.second;
                            found = true;
                        }
                    }
                    // If it's not found yet, put it in as a empty vector for pass 3.
                    // This makes sure that it does appear in the scope map, which is what we iterate through later.
                    if (!found)
                        importNode->getDataRef()->scope[name] = std::vector<NodeTree<ASTData>*>();
                }
            }
		}
	}

	return translationUnit;
}

std::set<std::string> ASTTransformation::parseTraits(NodeTree<Symbol>* traitsNode) {
    std::set<std::string> traits;
    //Every other one b/c comma separated
    for (auto i : slice(traitsNode->getChildren(), 0, -1, 2))
        traits.insert(concatSymbolTree(i));
    return traits;
}

//Second pass defines data inside objects, outside declaration statements, and function prototypes (since we have type_defs now)
void ASTTransformation::secondPass(NodeTree<ASTData>* ast, NodeTree<Symbol>* parseTree) {
	topScope = ast; //Top scope is maintained for templates, which need to add themselves to the top scope from where ever they are instantiated
	std::vector<NodeTree<Symbol>*> children = parseTree->getChildren();

	//Go through and declare data internal to objects as well as all function prototypes (methods and otherwise)
	//Note that this pass can instantiate class templates
	for (NodeTree<Symbol>* i : children) {
		if (i->getDataRef()->getName() == "type_def") {
			if (i->getChildren()[0]->getData().getName() == "template_dec") // It's a template
				continue;	//We've already set upt the class templates
			std::vector<NodeTree<Symbol>*> typedefChildren = i->getChildren();
			std::string name = concatSymbolTree(typedefChildren[0]);
			NodeTree<ASTData>* typeDef = ast->getDataRef()->scope[name][0]; //No overloaded types (besides uninstantiated templates, which can have multiple versions based on types or specilizations)

			//It's an alias. Note that if typedefChildren.size() == 1 it's because its a regular class with no body, i.e. {}
			if (typedefChildren.size() > 1 && typedefChildren[1]->getData().getName() == "type") {
        		Type* aliasedType = typeFromTypeNode(typedefChildren[1], ast, std::map<std::string, Type*>());
        		typeDef->getDataRef()->valueType = aliasedType;
                // only put in scope if it has a definition, that is it is not an aliased primitive
                if (aliasedType->typeDefinition)
                    typeDef->getDataRef()->scope["~enclosing_scope"][0] = aliasedType->typeDefinition; //So that object lookups find the right member. Note that this overrides translation_unit as a parent scope
               // std::cout << name << " alias's to " << aliasedType->typeDefinition << std::endl;
               // std::cout << "that is " << aliasedType->typeDefinition->getDataRef()->toString() << std::endl;
				continue;
			}
			//Do the inside of classes here
            secondPassDoClassInsides(typeDef, typedefChildren, std::map<std::string, Type*>());
		} else if (i->getDataRef()->getName() == "function") {
			//Do prototypes of functions
			ast->addChild(secondPassFunction(i, ast, std::map<std::string, Type*>()));
		} else if (i->getDataRef()->getName() == "declaration_statement") {
			//Do declaration statements
			ast->addChild(secondPassDeclaration(i, ast, std::map<std::string, Type*>()));
		}
	}
}

void ASTTransformation::secondPassDoClassInsides(NodeTree<ASTData>* typeDef, std::vector<NodeTree<Symbol>*> typedefChildren, std::map<std::string, Type*> templateTypeReplacements) {
    //We pull out this functionality into a new function because this is used in typeFromTypeNode to partially instantiate templates
	for (NodeTree<Symbol>* j : typedefChildren) {
		if (j->getDataRef()->getName() == "declaration_statement") {
			//do declaration
			typeDef->addChild(secondPassDeclaration(j, typeDef, templateTypeReplacements));
		} else if (j->getDataRef()->getName() == "function") {
			//do member method
			typeDef->addChild(secondPassFunction(j, typeDef, templateTypeReplacements));
		}
	}
}

//This function may need to partially instantiate a class template
NodeTree<ASTData>* ASTTransformation::secondPassDeclaration(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::map<std::string, Type*> templateTypeReplacements) {
	//Check here for method call (an error here)
    NodeTree<ASTData>* decStmt = addToScope("~enclosing_scope", scope, new NodeTree<ASTData>("declaration_statement", ASTData(declaration_statement)));
	std::string newIdentifierStr = concatSymbolTree(from->getChildren()[1]);
    Type* identifierType = typeFromTypeNode(from->getChildren()[0], scope, templateTypeReplacements);
	std::cout << "Declaring an identifier " << newIdentifierStr << " to be of type " << identifierType->toString() << std::endl;
	NodeTree<ASTData>* newIdentifier = addToScope("~enclosing_scope", decStmt, new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(newIdentifierStr, true), identifierType)));
	//scope->getDataRef()->scope[newIdentifierStr].push_back(newIdentifier);
    addToScope(newIdentifierStr, newIdentifier, scope); // NEW WAY!
	decStmt->addChild(newIdentifier);

	return decStmt;
}

//This function may need to partially instantiate a class template
NodeTree<ASTData>* ASTTransformation::secondPassFunction(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::map<std::string, Type*> templateTypeReplacements) {
	//If this is a function template
	std::vector<NodeTree<Symbol>*> children = from->getChildren();
	NodeTree<ASTData>* functionDef = NULL;
	std::string functionName;
	if (children[0]->getData().getName() == "template_dec") {
		functionName = concatSymbolTree(children[2]);
		functionDef = new NodeTree<ASTData>("function", ASTData(function, Symbol(functionName, true), new Type(template_type, from)));
        addToScope("~enclosing_scope", scope, functionDef);
        addToScope(functionName, functionDef, scope);
		std::map<std::string, Type*> yetToBeInstantiatedTemplateTypes;  //So that template types (like T) that have not been placed yet are found and given
                                                                        //a special Type() - baseType = template_type_type
		for (auto i : slice(children[0]->getChildren(), 1, -1, 2)) {//skip commas
            if (i->getChildren().size() == 1)
                yetToBeInstantiatedTemplateTypes[concatSymbolTree(i)] = new Type(template_type_type); //This may have to be combined with templateTypeReplacements if we do templated member functions inside of templated classes
            else //has traits
                yetToBeInstantiatedTemplateTypes[concatSymbolTree(i->getChildren()[0])] = new Type(template_type_type, parseTraits(i->getChildren()[1])); //This may have to be combined with templateTypeReplacements if we do templated member functions inside of templated classes
        }
        // Just to see, I don't think templated functions actually need parameters at this point, and we might not have enough info anyway...
        auto transChildren = transformChildren(slice(children,3,-2), std::set<int>(), functionDef, std::vector<Type>(), yetToBeInstantiatedTemplateTypes);
        std::cout << "Template function " << functionName << " has these parameters: ";
        for (auto i : transChildren)
            std::cout << "||" << i->getDataRef()->toString() << "|| ";
        std::cout << "DoneList" << std::endl;
        functionDef->addChildren(transChildren);

		std::cout << "Finished Non-Instantiated Template function " << functionName << std::endl;
		return functionDef;
	}
	functionName = concatSymbolTree(children[1]);
    functionDef = new NodeTree<ASTData>("function", ASTData(function, Symbol(functionName, true), typeFromTypeNode(children[0], scope, templateTypeReplacements)));
    addToScope("~enclosing_scope", scope, functionDef);
    addToScope(functionName, functionDef, scope);
	//We only do the parameter nodes. We don't do the body yet, as this is the secondPass
    auto transChildren = transformChildren(slice(children,2,-2), std::set<int>(), functionDef, std::vector<Type>(), templateTypeReplacements);

	// std::cout << "REGULAR function " << functionName << " has " << transChildren.size() << " parameters: ";
	// for (auto i : transChildren)
	// 	std::cout << "||" << i->getDataRef()->toString() << "|| ";
	// std::cout << "DoneList" << std::endl;


	functionDef->addChildren(transChildren);
	return functionDef;
}

//The third pass finishes up by doing all function bodies
void ASTTransformation::thirdPass(NodeTree<ASTData>* ast, NodeTree<Symbol>* parseTree) {
	topScope = ast; //Top scope is maintained for templates, which need to add themselves to the top scope from where ever they are instantiated
	std::vector<NodeTree<Symbol>*> children = parseTree->getChildren();

	//Go through and finish both regular functions and class methods
	//Note that this pass can instantiate class AND function templates
	for (NodeTree<Symbol>* i : children) {
		if (i->getDataRef()->getName() == "type_def") {
			if (i->getChildren()[0]->getData().getName() == "template_dec") // It's a template
				continue;	//We've already set up the class templates
			std::vector<NodeTree<Symbol>*> typedefChildren = i->getChildren();
			std::string name = concatSymbolTree(typedefChildren[0]);
			NodeTree<ASTData>* typeDef = ast->getDataRef()->scope[name][0]; //No overloaded types

			//It's an alias. Note that typedefChildren.size() can equal one when it's a regular class with an empty body, i.e. {}
            if (typedefChildren.size() > 1 && typedefChildren[1]->getData().getName() == "type")
				continue;	//We're done with aliases too

			//Do the inside of classes here
			for (NodeTree<Symbol>* j : typedefChildren) {
				if (j->getDataRef()->getName() == "function") {
					thirdPassFunction(j, searchScopeForFunctionDef(typeDef, j, std::map<std::string, Type*>()), std::map<std::string, Type*>()); 	//do member method
				}
			}
		} else if (i->getDataRef()->getName() == "function") {
			//Do prototypes of functions
			if (i->getChildren()[0]->getData().getName() == "template_dec")
				continue; //We've already set up function templates
			thirdPassFunction(i, searchScopeForFunctionDef(ast, i, std::map<std::string, Type*>()), std::map<std::string, Type*>());
		}
	}

    // We do these here, in a loop, so that we can do mututally recursive definitions
    // even inside of class templates. As its methods may cause partial instantiation of
    // other class templates, we need to do this until the size no longer changes.
    std::vector<NodeTree<ASTData>*> classTemplates;
    int lastSize = 0;
    while (lastSize != ast->getDataRef()->scope.size()) {
        lastSize = ast->getDataRef()->scope.size();
        classTemplates.clear();
        for (auto i : ast->getDataRef()->scope) {
            if (i.second[0]->getDataRef()->type == type_def && i.second[0]->getDataRef()->valueType->templateTypeReplacement.size()) {
                classTemplates.push_back(i.second[0]);
                std::cout << "Saving " << i.second[0]->getDataRef()->toString() << " to instantiate." << std::endl;
            }
        }
        for (auto i : classTemplates) {
            Type* classTemplateType = i->getDataRef()->valueType;
            std::cout << "Instantiating template " << i->getDataRef()->toString() << std::endl;
            for (NodeTree<Symbol>* j : classTemplateType->templateDefinition->getChildren())
                if (j->getDataRef()->getName() == "function")
                    thirdPassFunction(j, searchScopeForFunctionDef(i, j, classTemplateType->templateTypeReplacement), classTemplateType->templateTypeReplacement); 	//do member method
            classTemplateType->templateTypeReplacement.clear(); // This template has been fully instantiated, clear it's map so it won't be instantiated again
        }
    }
}

//This function finds the right AST definition in a scope given its parseTree
NodeTree<ASTData>* ASTTransformation::searchScopeForFunctionDef(NodeTree<ASTData>* scope, NodeTree<Symbol>* parseTree, std::map<std::string, Type*> templateTypeReplacements) {
	std::string functionName = concatSymbolTree(parseTree->getChildren()[1]);
	std::vector<Type> types;
	std::vector<NodeTree<Symbol>*> children = parseTree->getChildren();
	//Skipping the initial return type and identifier as well as the final code block
	std::cout << "\n Searching scope for function def, function is :" << concatSymbolTree(children[1]) << ", children size is " << children.size() << std::endl;
	for (int i = 2; i < children.size()-1; i+=2) { //Skip over commas
		std::cout << "Making type for lookup ||" << concatSymbolTree(children[i]) << "||" << std::endl;
		Type type = *typeFromTypeNode(children[i]->getChildren()[0], scope, templateTypeReplacements);
		std::cout << "Type made: " << type.toString() << std::endl;
		types.push_back(type);
	}
	std::cout << "About to search scope about " << concatSymbolTree(children[1]) << std::endl;
	NodeTree<ASTData>* result = functionLookup(scope, functionName, types);
	std::cout << "Done searching scope about " << concatSymbolTree(children[1]) << std::endl;
	return result;
}

//This function does the function bodies given its start (the prototype)
//It is used in the third pass to finish things up
//Note that it may instantiate class OR function templates, which need to be fully instantiated
void ASTTransformation::thirdPassFunction(NodeTree<Symbol>* from, NodeTree<ASTData>* functionDef, std::map<std::string, Type*> templateTypeReplacements) {
	NodeTree<Symbol>* codeBlock = from->getChildren()[from->getChildren().size()-1];
	functionDef->addChild(transform(codeBlock, functionDef, std::vector<Type>(), templateTypeReplacements));
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

    if (name == "identifier" || name == "scoped_identifier") {
		//Make sure we get the entire name
		std::string lookupName = concatSymbolTree(from);
		std::cout << "Looking up: " << lookupName << std::endl;
		if (types.size()) {
            newNode = functionLookup(scope, lookupName, types);
		    if (newNode == NULL) {
			    std::cerr << "scope lookup error! Could not find " << lookupName << " in identifier (functionLookup)" << std::endl;
			    throw "LOOKUP ERROR: " + lookupName;
		    }
        } else {
            auto possibleMatches = scopeLookup(scope, lookupName);
            if (!possibleMatches.size()) {
			    std::cerr << "scope lookup error! Could not find " << lookupName << " in identifier (scopeLookup)" << std::endl;
			    throw "LOOKUP ERROR: " + lookupName;
            }
            newNode = possibleMatches[0];
        }
        return newNode;
	} else if (name == "type_def") {
		//If it is an alisis of a type
		std::string typeAlias;
		std::cout << "The scope here at type_def is " << scope->getDataRef()->toString() << std::endl;
		if (children[1]->getData().getName() == "type") {
			typeAlias = concatSymbolTree(children[0]);
			newNode = scope->getDataRef()->scope[typeAlias][0];	//The node for this type_def has already been made by translation_unit.
															//This is done so that types that reference each other can be declared in any order

			newNode->getDataRef()->valueType = typeFromTypeNode(children[1], scope, templateTypeReplacements);
			skipChildren.insert(0); //Don't want any children, it's unnecessary for ailising
			skipChildren.insert(1);
		} else { //Is a struct or class
			Type* objectType = NULL;
			if (children[0]->getData().getName() == "template_dec") {
				typeAlias = concatSymbolTree(children[1]);
				std::cout << "Template Type!"<<std::endl;
				newNode = scope->getDataRef()->scope[typeAlias][0];	//The node for this type_def has already been made by translation_unit.
																	//This is done so that types that reference each other can be declared in any order
				// std::cout << "typeAlias is " << typeAlias << " and newNode is " << newNode << std::endl;

				//So we give this typedef its name without any template types and make its type template_type, and point to this from node.
				//Then, when this template is instantiated, it will run transform on from with the types filled in.
				objectType = new Type(template_type, from);
			} else {
				typeAlias = concatSymbolTree(children[0]);

				newNode = scope->getDataRef()->scope[typeAlias][0];	//The node for this type_def has already been made by translation_unit.
																	//This is done so that types that reference each other can be declared in any order

				objectType = new Type(newNode);
				skipChildren.insert(0); //Identifier lookup will be ourselves, as we just added ourselves to the scope
			}
			newNode->getDataRef()->valueType = objectType; //Type is self-referential since this is the definition
		}
        // ?? why not this?
		//scope->getDataRef()->scope[typeAlias].push_back(newNode);
		//newNode->getDataRef()->scope["~enclosing_scope"].push_back(scope);
        addToScope("~enclosing_scope", scope, newNode);


		//Templates are done here. No need to go farther
		if (children[0]->getData().getName() == "template_dec")
			return newNode;
		scope = newNode;
	} else if (name == "function") {
		std::string functionName;
/*MULTHERE*/		//If this is a function template
		if (children[0]->getData().getName() == "template_dec") {
			functionName = concatSymbolTree(children[2]);
			newNode = new NodeTree<ASTData>(name, ASTData(function, Symbol(functionName, true), new Type(template_type, from)));
            addToScope(functionName, newNode, scope);
            addToScope("~enclosing_scope", scope, newNode);
			std::map<std::string, Type*> yetToBeInstantiatedTemplateTypes; //So that template types (like T) that have not been placed yet are found and given
																			//a special Type() - baseType = template_type_type
		    for (auto i : slice(children[0]->getChildren(), 1, -1, 2)) //skip commas
                yetToBeInstantiatedTemplateTypes[concatSymbolTree(i)] = new Type(template_type_type); //This may have to be combined with templateTypeReplacements if we do templated member functions inside of templated classes

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
        for (auto child: children)
            std::cout << "Function child: " << child->getDataRef()->toString() << std::endl;
		newNode = new NodeTree<ASTData>(name, ASTData(function, Symbol(functionName, true), typeFromTypeNode(children[0], scope, templateTypeReplacements)));
		skipChildren.insert(0);
		skipChildren.insert(1);
        addToScope(functionName, newNode, scope);
        addToScope("~enclosing_scope", scope, newNode);
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
        addToScope("~enclosing_scope", scope, newNode);
		scope = newNode;
	} else if (name == "typed_parameter") {
		//newNode = transform(children[1]); //Transform to get the identifier
		std::string parameterName = concatSymbolTree(children[1]);
		std::cout << "Doing typed parameter " << parameterName << std::endl;
		//std::string typeString = concatSymbolTree(children[0]);//Get the type (left child) and set our new identifer to be that type
		newNode = new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(parameterName, true), typeFromTypeNode(children[0], scope, templateTypeReplacements)));
        addToScope(parameterName, newNode, scope);
        addToScope("~enclosing_scope", scope, newNode);
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
				std::cerr << "scope lookup error! Could not find " << functionCallString << " in boolean stuff " << std::endl;
				throw "LOOKUP ERROR: " + functionCallString;
			}
			newNode = function;
			// newNode = new NodeTree<ASTData>(functionCallString, ASTData(function_call, function->getDataRef()->valueType));
			// newNode->addChild(function); // First child of function call is a link to the function
			// newNode->addChildren(transformedChildren);
		} else {
			//std::cout << children.size() << std::endl;
            // XXX What the heck is this
			if (children.size() == 0)
				return new NodeTree<ASTData>();
			return transform(children[0], scope, types, templateTypeReplacements); //Just a promoted term, so do child
		}
	//Here's the order of ops stuff
	} else if (name == "expression" || name == "shiftand" || name == "term" || name == "unarad" || name == "access_operation") {
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
			if (functionCallName == "[")
                functionCallName = "[]"; //fudge the lookup of brackets because only one is at children[1] (the other is at children[3])
            //std::cout << "scope lookup from expression or similar" << std::endl;
			std::vector<NodeTree<ASTData>*> transformedChildren; transformedChildren.push_back(lhs); transformedChildren.push_back(rhs);
			newNode = doFunction(scope, functionCallName, transformedChildren, templateTypeReplacements);
			if (newNode == NULL) {
				std::cerr << "scope lookup error! Could not find " << functionCallName << " in expression " << std::endl;
				throw "LOOKUP ERROR: " + functionCallName;
			}
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
            // I think this is where we look at pre vs post operators
			if (funcName == "*" || funcName == "&" || funcName == "++" || funcName == "--" || funcName == "+" || funcName == "-" || funcName == "!" || funcName == "~")
				param = transform(children[1], scope, types, templateTypeReplacements);
			else
				funcName = concatSymbolTree(children[1]), param = transform(children[0], scope, types, templateTypeReplacements);

            std::cout << "\t\t\t funcName= " << funcName << " param: " << param->getDataRef()->symbol.getName() << std::endl;
			//std::cout << "scope lookup from factor" << std::endl;
			std::vector<NodeTree<ASTData>*> transformedChildren; transformedChildren.push_back(param);
			NodeTree<ASTData>* function = doFunction(scope, funcName, transformedChildren, templateTypeReplacements);
            std::cout << "\t\t\t AFTER dofunction= " << std::endl;
			if (function == NULL) {
				std::cerr << "scope lookup error! Could not find " << funcName  << " in factor " << std::endl;
				throw "LOOKUP ERROR: " + funcName;
			}

			return function;
		} else {
			return transform(children[0], scope, types, templateTypeReplacements); //Just a promoted child, so do it instead
		}
	} else if (name == "statement") {
        //XXX
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
				std::cerr << "scope lookup error! Could not find " << functionName  << " in assignment_statement " << std::endl;
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
        addToScope(newIdentifierStr, newIdentifier, scope);
        addToScope("~enclosing_scope", scope, newNode);
        addToScope("~enclosing_scope", newNode, newIdentifier);
		newNode->addChild(newIdentifier);

        if (children.size() > 2 && concatSymbolTree(children[2]) == ".") {
            //A bit of a special case for declarations - if there's anything after just the normal 1 node declaration, it's either
            //an expression that is assigned to the declaration (int a = 4;) or a member call (Object a.constructAThing())
            //This code is a simplified version of the code in function_call with respect to access_operation.
            //Note that in this case, what is lhs there is our newIdentifier here (the declaration of the left side of the access operation)
            auto sliced = slice(children, 4, -1);
            std::vector<NodeTree<ASTData>*> initPositionFuncParams = transformChildren(sliced, std::set<int>(), scope, types, templateTypeReplacements);
            NodeTree<ASTData>* rhs = transform(children[3], identifierType->typeDefinition, mapNodesToTypes(initPositionFuncParams), templateTypeReplacements); //If an access operation, then the right side will be in the lhs's type's scope
			std::vector<NodeTree<ASTData>*> transformedChildren; transformedChildren.push_back(newIdentifier); transformedChildren.push_back(rhs);
			NodeTree<ASTData>* accessFuncCall = doFunction(scope, ".", transformedChildren, templateTypeReplacements);
		    accessFuncCall->getDataRef()->valueType = rhs->getDataRef()->valueType;
            //Now we borrow a bit of code from function_call below to actually use our new accessFuncCall to setup a "initPosition" function call
            //that will follow the identifier in this declaration node
            std::string initPosFuncName = newIdentifierStr + "." + concatSymbolTree(children[3]);
            NodeTree<ASTData>* initPositionFuncCall = new NodeTree<ASTData>(initPosFuncName, ASTData(function_call, Symbol(initPosFuncName, true)));
            initPositionFuncCall->addChild(accessFuncCall);
            initPositionFuncCall->getDataRef()->valueType = accessFuncCall->getDataRef()->valueType;
            initPositionFuncCall->addChildren(initPositionFuncParams);
            newNode->addChild(initPositionFuncCall);
            return newNode;
        }

		skipChildren.insert(0); //These, the type and the identifier, have been taken care of.
		skipChildren.insert(1);
        newNode->addChildren(transformChildren(children, skipChildren, scope, types, templateTypeReplacements));
	    return newNode;
    } else if (name == "if_comp") {
		newNode = new NodeTree<ASTData>(name, ASTData(if_comp));
		newNode->addChild(addToScope("~enclosing_scope", scope, new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(concatSymbolTree(children[0]),true)))));
        std::cout << "XXX scope is " << scope << std::endl;
        addToScope("~enclosing_scope", scope, newNode);
		skipChildren.insert(0); //Don't do the identifier. The identifier lookup will fail. That's why we do it here.
	} else if (name == "simple_passthrough") {
		newNode = new NodeTree<ASTData>(name, ASTData(simple_passthrough));
        addToScope("~enclosing_scope", scope, newNode);
        std::cout << "XXX scope is " << scope << std::endl;
	} else if (name == "passthrough_params") {
		newNode = new NodeTree<ASTData>(name, ASTData(passthrough_params));
        addToScope("~enclosing_scope", scope, newNode);
	} else if (name == "in_passthrough_params") {
		newNode = new NodeTree<ASTData>(name, ASTData(in_passthrough_params));
        addToScope("~enclosing_scope", scope, newNode);
	} else if (name == "out_passthrough_params") {
		newNode = new NodeTree<ASTData>(name, ASTData(out_passthrough_params));
        addToScope("~enclosing_scope", scope, newNode);
	} else if (name == "opt_string") {
		newNode = new NodeTree<ASTData>(name, ASTData(opt_string));
        addToScope("~enclosing_scope", scope, newNode);
	} else if (name == "param_assign") {
		newNode = new NodeTree<ASTData>(name, ASTData(param_assign));
        addToScope("~enclosing_scope", scope, newNode);
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
        addToScope("~enclosing_scope", scope, newNode);
	} else if (name == "number") {
		return transform(children[0], scope, types, templateTypeReplacements);
	} else if (name == "integer") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(from), true), new Type(integer)));
	} else if (name == "floating_literal") {
        std::string literal = concatSymbolTree(from);
        ValueType type = double_percision;
        if (literal.back() == 'f') {
            literal = literal.substr(0, literal.length()-1);
            type = floating;
        } else if (literal.back() == 'd') {
            literal = literal.substr(0, literal.length()-1);
            type = double_percision;
        }
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(literal, true), new Type(type)));
	} else if (name == "char") { //Is this correct? This might be a useless old thing
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(children[0]), true), new Type(character, 1))); //Indirection of 1 for array
	} else if (name == "string" || name == "triple_quoted_string") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(children[0]), true), new Type(character, 1))); //Indirection of 1 for array
	} else if (name == "character") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(children[0]), true), new Type(character, 0))); //Indirection of 0 for character
	} else if (name == "bool") {
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(concatSymbolTree(children[0]), true), new Type(boolean, 0))); //Indirection of 0 for character
	} else if (name == "AmbiguityPackOuter" || name == "AmbiguityPackInner") {
        std::cerr << "///////////////////////////////////////////////////////////////////////////////" << std::endl;
        std::cerr << "Ambigious program when parsed by this grammer! This is a bug, please report it." << std::endl;
        std::cerr << "///////////////////////////////////////////////////////////////////////////////" << std::endl;
        throw "Ambigious parse!";
    } else {
        // Should get rid of this eventually. Right now it handles cases like sign, alpha, a comma, etc
        std::cout << "Unhandled syntax node: " << name << std::endl;
		return new NodeTree<ASTData>();
	}

	//Do all children but the ones we skip
    newNode->addChildren(transformChildren(children, skipChildren, scope, types, templateTypeReplacements));
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
	auto LLElementIterator = languageLevelOperators.find(lookup);
	NodeTree<ASTData>* newNode;
	if (LLElementIterator != languageLevelOperators.end()) {
		std::cout << "Checking for early method level operator overload" << std::endl;
		std::string lookupOp = "operator" + lookup;
		for (auto i : nodes)
			std::cout << i->getDataRef()->toString() << " ";
		std::cout << std::endl;
		NodeTree<ASTData>* operatorMethod = NULL;
		if (nodes[0]->getDataRef()->valueType && nodes[0]->getDataRef()->valueType->typeDefinition)
			operatorMethod = functionLookup(nodes[0]->getDataRef()->valueType->typeDefinition, lookupOp, mapNodesToTypes(slice(nodes,1,-1)));
		if (operatorMethod) {
			//Ok, so we construct
			std::cout << "Early method level operator was found" << std::endl;
			//return operatorMethod;
			NodeTree<ASTData>* newNode = new NodeTree<ASTData>(lookupOp, ASTData(function_call, Symbol(lookupOp, true)));
			NodeTree<ASTData>* dotFunctionCall = new NodeTree<ASTData>(".", ASTData(function_call, Symbol(".", true)));
			dotFunctionCall->addChild(languageLevelOperators["."][0]); //function definition
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
	NodeTree<ASTData>* function = functionLookup(scope, lookup, mapNodesToTypes(nodes));
    std::cout << "Num of newNode children " <<  newNode->getChildren().size() << std::endl;
	newNode->addChild(function);
    std::cout << "Num of newNode children " <<  newNode->getChildren().size() << std::endl;
	newNode->addChildren(nodes);
    std::cout << "Num of newNode children " <<  newNode->getChildren().size() << std::endl;
    std::cout << "nodes " <<  nodes.size() << std::endl;

	//Specially handle dereference and address of to assign the correct type
	//We need some significant other type corrections here, maybe to the point of being their own function. (int + float, etc.)
    std::cout << "the passed in nodes" << std::endl;
	for (auto i : nodes)
		std::cout << i->getDataRef()->toString() << " ";
	std::cout<<std::endl;


	std::vector<Type> oldTypes = mapNodesToTypes(nodes);
    std::cout << "the oldtypes size" << oldTypes.size() << std::endl;
	if ((nodes.size() != 2 && lookup == "*") || lookup == "&" || lookup == "[]") {
		Type* newType = oldTypes[0].clone();
		if (lookup == "*" || lookup == "[]")
			newType->decreaseIndirection();
		else
			newType->increaseIndirection();

		newNode->getDataRef()->valueType = newType, std::cout << "Operator " + lookup << " is altering indirection from " << oldTypes[0].toString() << " to " << newType->toString() << std::endl;
	} else {
		newNode->getDataRef()->valueType = function->getDataRef()->valueType, std::cout << "Some other ||" << lookup << "||" << std::endl;
	}

    // Set the value of this function call if it has not already been set
    // It's important that it's the last parameter, the rhs if it has one
    // because of the . operator, etc
    if (newNode->getDataRef()->valueType == NULL) {
        std::cout << "The value type from doFunction was null! (for " << lookup << ")" << std::endl;
        newNode->getDataRef()->valueType = oldTypes[oldTypes.size()-1].clone();
        std::cout << "function call to " << lookup << " - " << newNode->getName() << " is now " << newNode->getDataRef()->valueType  << std::endl;
    }
	return newNode;
}

//Lookup a function that takes in parameters matching the types passed in
NodeTree<ASTData>* ASTTransformation::functionLookup(NodeTree<ASTData>* scope, std::string lookup, std::vector<Type> types) {
	//We first check to see if it's one of the special reserved identifiers (only this, for now) and return early if it is.
	auto LLElementIterator = languageLevelReservedWords.find(lookup);
	if (LLElementIterator != languageLevelReservedWords.end()) {
		std::cout << "found it at language level as reserved word." << std::endl;
		return LLElementIterator->second[0];
    }
    //We search the languageLevelOperators to see if it's an operator. If so, we modifiy the lookup with a preceding "operator"
	LLElementIterator = languageLevelOperators.find(lookup);
	if (LLElementIterator != languageLevelOperators.end())
		lookup = "operator" + lookup;

    //Look up the name
    std::vector<NodeTree<ASTData>*> possibleMatches = scopeLookup(scope, lookup);
    std::cout << "Function lookup of " << lookup << " has " << possibleMatches.size() << " possible matches." << std::endl;
    if (possibleMatches.size()) {
		for (auto i : possibleMatches) {
			//We're not looking for types
			if (i->getDataRef()->type == type_def)
				continue;

            std::vector<NodeTree<ASTData>*> children = i->getChildren();
			//We subtract one from the children to get the type size only if there is at least one child AND
			// the last node is actually a body node, as it may not have been generated yet if we're in the body
			//and this function is recursive or if this is a non-instantiated template function
            int numTypes = (children.size() > 0 && children[children.size()-1]->getDataRef()->type == code_block) ? children.size()-1 : children.size();
			if (types.size() != numTypes) {
				std::cout << "Type sizes do not match between two " << lookup << "(" << types.size() << "," << numTypes << "), types are: ";
				for (auto j : types)
					std::cout << j.toString() << " ";
				std::cout << std::endl;
                std::cout << "Versus" << std::endl;
                for (int j = 0; j < numTypes; j++) {
                    std::cout << " vs " << children[j]->getDataRef()->valueType->toString() << std::endl;
                }
                for (auto child: children)
                    std::cout << "\t" << child->getDataRef()->toString() << std::endl;
                std::cout << std::endl;
				continue;
			}
			bool typesMatch = true;
			for (int j = 0; j < types.size(); j++) {
				Type* tmpType = children[j]->getDataRef()->valueType;
				//Don't worry if types don't match if it's a template type
				// std::cout << "Checking for segfaults, we have" << std::endl;
				// std::cout << types[j].toString() << std::endl;
				// std::cout << tmpType->toString() << std::endl;
				// std::cout << "Done!" << std::endl;
				if (types[j] != *tmpType && tmpType->baseType != template_type_type) {
					typesMatch = false;
					std::cout << "Types do not match between two " << lookup << " " << types[j].toString();
					std::cout << " vs " << children[j]->getDataRef()->valueType->toString() << std::endl;
					break;
				}
			}
			if (typesMatch)
				return i;
		}
	}

	std::cout << "could not find " << lookup << " in standard scopes, checking for operator" << std::endl;
	//Note that we don't check for types. At some point we should, as we don't know how to add objects/structs without overloaded operators, etc
	//Also, we've already searched for the element because this is also how we keep track of operator overloading
	if (LLElementIterator != languageLevelOperators.end()) {
		std::cout << "found it at language level as operator." << std::endl;
		return LLElementIterator->second[0];
	}
	std::cout << "Did not find, returning NULL" << std::endl;
    return NULL;
}

//Lookup class templates. It evaluates possible matches on traits
NodeTree<ASTData>* ASTTransformation::templateClassLookup(NodeTree<ASTData>* scope, std::string lookup, std::vector<Type*> templateInstantiationTypes) {
    std::set<NodeTree<ASTData>*> mostFittingTemplates;
    int bestNumTraitsSatisfied = -1;
    auto possibleMatches = scopeLookup(scope, lookup);
    std::cout << "Template Class instantiation has " << possibleMatches.size() << " possible matches." << std::endl;
    for (auto i : possibleMatches) {
	    NodeTree<Symbol>* templateSyntaxTree = i->getDataRef()->valueType->templateDefinition;

        auto nameTraitsPairs = makeTemplateNameTraitPairs(templateSyntaxTree->getChildren()[0]);
        //Check if sizes match between the placeholder and actual template types
        if (nameTraitsPairs.size() != templateInstantiationTypes.size())
            continue;

        bool traitsEqual = true;
        int typeIndex = 0;
        int currentTraitsSatisfied = 0;
        for (auto j : nameTraitsPairs) {
            if (!subset(j.second, templateInstantiationTypes[typeIndex]->traits)) {
                traitsEqual = false;
                std::cout << "Traits not subset for " << j.first << " and " << templateInstantiationTypes[typeIndex]->toString() << ": ";
				//std::cout <<  baseType << " " <<  indirection << " " << typeDefinition << " " <<  templateDefinition << " " <<  traits << ;
                std::copy(j.second.begin(), j.second.end(), std::ostream_iterator<std::string>(std::cout, " "));
                std::cout << " vs ";
                std::copy(templateInstantiationTypes[typeIndex]->traits.begin(), templateInstantiationTypes[typeIndex]->traits.end(), std::ostream_iterator<std::string>(std::cout, " "));
                std::cout << std::endl;
                break;
            } else {
                std::cout << "Traits ARE subset for " << j.first << " and " << templateInstantiationTypes[typeIndex]->toString() << ": ";
				//std::cout <<  baseType << " " <<  indirection << " " << typeDefinition << " " <<  templateDefinition << " " <<  traits << ;
                std::copy(j.second.begin(), j.second.end(), std::ostream_iterator<std::string>(std::cout, " "));
                std::cout << " vs ";
                std::copy(templateInstantiationTypes[typeIndex]->traits.begin(), templateInstantiationTypes[typeIndex]->traits.end(), std::ostream_iterator<std::string>(std::cout, " "));
                std::cout << std::endl;
            }
            currentTraitsSatisfied += j.second.size();
            typeIndex++;
        }
        if (!traitsEqual)
            continue;

        //See if this is a better match than the current best
        if (currentTraitsSatisfied > bestNumTraitsSatisfied) {
            mostFittingTemplates.clear();
            std::cout << "Class satisfying " << currentTraitsSatisfied << " beats previous " << bestNumTraitsSatisfied << std::endl;
            bestNumTraitsSatisfied = currentTraitsSatisfied;
        } else if (currentTraitsSatisfied < bestNumTraitsSatisfied)
            continue;
        mostFittingTemplates.insert(i);
        std::cout << "Current class fits, satisfying " << currentTraitsSatisfied << " traits" << std::endl;
    }
    if (!mostFittingTemplates.size()) {
        std::cout << "No template classes fit for " << lookup << "!" << std::endl;
        throw "No matching template classes";
    } else if (mostFittingTemplates.size() > 1) {
        std::cout << "Multiple template classes fit with equal number of traits satisfied for " << lookup << "!" << std::endl;
        throw "Multiple matching template classes";
    }
    return *mostFittingTemplates.begin();
}

//Lookup function for template functions. It has some extra concerns compared to function lookup, namely traits
NodeTree<ASTData>* ASTTransformation::templateFunctionLookup(NodeTree<ASTData>* scope, std::string lookup, std::vector<Type*> templateInstantiationTypes, std::vector<Type> types) {
    std::set<NodeTree<ASTData>*> mostFittingTemplates;
    int bestNumTraitsSatisfied = -1;
    auto possibleMatches = scopeLookup(scope, lookup);
    std::cout << "Template Function instantiation has " << possibleMatches.size() << " possible matches." << std::endl;
    int index = 1;
    for (auto i : possibleMatches) {
        std::cout << "Possibility " << index++ << std::endl;
	    NodeTree<Symbol>* templateSyntaxTree = i->getDataRef()->valueType->templateDefinition;
        if (!templateSyntaxTree) {
            std::cout << "Not a template, skipping" << std::endl;
            continue;
        }

        auto nameTraitsPairs = makeTemplateNameTraitPairs(templateSyntaxTree->getChildren()[0]);
        //Check if sizes match between the placeholder and actual template types
        if (nameTraitsPairs.size() != templateInstantiationTypes.size())
            continue;

        std::map<std::string, Type*> typeMap;
        bool traitsEqual = true;
        int typeIndex = 0;
        int currentTraitsSatisfied = 0;
        for (auto j : nameTraitsPairs) {
            if (!subset(j.second, templateInstantiationTypes[typeIndex]->traits)) {
                traitsEqual = false;
                std::cout << "Traits not a subset for " << j.first << " and " << templateInstantiationTypes[typeIndex]->toString() << ": ";
                std::copy(j.second.begin(), j.second.end(), std::ostream_iterator<std::string>(std::cout, " "));
                std::cout << " vs ";
                std::copy(templateInstantiationTypes[typeIndex]->traits.begin(), templateInstantiationTypes[typeIndex]->traits.end(), std::ostream_iterator<std::string>(std::cout, " "));
                std::cout << std::endl;
                break;
            } else {
                std::cout << "Traits ARE a subset for " << j.first << " and " << templateInstantiationTypes[typeIndex]->toString() << ": ";
                std::copy(j.second.begin(), j.second.end(), std::ostream_iterator<std::string>(std::cout, " "));
                std::cout << " vs ";
                std::copy(templateInstantiationTypes[typeIndex]->traits.begin(), templateInstantiationTypes[typeIndex]->traits.end(), std::ostream_iterator<std::string>(std::cout, " "));
                std::cout << std::endl;
            }
            //As we go, build up the typeMap for when we transform the parameters for parameter checking
            typeMap[j.first] = templateInstantiationTypes[typeIndex];
            currentTraitsSatisfied += j.second.size();
            typeIndex++;
        }
        if (!traitsEqual)
            continue;

        std::vector<NodeTree<Symbol>*> functionParameters = slice(templateSyntaxTree->getChildren(), 3, -2, 2); //skip template, return type, name, intervening commas, and the code block
        std::cout << functionParameters.size() << " " << types.size() << std::endl;
        if (functionParameters.size() != types.size())
            continue;

        bool parameterTypesMatch = true;
        for (int j = 0; j < functionParameters.size(); j++) {
            auto paramType = typeFromTypeNode(functionParameters[j]->getChildren()[0], scope, typeMap);
            std::cout << "Template param type: " << paramType->toString() << " : Needed Type: " << types[j].toString() << std::endl;
            if (*paramType != types[j]) {
                parameterTypesMatch = false;
                std::cout << "Not equal template param: " << paramType->toString() << " : Needed Type actual param: " << types[j].toString() << std::endl;
                break;
            }
        }
        if (!parameterTypesMatch)
            continue;
        //See if this is a better match than the current best
        if (currentTraitsSatisfied > bestNumTraitsSatisfied) {
            mostFittingTemplates.clear();
            std::cout << "Function satisfying " << currentTraitsSatisfied << " beats previous " << bestNumTraitsSatisfied << std::endl;
            bestNumTraitsSatisfied = currentTraitsSatisfied;
        } else if (currentTraitsSatisfied < bestNumTraitsSatisfied)
            continue;
        mostFittingTemplates.insert(i);
        std::cout << "Current function fits, satisfying " << currentTraitsSatisfied << " traits" << std::endl;
    }
    if (!mostFittingTemplates.size()) {
        std::cerr << "No template functions fit for " << lookup << "!" << std::endl;
        throw "No matching template functions";
    } else if (mostFittingTemplates.size() > 1) {
        std::cerr << "Multiple template functions fit with equal number of traits satisfied for " << lookup << "!" << std::endl;
        throw "Multiple matching template functions";
    }
    return *mostFittingTemplates.begin();
}

//Extract pairs of type names and traits
std::vector<std::pair<std::string, std::set<std::string>>> ASTTransformation::makeTemplateNameTraitPairs(NodeTree<Symbol>* templateNode) {
    std::vector<NodeTree<Symbol>*> templateParams = slice(templateNode->getChildren(), 1, -2, 2); //Skip <, >, and interveaning commas
    std::vector<std::pair<std::string, std::set<std::string>>> typePairs;
    for (auto i : templateParams) {
        if (i->getChildren().size() > 1)
            typePairs.push_back(std::make_pair(concatSymbolTree(i->getChildren()[0]), parseTraits(i->getChildren()[1])));
        else
            typePairs.push_back(std::make_pair(concatSymbolTree(i->getChildren()[0]), std::set<std::string>()));
    }
    return typePairs;
}

std::map<std::string, Type*> ASTTransformation::makeTemplateFunctionTypeMap(NodeTree<Symbol>* templateNode, std::vector<Type*> types) {
    auto typePairs = makeTemplateNameTraitPairs(templateNode);
    std::map<std::string, Type*> typeMap;
    int typeIndex = 0;
    std::cout << typePairs.size() << " " << types.size() << std::endl;
    for (auto i : typePairs) {
        typeMap[i.first] = types[typeIndex];
        std::cout << "Mapping " << i.first << " to " << types[typeIndex]->toString() << std::endl;
        typeIndex++;
    }
    return typeMap;
}

// We need recursion protection
std::vector<NodeTree<ASTData>*> ASTTransformation::scopeLookup(NodeTree<ASTData>* scope, std::string lookup, bool includeModules) {
    return scopeLookup(scope, lookup, includeModules, std::vector<NodeTree<ASTData>*>());
}

std::vector<NodeTree<ASTData>*> ASTTransformation::scopeLookup(NodeTree<ASTData>* scope, std::string lookup, bool includeModules, std::vector<NodeTree<ASTData>*> visited) {
    std::cout << "Scp]|[e looking up " << lookup << std::endl;
    // Don't visit this node again when looking for the smae lookup. Note that we don't prevent coming back for the scope operator, as that should be able to come back.
    visited.push_back(scope);
	//We first check to see if it's one of the special reserved identifiers (only this, for now) and return early if it is.
	auto LLElementIterator = languageLevelReservedWords.find(lookup);
	if (LLElementIterator != languageLevelReservedWords.end()) {
		std::cout << "found it at language level as reserved word." << std::endl;
		return LLElementIterator->second;
    }
    std::vector<NodeTree<ASTData>*> matches;
    // First, we check for scope operator (::) but only if occurs before a "<" as this would signal the beginning of a template instatiation inside type
    // If we find it, we look up the left side of the :: and then use the resuts as the scope for looking up the right side, recursively.
    size_t scopeOpPos = lookup.find("::");
    size_t angleBrktPos = lookup.find("<");
    if (scopeOpPos != std::string::npos && (angleBrktPos == std::string::npos || scopeOpPos < angleBrktPos)) {
		std::cout << "Has :: operator, doing left then right" << std::endl;
        for (auto scopeMatch : scopeLookup(scope, strSlice(lookup, 0, scopeOpPos), true)) {
            std::cout << "Trying right side with found left side " << scopeMatch->getDataRef()->toString()  << std::endl;
            auto addMatches = scopeLookup(scopeMatch, strSlice(lookup, scopeOpPos+2, -1), includeModules);
            matches.insert(matches.end(), addMatches.begin(), addMatches.end());
        }
        return matches;
    }

    std::map<std::string, std::vector<NodeTree<ASTData>*>> scopeMap = scope->getDataRef()->scope;
    auto possibleMatches = scopeMap.find(lookup);
    if (possibleMatches != scopeMap.end()) {
        for (auto i : possibleMatches->second)
            if (includeModules || i->getName() != "translation_unit")
                matches.push_back(i);
        std::cout << "Found " << possibleMatches->second.size() << " match(s) at " << scope->getDataRef()->toString() << std::endl;
    }
    // Add results from our enclosing scope, if it exists.
    // If it doesn't we should be at the top of a translation unit, and we should check the scope of import statements.
    auto enclosingIterator = scopeMap.find("~enclosing_scope");
	if (enclosingIterator != scopeMap.end()) {
        std::vector<NodeTree<ASTData>*> upperResult = scopeLookup(enclosingIterator->second[0], lookup, includeModules, visited);
	    matches.insert(matches.end(), upperResult.begin(), upperResult.end());
    } else {
        // Ok, let's iterate through and check for imports
        for (auto child : scope->getChildren()) {
            if (child->getDataRef()->type == import) {
                auto importScope = child->getDataRef()->scope;
                // Check if there is a match named explicily in the import's scope (i.e. looking for a and the import is import somefile: a;)
                // If so, add it's members to our matches
                auto importLookupItr = importScope.find(lookup);
                if (importLookupItr != importScope.end()) {
                    auto addMatches = importLookupItr->second;
                    matches.insert(matches.end(), addMatches.begin(), addMatches.end());
                }
                // Check if there is an uncionditional import to follow (i.e. import somefile: *;)
                // If so, continue the search in that scope
                auto importStarItr = importScope.find("*");
                if (importStarItr != importScope.end()) {
                    auto addMatches = scopeLookup(importStarItr->second[0], lookup, includeModules, visited);
                    matches.insert(matches.end(), addMatches.begin(), addMatches.end());
                }
            }
        }
    }
	return matches;
}

// Find the translation unit that is the top of the passed in node
NodeTree<ASTData>* ASTTransformation::getUpperTranslationUnit(NodeTree<ASTData>* node) {
    auto scope = node->getDataRef()->scope;
    auto iter = scope.find("~enclosing_scope");
    while(iter != scope.end()) {
        node = iter->second[0];
        scope = node->getDataRef()->scope;
        iter = scope.find("~enclosing_scope");
    }
    return node;
}

//Create a type from a syntax tree. This can get complicated with templates
Type* ASTTransformation::typeFromTypeNode(NodeTree<Symbol>* typeNode, NodeTree<ASTData>* scope, std::map<std::string, Type*> templateTypeReplacements) {
	std::string typeIn = concatSymbolTree(typeNode);

	int indirection = 0;
	ValueType baseType;
	NodeTree<ASTData>* typeDefinition = NULL;
    std::set<std::string> traits;
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

        auto possibleMatches = scopeLookup(scope, edited);
        if (possibleMatches.size()) {
            typeDefinition = possibleMatches[0];
            traits = typeDefinition->getDataRef()->valueType->traits;
        }
		//So either this is an uninstatiated template class type, or this is literally a template type T, and we should get it from our
		//templateTypeReplacements map. We try this first
		if (templateTypeReplacements.find(edited) != templateTypeReplacements.end()) {
			std::cout << "Template type! (" << edited << ")" << std::endl;
			Type* templateTypeReplacement = templateTypeReplacements[edited]->clone();
			templateTypeReplacement->modifyIndirection(indirection);
			return templateTypeReplacement;
		}
		std::cout << edited << " was not found in templateTypeReplacements" << std::endl;
		std::cout << "templateTypeReplacements consists of : ";
		for (auto i : templateTypeReplacements)
			std::cout << i.first << " ";
		std::cout << std::endl;

        // getChildren()[1] is \* because of pointer instead of template_inst
        // To counter this, for every indirection we step down a level
        for (int i = 0; i < indirection; i++)
            typeNode = typeNode->getChildren()[0];

        std::cout << possibleMatches.size() << " " << typeNode->getChildren().size() << std::endl;
        if (typeNode->getChildren().size() > 1)
            std::cout << typeNode->getChildren()[1]->getDataRef()->getName() << std::endl;
        //If not, we better instantiate it and then add it to the highest (not current) scope
		if (possibleMatches.size() == 0 && typeNode->getChildren().size() > 1 && typeNode->getChildren()[1]->getData().getName() == "template_inst") {
		 	std::cout << "Template type: " << edited << " not yet instantiated" << std::endl;

            //We pull out the replacement types first so that we can choose the correct possibly overloaded template
            std::vector<NodeTree<Symbol>*> templateParamInstantiationNodes = slice(typeNode->getChildren()[1]->getChildren(), 1, -2, 2); //same
            std::vector<Type*> templateParamInstantiationTypes;
            std::string instTypeString = "";
            for (int i = 0; i < templateParamInstantiationNodes.size(); i++) {
                Type* instType = typeFromTypeNode(templateParamInstantiationNodes[i], scope, templateTypeReplacements);
                /*******************************************************************************
                 * WE RETURN EARLY IF ONE OF OUR REPLACEMENT INST TYPES IS TEMPLATE_TYPE_TYPE
                 * WE DO THIS BECAUSE WE CAN'T ACTUALLY INSTATNTIATE WITH THIS.
                 * THIS CAN HAPPEN IN THE FOLLOWING SITUATIONS.
                 * template<T> |T| fun(|vec<T>| a) { return a.at(0); }
                 * etc
                 *******************************************************************************/
                if (instType->baseType == template_type_type)
                    return instType;
                templateParamInstantiationTypes.push_back(instType);
                instTypeString += (instTypeString == "") ? instType->toString(false) : "," + instType->toString(false);
            }

            //Finish creating the new name for this instantiation
            std::string classNameWithoutTemplate = concatSymbolTree(typeNode->getChildren()[0]);
            std::string templateLookupName = classNameWithoutTemplate + "<" + instTypeString + ">";

            // Recheck for prior definition here, now that we have the true name.
            possibleMatches = scopeLookup(scope, templateLookupName);
            if (possibleMatches.size()) {
                typeDefinition = possibleMatches[0];
                traits = typeDefinition->getDataRef()->valueType->traits;
                std::cout << "Found already instantiated template of " << templateLookupName << " at second check" << std::endl;
            } else {
                std::cout << "Did not find already instantiated template of " << templateLookupName << " at second check" << std::endl;
                //Look up this template's plain definition. It's type has the syntax tree that we need to parse
                NodeTree<ASTData>* templateDefinition = templateClassLookup(scope, concatSymbolTree(typeNode->getChildren()[0]), templateParamInstantiationTypes);
                if (templateDefinition == NULL)
                    std::cout << "Template definition is null!" << std::endl;
                else
                    std::cout << "Template definition is not null!" << std::endl;

                std::string fullyInstantiatedName = templateDefinition->getDataRef()->symbol.getName() + "<" + instTypeString + ">";

                NodeTree<Symbol>* templateSyntaxTree = templateDefinition->getDataRef()->valueType->templateDefinition;
                //Create a new map of template type names to actual types.
                std::vector<NodeTree<Symbol>*> templateParamPlaceholderNodes = slice(templateSyntaxTree->getChildren()[0]->getChildren(), 1, -2, 2); //Don't get beginning or end for < or >, skip commas in the middle
                std::map<std::string, Type*> newTemplateTypeReplacement;
                for (int i = 0; i < templateParamInstantiationTypes.size(); i++)
                    newTemplateTypeReplacement[concatSymbolTree(templateParamPlaceholderNodes[i])] = templateParamInstantiationTypes[i];

                typeDefinition = new NodeTree<ASTData>("type_def", ASTData(type_def, Symbol(fullyInstantiatedName, true, fullyInstantiatedName)));
                traits = templateDefinition->getDataRef()->valueType->traits; // We have the same traits as the template definition
                Type* selfType = new Type(typeDefinition, traits); // Type is self-referential since this is the definition.
                typeDefinition->getDataRef()->valueType = selfType;

                //Note that we're adding to the current top scope. This makes it more efficient by preventing multiple instantiation and should not cause any problems
                //It also makes sure it gets generated in the right place
                //std::cout << "Adding to top scope and template's origional scope with fullyInstantiatedName " << fullyInstantiatedName << std::endl;
                //topScope->getDataRef()->scope[fullyInstantiatedName].push_back(typeDefinition);
                //topScope->addChild(typeDefinition); Add this object the the highest scope's

                // Actually, let's just put it in the scope of the origional template, which should work just fine under the new scoping rules and will ACTUALLY prevent multiple instantiation.
                // At least, hopefully it will if we also check it's scope for it. Which I think it should be anyway. Yeah, I think it should work.
                std::cout << "Adding to template top scope and template's origional scope with fullyInstantiatedName " << fullyInstantiatedName << std::endl;
                auto templateTopScope = getUpperTranslationUnit(templateDefinition);
                templateTopScope->getDataRef()->scope[fullyInstantiatedName].push_back(typeDefinition);
                templateTopScope->addChild(typeDefinition); // Add this object the the highest scope's

                //NodeTree<ASTData>* templateHighScope = templateDefinition->getDataRef()->scope["~enclosing_scope"][0];
                //if (topScope != templateHighScope)
                    //templateHighScope->getDataRef()->scope[fullyInstantiatedName].push_back(typeDefinition);
                // We put it in the scope of the template so that it can find itself (as it's scope is its template definition)
                addToScope(fullyInstantiatedName, typeDefinition, templateDefinition);
                //Note that the instantiated template's scope is the template's definition.
                addToScope("~enclosing_scope", templateDefinition, typeDefinition);

                // We only partially instantiate templates no matter what now
                // They are all fully instantiated in the loop at the end of the 4th pass
                // This is done for code simplicity and so that that loop can do template class methods
                // that instantiate other templates that instantiate other templates while still retaining the
                // deferred method allowing us to correctly instantiate multiple levels of mututally recursive definitions.
                selfType->templateDefinition = templateSyntaxTree; //We're going to still need this when we finish instantiating
                selfType->templateTypeReplacement = newTemplateTypeReplacement; //Save the types for use when this is fully instantiated in pass 4
                secondPassDoClassInsides(typeDefinition, templateSyntaxTree->getChildren(), newTemplateTypeReplacement); //Use these types when instantiating data members
            }
        } else if (possibleMatches.size() == 0) {
        	std::cout << "Could not find type " << edited << ", returning NULL" << std::endl;
    		return NULL;
		} else {
			std::cout << "Type: " << edited << " already instantiated with " << typeDefinition << ", will be " << Type(baseType, typeDefinition, indirection, traits).toString() << std::endl;
		}
	}
	Type* toReturn = new Type(baseType, typeDefinition, indirection, traits);
	std::cout << "Returning type " << toReturn->toString() << std::endl;
	return toReturn;
}

NodeTree<ASTData>* ASTTransformation::findOrInstantiateFunctionTemplate(std::vector<NodeTree<Symbol>*> children, NodeTree<ASTData>* scope, std::vector<Type> types, std::map<std::string, Type*> templateTypeReplacements) {
	//First look to see if we can find this already instantiated
	std::cout << "\n\nFinding or instantiating templated function\n\n" << std::endl;
	std::string functionName = concatSymbolTree(children[0]);

    auto unsliced = children[1]->getChildren();
    std::vector<NodeTree<Symbol>*> templateParamInstantiationNodes = slice(unsliced, 1 , -2, 2);//skip <, >, and commas
    std::string instTypeString = "";
    std::vector<Type*> templateActualTypes;
    for (int i = 0; i < templateParamInstantiationNodes.size(); i++) {
        Type* instType = typeFromTypeNode(templateParamInstantiationNodes[i],scope, templateTypeReplacements);
        instTypeString += (instTypeString == "" ? instType->toString() : "," + instType->toString());
        templateActualTypes.push_back(instType);
    }
    std::cout << "Size: " << templateParamInstantiationNodes.size() << std::endl;
	std::string fullyInstantiatedName = functionName + "<" + instTypeString + ">";
	std::cout << "Looking for " << fullyInstantiatedName << std::endl;

    std::cout << "Types are : ";
	for (auto i : types)
		std::cout << " " << i.toString();
	std::cout << std::endl;

	NodeTree<ASTData>* instantiatedFunction = functionLookup(scope, fullyInstantiatedName, types);
	//If it already exists, return it
	if (instantiatedFunction) {
		std::cout << fullyInstantiatedName << " already exists! Returning" << std::endl;
		return instantiatedFunction;
    } else {
        instantiatedFunction = functionLookup(topScope, fullyInstantiatedName, types);
        if (instantiatedFunction) {
            std::cout << fullyInstantiatedName << "already exists! Found in TopScope" << std::endl;
            return instantiatedFunction;
        }
		std::cout << fullyInstantiatedName << " does NOT exist" << std::endl;
	}

	//Otherwise, we're going to instantiate it
	//Find the template definitions
	NodeTree<ASTData>* templateDefinition = templateFunctionLookup(scope, functionName, templateActualTypes, types);
	if (templateDefinition == NULL) {
		std::cout << functionName << " search turned up null, returing null" << std::endl;
		return NULL;
	}

	NodeTree<Symbol>* templateSyntaxTree = templateDefinition->getDataRef()->valueType->templateDefinition;
	// Makes a map between the names of the template placeholder parameters and the provided types
    std::map<std::string, Type*> newTemplateTypeReplacement = makeTemplateFunctionTypeMap(templateSyntaxTree->getChildren()[0], templateActualTypes);

    std::vector<NodeTree<Symbol>*> templateChildren = templateSyntaxTree->getChildren();
	for (int i = 0; i < templateChildren.size(); i++)
		std::cout << ", " << i << " : " << templateChildren[i]->getDataRef()->getName();
	std::cout << std::endl;

	instantiatedFunction = new NodeTree<ASTData>("function", ASTData(function, Symbol(fullyInstantiatedName, true), typeFromTypeNode(templateChildren[1], scope, newTemplateTypeReplacement)));
	std::set<int> skipChildren;
	skipChildren.insert(0);
	skipChildren.insert(1);
	skipChildren.insert(2);
	//scope->getDataRef()->scope[fullyInstantiatedName].push_back(instantiatedFunction);
	//instantiatedFunction->getDataRef()->scope["~enclosing_scope"].push_back(templateDefinition->getDataRef()->scope["~enclosing_scope"][0]); //Instantiated Template Function's scope is it's template's definition's scope
    addToScope("~enclosing_scope", templateDefinition->getDataRef()->scope["~enclosing_scope"][0], instantiatedFunction);
    // Arrrrrgh this has a hard time working because the functions will need to see their parameter once they are emitted as C.
    // HAHAHAHAHA DOESN'T MATTER ALL ONE C FILE NOW, swap back to old way
    auto templateTopScope = getUpperTranslationUnit(templateDefinition);
    //templateTopScope->getDataRef()->scope[fullyInstantiatedName].push_back(instantiatedFunction);
    addToScope(fullyInstantiatedName, instantiatedFunction, templateTopScope);
    templateTopScope->addChild(instantiatedFunction); // Add this object the the highest scope's
    //topScope->getDataRef()->scope[fullyInstantiatedName].push_back(instantiatedFunction);
    //topScope->addChild(instantiatedFunction); //Add this object the the highest scope's

	std::cout << "About to do children of " << functionName << " to " << fullyInstantiatedName << std::endl;
	instantiatedFunction->addChildren(transformChildren(templateSyntaxTree->getChildren(), skipChildren, instantiatedFunction, std::vector<Type>(), newTemplateTypeReplacement));

	std::cout << "Fully Instantiated function " << functionName << " to " << fullyInstantiatedName << std::endl;
	return instantiatedFunction;
}

NodeTree<ASTData>* ASTTransformation::addToScope(std::string name, NodeTree<ASTData>* toAdd, NodeTree<ASTData>* addTo) {
    addTo->getDataRef()->scope[name].push_back(toAdd);
    return addTo;
}


