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
	languageLevelOperators["="].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("=", true), NULL))));
	languageLevelOperators["+="].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("+=", true), NULL))));
	languageLevelOperators["-="].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("-=", true), NULL))));
	languageLevelOperators["."].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol(".", true), NULL))));
	languageLevelOperators["->"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("->", true), NULL))));
	languageLevelOperators["[]"].push_back(addToScope("~enclosing_scope", builtin_trans_unit, new NodeTree<ASTData>("function", ASTData(function, Symbol("[]", true), NULL))));
}

ASTTransformation::~ASTTransformation() {
}

NodeTree<Symbol>* ASTTransformation::getNode(std::string lookup, NodeTree<Symbol>* parent) {
    auto results = getNodes(lookup, parent);
    if (results.size() > 1)
        throw "too many results";
    if (results.size())
        return results[0];
    return nullptr;
}
NodeTree<Symbol>* ASTTransformation::getNode(std::string lookup, std::vector<NodeTree<Symbol>*> nodes) {
    auto results = getNodes(lookup, nodes);
    if (results.size() > 1)
        throw "too many results";
    if (results.size())
        return results[0];
    return nullptr;
}
std::vector<NodeTree<Symbol>*> ASTTransformation::getNodes(std::string lookup, NodeTree<Symbol>* parent) {
    return getNodes(lookup, parent->getChildren());
}
std::vector<NodeTree<Symbol>*> ASTTransformation::getNodes(std::string lookup, std::vector<NodeTree<Symbol>*> nodes) {
    std::vector<NodeTree<Symbol>*> results;
    for (auto i : nodes)
        if (i->getDataRef()->getName() == lookup)
            results.push_back(i);
    return results;
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
			std::string name = concatSymbolTree(i->getChildren()[0]);
			NodeTree<ASTData>* firstDec = addToScope("~enclosing_scope", translationUnit, new NodeTree<ASTData>("type_def", ASTData(type_def, Symbol(name, true, name))));
            addToScope(name, firstDec, translationUnit);
			translationUnit->addChild(firstDec);
			//If this is a template, go ahead and set it up. Pass 2 needs templates set up so it can (partially) instantiate them.
				//So we give this typedef its name without any template types and make its type template_type, and point to this from node.
				//Then, when this template is instantiated, it will run transform on from with the types filled in.
			auto typedefChildren = i->getChildren();
            if (typedefChildren.size()>1 && typedefChildren[1]->getData().getName() == "template_dec") {
				if (typedefChildren.size() > 2 && typedefChildren[2]->getData().getName() == "traits")
                    firstDec->getDataRef()->valueType = new Type(template_type, i, parseTraits(i->getChildren()[2]));
                else
                    firstDec->getDataRef()->valueType = new Type(template_type, i);
            } else if (typedefChildren.size() > 1 && typedefChildren[1]->getData().getName() == "traits") {
                firstDec->getDataRef()->valueType = new Type(firstDec, parseTraits(i->getChildren()[1]));
            } else if (typedefChildren.size() == 1 || typedefChildren[1]->getData().getName() != "type") { //We don't make the type for alises, because the second pass will assign it the type it points to
                firstDec->getDataRef()->valueType = new Type(firstDec);
            }

		}  else if (i->getDataRef()->getName() == "if_comp") {
            std::cout << "IF COMP" << std::endl;
            NodeTree<ASTData>* newNode = addToScope("~enclosing_scope", translationUnit, new NodeTree<ASTData>(i->getDataRef()->getName(), ASTData(if_comp)));
            newNode->addChild(addToScope("~enclosing_scope", newNode, new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(concatSymbolTree(i->getChildren()[0]),true)))));
            std::set<int> skipChildren;
            skipChildren.insert(0); //Don't do the identifier. The identifier lookup will fail. That's why we do it here.
            newNode->addChildren(transformChildren(i->getChildren(), skipChildren, translationUnit, std::vector<Type>(), false, std::map<std::string, Type*>()));
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
			if (i->getChildren().size()>1 && i->getChildren()[1]->getData().getName() == "template_dec") // It's a template
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
    return transform(from, scope, std::vector<Type>(), false, templateTypeReplacements);
}

//This function may need to partially instantiate a class template
NodeTree<ASTData>* ASTTransformation::secondPassFunction(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::map<std::string, Type*> templateTypeReplacements) {
	//If this is a function template
	std::vector<NodeTree<Symbol>*> children = from->getChildren();
	NodeTree<ASTData>* functionDef = NULL;
	std::string functionName;
	if (children[1]->getData().getName() == "template_dec") {
		functionName = concatSymbolTree(children[0]);
		functionDef = new NodeTree<ASTData>("function", ASTData(function, Symbol(functionName, true), new Type(template_type, from)));
        addToScope("~enclosing_scope", scope, functionDef);
        addToScope(functionName, functionDef, scope);
		std::map<std::string, Type*> yetToBeInstantiatedTemplateTypes;  //So that template types (like T) that have not been placed yet are found and given
                                                                        //a special Type() - baseType = template_type_type
		for (auto i : slice(children[1]->getChildren(), 1, -1, 2)) {//skip commas
            if (i->getChildren().size() == 1)
                yetToBeInstantiatedTemplateTypes[concatSymbolTree(i)] = new Type(template_type_type); //This may have to be combined with templateTypeReplacements if we do templated member functions inside of templated classes
            else //has traits
                yetToBeInstantiatedTemplateTypes[concatSymbolTree(i->getChildren()[0])] = new Type(template_type_type, parseTraits(i->getChildren()[1])); //This may have to be combined with templateTypeReplacements if we do templated member functions inside of templated classes
        }
		std::cout << "Finished Non-Instantiated Template function " << functionName << std::endl;
		return functionDef;
	}
	functionName = concatSymbolTree(children[0]);
    auto returnTypeNode = getNode("type", getNode("typed_return", children)); // if null, the typed_return had no children and we're supposed to automatically do a void type
    auto returnType = returnTypeNode ? typeFromTypeNode(returnTypeNode, scope, templateTypeReplacements): new Type(void_type);
    functionDef = new NodeTree<ASTData>("function", ASTData(function, Symbol(functionName, true), returnType));
    addToScope("~enclosing_scope", scope, functionDef);
    addToScope(functionName, functionDef, scope);
	//We only do the parameter nodes. We don't do the body yet, as this is the secondPass
    //auto transChildren = transformChildren(slice(children,1,-3, 2), std::set<int>(), functionDef, std::vector<Type>(), false, templateTypeReplacements);
    auto transChildren = transformChildren(getNodes("typed_parameter", children), std::set<int>(), functionDef, std::vector<Type>(), false, templateTypeReplacements);

	functionDef->addChildren(transChildren);
    // Swap the function type over to be the correct type (a function with parameter and return types, etc)
    functionDef->getDataRef()->valueType = new Type(mapNodesToTypePointers(transChildren), functionDef->getDataRef()->valueType);
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
			if (i->getChildren().size() > 1 && i->getChildren()[1]->getData().getName() == "template_dec") // It's a template
				continue;	//We've already set up the class templates
			std::vector<NodeTree<Symbol>*> typedefChildren = i->getChildren();
			std::string name = concatSymbolTree(typedefChildren[0]);
			NodeTree<ASTData>* typeDef = ast->getDataRef()->scope[name][0]; //No overloaded types

			//It's an alias. Note that typedefChildren.size() can equal one when it's a regular class with an empty body, i.e. {}
            if (typedefChildren.size() > 1 && typedefChildren[1]->getData().getName() == "type")
				continue;	//We're done with aliases too

			//Do the inside of classes here
			for (NodeTree<Symbol>* j : typedefChildren) {
                // skip templated member functions
				if (j->getDataRef()->getName() == "function" && j->getChildren()[1]->getDataRef()->getName() != "template_dec") {
					thirdPassFunction(j, searchScopeForFunctionDef(typeDef, j, std::map<std::string, Type*>()), std::map<std::string, Type*>()); 	//do member method
				}
			}
		} else if (i->getDataRef()->getName() == "function") {
			//Do prototypes of functions
			if (i->getChildren()[1]->getData().getName() == "template_dec")
				continue; //We've already set up function templates
			thirdPassFunction(i, searchScopeForFunctionDef(ast, i, std::map<std::string, Type*>()), std::map<std::string, Type*>());
		}
	}

    // We do these here, in a loop, so that we can do mututally recursive definitions
    // even inside of class templates. As its methods may cause partial instantiation of
    // other class templates, we need to do this until the size no longer changes.
    std::vector<NodeTree<ASTData>*> classTemplates;
    std::set<Type*> finishedClassTemplateTypes;
    int lastSize = 0;
    while (lastSize != ast->getDataRef()->scope.size()) {
        lastSize = ast->getDataRef()->scope.size();
        classTemplates.clear();
        for (auto i : ast->getDataRef()->scope) {
            // we actually keep track of the template type replacements now, and need them after they're instantiated, so we use a set to check if we've done it now
            if (i.second[0]->getDataRef()->type == type_def && i.second[0]->getDataRef()->valueType->templateTypeReplacement.size()
                    && finishedClassTemplateTypes.find(i.second[0]->getDataRef()->valueType) == finishedClassTemplateTypes.end()) {
                classTemplates.push_back(i.second[0]);
                std::cout << "Saving " << i.second[0]->getDataRef()->toString() << " to instantiate." << std::endl;
            }
        }
        for (auto i : classTemplates) {
            Type* classTemplateType = i->getDataRef()->valueType;
            std::cout << "Instantiating template " << i->getDataRef()->toString() << std::endl;
            for (NodeTree<Symbol>* j : classTemplateType->templateDefinition->getChildren())
                if (j->getDataRef()->getName() == "function" && j->getChildren()[1]->getDataRef()->getName() != "template_dec")
                    thirdPassFunction(j, searchScopeForFunctionDef(i, j, classTemplateType->templateTypeReplacement), classTemplateType->templateTypeReplacement); 	//do member method
            finishedClassTemplateTypes.insert(classTemplateType);
        }
    }
}

//This function finds the right AST definition in a scope given its parseTree
NodeTree<ASTData>* ASTTransformation::searchScopeForFunctionDef(NodeTree<ASTData>* scope, NodeTree<Symbol>* parseTree, std::map<std::string, Type*> templateTypeReplacements) {
	std::string functionName = concatSymbolTree(parseTree->getChildren()[0]);
	std::vector<Type> types;
	std::vector<NodeTree<Symbol>*> children = parseTree->getChildren();
	//Skipping the initial return type and identifier as well as the final code block
	std::cout << "\n Searching scope for function def, function is: " << concatSymbolTree(children[0]) << ", children size is " << children.size() << std::endl;
	for (auto param: getNodes("typed_parameter", children)) { //Skip over commas
		std::cout << "Making type for lookup ||" << concatSymbolTree(param) << "||" << std::endl;
		Type type = *typeFromTypeNode(param->getChildren().back(), scope, templateTypeReplacements);
		std::cout << "Type made: " << type.toString() << std::endl;
		types.push_back(type);
	}
	std::cout << "About to search scope about " << concatSymbolTree(children[0]) << std::endl;
	NodeTree<ASTData>* result = functionLookup(scope, functionName, types);
	std::cout << "Done searching scope about " << concatSymbolTree(children[0]) << std::endl;
	return result;
}

//This function does the function bodies given its start (the prototype)
//It is used in the third pass to finish things up
//Note that it may instantiate class OR function templates, which need to be fully instantiated
void ASTTransformation::thirdPassFunction(NodeTree<Symbol>* from, NodeTree<ASTData>* functionDef, std::map<std::string, Type*> templateTypeReplacements) {
	NodeTree<Symbol>* codeBlock = from->getChildren().back();
	functionDef->addChild(transform(codeBlock, functionDef, std::vector<Type>(), false, templateTypeReplacements));
}

NodeTree<ASTData>* ASTTransformation::transform(NodeTree<Symbol>* from) {
	//Set up top scope
	return transform(from, NULL, std::vector<Type>(), false, std::map<std::string, Type*>());
}

NodeTree<ASTData>* ASTTransformation::transform(NodeTree<Symbol>* from, NodeTree<ASTData>* scope, std::vector<Type> types, bool limitToFunction, std::map<std::string, Type*> templateTypeReplacements) {
	Symbol current = from->getData();
	std::string name = current.getName();
	NodeTree<ASTData>* newNode = NULL;
	std::vector<NodeTree<Symbol>*> children = from->getChildren();
	std::set<int> skipChildren;

    if (name == "identifier" || name == "scoped_identifier") {
		//Make sure we get the entire name
		std::string lookupName = concatSymbolTree(from);
		std::cout << "Looking up: " << lookupName << std::endl;
		if (limitToFunction) {
            newNode = functionLookup(scope, lookupName, types);
		    if (newNode == NULL) {
			    std::cout << "scope lookup failed! Could not find " << lookupName << " in identifier (functionLookup)" << std::endl;
			    std::cout << "(maybe this is supposted to happen because the function is a template and we're infrencing), or this is a operator() call" << std::endl;
                // Ok, now we try the case where the lookupName is an object, and we'll try to look for operator()
                // in its scope
                for (auto possibleObj : scopeLookup(scope, lookupName)) {
                    NodeTree<ASTData> *typeDefinition = possibleObj->getDataRef()->valueType->typeDefinition;
                    if (typeDefinition) {
                        // ugly for now, it's just operator because the ( and ) have been removed by a removal
                        // pass
                        NodeTree<ASTData>* perenOp = functionLookup(typeDefinition, "operator", types);
                        if (perenOp) {
                            NodeTree<ASTData>* dotFunctionCall = new NodeTree<ASTData>(".", ASTData(function_call, Symbol(".", true), perenOp->getDataRef()->valueType));
                            dotFunctionCall->addChild(languageLevelOperators["."][0]); //function definition
                            dotFunctionCall->addChild(possibleObj); // The object whose method we're calling
                            dotFunctionCall->addChild(perenOp); //The method we're calling
                            return dotFunctionCall;
                        }
                    }
                }
                return nullptr;
		    }
        } else {
            auto possibleMatches = scopeLookup(scope, lookupName);
            if (!possibleMatches.size()) {
			    std::cerr << "scope lookup error! Could not find " << lookupName << " in identifier (scopeLookup)" << std::endl;
			    throw "LOOKUP ERROR: " + lookupName;
            }
            // can't cull out functiokns b/c we might want them as values
            newNode = possibleMatches[0];
            //newNode = nullptr;
            //for (auto i : possibleMatches) {
                //if (i->getDataRef()->valueType->baseType != function_type) {
                    //newNode = i;
                    //break;
                //}
            //}
            //if (!newNode) {
				//std::cerr << "scope lookup error! only found functions for " << lookupName << " in identifier (scopeLookup)" << std::endl;
				//throw "LOOKUP ERROR: " + lookupName;
            //}
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
			if (children[1]->getData().getName() == "template_dec") {
				typeAlias = concatSymbolTree(children[0]);
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
        addToScope("~enclosing_scope", scope, newNode);


		//Templates are done here. No need to go farther
		if (children[1]->getData().getName() == "template_dec")
			return newNode;
		scope = newNode;
	} else if (name == "function" || name == "lambda") {
		std::string functionName;
		//If this is a function template
		if (children[1]->getData().getName() == "template_dec") {
			functionName = concatSymbolTree(children[0]);
			newNode = new NodeTree<ASTData>(name, ASTData(function, Symbol(functionName, true), new Type(template_type, from)));
            addToScope(functionName, newNode, scope);
            addToScope("~enclosing_scope", scope, newNode);
			std::map<std::string, Type*> yetToBeInstantiatedTemplateTypes; //So that template types (like T) that have not been placed yet are found and given
																			//a special Type() - baseType = template_type_type
		    for (auto i : slice(children[1]->getChildren(), 1, -1, 2)) //skip commas
                yetToBeInstantiatedTemplateTypes[concatSymbolTree(i)] = new Type(template_type_type); //This may have to be combined with templateTypeReplacements if we do templated member functions inside of templated classes

            auto transChildren = transformChildren(slice(children,3,-2), std::set<int>(), newNode, types, limitToFunction, yetToBeInstantiatedTemplateTypes);
			std::cout << "Template function " << functionName << " has these parameters: ";
			for (auto i : transChildren)
				std::cout << "||" << i->getDataRef()->toString() << "|| ";
			std::cout << "??||" << std::endl;
			newNode->addChildren(transChildren);

			std::cout << "Finished Non-Instantiated Template function " << functionName << std::endl;
			return newNode;
		}
        if (name == "lambda")
            functionName = "lambda" + intToString(lambdaID++);
        else
            functionName = concatSymbolTree(children[0]);
        auto returnTypeNode = getNode("type", getNode("typed_return", children)); // if null, the typed_return had no children and we're supposed to automatically do a void type
        auto returnType = returnTypeNode ? typeFromTypeNode(returnTypeNode, scope, templateTypeReplacements): new Type(void_type);
        newNode = new NodeTree<ASTData>(name, ASTData(function, Symbol(functionName, true), returnType));
        addToScope(functionName, newNode, scope);
        addToScope("~enclosing_scope", scope, newNode);
		scope = newNode;
        // If lambda, add to top scope so it gets emitted
        if (name == "lambda")
            addToScope(functionName, newNode, topScope);

        auto parameters = transformChildren(getNodes("typed_parameter", children), skipChildren, scope, types, limitToFunction, templateTypeReplacements);
        newNode->addChildren(parameters);
        // update type with actual type
        newNode->getDataRef()->valueType = new Type(mapNodesToTypePointers(parameters), newNode->getDataRef()->valueType);
        auto statement = transform(getNode("statement", children), scope, types, limitToFunction, templateTypeReplacements);
        if (name == "lambda")
            newNode->getDataRef()->closedVariables = findVariablesToClose(newNode, statement);
        for (auto i : newNode->getDataRef()->closedVariables)
            std::cout << "OK, CLOSED: " << i->getDataRef()->toString() << std::endl;
        newNode->addChild(statement);
		std::cout << "finished function" << functionName << std::endl;
        return newNode;

	} else if (name == "code_block") {
		newNode = new NodeTree<ASTData>(name, ASTData(code_block));
        addToScope("~enclosing_scope", scope, newNode);
		scope = newNode;
	} else if (name == "typed_parameter") {
		//newNode = transform(children[1]); //Transform to get the identifier
		std::string parameterName = concatSymbolTree(children[0]);
		std::cout << "Doing typed parameter " << parameterName << std::endl;
		//std::string typeString = concatSymbolTree(children[0]);//Get the type (left child) and set our new identifer to be that type
		newNode = new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(parameterName, true), typeFromTypeNode(children[2], scope, templateTypeReplacements)));
        addToScope(parameterName, newNode, scope);
        addToScope("~enclosing_scope", scope, newNode);
		std::cout << "Done doing typed_parameter " << parameterName << std::endl;
		return newNode;
	} else if (name == "boolean_expression" || name == "and_boolean_expression" || name == "bool_exp") {
		//If this is an actual part of an expression, not just a premoted term
		if (children.size() > 1) {
			//We do children first so we can do appropriate scope searching with types (yay operator overloading!)
			skipChildren.insert(1);
			std::vector<NodeTree<ASTData>*> transformedChildren = transformChildren(children, skipChildren, scope, types, limitToFunction, templateTypeReplacements);
			std::string functionCallString = concatSymbolTree(children[1]);
			NodeTree<ASTData>* function = doFunction(scope, functionCallString, transformedChildren, templateTypeReplacements);
			if (function == NULL) {
				std::cerr << "scope lookup error! Could not find " << functionCallString << " in boolean stuff " << std::endl;
				throw "LOOKUP ERROR: " + functionCallString;
			}
			newNode = function;
		} else {
            // XXX What the heck is this
			if (children.size() == 0)
				return new NodeTree<ASTData>();
			return transform(children[0], scope, types, limitToFunction, templateTypeReplacements); //Just a promoted term, so do child
		}
	//Here's the order of ops stuff
	} else if (name == "expression" || name == "shiftand" || name == "term" || name == "unarad" || name == "access_operation") {
		//If this is an actual part of an expression, not just a premoted child
		if (children.size() > 2) {
			NodeTree<ASTData>* lhs = transform(children[0], scope, std::vector<Type>(),limitToFunction, templateTypeReplacements); //LHS does not inherit types
			NodeTree<ASTData>* rhs;
			if (name == "access_operation") {
				std::cout << "lhs is: " << lhs->getDataRef()->toString() << std::endl;
				rhs = transform(children[2], lhs->getDataRef()->valueType->typeDefinition, types, limitToFunction, templateTypeReplacements); //If an access operation, then the right side will be in the lhs's type's scope
                // this might be a template member function, so do like below would do, but make it our rhs
                if (rhs == nullptr)
                    rhs = findOrInstantiateFunctionTemplate(slice(children,2,-1), lhs->getDataRef()->valueType->typeDefinition, types, templateTypeReplacements);
			} else
				rhs = transform(children[2], scope, types, limitToFunction, templateTypeReplacements);

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
		}
        if (children.size() == 1) {
			newNode = transform(children[0], scope, types, limitToFunction, templateTypeReplacements); //Just a promoted child, so do it instead
            if (newNode)
                return newNode;
        }
        // So if children.size() != 1, or that returned null because the function lookup failed,
        // we try to do a template instatiation. If it had 2 children, it's an instantion, if it has 1
        // maybe it's a template instantiation we're supposed to infer the types for. Either way, we let
        // findorinstantiatefunctiontemplate take care of it.
        return findOrInstantiateFunctionTemplate(children, scope, types, templateTypeReplacements);
	} else if (name == "factor") { //Do factor here, as it has all the weird unary operators
		//If this is an actual part of an expression, not just a premoted child
		//NO SUPPORT FOR CASTING YET
		std::string funcName;
		if (children.size() == 2) {
			funcName = concatSymbolTree(children[0]);
			NodeTree<ASTData>* param;
            // I think this is where we look at pre vs post operators
			if (funcName == "*" || funcName == "&" || funcName == "++" || funcName == "--" || funcName == "+" || funcName == "-" || funcName == "!" || funcName == "~")
				param = transform(children[1], scope, types, limitToFunction, templateTypeReplacements);
			else
				funcName = concatSymbolTree(children[1]), param = transform(children[0], scope, types, limitToFunction, templateTypeReplacements);

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
			return transform(children[0], scope, types, limitToFunction, templateTypeReplacements); //Just a promoted child, so do it instead
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
	} else if (name == "break_statement") {
		newNode = new NodeTree<ASTData>(name, ASTData(break_statement));
	} else if (name == "continue_statement") {
		newNode = new NodeTree<ASTData>(name, ASTData(continue_statement));
	} else if (name == "defer_statement") {
		newNode = new NodeTree<ASTData>(name, ASTData(defer_statement));
	} else if (name == "assignment_statement") {
		std::string assignFuncName = concatSymbolTree(children[1]);
        NodeTree<ASTData>* lhs = transform(children[0], scope, types, limitToFunction, templateTypeReplacements);
        NodeTree<ASTData>* rhs = transform(children[2], scope, types, limitToFunction, templateTypeReplacements);
        std::vector<NodeTree<ASTData>*> transformedChildren; transformedChildren.push_back(lhs); transformedChildren.push_back(rhs);

        // see if this is an overloaded assignment
        NodeTree<ASTData>* function = doFunction(scope, assignFuncName, transformedChildren, templateTypeReplacements);
        if (function)
            return function;

        newNode = new NodeTree<ASTData>(name, ASTData(assignment_statement));
		if (assignFuncName == "=") {
			newNode->addChildren(transformedChildren);
		} else {
			//For assignments like += or *=, expand the syntatic sugar.
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
		std::string newIdentifierStr = concatSymbolTree(children[0]);
		NodeTree<Symbol>* typeSyntaxNode = getNode("type", children);
		Type* identifierType = typeSyntaxNode ? typeFromTypeNode(typeSyntaxNode, scope, templateTypeReplacements) : nullptr;

        if (identifierType)
            std::cout << "Declaring an identifier " << newIdentifierStr << " to be of type " << identifierType->toString() << std::endl;
        else
            std::cout << "Declaring an identifier " << newIdentifierStr << " with type to be type-inferenced " << std::endl;

        if (children.size() > 1 && concatSymbolTree(children[1]) == ".") {
            NodeTree<ASTData>* newIdentifier = new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(newIdentifierStr, true), identifierType));
            addToScope(newIdentifierStr, newIdentifier, scope);
            addToScope("~enclosing_scope", scope, newNode);
            addToScope("~enclosing_scope", newNode, newIdentifier);
            newNode->addChild(newIdentifier);
            //A bit of a special case for declarations - if there's anything after just the normal 1 node declaration, it's either
            //an expression that is assigned to the declaration (int a = 4;) or a member call (Object a.constructAThing())
            //This code is a simplified version of the code in function_call with respect to access_operation.
            //Note that in this case, what is lhs there is our newIdentifier here (the declaration of the left side of the access operation)
            auto sliced = slice(children, 3, -3);
            std::vector<NodeTree<ASTData>*> initPositionFuncParams = transformChildren(sliced, std::set<int>(), scope, types, limitToFunction, templateTypeReplacements);
            NodeTree<ASTData>* rhs = transform(children[2], identifierType->typeDefinition, mapNodesToTypes(initPositionFuncParams), true, templateTypeReplacements); //If an access operation, then the right side will be in the lhs's type's scope
			std::vector<NodeTree<ASTData>*> transformedChildren; transformedChildren.push_back(newIdentifier); transformedChildren.push_back(rhs);
			NodeTree<ASTData>* accessFuncCall = doFunction(scope, ".", transformedChildren, templateTypeReplacements);
		    accessFuncCall->getDataRef()->valueType = rhs->getDataRef()->valueType;
            //Now we borrow a bit of code from function_call below to actually use our new accessFuncCall to setup a "initPosition" function call
            //that will follow the identifier in this declaration node
            std::string initPosFuncName = newIdentifierStr + "." + concatSymbolTree(children[2]);
            NodeTree<ASTData>* initPositionFuncCall = new NodeTree<ASTData>(initPosFuncName, ASTData(function_call, Symbol(initPosFuncName, true)));
            initPositionFuncCall->addChild(accessFuncCall);
            initPositionFuncCall->getDataRef()->valueType = accessFuncCall->getDataRef()->valueType;
            initPositionFuncCall->addChildren(initPositionFuncParams);
            newNode->addChild(initPositionFuncCall);
            return newNode;
        }

        auto boolExp = getNode("boolean_expression", children);
        NodeTree<ASTData>* toAssign = boolExp ? transform(boolExp, scope, types, limitToFunction, templateTypeReplacements) : nullptr;
        // for type inferencing
        if (!identifierType) {
            if (toAssign)
                identifierType = toAssign->getDataRef()->valueType;
            else
                throw "have to inference but no expression";
        }

		NodeTree<ASTData>* newIdentifier = new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(newIdentifierStr, true), identifierType));
        addToScope(newIdentifierStr, newIdentifier, scope);
        addToScope("~enclosing_scope", scope, newNode);
        addToScope("~enclosing_scope", newNode, newIdentifier);

		newNode->addChild(newIdentifier);
        if (toAssign)
            newNode->addChild(toAssign);
	    return newNode;
    } else if (name == "if_comp") {
		newNode = new NodeTree<ASTData>(name, ASTData(if_comp));
		newNode->addChild(addToScope("~enclosing_scope", scope, new NodeTree<ASTData>("identifier", ASTData(identifier, Symbol(concatSymbolTree(children[0]),true)))));
        addToScope("~enclosing_scope", scope, newNode);
		skipChildren.insert(0); //Don't do the identifier. The identifier lookup will fail. That's why we do it here.
	} else if (name == "simple_passthrough") {
		newNode = new NodeTree<ASTData>(name, ASTData(simple_passthrough));
        addToScope("~enclosing_scope", scope, newNode);
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
		std::vector<NodeTree<ASTData>*> transformedChildren = transformChildren(children, skipChildren, scope, types, limitToFunction, templateTypeReplacements);
		std::cout << "scope lookup from function_call: " << functionCallName << std::endl;
		for (auto i : children)
			std::cout << i << " : " << i->getName()  << " : " << i->getDataRef()->getName() << std::endl;

		NodeTree<ASTData>* function = transform(children[0], scope, mapNodesToTypes(transformedChildren), true, templateTypeReplacements);
		std::cout << "The thing: " << function << " : " << function->getName() << std::endl;
		for (auto i : function->getChildren())
			std::cout << i->getName() << " ";
		std::cout << std::endl;
		newNode->addChild(function);
        // note that we now get the return type from the function call's type
		newNode->getDataRef()->valueType = function->getDataRef()->valueType->returnType;
		newNode->addChildren(transformedChildren);
		return newNode;
	} else if (name == "parameter") {
		return transform(children[0], scope, types, limitToFunction, templateTypeReplacements); //Don't need a parameter node, just the value
	} else if (name == "type") {
		std::string theConcat = concatSymbolTree(from); //We have no symbol, so this will concat our children
		newNode = new NodeTree<ASTData>(name, ASTData(value, Symbol(theConcat, true), typeFromTypeNode(from, scope, templateTypeReplacements)));
        addToScope("~enclosing_scope", scope, newNode);
	} else if (name == "number") {
		return transform(children[0], scope, types, limitToFunction, templateTypeReplacements);
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
        std::cerr << concatSymbolTree(from) << std::endl;
        throw "Ambigious parse!";
    } else {
        // Should get rid of this eventually. Right now it handles cases like sign, alpha, a comma, etc
        std::cout << "Unhandled syntax node: " << name << std::endl;
		return new NodeTree<ASTData>();
	}

	//Do all children but the ones we skip
    newNode->addChildren(transformChildren(children, skipChildren, scope, types, limitToFunction, templateTypeReplacements));
	return newNode;
}

//We use this functionality a lot at different places
std::vector<NodeTree<ASTData>*> ASTTransformation::transformChildren(std::vector<NodeTree<Symbol>*> children, std::set<int> skipChildren, NodeTree<ASTData>* scope, std::vector<Type> types, bool limitToFunction, std::map<std::string, Type*> templateTypeReplacements) {
	std::vector<NodeTree<ASTData>*> transformedChildren;
	// In general, iterate through children and do them. Might not do this for all children.
	for (int i = 0; i < children.size(); i++) {
		if (skipChildren.find(i) == skipChildren.end()) {
			NodeTree<ASTData>* transChild = transform(children[i], scope, types, limitToFunction, templateTypeReplacements);
			if (transChild->getDataRef()->type) //Only add the children that have a real ASTData::ASTType, that is, legit ASTData.
				transformedChildren.push_back(transChild);
			else
				delete transChild;
		}
	}
	return transformedChildren;
}

//Extract types from already transformed nodes
std::vector<Type*> ASTTransformation::mapNodesToTypePointers(std::vector<NodeTree<ASTData>*> nodes) {
	std::vector<Type*> types;
	for (auto i : nodes) {
		std::cout << i->getDataRef()->toString() << std::endl;
		types.push_back((i->getDataRef()->valueType));
	}
	return types;
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
			NodeTree<ASTData>* dotFunctionCall = new NodeTree<ASTData>(".", ASTData(function_call, Symbol(".", true), operatorMethod->getDataRef()->valueType));
			dotFunctionCall->addChild(languageLevelOperators["."][0]); //function definition
			dotFunctionCall->addChild(nodes[0]); // The object whose method we're calling
			dotFunctionCall->addChild(operatorMethod); //The method we're calling
			newNode->addChild(dotFunctionCall); // First child of function call is a link to the function definition
			newNode->addChildren(slice(nodes, 1, -1)); //The rest of the parameters to the operator


			//Set the value of this function call
			newNode->getDataRef()->valueType = operatorMethod->getDataRef()->valueType->returnType;
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
        std::cout << "Some other ||" << lookup << "||" << std::endl;
        if (function->getDataRef()->valueType)
            newNode->getDataRef()->valueType = function->getDataRef()->valueType->returnType;
	}

    // Set the value of this function call if it has not already been set
    // It's important that it's the last parameter, the rhs if it has one
    // because of the . operator, etc
    if (newNode->getDataRef()->valueType == NULL) {
        std::cout << "The value type from doFunction was null! (for " << lookup << ")" << std::endl;
        Type* newType = nullptr;
        if (lookup == "->")
            newType = oldTypes.back().clone();
        else if (oldTypes.front().getIndirection())
            newType = oldTypes.front().clone();
        else if (oldTypes.back().getIndirection())
            newType = oldTypes.back().clone();
        else
            newType = (oldTypes.front().baseType > oldTypes.back().baseType) ? oldTypes.front().clone() : oldTypes.back().clone();
        //if (!newType)
            //newType = oldTypes.back().clone();

        newNode->getDataRef()->valueType = newType;
        std::cout << "function call to " << lookup << " - " << newNode->getName() << " is now " << newNode->getDataRef()->valueType  << std::endl;
    }
	return newNode;
}
// checks to see if scope is in node's parent scope chain
bool ASTTransformation::inScopeChain(NodeTree<ASTData>* node, NodeTree<ASTData>* scope) {
    auto nodeScope = node->getDataRef()->scope;
    auto enclosingItr = nodeScope.find("~enclosing_scope");
    if (enclosingItr == nodeScope.end())
        return false;
    if (enclosingItr->second[0] == scope)
        return true;
    return inScopeChain(enclosingItr->second[0], scope);
}
// We return a set of all identifers used in the children of stat that are not declared somewhere below stat
// used to calculate the closedvariables for closures
std::set<NodeTree<ASTData>*> ASTTransformation::findVariablesToClose(NodeTree<ASTData>* func, NodeTree<ASTData>* stat) {
    std::set<NodeTree<ASTData>*> closed;
    for (auto child: stat->getChildren()) {
//enum ASTType {undef, translation_unit, interpreter_directive, import, identifier, type_def,
	//function, code_block, typed_parameter, expression, boolean_expression, statement,
	//if_statement, while_loop, for_loop, return_statement, break_statement, continue_statement, defer_statement,
    //assignment_statement, declaration_statement, if_comp, simple_passthrough, passthrough_params,
    //in_passthrough_params, out_passthrough_params, opt_string, param_assign, function_call, value};
        if (child->getDataRef()->type == function || child->getDataRef()->type == translation_unit
            || child->getDataRef()->type == type_def || child->getDataRef()->type == value
                )
            continue;
        if (child->getDataRef()->type == function_call && (child->getDataRef()->symbol.getName() == "." || child->getDataRef()->symbol.getName() == "->")) {
            // only search on the left side of access operators like . and ->
            auto recClosed = findVariablesToClose(func, child->getChildren().front());
            closed.insert(recClosed.begin(), recClosed.end());
            continue;
        }
        if (child->getDataRef()->type == identifier && !inScopeChain(child, func))
            closed.insert(child);
        auto recClosed = findVariablesToClose(func, child);
        closed.insert(recClosed.begin(), recClosed.end());
    }
    return closed;
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
            Type* functionType = i->getDataRef()->valueType;

            int numTypes = functionType->parameterTypes.size();
			if (types.size() != numTypes) {
				std::cout << "Type sizes do not match between two " << lookup << "(" << types.size() << "," << numTypes << "), types are: ";
				for (auto j : types)
					std::cout << j.toString() << " ";
				std::cout << std::endl;
                std::cout << "Versus" << std::endl;
                for (int j = 0; j < numTypes; j++)
                    std::cout << functionType->parameterTypes[j]->toString() << " ";
                std::cout << std::endl;
				continue;
			}
			bool typesMatch = true;
			for (int j = 0; j < types.size(); j++) {
				Type* tmpType = functionType->parameterTypes[j];
				//Don't worry if types don't match if it's a template type
				//if (types[j] != *tmpType && tmpType->baseType != template_type_type) {
                // WE DO WORRY NOW B/C template type infrence is ugly and we need this to fail
                // for regular function lookups so that we know to retry with a template
				if (types[j] != *tmpType) {
					typesMatch = false;
					std::cout << "Types do not match between two " << lookup << " " << types[j].toString();
					std::cout << " vs " << tmpType->toString() << std::endl;
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
        if (i->getDataRef()->type != type_def)
            continue;
	    NodeTree<Symbol>* templateSyntaxTree = i->getDataRef()->valueType->templateDefinition;

        auto nameTraitsPairs = makeTemplateNameTraitPairs(templateSyntaxTree->getChildren()[1]);
        //Check if sizes match between the placeholder and actual template types
        if (nameTraitsPairs.size() != templateInstantiationTypes.size())
            continue;

        bool traitsEqual = true;
        int typeIndex = 0;
        int currentTraitsSatisfied = 0;
        for (auto j : nameTraitsPairs) {
            // error out if not subset, or if we're a pointer but should have traits
            if (!subset(j.second, templateInstantiationTypes[typeIndex]->traits) || (templateInstantiationTypes[typeIndex]->getIndirection() && j.second.size())) {
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

void ASTTransformation::unifyType(NodeTree<Symbol> *syntaxType, Type type, std::map<std::string, Type>* templateTypeMap, std::map<std::string, Type*> typeMap) {
    // Ok, 3 options for syntaxType here.
    //  1) This a basic type. (int, or object, etc)
    //      THIS ONE will fall through and get put in the map, but it
    //      doesn't matter b/c it'll get filterd out in unifyTemplateFunction
    //      I also kina feel like maybe I need to worry about typeMap, which is why
    //      I passed it in... It would contain the typemap from our scope if we are
    //      doing a template member function of a templated object
    //  2) This is a template type type (i.e. T)
    //      match! set up templateTypeMap[T] -> type
    //  3) This some sort of instantiated template
    //      a) instantiated with some other type (i.e. vector<int>)
    //          THIS ONE will fall through and get put in the map, but it
    //          doesn't matter b/c it'll get filterd out in unifyTemplateFunction
    //      b) instantiated with a template type type (i.e. vector<T>)
    //          this will be a bit of a pain too
    //   4) This is a pointer type, go down a pointer level
    //   5) This is a function type, unify on parameter types and return type

    auto children = syntaxType->getChildren();

    if (children.back()->getDataRef()->getName() == "function_type") {
        if (!type.returnType)
            return;
        auto childrenTypes = getNodes("type", children.back()->getChildren());
        // unify params
        for (int i = 0; i < childrenTypes.size()-1; i++)
            unifyType(childrenTypes[i], *type.parameterTypes[i], templateTypeMap, typeMap);
        unifyType(childrenTypes.back(), *type.returnType, templateTypeMap, typeMap);
        return;
    }

    if (children.size() == 1) {
        (*templateTypeMap)[concatSymbolTree(children.front())] = type;
    //      I also kina feel like maybe I need to worry about typeMap, which is why
    //      I passed it in... It would contain the typemap from our scope if we are
    //      doing a template member function of a templated object
    } else {
        // go down one in our pointer
        if (children.back()->getDataRef()->getValue() == "*") {
            // gotta be a better way to do this
            Type* clonedType = type.clone();
            clonedType->decreaseIndirection();
            unifyType(children.front(), *clonedType, templateTypeMap, typeMap);
            delete clonedType;
            return;
        }

        if (type.typeDefinition) {
            // ok, what happens here is that we get the origional type from our type. This is
            // the same as the type we have now but it still has extra data from when it was instantiated
            // like the templateTypeReplacement map, which we'll use.
            // We get the <T, int, ...> etc part from the template we're matching against and unify it with the
            // actual types the type we're unifying with used by passing it's <A, B, ...> through the templateTypeReplacement
            // to get the type it was instantiated with.
            auto origionalType = type.typeDefinition->getDataRef()->valueType;
            auto typeTemplateDefinition = origionalType->templateDefinition;
            if (typeTemplateDefinition && concatSymbolTree(getNode("scoped_identifier", children)) == concatSymbolTree(getNode("identifier", typeTemplateDefinition->getChildren()))) {
                    std::vector<NodeTree<Symbol>*> uninTypeInstTypes = getNodes("type", getNode("template_inst", children));
                    std::vector<NodeTree<Symbol>*> typeInstTypes = getNodes("template_param", getNode("template_dec", typeTemplateDefinition->getChildren()));
                    for (int i = 0; i < uninTypeInstTypes.size(); i++) {
                        std::cout << concatSymbolTree(uninTypeInstTypes[i]) << " : " << origionalType->toString() << " : " <<  concatSymbolTree(typeInstTypes[i]) << std::endl;
                        std::cout << "which is  " <<  origionalType->templateTypeReplacement[concatSymbolTree(typeInstTypes[i])]->toString() << std::endl;
                        //std::cout << "which is  " <<  *origionalType->templateTypeReplacement[concatSymbolTree(typeInstTypes[i])] << std::endl;
                        unifyType(uninTypeInstTypes[i], *origionalType->templateTypeReplacement[concatSymbolTree(typeInstTypes[i])], templateTypeMap, typeMap);
                    }

                    return;
            }
        }
        throw "the inference just isn't good enough";
    }
}

void ASTTransformation::unifyTemplateFunction(NodeTree<ASTData>* templateFunction, std::vector<Type> types, std::vector<Type*>* templateInstantiationTypes, std::map<std::string, Type*> typeMap) {
    NodeTree<Symbol>* templateSyntaxTree = templateFunction->getDataRef()->valueType->templateDefinition;
    std::vector<NodeTree<Symbol>*> templateParameters = getNodes("typed_parameter", templateSyntaxTree);
    if (templateParameters.size() != types.size())
        return;
    std::map<std::string, Type> templateTypeMap;
    for (int i = 0; i < types.size(); i++)
        unifyType(getNode("type", templateParameters[i]), types[i], &templateTypeMap, typeMap);
    for (auto instantiationParam : getNodes("template_param", getNode("template_dec", templateSyntaxTree))) {
        templateInstantiationTypes->push_back(templateTypeMap[concatSymbolTree(getNode("identifier", instantiationParam))].clone()); // gotta be careful of catching the traits in the concat
    }
}

//Lookup function for template functions. It has some extra concerns compared to function lookup, namely traits
NodeTree<ASTData>* ASTTransformation::templateFunctionLookup(NodeTree<ASTData>* scope, std::string lookup, std::vector<Type*>* templateInstantiationTypes, std::vector<Type> types, std::map<std::string, Type*> scopeTypeMap) {
    std::map<NodeTree<ASTData>*, std::vector<Type*>> templateInstantiationTypesPerFunction;
    std::set<NodeTree<ASTData>*> mostFittingTemplates;
    int bestNumTraitsSatisfied = -1;
    auto possibleMatches = scopeLookup(scope, lookup);
    std::cout << "Template Function instantiation has " << possibleMatches.size() << " possible matches." << std::endl;
    int index = 1;
    for (auto i : possibleMatches) {
        if (i->getDataRef()->type != function)
            continue;
        std::cout << "Possibility " << index++ << std::endl;
	    NodeTree<Symbol>* templateSyntaxTree = i->getDataRef()->valueType->templateDefinition;
        if (!templateSyntaxTree) {
            std::cout << "Not a template, skipping" << std::endl;
            continue;
        }
        // We have the type map here because we might want to augment it with the typeMap from
        // the current scope, which would happen if we're trying to instantiate a template member function
        std::map<std::string, Type*> typeMap = scopeTypeMap;
        // If template instantiation was explicit, use those types. Otherwise, unify to find them
        if (templateInstantiationTypes->size()) {
            templateInstantiationTypesPerFunction[i] = *templateInstantiationTypes;
            std::cout << "passed in types" << std::endl;
        }else{
            unifyTemplateFunction(i, types, &templateInstantiationTypesPerFunction[i], typeMap);
            std::cout << "unified types" << std::endl;
        }
        std::cout << "TYPES ARE: ";
        for (Type *a : templateInstantiationTypesPerFunction[i])
            std::cout << a->toString() << " : ";
        std::cout << std::endl;
        auto nameTraitsPairs = makeTemplateNameTraitPairs(templateSyntaxTree->getChildren()[1]);
        //Check if sizes match between the placeholder and actual template types
        if (nameTraitsPairs.size() != templateInstantiationTypesPerFunction[i].size())
            continue;

        bool traitsEqual = true;
        int typeIndex = 0;
        int currentTraitsSatisfied = 0;
        for (auto j : nameTraitsPairs) {
            // error out if not subset, or if we're a pointer but should have traits
            if (!subset(j.second, templateInstantiationTypesPerFunction[i][typeIndex]->traits) || (templateInstantiationTypesPerFunction[i][typeIndex]->getIndirection() && j.second.size())) {
                traitsEqual = false;
                std::cout << "Traits not a subset for " << j.first << " and " << templateInstantiationTypesPerFunction[i][typeIndex]->toString() << ": |";
                std::copy(j.second.begin(), j.second.end(), std::ostream_iterator<std::string>(std::cout, " "));
                std::cout << "| vs |";
                std::copy(templateInstantiationTypesPerFunction[i][typeIndex]->traits.begin(), templateInstantiationTypesPerFunction[i][typeIndex]->traits.end(), std::ostream_iterator<std::string>(std::cout, " "));
                std::cout << "|" << std::endl;
                break;
            } else {
                std::cout << "Traits ARE a subset for " << j.first << " and " << templateInstantiationTypesPerFunction[i][typeIndex]->toString() << ": ";
                std::copy(j.second.begin(), j.second.end(), std::ostream_iterator<std::string>(std::cout, " "));
                std::cout << " vs ";
                std::copy(templateInstantiationTypesPerFunction[i][typeIndex]->traits.begin(), templateInstantiationTypesPerFunction[i][typeIndex]->traits.end(), std::ostream_iterator<std::string>(std::cout, " "));
                std::cout << std::endl;
            }
            //As we go, build up the typeMap for when we transform the parameters for parameter checking
            typeMap[j.first] = templateInstantiationTypesPerFunction[i][typeIndex];
            currentTraitsSatisfied += j.second.size();
            typeIndex++;
        }
        if (!traitsEqual)
            continue;

        //std::vector<NodeTree<Symbol>*> functionParameters = slice(templateSyntaxTree->getChildren(), 2, -4, 2); //skip name, intervening commas, return type, and the code block
        std::vector<NodeTree<Symbol>*> functionParameters = getNodes("typed_parameter", templateSyntaxTree->getChildren());
        std::cout << functionParameters.size() << " " << types.size() << std::endl;
        if (functionParameters.size() != types.size())
            continue;

        bool parameterTypesMatch = true;
        for (int j = 0; j < functionParameters.size(); j++) {
            auto paramType = typeFromTypeNode(functionParameters[j]->getChildren()[2], scope, typeMap);
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
        std::cerr << "No template functions fit for " << lookup << "(";
        for (auto t : types)
            std::cerr << t.toString() + ", ";
        std::cerr << ")!" << std::endl;
        throw "No matching template functions";
    } else if (mostFittingTemplates.size() > 1) {
        std::cerr << "Multiple template functions fit with equal number of traits satisfied for " << lookup << "!" << std::endl;
        throw "Multiple matching template functions";
    }
    // Assign our most fitting instantiation types to what we were passed in
    // if it was empty
    if (templateInstantiationTypes->size() == 0)
        *templateInstantiationTypes = templateInstantiationTypesPerFunction[*mostFittingTemplates.begin()];
    std::cout << *mostFittingTemplates.begin() << std::endl;
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

std::map<std::string, Type*> ASTTransformation::makeTemplateFunctionTypeMap(NodeTree<Symbol>* templateNode, std::vector<Type*> types, std::map<std::string, Type*> scopeTypeMap) {
    auto typePairs = makeTemplateNameTraitPairs(templateNode);
    // we start with the scopeTypeMap because we want to combine
    // them (this is for templated member functions of templated objects)
    std::map<std::string, Type*> typeMap = scopeTypeMap;
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
    return scopeLookup(scope, lookup, includeModules, std::set<NodeTree<ASTData>*>());
}

std::vector<NodeTree<ASTData>*> ASTTransformation::scopeLookup(NodeTree<ASTData>* scope, std::string lookup, bool includeModules, std::set<NodeTree<ASTData>*> visited) {
    std::cout << "Scp]|[e looking up " << lookup << std::endl;
    std::cout << "current: " << scope->getDataRef()->toString() << std::endl;
    for (auto i : scope->getDataRef()->scope)
        std::cout << "\t" << i.first << std::endl;
        //std::cout << i.first << " : " << i.second->toString() << std::endl;
    // Don't visit this node again when looking for the smae lookup. Note that we don't prevent coming back for the scope operator, as that should be able to come back.
    if (visited.find(scope) != visited.end())
        return std::vector<NodeTree<ASTData>*>();
    visited.insert(scope);
	//We first check to see if it's one of the special reserved identifiers (only this, for now) and return early if it is.
	auto LLElementIterator = languageLevelReservedWords.find(lookup);
	if (LLElementIterator != languageLevelReservedWords.end()) {
		std::cout << "found it at language level as reserved word." << std::endl;
		NodeTree<ASTData>* identifier = LLElementIterator->second[0];
        if (lookup == "this") {
            identifier = new NodeTree<ASTData>("identifier", identifier->getData());
            // if we're looking for this, traverse up until we find the declaration of this object and assign it's type to this
            NodeTree<ASTData>* trans;
            for (trans = scope; trans->getDataRef()->type != type_def; trans = trans->getDataRef()->scope["~enclosing_scope"][0]);
            identifier->getDataRef()->valueType = trans->getDataRef()->valueType->clone();
            identifier->getDataRef()->valueType->increaseIndirection();
        }
        std::vector<NodeTree<ASTData>*> thingy; thingy.push_back(identifier);
        return thingy;
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
    // To counter this, for every indirection we step down a level
	while (typeIn[typeIn.size() - indirection - 1] == '*') {
        indirection++;
        typeNode = typeNode->getChildren()[0];
    };
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
	else if (typeNode->getChildren().size() && typeNode->getChildren()[0]->getDataRef()->getName() == "function_type") {
		baseType = function_type;
        std::vector<Type*> types;
        for (auto typeSyntaxNode : getNodes("type", typeNode->getChildren()[0]->getChildren()))
            types.push_back(typeFromTypeNode(typeSyntaxNode, scope, templateTypeReplacements));
        return new Type(slice(types, 0, -2),types.back());
    } else {
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
                 * fun example<T>(a:vec<T>):T { return a.at(0); }
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

                std::string fullyInstantiatedName = templateDefinition->getDataRef()->symbol.getName() + "<" + instTypeString + ">";

                NodeTree<Symbol>* templateSyntaxTree = templateDefinition->getDataRef()->valueType->templateDefinition;
                //Create a new map of template type names to actual types.
                std::vector<NodeTree<Symbol>*> templateParamPlaceholderNodes = slice(templateSyntaxTree->getChildren()[1]->getChildren(), 1, -2, 2); //Don't get beginning or end for < or >, skip commas in the middle
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
                for (auto daPair : selfType->templateTypeReplacement) {
                    std::cout << " BREAK HERE " << daPair.first << " : " << daPair.second->toString() << std::endl;
                    if (daPair.second == NULL)
                        std::cout << " BREAK HERE " << std::endl;
                }
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
    std::string fullyInstantiatedName;
    std::string scopelessFullyInstantiatedName;
    std::vector<Type*> templateActualTypes;
	NodeTree<ASTData>* templateDefinition = NULL;

    // If this is a templated member function, we should also add this function to the object
	NodeTree<ASTData>* objectForTemplateMethod = NULL;
    // Ok, our scope might have a typeMap if we are inside a templated object and are looking
    // for a templated member function
    std::map<std::string, Type*> scopeTypeMap;
    if (scope->getDataRef()->valueType && scope->getDataRef()->valueType->typeDefinition
            && scope->getDataRef()->valueType->typeDefinition->getDataRef()->valueType) {
        objectForTemplateMethod = scope->getDataRef()->valueType->typeDefinition;
        scopeTypeMap = objectForTemplateMethod->getDataRef()->valueType->templateTypeReplacement;
    }

    // Are we supposed to infer our instantiation, or not? If we have only one child we're inferring as we don't
    // have the actual instantiation part. If do have the instantiation part, then we'll use that.
    // Note that as a part o finferring the instantiation we already find the template, so we make that
    // condtitional too (templateDefinition)
    std::string instTypeString = "";
    if (children.size() == 1) {
        // templateFunctionLookup adds the actual types to templateActualTypes if it's currently empty
        templateDefinition = templateFunctionLookup(scope, functionName, &templateActualTypes, types, scopeTypeMap);
        for (auto instType : templateActualTypes)
            instTypeString += (instTypeString == "" ? instType->toString() : "," + instType->toString());
    } else {
        auto unsliced = children[1]->getChildren();
        std::vector<NodeTree<Symbol>*> templateParamInstantiationNodes = slice(unsliced, 1 , -2, 2);//skip <, >, and commas
        for (int i = 0; i < templateParamInstantiationNodes.size(); i++) {
            Type* instType = typeFromTypeNode(templateParamInstantiationNodes[i], scope, templateTypeReplacements);
            instTypeString += (instTypeString == "" ? instType->toString() : "," + instType->toString());
            templateActualTypes.push_back(instType);
        }
        std::cout << "Size: " << templateParamInstantiationNodes.size() << std::endl;
    }
    fullyInstantiatedName = functionName + "<" + instTypeString + ">";
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
    // templateFunctionLookup adds the actual types to templateActualTypes if it's currently empty
    // by here, it's not as either we had the instantiation already or we figured out out before
    // and are not actually doing this call
    if (!templateDefinition)
        templateDefinition = templateFunctionLookup(scope, functionName, &templateActualTypes, types, scopeTypeMap);
	if (templateDefinition == NULL) {
		std::cout << functionName << " search turned up null, returing null" << std::endl;
		return NULL;
	}
    scopelessFullyInstantiatedName = templateDefinition->getDataRef()->symbol.getName() + "<" + instTypeString + ">";

	NodeTree<Symbol>* templateSyntaxTree = templateDefinition->getDataRef()->valueType->templateDefinition;
	// Makes a map between the names of the template placeholder parameters and the provided types
    std::map<std::string, Type*> newTemplateTypeReplacement = makeTemplateFunctionTypeMap(templateSyntaxTree->getChildren()[1], templateActualTypes, scopeTypeMap);

    std::vector<NodeTree<Symbol>*> templateChildren = templateSyntaxTree->getChildren();
	for (int i = 0; i < templateChildren.size(); i++)
		std::cout << ", " << i << " : " << templateChildren[i]->getDataRef()->getName();
	std::cout << std::endl;

    // return type should be looked up in template's scope
    auto returnTypeNode = getNode("type", getNode("typed_return", templateChildren)); // if null, the typed_return had no children and we're supposed to automatically do a void type
    auto returnType = returnTypeNode ? typeFromTypeNode(returnTypeNode, templateDefinition, newTemplateTypeReplacement): new Type(void_type);
	instantiatedFunction = new NodeTree<ASTData>("function", ASTData(function, Symbol(scopelessFullyInstantiatedName, true), returnType));
    addToScope("~enclosing_scope", templateDefinition->getDataRef()->scope["~enclosing_scope"][0], instantiatedFunction);
    addToScope(scopelessFullyInstantiatedName, instantiatedFunction, templateDefinition->getDataRef()->scope["~enclosing_scope"][0]);
    templateDefinition->getDataRef()->scope["~enclosing_scope"][0]->addChild(instantiatedFunction); // Add this object the the highest scope's

	std::cout << "About to do children of " << functionName << " to " << fullyInstantiatedName << std::endl;

    std::set<int> skipChildren;
    auto parameters = transformChildren(getNodes("typed_parameter", templateSyntaxTree->getChildren()), skipChildren, instantiatedFunction, std::vector<Type>(), false, newTemplateTypeReplacement);
    instantiatedFunction->addChildren(parameters);
    // update type with actual type
    instantiatedFunction->getDataRef()->valueType = new Type(mapNodesToTypePointers(parameters), instantiatedFunction->getDataRef()->valueType);
    instantiatedFunction->addChild(transform(getNode("statement", templateSyntaxTree->getChildren()), instantiatedFunction, std::vector<Type>(), false, newTemplateTypeReplacement));

	std::cout << "Fully Instantiated function " << functionName << " to " << fullyInstantiatedName << std::endl;
	return instantiatedFunction;
}

NodeTree<ASTData>* ASTTransformation::addToScope(std::string name, NodeTree<ASTData>* toAdd, NodeTree<ASTData>* addTo) {
    addTo->getDataRef()->scope[name].push_back(toAdd);
    return addTo;
}


