#include "CGenerator.h"

CGenerator::CGenerator() : generatorString("__C__") {
	tabLevel = 0;
}
CGenerator::~CGenerator() {
}

// Note the use of std::pair to hold two strings - the running string for the header file and the running string for  the c file.
void CGenerator::generateCompSet(std::map<std::string, NodeTree<ASTData>*> ASTs, std::string outputName) {
	//Generate an entire set of files
	std::string buildString = "#!/bin/sh\ncc -std=c99 ";
	std::cout << "\n\n =====GENERATE PASS===== \n\n" << std::endl;
    // This is made earlier now, as we want to put the dot files here too
    //if (mkdir(("./" + outputName).c_str(), 0755)) {
        //std::cerr << "\n\n =====GENERATE PASS===== \n\n" << std::endl;
        //std::cerr << "Could not make directory " << outputName << std::endl;
        ////throw "could not make directory ";
    //}

    std::cout << "\n\nGenerate pass for: " << outputName << std::endl;
    buildString += outputName + ".c ";
    std::ofstream outputCFile, outputHFile;
    outputCFile.open(outputName + "/" + outputName + ".c");
    outputHFile.open(outputName + "/" + outputName + ".h");
    if (outputCFile.is_open() || outputHFile.is_open()) {
        // Prequel common to all files
        auto chPair = generateTranslationUnit(outputName, ASTs);
        outputHFile << "#include <stdbool.h>\n#include <stdlib.h>\n#include <stdio.h>\n" << chPair.first;
        outputCFile << "#include \"" + outputName + ".h\"\n\n" << chPair.second;
    } else {
        std::cerr << "Cannot open file " << outputName << ".c/h" << std::endl;
    }
    outputCFile.close();
    outputHFile.close();

	buildString += "-o " + outputName;
	std::ofstream outputBuild;
	outputBuild.open(outputName + "/" + split(outputName, '/').back() + ".sh");
	outputBuild << buildString;
	outputBuild.close();
}

std::string CGenerator::tabs() {
	std::string returnTabs;
	for (int i = 0; i < tabLevel; i++)
		returnTabs += "\t";
	return returnTabs;
}

std::string CGenerator::generateClassStruct(NodeTree<ASTData>* from) {
    auto data = from->getData();
    auto children = from->getChildren();
    std::string objectString = "struct __struct_dummy_" + CifyName(data.symbol.getName()) + "__ {\n";
    tabLevel++;
    for (int i = 0; i < children.size(); i++) {
        std::cout << children[i]->getName() << std::endl;
        if (children[i]->getName() != "function")
            objectString += tabs() + generate(children[i], nullptr) + "\n";
    }
    tabLevel--;
    objectString += "};";
    return objectString;
}

// This method recurseivly generates all aliases of some definition
std::string CGenerator::generateAliasChains(std::map<std::string, NodeTree<ASTData>*> ASTs, NodeTree<ASTData>* definition) {
    std::string output;
    for (auto trans : ASTs) {
        for (auto i = trans.second->getDataRef()->scope.begin(); i != trans.second->getDataRef()->scope.end(); i++) {
            for (auto declaration : i->second) {
                auto declarationData = declaration->getDataRef();
                if (declarationData->type == type_def
                        && declarationData->valueType->typeDefinition != declaration
                        && declarationData->valueType->typeDefinition == definition) {
                    output += "typedef " + CifyName(definition->getDataRef()->symbol.getName()) + " " +  CifyName(declarationData->symbol.getName()) + ";\n";
                    // Recursively add the ones that depend on this one
                    output += generateAliasChains(ASTs, declaration);
                }
            }
        }
    }
    return output;
}

bool CGenerator::isUnderTranslationUnit(NodeTree<ASTData>* from, NodeTree<ASTData>* node) {
    auto scope = from->getDataRef()->scope;
    for (auto i : scope)
        for (auto j : i.second)
            if (j == node)
                return true;

    auto upper = scope.find("~enclosing_scope");
    if (upper != scope.end())
        return isUnderTranslationUnit(upper->second[0], node);
    return false;
}

NodeTree<ASTData>* CGenerator::highestScope(NodeTree<ASTData>* node) {
    auto it = node->getDataRef()->scope.find("~enclosing_scope");
    while (it != node->getDataRef()->scope.end()) {
        node = it->second[0];
        it = node->getDataRef()->scope.find("~enclosing_scope");
    }
    return node;
}

// We do translation units in their own function so they can do the pariwise h/c stuff and regualr in function body generation does not
std::pair<std::string, std::string> CGenerator::generateTranslationUnit(std::string name, std::map<std::string, NodeTree<ASTData>*> ASTs) {
    // We now pass in the entire map of ASTs and loop through them so that we generate out into a single file
    std::string cOutput, hOutput;
    // Ok, so we've got to do this in passes to preserve mututally recursive definitions.
    //
    // First Pass: All classes get "struct dummy_thing; typedef struct dummy_thing thing;".
    //                  Also, other typedefs follow after their naming.
    // Second Pass: All top level variable declarations
    // Third Pass: Define all actual structs of a class, in correct order (done with posets)
    // Fourth Pass: Declare all function prototypes (as functions may be mutually recursive too).
    //                  (this includes object methods)
    // Fifth Pass: Define all functions (including object methods).

    // However, most of these do not actually have to be done as separate passes. First, second, fourth, and fifth
    // are done simultanously, but append to different strings that are then concatinated properly, in order.

    std::string importIncludes = "/**\n * Import Includes\n */\n\n";
    std::string variableExternDeclarations = "/**\n * Extern Variable Declarations \n */\n\n";
    std::string plainTypedefs = "/**\n * Plain Typedefs\n */\n\n";
    std::string variableDeclarations = "/**\n * Variable Declarations \n */\n\n";
    std::string classStructs = "/**\n * Class Structs\n */\n\n";
    std::string functionPrototypes = "/**\n * Function Prototypes\n */\n\n";
    std::string functionDefinitions = "/**\n * Function Definitions\n */\n\n";


    // And get the correct order for emiting classes, but not if they're not in our file, then they will get included
    // Note that this is not sufsticated enough for some multiple file mutually recursive types, but I want to get this simple version working first
    Poset<NodeTree<ASTData>*> typedefPoset;
    for (auto trans : ASTs) {
        auto children = trans.second->getChildren();
        for (int i = 0; i < children.size(); i++) {
            if (children[i]->getDataRef()->type == type_def) {
                // If we're an alias type, continue. We handle those differently
                if (children[i]->getDataRef()->valueType->typeDefinition != children[i])
                    continue;

                typedefPoset.addVertex(children[i]); // We add this definition by itself just in case there are no dependencies.
                // If it has dependencies, there's no harm in adding it here
                // Go through every child in the class looking for declaration statements. For each of these that is not a primitive type
                // we will add a dependency from this definition to that definition in the poset.
                std::vector<NodeTree<ASTData>*> classChildren = children[i]->getChildren();
                for (auto j : classChildren) {
                    if (j->getDataRef()->type == declaration_statement) {
                        Type* decType = j->getChildren()[0]->getDataRef()->valueType;           // Type of the declaration
                        if (decType->typeDefinition && decType->getIndirection() == 0)	        // If this is a custom type and not a pointer
                            typedefPoset.addRelationship(children[i], decType->typeDefinition); // Add a dependency
                    }
                }
            }
        }
    }
    //Now generate the typedef's in the correct, topological order
    for (NodeTree<ASTData>* i : typedefPoset.getTopoSort())
        classStructs += generateClassStruct(i) + "\n";

    // Declare everything in translation unit scope here (now for ALL translation units). (allows stuff from other files, automatic forward declarations)
    // Also, everything in all of the import's scopes
    for (auto trans : ASTs) {
        for (auto i = trans.second->getDataRef()->scope.begin(); i != trans.second->getDataRef()->scope.end(); i++) {
            for (auto declaration : i->second) {
                std::vector<NodeTree<ASTData>*> decChildren = declaration->getChildren();
                ASTData declarationData = declaration->getData();
                switch(declarationData.type) {
                    case identifier:
                        variableDeclarations += ValueTypeToCType(declarationData.valueType) + " " + declarationData.symbol.getName() + "; /*identifier*/\n";
                        variableExternDeclarations += "extern " + ValueTypeToCType(declarationData.valueType) + " " + declarationData.symbol.getName() + "; /*extern identifier*/\n";
                        break;
                    case function:
                        {
                            if (declarationData.valueType->baseType == template_type)
                                functionPrototypes += "/* template function " + declarationData.symbol.toString() + " */\n";
                            else if (decChildren.size() == 0) //Not a real function, must be a built in passthrough
                                functionPrototypes += "/* built in function: " + declarationData.symbol.toString() + " */\n";
                            else {
                                functionPrototypes += "\n" + ValueTypeToCType(declarationData.valueType) + " ";
                                std::string nameDecoration, parameters;
                                for (int j = 0; j < decChildren.size()-1; j++) {
                                    if (j > 0)
                                        parameters += ", ";
                                    parameters += ValueTypeToCType(decChildren[j]->getData().valueType) + " " + generate(decChildren[j], nullptr);
                                    nameDecoration += "_" + ValueTypeToCTypeDecoration(decChildren[j]->getData().valueType);
                                }
                                functionPrototypes += CifyName(declarationData.symbol.getName() + nameDecoration) + "(" + parameters + "); /*func*/\n";
                                // generate function
                                std::cout << "Generating " << CifyName(declarationData.symbol.getName()) << std::endl;
                                functionDefinitions += generate(declaration, nullptr);
                            }
                        }
                        break;
                    case type_def:
                        //type
                        plainTypedefs += "/*typedef " + declarationData.symbol.getName() + " */\n";

                        if (declarationData.valueType->baseType == template_type) {
                            plainTypedefs += "/* non instantiated template " + declarationData.symbol.getName() + " */";
                        } else if (declarationData.valueType->typeDefinition != declaration) {
                            if (declarationData.valueType->typeDefinition)
                                continue; // Aliases of objects are done with the thing it alises
                            // Otherwise, we're actually a renaming of a primitive, can generate here
                            plainTypedefs += "typedef " + ValueTypeToCType(declarationData.valueType) + " " + CifyName(declarationData.symbol.getName()) + ";\n";
                            plainTypedefs += generateAliasChains(ASTs, declaration);
                        } else {
                            plainTypedefs += "typedef struct __struct_dummy_" + CifyName(declarationData.symbol.getName()) + "__ " + CifyName(declarationData.symbol.getName())  + ";\n";
                            functionPrototypes += "/* Method Prototypes for " + declarationData.symbol.getName() + " */\n";
                            // We use a seperate string for this because we only include it if this is the file we're defined in
                            std::string objectFunctionDefinitions = "/* Method Definitions for " + declarationData.symbol.getName() + " */\n";
                            for (int j = 0; j < decChildren.size(); j++) {
                                std::cout << decChildren[j]->getName() << std::endl;
                                if (decChildren[j]->getName() == "function") //If object method
                                    objectFunctionDefinitions += generateObjectMethod(declaration, decChildren[j], &functionPrototypes) + "\n";
                            }
                            // Add all aliases to the plain typedefs. This will add any alias that aliases to this object, and any alias that aliases to that, and so on
                            plainTypedefs += generateAliasChains(ASTs, declaration);
                            functionPrototypes += "/* Done with " + declarationData.symbol.getName() + " */\n";
                            // include methods
                            functionDefinitions += objectFunctionDefinitions + "/* Done with " + declarationData.symbol.getName() + " */\n";
                        }
                        break;
                    default:
                        //std::cout << "Declaration? named " << declaration->getName() << " of unknown type " << ASTData::ASTTypeToString(declarationData.type) << " in translation unit scope" << std::endl;
                        cOutput += "/*unknown declaration named " + declaration->getName() + "*/\n";
                        hOutput += "/*unknown declaration named " + declaration->getName() + "*/\n";
                }
            }
        }
    }
    hOutput += plainTypedefs + importIncludes + variableExternDeclarations + classStructs + functionPrototypes;
    cOutput += variableDeclarations + functionDefinitions;
    return std::make_pair(hOutput, cOutput);
}

//The enclosing object is for when we're generating the inside of object methods. They allow us to check scope lookups against the object we're in
std::string CGenerator::generate(NodeTree<ASTData>* from, NodeTree<ASTData>* enclosingObject) {
	ASTData data = from->getData();
	std::vector<NodeTree<ASTData>*> children = from->getChildren();
    std::string output;
	switch (data.type) {
		case translation_unit:
		{
            // Should not happen! We do this in it's own function now!
            std::cerr << "Trying to normal generate a translation unit! That's a nono! (" << from->getDataRef()->toString() << ")" << std::endl;
            throw "That's not gonna work";
		}
			break;
		case interpreter_directive:
			//Do nothing
			break;
		case import:
            return "/* never reached import? */\n";
			//return "include \"" + data.symbol.getName() + ".h\" //woo importing!\n";
			//return "#include <" + data.symbol.getName() + ">\n";
		case identifier:
		{
            //but first, if we're this, we should just emit. (assuming enclosing object) (note that technically this would fall through, but for errors)
            if (data.symbol.getName() == "this") {
                if (enclosingObject)
                    return "this";
                else
                    std::cerr << "Error: this used in non-object scope" << std::endl;
            }
			//If we're in an object method, and our enclosing scope is that object, we're a member of the object and should use the this reference.
			std::string preName;
			if (enclosingObject && enclosingObject->getDataRef()->scope.find(data.symbol.getName()) != enclosingObject->getDataRef()->scope.end())
				preName += "this->";
			return preName + CifyName(data.symbol.getName()); //Cifying does nothing if not an operator overload
		}
		case function:
		{
			if (data.valueType->baseType == template_type)
				return "/* template function: " + data.symbol.getName() + " */";
			output += "\n" + ValueTypeToCType(data.valueType) + " ";
			std::string nameDecoration, parameters;
			for (int j = 0; j < children.size()-1; j++) {
				if (j > 0)
					parameters += ", ";
				parameters += ValueTypeToCType(children[j]->getData().valueType) + " " + generate(children[j], enclosingObject);
				nameDecoration += "_" + ValueTypeToCTypeDecoration(children[j]->getData().valueType);
			}
			output += CifyName(data.symbol.getName() + nameDecoration) + "(" + parameters + ")\n" + generate(children[children.size()-1], enclosingObject);
			return output;
		}
		case code_block:
        {
			output += "{\n";
            std::string destructorString = "";
            tabLevel++;
			for (int i = 0; i < children.size(); i++) {
				//std::cout << "Line " << i << std::endl;
				std::string line = generate(children[i], enclosingObject);
				//std::cout << line << std::endl;
				output += line;
                if (children[i]->getChildren().size() && children[i]->getChildren()[0]->getDataRef()->type == declaration_statement) {
                    NodeTree<ASTData> *identifier = children[i]->getChildren()[0]->getChildren()[0];
                    Type* declarationType = identifier->getDataRef()->valueType;
                    if (declarationType->getIndirection())
                        continue;
                    NodeTree<ASTData> *typeDefinition = declarationType->typeDefinition;
                    if (!typeDefinition)
                        continue;
                    if (typeDefinition->getDataRef()->scope.find("destruct") == typeDefinition->getDataRef()->scope.end())
                        continue;
                    destructorString += tabs() + CifyName(typeDefinition->getDataRef()->symbol.getName())
                        + "__" + "destruct" + "(&" + generate(identifier, enclosingObject) + ");\n";//Call the destructor
                }
            }
            output += destructorString;
            tabLevel--;
			output += tabs() + "}";
			return output;
        }
        case expression:
			output += " " + data.symbol.getName() + ", ";
		case boolean_expression:
			output += " " + data.symbol.getName() + " ";
		case statement:
			return tabs() + generate(children[0], enclosingObject) + ";\n";
		case if_statement:
			output += "if (" + generate(children[0], enclosingObject) + ")\n\t";
            // We have to see if the then statement is a regular single statement or a block.
            // If it's a block, because it's also a statement a semicolon will be emitted even though
            // we don't want it to be, as if (a) {b}; else {c}; is not legal C, but if (a) {b} else {c}; is.
            if (children[1]->getChildren()[0]->getDataRef()->type == code_block) {
                std::cout << "Then statement is a block, emitting the block not the statement so no trailing semicolon" << std::endl;
                output += generate(children[1]->getChildren()[0], enclosingObject);
            } else {
                std::cout << "Then statement is a simple statement, regular emitting the statement so trailing semicolon" << std::endl;
                output += generate(children[1], enclosingObject);
            }
			if (children.size() > 2)
				output += " else " + generate(children[2], enclosingObject);
			return output;
		case while_loop:
			output += "while (" + generate(children[0], enclosingObject) + ")\n\t" + generate(children[1], enclosingObject);
			return output;
		case for_loop:
			//The strSlice's are there to get ride of an unwanted return and an unwanted semicolon(s)
			output += "for (" + strSlice(generate(children[0], enclosingObject),0,-3) + generate(children[1], enclosingObject) + ";" + strSlice(generate(children[2], enclosingObject),0,-3) + ")\n\t" + generate(children[3], enclosingObject);
			return output;
		case return_statement:
			if (children.size())
				return "return " + generate(children[0], enclosingObject);
			else
				return "return";
		case assignment_statement:
			return generate(children[0], enclosingObject) + " = " + generate(children[1], enclosingObject);
		case declaration_statement:
			if (children.size() == 1)
				return ValueTypeToCType(children[0]->getData().valueType) + " " + generate(children[0], enclosingObject) + ";";
			else if (children[1]->getChildren().size() && children[1]->getChildren()[0]->getChildren().size() > 1
                                                 && children[1]->getChildren()[0]->getChildren()[1] == children[0]) {
                //That is, if we're a declaration with an init position call (Object a.construct())
                //We can tell if our function call (children[1])'s access operation([0])'s lhs ([1]) is the thing we just declared (children[0])
                return ValueTypeToCType(children[0]->getData().valueType) + " " + generate(children[0], enclosingObject) + "; " + generate(children[1]) + "/*Init Position Call*/";
            } else
				return ValueTypeToCType(children[0]->getData().valueType) + " " + generate(children[0], enclosingObject) + " = " + generate(children[1], enclosingObject) + ";";
		case if_comp:
			if (generate(children[0], enclosingObject) == generatorString)
				return generate(children[1], enclosingObject);
			return "";
		case simple_passthrough:
			return strSlice(generate(children[0], enclosingObject), 3, -4);
		case function_call:
		{
			//NOTE: The first (0th) child of a function call node is the declaration of the function

			//Handle operators specially for now. Will later replace with
			//Inlined functions in the standard library
			// std::string name = data.symbol.getName();
			// std::cout << name << " == " << children[0]->getData().symbol.getName() << std::endl;
			std::string name = children[0]->getDataRef()->symbol.getName();
			ASTType funcType = children[0]->getDataRef()->type;
			std::cout << "Doing function: " << name << std::endl;
			//Test for specail functions only if what we're testing is, indeed, the definition, not a function call that returns a callable function pointer
			if (funcType == function) {
				if (name == "++" || name == "--")
					return generate(children[1], enclosingObject) + name;
				if ( (name == "*" || name == "&" || name == "!" ) && children.size() == 2) //Is dereference, not multiplication, address-of, or other unary operator
					return name + "(" + generate(children[1], enclosingObject) + ")";
				if (name == "[]")
					return "(" + generate(children[1], enclosingObject) + ")[" +generate(children[2],enclosingObject) + "]";
				if (name == "+" || name == "-" || name == "*" || name == "/" || name == "==" || name == ">=" || name == "<=" || name == "!="
					|| name == "<" || name == ">" || name == "%" || name == "+=" || name == "-=" || name == "*=" || name == "/=" || name == "||"
					|| name == "&&") {
                    std::cout << "THIS IS IT NAME: " << name << std::endl;
					return "((" + generate(children[1], enclosingObject) + ")" + name + "(" + generate(children[2], enclosingObject) + "))";
                } else if (name == "." || name == "->") {
					if (children.size() == 1)
					 	return "/*dot operation with one child*/" + generate(children[0], enclosingObject) + "/*end one child*/";
					 //If this is accessing an actual function, find the function in scope and take the appropriate action. Probabally an object method
					 if (children[2]->getDataRef()->type == function) {
					 	std::string functionName = children[2]->getDataRef()->symbol.getName();
					 	NodeTree<ASTData>* possibleObjectType = children[1]->getDataRef()->valueType->typeDefinition;
					 	//If is an object method, generate it like one. Needs extension/modification for inheritence
					 	if (possibleObjectType) {
                            NodeTree<ASTData>* unaliasedTypeDef = getMethodsObjectType(possibleObjectType, functionName);
                            if (unaliasedTypeDef) { //Test to see if the function's a member of this type_def, or if this is an alias, of the original type. Get this original type if it exists.
					 		    std::string nameDecoration;
					 		    std::vector<NodeTree<ASTData>*> functionDefChildren = children[2]->getChildren(); //The function def is the rhs of the access operation
					 		    std::cout << "Decorating (in access-should be object) " << name << " " << functionDefChildren.size() << std::endl;
					 		    for (int i = 0; i < (functionDefChildren.size() > 0 ? functionDefChildren.size()-1 : 0); i++)
					 		    	nameDecoration += "_" + ValueTypeToCTypeDecoration(functionDefChildren[i]->getData().valueType);
/*HERE*/				 	    return CifyName(unaliasedTypeDef->getDataRef()->symbol.getName()) +"__" + CifyName(functionName + nameDecoration) + "(" + (name == "." ? "&" : "") + generate(children[1], enclosingObject) + ",";
					 		    //The comma lets the upper function call know we already started the param list
					 		    //Note that we got here from a function call. We just pass up this special case and let them finish with the perentheses
                            } else {
					 	        std::cout << "Is not in scope or not type" << std::endl;
					            return "((" + generate(children[1], enclosingObject) + ")" + name + functionName + ")";
                            }
                        } else {
					 	    std::cout << "Is not in scope or not type" << std::endl;
					        return "((" + generate(children[1], enclosingObject) + ")" + name + functionName + ")";
					 	}
					} else {
						//return "((" + generate(children[1], enclosingObject) + ")" + name + generate(children[2], enclosingObject) + ")";
						return "((" + generate(children[1], enclosingObject) + ")" + name + generate(children[2]) + ")";
					}
				} else {
					//It's a normal function call, not a special one or a method or anything. Name decorate.
					std::vector<NodeTree<ASTData>*> functionDefChildren = children[0]->getChildren();
					std::cout << "Decorating (none-special)" << name << " " << functionDefChildren.size() << std::endl;
					std::string nameDecoration;
					for (int i = 0; i < (functionDefChildren.size() > 0 ? functionDefChildren.size()-1 : 0); i++)
				 		nameDecoration += "_" + ValueTypeToCTypeDecoration(functionDefChildren[i]->getData().valueType);
				 	//Check to see if we're inside of an object and this is a method call
					bool isSelfObjectMethod = enclosingObject && contains(enclosingObject->getChildren(), children[0]);
					if (isSelfObjectMethod)
						output += CifyName(enclosingObject->getDataRef()->symbol.getName()) +"__";
/*HERE*/			output += CifyName(name + nameDecoration) + "(";
				 	if (isSelfObjectMethod)
				 		output += children.size() > 1 ? "this," : "this";
				}
			} else {
				//This part handles cases where our definition isn't the function definition (that is, it is probabally the return from another function)
				//It's probabally the result of an access function call (. or ->) to access an object method.
				std::string functionCallSource = generate(children[0], enclosingObject);
				if (functionCallSource[functionCallSource.size()-1] == ',') //If it's a member method, it's already started the parameter list.
					output += children.size() > 1 ? functionCallSource : functionCallSource.substr(0, functionCallSource.size()-1);
				else
					output += functionCallSource + "(";
			}
			for (int i = 1; i < children.size(); i++) //children[0] is the declaration
				if (i < children.size()-1)
					output += generate(children[i], enclosingObject) + ", ";
				else
					output += generate(children[i], enclosingObject);
			output += ") ";
			return output;
		}
		case value:
			return data.symbol.getName();

		default:
			std::cout << "Nothing!" << std::endl;
	}
	for (int i = 0; i < children.size(); i++)
		output += generate(children[i], enclosingObject);

	return output;
}
NodeTree<ASTData>* CGenerator::getMethodsObjectType(NodeTree<ASTData>* scope, std::string functionName) {
    //check the thing
    while (scope != scope->getDataRef()->valueType->typeDefinition) //type is an alias, follow it to the definition
        scope = scope->getDataRef()->valueType->typeDefinition;
    return (scope->getDataRef()->scope.find(functionName) != scope->getDataRef()->scope.end()) ? scope : NULL;
}

// Returns the function prototype in the out param and the full definition normally
std::string CGenerator::generateObjectMethod(NodeTree<ASTData>* enclosingObject, NodeTree<ASTData>* from, std::string *functionPrototype) {
	ASTData data = from->getData();
	Type enclosingObjectType = *(enclosingObject->getDataRef()->valueType); //Copy a new type so we can turn it into a pointer if we need to
	enclosingObjectType.increaseIndirection();
	std::vector<NodeTree<ASTData>*> children = from->getChildren();
	std::string nameDecoration, parameters;
	for (int i = 0; i < children.size()-1; i++) {
		parameters += ", " + ValueTypeToCType(children[i]->getData().valueType) + " " + generate(children[i]);
		nameDecoration += "_" + ValueTypeToCTypeDecoration(children[i]->getData().valueType);
	}
    std::string functionSignature = "\n" + ValueTypeToCType(data.valueType) + " " + CifyName(enclosingObject->getDataRef()->symbol.getName()) +"__"
		+ CifyName(data.symbol.getName()) + nameDecoration + "(" + ValueTypeToCType(&enclosingObjectType)
		+ " this" + parameters + ")";
    *functionPrototype += functionSignature + ";\n";
    return functionSignature + "\n" +  generate(children[children.size()-1], enclosingObject); //Pass in the object so we can properly handle access to member stuff
}


std::string CGenerator::ValueTypeToCType(Type *type) { return ValueTypeToCTypeThingHelper(type, "*"); }
std::string CGenerator::ValueTypeToCTypeDecoration(Type *type) { return ValueTypeToCTypeThingHelper(type, "_P__"); }
std::string CGenerator::ValueTypeToCTypeThingHelper(Type *type, std::string ptrStr) {
	std::string return_type;
	switch (type->baseType) {
		case none:
			if (type->typeDefinition)
				return_type = CifyName(type->typeDefinition->getDataRef()->symbol.getName());
			else
				return_type = "none";
			break;
		case void_type:
			return_type = "void";
			break;
		case boolean:
			return_type = "bool";
			break;
		case integer:
			return_type = "int";
			break;
		case floating:
			return_type = "float";
			break;
		case double_percision:
			return_type = "double";
			break;
		case character:
			return_type = "char";
			break;
		default:
			return_type = "unknown_ValueType";
			break;
	}
	for (int i = 0; i < type->getIndirection(); i++)
		return_type += ptrStr;
	return return_type;
}

std::string CGenerator::CifyName(std::string name) {
	std::string operatorsToReplace[] = { 	"+", "plus",
											"-", "minus",
											"*", "star",
											"/", "div",
											"%", "mod",
											"^", "carat",
											"&", "amprsd",
											"|", "pipe",
											"~", "tilde",
											"!", "exclamationpt",
											",", "comma",
											"=", "equals",
											"++", "doubleplus",
											"--", "doubleminus",
											"<<", "doubleleft",
											">>", "doubleright",
											"::", "scopeop",
											"==", "doubleequals",
											"!=", "notequals",
											"&&", "doubleamprsnd",
											"||", "doublepipe",
											"+=", "plusequals",
											"-=", "minusequals",
											"/=", "divequals",
											"%=", "modequals",
											"^=", "caratequals",
											"&=", "amprsdequals",
											"|=", "pipeequals",
											"*=", "starequals",
											"<<=", "doublerightequals",
											"<", "lessthan",
											">", "greaterthan",
											">>=", "doubleleftequals",
											"(", "openparen",
											")", "closeparen",
											"[", "openbracket",
											"]", "closebracket",
											" ", "space",
											".", "dot",
											"->", "arrow" };
	int length = sizeof(operatorsToReplace)/sizeof(std::string);
	//std::cout << "Length is " << length << std::endl;
	for (int i = 0; i < length; i+= 2) {
		size_t foundPos = name.find(operatorsToReplace[i]);
		while(foundPos != std::string::npos) {
			name = strSlice(name, 0, foundPos) + "_" + operatorsToReplace[i+1] + "_" + strSlice(name, foundPos+operatorsToReplace[i].length(), -1);
			foundPos = name.find(operatorsToReplace[i]);
		}
	}
	return name;
}

