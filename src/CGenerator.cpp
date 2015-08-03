#include "CGenerator.h"

CGenerator::CGenerator() : generatorString("__C__") {
	tabLevel = 0;
    id = 0;
    function_header = "fun_";
    functionTypedefString = "";
    functionTypedefStringPre = "";
}
CGenerator::~CGenerator() {
}

// Note the use of std::pair to hold two strings - the running string for the header file and the running string for  the c file.
void CGenerator::generateCompSet(std::map<std::string, NodeTree<ASTData>*> ASTs, std::string outputName) {
	//Generate an entire set of files
	std::string buildString = "#!/bin/sh\ncc -g -std=c99 ";
	std::cout << "\n\n =====GENERATE PASS===== \n\n" << std::endl;
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

	buildString += linkerString;
	buildString += "-o " + outputName;
	std::ofstream outputBuild;
	outputBuild.open(outputName + "/" + split(outputName, '/').back() + ".sh");
	outputBuild << buildString;
	outputBuild.close();
    std::cout << "DEFER DOUBLE STACK " << deferDoubleStack.size() << std::endl;
}

std::string CGenerator::tabs() {
	std::string returnTabs;
	for (int i = 0; i < tabLevel; i++)
		returnTabs += "\t";
	return returnTabs;
}

std::string CGenerator::getID() {
    return intToString(id++);
}

std::string CGenerator::generateClassStruct(NodeTree<ASTData>* from) {
    auto data = from->getData();
    auto children = from->getChildren();
    std::string objectString = "struct __struct_dummy_" + scopePrefix(from) + CifyName(data.symbol.getName()) + "__ {\n";
    tabLevel++;
    for (int i = 0; i < children.size(); i++) {
        std::cout << children[i]->getName() << std::endl;
        if (children[i]->getName() != "function")
            objectString += tabs() + generate(children[i], nullptr).oneString() + "\n";
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
                    output += "typedef " +
                        scopePrefix(definition) + CifyName(definition->getDataRef()->symbol.getName()) + " " +
                        scopePrefix(declaration) + CifyName(declarationData->symbol.getName()) + ";\n";
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
    std::string topLevelCPassthrough = "/**\n * Top Level C Passthrough\n */\n\n";
    std::string variableExternDeclarations = "/**\n * Extern Variable Declarations \n */\n\n";
    std::string plainTypedefs = "/**\n * Plain Typedefs\n */\n\n";
    std::string variableDeclarations = "/**\n * Variable Declarations \n */\n\n";
    std::string classStructs = "/**\n * Class Structs\n */\n\n";
    std::string functionPrototypes = "/**\n * Function Prototypes\n */\n\n";
    std::string functionDefinitions = "/**\n * Function Definitions\n */\n\n";
    // There also exists functionTypedefString which is a member variable that keeps
    // track of utility typedefs that allow our C type generation to be more sane
    // it is emitted in the h file right before functionPrototypes


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
    // Also c passthrough
    for (auto trans : ASTs) {
        // First go through and emit all the passthroughs, etc
        for (auto i : trans.second->getChildren()) {
            if (i->getDataRef()->type == if_comp)
                topLevelCPassthrough += generate(i, nullptr).oneString();
        }

        for (auto i = trans.second->getDataRef()->scope.begin(); i != trans.second->getDataRef()->scope.end(); i++) {
            for (auto declaration : i->second) {
                std::vector<NodeTree<ASTData>*> decChildren = declaration->getChildren();
                ASTData declarationData = declaration->getData();
                switch(declarationData.type) {
                    case identifier:
                        {
                        auto parent = declaration->getDataRef()->scope["~enclosing_scope"][0];
                        if (parent->getChildren().size() == 1)
                            variableDeclarations += ValueTypeToCType(declarationData.valueType,  scopePrefix(declaration) + declarationData.symbol.getName()) + "; /*identifier*/\n";
                        else
                            variableDeclarations += ValueTypeToCType(declarationData.valueType, generate(parent->getChildren()[0], nullptr, true, nullptr).oneString()) + " = " + generate(parent->getChildren()[1], nullptr, true, nullptr).oneString() + ";";
                        variableExternDeclarations += "extern " + ValueTypeToCType(declarationData.valueType, declarationData.symbol.getName()) + "; /*extern identifier*/\n";
                        break;
                        }
                    case function:
                        {
                            if (declarationData.valueType->baseType == template_type)
                                functionPrototypes += "/* template function " + declarationData.symbol.toString() + " */\n";
                            else if (decChildren.size() == 0) //Not a real function, must be a built in passthrough
                                functionPrototypes += "/* built in function: " + declarationData.symbol.toString() + " */\n";
                            else {
                                std::string nameDecoration, parameters;
                                if (declarationData.closedVariables.size())
                                    parameters += closureStructType(declarationData.closedVariables) + "*";
                                for (int j = 0; j < decChildren.size()-1; j++) {
                                    if (j > 0 || declarationData.closedVariables.size() )
                                        parameters += ", ";
                                    parameters += ValueTypeToCType(decChildren[j]->getData().valueType, generate(decChildren[j], nullptr).oneString());
                                    nameDecoration += "_" + ValueTypeToCTypeDecoration(decChildren[j]->getData().valueType);
                                }
                                functionPrototypes += "\n" + ValueTypeToCType(declarationData.valueType->returnType, ((declarationData.symbol.getName() == "main") ? "" : function_header + scopePrefix(declaration)) +
                                                    CifyName(declarationData.symbol.getName() + nameDecoration)) +
                                                    "(" + parameters + "); /*func*/\n";
                                // generate function
                                std::cout << "Generating " << scopePrefix(declaration) +
                                                            CifyName(declarationData.symbol.getName()) << std::endl;
                                functionDefinitions += generate(declaration, nullptr).oneString();
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
                            plainTypedefs += "typedef " + ValueTypeToCType(declarationData.valueType,
                                                scopePrefix(declaration) +
                                                CifyName(declarationData.symbol.getName())) + ";\n";
                            plainTypedefs += generateAliasChains(ASTs, declaration);
                        } else {
                            plainTypedefs += "typedef struct __struct_dummy_" +
                                                scopePrefix(declaration) + CifyName(declarationData.symbol.getName()) + "__ " +
                                                scopePrefix(declaration) + CifyName(declarationData.symbol.getName())  + ";\n";
                            functionPrototypes += "/* Method Prototypes for " + declarationData.symbol.getName() + " */\n";
                            // We use a seperate string for this because we only include it if this is the file we're defined in
                            std::string objectFunctionDefinitions = "/* Method Definitions for " + declarationData.symbol.getName() + " */\n";
                            for (int j = 0; j < decChildren.size(); j++) {
                                std::cout << decChildren[j]->getName() << std::endl;
                                if (decChildren[j]->getName() == "function"
                                        && decChildren[j]->getDataRef()->valueType->baseType != template_type) //If object method and not template
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
    hOutput += plainTypedefs + importIncludes + topLevelCPassthrough + functionTypedefStringPre + variableExternDeclarations + classStructs + functionTypedefString + functionPrototypes;
    cOutput += variableDeclarations + functionDefinitions;
    return std::make_pair(hOutput, cOutput);
}

//The enclosing object is for when we're generating the inside of object methods. They allow us to check scope lookups against the object we're in
CCodeTriple CGenerator::generate(NodeTree<ASTData>* from, NodeTree<ASTData>* enclosingObject, bool justFuncName, NodeTree<ASTData>* enclosingFunction) {
	ASTData data = from->getData();
	std::vector<NodeTree<ASTData>*> children = from->getChildren();
    //std::string output;
    CCodeTriple output;
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
            return CCodeTriple("/* never reached import? */\n");
		case identifier:
		{
            std::string preName = "";
            std::string postName = "";
            // check for this being a closed over variable
            // first, get declaring function, if it exists
            if (enclosingFunction) {
                if (enclosingFunction->getDataRef()->closedVariables.size()) {
                    std::cout << "WHOH IS A CLOSER" << std::endl;
                    if (enclosingFunction->getDataRef()->closedVariables.find(from) != enclosingFunction->getDataRef()->closedVariables.end()) {
                        preName += "(*closed_variables->";
                        postName += ")";
                    }
                }
            }
            // enclosing function comes first now, we might have a double closure that both close over the this pointer of an object
            //but first, if we're this, we should just emit. (assuming enclosing object) (note that technically this would fall through, but for errors)
            if (data.symbol.getName() == "this") {
                if (enclosingObject || enclosingFunction)
                    return CCodeTriple(preName + "this" + postName);
                std::cerr << "Error: this used in non-object scope" << std::endl;
                throw "Error: this used in non-object scope";
            }
			//If we're in an object method, and our enclosing scope is that object, we're a member of the object and should use the this reference.
			if (enclosingObject && enclosingObject->getDataRef()->scope.find(data.symbol.getName()) != enclosingObject->getDataRef()->scope.end())
				preName += "this->";
            // dereference references, but only if inside a function
			if (enclosingFunction && data.valueType->is_reference) {
				preName += "(*";
                postName += ")";
            }
            // we're scope prefixing EVERYTHING
			return preName + scopePrefix(from) + CifyName(data.symbol.getName()) + postName; //Cifying does nothing if not an operator overload
		}
		case function:
		{
			if (data.valueType->baseType == template_type)
				return "/* template function: " + data.symbol.getName() + " */";

            // we push on a new vector to hold parameters that might need a destructor call
            distructDoubleStack.push_back(std::vector<NodeTree<ASTData>*>());

			std::string nameDecoration, parameters;
            if (data.closedVariables.size())
                parameters += closureStructType(data.closedVariables) + " *closed_variables";
			for (int j = 0; j < children.size()-1; j++) {
				if (j > 0 || data.closedVariables.size())
					parameters += ", ";
				parameters += ValueTypeToCType(children[j]->getData().valueType, generate(children[j], enclosingObject, justFuncName, enclosingFunction).oneString());
				nameDecoration += "_" + ValueTypeToCTypeDecoration(children[j]->getData().valueType);
                // add parameters to distructDoubleStack so that their destructors will be called at return (if they exist)
                distructDoubleStack.back().push_back(children[j]);
			}
            // this is for using functions as values
            if (justFuncName) {
                std::string funcName;
                if (data.symbol.getName() != "main")
                    funcName += function_header + scopePrefix(from);
                funcName += CifyName(data.symbol.getName() + nameDecoration);
                if (from->getDataRef()->closedVariables.size()) {
                    std::string tmpStruct = "closureStruct" + getID();
                    output.preValue += closureStructType(data.closedVariables) + " " + tmpStruct + " = {";
                    bool notFirst = false;
                    for (auto var : data.closedVariables) {
                        if (notFirst)
                            output.preValue += ", ";
                        notFirst = true;
                        std::string varName = var->getDataRef()->symbol.getName();
                        std::string preName;
                        if (enclosingObject && enclosingObject->getDataRef()->scope.find(varName) != enclosingObject->getDataRef()->scope.end())
                            preName += "this->";
                        varName = (varName == "this") ? varName : scopePrefix(var) + varName;
                        // so that we can close over things that have been closed over by an enclosing closure
                        output.preValue += "." + varName + " = &/*woo*/" + generate(var, enclosingObject, justFuncName, enclosingFunction).oneString() + "/*woo*/";
                        //output.preValue += "." + varName + " = &" + preName + varName;
                    }
                    output.preValue += "};\n";
                    output += "("+ ValueTypeToCType(data.valueType, "") +"){(void*)" + funcName + ", &" + tmpStruct + "}";
                } else {
                    output += "("+ ValueTypeToCType(data.valueType, "") +"){" + funcName + ", NULL}";
                }
            } else {
            // Note that we always wrap out child in {}, as we now allow one statement functions without a codeblock
                output = "\n" + ValueTypeToCType(data.valueType->returnType, ((data.symbol.getName() == "main") ? "" : function_header + scopePrefix(from)) +
                        CifyName(data.symbol.getName() + nameDecoration)) + "(" + parameters + ") {\n" + generate(children[children.size()-1], enclosingObject, justFuncName, from).oneString();
                output += emitDestructors(reverse(distructDoubleStack.back()), enclosingObject);
                output += "}\n";
            }

            distructDoubleStack.pop_back();
			return output;
		}
		case code_block:
        {
			output += "{\n";
            tabLevel++;

            // we push on a new vector to hold parameters that might need a destructor call
            distructDoubleStack.push_back(std::vector<NodeTree<ASTData>*>());
            // we push on a new vector to hold deferred statements
            deferDoubleStack.push_back(std::vector<NodeTree<ASTData>*>());
			for (int i = 0; i < children.size(); i++)
				output += generate(children[i], enclosingObject, justFuncName, enclosingFunction).oneString();
            // we pop off the vector and go through them in reverse emitting them
            for (auto iter = deferDoubleStack.back().rbegin(); iter != deferDoubleStack.back().rend(); iter++)
                output += generate(*iter, enclosingObject, justFuncName, enclosingFunction).oneString();
            deferDoubleStack.pop_back();
            output += emitDestructors(reverse(distructDoubleStack.back()), enclosingObject);
            distructDoubleStack.pop_back();

            tabLevel--;
			output += tabs() + "}";

			return output;
        }
        case expression:
			output += " " + data.symbol.getName() + ", ";
		case boolean_expression:
			output += " " + data.symbol.getName() + " ";
		case statement:
            {
			CCodeTriple stat = generate(children[0], enclosingObject, justFuncName, enclosingFunction);
			return tabs() + stat.preValue + stat.value + ";\n" + stat.postValue ;
            }
		case if_statement:
			output += "if (" + generate(children[0], enclosingObject, true, enclosingFunction) + ")\n\t";
            // We have to see if the then statement is a regular single statement or a block.
            // If it's a block, because it's also a statement a semicolon will be emitted even though
            // we don't want it to be, as if (a) {b}; else {c}; is not legal C, but if (a) {b} else {c}; is.
            if (children[1]->getChildren()[0]->getDataRef()->type == code_block) {
                std::cout << "Then statement is a block, emitting the block not the statement so no trailing semicolon" << std::endl;
                output += generate(children[1]->getChildren()[0], enclosingObject, justFuncName, enclosingFunction).oneString();
            } else {
                // ALSO we always emit blocks now, to handle cases like defer when several statements need to be
                // run in C even though it is a single Kraken statement
                std::cout << "Then statement is a simple statement, regular emitting the statement so trailing semicolon" << std::endl;
                output += "{ " + generate(children[1], enclosingObject, justFuncName, enclosingFunction).oneString() + " }";
            }
            // Always emit blocks here too
			if (children.size() > 2)
				output += " else { " + generate(children[2], enclosingObject, justFuncName, enclosingFunction).oneString() + " }";
			return output;
		case while_loop:
            {
            // we push on a new vector to hold while stuff that might need a destructor call
            loopDistructStackDepth.push(distructDoubleStack.size());
            distructDoubleStack.push_back(std::vector<NodeTree<ASTData>*>());
            // keep track of the current size of the deferDoubleStack so that statements that
            // break or continue inside this loop can correctly emit all of the defers through
            // all of the inbetween scopes
            loopDeferStackDepth.push(deferDoubleStack.size());
            // gotta do like this so that the preconditions can happen every loop
			output += "while (1) {\n";
            CCodeTriple condtition = generate(children[0], enclosingObject, true, enclosingFunction);
            output += condtition.preValue;
            output += "if (!( " + condtition.value + ")) break;\n";
            output += condtition.postValue;
            output += generate(children[1], enclosingObject, justFuncName, enclosingFunction).oneString();
            output += emitDestructors(reverse(distructDoubleStack.back()),enclosingObject);
            output +=  + "}";

            distructDoubleStack.pop_back();
            loopDistructStackDepth.pop();
            // and pop it off again
            loopDeferStackDepth.pop();
			return output;
            }
		case for_loop:
            {
            // we push on a new vector to hold for stuff that might need a destructor call
            loopDistructStackDepth.push(distructDoubleStack.size());
            distructDoubleStack.push_back(std::vector<NodeTree<ASTData>*>());
            // keep track of the current size of the deferDoubleStack so that statements that
            // break or continue inside this loop can correctly emit all of the defers through
            // all of the inbetween scopes
            loopDeferStackDepth.push(deferDoubleStack.size());
			//The strSlice's are there to get ride of an unwanted return and an unwanted semicolon(s)

            std::string doUpdateName = "do_update" + getID();
            // INITIALIZER
			output += "{";
            output += generate(children[0], enclosingObject, true, enclosingFunction).oneString();
            output += "bool " + doUpdateName + " = false;\n";
			output += "for (;;) {";
            // UPDATE
            output += "if (" + doUpdateName + ") {";
            output += generate(children[2], enclosingObject, true, enclosingFunction).oneString();
            output += "}\n";
            output += doUpdateName + " = true;\n";
            // CONDITION
            // note that the postValue happens whether or not we break
            CCodeTriple condition = generate(children[1], enclosingObject, true, enclosingFunction);
            output += condition.preValue;
            output += "if (!(" + condition.value + ")) {\n";
            output += condition.postValue;
            output += "break;\n}";
            output += condition.postValue;
            // BODY
            output += generate(children[3], enclosingObject, justFuncName, enclosingFunction).oneString();
            output += emitDestructors(reverse(distructDoubleStack.back()),enclosingObject);
            output += "}";
			output += "}";
            distructDoubleStack.pop_back();
            loopDistructStackDepth.pop();
            // and pop it off again
            loopDeferStackDepth.pop();
			return output;
            }
		case return_statement:
            {
            // we pop off the vector and go through them in reverse emitting them, going
            // through all of both arrays, as return will go through all scopes
            for (auto topItr = deferDoubleStack.rbegin(); topItr != deferDoubleStack.rend(); topItr++)
                for (auto iter = (*topItr).rbegin(); iter != (*topItr).rend(); iter++)
                    output += generate(*iter, enclosingObject, justFuncName, enclosingFunction).oneString();

            std::string destructors = emitDestructors(reverse(flatten(distructDoubleStack)),enclosingObject);
			if (children.size()) {
                CCodeTriple expr = generate(children[0], enclosingObject, true, enclosingFunction);
                output.preValue += expr.preValue;
                std::string retTemp = "ret_temp" + getID();
                // use the function's return value so we do the right thing with references
                output.preValue += ValueTypeToCType(enclosingFunction->getDataRef()->valueType->returnType, retTemp) + ";\n";
                if (enclosingFunction->getDataRef()->valueType->returnType->is_reference)
                    output.preValue += retTemp + " = &" + expr.value + ";\n";
                else if (methodExists(children[0]->getDataRef()->valueType, "copy_construct", std::vector<Type>{children[0]->getDataRef()->valueType->withIncreasedIndirection()}))
                    output.preValue += generateMethodIfExists(children[0]->getDataRef()->valueType, "copy_construct", "&"+retTemp + ", &" + expr.value, std::vector<Type>{children[0]->getDataRef()->valueType->withIncreasedIndirection()});
                else
                    output.preValue += retTemp + " = " + expr.value + ";\n";
                // move expr post to before return
                output.value += expr.postValue;
                output.value += destructors;
                output.value += "return " + retTemp;
            } else {
                output.value += destructors;
				output += "return";
            }
            return output;
            }
		case break_statement:
            // handle everything that's been deferred all the way back to the loop's scope
            for (int i = deferDoubleStack.size()-1; i >= loopDeferStackDepth.top(); i--)
                for (auto iter = deferDoubleStack[i].rbegin(); iter != deferDoubleStack[i].rend(); iter++)
                    output += generate(*iter, enclosingObject, justFuncName, enclosingFunction).oneString();
            // ok, emit destructors to where the loop ends
            output += emitDestructors(reverse(flatten(slice(distructDoubleStack,loopDistructStackDepth.top(),-1))),enclosingObject);
            return output + "break";
		case continue_statement:
            // handle everything that's been deferred all the way back to the loop's scope
            for (int i = deferDoubleStack.size()-1; i >= loopDeferStackDepth.top(); i--)
                for (auto iter = deferDoubleStack[i].rbegin(); iter != deferDoubleStack[i].rend(); iter++)
                    output += generate(*iter, enclosingObject, justFuncName, enclosingFunction).oneString();
            // ok, emit destructors to where the loop ends
            output += emitDestructors(reverse(flatten(slice(distructDoubleStack,loopDistructStackDepth.top(),-1))),enclosingObject);
            return output + "continue";
		case defer_statement:
            deferDoubleStack.back().push_back(children[0]);
            return CCodeTriple("/*defer " + generate(children[0], enclosingObject, justFuncName, enclosingFunction).oneString() + "*/");
		case assignment_statement:
			return generate(children[0], enclosingObject, justFuncName, enclosingFunction).oneString() + " = " + generate(children[1], enclosingObject, true, enclosingFunction);
		case declaration_statement:
            // adding declaration to the distructDoubleStack so that we can call their destructors when leaving scope (}, return, break, continue)
            // but only if we're inside an actual doublestack
            if ((distructDoubleStack.size()))
                distructDoubleStack.back().push_back(children[0]);

			if (children.size() == 1)
				return ValueTypeToCType(children[0]->getData().valueType, generate(children[0], enclosingObject, justFuncName, enclosingFunction).oneString()) + ";";
			else if (children[1]->getChildren().size() && children[1]->getChildren()[0]->getChildren().size() > 1
                                                 && children[1]->getChildren()[0]->getChildren()[1] == children[0]) {
                //That is, if we're a declaration with an init position call (Object a.construct())
                //We can tell if our function call (children[1])'s access operation([0])'s lhs ([1]) is the thing we just declared (children[0])
                // be sure to end value by passing oneString true
                return ValueTypeToCType(children[0]->getData().valueType, generate(children[0], enclosingObject, justFuncName, enclosingFunction).oneString()) + "; " + generate(children[1], enclosingObject, true, enclosingFunction).oneString(true) + "/*Init Position Call*/";
            } else {
                // copy constructor if exists (even for non same types)
                if (methodExists(children[0]->getDataRef()->valueType, "copy_construct", std::vector<Type>{children[1]->getDataRef()->valueType->withIncreasedIndirection()})) {
                    CCodeTriple toAssign = generate(children[1], enclosingObject, true, enclosingFunction);
                    std::string assignedTo = generate(children[0], enclosingObject, justFuncName, enclosingFunction).oneString();
                    output.value = toAssign.preValue;
                    output.value += ValueTypeToCType(children[0]->getData().valueType, assignedTo) + ";\n";
                    // we put the thing about to be copy constructed in a variable so we can for sure take its address
                    std::string toAssignTemp = "copy_construct_param" + getID();
                    output.value += ValueTypeToCType(children[1]->getData().valueType->withoutReference(), toAssignTemp) + " = " + toAssign.value + ";\n";

                    output.value += generateMethodIfExists(children[0]->getDataRef()->valueType, "copy_construct", "&" + assignedTo + ", &" + toAssignTemp, std::vector<Type>{children[1]->getDataRef()->valueType->withIncreasedIndirection()}) + ";\n" + output.postValue;
                    output.value += toAssign.postValue;
                    return output;
                } else {
                    // we might use this address in the right hand side (recursive closures), so split it up
                    std::string assignTo = generate(children[0], enclosingObject, justFuncName, enclosingFunction).oneString();
                    output.preValue = ValueTypeToCType(children[0]->getData().valueType, assignTo) + ";\n";
                    output += assignTo + " = " + generate(children[1], enclosingObject, true, enclosingFunction) + ";";
                    return output;
                }
            }
		case if_comp:
            // Lol, this doesn't work because the string gets prefixed now
			//if (generate(children[0], enclosingObject, enclosingFunction) == generatorString)
			if (children[0]->getDataRef()->symbol.getName() == generatorString)
				return generate(children[1], enclosingObject, justFuncName, enclosingFunction);
			return CCodeTriple("");
		case simple_passthrough:
            {
                // Stuff is bit more interesting now! XXX
                std::string pre_passthrough, post_passthrough;
                // Handle input/output parameters
                if (children.front()->getDataRef()->type == passthrough_params) {
                    auto optParamAssignLists = children.front()->getChildren();
                    for (auto in_or_out : optParamAssignLists) {
                        for (auto assign : in_or_out->getChildren()) {
                            auto assignChildren = assign->getChildren();
                            if (in_or_out->getDataRef()->type == in_passthrough_params)
                                if (assignChildren.size() == 2)
                                    pre_passthrough += ValueTypeToCType(assignChildren[0]->getDataRef()->valueType, assignChildren[1]->getDataRef()->symbol.getName()) + " = " + generate(assignChildren[0], enclosingObject, enclosingFunction).oneString() + ";\n";
                                else
                                    pre_passthrough += ValueTypeToCType(assignChildren[0]->getDataRef()->valueType, assignChildren[0]->getDataRef()->symbol.getName()) + " = " + generate(assignChildren[0], enclosingObject, enclosingFunction).oneString() + ";\n";
                            else if (in_or_out->getDataRef()->type == out_passthrough_params)
                                if (assignChildren.size() == 2)
                                    post_passthrough += generate(assignChildren[0], enclosingObject, justFuncName, enclosingFunction).oneString() + " = " + assignChildren[1]->getDataRef()->symbol.getName() + ";\n";
                                else
                                    post_passthrough += generate(assignChildren[0], enclosingObject, justFuncName, enclosingFunction).oneString() + " = " + assignChildren[0]->getDataRef()->symbol.getName() + ";\n";
                            else
                                linkerString += " " + strSlice(generate(in_or_out, enclosingObject, justFuncName, enclosingFunction).oneString(), 1, -2) + " ";
                        }
                    }
                }
                // The actual passthrough string is the last child now, as we might
                // have passthrough_params be the first child
                // we don't generate, as that will escape the returns and we don't want that. We'll just grab the string
                //return pre_passthrough + strSlice(generate(children.back(, enclosingFunction), enclosingObject, justFuncName).oneString(), 3, -4) + post_passthrough;
                return pre_passthrough + strSlice(children.back()->getDataRef()->symbol.getName(), 3, -4) + post_passthrough;
            }
		case function_call:
		{
			//NOTE: The first (0th) child of a function call node is the declaration of the function

			//Handle operators specially for now. Will later replace with
			//Inlined functions in the standard library
			// std::string name = data.symbol.getName();
			// std::cout << name << " == " << children[0]->getData().symbol.getName() << std::endl;
			std::string name = children[0]->getDataRef()->symbol.getName();
			ASTType funcType = children[0]->getDataRef()->type;

            // UGLLLLYYYY
            // But we have these here because some stuff has to be moved out of the giant nested blocks below and this is the way to do it
            CCodeTriple functionCallSource;
            bool doClosureInstead = false;

			//std::cout << "Doing function: " << name << std::endl;
			//Test for special functions only if what we're testing is, indeed, the definition, not a function call that returns a callable function pointer
			if (funcType == function) {
				if (name == "++" || name == "--")
					return generate(children[1], enclosingObject, true, enclosingFunction) + name;
				if ( (name == "*" || name == "&" || name == "!" || name == "-" || name == "+" ) && children.size() == 2) //Is dereference, not multiplication, address-of, or other unary operator
					return name + "(" + generate(children[1], enclosingObject, true, enclosingFunction) + ")";
				if (name == "[]")
					return "(" + generate(children[1], enclosingObject, true, enclosingFunction) + ")[" + generate(children[2],enclosingObject, true, enclosingFunction) + "]";
				if (name == "+" || name == "-" || name == "*" || name == "/" || name == "==" || name == ">=" || name == "<=" || name == "!="
					|| name == "<" || name == ">" || name == "%" || name == "=" || name == "+=" || name == "-=" || name == "*=" || name == "/=") {
					return "((" + generate(children[1], enclosingObject, true, enclosingFunction) + ")" + name + "(" + generate(children[2], enclosingObject, true, enclosingFunction) + "))";
                } else if (name == "&&" || name == "||") {
                    // b/c short circuiting, these have to be done seperately
                    CCodeTriple lhs = generate(children[1], enclosingObject, true, enclosingFunction);
                    CCodeTriple rhs = generate(children[2], enclosingObject, true, enclosingFunction);
                    output.preValue = lhs.preValue;
                    std::string shortcircuit_result = "shortcircuit_result" + getID();
                    output.preValue += "bool " + shortcircuit_result + " = " + lhs.value + ";\n";
                    output.preValue += lhs.postValue;
                    output.preValue += "if (" + std::string(name == "||" ? "!":"") + shortcircuit_result + ") { \n";
                    output.preValue += rhs.preValue;
                    output.preValue += shortcircuit_result + " = " + rhs.value + ";\n";
                    output.preValue += rhs.postValue;
                    output.preValue += "}\n";
                    output.value = shortcircuit_result;
                    return output;
                } else if (name == "." || name == "->") {
					if (children.size() == 1)
					 	return "/*dot operation with one child*/" + generate(children[0], enclosingObject, true, enclosingFunction).oneString() + "/*end one child*/";
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
								 //std::cout << "Decorating (in access-should be object) " << name << " " << functionDefChildren.size() << std::endl;
					 		    for (int i = 0; i < (functionDefChildren.size() > 0 ? functionDefChildren.size()-1 : 0); i++)
					 		    	nameDecoration += "_" + ValueTypeToCTypeDecoration(functionDefChildren[i]->getData().valueType);
                                // Note that we only add scoping to the object, as this specifies our member function too
/*HERE*/				 	    return function_header + scopePrefix(unaliasedTypeDef) + CifyName(unaliasedTypeDef->getDataRef()->symbol.getName()) +"__" +
                                        CifyName(functionName + nameDecoration) + "(" + (name == "." ? "&" : "") + generate(children[1], enclosingObject, true, enclosingFunction) + ",";
					 		    //The comma lets the upper function call know we already started the param list
					 		    //Note that we got here from a function call. We just pass up this special case and let them finish with the perentheses
                            } else {
					 	        std::cout << "Is not in scope or not type" << std::endl;
					            return "((" + generate(children[1], enclosingObject, true, enclosingFunction) + ")" + name + functionName + ")";
                            }
                        } else {
					 	    std::cout << "Is not in scope or not type" << std::endl;
					        return "((" + generate(children[1], enclosingObject, true, enclosingFunction) + ")" + name + functionName + ")";
					 	}
					} else {
						//return "((" + generate(children[1], enclosingObject, enclosingFunction) + ")" + name + generate(children[2], enclosingObject, enclosingFunction) + ")";
						return "((" + generate(children[1], enclosingObject, true, enclosingFunction) + ")" + name + generate(children[2], nullptr, true, enclosingFunction) + ")";
					}
				} else {
                    // this could a closure literal. sigh, I know.
                    if (children[0]->getDataRef()->closedVariables.size()) {
                        functionCallSource = generate(children[0], enclosingObject, true, enclosingFunction);
                        doClosureInstead = true;
                    } else {
                        //It's a normal function call, not a special one or a method or anything. Name decorate.
                        std::vector<NodeTree<ASTData>*> functionDefChildren = children[0]->getChildren();
                        //std::cout << "Decorating (none-special)" << name << " " << functionDefChildren.size() << std::endl;
                        std::string nameDecoration;
                        for (int i = 0; i < (functionDefChildren.size() > 0 ? functionDefChildren.size()-1 : 0); i++)
                            nameDecoration += "_" + ValueTypeToCTypeDecoration(functionDefChildren[i]->getData().valueType);
                        // it is possible that this is an object method from inside a closure
                        // in which case, recover the enclosing object from this
                        bool addClosedOver = false;
                        if (enclosingFunction && enclosingFunction->getDataRef()->closedVariables.size()) {
                            for (auto closedVar : enclosingFunction->getDataRef()->closedVariables) {
                                if (closedVar->getDataRef()->symbol.getName() == "this") {
                                    enclosingObject = closedVar->getDataRef()->valueType->typeDefinition;
                                    addClosedOver = true;
                                }
                            }
                        }
                        //Check to see if we're inside of an object and this is a method call
                        bool isSelfObjectMethod = enclosingObject && contains(enclosingObject->getChildren(), children[0]);
                        if (isSelfObjectMethod) {
                            output += function_header + scopePrefix(children[0]) + CifyName(enclosingObject->getDataRef()->symbol.getName()) +"__";
                            output += CifyName(name + nameDecoration) + "(";
                            output += std::string(addClosedOver ? "(*closed_variables->this)" : "this") + (children.size() > 1 ? "," : "");
                        } else {
                            output += function_header + scopePrefix(children[0]) + CifyName(name + nameDecoration) + "(";
                        }
                    }
				}
			} else {
				//This part handles cases where our definition isn't the function definition (that is, it is probabally the return from another function)
				//It's probabally the result of an access function call (. or ->) to access an object method.
                //OR a function value!
                //
                //THIS IS UUUUUGLLYYY too. We moved the closure part out to after the generation of the params becuase it needs to use them twice
				functionCallSource = generate(children[0], enclosingObject, true, enclosingFunction);
				if (functionCallSource.value[functionCallSource.value.size()-1] == ',') //If it's a member method, it's already started the parameter list.
					output += children.size() > 1 ? functionCallSource : CCodeTriple(functionCallSource.preValue, functionCallSource.value.substr(0, functionCallSource.value.size()-1), functionCallSource.postValue);
				else {
                    doClosureInstead = true;
                }
			}
            CCodeTriple parameters;
            // see if we should copy_construct / referencize all the parameters
			for (int i = 1; i < children.size(); i++) { //children[0] is the declaration
                Type* func_param_type = children[0]->getDataRef()->valueType->parameterTypes[i-1];
                // ok, if our param is a reference returned by another function, we don't actually want this type to be a reference if it is now.
                Type *param_type = children[i]->getDataRef()->valueType->withoutReference();
                // don't copy_construct references
                if (func_param_type->is_reference) {
                        parameters += "&" + generate(children[i], enclosingObject, true, enclosingFunction);
                } else if (methodExists(children[i]->getDataRef()->valueType, "copy_construct", std::vector<Type>{param_type->withIncreasedIndirection()})) {
                    std::string tmpParamName = "param" + getID();
                    CCodeTriple paramValue = generate(children[i], enclosingObject, true, enclosingFunction);
                    parameters.preValue += paramValue.preValue;
                    parameters.preValue += ValueTypeToCType(param_type, tmpParamName) + ";\n";
                    parameters.preValue += generateMethodIfExists(param_type, "copy_construct", "&"+tmpParamName + ", &" + paramValue.value, std::vector<Type>{children[i]->getDataRef()->valueType->withIncreasedIndirection()});
                    parameters.value += tmpParamName;
                    parameters.postValue += paramValue.postValue;
                } else {
                    parameters += generate(children[i], enclosingObject, true, enclosingFunction);
                }
				if (i < children.size()-1)
					parameters += ", ";
            }
            if (doClosureInstead) {
                Type* funcType = children[0]->getDataRef()->valueType;
                Type* retType = funcType->returnType;
                bool doRet = retType->baseType != void_type || retType->getIndirection();
                std::string tmpName = "functionValueTmp" + getID();
                std::string retTmpName = "closureRetTemp" + getID();
                output += CCodeTriple(parameters.preValue + functionCallSource.preValue + ValueTypeToCType(funcType, tmpName) + " = " + functionCallSource.value + ";\n"
                        + (doRet ? ValueTypeToCType(retType, retTmpName) + ";\n" : "")
                        + "if (" + tmpName + ".data) { " + (doRet ? (retTmpName + " =") : "") + " (("+ ValueTypeToCTypeDecoration(funcType,ClosureFunctionPointerTypeWithClosedParam)  +") (" + tmpName + ".func))(" + tmpName + ".data" + (children.size() > 1 ? ", " : "")  + parameters.value + "); }\n"
                        + "else { " + (doRet ? (retTmpName + " = ") : "") + " (("+ ValueTypeToCTypeDecoration(funcType,ClosureFunctionPointerTypeWithoutClosedParam)  +") (" + tmpName + ".func))(" + parameters.value + "); }\n",
                        (doRet ? retTmpName : ""),
                        parameters.postValue + functionCallSource.postValue);
            } else {
                output += parameters + ") ";
            }
            // see if we should add a destructer call to this postValue
			Type* retType = children[0]->getDataRef()->valueType->returnType;
            if (retType->baseType != void_type) {
                // we always use return temps now :( (for psudo-pod objects that still have methods called on them, like range(1,3).for_each(...)
                std::string retTempName = "return_temp" + getID();
                output.preValue += ValueTypeToCType(retType, retTempName) + " = " + output.value + ";\n";
                output.value = retTempName;
                if (retType->is_reference)
                    output.value = "(*" + output.value + ")";
                else if (methodExists(retType, "destruct", std::vector<Type>())) {
                    output.postValue = generateMethodIfExists(retType, "destruct", "&"+retTempName, std::vector<Type>()) + ";\n" + output.postValue;
                }
            }
			return output;
		}
		case value:
        {
            // ok, we now check for it being a string and escape all returns if it is (so that multiline strings work)
            if (data.symbol.getName()[0] == '"') {
                std::string innerString = strSlice(data.symbol.getName(), 0, 3) == "\"\"\""
                                            ? strSlice(data.symbol.getName(), 3, -4)
                                            : strSlice(data.symbol.getName(), 1, -2);
                std::string newStr;
                for (auto character: innerString)
                    if (character == '\n')
                        newStr += "\\n";
                    else if (character == '"')
                        newStr += "\\\"";
                    else
                        newStr += character;
                return "\"" + newStr + "\"";
            }
			return data.symbol.getName();
        }

		default:
			std::cout << "Nothing!" << std::endl;
	}
	for (int i = 0; i < children.size(); i++)
		output += generate(children[i], enclosingObject, justFuncName, enclosingFunction).oneString();

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
    distructDoubleStack.push_back(std::vector<NodeTree<ASTData>*>());

	ASTData data = from->getData();
	Type enclosingObjectType = *(enclosingObject->getDataRef()->valueType); //Copy a new type so we can turn it into a pointer if we need to
	enclosingObjectType.increaseIndirection();
	std::vector<NodeTree<ASTData>*> children = from->getChildren();
	std::string nameDecoration, parameters;
    if (!children.size()) {
        //problem
        std::cerr << " no children " << std::endl;
    }
	for (int i = 0; i < children.size()-1; i++) {
		parameters += ", " + ValueTypeToCType(children[i]->getData().valueType, generate(children[i]).oneString());
		nameDecoration += "_" + ValueTypeToCTypeDecoration(children[i]->getData().valueType);

        distructDoubleStack.back().push_back(children[i]);
	}
    std::string functionSignature = "\n" + ValueTypeToCType(data.valueType->returnType, function_header + scopePrefix(from) +  CifyName(enclosingObject->getDataRef()->symbol.getName()) +"__"
		+ CifyName(data.symbol.getName()) + nameDecoration) + "(" + ValueTypeToCType(&enclosingObjectType, "this") + parameters + ")";
    *functionPrototype += functionSignature + ";\n";
    // Note that we always wrap out child in {}, as we now allow one statement functions without a codeblock
    //
    std::string output;
    output += functionSignature + " {\n" +  generate(children.back(), enclosingObject, false, from).oneString();
    output += emitDestructors(reverse(distructDoubleStack.back()), enclosingObject);
    output += "}\n"; //Pass in the object so we can properly handle access to member stuff
    distructDoubleStack.pop_back();
    return output;
}

NodeTree<ASTData>* CGenerator::getMethod(Type* type, std::string method, std::vector<Type> types) {
    if (type->getIndirection())
        return nullptr;
    NodeTree<ASTData> *typeDefinition = type->typeDefinition;
    if (typeDefinition) {
        auto definitionItr = typeDefinition->getDataRef()->scope.find(method);
        if (definitionItr != typeDefinition->getDataRef()->scope.end()) {
            for (auto method : definitionItr->second) {
                bool methodFits = true;
                std::vector<Type> methodTypes = dereferenced(method->getDataRef()->valueType->parameterTypes);
                if (types.size() != methodTypes.size())
                    continue;
                for (int i = 0; i < types.size(); i++) {
                    // don't care about references
                    if (!types[i].test_equality(methodTypes[i], false)) {
                        methodFits = false;
                        break;
                    }
                }
                if (methodFits)
                    return method;
            }
        }
    }
    return nullptr;
}

bool CGenerator::methodExists(Type* type, std::string method, std::vector<Type> types) {
    return getMethod(type, method, types) != nullptr;
}

std::string CGenerator::generateMethodIfExists(Type* type, std::string method, std::string parameter, std::vector<Type> methodTypes) {
    NodeTree<ASTData> *methodDef = getMethod(type, method, methodTypes);
    if (methodDef) {
        NodeTree<ASTData> *typeDefinition = type->typeDefinition;
        std::string nameDecoration;
        for (Type *paramType : methodDef->getDataRef()->valueType->parameterTypes)
            nameDecoration += "_" + ValueTypeToCTypeDecoration(paramType);
        return function_header + scopePrefix(typeDefinition) + CifyName(typeDefinition->getDataRef()->symbol.getName()) + "__" + method + nameDecoration + "(" + parameter + ");\n";
    }
    return "";
}

std::string CGenerator::emitDestructors(std::vector<NodeTree<ASTData>*> identifiers, NodeTree<ASTData>* enclosingObject) {
    std::string destructorString = "";
    for (auto identifier : identifiers)
        if (!identifier->getDataRef()->valueType->is_reference)
            destructorString += tabs() + generateMethodIfExists(identifier->getDataRef()->valueType, "destruct", "&" + generate(identifier, enclosingObject).oneString(), std::vector<Type>());
    return destructorString;
}

std::string CGenerator::closureStructType(std::set<NodeTree<ASTData>*> closedVariables) {
    auto it = closureStructMap.find(closedVariables);
    if (it != closureStructMap.end())
        return it->second;
    std::string typedefString = "typedef struct { ";
    // note the increased indirection b/c we're using references to what we closed over
    for (auto var : closedVariables) {
        // unfortunatly we can't just do it with increased indirection b/c closing over function values
        // will actually change the underlying function's type. We cheat and just add a *
        //auto tmp = var->getDataRef()->valueType->withIncreasedIndirection();
        std::string varName = var->getDataRef()->symbol.getName();
        varName = (varName == "this") ? varName : scopePrefix(var) + varName;
        typedefString += ValueTypeToCType(var->getDataRef()->valueType, "*"+varName) + ";";
    }
    std::string structName = "closureStructType" + getID();
    typedefString += " } " + structName + ";\n";
    functionTypedefString += typedefString;
    closureStructMap[closedVariables] = structName;
    return structName;
}

std::string CGenerator::ValueTypeToCType(Type *type, std::string declaration, ClosureTypeSpecialType closureSpecial) { return ValueTypeToCTypeThingHelper(type, " " + declaration, closureSpecial); }
std::string CGenerator::ValueTypeToCTypeDecoration(Type *type, ClosureTypeSpecialType closureSpecial) { return CifyName(ValueTypeToCTypeThingHelper(type, "", closureSpecial)); }
std::string CGenerator::ValueTypeToCTypeThingHelper(Type *type, std::string declaration, ClosureTypeSpecialType closureSpecial) {
	std::string return_type;
    bool do_ending = true;
	switch (type->baseType) {
		case none:
			if (type->typeDefinition)
				return_type = scopePrefix(type->typeDefinition) + CifyName(type->typeDefinition->getDataRef()->symbol.getName());
			else
				return_type = "none";
			break;
		case function_type:
            {
                std::string indr_str;
                for (int i = 0; i < type->getIndirection(); i++)
                    indr_str += "*";

                auto it = functionTypedefMap.find(*type);
                if (it != functionTypedefMap.end()) {
                    if (closureSpecial == ClosureFunctionPointerTypeWithClosedParam)
                        return_type = it->second.second + declaration;
                    else if (closureSpecial == ClosureFunctionPointerTypeWithoutClosedParam)
                        return_type = it->second.third + declaration;
                    else
                        return_type = it->second.first + declaration;
                } else {
                    std::string typedefWithoutVoidStr  = "typedef ";
                    std::string typedefWithVoidStr  = "typedef ";
                    std::string typedefWithoutVoidID = "ID_novoid_" + CifyName(type->toString(false));
                    std::string typedefWithVoidID = "ID_withvoid_" + CifyName(type->toString(false));
                    std::string typedefStructID = "ID_struct_" + CifyName(type->toString(false));

                    // How I wish the world were this kind. Because of C name resolution not looking ahead, this definition needs to be BEFORE
                    // the object definitions. So to prevent circular dependencies, I'm making this take in a void pointer and we'll simply
                    // cast in both cases, whether or not there's a data pointer. Sigh.
                    //std::string typedefStructStr  = "typedef struct {" + typedefWithoutVoidID + " func; void* val; } " + typedefStructID + ";\n";
                    std::string typedefStructStr  = "typedef struct { void* func; void* data; } " + typedefStructID + ";\n";

                    typedefWithoutVoidStr += ValueTypeToCTypeThingHelper(type->returnType, "", closureSpecial);
                    typedefWithVoidStr += ValueTypeToCTypeThingHelper(type->returnType, "", closureSpecial);
                    typedefWithoutVoidStr += " (*" + typedefWithoutVoidID + ")(";
                    typedefWithVoidStr += " (*" + typedefWithVoidID + ")(";

                    typedefWithVoidStr += "void*";
                    if (type->parameterTypes.size() == 0)
                        typedefWithoutVoidStr += "void";
                    else
                        for (int i = 0; i < type->parameterTypes.size(); i++) {
                            typedefWithoutVoidStr += (i != 0 ? ", " : "") + ValueTypeToCTypeThingHelper(type->parameterTypes[i], "", closureSpecial);
                            typedefWithVoidStr += ", " + ValueTypeToCTypeThingHelper(type->parameterTypes[i], "", closureSpecial);
                        }
                    typedefWithoutVoidStr += ");\n";
                    typedefWithVoidStr += ");\n";
                    // again, sigh
                    functionTypedefString += typedefWithoutVoidStr;
                    functionTypedefString += typedefWithVoidStr;
                    functionTypedefStringPre += typedefStructStr;
                    //functionTypedefString += typedefStructStr;
                    if (closureSpecial == ClosureFunctionPointerTypeWithClosedParam)
                        return_type = typedefWithVoidID + indr_str + declaration;
                    else
                        return_type = typedefStructID + indr_str + declaration;

                    functionTypedefMap[*type] = make_triple(typedefStructID, typedefWithVoidID, typedefWithoutVoidID);
                }
                do_ending = false;
            }
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
    if (!do_ending)
        return return_type;
	for (int i = 0; i < type->getIndirection(); i++)
		return_type += "*";
    if (type->is_reference)
		return_type += " /*ref*/ *";
    return return_type + declaration;
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
											"!", "exlmtnpt",
											",", "comma",
											"=", "eq",
											"++", "dbplus",
											"--", "dbminus",
											"<<", "dbleft",
											">>", "dbright",
											"::", "scopeop",
											":", "colon",
											"==", "dbq",
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
											"<", "lt",
											">", "gt",
											">>=", "doubleleftequals",
											"(", "openparen",
											")", "closeparen",
											"[", "obk",
											"]", "cbk",
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
// Generate the scope prefix, that is "file_class_" for a method, etc
// What do we still need to handle? Packages! But we don't have thoes yet....
std::string CGenerator::scopePrefix(NodeTree<ASTData>* from) {
    //return "";
    std::string suffix = "_scp_";
    ASTData data = from->getData();
    if (data.type == translation_unit)
        return CifyName(data.symbol.getName()) + suffix;
    // so we do prefixing for stuff that c doesn't already scope:
    // different files. That's it for now. Methods are already lowered correctly with their parent object,
    // that parent object will get scoped. When we add a package system, we'll have to then add their scoping here
    return scopePrefix(from->getDataRef()->scope["~enclosing_scope"][0]);
}

