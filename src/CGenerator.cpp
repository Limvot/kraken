#include "CGenerator.h"

CGenerator::CGenerator() : generatorString("__C__") {
	tabLevel = 0;
}
CGenerator::~CGenerator() {

}

void CGenerator::generateCompSet(std::map<std::string, NodeTree<ASTData>*> ASTs, std::string outputName) {
	//Generate an entire set of files
	std::string buildString = "#!/bin/sh\ncc -std=c99 ";
	for (auto i = ASTs.begin(); i != ASTs.end(); i++) {
		buildString += i->first + ".c ";
		std::ofstream outputCFile;
		outputCFile.open(i->first + ".c");
		if (outputCFile.is_open()) {
			outputCFile << generate(i->second);
		} else {
			std::cout << "Cannot open file " << i->first << ".c" << std::endl;
		}
		outputCFile.close();
	}
	buildString += "-o " + outputName;
	std::ofstream outputBuild;
	outputBuild.open(outputName + ".sh");
	outputBuild << buildString;
	outputBuild.close();
}

std::string CGenerator::tabs() {
	std::string returnTabs;
	for (int i = 0; i < tabLevel; i++)
		returnTabs += "\t";
	return returnTabs;
}

//The enclosing object is for when we're generating the inside of object methods. They allow us to check scope lookups against the object we're in
std::string CGenerator::generate(NodeTree<ASTData>* from, NodeTree<ASTData>* enclosingObject) {
	ASTData data = from->getData();
	std::vector<NodeTree<ASTData>*> children = from->getChildren();
	std::string output = "";
	switch (data.type) {
		case translation_unit:
			//Do here because we may need the typedefs before the declarations of variables
			//Note that we need to be careful of the order, though, as some declarations depend on others.
			//What is this then? It's a poset! Wooo posets!
		{
			Poset<NodeTree<ASTData>*> typedefPoset;
			for (int i = 0; i < children.size(); i++) {
				if (children[i]->getDataRef()->type == type_def) {
					typedefPoset.addVertex(children[i]); //We add this definition by itself just in case there are no dependencies.
													//If it has dependencies, there's no harm in adding it here
					//Go through every child in the class looking for declaration statements. For each of these that is not a primitive type
					//we will add a dependency from this definition to that definition in the poset.
					std::vector<NodeTree<ASTData>*> classChildren = children[i]->getChildren();
					for (auto j : classChildren) {
						if (j->getDataRef()->type == declaration_statement) {
							Type* decType = j->getChildren()[0]->getDataRef()->valueType; //Type of the declaration
							if (decType->typeDefinition && decType->getIndirection() == 0)	//If this is a custom type and not a pointer
								typedefPoset.addRelationship(children[i], decType->typeDefinition); //Add a dependency
						}
					}
					//In case there are pointer dependencies. If the typedef has no children, then it is a simple renaming and we don't need to predeclare the class (maybe?)
					if (classChildren.size())
						output += "struct " + CifyName(children[i]->getDataRef()->symbol.getName()) + ";\n";
                    else if (children[i]->getDataRef()->valueType->typeDefinition != children[i] && !children[i]->getDataRef()->valueType->templateDefinition) //Isn't uninstantiated template or 0 parameter class, so must be alias
                        typedefPoset.addRelationship(children[i], children[i]->getDataRef()->valueType->typeDefinition); //An alias typedef depends on the type it aliases being declared before it
				}
			}
			//Now generate the typedef's in the correct, topological order
			for (NodeTree<ASTData>* i : typedefPoset.getTopoSort())
				output += generate(i, enclosingObject) + "\n";

			//Declare everything in translation unit scope here. (allows stuff from other files, automatic forward declarations)
			for (auto i = data.scope.begin(); i != data.scope.end(); i++) {
				for (auto overloadedMembers : i->second) {
					NodeTree<ASTData>* declaration = overloadedMembers;
					std::vector<NodeTree<ASTData>*> decChildren = declaration->getChildren();
					ASTData declarationData = declaration->getData();
					switch(declarationData.type) {
						case identifier:
							output += ValueTypeToCType(declarationData.valueType) + " " + declarationData.symbol.getName() + "; /*identifier*/\n";
							break;
						case function:
						{
							if (declarationData.valueType->baseType == template_type)
								output += "/* template function " + declarationData.symbol.toString() + " */\n";
							else if (decChildren.size() == 0) //Not a real function, must be a built in passthrough
								output += "/* built in function: " + declarationData.symbol.toString() + " */\n";
							else {
								output += "\n" + ValueTypeToCType(declarationData.valueType) + " ";
								std::string nameDecoration, parameters;
								for (int j = 0; j < decChildren.size()-1; j++) {
									if (j > 0)
										parameters += ", ";
									parameters += ValueTypeToCType(decChildren[j]->getData().valueType) + " " + generate(decChildren[j], enclosingObject);
									nameDecoration += "_" + ValueTypeToCTypeDecoration(decChildren[j]->getData().valueType);
								}
								output += CifyName(declarationData.symbol.getName() + nameDecoration) + "(" + parameters + "); /*func*/\n";
							}
						}
							break;
						case type_def:
							//type
							output += "/*typedef " + declarationData.symbol.getName() + " */\n";
							break;
						default:
							//std::cout << "Declaration? named " << declaration->getName() << " of unknown type " << ASTData::ASTTypeToString(declarationData.type) << " in translation unit scope" << std::endl;
							output += "/*unknown declaration named " + declaration->getName() + "*/\n";
					}
				}
			}
			//Do here because we need the newlines
			for (int i = 0; i < children.size(); i++)
				if (children[i]->getDataRef()->type != type_def)
					output += generate(children[i], enclosingObject) + "\n";
			return output;
		}
			break;
		case interpreter_directive:
			//Do nothing
			break;
		case import:
			return "/* would import \"" + data.symbol.getName() + "\" but....*/\n";
			//return "#include <" + data.symbol.getName() + ">\n";
		case identifier:
		{
			//If we're in an object method, and our enclosing scope is that object, we're a member of the object and should use the self reference.
			std::string preName;
			if (enclosingObject && enclosingObject->getDataRef()->scope.find(data.symbol.getName()) != enclosingObject->getDataRef()->scope.end())
				preName += "self->";
			if (false)
				for (int j = 0; j < children.size()-1; j++)
					preName += ValueTypeToCType(children[j]->getData().valueType) + "_";
			return preName + CifyName(data.symbol.getName()); //Cifying does nothing if not an operator overload
		}
		case type_def:
			if (data.valueType->baseType == template_type) {
				return "/* non instantiated template " + data.symbol.getName() + " */";
			} else if (children.size() == 0) {
				return "typedef " + ValueTypeToCType(data.valueType) + " " + data.symbol.getName() + ";";
			} else {
				std::string objectString = "typedef struct __struct_dummy_" + CifyName(data.symbol.getName()) + "__ {\n";
				std::string postString; //The functions have to be outside the struct definition
				tabLevel++;
				for (int i = 0; i < children.size(); i++) {
					std::cout << children[i]->getName() << std::endl;
					if (children[i]->getName() == "function") //If object method
						postString += generateObjectMethod(from, children[i]) + "\n";
					else
						objectString += tabs() + generate(children[i], enclosingObject) + "\n";
				}
				tabLevel--;
				objectString += "} " + CifyName(data.symbol.getName()) + ";";
				return objectString + postString; //Functions come after the declaration of the struct
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
			output += "{\n";
			tabLevel++;
			for (int i = 0; i < children.size(); i++) {
				//std::cout << "Line " << i << std::endl;
				std::string line = generate(children[i], enclosingObject);
				//std::cout << line << std::endl;
				output += line;
			}
			tabLevel--;
			output += tabs() + "}";
			return output;
		case expression:
			output += " " + data.symbol.getName() + ", ";
		case boolean_expression:
			output += " " + data.symbol.getName() + " ";
		case statement:
			return tabs() + generate(children[0], enclosingObject) + ";\n";
		case if_statement:
			output += "if (" + generate(children[0], enclosingObject) + ")\n\t" + generate(children[1], enclosingObject);
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
			else
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
				if ( (name == "*" || name == "&") && children.size() == 2) //Is dereference, not multiplication, or address-of
					return name + "(" + generate(children[1], enclosingObject) + ")";
				if (name == "[]")
					return "(" + generate(children[1], enclosingObject) + ")[" +generate(children[2],enclosingObject) + "]";
				if (name == "+" || name == "-" || name == "*" || name == "/" || name == "==" || name == ">=" || name == "<=" || name == "!="
					|| name == "<" || name == ">" || name == "%" || name == "+=" || name == "-=" || name == "*=" || name == "/=" || name == "||"
					|| name == "&&" || name == "!" )
					return "((" + generate(children[1], enclosingObject) + ")" + name + "(" + generate(children[2], enclosingObject) + "))";
				else if (name == "." || name == "->") {
					if (children.size() == 1)
					 	return "/*dot operation with one child*/" + generate(children[0], enclosingObject) + "/*end one child*/";
					 //If this is accessing an actual function, find the function in scope and take the appropriate action. Probabally an object method
					 if (children[2]->getDataRef()->type == function) {
					 	std::string functionName = children[2]->getDataRef()->symbol.getName();
					 	NodeTree<ASTData>* possibleObjectType = children[1]->getDataRef()->valueType->typeDefinition;
					 	//If is an object method, generate it like one. Needs extension/modification for inheritence
					 	if (possibleObjectType && possibleObjectType->getDataRef()->scope.find(functionName) != possibleObjectType->getDataRef()->scope.end()) {
					 		std::string nameDecoration;
					 		std::vector<NodeTree<ASTData>*> functionDefChildren = children[2]->getChildren(); //The function def is the rhs of the access operation
					 		std::cout << "Decorating (in access-should be object) " << name << " " << functionDefChildren.size() << std::endl;
					 		for (int i = 0; i < (functionDefChildren.size() > 0 ? functionDefChildren.size()-1 : 0); i++)
					 			nameDecoration += "_" + ValueTypeToCTypeDecoration(functionDefChildren[i]->getData().valueType);
/*HERE*/				 	return CifyName(possibleObjectType->getDataRef()->symbol.getName()) +"__" + CifyName(functionName + nameDecoration) + "(" + (name == "." ? "&" : "") + generate(children[1], enclosingObject) + ",";
					 		//The comma lets the upper function call know we already started the param list
					 		//Note that we got here from a function call. We just pass up this special case and let them finish with the perentheses
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
						output += enclosingObject->getDataRef()->symbol.getName() +"__";
/*HERE*/			output += CifyName(name + nameDecoration) + "(";
				 	if (isSelfObjectMethod)
				 		output += children.size() > 1 ? "self," : "self";
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

std::string CGenerator::generateObjectMethod(NodeTree<ASTData>* enclosingObject, NodeTree<ASTData>* from) {
	std::string output;
	ASTData data = from->getData();
	Type enclosingObjectType = *(enclosingObject->getDataRef()->valueType); //Copy a new type so we can turn it into a pointer if we need to
	enclosingObjectType.increaseIndirection();
	std::vector<NodeTree<ASTData>*> children = from->getChildren();
	std::string nameDecoration, parameters;
	for (int i = 0; i < children.size()-1; i++) {
		parameters += ", " + ValueTypeToCType(children[i]->getData().valueType) + " " + generate(children[i]);
		nameDecoration += "_" + ValueTypeToCTypeDecoration(children[i]->getData().valueType);
	}
	output += "\n" + ValueTypeToCType(data.valueType) + " " + CifyName(enclosingObject->getDataRef()->symbol.getName()) +"__"
		+ CifyName(data.symbol.getName()) + nameDecoration + "(" + ValueTypeToCType(&enclosingObjectType)
		+ " self" + parameters + ")\n" + generate(children[children.size()-1], enclosingObject); //Pass in the object so we can properly handle access to member stuff
	return output;
}

std::string CGenerator::ValueTypeToCType(Type *type) {
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
		return_type += "*";
	return return_type;
}

std::string CGenerator::ValueTypeToCTypeDecoration(Type *type) {
	std::string return_type;
	switch (type->baseType) {
		case none:
			if (type->typeDefinition)
				return_type = type->typeDefinition->getDataRef()->symbol.getName();
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
		return_type += "_P__";
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

