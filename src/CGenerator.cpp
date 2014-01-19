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

std::string CGenerator::generate(NodeTree<ASTData>* from) {
	ASTData data = from->getData();
	std::vector<NodeTree<ASTData>*> children = from->getChildren();
		std::string output = "";
	switch (data.type) {
		case translation_unit:
			//Do here because we may need the typedefs before the declarations of variables
			for (int i = 0; i < children.size(); i++)
				if (children[i]->getDataRef()->type == type_def)
					output += generate(children[i]) + "\n";
			//Declare everything in translation unit scope here. (allows stuff from other files, automatic forward declarations)
			for (auto i = data.scope.begin(); i != data.scope.end(); i++) {
				NodeTree<ASTData>* declaration = i->second;
				std::vector<NodeTree<ASTData>*> decChildren = declaration->getChildren();
				ASTData declarationData = i->second->getData();
				switch(declarationData.type) {
					case identifier:
						output += ValueTypeToCType(declarationData.valueType) + " " + declarationData.symbol.getName() + "; /*identifier*/\n";
						break;
					case function:
						output += "\n" + ValueTypeToCType(declarationData.valueType) + " " + declarationData.symbol.getName() + "(";
						for (int j = 0; j < decChildren.size()-1; j++) {
							if (j > 0)
								output += ", ";
							output += ValueTypeToCType(decChildren[j]->getData().valueType) + " " + generate(decChildren[j]);
						}
						output += "); /*func*/\n";
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
			//Do here because we need the newlines
			for (int i = 0; i < children.size(); i++)
				if (children[i]->getDataRef()->type != type_def)
					output += generate(children[i]) + "\n";
			return output;
			break;
		case interpreter_directive:
			//Do nothing
			break;
		case import:
			return "/* would import \"" + data.symbol.getName() + "\" but....*/\n";
			//return "#include <" + data.symbol.getName() + ">\n";
		case identifier:
			return data.symbol.getName();
		case type_def:
			if (children.size() == 0) {
				return "typedef " + ValueTypeToCType(data.valueType) + " " + data.symbol.getName() + ";";
			} else {
				std::string objectString = "typedef struct __struct_dummy_" + data.symbol.getName() + "__ {\n";
				for (int i = 0; i < children.size(); i++)
					objectString += generate(children[i]) + "\n";
				objectString += "} " + data.symbol.getName() + ";";
				return objectString;
			}
		case function:
			output += "\n" + ValueTypeToCType(data.valueType) + " " + data.symbol.getName() + "(";
			for (int i = 0; i < children.size()-1; i++) {
				if (i > 0)
					output += ", ";
				output += ValueTypeToCType(children[i]->getData().valueType) + " " + generate(children[i]);
			}
			output+= ")\n" + generate(children[children.size()-1]);
			return output;
		case code_block:
			output += "{\n";
			tabLevel++;
			for (int i = 0; i < children.size(); i++)
				output += generate(children[i]);
			tabLevel--;
			output += tabs() + "}";
			return output;
		case expression:
			output += " " + data.symbol.getName() + ", ";
		case boolean_expression:
			output += " " + data.symbol.getName() + " ";
		case statement:
			return tabs() + generate(children[0]) + ";\n";
		case if_statement:
			output += "if (" + generate(children[0]) + ")\n\t" + generate(children[1]);
			if (children.size() > 2)
				output += " else " + generate(children[2]);
			return output;
		case while_loop:
			output += "while (" + generate(children[0]) + ")\n\t" + generate(children[1]);
			return output;
		case for_loop:
			//The strSlice's are there to get ride of an unwanted return and an unwanted semicolon(s)
			output += "for (" + strSlice(generate(children[0]),0,-3) + generate(children[1]) + ";" + strSlice(generate(children[2]),0,-3) + ")\n\t" + generate(children[3]);
			return output;
		case return_statement:
			if (children.size())
				return "return " + generate(children[0]);
			else
				return "return";
		case assignment_statement:
			return generate(children[0]) + " = " + generate(children[1]);
		case declaration_statement:
			if (children.size() == 1)
				return ValueTypeToCType(children[0]->getData().valueType) + " " + generate(children[0]) + ";";
			else
				return ValueTypeToCType(children[0]->getData().valueType) + " " + generate(children[0]) + " = " + generate(children[1]) + ";";
		case if_comp:
			if (generate(children[0]) == generatorString)
				return generate(children[1]);
			return "";
		case simple_passthrough:
			return strSlice(generate(children[0]), 3, -4);
		case function_call:
		{
			//NOTE: The first (0th) child of a function call node is the declaration of the function

			//Handle operators specially for now. Will later replace with
			//Inlined functions in the standard library
			std::string name = data.symbol.getName();
			//std::cout << name << " == " << children[0]->getData().symbol.getName() << std::endl;
			if (name == "++" || name == "--")
				return generate(children[1]) + name;
			if (name == "*" && children.size() == 2) //Is dereference, not multiplication
				return "*(" + generate(children[1]) + ")";
			if (name == "+" || name == "-" || name == "*" || name == "/" || name == "==" || name == ">=" || name == "<=" || name == "!="
				|| name == "<" || name == ">" || name == "%" || name == "+=" || name == "-=" || name == "*=" || name == "/=" || name == "||"
				|| name == "&&" || name == "!" )
				return "((" + generate(children[1]) + ")" + name + "(" + generate(children[2]) + "))";
			else if (name == "." || name == "->")
				return "((" + generate(children[1]) + ")" + name + generate(children[2]) + ")";
			output += data.symbol.getName() + "(";
			for (int i = 1; i < children.size(); i++) //children[0] is the declaration
				if (i < children.size()-1)
					output += generate(children[i]) + ", ";
				else output += generate(children[i]);
			output += ") ";
			return output;
		}
		case value:
			return data.symbol.getName();

		default:
			std::cout << "Nothing!" << std::endl;
	}
	for (int i = 0; i < children.size(); i++)
		output += generate(children[i]);

	return output;
}

std::string CGenerator::ValueTypeToCType(Type *type) {
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
	for (int i = 0; i < type->indirection; i++)
		return_type += "*";
	return return_type;
}
