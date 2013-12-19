#include "CGenerator.h"

CGenerator::CGenerator() {
	tabLevel = 0;
}
CGenerator::~CGenerator() {

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
			//Do nothing
			break;
		case interpreter_directive:
			//Do nothing
			break;
		case import:
			return "#include <" + data.symbol.getName() + ">\n";
		case identifier:
			return data.symbol.getName();
		case function:
			output += "\n" + ValueTypeToCType(data.valueType) + " " + data.symbol.getName() + "(";
			for (int i = 0; i < children.size()-1; i++) {
				if (i > 0)
					output += ", ";
				output += ASTData::ValueTypeToString(children[i]->getData().valueType) + " " + generate(children[i]);
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
			//The strSlice's are there to get ride of an unwanted return and an unwanted semicolon
			output += "for (" + strSlice(generate(children[0]),0,-2) + generate(children[1]) + ";" + strSlice(generate(children[2]),0,-3) + ")\n\t" + generate(children[3]);
			return output;
		case return_statement:
			return "return " + generate(children[0]);
		case assignment_statement:
			return generate(children[0]) + " = " + generate(children[1]);
		case declaration_statement:
			return ASTData::ValueTypeToString(children[0]->getData().valueType) + " " + generate(children[0]) + " = " + generate(children[1]);
		case function_call:
		{
			//Handle operators specially for now. Will later replace with
			//Inlined functions in the standard library
			std::string name = data.symbol.getName();
			if (name == "++" || name == "--")
				return generate(children[0]) + name;
			if (name == "+" || name == "-" || name == "*" || name == "/" || name == "==" || name == ">=" || name == "<=" || name == "!=" || name == "<" || name == ">" || name == "%" || name == "+=" || name == "-=" || name == "*=" || name == "/=") {
				return "((" + generate(children[0]) + ")" + name + "(" + generate(children[1]) + "))";
			}
			output += data.symbol.getName() + "(";
			for (int i = 0; i < children.size(); i++)
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

std::string CGenerator::ValueTypeToCType(ValueType type) {
	switch (type) {
		case none:
			return "none";
			break;
		case boolean:
			return "bool";
			break;
		case integer:
			return "int";
			break;
		case floating:
			return "float";
			break;
		case double_percision:
			return "double";
			break;
		case char_string:
			return "char*";
			break;
		default:
			return "unknown_ValueType";
	}
}
