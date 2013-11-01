#include "ASTData.h"

ASTData::ASTData() {
	this->type = undef;
}

ASTData::ASTData(ASTType type, ValueType valueType) {
	this->type = type;
	this->valueType = valueType;
}

ASTData::ASTData(ASTType type, Symbol symbol, ValueType valueType) {
	this->type = type;
	this->valueType = valueType;
	this->symbol = symbol;
}


ASTData::~ASTData() {
}

std::string ASTData::toString() {
	return ASTTypeToString(type) + (symbol.isTerminal() ? " " + symbol.toString() : "") + (valueType ? " " + ValueTypeToString(valueType) : "");
}

ValueType ASTData::strToType(std::string type) {
	if (type == "bool") 
		return boolean;
	else if (type == "int")
		return integer;
	else if (type == "float")
		return floating;
	else if (type == "double")
		return double_percision;
	else if (type == "string")
		return char_string;
	else return none;
}

std::string ASTData::ValueTypeToString(ValueType type) {
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
			return "string";
			break;
		default:
			return "unknown_ValueType";
	}
}

std::string ASTData::ASTTypeToString(ASTType type) {
	switch (type) {
		case translation_unit:
			return "translation_unit";
			break;
		case interpreter_directive:
			return "interpreter_directive";
			break;
		case identifier:
			return "identifier";
			break;
		case import:
			return "import";
			break;
		case function:
			return "function";
			break;
		case code_block:
			return "code_block";
			break;
		case typed_parameter:
			return "typed_parameter";
			break;
		case expression:
			return "expression";
			break;
		case boolean_expression:
			return "boolean_expression";
			break;
		case statement:
			return "statement";
			break;
		case if_statement:
			return "if_statement";
			break;
		case return_statement:
			return "return_statement";
			break;
		case assignment_statement:
			return "assignment_statement";
			break;
		case declaration_statement:
			return "declaration_statement";
			break;
		case function_call:
			return "function_call";
			break;
		case value:
			return "value";
			break;
		default:
			return "unknown_ASTType";
	}
}
