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
	if (type == "void")
		return void_type;
	else if (type == "bool") 
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
		case void_type:
			return "void";
		case boolean:
			return "bool";
		case integer:
			return "int";
		case floating:
			return "float";
		case double_percision:
			return "double";
		case char_string:
			return "string";
		default:
			return "unknown_ValueType";
	}
}

std::string ASTData::ASTTypeToString(ASTType type) {
	switch (type) {
		case translation_unit:
			return "translation_unit";
		case interpreter_directive:
			return "interpreter_directive";
		case identifier:
			return "identifier";
		case import:
			return "import";
		case function:
			return "function";
		case code_block:
			return "code_block";
		case typed_parameter:
			return "typed_parameter";
		case expression:
			return "expression";
		case boolean_expression:
			return "boolean_expression";
		case statement:
			return "statement";
		case if_statement:
			return "if_statement";
		case while_loop:
			return "while_loop";
		case for_loop:
			return "for_loop";
		case return_statement:
			return "return_statement";
		case assignment_statement:
			return "assignment_statement";
		case declaration_statement:
			return "declaration_statement";
		case if_comp:
			return "if_comp";
		case simple_passthrough:
			return "simple_passthrough";
		case function_call:
			return "function_call";
		case value:
			return "value";
		default:
			return "unknown_ASTType";
	}
}
