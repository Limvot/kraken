#include "ASTData.h"

ASTData::ASTData() {
	this->type = undef;
	this->valueType = NULL;
}

ASTData::ASTData(ASTType type, Type *valueType) {
	this->type = type;
	this->valueType = valueType;
}

ASTData::ASTData(ASTType type, Symbol symbol, Type *valueType) {
	this->type = type;
	this->valueType = valueType;
	this->symbol = symbol;
}


ASTData::~ASTData() {
}

std::string ASTData::toString() {
	return 	ASTTypeToString(type) + " " +
			(symbol.isTerminal() ? " " + symbol.toString() : "") + " " +
			(valueType ? valueType->toString() : "no_type");
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
		case type_def:
			return "type_def";
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
		case passthrough_params:
			return "passthrough_params";
		case function_call:
			return "function_call";
		case value:
			return "value";
		default:
			return "unknown_ASTType";
	}
}
