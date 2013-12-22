#ifndef ASTDATA_H
#define ASTDATA_H

#include <vector>
#include <set>
#include "Symbol.h"

#ifndef NULL
#define NULL 0
#endif

enum ASTType {undef, translation_unit, interpreter_directive, import, identifier,
	function, code_block,
	typed_parameter, expression, boolean_expression, statement,
	if_statement, while_loop, for_loop, return_statement, assignment_statement, declaration_statement,
	if_comp, simple_passthrough, function_call, value};
enum ValueType {none, void_type, boolean, integer, floating, double_percision, char_string };


class ASTData {
	public:
		ASTData();
		ASTData(ASTType type, ValueType valueType = none);
		ASTData(ASTType type, Symbol symbol, ValueType valueType = none);
		~ASTData();
		std::string toString();
		static std::string ASTTypeToString(ASTType type);
		static std::string ValueTypeToString(ValueType type);
		static ValueType strToType(std::string type);
		ASTType type;
		ValueType valueType;
		Symbol symbol;
	private:

};

#endif