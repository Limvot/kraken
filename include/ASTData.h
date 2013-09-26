#ifndef ASTDATA_H
#define ASTDATA_H

#ifndef NULL
#define NULL 0
#endif

#include "Symbol.h"

#include <vector>

class ASTData {
	public:
		enum ASTType {translation_unit, interpreter_directive, identifier,
					import, interpreter_directive, function, code_block,
					typed_parameter, expression, boolean_expression, statement,
					if_statement, return_statement, assignment_statement, function_call,
					value};
		enum ValueType {none, boolean, integer, floating, double_percision, char_string }

		ASTData(ASTType type, ValueType valueType = none);
		ASTData(ASTType type, Symbol* symbol, ValueType valueType = none);
		ASTType type;
		Symbol* symbol;
	private:

};

#endif