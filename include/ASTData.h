#ifndef ASTDATA_H
#define ASTDATA_H

#include <vector>
#include <set>

#include "Symbol.h"
#include "Type.h"

#ifndef NULL
#define NULL 0
#endif

enum ASTType {undef, translation_unit, interpreter_directive, import, identifier,
	function, code_block,
	typed_parameter, expression, boolean_expression, statement,
	if_statement, while_loop, for_loop, return_statement, assignment_statement, declaration_statement,
	if_comp, simple_passthrough, function_call, value};

class ASTData {
	public:
		ASTData();
		ASTData(ASTType type, Type valueType = Type());
		ASTData(ASTType type, Symbol symbol, Type valueType = Type());
		~ASTData();
		std::string toString();
		static std::string ASTTypeToString(ASTType type);
		ASTType type;
		Type valueType;
		Symbol symbol;
	private:

};

#endif