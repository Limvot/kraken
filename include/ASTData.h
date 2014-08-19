#ifndef ASTDATA_H
#define ASTDATA_H

#include <vector>
#include <map>

#include "Symbol.h"
//Circular dependency
class Type;
#include "Type.h"

#ifndef NULL
#define NULL ((void*)0)
#endif

enum ASTType {undef, translation_unit, interpreter_directive, import, identifier, type_def,
	function, code_block, typed_parameter, expression, boolean_expression, statement,
	if_statement, while_loop, for_loop, return_statement, assignment_statement, declaration_statement,
	if_comp, simple_passthrough, function_call, value};

class ASTData {
	public:
		ASTData();
		ASTData(ASTType type, Type *valueType = NULL);
		ASTData(ASTType type, Symbol symbol, Type *valueType = NULL);
		~ASTData();
		std::string toString();
		static std::string ASTTypeToString(ASTType type);
		ASTType type;
		Type* valueType;
		Symbol symbol;
		std::map<std::string, std::vector<NodeTree<ASTData>*>> scope;
	private:

};

#endif
