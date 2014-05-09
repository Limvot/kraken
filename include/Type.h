#ifndef TYPE_H
#define TYPE_H

#ifndef NULL
#define NULL ((void*)0)
#endif

#include <string>
#include <iostream>

//Circular dependency
class ASTData;
#include "ASTData.h"
#include "util.h"

enum ValueType {none, template_type, void_type, boolean, integer, floating, double_percision, character };


class Type {
	public:
		Type();
		Type(ValueType typeIn, int indirectionIn);
		Type(ValueType typeIn);
		Type(NodeTree<ASTData>* typeDefinitionIn);
		Type(NodeTree<ASTData>* typeDefinitionIn, int indirectionIn);
		Type(ValueType typeIn, NodeTree<ASTData>* typeDefinitionIn, int indirectionIn);
		Type(ValueType typeIn, NodeTree<Symbol>* templateDefinitionIn);
		~Type();
		bool const operator==(const Type &other)const;
		bool const operator!=(const Type &other)const;
		Type* clone();
		std::string toString();
		ValueType baseType;
		NodeTree<ASTData>* typeDefinition;
		int indirection;
		NodeTree<Symbol>* templateDefinition;
	private:
};

#endif