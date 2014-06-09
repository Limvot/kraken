#ifndef TYPE_H
#define TYPE_H

#ifndef NULL
#define NULL ((void*)0)
#endif

#ifndef SEGFAULT
#define SEGFAULT (*((char*)0)), std::cout << "\t\t\t\tNEGATIVE*************************************************************************" << std::endl;
#endif

#include <string>
#include <iostream>

//Circular dependency
class ASTData;
#include "ASTData.h"
#include "util.h"

enum ValueType {none, template_type, template_type_type, void_type, boolean, integer, floating, double_percision, character };


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
		int getIndirection();
		void setIndirection(int indirectionIn);
		void increaseIndirection();
		void decreaseIndirection();
		void modifyIndirection(int mod);
		void check();

        ValueType baseType;
		NodeTree<ASTData>* typeDefinition;
		NodeTree<Symbol>* templateDefinition;
        std::map<std::string, Type*> templateTypeReplacement;
	private:
		int indirection;
};

#endif
