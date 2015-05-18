#ifndef TYPE_H
#define TYPE_H

#ifndef NULL
#define NULL ((void*)0)
#endif

#include <string>
#include <iostream>
#include <set>

//Circular dependency
class ASTData;
#include "ASTData.h"
#include "util.h"

enum ValueType {none, template_type, template_type_type, void_type, boolean, integer, floating, double_percision, character, function_type };


class Type {
	public:
		Type();
		Type(ValueType typeIn, int indirectionIn = 0);
        Type(ValueType typeIn, std::set<std::string> traitsIn); //Mostly for template type type's
		Type(NodeTree<ASTData>* typeDefinitionIn, int indirectionIn = 0);
		Type(NodeTree<ASTData>* typeDefinitionIn, std::set<std::string> traitsIn);
		Type(ValueType typeIn, NodeTree<ASTData>* typeDefinitionIn, int indirectionIn, std::set<std::string> traitsIn);
		Type(ValueType typeIn, NodeTree<ASTData>* typeDefinitionIn, int indirectionIn, std::set<std::string> traitsIn, std::vector<Type*> parameterTypesIn, Type* returnTypeIn);
		Type(std::vector<Type*> parameterTypesIn, Type* returnTypeIn);
		Type(ValueType typeIn, NodeTree<Symbol>* templateDefinitionIn, std::set<std::string> traitsIn = std::set<std::string>());
		~Type();
		bool const operator==(const Type &other)const;
		bool const operator!=(const Type &other)const;
		Type* clone();
		std::string toString(bool showTraits = true);
		int getIndirection();
		void setIndirection(int indirectionIn);
		void increaseIndirection();
		void decreaseIndirection();
		void modifyIndirection(int mod);

        ValueType baseType;
		NodeTree<ASTData>* typeDefinition;
		NodeTree<Symbol>* templateDefinition;
        std::map<std::string, Type*> templateTypeReplacement;
        std::set<std::string> traits;
        std::vector<Type*> parameterTypes;
        Type *returnType;
    private:
		int indirection;
};

#endif
