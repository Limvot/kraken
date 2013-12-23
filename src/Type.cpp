#include "Type.h"

Type::Type() {
	indirection = 0;
	baseType = none;
}

Type::Type(ValueType typeIn) {
	indirection = 0;
	baseType = typeIn;
}

Type::Type(ValueType typeIn, int indirectionIn) {
	indirection = indirectionIn;
	baseType = typeIn;
}

Type::Type(std::string typeIn) {
	indirection = 0;
	while (typeIn[typeIn.size() - indirection - 1] == '*') indirection++;
	std::string edited = strSlice(typeIn, 0, -(indirection + 1));
	if (edited == "void")
		baseType = void_type;
	else if (edited == "bool")
		baseType = boolean;
	else if (edited == "int")
		baseType = integer;
	else if (edited == "float")
		baseType = floating;
	else if (edited == "double")
		baseType = double_percision;
	else if (edited == "char")
		baseType = character;
	else
		baseType = none;
	std::cout << ":ALKJF:LSKDJF:SDJF:LKSJDF\t\t\t" << typeIn << "\t" << edited << std::endl;
}


Type::~Type() {
}

std::string Type::toString() {
	std::string typeString;
	switch (baseType) {
		case none:
			typeString = "none";
			break;
		case void_type:
			typeString = "void";
			break;
		case boolean:
			typeString = "bool";
			break;
		case integer:
			typeString = "int";
			break;
		case floating:
			typeString = "float";
			break;
		case double_percision:
			typeString = "double";
			break;
		case character:
			typeString = "char";
			break;
		default:
			typeString = "unknown_type";
	}
	for (int i = 0; i < indirection; i++)
		typeString += "*";
	return typeString;
}
