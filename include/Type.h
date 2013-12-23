#ifndef TYPE_H
#define TYPE_H

#ifndef NULL
#define NULL 0
#endif

#include <string>
#include <iostream>

#include "util.h"

enum ValueType {none, void_type, boolean, integer, floating, double_percision, character };


class Type {
	public:
		Type();
		Type(ValueType typeIn, int indirectionIn);
		Type(ValueType typeIn);
		Type(std::string typeIn);
		~Type();
		std::string toString();
		ValueType baseType;
		int indirection;
	private:
};

#endif