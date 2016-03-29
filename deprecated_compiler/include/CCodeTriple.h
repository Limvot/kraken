#ifndef CCODETRIPLE_H
#define CCODETRIPLE_H

#include <string>
#include <iostream>
#include "util.h"

class CCodeTriple {
	public:
        CCodeTriple(std::string pre, std::string val, std::string post);
        CCodeTriple(std::string val);
        CCodeTriple(const char* val);
		CCodeTriple();
		~CCodeTriple();
        std::string oneString(bool endValue = false);
        CCodeTriple & operator=(const CCodeTriple &rhs);
        CCodeTriple & operator+=(const CCodeTriple &rhs);

		std::string preValue;
        std::string value;
        std::string postValue;
	private:
};
CCodeTriple operator+(const CCodeTriple &a, const CCodeTriple &b);
#endif //CCODETRIPLE_H
