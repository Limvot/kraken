#ifndef SYMBOL_H
#define SYMBOL_H

#ifndef NULL
#define NULL 0
#endif

#include <vector>
#include <string>

class Symbol {
	public:
		Symbol(std::string name, bool isTerminal);
		~Symbol();
		bool const operator==(const Symbol &other);
		std::string toString();
	private:
		std::string name;
		bool isTerminal;


};

#endif