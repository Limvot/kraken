#include "util.h"
#include "ParseRule.h"
#include "ParseAction.h"
#include "Symbol.h"
#include "State.h"

#ifndef TABLE_H
#define TABLE_H

class Table {
	public:
		Table();
		~Table();
		void setSymbols(Symbol EOFSymbol, Symbol nullSymbol);
		void add(int stateNum, Symbol tranSymbol, ParseAction* action);
		void remove(int stateNum, Symbol tranSymbol);
		std::vector<ParseAction*>* get(int state, Symbol token);
		ParseAction* getShift(int state, Symbol token);
		std::string toString();
	private:
		std::vector< std::vector< std::vector<ParseAction*>* >* >  table;
		std::vector<Symbol> symbolIndexVec;
		//The EOFSymbol, a pointer because of use in table, etc
		Symbol EOFSymbol;
		//The nullSymbol, ditto with above. Also used in comparisons
		Symbol nullSymbol;
};

#endif
