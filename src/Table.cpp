#include "Table.h"

Table::Table() {
	//
}

Table::~Table() {
	//
}

void Table::setSymbols(Symbol* EOFSymbol, Symbol* nullSymbol) {
	this->EOFSymbol = EOFSymbol;
	this->nullSymbol = nullSymbol;
}

void Table::add(int stateNum, Symbol* tranSymbol, ParseAction* action) {

	//If this is the first time we're adding to the table, add the EOF character
	if (symbolIndexVec.size() == 0)
		symbolIndexVec.push_back(EOFSymbol);

	//If state not in table, add up to and it.
	//std::cout << "table size is " << table.size() <<std::endl;
	while (stateNum >= table.size()) {
		//std::cout << "Pushing back table" << std::endl;
		table.push_back(new std::vector<std::vector< ParseAction*>* >());
	}

	//find out what index this symbol is on
	int symbolIndex = -1;
	for (std::vector<Symbol*>::size_type i = 0; i < symbolIndexVec.size(); i++) {
		if ( *(symbolIndexVec[i]) == *tranSymbol ) {
			//Has been found
			symbolIndex = i;
			break;
		}
	}
	//std::cout << "symbolIndex is " << symbolIndex << std::endl;

	//If we've never done this symbol, add it
	if (symbolIndex < 0) {
	//	std::cout << "pushing back symbolIndexVec" <<std::endl;
		symbolIndex = symbolIndexVec.size();
		symbolIndexVec.push_back(tranSymbol);
	}

	//std::cout << "symbolIndex is " << symbolIndex << " which is " << symbolIndexVec[symbolIndex]->toString() << std::endl;

	//std::cout << table[stateNum] << " ";
	while (symbolIndex >= table[stateNum]->size()) {
		table[stateNum]->push_back(NULL);
	}

	//If this table slot is empty
	//std::cout << "table[stateNum] is " << table[stateNum] << std::endl;
	//std::cout << "blank is " << (*(table[stateNum]))[symbolIndex] << std::endl;
	
	if ( (*(table[stateNum]))[symbolIndex] == NULL ) {
		//std::cout << "Null, adding " << action->toString() << std::endl;
		std::vector<ParseAction*>* actionList = new std::vector<ParseAction*>();
		actionList->push_back(action);
		(*(table[stateNum]))[symbolIndex] = actionList;
	}
	//If the slot is not empty and does not contain ourself, then it is a conflict
	//else if ( !(*(table[stateNum]))[symbolIndex]->equalsExceptLookahead(*action)) {
	else {
		//std::cout << "not Null!" << std::endl;
		//std::cout << "State: " << stateNum << " Conflict between old: " << (*(table[stateNum]))[symbolIndex]->toString() << " and new: " << action->toString() << " on " << tranSymbol->toString() << std::endl; 

		(*(table[stateNum]))[symbolIndex]->push_back(action);
	}
}

std::vector<ParseAction*>* Table::get(int state, Symbol* token) {
	int symbolIndex = -1;
	for (std::vector<Symbol*>::size_type i = 0; i < symbolIndexVec.size(); i++) {
		if ( *(symbolIndexVec[i]) == *token) {
			symbolIndex = i;
			break;
		}
	}

	//This is the accepting state, as it is the 1th's state's reduction on EOF, which is 0 in the symbolIndexVec
	//(This assumes singular goal assignment, a simplification for now)
	std::vector<ParseAction*>* action = (*(table[state]))[symbolIndex];

	if (state == 1 && symbolIndex == 0) {
		if (action == NULL)
			action = new std::vector<ParseAction*>();
		action->push_back(new ParseAction(ParseAction::ACCEPT));
	}

	//If ourside the symbol range of this state (same as NULL), reject
	if ( symbolIndex >= table[state]->size() ) {
		action = new std::vector<ParseAction*>();
		action->push_back(new ParseAction(ParseAction::REJECT));
	}

	//If null, reject. (this is a space with no other action)
	if (action == NULL) {
		action = new std::vector<ParseAction*>();
		action->push_back(new ParseAction(ParseAction::REJECT));
	}

	//Otherwise, we have something, so return it
	return (action);
}

std::string Table::toString() {
	std::string concat = "";
	for (std::vector<Symbol*>::size_type i = 0; i < symbolIndexVec.size(); i++)
		concat += "\t" + symbolIndexVec[i]->toString();
	concat += "\n";

	for (std::vector< std::vector< std::vector< ParseRule* >* >* >::size_type i = 0; i < table.size(); i++) {
		concat += intToString(i) + "\t";
		for (std::vector< std::vector< ParseRule* >* >::size_type j = 0; j < table[i]->size(); j++) {
			if ( (*(table[i]))[j] != NULL) {
				for (std::vector< ParseRule* >::size_type k = 0; k < (*(table[i]))[j]->size(); k++) {
					concat += (*((*(table[i]))[j]))[k]->toString() + "\t";
				}
			} else {
				concat += "NULL\t";
			}
		}
		concat += "\n";
	}
	return(concat);
}
