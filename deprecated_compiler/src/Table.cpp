#include "Table.h"

Table::Table() {
	//
}

Table::~Table() {
	//
}

void Table::exportTable(std::ofstream &file) {
	//Save symbolIndexVec
	int size = symbolIndexVec.size();
	file.write((char*)&size, sizeof(int));
	for (int i = 0; i < symbolIndexVec.size(); i++) {
		//Save the name
		std::string symbolName = symbolIndexVec[i].getName(); //Get the string
		size = symbolName.size()+1;
		file.write((char*)&size, sizeof(int)); //Save size of string
		file.write((char*)(symbolName.c_str()), size); //Save the string

		//Save the value
		std::string symbolValue = symbolIndexVec[i].getValue(); //Get the string
		size = symbolValue.size()+1;
		file.write((char*)&size, sizeof(int)); //Save size of string
		file.write((char*)(symbolValue.c_str()), size); //Save the string

		bool isTerminal = symbolIndexVec[i].isTerminal();
		file.write((char*)&isTerminal, sizeof(bool)); //Save the true false
	}

	//Save the actual table
	size = table.size();
	file.write((char*)&size, sizeof(int));
	for (int i = 0; i < table.size(); i++) {
		//each item is a middle vector
		//std::vector< std::vector< std::vector<ParseAction*>* >* >  table;
		std::vector< std::vector<ParseAction*>* >* middleVector = table[i];
		int middleVectorSize = middleVector->size();
		file.write((char*)&middleVectorSize, sizeof(int));

		for (int j = 0; j < middleVectorSize; j++) {
			//each item is an inner vector
			std::vector<ParseAction*>* innerVector = (*middleVector)[j];
			int innerVectorSize = 0;
			if (innerVector)
				innerVectorSize = innerVector->size();
			else
				innerVectorSize = 0;
			file.write((char*)&innerVectorSize, sizeof(int));

			for (int k = 0; k < innerVectorSize; k++) {
				//Save the type
				ParseAction* toSave = (*innerVector)[k];
				ParseAction::ActionType actionType = toSave->action;
				file.write((char*)&actionType, sizeof(ParseAction::ActionType));
				//Save the reduce rule if necessary
				if (actionType == ParseAction::REDUCE) {
					//Save the reduce rule
					ParseRule* rule = toSave->reduceRule;
					//int pointer index
					int ptrIndx = rule->getIndex();
					file.write((char*)&ptrIndx, sizeof(int));

					//Symbol leftHandle
					Symbol leftHandle = rule->getLeftSide();
					//Save the name
					std::string symbolName = leftHandle.getName(); //Get the string
					size = symbolName.size()+1;
					file.write((char*)&size, sizeof(int)); //Save size of string
					file.write((char*)(symbolName.c_str()), size); //Save the string

					//Save the value
					std::string symbolValue = leftHandle.getValue(); //Get the string
					size = symbolValue.size()+1;
					file.write((char*)&size, sizeof(int)); //Save size of string
					file.write((char*)(symbolValue.c_str()), size); //Save the string

					bool isTerminal = leftHandle.isTerminal();
					file.write((char*)&isTerminal, sizeof(bool)); //Save the true false

					//std::vector<Symbol>* lookahead;
					//Should not need

					//std::vector<Symbol> rightSide;
					std::vector<Symbol> rightSide = rule->getRightSide();
					size = rightSide.size();
					//std::cout << leftHandle.toString() << std::endl;
					file.write((char*)&size, sizeof(int));
					for (int l = 0; l < rightSide.size(); l++) {
						//Save the name
						symbolName = rightSide[l].getName(); //Get the string
						size = symbolName.size()+1;
						file.write((char*)&size, sizeof(int)); //Save size of string
						file.write((char*)(symbolName.c_str()), size); //Save the string
						//
						//Save the value
						symbolValue = rightSide[l].getValue(); //Get the string
						size = symbolValue.size()+1;
						file.write((char*)&size, sizeof(int)); //Save size of string
						file.write((char*)(symbolValue.c_str()), size); //Save the string
						//
						isTerminal = rightSide[l].isTerminal();
						file.write((char*)&isTerminal, sizeof(bool)); //Save the true false
					}
				}
				int shiftState = toSave->shiftState;
				file.write((char*)&shiftState, sizeof(int));
			}
		}

	}
}

void Table::importTable(char* tableData) {
	//Load symbolIndexVec

	int size = *((int*)tableData);
	tableData += sizeof(int);
	for (int i = 0; i < size; i++) {
		int stringLen = *((int*)tableData);
		tableData += sizeof(int);
		std::string symbolName = std::string(tableData);
		tableData += stringLen*sizeof(char);
		stringLen = *((int*)tableData);
		tableData += sizeof(int);
		std::string symbolValue = std::string(tableData);
		tableData += stringLen*sizeof(char);

		bool isTerminal = *((bool*)tableData);
		tableData += sizeof(bool);

		symbolIndexVec.push_back(Symbol(symbolName, isTerminal, symbolValue));
	}

	//Now for the actual table
	int tableSize = *((int*)tableData);
	tableData += sizeof(int);
	for (int i = 0; i < tableSize; i++) {
		//each item is a middle vector
		std::vector< std::vector<ParseAction*>* >* middleVector = new std::vector< std::vector<ParseAction*>* >();
		table.push_back(middleVector);

		int middleVectorSize = *((int*)tableData);
		tableData += sizeof(int);
		for (int j = 0; j < middleVectorSize; j++) {
			//each item is an inner vector
			std::vector<ParseAction*>* innerVector = new std::vector<ParseAction*>();
			middleVector->push_back(innerVector);
			int innerVectorSize = *((int*)tableData);
			tableData += sizeof(int);
			for (int k = 0; k < innerVectorSize; k++) {
				//each item is a ParseRule
				ParseAction::ActionType action = *((ParseAction::ActionType*)tableData);
				tableData += sizeof(ParseAction::ActionType);
				//If reduce, import the reduce rule
				ParseRule* reduceRule = NULL;
				if (action == ParseAction::REDUCE) {
					int ptrIndx = *((int*)tableData);
					tableData += sizeof(int);

					size = *((int*)tableData);
					tableData += sizeof(int);
					std::string leftHandleName = std::string(tableData);
					tableData += size*sizeof(char);
					size = *((int*)tableData);
					tableData += sizeof(int);
					std::string leftHandleValue = std::string(tableData);
					tableData += size*sizeof(char);

					bool isTerminal = *((bool*)tableData);
					tableData += sizeof(bool);

					//right side
					std::vector<Symbol> rightSide;
					size = *((int*)tableData);
					tableData += sizeof(int);
					for (int l = 0; l < size; l++) {
						int inStringLen = *((int*)tableData);
						tableData += sizeof(int);
						std::string inSymbolName = std::string(tableData);
						tableData += inStringLen*sizeof(char);

						inStringLen = *((int*)tableData);
						tableData += sizeof(int);
						std::string inSymbolValue = std::string(tableData);
						tableData += inStringLen*sizeof(char);

						bool inIsTerminal = *((bool*)tableData);
						tableData += sizeof(bool);
						rightSide.push_back(Symbol(inSymbolName, inIsTerminal, inSymbolValue));
					}
					reduceRule = new ParseRule(Symbol(leftHandleName, isTerminal, leftHandleValue), ptrIndx, rightSide, std::vector<Symbol>());
				}
				int shiftState = *((int*)tableData);
				tableData += sizeof(int);

				//And push the new action back
				if (reduceRule)
					innerVector->push_back(new ParseAction(action, reduceRule));
				else
					innerVector->push_back(new ParseAction(action, shiftState));
			}
		}
	}
}

void Table::setSymbols(Symbol EOFSymbol, Symbol nullSymbol) {
	this->EOFSymbol = EOFSymbol;
	this->nullSymbol = nullSymbol;
}

void Table::add(int stateNum, Symbol tranSymbol, ParseAction* action) {

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
	for (std::vector<Symbol>::size_type i = 0; i < symbolIndexVec.size(); i++) {
		if ( symbolIndexVec[i] == tranSymbol ) {
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

		//Check to see if this action is already in the list

		//(*(table[stateNum]))[symbolIndex]->push_back(action);
		bool alreadyIn = false;
		for (std::vector<ParseAction*>::size_type i = 0; i < (*(table[stateNum]))[symbolIndex]->size(); i++)
			if (*((*((*(table[stateNum]))[symbolIndex]))[i]) == *action)
				alreadyIn = true;
		if (!alreadyIn)
			(*(table[stateNum]))[symbolIndex]->push_back(action);
	}
}

void Table::remove(int stateNum, Symbol tranSymbol) {
	//find out what index this symbol is on
	int symbolIndex = -1;
	for (std::vector<Symbol>::size_type i = 0; i < symbolIndexVec.size(); i++) {
		if ( symbolIndexVec[i] == tranSymbol ) {
			//Has been found
			symbolIndex = i;
			break;
		}
	}
	(*(table[stateNum]))[symbolIndex] = NULL;
}

std::vector<ParseAction*>* Table::get(int state, Symbol token) {
	int symbolIndex = -1;
	for (std::vector<Symbol>::size_type i = 0; i < symbolIndexVec.size(); i++) {
		if ( symbolIndexVec[i] == token) {
			symbolIndex = i;
			break;
		}
	}

	if (symbolIndex == -1) {
		std::cout << "Unrecognized symbol: " << token.toString() << ", cannot get from table!" << std::endl;
		return NULL;
	}

	//std::cout << "Get for state: " << state << ", and Symbol: " << token.toString() << std::endl;
	if (state < 0 || state >= table.size()) {
		std::cout << "State bad: " << state << std::endl;
		return NULL;
	}

	std::vector<ParseAction*>* action = NULL;

	if (symbolIndex < 0 || symbolIndex >= table[state]->size()) {
		//std::cout << "Symbol bad for this state: " << token.toString() << ". This is a reject." << std::endl;
	} else {
		action = (*(table[state]))[symbolIndex];
	}

	//This is the accepting state, as it is the 1th's state's reduction on EOF, which is 0 in the symbolIndexVec
	//(This assumes singular goal assignment, a simplification for now)
	if (state == 1 && symbolIndex == 0) {
		if (action == NULL)
			action = new std::vector<ParseAction*>();
		action->push_back(new ParseAction(ParseAction::ACCEPT));
	}

	//If outside the symbol range of this state (same as NULL), reject
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
	return action;
}

ParseAction* Table::getShift(int state, Symbol token) {
	std::vector<ParseAction*>* actions = get(state, token);
	ParseAction* shift = NULL;
	for (int i = 0; i < actions->size(); i++) {
		if ((*actions)[i]->action == ParseAction::SHIFT) {
			shift = (*actions)[i];
			break;
		}
	}
	return shift;
}

std::vector<std::pair<std::string, ParseAction>> Table::stateAsParseActionVector(int state) {
    std::vector<std::pair<std::string, ParseAction>> reconstructedState;
    std::vector<std::vector<ParseAction*>*>* stateVec = table[state];
    for (int i = 0; i < stateVec->size(); i++)
        if (std::vector<ParseAction*>* forStateAndSymbol = (*stateVec)[i])
            for (int j = 0; j < forStateAndSymbol->size(); j++)
                reconstructedState.push_back(std::make_pair(symbolIndexVec[i].toString(),*((*forStateAndSymbol)[j])));

    return reconstructedState;
}

std::string Table::toString() {
	std::string concat = "";
	for (std::vector<Symbol>::size_type i = 0; i < symbolIndexVec.size(); i++)
		concat += "\t" + symbolIndexVec[i].toString();
	concat += "\n";

	for (std::vector< std::vector< std::vector< ParseRule* >* >* >::size_type i = 0; i < table.size(); i++) {
		concat += intToString(i) + " is the state\t";
		for (std::vector< std::vector< ParseRule* >* >::size_type j = 0; j < table[i]->size(); j++) {
			concat += "for " + symbolIndexVec[j].toString() + " do ";
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
