#include "Parser.h"

Parser::Parser() {

}

Parser::~Parser() {

}

Symbol* Parser::getOrAddSymbol(std::string symbolString, bool isTerminal) {
	Symbol* symbol;
	if (symbols.find(symbolString) == symbols.end()) {
		symbol = new Symbol(symbolString, isTerminal);
		symbols[symbolString] = symbol;
	} else {
		symbol = symbols[symbolString];
	}
	return(symbol);
}

void Parser::loadGrammer(std::string grammerInputString) {
	reader.setString(grammerInputString);

	std::string currToken = reader.word();

	while(currToken != "") {
		//Load the left of the rule
		ParseRule* currentRule = new ParseRule();
		Symbol* leftSide = getOrAddSymbol(currToken, false); //Left handle is never a terminal
		currentRule->setLeftHandle(leftSide);
		reader.word(); //Remove the =
		//Add the right side, adding new Symbols to symbol map.
		currToken = reader.word();
		while (currToken != ";") {
			currentRule->appendToRight(getOrAddSymbol(currToken, currToken.at(0)=='\"')); //If first character is a ", then is a terminal
			currToken = reader.word();
			//If there are multiple endings to this rule, finish this rule and start a new one with same left handle
			if (currToken == "|") {
				loadedGrammer.push_back(currentRule);
				currentRule = new ParseRule();
				currentRule->setLeftHandle(leftSide);
				currToken = reader.word();
			}
		}
		//Add new rule to grammer
		loadedGrammer.push_back(currentRule);
		//Get next token
		currToken = reader.word();
	}
	std::cout << "Parsed!\n";
}

std::vector<Symbol*>* Parser::firstSet(Symbol* token) {
	std::vector<Symbol*>* first = new std::vector<Symbol*>();
	//First, if the symbol is a terminal, than it's first set is just itself.
	if (token->isTerminal()) {
		first->push_back(token);
		return(first);
	}
	//Otherwise....
	//Ok, to make a first set, go through the grammer, if the token is part of the left side, add it's production's first token's first set.
	//Theoretically, if that one includes mull, do the next one too. However, null productions have not yet been implemented.
	Symbol* rightToken = NULL;
	std::vector<Symbol*>* recursiveFirstSet = NULL;
	for (std::vector<ParseRule*>::size_type i = 0; i < loadedGrammer.size(); i++) {
		if (*token == *(loadedGrammer[i]->getLeftSide())) {
			rightToken = loadedGrammer[i]->getRightSide()[0]; //Get the first token of the right side of this rule
			if (rightToken->isTerminal())
				first->push_back(rightToken);
			else {
				//Add the entire set
				recursiveFirstSet = firstSet(rightToken);
				first->insert(first->end(), recursiveFirstSet->begin(), recursiveFirstSet->end());
			}
		}
	}
	return(first);
}

void Parser::printFirstSets() {
	std::vector<Symbol*>* first = NULL;
	for (std::vector<Symbol*>::size_type i = 0; i < symbolIndexVec.size(); i++) {
		first = firstSet(symbolIndexVec[i]);
		std::cout << "First set of " << symbolIndexVec[i]->toString() << " is: ";
		for (std::vector<Symbol*>::size_type j = 0; j < first->size(); j++)
			std::cout << (*first)[j]->toString() << " ";
		std::cout << std::endl;
	}
}


//Return the correct lookahead. This followSet is built based on the current rule's lookahead if at end, or the next Symbol's first set.
std::vector<Symbol*>* Parser::incrementiveFollowSet(ParseRule* rule) {
	//Advance the pointer past the current Symbol (the one we want the followset for) to the next symbol (which might be in our follow set, or might be the end)
	rule = rule->clone();
	rule->advancePointer();
	if (rule->isAtEnd())
		return rule->getLookahead();
	return firstSet(rule->getAtNextIndex());

}

void Parser::createStateSet() {
	std::cout << "Begining creation of stateSet" << std::endl;
	//First state has no parents

	//Set the first state's basis to be the goal rule with lookahead EOF
	ParseRule* goalRule = loadedGrammer[0]->clone();
	std::vector<Symbol*>* goalRuleLookahead = new std::vector<Symbol*>();
	goalRuleLookahead->push_back(new Symbol("$EOF$", false));
	goalRule->setLookahead(goalRuleLookahead);
	stateSets.push_back( new State(0, goalRule));
	//std::cout << "Begining for main set for loop" << std::endl;
	for (std::vector< State* >::size_type i = 0; i < stateSets.size(); i++) {
		//closure
		closure(stateSets[i]);
		//Add the new states
		addStates(&stateSets, stateSets[i]);
	}
}

void Parser::closure(State* state) {
	//Add all the applicable rules.
	//std::cout << "Closure on " << state->toString() << " is" << std::endl;
	std::vector<ParseRule*>* stateTotal = state->getTotal();
	for (std::vector<ParseRule*>::size_type i = 0; i < stateTotal->size(); i++) {
		ParseRule* currentStateRule = (*stateTotal)[i];
		for (std::vector<ParseRule*>::size_type j = 0; j < loadedGrammer.size(); j++) {
			//If the current symbol in the rule is not null (rule completed) and it equals a grammer's left side
			ParseRule* currentGramRule = loadedGrammer[j]->clone();
			if ( !currentStateRule->isAtEnd() && *(currentStateRule->getAtNextIndex()) == *(currentGramRule->getLeftSide())) {
				//std::cout << (*stateTotal)[i]->getAtNextIndex()->toString() << " has an applicable production " << loadedGrammer[j]->toString() << std::endl;
				//Now, add the correct lookahead. This followSet is built based on the current rule's lookahead if at end, or the next Symbol's first set.
				currentGramRule->setLookahead(incrementiveFollowSet(currentStateRule));

				std::vector<Symbol*>* gramRuleLookahead = currentGramRule->getLookahead();
				//std::cout << "Current lookahead for  " << currentGramRule->toString() << std::endl;
				//Check to make sure not already in
				bool isAlreadyInState = false;
				for (std::vector<ParseRule*>::size_type k = 0; k < stateTotal->size(); k++) {
					if (*((*stateTotal)[k]) == *currentGramRule) {
						isAlreadyInState = true;
						break;
					}
				}
				if (!isAlreadyInState) {
					state->remaining.push_back(currentGramRule);
					stateTotal = state->getTotal();
				}
			}
		}
	}
	//std::cout << state->toString() << std::endl;
}

//Adds state if it doesn't already exist.
void Parser::addStates(std::vector< State* >* stateSets, State* state) {
	std::vector< State* > newStates;
	//For each rule in the state we already have
	std::vector<ParseRule*>* currStateTotal = state->getTotal();
	for (std::vector<ParseRule*>::size_type i = 0; i < currStateTotal->size(); i++) {
		//Clone the current rule
		ParseRule* advancedRule = (*currStateTotal)[i]->clone();
		//Try to advance the pointer, if sucessful see if it is the correct next symbol
		if (advancedRule->advancePointer()) { 
			//Technically, it should be the set of rules sharing this symbol advanced past in the basis for new state

			//So search our new states to see if any of them use this advanced symbol as a base.
			//If so, add this rule to them.
			//If not, create it.
			bool symbolAlreadyInState = false;
			for (std::vector< State* >::size_type j = 0; j < newStates.size(); j++) {
				if (*(newStates[j]->basis[0]->getAtIndex()) == *(advancedRule->getAtIndex())) {
					symbolAlreadyInState = true;
					//So now check to see if this exact rule is in this state
					if (!newStates[j]->containsRule(advancedRule))
						newStates[j]->basis.push_back(advancedRule);
					//We found a state with the same symbol, so stop searching
					break;
				}
			}
			if (!symbolAlreadyInState) {
				State* newState = new State(stateSets->size()+newStates.size(),advancedRule, state);
				newStates.push_back(newState);
			}
		}
		//Also add any completed rules as reduces in the action table
		//See if reduce
		//Also, this really only needs to be done for the state's basis, but we're already iterating through, so...
		if ((*currStateTotal)[i]->isAtEnd()) {
			std::vector<Symbol*>* lookahead = (*currStateTotal)[i]->getLookahead();
			for (std::vector<Symbol*>::size_type j = 0; j < lookahead->size(); j++)
				addToTable(state, (*lookahead)[j], new ParseAction(ParseAction::REDUCE, (*currStateTotal)[i]));
		} else {
			//std::cout << (*currStateTotal)[i]->toString() << " is NOT at end" << std::endl;
		}
	}
	//Put all our new states in the set of states only if they're not already there.
	bool stateAlreadyInAllStates = false;
	Symbol* currStateSymbol;
	for (std::vector< State * >::size_type i = 0; i < newStates.size(); i++) {
		stateAlreadyInAllStates = false;
		currStateSymbol = (*(newStates[i]->getBasis()))[0]->getAtIndex();
		for (std::vector< State * >::size_type j = 0; j < stateSets->size(); j++) {
			if (newStates[i]->basisEquals(*((*stateSets)[j]))) {
				stateAlreadyInAllStates = true;
				//If it does exist, we should add it as the shift/goto in the action table
				//std::cout << "State exists, is " << j << std::endl;
				(*stateSets)[j]->addParents(newStates[i]->getParents());
				addToTable(state, currStateSymbol, new ParseAction(ParseAction::SHIFT, j));
				break;
			}/* else {
				std::cout << "State " << newStates[i]->toString() << " does not equal " << (*stateSets)[j]->toString() << std::endl;
			}*/
		}
		if (!stateAlreadyInAllStates) {
			stateSets->push_back(newStates[i]);
			//If the state does not already exist, add it and add it as the shift/goto in the action table
			//std::cout << "State does not exist" << std::endl;
			//std::cout << "State is " << newStates[i]->toString() << std::endl;
			addToTable(state, currStateSymbol, new ParseAction(ParseAction::SHIFT, stateSets->size()-1));
		}
	}
}

std::string Parser::stateSetToString() {
	std::string concat = "";
	for (std::vector< State *>::size_type i = 0; i < stateSets.size(); i++) {
		concat += stateSets[i]->toString();
	}
	return concat;
}

void Parser::addToTable(State* fromState, Symbol* tranSymbol, ParseAction* action) {

	//If this is the first time we're adding to the table, add the EOF character
	if (symbolIndexVec.size() == 0)
		symbolIndexVec.push_back(new Symbol("$EOF$", false));

	//find what state num the from state is
	int stateNum = -1;
	for (std::vector<State*>::size_type i = 0; i < stateSets.size(); i++) {
		if (*(stateSets[i]) == *fromState) {
			stateNum = i;
			break;
		}
	}

	//std::cout << "stateNum is " << stateNum << std::endl;

	//If state not in table, add up to and it.
	//std::cout << "table size is " << table.size() <<std::endl;
	while (stateNum >= table.size()) {
		//std::cout << "Pushing back table" << std::endl;
		table.push_back(new std::vector<ParseAction*>);
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
		(*(table[stateNum]))[symbolIndex] = action;
	}
	//If the slot is not empty and does not contain ourself, then it is a conflict
	else if ( *((*(table[stateNum]))[symbolIndex]) != *action) {
		//std::cout << "not Null!" << std::endl;
		std::cout << "State: " << stateNum << " Conflict between old: " << (*(table[stateNum]))[symbolIndex]->toString() << " and new: " << action->toString() << std::endl; 
		//Don't overwrite
		//(*(table[stateNum]))[symbolIndex] = action;
	}
}

std::string Parser::tableToString() {
	std::string concat = "";
	for (std::vector<Symbol*>::size_type i = 0; i < symbolIndexVec.size(); i++)
		concat += "\t" + symbolIndexVec[i]->toString();
	concat += "\n";

	for (std::vector< std::vector< ParseRule* > >::size_type i = 0; i < table.size(); i++) {
		concat += intToString(i) + "\t";
		for (std::vector< ParseRule* >::size_type j = 0; j < table[i]->size(); j++) {
			if ( (*(table[i]))[j] != NULL)
				concat += (*(table[i]))[j]->toString() + "\t";
			else
				concat += "NULL\t";
		}
		concat += "\n";
	}
	return(concat);
}

ParseAction* Parser::getTable(int state, Symbol* token) {
	int symbolIndex = -1;
	for (std::vector<Symbol*>::size_type i = 0; i < symbolIndexVec.size(); i++) {
		if ( *(symbolIndexVec[i]) == *token) {
			symbolIndex = i;
			break;
		}
	}

	//This is the accepting state, as it is the 1th's state's reduction on EOF, which is 0 in the symbolIndexVec
	//(This assumes singular goal assignment, a simplification for now)
	if (state == 1 && symbolIndex == 0)
		return(new ParseAction(ParseAction::ACCEPT));

	//If ourside the symbol range of this state (same as NULL), reject
	if ( symbolIndex >= table[state]->size() )
		return(new ParseAction(ParseAction::REJECT));

	ParseAction* action = (*(table[state]))[symbolIndex];
	//If null, reject. (this is a space with no other action)
	if (action == NULL)
		return(new ParseAction(ParseAction::REJECT));

	//Otherwise, we have something, so return it
	return (action);
}

NodeTree* Parser::parseInput(Lexer* lexer) {
	Symbol* token = lexer->next();
	ParseAction* action;

	stateStack.push(0);
	symbolStack.push(new Symbol("INVALID", false));

	while (true) {
		std::cout << "In state: " << intToString(stateStack.top()) << std::endl;
		action = getTable(stateStack.top(), token);
		//std::cout << "Doing ParseAction: " << action->toString() << std::endl;
		switch (action->action) {
			case ParseAction::REDUCE:
			{
				std::cout << "Reduce by " << action->reduceRule->toString() << std::endl;
				
				int rightSideLength = action->reduceRule->getRightSide().size();
				//Keep track of symbols popped for parse tree
				std::vector<Symbol*> poppedSymbols;
				for (int i = 0; i < rightSideLength; i++) {
					poppedSymbols.push_back(symbolStack.top());
					stateStack.pop();
					symbolStack.pop();
				}
				std::reverse(poppedSymbols.begin(), poppedSymbols.end()); //To put in order
				//Assign the new tree to the new Symbol
				Symbol* newSymbol = action->reduceRule->getLeftSide()->clone();
				newSymbol->setSubTree(reduceTreeCombine(newSymbol, poppedSymbols));
				symbolStack.push(newSymbol);
				//std::cout << "top of state is " << intToString(stateStack.top()) << " symbolStack top is " << symbolStack.top()->toString() << std::endl;
				stateStack.push(getTable(stateStack.top(), symbolStack.top())->shiftState);
				//std::cout << "Reduced, now condition is" << std::endl;
				//std::cout << "top of state is " << intToString(stateStack.top()) << " symbolStack top is " << symbolStack.top()->toString() << std::endl;
				break;
			}
			case ParseAction::SHIFT:
				std::cout << "Shift " << token->toString() << std::endl;

				symbolStack.push(token);
				token = lexer->next();
				stateStack.push(action->shiftState);
				break;
			case ParseAction::ACCEPT:
				std::cout << "ACCEPTED!" << std::endl;
				return(symbolStack.top()->getSubTree());
				break;
			case ParseAction::REJECT:
				std::cout << "REJECTED!" << std::endl;
				std::cout << "REJECTED Symbol was " << token->toString() << std::endl;
				return(NULL);
				break;
		}
	}
}

NodeTree* Parser::reduceTreeCombine(Symbol* newSymbol, std::vector<Symbol*> &symbols) {
	NodeTree* newTree = new NodeTree(newSymbol->getName(), newSymbol);
	for (std::vector<Symbol*>::size_type i = 0; i < symbols.size(); i++) {
		if (symbols[i]->isTerminal())
			newTree->addChild(new NodeTree(symbols[i]->getName(), symbols[i]));
		else
			newTree->addChild(symbols[i]->getSubTree());
	}
	return(newTree);
}

std::string Parser::grammerToString() {
	//Iterate through the vector, adding string representation of each grammer rule
	std::cout << "About to toString\n";
	std::string concat = "";
	for (int i = 0; i < loadedGrammer.size(); i++) {
		concat += loadedGrammer[i]->toString() + "\n";
	}
	return(concat);
}

std::string Parser::grammerToDOT() {
	//Iterate through the vector, adding DOT representation of each grammer rule
	//std::cout << "About to DOT export\n";
	std::string concat = "";
	for (int i = 0; i < loadedGrammer.size(); i++) {
		concat += loadedGrammer[i]->toDOT();
	}
	return("digraph Kraken_Grammer { \n" + concat + "}");
}

