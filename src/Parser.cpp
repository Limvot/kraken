#include "Parser.h"

Parser::Parser() : EOFSymbol("$EOF$", true), nullSymbol("$NULL$", true), invalidSymbol("$INVALID$", true){
	table.setSymbols(EOFSymbol, nullSymbol);
}

Parser::~Parser() {
}

void Parser::exportTable(std::ofstream &file) {
	//Do table
	table.exportTable(file);
}
void  Parser::importTable(char* tableData) {
	//Do table
	table.importTable(tableData);
	return;
}

Symbol Parser::getOrAddSymbol(std::string symbolString, bool isTerminal) {
	Symbol symbol;
	std::pair<std::string, bool> entry = std::make_pair(symbolString, isTerminal);
	if (symbols.find(entry) == symbols.end()) {
		symbol = Symbol(symbolString, isTerminal);
		symbols[entry] = symbol;
	} else {
		symbol = symbols[entry];
	}
	return(symbol);
}

void Parser::loadGrammer(std::string grammerInputString) {
	reader.setString(grammerInputString);

	std::string currToken = reader.word(false); //Don't truncate so we can find the newline correctly (needed for comments)

	while(currToken != "") {
        //First, if this starts with a '#', skip this
        if (currToken.front() == '#') {
            //If this line is more than one token long, eat it
            std::cout << "Ate: " << currToken << std::endl;
            if (currToken.back() != '\n')
                std::cout << "Eating " << reader.line() << " b/c grammer comment" << std::endl;
            currToken = reader.word(false);
            continue;
        }
        if (currToken.back() == '\n' || currToken.back() == ' ' || currToken.back() == '\t')
            currToken.erase(currToken.size()-1);

        //Load the left of the rule
		ParseRule* currentRule = new ParseRule();
		Symbol leftSide = getOrAddSymbol(currToken, false); //Left handle is never a terminal
		currentRule->setLeftHandle(leftSide);
		reader.word(); //Remove the =
		//Add the right side, adding Symbols to symbol map.
		currToken = reader.word();
		while (currToken != ";") {

			//If there are multiple endings to this rule, finish this rule and start a new one with same left handle
			while (currToken == "|") {
				//If we haven't added anything, that means that this is a null rule
				if (currentRule->getRightSide().size() == 0)
					currentRule->appendToRight(nullSymbol);

				loadedGrammer.push_back(currentRule);
				currentRule = new ParseRule();
				currentRule->setLeftHandle(leftSide);
				currToken = reader.word();
			}

			if (currToken == ";")
				break;

			if (currToken[0] == '\"') {
				//Remove the quotes
				currToken = currToken.substr(1,currToken.length()-2);
				lexer.addRegEx(currToken);
				currentRule->appendToRight(getOrAddSymbol(currToken, true)); //If first character is a ", then is a terminal
			} else {
				currentRule->appendToRight(getOrAddSymbol(currToken, false));
			}
			currToken = reader.word();
		}
		//Add new rule to grammer
		//If we haven't added anything, that means that this is a null rule
		if (currentRule->getRightSide().size() == 0)
			currentRule->appendToRight(nullSymbol);

		loadedGrammer.push_back(currentRule);
		//Get next token
		currToken = reader.word(false);
	}
	//std::cout << "Parsed!\n";

	// for (std::vector<ParseRule*>::size_type i = 0; i < loadedGrammer.size(); i++)
	// 	std::cout << loadedGrammer[i]->toString() << std::endl;
}

void Parser::createStateSet() {
	std::cout << "Begining creation of stateSet" << std::endl;
	//First state has no parents

	//Set the first state's basis to be the goal rule with lookahead EOF
	ParseRule* goalRule = loadedGrammer[0]->clone();
	std::vector<Symbol> goalRuleLookahead;
	goalRuleLookahead.push_back(EOFSymbol);
	goalRule->setLookahead(goalRuleLookahead);
	State* zeroState = new State(0, goalRule);
	stateSets.push_back(zeroState);
	std::queue<State*> toDo;
	toDo.push(zeroState);
	//std::cout << "Begining for main set for loop" << std::endl;
	while (toDo.size()) {
		//closure
		closure(toDo.front());
		//Add the new states
		addStates(&stateSets, toDo.front(), &toDo);
		toDo.pop();
	}
	table.remove(1, EOFSymbol);
}

int Parser::stateNum(State* state) {
	for (std::vector<State*>::size_type i = 0; i < stateSets.size(); i++) {
		if (*(stateSets[i]) == *state) {
			return i;
		}
	}
	return -1;
}

std::vector<Symbol> Parser::firstSet(Symbol token, std::vector<Symbol> avoidList, bool addNewTokens) {
	if (tokenFirstSet.find(token) != tokenFirstSet.end())
        return tokenFirstSet[token];
    //If we've already done this token, don't do it again
	for (std::vector<Symbol>::size_type i = 0; i < avoidList.size(); i++)
		if (avoidList[i] == token)
			return std::vector<Symbol>();
	avoidList.push_back(token);

    std::vector<Symbol> first;
	//First, if the symbol is a terminal, than it's first set is just itself.
	if (token.isTerminal()) {
		first.push_back(token);
		return(first);
	}
	//Otherwise....
	//Ok, to make a first set, go through the grammer, if the token it's left side, add it's production's first token's first set.
	//If that one includes mull, do the next one too (if it exists).
	Symbol rightToken;
	std::vector<Symbol> recursiveFirstSet;
	for (std::vector<ParseRule*>::size_type i = 0; i < loadedGrammer.size(); i++) {
		if (token == loadedGrammer[i]->getLeftSide()) {
			//Loop through the rule adding first sets for each token if the previous token contained NULL
			int j = 0;
			do {
				rightToken = loadedGrammer[i]->getRightSide()[j]; //Get token of the right side of this rule
				if (rightToken.isTerminal()) {
					recursiveFirstSet.push_back(rightToken);
				} else {
					//Add the entire set
					recursiveFirstSet = firstSet(rightToken, avoidList, false);//Don't add children to cache, as early termination may cause them to be incomplete
				}
				first.insert(first.end(), recursiveFirstSet.begin(), recursiveFirstSet.end());
				j++;
			} while (isNullable(rightToken) && loadedGrammer[i]->getRightSide().size() > j);
		}
	}
    if (addNewTokens)
        tokenFirstSet[token] = first;
	return(first);
}

bool Parser::isNullable(Symbol token) {
    if (tokenNullable.find(token) != tokenNullable.end())
        return tokenNullable[token];
    bool nullable = isNullableHelper(token, std::set<Symbol>());
    tokenNullable[token] = nullable;
    return nullable;
}
//We use this helper function to recurse because it is possible to wind up with loops, and if so we want
//early termination. However, this means that nullable determinations in the middle of the loop are inaccurate
//(since we terminated early), so we don't want to save them. Thus, for simplicity, only the main method will
//add to the cache. This is somewhat unfortunate for preformance, but the necessary additions to keep track of
//invalidated state are more complicated than it's worth.
bool Parser::isNullableHelper(Symbol token, std::set<Symbol> done) {
    if (token.isTerminal())
        return token == nullSymbol;
    if (done.find(token) != done.end())
        return false;
    done.insert(token);
    if (tokenNullable.find(token) != tokenNullable.end())
        return tokenNullable[token];

    for (std::vector<ParseRule*>::size_type i = 0; i < loadedGrammer.size(); i++) {
		if (token == loadedGrammer[i]->getLeftSide()) {
            auto rightSide = loadedGrammer[i]->getRightSide();
            bool ruleNullable = true;
            for (int j = 0; j < rightSide.size(); j++) {
                if (!isNullableHelper(rightSide[j], done)) {
                    ruleNullable = false;
                    break;
                }
            }
            if (ruleNullable)
                return true;
        }
    }
    return false;
}

//Return the correct lookahead. This followSet is built based on the current rule's lookahead if at end, or the next Symbol's first set.
std::vector<Symbol> Parser::incrementiveFollowSet(ParseRule* rule) {
	//Advance the pointer past the current Symbol (the one we want the followset for) to the next symbol (which might be in our follow set, or might be the end)
	rule = rule->clone();
	rule->advancePointer();

	//Get the first set of the next Symbol. If it contains nullSymbol, keep doing for the next one
	std::vector<Symbol> followSet;
	std::vector<Symbol> symbolFirstSet;
	bool symbolFirstSetHasNull = true;
	while (symbolFirstSetHasNull && !rule->isAtEnd()) {
		symbolFirstSetHasNull = false;
		symbolFirstSet = firstSet(rule->getAtNextIndex());
		for (std::vector<Symbol>::size_type i = 0; i < symbolFirstSet.size(); i++) {
			if (symbolFirstSet[i] == nullSymbol) {
				symbolFirstSetHasNull = true;
				symbolFirstSet.erase(symbolFirstSet.begin()+i);
				break;
			}
		}
		followSet.insert(followSet.end(), symbolFirstSet.begin(), symbolFirstSet.end());
		rule->advancePointer();
	}
	if (rule->isAtEnd()) {
		symbolFirstSet = rule->getLookahead();
		followSet.insert(followSet.end(), symbolFirstSet.begin(), symbolFirstSet.end());
	}
	std::vector<Symbol> followSetReturn;
	for (std::vector<Symbol>::size_type i = 0; i < followSet.size(); i++) {
		bool alreadyIn = false;
		for (std::vector<Symbol>::size_type j = 0; j < followSetReturn.size(); j++)
			if (followSet[i] == followSetReturn[j]) {
				alreadyIn = true;
				break;
			}
		if (!alreadyIn)
			followSetReturn.push_back(followSet[i]);
	}
    delete rule;
	return followSetReturn;
}

void Parser::closure(State* state) {
	//Add all the applicable rules.
	//std::cout << "Closure on " << state->toString() << " is" << std::endl;
	std::vector<ParseRule*> stateTotal = state->getTotal();
	for (std::vector<ParseRule*>::size_type i = 0; i < stateTotal.size(); i++) {
		ParseRule* currentStateRule = stateTotal[i];
		//If it's at it's end, move on. We can't advance it.
		if(currentStateRule->isAtEnd())
			continue;
		for (std::vector<ParseRule*>::size_type j = 0; j < loadedGrammer.size(); j++) {
			//If the current symbol in the rule is not null (rule completed) and it equals a grammer's left side
			ParseRule* currentGramRule = loadedGrammer[j]->clone();
			if (currentStateRule->getAtNextIndex() == currentGramRule->getLeftSide()) {
				//std::cout << (*stateTotal)[i]->getAtNextIndex()->toString() << " has an applicable production " << loadedGrammer[j]->toString() << std::endl;
				//Now, add the correct lookahead. This followSet is built based on the current rule's lookahead if at end, or the next Symbol's first set.
				//std::cout << "Setting lookahead for " << currentGramRule->toString() << " in state " << state->toString() << std::endl;
				currentGramRule->setLookahead(incrementiveFollowSet(currentStateRule));

				//Check to make sure not already in
				bool isAlreadyInState = false;
				for (std::vector<ParseRule*>::size_type k = 0; k < stateTotal.size(); k++) {
					if (stateTotal[k]->equalsExceptLookahead(*currentGramRule)) {
						//std::cout << (*stateTotal)[k]->toString() << std::endl;
						stateTotal[k]->addLookahead(currentGramRule->getLookahead());
						isAlreadyInState = true;
						delete currentGramRule;
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
void Parser::addStates(std::vector< State* >* stateSets, State* state, std::queue<State*>* toDo) {
	std::vector< State* > newStates;
	//For each rule in the state we already have
	std::vector<ParseRule*> currStateTotal = state->getTotal();
	for (std::vector<ParseRule*>::size_type i = 0; i < currStateTotal.size(); i++) {
		//Clone the current rule
		ParseRule* advancedRule = currStateTotal[i]->clone();
		//Try to advance the pointer, if sucessful see if it is the correct next symbol
		if (advancedRule->advancePointer()) {
			//Technically, it should be the set of rules sharing this symbol advanced past in the basis for new state

			//So search our new states to see if any of them use this advanced symbol as a base.
			//If so, add this rule to them.
			//If not, create it.
			bool symbolAlreadyInState = false;
			for (std::vector< State* >::size_type j = 0; j < newStates.size(); j++) {
				if (newStates[j]->basis[0]->getAtIndex() == advancedRule->getAtIndex()) {
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
		std::vector<Symbol> lookahead = currStateTotal[i]->getLookahead();
		if (currStateTotal[i]->isAtEnd()) {
			for (std::vector<Symbol>::size_type j = 0; j < lookahead.size(); j++)
				table.add(stateNum(state), lookahead[j], new ParseAction(ParseAction::REDUCE, currStateTotal[i]));
		} else if (currStateTotal[i]->getAtNextIndex() == nullSymbol) {
			//If is a rule that produces only NULL, add in the approprite reduction, but use a new rule with a right side of length 0. (so we don't pop off stack)
			ParseRule* nullRule = currStateTotal[i]->clone();
			nullRule->setRightSide(std::vector<Symbol>());
			for (std::vector<Symbol>::size_type j = 0; j < lookahead.size(); j++)
				table.add(stateNum(state), lookahead[j], new ParseAction(ParseAction::REDUCE, nullRule));
		}
	}
	//Put all our new states in the set of states only if they're not already there.
	bool stateAlreadyInAllStates = false;
	Symbol currStateSymbol;
	for (std::vector< State * >::size_type i = 0; i < newStates.size(); i++) {
		stateAlreadyInAllStates = false;
		currStateSymbol = (*(newStates[i]->getBasis()))[0]->getAtIndex();
		for (std::vector< State * >::size_type j = 0; j < stateSets->size(); j++) {
			if (newStates[i]->basisEquals(*((*stateSets)[j]))) {
				stateAlreadyInAllStates = true;
				//If it does exist, we should add it as the shift/goto in the action table
				(*stateSets)[j]->addParents(newStates[i]->getParents());
				table.add(stateNum(state), currStateSymbol, new ParseAction(ParseAction::SHIFT, j));
				break;
			}
		}
		if (!stateAlreadyInAllStates) {
			//If the state does not already exist, add it and add it as the shift/goto in the action table
			stateSets->push_back(newStates[i]);
			toDo->push(newStates[i]);
			table.add(stateNum(state), currStateSymbol, new ParseAction(ParseAction::SHIFT, stateSets->size()-1));
		}
	}
}

std::string Parser::stateSetToString() {
	std::string concat = "";
	for (std::vector< State *>::size_type i = 0; i < stateSets.size(); i++) {
		concat += intToString(i) + " is " + stateSets[i]->toString();
	}
	return concat;
}


std::string Parser::tableToString() {
	return table.toString();
}

//parseInput is now pure virtual

NodeTree<Symbol>* Parser::reduceTreeCombine(Symbol newSymbol, std::vector<Symbol> &symbols) {
	NodeTree<Symbol>* newTree = new NodeTree<Symbol>(newSymbol.getName(), newSymbol);
	for (std::vector<Symbol>::size_type i = 0; i < symbols.size(); i++) {
		if (symbols[i].isTerminal())
			newTree->addChild(new NodeTree<Symbol>(symbols[i].getName(), symbols[i]));
		else
			newTree->addChild(symbols[i].getSubTree());
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

