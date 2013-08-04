#include "RNGLRParser.h"

RNGLRParser::RNGLRParser() {
	//
}

RNGLRParser::~RNGLRParser() {
	//
}

NodeTree<Symbol*>* RNGLRParser::parseInput(std::string inputString) {

	//Check for no tokens
	bool accepting = false;
	if (inputString == "") {
		std::vector<ParseAction*>* zeroStateActions = table.get(0,EOFSymbol);
		for (int i = 0; i < zeroStateActions->size(); i++) {
			if ((*zeroStateActions)[i]->action == ParseAction::REDUCE)
				accepting = true;
		}
		if (accepting)
			std::cout << "Accepted!" << std::endl;
		else
			std::cout << "Rejected, no input (with no accepting state)" << std::endl;
		return new NodeTree<Symbol*>();
	}

	lexer.setInput(inputString);
	//Now fully lex our input because this algorithm was designed in that manner and simplifies this first implementation.
	//It could be converted to on-line later.
	Symbol* currentToken = lexer.next();
	input.push_back(currentToken);
	while (*currentToken != *EOFSymbol) {
		std::cout << EOFSymbol->toString() << " " << currentToken->toString() << std::endl;
		currentToken = lexer.next();
		if (currentToken != NULL) {
			input.push_back(currentToken);
		} else {
			std::cout << "Rejected, lexer unable to fully tokenize sentence" << std::endl;
			return new NodeTree<Symbol*>();
		}
	}

	std::cout << "\n\n\nDone with Lexing\n\n\n" << std::endl;

	
	for (int i = 0; i < input.size(); i++)
		std::cout << "|" << input[i]->toString() << "|";
	std::cout << std::endl;


	std::cout << "Setting up 0th frontier, first actions, toShift, toReduce" << std::endl;

	//Frontier 0, new node with state 0
	NodeTree<int>* v0 = gss.newNode(0);
	gss.addToFrontier(0,v0);

	std::cout << "Done setting up new frontier" << std::endl;

	std::vector<ParseAction*> firstActions = *(table.get(0, input[0]));
	for (std::vector<ParseAction*>::size_type i = 0; i < firstActions.size(); i++) {
		if (firstActions[i]->action == ParseAction::SHIFT)
			toShift.push(std::make_pair(v0,firstActions[i]->shiftState));
		else if (firstActions[i]->action == ParseAction::REDUCE && firstActions[i]->reduceRule->getRightSide().size() == 0) {
			toReduce.push(std::make_pair(std::make_pair(v0, firstActions[i]->reduceRule->getLeftSide()), 0));
		}
	}

	std::cout << "GSS:\n" << gss.toString() << std::endl;

	std::cout << "Starting parse loop" << std::endl;

	for (int i = 0; i < input.size(); i++) {
		std::cout << "Checking if frontier " << i << " is empty" << std::endl;
		if (gss.frontierIsEmpty(i)) {
			std::cout << "Frontier " << i << " is empty." << std::endl;
			std::cout << "Failed on " << input[i]->toString() << " next: " << input[i+1]->toString() << std::endl;
			break;
		}
		while (toReduce.size() != 0) {
			std::cout << "Reducing for " << i << std::endl;
			//std::cout << "GSS:\n" << gss.toString() << std::endl;
			reducer(i);
		}
		std::cout << "Shifting for " << i << std::endl;
		shifter(i);
		std::cout << "GSS:\n" << gss.toString() << std::endl;
	}
	std::cout << "Done with parsing loop, checking for acceptance" << std::endl;
	if (gss.frontierHasAccState(input.size()-1))
		std::cout << "Accepted!" << std::endl;
	else
		std::cout << "Rejected!" << std::endl;

	std::cout << "GSS:\n" << gss.toString() << std::endl;
	return new NodeTree<Symbol*>();
}

void RNGLRParser::reducer(int i) {
	std::pair< std::pair<NodeTree<int>*, Symbol*>, int > reduction = toReduce.front();
	toReduce.pop();
	std::cout << "Doing reduction of length " << reduction.second << " from state " << reduction.first.first->getData() << " to symbol " << reduction.first.second->toString() << std::endl;
	int pathLength = reduction.second > 0 ? reduction.second -1 : 0;
	std::vector<NodeTree<int>*>* reachable = gss.getReachable(reduction.first.first, pathLength);
	for (std::vector<NodeTree<int>*>::size_type j = 0; j < reachable->size(); j++) {
		NodeTree<int>* currentReached = (*reachable)[j];
		std::cout << "Getting the shfit state for state " << currentReached->getData() << " and symbol " << reduction.first.second->toString() << std::endl;
		int toState = table.getShift(currentReached->getData(), reduction.first.second)->shiftState;
		NodeTree<int>* toStateNode = gss.inFrontier(i, toState);
		if (toStateNode) {
			if (!gss.hasEdge(toStateNode, currentReached)) {
				gss.addEdge(toStateNode, currentReached);
				if (reduction.second != 0) {
					//Do all non null reduction
					std::vector<ParseAction*> actions = *(table.get(toState, input[i]));
					for (std::vector<ParseAction*>::size_type k = 0; k < actions.size(); k++)
						if (actions[k]->action == ParseAction::REDUCE && actions[k]->reduceRule->getRightSize() != 0)
							toReduce.push(std::make_pair(std::make_pair(currentReached, actions[k]->reduceRule->getLeftSide()), actions[k]->reduceRule->getRightSize()));
				}
			}
		} else {
			toStateNode = gss.newNode(toState);
			gss.addToFrontier(i, toStateNode);
			gss.addEdge(toStateNode, currentReached);
			
			std::vector<ParseAction*> actions = *(table.get(toState, input[i]));
			for (std::vector<ParseAction*>::size_type k = 0; k < actions.size(); k++) {
				//Shift
				if (actions[k]->action == ParseAction::SHIFT)
					toShift.push(std::make_pair(toStateNode, actions[k]->shiftState));
				else if (actions[k]->action == ParseAction::REDUCE && actions[k]->reduceRule->getRightSize() != 0)
					toReduce.push(std::make_pair(std::make_pair(currentReached, actions[k]->reduceRule->getLeftSide()), actions[k]->reduceRule->getRightSize()));
				else if (actions[k]->action == ParseAction::REDUCE && actions[k]->reduceRule->getRightSize() == 0)
					toReduce.push(std::make_pair(std::make_pair(toStateNode, actions[k]->reduceRule->getLeftSide()), actions[k]->reduceRule->getRightSize()));
			}
		}
	}
}

void RNGLRParser::shifter(int i) {
	if (i != input.size()-1) {
		std::queue< std::pair<NodeTree<int>*, int> > nextShifts;
		while (!toShift.empty()) {
			std::pair<NodeTree<int>*, int> shift = toShift.front();
			toShift.pop();
			std::cout << "Current potential shift from " << shift.first->getData() << " to " << shift.second << std::endl;
			NodeTree<int>* shiftTo = gss.inFrontier(i+1, shift.second);
			if (shiftTo) {
				std::cout << "State already existed, just adding edge" << std::endl;
				gss.addEdge(shiftTo, shift.first);
				std::vector<ParseAction*> actions = *(table.get(shift.second, input[i+1]));
				for (std::vector<ParseAction*>::size_type j = 0; j < actions.size(); j++) {
					if (actions[j]->action == ParseAction::REDUCE && actions[j]->reduceRule->getRightSize() != 0)
						toReduce.push(std::make_pair(std::make_pair(shift.first, actions[j]->reduceRule->getLeftSide()), actions[j]->reduceRule->getRightSize()));
				}
			} else {
				std::cout << "State did not already exist, adding" << std::endl;
				shiftTo = gss.newNode(shift.second);
				gss.addToFrontier(i+1, shiftTo);
				gss.addEdge(shiftTo, shift.first);
				std::vector<ParseAction*> actions = *(table.get(shift.second, input[i+1]));
				for (std::vector<ParseAction*>::size_type j = 0; j < actions.size(); j++) {
					std::cout << "Adding action " << actions[j]->toString() << " to either nextShifts or toReduce" << std::endl;
					//Shift
					if (actions[j]->action == ParseAction::SHIFT)
						nextShifts.push(std::make_pair(shiftTo, actions[j]->shiftState));
					else if (actions[j]->action == ParseAction::REDUCE && actions[j]->reduceRule->getRightSize() != 0)
						toReduce.push(std::make_pair(std::make_pair(shift.first, actions[j]->reduceRule->getLeftSide()), actions[j]->reduceRule->getRightSize()));
					else if (actions[j]->action == ParseAction::REDUCE && actions[j]->reduceRule->getRightSize() == 0)
						toReduce.push(std::make_pair(std::make_pair(shiftTo, actions[j]->reduceRule->getLeftSide()), actions[j]->reduceRule->getRightSize()));
				}
			}
		}
		toShift = nextShifts;
	}
}

//Have to use own add states function in order to construct RN table instead of LALR table
void RNGLRParser::addStates(std::vector< State* >* stateSets, State* state) {
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
		std::vector<Symbol*>* lookahead = (*currStateTotal)[i]->getLookahead();
		if ((*currStateTotal)[i]->isAtEnd()) {
			for (std::vector<Symbol*>::size_type j = 0; j < lookahead->size(); j++)
				table.add(stateNum(state), (*lookahead)[j], new ParseAction(ParseAction::REDUCE, (*currStateTotal)[i]));
		// } else if (*((*currStateTotal)[i]->getAtNextIndex()) == *nullSymbol) {
		//If this has an appropriate ruduction to null, get the reduce trees out
		} else if (reducesToNull((*currStateTotal)[i])) {
			std::cout << (*currStateTotal)[i]->toString() << " REDUCES TO NULL" << std::endl;
			//If is a rule that produces only NULL, add in the approprite reduction, but use a new rule with a right side of length 0. (so we don't pop off stack)
			ParseRule* nullRule = (*currStateTotal)[i]->clone();
			nullRule->setRightSide(* new std::vector<Symbol*>());
			for (std::vector<Symbol*>::size_type j = 0; j < lookahead->size(); j++)
				table.add(stateNum(state), (*lookahead)[j], new ParseAction(ParseAction::REDUCE, nullRule));
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
				(*stateSets)[j]->addParents(newStates[i]->getParents());
				table.add(stateNum(state), currStateSymbol, new ParseAction(ParseAction::SHIFT, j));
				break;
			}
		}
		if (!stateAlreadyInAllStates) {
			//If the state does not already exist, add it and add it as the shift/goto in the action table
			stateSets->push_back(newStates[i]);
			table.add(stateNum(state), currStateSymbol, new ParseAction(ParseAction::SHIFT, stateSets->size()-1));
		}
	}
}

bool RNGLRParser::reducesToNull(ParseRule* rule) {
	std::vector<Symbol*> avoidList;
	return reducesToNull(rule, avoidList);
}

bool RNGLRParser::reducesToNull(ParseRule* rule, std::vector<Symbol*> avoidList) {
	for (std::vector<Symbol*>::size_type i = 0; i < avoidList.size(); i++)
		if (*(rule->getLeftSide()) == *(avoidList[i]))
			return false;

	avoidList.push_back(rule->getLeftSide());

	std::vector<Symbol*> rightSide = rule->getRightSide();
	bool reduces = true;
	for (std::vector<Symbol*>::size_type i = 0; i < rightSide.size(); i++) {
		if (*rightSide[i] == *nullSymbol)
			continue;
		if (rightSide[i]->isTerminal()) {
			reduces = false;
			break;
		}
		bool subSymbolReduces = false;
		for (std::vector<ParseRule*>::size_type j = 0; j < loadedGrammer.size(); j++) {
			if (*(loadedGrammer[j]->getLeftSide()) == *(rightSide[i])) {
				if(reducesToNull(loadedGrammer[j], avoidList)) {
					subSymbolReduces = true;
					break;
				}
			}
		}
		if (!subSymbolReduces) {
			reduces = false;
			break;
		}
	}
	return reduces;
}

