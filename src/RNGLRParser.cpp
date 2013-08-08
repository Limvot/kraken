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
		if (accepting) {
			std::cout << "Accepted!" << std::endl;
			return getNullableParts(stateSets[0]->getBasis()->operator[0]->getLeftSide());
		} else {
			std::cout << "Rejected, no input (with no accepting state)" << std::endl;
		}
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
			Reduction newReduction = {v0, firstActions[i]->reduceRule->getLeftSide(), 0, getNullableIndex(firstActions[i]->reduceRule), new NodeTree<Symbol*>("null", nullSymbol)}
			toReduce.push(newReduction);
			//toReduce.push(std::make_pair(std::make_pair(v0, firstActions[i]->reduceRule->getLeftSide()), 0));
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

		//Clear the vector of SPPF nodes created every step
		for (std::vector<NodeTree<Symbol*>*>::size_type j = 0; j < SPPFStepNodes.size(); j++)
			SPPFStepNodes[j] = NULL;
		SPPFStepNodes.clear();

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
	NodeTree<int>* accState = gss.frontierHasAccState(input.size()-1);
	if (accState) {
		std::cout << "Accepted!" << std::endl;
		return gss.getEdge(accState, v0);
	} else {
		std::cout << "Rejected!" << std::endl;
	}

	std::cout << "GSS:\n" << gss.toString() << std::endl;
	return NULL;
}

void RNGLRParser::reducer(int i) {
	Reduction reduction = toReduce.front();
	toReduce.pop();
	std::cout << "Doing reduction of length " << reduction.length << " from state " << reduction.from->getData() << " to symbol " << reduction.first.second->toString() << std::endl;
	int pathLength = reduction.length > 0 ? reduction.length -1 : 0;
	//Get every reachable path
	std::vector<std::vector<NodeTree<int>*> >* paths =  gss.getReachablePaths(reduction.from, pathLength);

	for (std::vector<std::vector<NodeTree<int>*> >::size_type j = 0; j < paths->size(); j++) {

		//Algorithm expects path in reverse order
		std::vector<NodeTree<int>*> currentPath = (*paths)[j];
		std::reverse(currentPath.begin(), currentPath.end());
		//Add label of first edge to the end, (since reversed, this is the correct place)

		//Get the edges for the current path
		std::vector<NodeTree<Symbol*>*> pathEdges = getPathEdges(currentPath);
		//If the reduction length is 0, label as passed in is null
		if (reduction.length != 0)
			pathEdges.push_back(reduction.label);
		//The end of the current path (remember reversed)
		NodeTree<int>* currentReached = currentPath[0];

		std::cout << "Getting the shfit state for state " << currentReached->getData() << " and symbol " << reduction.symbol->toString() << std::endl;
		int toState = table.getShift(currentReached->getData(), reduction.symbol)->shiftState;

		//If reduction length is 0, then we make the new label the appropriate nullable parts
		NodeTree<Symbol*>* newLabel = NULL;
		if (reduction.length == 0) {
			newLabel = getNullableParts(reduction.nullablePartsIndex);
		} else {
			//Otherwise, we create the new label if we haven't already
			int reachedFrontier = gss.getContainingFrontier(currentReached);
			for (std::vector<std::pair<NodeTree<Symbol*>*, int> >::size_type k = 0; k < SPPFStepNodes.size(); k++) {
				if ( SPPFStepNodes[k].second == reachedFrontier && *(SPPFStepNodes[k].first->data) == *(reduction.symbol)) {
					newLabel = SPPFStepNodes[k].first;
					break;
				}
			}
			if (!newLabel) {
				newLabel = new NodeTree<Symbol*>("frontier: " + intToString(reachedFrontier), reduction.symbol);
				SPPFStepNodes.push_back(std::make_pair(newLabel, reachedFrontier));
			}
		}

		NodeTree<int>* toStateNode = gss.inFrontier(i, toState);
		if (toStateNode) {
			if (!gss.hasEdge(toStateNode, currentReached)) {
				gss.addEdge(toStateNode, currentReached, newLabel);
				if (reduction.length != 0) {
					//Do all non null reduction
					std::cout << "Checking for non-null reductions in states that already existed" << std::endl;
					std::vector<ParseAction*> actions = *(table.get(toState, input[i]));
					for (std::vector<ParseAction*>::size_type k = 0; k < actions.size(); k++) {
						if (actions[k]->action == ParseAction::REDUCE && actions[k]->reduceRule->getRightSize() != 0) {
							Reduction newReduction = {currentReached, actions[k]->reduceRule->getLeftSide(), actions[k]->reduceRule->getRightSize(), getNullableIndex(actions[k]->reduceRule), newLabel}
							toReduce.push(newReduction);
						}
					}
				}
			}
		} else {
			toStateNode = gss.newNode(toState);
			gss.addToFrontier(i, toStateNode);
			gss.addEdge(toStateNode, currentReached, newLabel);
			
			std::cout << "Adding shifts and reductions for a state that did not exist" << std::endl;
			std::vector<ParseAction*> actions = *(table.get(toState, input[i]));
			for (std::vector<ParseAction*>::size_type k = 0; k < actions.size(); k++) {
				std::cout << "Action is " << actions[k]->toString() << std::endl;
				if (actions[k]->action == ParseAction::SHIFT) {
					toShift.push(std::make_pair(toStateNode, actions[k]->shiftState));
				} else if (actions[k]->action == ParseAction::REDUCE && actions[k]->reduceRule->getRightSize() == 0) {
					Reduction newReduction = {toStateNode, actions[k]->reduceRule->getLeftSide(), 0, getNullableIndex(actions[k]->reduceRule), new NodeTree<Symbol*>("null", nullSymbol)}
					toReduce.push(newReduction);
				} else if (reduction.length != 0 && actions[k]->action == ParseAction::REDUCE && actions[k]->reduceRule->getRightSize() != 0) {
					Reduction newReduction = {currentReached, actions[k]->reduceRule->getLeftSide(), actions[k]->reduceRule->getRightSize(), getNullableIndex(actions[k]->reduceRule), newLabel}
					toReduce.push(newReduction);
				}
			}
		}
		if (reduction.length != 0)
			addChildren(newLabel, &pathEdges, reduction.nullablePartsIndex);
	}
}

void RNGLRParser::shifter(int i) {
	if (i != input.size()-1) {
		std::queue< std::pair<NodeTree<int>*, int> > nextShifts;
		NodeTree<Symbol*> nextLabel = new NodeTree<Symbol*>("frontier: " + intToString(i), input[i]);
		while (!toShift.empty()) {
			std::pair<NodeTree<int>*, int> shift = toShift.front();
			toShift.pop();
			std::cout << "Current potential shift from " << shift.first->getData() << " to " << shift.second << std::endl;
			NodeTree<int>* shiftTo = gss.inFrontier(i+1, shift.second);
			if (shiftTo) {
				std::cout << "State already existed, just adding edge" << std::endl;
				gss.addEdge(shiftTo, shift.first, nextLabel);
				std::vector<ParseAction*> actions = *(table.get(shift.second, input[i+1]));
				for (std::vector<ParseAction*>::size_type j = 0; j < actions.size(); j++) {
					if (actions[j]->action == ParseAction::REDUCE && actions[j]->reduceRule->getRightSize() != 0) {
						Reduction newReduction = {shift.first, actions[j]->reduceRule->getLeftSide(), actions[j]->reduceRule->getRightSize(), getNullableIndex(actions[j]->reduceRule), newLabel}
						toReduce.push(newReduction);
					}
				}
			} else {
				std::cout << "State did not already exist, adding" << std::endl;
				shiftTo = gss.newNode(shift.second);
				gss.addToFrontier(i+1, shiftTo);
				gss.addEdge(shiftTo, shift.first, newLabel);
				std::vector<ParseAction*> actions = *(table.get(shift.second, input[i+1]));
				for (std::vector<ParseAction*>::size_type j = 0; j < actions.size(); j++) {
					std::cout << "Adding action " << actions[j]->toString() << " to either nextShifts or toReduce" << std::endl;
					//Shift
					if (actions[j]->action == ParseAction::SHIFT) {
						nextShifts.push(std::make_pair(shiftTo, actions[j]->shiftState));
					} else if (actions[j]->action == ParseAction::REDUCE && actions[j]->reduceRule->getRightSize() != 0) {
						Reduction newReduction = {shift.first, actions[j]->reduceRule->getLeftSide(), actions[j]->reduceRule->getRightSize(), getNullableIndex(actions[j]->reduceRule), newLabel}
						toReduce.push(newReduction);
					} else if (actions[j]->action == ParseAction::REDUCE && actions[j]->reduceRule->getRightSize() == 0) {
						Reduction newReduction = {shiftTo, actions[j]->reduceRule->getLeftSide(), 0, getNullableIndex(actions[j]->reduceRule), new NodeTree<Symbol*>("null", nullSymbol)}
						toReduce.push(newReduction);
					}
				}
			}
		}
		toShift = nextShifts;
	}
}

void RNGLRParser::addChildren(NodeTree<Symbol*>* parent, std::vector<NodeTree<Symbol*>*>* children, int nullablePartsIndex) {
	if (nullablePartsIndex != 0)
		children->push_back(getNullableParts(nullablePartsIndex);
	if (!belongsToFamily(parent, children)) {
		if (parent->getChildren().size() == 0) {
			parent->addChildren(children);
		} else {
			if (!arePacked(parent->getChildren())) {
				NodeTree<Symbol*>* subParent = new NodeTree<Symbol*>();
				setPacked(subParent, true);
				subParent->addChildren(&(parent->getChildren());
				parent->clearChildren();
				parent->addChild(subParent);
			}
			NodeTree<Symbol*>* t = new NodeTree<Symbol*>();
			setPacked(t, true);
			parent->addChild(t);
			t->addChildren(children);
		}
	}
}

bool RNGLRParser::belongsToFamily(NodeTree<Symbol*>* node, std::vector<NodeTree<Symbol*>*>* nodes) {
	std::vector<NodeTree<Symbol*>*> children = node->getChildren();
	for (std::vector<NodeTree<Symbol*>*>::size_type i = 0; i < nodes->size(); i++) {
		bool containsOne = false;
		for (std::vector<NodeTree<Symbol*>*>::size_type j = 0; j < children.size(); j++) {
			if ((*(*nodes)[i]) == *(children[j])) {
				containsOne = true;
				break;
			}
		}
		if (!containsOne) {
			return false;
		}
	}
	return true;
}

bool RNGLRParser::arePacked(std::vector<NodeTree<Symbol*>*>* nodes) {
	bool packed = true;
	for (std::vector<NodeTree<Symbol*>*>::size_type i = 0; i < nodes->size(); i++)
		packed &= packedMap[node];
	return packed;
}

bool RNGLRParser::isPacked(NodeTree<Symbol*>* node) {
	return packedMap[node];
}

void RNGLRParser::setPacked(NodeTree<Symbol*>* node, bool isPacked) {
	packedMap[node] = isPacked;
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

int RNGLRParser::getNullableIndex(ParseRule* rule) {
	return 1;
}

NodeTree<Symbol*> RNGLRParser::getNullableParts(ParseRule* rule) {
	return new NodeTree<Symbol*>("null", nullSymbol);
}

NodeTree<Symbol*> RNGLRParser::getNullableParts(Symbol* symbol) {
	return new NodeTree<Symbol*>("null", nullSymbol);
}

NodeTree<Symbol*> RNGLRParser::getNullableParts(int index) {
	if (index == 0)
		return new NodeTree<Symbol*>("not_null", nullSymbol);
	return new NodeTree<Symbol*>("null", nullSymbol);
}

std::vector<NodeTree<Symbol*>*> RNGLRParser::getPathEdges(std::vector<NodeTree<int>*> path) {
	std::vector<NodeTree<Symbol*>*> pathEdges;
	for (std::vector<NodeTree<int>*>::size_type i < path.size()-1; i++)
		pathEdges.push_back(gss.getEdge(path[i], path[i+1]));
	return pathEdges;
}

