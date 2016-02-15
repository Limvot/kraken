#include "RNGLRParser.h"
#include <fstream>

//sorry about the macros
#define RESET "\033[0m"
#define BOLDRED "\033[1m\033[31m"
#define BOLDWHITE "\033[1m\033[37m"
#define BOLDGREEN "\033[1m\033[32m"
#define BOLDYELLOW "\033[1m\033[33m"
#define BOLDBLUE "\033[1m\033[34m"
#define BOLDMAGENTA "\033[1m\033[35m"
#define BOLDCYAN "\033[1m\033[36m"

RNGLRParser::RNGLRParser() {
	//
}

RNGLRParser::~RNGLRParser() {
	//
}

void RNGLRParser::printReconstructedFrontier(int frontier) {
    std::vector<int> lastFrontier = gss.getFrontier(frontier);
    for (int j = 0; j < lastFrontier.size(); j++) {
        std::cout << "State: " << lastFrontier[j] << std::endl;
        std::vector<std::pair<std::string, ParseAction>> stateParseActions = table.stateAsParseActionVector(lastFrontier[j]);
        std::set<std::pair<std::string, ParseAction>> noRepeats;
        for (auto k : stateParseActions)
            noRepeats.insert(k);
        for (auto k : noRepeats)
            std::cout << k.first << " " << k.second.toString(false) << std::endl;
        std::cout << std::endl;
    }
}

NodeTree<Symbol>* RNGLRParser::parseInput(std::string inputString, std::string filename, bool highlight_errors) {
	input.clear();
	gss.clear();
	while(!toReduce.empty()) toReduce.pop();
	while(!toShift.empty()) toReduce.pop();
	SPPFStepNodes.clear();
	nullableParts.clear();
	packedMap.clear();
    bool errord = false;

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
			return getNullableParts((*(stateSets[0]->getBasis()))[0]->getLeftSide());
		} else {
			std::cerr << "Rejected, no input (with no accepting state)" << std::endl;
		}
		return new NodeTree<Symbol>();
	}

	lexer.reset();
	lexer.setInput(inputString);
	//Now fully lex our input because this algorithm was designed in that manner and simplifies this first implementation.
	//It could be converted to on-line later.
    int tokenNum = 1;
	Symbol currentToken = lexer.next();
	input.push_back(currentToken);
	while (currentToken != EOFSymbol) {
		currentToken = lexer.next();
		//std::cout << "CurrentToken is " << currentToken.toString() << std::endl;
		if (currentToken == invalidSymbol) {
			std::cerr << filename << ":" << findLine(tokenNum) << std::endl;
            errord = true;
            std::cerr << "lex error" << std::endl;
			std::cerr << "Invalid Symbol!" << std::endl;
			throw "Invalid Symbol, cannot lex";
		}
		input.push_back(currentToken);
        tokenNum++;
	}

	// std::cout << "\nDone with Lexing, length:" << input.size() << std::endl;
	// std::cout << input[0].toString() << std::endl;


	// for (int i = 0; i < input.size(); i++)
	// 	std::cout << "|" << input[i]->toString() << "|";
	// std::cout << std::endl;


	//std::cout << "Setting up 0th frontier, first actions, toShift, toReduce" << std::endl;

	//Frontier 0, new node with state 0
	NodeTree<int>* v0 = gss.newNode(0);
	gss.addToFrontier(0,v0);

	//std::cout << "Done setting up new frontier" << std::endl;

	std::vector<ParseAction*> firstActions = *(table.get(0, input[0]));
	for (std::vector<ParseAction*>::size_type i = 0; i < firstActions.size(); i++) {
		if (firstActions[i]->action == ParseAction::SHIFT)
			toShift.push(std::make_pair(v0,firstActions[i]->shiftState));
		else if (firstActions[i]->action == ParseAction::REDUCE && fullyReducesToNull(firstActions[i]->reduceRule)) {
			Reduction newReduction = {v0, firstActions[i]->reduceRule->getLeftSide(), 0, getNullableParts(firstActions[i]->reduceRule), NULL};
			toReduce.push(newReduction);
		}
	}

	// std::cout << "GSS:\n" << gss.toString() << std::endl;

	//std::cout << "Starting parse loop" << std::endl;

	for (int i = 0; i < input.size(); i++) {
		// std::cout << "Checking if frontier " << i << " is empty" << std::endl;
        if (gss.frontierIsEmpty(i)) {
            //std::cout << "Frontier " << i << " is empty." << std::endl;
            //std::cerr << "Parsing failed on " << input[i].toString() << std::endl;
            //std::cerr << "Problem is on line: " << findLine(i) << std::endl;
            //			std::cerr << filename << ":" << findLine(i) << std::endl;
            errord = true;
            if (highlight_errors)
                std::cout << BOLDBLUE;
            std::cout << filename  << ":" << findLine(i) << std::endl;
            if (highlight_errors)
                std::cout << BOLDMAGENTA;
            std::cout << ": parse error" << std::endl;

            std::ifstream infile(filename);
            std::string line;
            int linecount = 0;
            while(std::getline(infile,line))
            {
                if(linecount == findLine(i) - 1) {
                    if (highlight_errors)
                        std::cout << BOLDRED;
                    std::cout << line << std::endl;
                }
                linecount++;
            }
            if (highlight_errors)
                std::cout << RESET << std::endl;

            break;
        }

		//Clear the vector of SPPF nodes created every step
		SPPFStepNodes.clear();

		while (toReduce.size() != 0) {
			//std::cout << "Reducing for " << i << std::endl;
			//std::cout << "GSS:\n" << gss.toString() << std::endl;
			reducer(i);
		}
		// std::cout << "Shifting for " << i << std::endl;
		shifter(i);
		//std::cout << "GSS:\n" << gss.toString() << std::endl;
	}
	//std::cout << "Done with parsing loop, checking for acceptance" << std::endl;
	NodeTree<int>* accState = gss.frontierGetAccState(input.size()-1);
	if (accState) {
		std::cout << "Accepted!" << std::endl;
		return gss.getEdge(accState, v0);
	}

    if (!errord) {
        std::cerr << filename << ":" << findLine(input.size())-2 << std::endl;
        std::cerr << "parse error" << std::endl;
        std::cerr << "Nearby is:" << std::endl;
    }

	std::cerr << "Rejected!" << std::endl;
	// std::cout << "GSS:\n" << gss.toString() << std::endl;
	return NULL;
}

void RNGLRParser::reducer(int i) {
	Reduction reduction = toReduce.front();
	toReduce.pop();
	//std::cout << "Doing reduction of length " << reduction.length << " from state " << reduction.from->getData() << " to symbol " << reduction.symbol->toString() << std::endl;
	int pathLength = reduction.length > 0 ? reduction.length -1 : 0;
	//Get every reachable path
	std::vector<std::vector<NodeTree<int>*> >* paths =  gss.getReachablePaths(reduction.from, pathLength);

	for (std::vector<std::vector<NodeTree<int>*> >::size_type j = 0; j < paths->size(); j++) {

		std::vector<NodeTree<int>*> currentPath = (*paths)[j];

		//Get the edges for the current path
		std::vector<NodeTree<Symbol>*> pathEdges = getPathEdges(currentPath);
		std::reverse(pathEdges.begin(), pathEdges.end());
		//If the reduction length is 0, label as passed in is null
		if (reduction.length != 0)
			pathEdges.push_back(reduction.label);
		//The end of the current path
		NodeTree<int>* currentReached = currentPath[currentPath.size()-1];

		//std::cout << "Getting the shift state for state " << currentReached->getData() << " and symbol " << reduction.symbol.toString() << std::endl;
		int toState = table.getShift(currentReached->getData(), reduction.symbol)->shiftState;

		//If reduction length is 0, then we make the new label the appropriate nullable parts
		NodeTree<Symbol>* newLabel = NULL;
		if (reduction.length == 0) {
			newLabel = reduction.nullableParts;
		} else {
			//Otherwise, we create the new label if we haven't already
			int reachedFrontier = gss.getContainingFrontier(currentReached);
			for (std::vector<std::pair<NodeTree<Symbol>*, int> >::size_type k = 0; k < SPPFStepNodes.size(); k++) {
				if ( SPPFStepNodes[k].second == reachedFrontier && SPPFStepNodes[k].first->getData() == reduction.symbol) {
					newLabel = SPPFStepNodes[k].first;
					break;
				}
			}
			if (!newLabel) {
				newLabel = new NodeTree<Symbol>("frontier: " + intToString(reachedFrontier), reduction.symbol);
				SPPFStepNodes.push_back(std::make_pair(newLabel, reachedFrontier));
			}
		}

		NodeTree<int>* toStateNode = gss.inFrontier(i, toState);
		if (toStateNode) {
			if (!gss.hasEdge(toStateNode, currentReached)) {
				gss.addEdge(toStateNode, currentReached, newLabel);
				if (reduction.length != 0) {
					//Do all non null reduction
					//std::cout << "Checking for non-null reductions in states that already existed" << std::endl;
					std::vector<ParseAction*> actions = *(table.get(toState, input[i]));
					for (std::vector<ParseAction*>::size_type k = 0; k < actions.size(); k++) {
						if (actions[k]->action == ParseAction::REDUCE && !fullyReducesToNull(actions[k]->reduceRule)) {
							Reduction newReduction = {currentReached, actions[k]->reduceRule->getLeftSide(), actions[k]->reduceRule->getIndex(), getNullableParts(actions[k]->reduceRule), newLabel};
							toReduce.push(newReduction);
						}
					}
				}
			}
		} else {
			toStateNode = gss.newNode(toState);
			gss.addToFrontier(i, toStateNode);
			gss.addEdge(toStateNode, currentReached, newLabel);

			//std::cout << "Adding shifts and reductions for a state that did not exist" << std::endl;
			std::vector<ParseAction*> actions = *(table.get(toState, input[i]));
			for (std::vector<ParseAction*>::size_type k = 0; k < actions.size(); k++) {
				//std::cout << "Action is " << actions[k]->toString() << std::endl;
				if (actions[k]->action == ParseAction::SHIFT) {
					toShift.push(std::make_pair(toStateNode, actions[k]->shiftState));
				} else if (actions[k]->action == ParseAction::REDUCE && fullyReducesToNull(actions[k]->reduceRule)) {
					Reduction newReduction = {toStateNode, actions[k]->reduceRule->getLeftSide(), 0, getNullableParts(actions[k]->reduceRule), NULL};
					toReduce.push(newReduction);
				} else if (reduction.length != 0 && actions[k]->action == ParseAction::REDUCE && !fullyReducesToNull(actions[k]->reduceRule)) {
					Reduction newReduction = {currentReached, actions[k]->reduceRule->getLeftSide(), actions[k]->reduceRule->getIndex(), getNullableParts(actions[k]->reduceRule), newLabel};
					toReduce.push(newReduction);
				}
			}
		}
		if (reduction.length != 0)
			addChildren(newLabel, &pathEdges, reduction.nullableParts);
	}
}

void RNGLRParser::shifter(int i) {
	if (i != input.size()-1) {
		std::queue< std::pair<NodeTree<int>*, int> > nextShifts;
		NodeTree<Symbol>* newLabel = new NodeTree<Symbol>("frontier: " + intToString(i), input[i]);
		while (!toShift.empty()) {
			std::pair<NodeTree<int>*, int> shift = toShift.front();
			toShift.pop();
			//std::cout << "Current potential shift from " << shift.first->getData() << " to " << shift.second << std::endl;
			NodeTree<int>* shiftTo = gss.inFrontier(i+1, shift.second);
			if (shiftTo) {
				//std::cout << "State already existed, just adding edge" << std::endl;
				gss.addEdge(shiftTo, shift.first, newLabel);
				std::vector<ParseAction*> actions = *(table.get(shift.second, input[i+1]));
				for (std::vector<ParseAction*>::size_type j = 0; j < actions.size(); j++) {
					if (actions[j]->action == ParseAction::REDUCE && !fullyReducesToNull(actions[j]->reduceRule)) {
						Reduction newReduction = {shift.first, actions[j]->reduceRule->getLeftSide(), actions[j]->reduceRule->getIndex(), getNullableParts(actions[j]->reduceRule), newLabel};
						toReduce.push(newReduction);
					}
				}
			} else {
				//std::cout << "State did not already exist, adding" << std::endl;
				shiftTo = gss.newNode(shift.second);
				gss.addToFrontier(i+1, shiftTo);
				gss.addEdge(shiftTo, shift.first, newLabel);
				std::vector<ParseAction*> actions = *(table.get(shift.second, input[i+1]));
				for (std::vector<ParseAction*>::size_type j = 0; j < actions.size(); j++) {
					//std::cout << "Adding action " << actions[j]->toString() << " to either nextShifts or toReduce" << std::endl;
					//Shift
					if (actions[j]->action == ParseAction::SHIFT) {
						nextShifts.push(std::make_pair(shiftTo, actions[j]->shiftState));
					} else if (actions[j]->action == ParseAction::REDUCE && !fullyReducesToNull(actions[j]->reduceRule)) {
						Reduction newReduction = {shift.first, actions[j]->reduceRule->getLeftSide(), actions[j]->reduceRule->getIndex(), getNullableParts(actions[j]->reduceRule), newLabel};
						toReduce.push(newReduction);
					} else if (actions[j]->action == ParseAction::REDUCE && fullyReducesToNull(actions[j]->reduceRule)) {
						Reduction newReduction = {shiftTo, actions[j]->reduceRule->getLeftSide(), 0, getNullableParts(actions[j]->reduceRule), NULL};
						toReduce.push(newReduction);
					}
				}
			}
		}
		toShift = nextShifts;
	}
}

void RNGLRParser::addChildren(NodeTree<Symbol>* parent, std::vector<NodeTree<Symbol>*>* children, NodeTree<Symbol>* nullableParts) {
	if (nullableParts)
		children->push_back(nullableParts);

	if (!belongsToFamily(parent, children)) {
		if (parent->getChildren().size() == 0) {
			parent->addChildren(children);
		} else {
			if (!arePacked(parent->getChildren())) {
				NodeTree<Symbol>* subParent = new NodeTree<Symbol>("AmbiguityPackInner", Symbol("AmbiguityPackInner", true));
				setPacked(subParent, true);
				std::vector<NodeTree<Symbol>*> tmp = parent->getChildren();
				subParent->addChildren(&tmp);
				parent->clearChildren();
				parent->addChild(subParent);
			}
			NodeTree<Symbol>* t = new NodeTree<Symbol>("AmbiguityPackOuter", Symbol("AmbiguityPackInner", true));
			setPacked(t, true);
			parent->addChild(t);
			t->addChildren(children);
		}
	}
}

bool RNGLRParser::belongsToFamily(NodeTree<Symbol>* node, std::vector<NodeTree<Symbol>*>* nodes) {
	//std::cout << "Checking " << node->getData()->toString() << "'s family" << std::endl;
	std::vector<NodeTree<Symbol>*> children = node->getChildren();
	for (std::vector<NodeTree<Symbol>*>::size_type i = 0; i < nodes->size(); i++) {
		bool containsOne = false;
		for (std::vector<NodeTree<Symbol>*>::size_type j = 0; j < children.size(); j++) {
			//Not sure where null comes from. For right now, just check to be sure we don't segfault
			if ((*nodes)[i] == children[j] || ( (*nodes)[i] != NULL && children[j] != NULL && (*(*nodes)[i]) == *(children[j]) )) {
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

bool RNGLRParser::arePacked(std::vector<NodeTree<Symbol>*> nodes) {
	bool packed = true;
	for (std::vector<NodeTree<Symbol>*>::size_type i = 0; i < nodes.size(); i++)
		packed &= packedMap[*(nodes[i])];
	return packed;
}

bool RNGLRParser::isPacked(NodeTree<Symbol>* node) {
	return packedMap[*node];
}

void RNGLRParser::setPacked(NodeTree<Symbol>* node, bool isPacked) {
	packedMap[*node] = isPacked;
}


//Have to use own add states function in order to construct RN table instead of LALR table
void RNGLRParser::addStates(std::vector< State* >* stateSets, State* state, std::queue<State*>* toDo) {
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
					//Add rule to state, combining with idenical rule except lookahead if exists
					newStates[j]->addRuleCombineLookahead(advancedRule);
					//We found a state with the same symbol, so stop searching
					break;
				}
			}
			if (!symbolAlreadyInState) {
				State* newState = new State(stateSets->size()+newStates.size(),advancedRule, state);
				newStates.push_back(newState);
			}
		} else {
            delete advancedRule;
        }
	}
	//Put all our new states in the set of states only if they're not already there.
	bool stateAlreadyInAllStates = false;
	Symbol currStateSymbol;
	for (std::vector< State * >::size_type i = 0; i < newStates.size(); i++) {
		stateAlreadyInAllStates = false;
		currStateSymbol = (*(newStates[i]->getBasis()))[0]->getAtIndex();
		for (std::vector< State * >::size_type j = 0; j < stateSets->size(); j++) {
			if (newStates[i]->basisEqualsExceptLookahead(*((*stateSets)[j]))) {
			//if (newStates[i]->basisEquals(*((*stateSets)[j]))) {
				stateAlreadyInAllStates = true;
				//If it does exist, we should add it as the shift/goto in the action table
				//std::cout << "newStates[" << i << "] == stateSets[" << j << "]" << std::endl;

				if (!((*stateSets)[j]->basisEquals(*(newStates[i]))))
					toDo->push((*stateSets)[j]);

				(*stateSets)[j]->combineStates(*(newStates[i]));
				//std::cout << j << "\t Hay, doing an inside loop state reductions!" << std::endl;
				addStateReductionsToTable((*stateSets)[j]);
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
	addStateReductionsToTable(state);
}


void RNGLRParser::addStateReductionsToTable(State* state) {
	std::vector<ParseRule*> currStateTotal = state->getTotal();
	//std::cout << currStateTotal->size()  << "::" << state->getNumber() << std::endl;
	for (std::vector<ParseRule*>::size_type i = 0; i < currStateTotal.size(); i++) {
		//See if reduce
		//Also, this really only needs to be done for the state's basis, but we're already iterating through, so...
		std::vector<Symbol> lookahead = currStateTotal[i]->getLookahead();
		if (currStateTotal[i]->isAtEnd()) {
			for (std::vector<Symbol>::size_type j = 0; j < lookahead.size(); j++) {
				table.add(stateNum(state), lookahead[j], new ParseAction(ParseAction::REDUCE, currStateTotal[i]));
			}
		//If this has an appropriate ruduction to null, get the reduce trees out
		} else if (reducesToNull(currStateTotal[i])) {
			//std::cout << (*currStateTotal)[i]->toString() << " REDUCES TO NULL" << std::endl;
			//It used to be that if is a rule that produces only NULL, add in the approprite reduction, but use a new rule with a right side that is equal to
			//the part that we've already gone through in the rule. (so we don't pop extra off stack)
			//Now we use the same rule and make sure that the index location is used
			for (std::vector<Symbol>::size_type j = 0; j < lookahead.size(); j++)
				table.add(stateNum(state), lookahead[j], new ParseAction(ParseAction::REDUCE, currStateTotal[i]));
		}
	}
}

bool RNGLRParser::fullyReducesToNull(ParseRule* rule) {
	return rule->getIndex() == 0 && reducesToNull(rule);
}

bool RNGLRParser::reducesToNull(ParseRule* rule) {
    auto itr = reduceToNullMap.find(rule);
    if (itr != reduceToNullMap.end())
        return itr->second;
	std::vector<Symbol> avoidList;
	auto val = reducesToNull(rule, avoidList);
    reduceToNullMap[rule] = val;
	return val;
}

bool RNGLRParser::reducesToNull(ParseRule* rule, std::vector<Symbol> avoidList) {
	//If the rule is completed and not null, it doesn't reduce to null, it's just completed.
	if (rule->isAtEnd() && rule->getRightSize() != 0)
		return false;

	for (std::vector<Symbol>::size_type i = 0; i < avoidList.size(); i++)
		if (rule->getLeftSide() == avoidList[i])
			return false;

	avoidList.push_back(rule->getLeftSide());

	std::vector<Symbol> rightSide = rule->getRightSide();
	bool reduces = true;
	for (std::vector<Symbol>::size_type i = rule->getIndex(); i < rightSide.size(); i++) {
		if (rightSide[i] == nullSymbol)
			continue;
		if (rightSide[i].isTerminal()) {
			reduces = false;
			break;
		}
		bool subSymbolReduces = false;
		for (std::vector<ParseRule*>::size_type j = 0; j < loadedGrammer.size(); j++) {
			if (loadedGrammer[j]->getLeftSide() == rightSide[i]) {
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

NodeTree<Symbol>* RNGLRParser::getNullableParts(ParseRule* rule) {
	return getNullableParts(rule, std::vector<NodeTree<Symbol>*>());
}

NodeTree<Symbol>* RNGLRParser::getNullableParts(ParseRule* rule, std::vector<NodeTree<Symbol>*> avoidList) {
	if (reducesToNull(rule)) {
		//std::cout << "Reduces to null so adding parts " << rule->toString() << std::endl;
		Symbol symbol = rule->getLeftSide();
		NodeTree<Symbol>* symbolNode = new NodeTree<Symbol>(symbol.getName(), symbol);
		if (rule->getAtNextIndex() == nullSymbol) {
			symbolNode->addChild(new NodeTree<Symbol>(nullSymbol.getName(), nullSymbol));
		} else {
			//Find recursively
			ParseRule* iterate = rule->clone();
			while (!iterate->isAtEnd()) {
				//Check to see if we've done this symbol already, if so use it
				for (std::vector<NodeTree<Symbol>*>::size_type i = 0; i < avoidList.size(); i++) {
					if (iterate->getAtNextIndex() == avoidList[i]->getData()) {
						symbolNode->addChild(avoidList[i]);
						break;
					}
				}
				//We haven't so do it recursively
				for (std::vector<ParseRule*>::size_type i = 0; i < loadedGrammer.size(); i++) {
					if (fullyReducesToNull(loadedGrammer[i]) && iterate->getAtNextIndex() == loadedGrammer[i]->getLeftSide()) {
						NodeTree<Symbol>* symbolTree = getNullableParts(loadedGrammer[i], avoidList);
						avoidList.push_back(symbolTree);
						symbolNode->addChild(symbolTree);
					}
				}
				iterate->advancePointer();
			}
		}
		return symbolNode;
	}
	return NULL;
}

NodeTree<Symbol>* RNGLRParser::getNullableParts(Symbol symbol) {
	return new NodeTree<Symbol>("CRAZY_SYMBOL", nullSymbol);
}

std::vector<NodeTree<Symbol>*> RNGLRParser::getPathEdges(std::vector<NodeTree<int>*> path) {
	std::vector<NodeTree<Symbol>*> pathEdges;
	for (std::vector<NodeTree<int>*>::size_type i = 0; i < path.size()-1; i++)
		pathEdges.push_back(gss.getEdge(path[i], path[i+1]));
	return pathEdges;
}

int RNGLRParser::findLine(int tokenNum) {
	int lineNo = 1;
	for (int i = 0; i < tokenNum; i++) {
		std::string tokenString = input[i].getValue();
		for (int j = 0; j < tokenString.size(); j++)
			if (tokenString[j] == '\n')
				lineNo++;
	}
	return lineNo;
}
