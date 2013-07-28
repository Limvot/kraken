
RNGLRParser::parseInput(std::string inputString) {

	//Check for no tokens
	if (inputString == "") {
		if (table.get(0,EOFSymbol)->action == ParseAction::REDUCE)
			std::cout << "Accepted!" << std::endl;
		else
			std::cout << "Rejected, no input (with no accepting state)" << std::endl;
		return;
	}

	lexer.setInput(inputString);
	//Now fully lex our input because this algorithm was designed in that manner and simplifies this first implementation.
	//It could be converted to on-line later.
	Symbol* currentToken = lexer.next();
	input.push_back(currentToken);
	while (*currentToken != *EOFToken) {
		currentToken = lexer.next();
		input.push_back(currentToken);
	}

	//Frontier 0, new node with state 0
	GSSNode* v0 = gss.newNode(0);
	gss.addToFrontier(0,v0);

	std::vector<ParseAction*> firstActions = table.get(0, input[0]);
	for (std::vector<ParseAction*>::size_type i = 0; i < firstActions.size(); i++) {
		if (firstActions[i]->action == ParseAction::SHIFT)
			toShift.push_back(std::make_pair(v0,firstActions[i]->toState()));
		else if (firstActions[i]->action == ParseAction::REDUCE && firstActions[i]->reduceRule->getRightSide()->size() == 0) {
			toReduce.push_back(std::make_pair(std::make_pair(v0, firstActions[i]->reduceRule->getLeftSide()), 0));
		}
	}

	for (int i = 0; i < input.size(); i++) {
		if (gss.frontierIsEmpty(i))
			break;
		while (toReduce.size() != 0)
			reducer(i);
		shifter(i);
	}
	if (gss.frontierHasAccSt(input.size()-1))
		std::cout << "Accepted!" << std::endl;
	else
		std::cout << "Rejected!" << std::endl;
	return;
}

RNGLRParser::reducer(int i) {
	std::pair< std::pair<GSSNode*, Symbol*>, int > reduction = toReduce.front();
	int pathLength = reduction.second > 0 : reduction.second -1 ? 0;
	std::vector<GSSNode*>* reachable = gss.getReachable(reduction.first.first, pathLength);
	for (std::vector<GSSNode*>::size_type j = 0; j < reachable->size(); j++) {
		GSSNode* currentReached = (*reachable)[j];
		int toState = table.getShift(currentReached->state(), reduction.first.second);
		GSSNode* toStateNode = gss.inFrontier(i, toState);
		if (toStateNode) {
			if (!gss.hasEdge(toStateNode, currentReached)) {
				gss.addEdge(toStateNode, currentReached);
				if (reduction.second != 0) {
					//Do all non null reductions
				}
			}
		} else {
			toStateNode = gss.newNode(toState);
			gss.addToFrontier(i, toStateNode);
			gss.addEdge(toStateNode, currentReached);
			
			std::vector<ParseAction*> actions = table.get(toState, input[i+1]);
			for (std::vector<ParseAction*>::size_type k = 0; k < actions.size(); k++) {
				//Shift
				if (actions[k]->action == ParseAction::SHIFT)
					nextShifts.push_back(std::make_pair(toStateNode, actions[k]->shiftState));
				else if (actions[k]->action == ParseAction::REDUCE && actions[k]->reduceRule()->size() != 0)
					toReduce.push_back(std::make_pair(std::make_pair(currentReached, actions[k]->reduceRule->getLeftSide()), actions[k]->reduceRule->size()));
				else (actions[k]->action == ParseAction::REDUCE && actions[k]->reduceRule()->size() == 0)
					toReduce.push_back(std::make_pair(std::make_pair(toStateNode, actions[k]->reduceRule->getLeftSide()), actions[k]->reduceRule->size()));
			}

		}
	}
}

RNGLRParser::shifter(int i) {
	if (i != input.length()-1) {
		std::queue<ParseAction*> nextShifts;
		while (!toShift.empty()) {
			std::pair<GSSNode*, int> shift = toShift.front();
			GSSNode* shiftTo = gss.inFrontier(i+1, shift.second);
			if (shiftTo) {
				gss.addEdge(shiftTo, shift.first);
				std::vector<ParseAction*> actions = table.get(shift.second, input[i+2]);
				for (std::vector<ParseAction*>::size_type j = 0; j < actions.size(); j++) {
					if (actions[j]->action == ParseAction::REDUCE && actions[j]->reduceRule->size() != 0)
						toReduce.push_back(std::make_pair(std::make_pair(shift.first, actions[j]->reduceRule->getLeftSide()), actions[j]->reduceRule->size()));
				}
			} else {
				shiftTo = gss.newNode(shift.second);
				gss.addToFrontier(i+1, shiftTo);
				gss.addEdge(shiftTo, shift.first);
				std::vector<ParseAction*> actions = table.get(shift.toState(), input[i+2]);
				for (std::vector<ParseAction*>::size_type j = 0; j < actions.size(); j++) {
					//Shift
					if (actions[j]->action == ParseAction::SHIFT)
						nextShifts.push_back(std::make_pair(shiftTo, actions[j]->shiftState));
					else if (actions[j]->action == ParseAction::REDUCE && actions[j]->reduceRule()->size() != 0)
						toReduce.push_back(std::make_pair(std::make_pair(shift.first, actions[j]->reduceRule->getLeftSide()), actions[j]->reduceRule->size()));
					else (actions[j]->action == ParseAction::REDUCE && actions[j]->reduceRule()->size() == 0)
						toReduce.push_back(std::make_pair(std::make_pair(shiftTo, actions[j]->reduceRule->getLeftSide()), actions[j]->reduceRule->size()));
				}
			}
		}
		toShift = nextShifts;
	}
}