#include "LALRParser.h"

LALRParser::LALRParser() {
	//Nothing to do in this version
}
LALRParser::~LALRParser() {
	//Nothing to do in this version
}

NodeTree<Symbol*>* LALRParser::parseInput(std::string inputString) {
	lexer.setInput(inputString);
	Symbol* token = lexer.next();
	std::vector<ParseAction*>* actionList;
	ParseAction* action;

	stateStack.push(0);
	symbolStack.push(new Symbol("INVALID", false));

	while (true) {
		std::cout << "In state: " << intToString(stateStack.top()) << std::endl;
		actionList = table.get(stateStack.top(), token);
		action = (*(actionList))[actionList->size()-1];
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
				std::cout << "top of state is " << intToString(stateStack.top()) << " symbolStack top is " << symbolStack.top()->toString() << std::endl;
		
				actionList = table.get(stateStack.top(), symbolStack.top());
				action = (*(actionList))[actionList->size()-1];
		
				stateStack.push(action->shiftState);
				//std::cout << "Reduced, now condition is" << std::endl;
				//std::cout << "top of state is " << intToString(stateStack.top()) << " symbolStack top is " << symbolStack.top()->toString() << std::endl;
				break;
			}
			case ParseAction::SHIFT:
				std::cout << "Shift " << token->toString() << std::endl;

				symbolStack.push(token);
				token = lexer.next();
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
			default:
				std::cout << "INVALID PARSE ACTION!" << std::endl;
				break;
		}
	}
}