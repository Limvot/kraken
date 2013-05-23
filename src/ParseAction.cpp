#include "ParseAction.h"

ParseAction::ParseAction(ActionType action, ParseRule* reduceRule, int shiftState) {
	this->action = action;
	this->reduceRule = reduceRule;
	this->shiftState = shiftState;
}

ParseAction::~ParseAction() {

}

std::string ParseAction::actionToString(ActionType action) {
	switch (action) {
		case REDUCE:
			return "reduce";
			break;
		case SHIFT:
			return "shift";
			break;
		case ACCEPT:
			return "accept";
			break;
		case REJECT:
			return "reject";
			break;
	}
}

std::string ParseAction::toString() {
	std::string outputString = actionToString(action);
	if (reduceRule)
		outputString += " " + reduceRule->toString();
	if (shiftState)
		outputString += " " + shiftState;
	return(outputString);
}