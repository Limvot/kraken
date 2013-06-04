#include "ParseAction.h"

ParseAction::ParseAction(ActionType action) {
	this->action = action;
	this->reduceRule = NULL;
	this->shiftState = -1;
}

ParseAction::ParseAction(ActionType action, ParseRule* reduceRule) {
	this->action = action;
	this->reduceRule = reduceRule;
	this->shiftState = -1;
}

ParseAction::ParseAction(ActionType action, int shiftState) {
	this->action = action;
	this->reduceRule = NULL;
	this->shiftState = shiftState;
}

ParseAction::~ParseAction() {

}


const bool ParseAction::operator==(const ParseAction &other) {
	return( action == other.action && ( reduceRule == other.reduceRule || *reduceRule == *(other.reduceRule) ) && shiftState == other.shiftState);
}

const bool ParseAction::operator!=(const ParseAction &other) {
	return !(this->operator==(other));
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
	std::string outputString = "";
	outputString += actionToString(action);
	if (reduceRule != NULL)
		outputString += " " + reduceRule->toString(); 
	if (shiftState != -1)
		outputString += " " + intToString(shiftState); 
	return(outputString);
}