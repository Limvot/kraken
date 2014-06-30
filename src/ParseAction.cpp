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

const bool ParseAction::equalsExceptLookahead(const ParseAction &other) const {
	return( action == other.action && ( reduceRule == other.reduceRule || reduceRule->equalsExceptLookahead(*(other.reduceRule)) ) && shiftState == other.shiftState);
}

const bool ParseAction::operator==(const ParseAction &other) const {
	return( action == other.action && ( reduceRule == other.reduceRule || *reduceRule == *(other.reduceRule) ) && shiftState == other.shiftState);
}

const bool ParseAction::operator!=(const ParseAction &other) const {
	return !(this->operator==(other));
}

//Exists so we can put ParseActions into sets
const bool ParseAction::operator<(const ParseAction &other) const {
    if (action != other.action)
        return action < other.action;
    if (reduceRule != other.reduceRule) {
        if (! (reduceRule && other.reduceRule)) {
           return reduceRule < other.reduceRule;
        } else {
            return *reduceRule < *(other.reduceRule);
        }
    }

    return shiftState < other.shiftState;
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
		default:
			return "INVALID PARSE ACTION";
	}
}

std::string ParseAction::toString(bool printRuleLookahead) {
	std::string outputString = "";
	outputString += actionToString(action);
	if (reduceRule != NULL)
		outputString += " " + reduceRule->toString(printRuleLookahead);
	if (shiftState != -1)
		outputString += " " + intToString(shiftState);
	return(outputString);
}
