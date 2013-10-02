#include "ASTData.h"

ASTData::ASTData(ASTType type, ValueType valueType) {
	this->type = type;
	this->valueType = valueType;
}

ASTData::ASTData(ASTType type, Symbol symbol, ValueType valueType) {
	this->type = type;
	this->valueType = valueType;
	this->symbol = symbol;
}


ASTData::~ASTData() {
}

std::string ASTData::toString() {
	return "ASTData!";
}
