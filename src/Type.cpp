#include "Type.h"

Type::Type() {
	indirection = 0;
	baseType = none;
	typeDefinition = nullptr;
	templateDefinition = nullptr;
    returnType = nullptr;
    templateInstantiated = false;
}

Type::Type(ValueType typeIn, int indirectionIn) {
	indirection = indirectionIn;
	baseType = typeIn;
	typeDefinition = nullptr;
	templateDefinition = nullptr;
    returnType = nullptr;
    templateInstantiated = false;
}

Type::Type(ValueType typeIn, std::set<std::string> traitsIn) {
	indirection = 0;
	baseType = typeIn;
    traits = traitsIn;
	typeDefinition = nullptr;
	templateDefinition = nullptr;
    returnType = nullptr;
    templateInstantiated = false;
}

Type::Type(NodeTree<ASTData>* typeDefinitionIn, int indirectionIn) {
	indirection = indirectionIn;
	baseType = none;
	typeDefinition = typeDefinitionIn;
	templateDefinition = nullptr;
    returnType = nullptr;
    templateInstantiated = false;
}

Type::Type(NodeTree<ASTData>* typeDefinitionIn, std::set<std::string> traitsIn) {
	indirection = 0;
	baseType = none;
	typeDefinition = typeDefinitionIn;
    traits = traitsIn;
	templateDefinition = nullptr;
    returnType = nullptr;
    templateInstantiated = false;
}

Type::Type(ValueType typeIn, NodeTree<ASTData>* typeDefinitionIn, int indirectionIn, std::set<std::string> traitsIn) {
	baseType = typeIn;
	indirection = indirectionIn;
	typeDefinition = typeDefinitionIn;
    traits = traitsIn;
    templateDefinition = nullptr;
    returnType = nullptr;
    templateInstantiated = false;
}

Type::Type(ValueType typeIn, NodeTree<ASTData>* typeDefinitionIn, int indirectionIn, std::set<std::string> traitsIn, std::vector<Type*> parameterTypesIn, Type* returnTypeIn) {
	baseType = typeIn;
	indirection = indirectionIn;
	typeDefinition = typeDefinitionIn;
    traits = traitsIn;
    templateDefinition = nullptr;
    parameterTypes = parameterTypesIn;
    returnType = returnTypeIn;
    templateInstantiated = false;
}
Type::Type(std::vector<Type*> parameterTypesIn, Type* returnTypeIn) {
	baseType = function_type;
	indirection = 0;
	typeDefinition = nullptr;
    templateDefinition = nullptr;
    parameterTypes = parameterTypesIn;
    returnType = returnTypeIn;
    templateInstantiated = false;
}

Type::Type(ValueType typeIn, NodeTree<Symbol>* templateDefinitionIn, std::set<std::string> traitsIn) {
	indirection = 0;
	baseType = typeIn;
	typeDefinition = nullptr;
	templateDefinition = templateDefinitionIn;
    traits = traitsIn;
    returnType = nullptr;
    templateInstantiated = false;
}


Type::~Type() {
}

const bool Type::operator==(const Type &other) const {
	bool first_part = ( baseType == other.baseType && indirection == other.indirection && typeDefinition == other.typeDefinition && templateDefinition == other.templateDefinition && other.traits == traits);
    if (!first_part)
        return false;
    if ((returnType && !other.returnType) || (!returnType && other.returnType))
        return false;
    if (returnType && other.returnType)
        if (*returnType != *other.returnType)
            return false;
    if (parameterTypes.size() != other.parameterTypes.size())
        return false;
    for (int i = 0; i < parameterTypes.size(); i++)
        if (*parameterTypes[i] != *other.parameterTypes[i])
            return false;
    return true;
}

const bool Type::operator!=(const Type &other) const {
	return(!this->operator==(other));
}
const bool Type::operator<(const Type &other) const {
    if (baseType != other.baseType)
        return baseType < other.baseType;
    if (indirection != other.indirection)
        return indirection < other.indirection;
    if (typeDefinition != other.typeDefinition)
        return typeDefinition < other.typeDefinition;
    if (templateDefinition != other.templateDefinition)
        return templateDefinition < other.templateDefinition;
    if (traits != other.traits)
        return traits < other.traits;
    if ((returnType && !other.returnType) || (!returnType && other.returnType))
        return returnType < other.returnType;
    if (returnType && other.returnType)
        if (*returnType != *other.returnType)
            return *returnType < *other.returnType;
    if (parameterTypes.size() != other.parameterTypes.size())
        return parameterTypes.size() < other.parameterTypes.size();
    for (int i = 0; i < parameterTypes.size(); i++)
        if (*parameterTypes[i] != *other.parameterTypes[i])
            return *parameterTypes[i] < *other.parameterTypes[i];
    return false;
}

std::string Type::toString(bool showTraits) {
	std::string typeString;
	switch (baseType) {
		case none:
			if (typeDefinition)
				typeString = typeDefinition->getDataRef()->symbol.getName();
			else
				typeString = "none";
			break;
		case template_type:
			typeString = "template: " + templateDefinition->getDataRef()->toString();
			break;
		case template_type_type:
			typeString = "template_type_type";
			break;
		case void_type:
			typeString = "void";
			break;
		case boolean:
			typeString = "bool";
			break;
		case integer:
			typeString = "int";
			break;
		case floating:
			typeString = "float";
			break;
		case double_percision:
			typeString = "double";
			break;
		case character:
			typeString = "char";
			break;
		case function_type:
			typeString = "function(";
            for (Type *param : parameterTypes)
                typeString += param->toString();
            typeString += "): " + returnType->toString();
			break;
		default:
			if (typeDefinition)
				typeString = typeDefinition->getDataRef()->symbol.getName();
			else
				typeString = "unknown_type";
	}
	for (int i = 0; i < indirection; i++)
		typeString += "*";
    if (indirection < 0)
		typeString += "negative indirection: " + intToString(indirection);
    if (traits.size() && showTraits) {
        typeString += "[ ";
        for (auto i : traits)
            typeString += i + " ";
        typeString += "]";
    }
	//std::cout << "Extra components of " << typeString << " are " << indirection << " " << typeDefinition << " " << templateDefinition << std::endl;
	return typeString;
}

Type* Type::clone() {
	return new Type(baseType, typeDefinition, indirection, traits, parameterTypes, returnType);
}

int Type::getIndirection() {
	return indirection;
}

void Type::setIndirection(int indirectionIn) {
	indirection = indirectionIn;
}

void Type::increaseIndirection() {
	setIndirection(indirection+1);
}
void Type::decreaseIndirection() {
	setIndirection(indirection-1);
}

void Type::modifyIndirection(int mod) {
	setIndirection(indirection + mod);
}

Type Type::withIncreasedIndirection() {
   Type *newOne = clone();
    newOne->increaseIndirection();
    return *newOne;
}
Type Type::withDecreasedIndirection() {
   Type *newOne = clone();
    newOne->decreaseIndirection();
    return *newOne;
}

