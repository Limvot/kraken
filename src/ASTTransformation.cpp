#include "ASTTransformation.h"

ASTTransformation::ASTTransformation() {
	//
}

ASTTransformation::~ASTTransformation() {
	//
}

virtual NodeTree<Symbol*>* ASTTransformation::transform(NodeTree<ASTData>* from) {
	return NULL;
}
