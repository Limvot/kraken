#ifndef ASTTRANSFORMATION_H
#define ASTTRANSFORMATION_H

#include "NodeTransformation.h"

class ASTTransformation: public Transformation<Symbol*,ASTData> {
	public:
		ASTTransformation();
		~ASTTransformation();
		virtual NodeTree<Symbol*>* transform(NodeTree<ASTData>* from);

	private:
		//Nothing
};

#endif