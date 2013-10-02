#ifndef ASTTRANSFORMATION_H
#define ASTTRANSFORMATION_H

#include "ASTData.h"
#include "NodeTransformation.h"

class ASTTransformation: public NodeTransformation<Symbol,ASTData> {
	public:
		ASTTransformation();
		~ASTTransformation();
		virtual NodeTree<ASTData>* transform(NodeTree<Symbol>* from);

	private:
		//Nothing
};
#endif
