#ifndef CGENERATOR_H
#define CGENERATOR_H

#include <string>
#include <iostream>

#include "NodeTree.h"
#include "ASTData.h"

#include "util.h"


class CGenerator {
	public:
		CGenerator();
		~CGenerator();
		std::string generate(NodeTree<ASTData>* from);
		static std::string ValueTypeToCType(ValueType type);
		std::string generatorString;
	private:
		std::string tabs();
		int tabLevel;
};
#endif