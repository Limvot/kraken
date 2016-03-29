#include <iostream>
#include <string>

#include <stdlib.h>

#include "util.h"

#ifndef TESTER_H
#define TESTER_H

class Tester {
	public:
		Tester(std::string krakenInvocation, std::string krakenGrammerLocation);
		~Tester();
		bool run(std::string fileName);
		bool compareFiles(std::string file1Path, std::string file2Path);
		void cleanExtras(std::string path);

	private:
		std::string krakenInvocation;
		std::string krakenGrammerLocation;
		std::string removeCmd;
		std::string resultsExtention;
		std::string expectedExtention;
		std::string krakenExtention;
		std::string shell;
		std::string changePermissions;
		std::string redirect;
		std::string sep;
		std::string cd;
};
#endif
