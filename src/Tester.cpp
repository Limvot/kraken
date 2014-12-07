#include "Tester.h"

Tester::Tester(std::string krakenInvocation, std::string krakenGrammerLocation) : krakenInvocation(krakenInvocation), krakenGrammerLocation(krakenGrammerLocation) {
	//initlization list
	removeCmd = "rm";
	resultsExtention = ".results";
	expectedExtention = ".expected_results";
	krakenExtention = ".krak";
	changePermissions = "chmod 755";
	shell = "sh";
	redirect = ">";
}

Tester::~Tester() {
	//Nothing
}

int Tester::ssystem(std::string command) {
	return system(command.c_str());
}

void Tester::cleanExtras(std::string fileName) {
	ssystem(removeCmd + " " + fileName);
	ssystem(removeCmd + " " + fileName + krakenExtention + "out*");
	ssystem(removeCmd + " " + fileName + krakenExtention + ".c");
	ssystem(removeCmd + " " + fileName + ".sh");
	ssystem(removeCmd + " " + fileName + resultsExtention);
}

bool Tester::run(std::string fileName) {
	std::cout << "Testing: " << fileName << " with " << krakenInvocation << " and " << krakenGrammerLocation << std::endl;
	
	cleanExtras(fileName);
	
	ssystem(changePermissions + " " + fileName);
	ssystem(krakenInvocation + " " + fileName + krakenExtention + " " + krakenGrammerLocation + " " + fileName);
	ssystem(shell + " " + fileName + ".sh");
	ssystem(fileName + " " + redirect + " " + fileName + resultsExtention);

	bool result = compareFiles(fileName + expectedExtention, fileName + resultsExtention);
	
	//If the test was succesful, we don't need all the extra files
	if (result)
		cleanExtras(fileName);

	return result;
}

bool Tester::compareFiles(std::string file1Path, std::string file2Path) {
	std::ifstream file1, file2;
	file1.open(file1Path);
	if (!file1.is_open()) {
		std::cout << file1Path << " could not be opened!" << std::endl;
		return false;
	}
	file2.open(file2Path);
	if (!file2.is_open()) {
		std::cout << file2Path << " could not be opened!" << std::endl;
		return false;
	}

	std::string file1contents = readFile(file1);
	std::string file2contents = readFile(file2);

	// std::cout << "file1: " << file1contents << std::endl;
	// std::cout << "file2: " << file2contents << std::endl;
	// std::cout << "comp: " << file1contents.compare(file2contents) << std::endl;
	return file1contents.compare(file2contents) == 0;
}
