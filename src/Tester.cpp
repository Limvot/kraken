#include "Tester.h"

Tester::Tester(std::string krakenInvocation, std::string krakenGrammerLocation) : krakenInvocation(krakenInvocation), krakenGrammerLocation(krakenGrammerLocation) {
	//initlization list
	removeCmd = "rm -r";
	resultsExtention = ".results";
	expectedExtention = ".expected_results";
	krakenExtention = ".krak";
	changePermissions = "chmod 755";
	shell = "sh";
    cd = "cd";
	redirect = ">";
	sep = "/";
}

Tester::~Tester() {
	//Nothing
}

void Tester::cleanExtras(std::string fileName) {
	ssystem(removeCmd + " " + fileName);
}

bool Tester::run(std::string path) {
    std::string fileName = split(path, *sep.c_str()).back();
	std::cout << "Testing: " << fileName << " with " << krakenInvocation << " and " << krakenGrammerLocation << std::endl;

	cleanExtras(path);
	ssystem(krakenInvocation + " " + path + krakenExtention + " " + path);
    // done automatically now
	//ssystem(changePermissions + " " + path + sep + fileName + ".sh");
	//ssystem(cd + " " + path + "; " + "./" + fileName + ".sh");
	//ssystem(changePermissions + " " + path + sep + fileName);
	ssystem(path + sep + fileName + " " + redirect + " " + path + sep + fileName + resultsExtention);

	bool result = compareFiles(fileName + expectedExtention, path + sep + fileName + resultsExtention);

	//If the test was succesful, we don't need all the extra files
	if (result)
		cleanExtras(path);

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

	return file1contents.compare(file2contents) == 0;
}
