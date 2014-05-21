#include "util.h"

std::string intToString(int theInt) {
	std::stringstream converter;
	converter << theInt;
	return converter.str();
}

std::string replaceExEscape(std::string first, std::string search, std::string replace) {
    size_t pos = 0;
    while (pos <= first.size()-search.size()) {
        pos = first.find(search, pos);
        if (pos == std::string::npos)
            break;
        //std::cout << "Position is " << pos << " size of first is " << first.size() << " size of replace is " << replace.size() << std::endl;
        //If excaped, don't worry about it.
        if (pos > 0) {
            int numBackslashes = 0;
            int countBack = 1;
            while (pos-countBack >= 0 && first[pos-countBack] == '\\') {
                numBackslashes++;
                countBack++;
            }
            if (numBackslashes % 2 == 1) {
                pos++;
                continue;
            }
        }
        first = first.replace(pos, search.size(), replace);
        pos += replace.size();
    }
    return first;
}

//String slicing is crazy useful. substr isn't bad, but slicing with negative indicies is wonderful
std::string strSlice(std::string str, int begin, int end) {
    if (begin < 0)
        begin += str.length()+1;
    if (end < 0)
        end += str.length()+1;
    return str.substr(begin, end-begin);
}

int findPerenEnd(std::string str, int i) {
    int numHangingOpen = 0;
    for (; i< str.length(); i++) {
        if (str[i] == '(')
            numHangingOpen++;
        else if (str[i] == ')')
            numHangingOpen--;
        if (numHangingOpen == 0)
            return i;
    }
}

std::vector<std::string> split(const std::string &str, char delim) {
    std::stringstream ss(str);
    std::string word;
    std::vector<std::string> splitVec;
    while (std::getline(ss, word, delim))
        splitVec.push_back(word);
    return splitVec;
}

std::string join(const std::vector<std::string> &strVec, std::string joinStr) {
    if (strVec.size() == 0)
        return "";
    std::string joinedStr = strVec[0];
    for (int i = 1; i < strVec.size(); i++)
        joinedStr += joinStr + strVec[i];
    return joinedStr;
}

std::string readFile(std::istream &file) {
    std::string line, contents;
    while(file.good()) {
        getline(file, line);
        contents.append(line+"\n");
    }
    return contents;
}


