#ifndef UTIL_H
#define UTIL_H

#ifndef NULL
#define NULL ((void*)0)
#endif

#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <fstream>
#include <cstring>

std::string intToString(int theInt);
std::string replaceExEscape(std::string first, std::string search, std::string replace);
std::string strSlice(std::string str, int begin, int end);
int findPerenEnd(std::string str, int i);
std::vector<std::string> split(const std::string &str, char delim);
std::string join(const std::vector<std::string> &strVec, std::string joinStr);
std::string readFile(std::istream &file);


template <typename T>
bool contains(std::vector<T> vec, T item) {
    for (auto i : vec)
        if (i == item)
            return true;
    return false;
}

template <typename T>
std::vector<T> slice(std::vector<T> vec, int begin, int end) {
    std::vector<T> toReturn;
    if (begin < 0)
        begin += vec.size()+1;
    if (end < 0)
        end += vec.size()+1;
    for (int i = begin; i < end; i++)
    	toReturn.push_back(vec[i]);
    return toReturn;
}

#endif
