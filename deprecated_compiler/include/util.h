#ifndef UTIL_H
#define UTIL_H

#ifndef NULL
#define NULL ((void*)0)
#endif

#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <set>
#include <fstream>
#include <cstring>

int ssystem(std::string command);
std::string intToString(int theInt);
std::string replaceExEscape(std::string first, std::string search, std::string replace);
std::string strSlice(std::string str, int begin, int end);
int findPerenEnd(std::string str, int i);
std::vector<std::string> split(const std::string &str, char delim);
std::string join(const std::vector<std::string> &strVec, std::string joinStr);
std::string readFile(std::istream &file);
std::string padWithSpaces(std::string str, int padTo);

template <typename T, typename U, typename V>
class triple {
    public:
        T first;
        U second;
        V third;
};
template <typename T, typename U, typename V>
triple<T,U,V> make_triple(T f, U s, V t) {
    triple<T,U,V> out;
    out.first = f;
    out.second = s;
    out.third = t;
    return out;
}

template <typename T>
bool contains(std::vector<T> vec, T item) {
    for (auto i : vec)
        if (i == item)
            return true;
    return false;
}

template <typename T>
std::vector<T> flatten(std::vector<std::vector<T>> vec) {
    std::vector<T> flat;
    for (auto i : vec)
        flat.insert(flat.end(), i.begin(), i.end());
    return flat;
}

template <typename T>
std::vector<T> reverse(std::vector<T> vec) {
    std::vector<T> flat;
    flat.insert(flat.end(), vec.rbegin(), vec.rend());
    return flat;
}

template <typename T>
std::vector<T> dereferenced(std::vector<T*> vec) {
    std::vector<T> de;
    for (T* i:vec)
        de.push_back(*i);
    return de;
}

template <typename T>
std::vector<T> slice(std::vector<T> vec, int begin, int end, int step = 1) {
    std::vector<T> toReturn;
    if (begin < 0)
        begin += vec.size()+1;
    if (end < 0)
        end += vec.size()+1;
    for (int i = begin; i < end; i += step)
    	toReturn.push_back(vec[i]);
    return toReturn;
}

template <typename T>
bool subset(std::set<T> a, std::set<T> b) {
    for (auto i : a)
        if (b.find(i) == b.end())
            return false;
    return true;
}
#endif
