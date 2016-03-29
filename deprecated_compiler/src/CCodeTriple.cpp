#include "CCodeTriple.h"

CCodeTriple::CCodeTriple(std::string pre, std::string val, std::string post) {
    preValue = pre;
    value = val;
    postValue = post;
}

CCodeTriple::CCodeTriple(std::string val) {
    value = val;
}
CCodeTriple::CCodeTriple(const char* val) {
    value = val;
}

CCodeTriple::CCodeTriple() {
}

CCodeTriple::~CCodeTriple() {
}

std::string CCodeTriple::oneString(bool endValue) {
    return preValue + value + (endValue ? ";" : "") + postValue;
}

CCodeTriple & CCodeTriple::operator=(const CCodeTriple &rhs) {
    preValue = rhs.preValue;
    value = rhs.value;
    postValue = rhs.postValue;
    return *this;
}

CCodeTriple & CCodeTriple::operator+=(const CCodeTriple &rhs) {
    preValue += rhs.preValue;
    //preValue = rhs.preValue + preValue;
    value += rhs.value;
    postValue = rhs.postValue + postValue;
    return *this;
}

CCodeTriple operator+(const CCodeTriple &a, const CCodeTriple &b) {
    return CCodeTriple(a.preValue + b.preValue, a.value + b.value, b.postValue + a.postValue);
    //return CCodeTriple(b.preValue + a.preValue, a.value + b.value, b.postValue + a.postValue);
}

