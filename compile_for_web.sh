#!/usr/bin/env bash

emcc ./k_prime.krak.c -o k_prime.html -s EXPORTED_FUNCTIONS='["_main"]' -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' -s ERROR_ON_UNDEFINED_SYMBOLS=0
#emcc ./k_prime.krak.c -o k_prime.js -s EXPORTED_FUNCTIONS='["_fun_execute_code_starcharactercolonobkcbk_"]' -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' -s ERROR_ON_UNDEFINED_SYMBOLS=0
