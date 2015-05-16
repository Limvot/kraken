#include "./test_topLevelVarInit.h"

/*unknown declaration named translation_unit*/
/*unknown declaration named translation_unit*/
/*unknown declaration named translation_unit*/
/*unknown declaration named translation_unit*/
/*unknown declaration named translation_unit*/
/*unknown declaration named translation_unit*/
/*unknown declaration named translation_unit*/
/**
 * Variable Declarations 
 */

int _dot__div_test_topLevelVarInit_dot_krak_scp_a; /*identifier*/
/**
 * Function Definitions
 */


int main()
{
	io_dot_krak_scp_println_int(_dot__div_test_topLevelVarInit_dot_krak_scp_a) ;
	return 0;
}
void io_dot_krak_scp_print_char_P__(char* io_dot_krak_scp_toPrint)
{
	{
		char* toPrint = io_dot_krak_scp_toPrint;

			printf(toPrint);
		;
	};
	return;
}
void io_dot_krak_scp_print_string_dot_krak_scp_string(string_dot_krak_scp_string io_dot_krak_scp_toPrint)
{
	io_dot_krak_scp_print_char_P__(string_dot_krak_scp_string__toCharArray(&io_dot_krak_scp_toPrint) ) ;
}
void io_dot_krak_scp_print_int(int io_dot_krak_scp_toPrint)
{
	{
		int toPrint = io_dot_krak_scp_toPrint;

			printf("%d", toPrint);
		;
	};
	return;
}
void io_dot_krak_scp_print_float(float io_dot_krak_scp_toPrint)
{
	{
		float toPrint = io_dot_krak_scp_toPrint;

			printf("%f", toPrint);
		;
	};
	return;
}
void io_dot_krak_scp_print_double(double io_dot_krak_scp_toPrint)
{
	{
		double toPrint = io_dot_krak_scp_toPrint;

			printf("%f", toPrint);
		;
	};
	return;
}
void io_dot_krak_scp_println()
{
	io_dot_krak_scp_print_char_P__("\n") ;
}
void io_dot_krak_scp_println_char_P__(char* io_dot_krak_scp_toPrint)
{
	io_dot_krak_scp_print_char_P__(io_dot_krak_scp_toPrint) ;
	io_dot_krak_scp_println() ;
}
void io_dot_krak_scp_println_string_dot_krak_scp_string(string_dot_krak_scp_string io_dot_krak_scp_toPrint)
{
	io_dot_krak_scp_println_char_P__(string_dot_krak_scp_string__toCharArray(&io_dot_krak_scp_toPrint) ) ;
}
void io_dot_krak_scp_println_int(int io_dot_krak_scp_toPrint)
{
	io_dot_krak_scp_print_int(io_dot_krak_scp_toPrint) ;
	io_dot_krak_scp_println() ;
}
void io_dot_krak_scp_println_float(float io_dot_krak_scp_toPrint)
{
	io_dot_krak_scp_print_float(io_dot_krak_scp_toPrint) ;
	io_dot_krak_scp_println() ;
}
void io_dot_krak_scp_println_double(double io_dot_krak_scp_toPrint)
{
	io_dot_krak_scp_print_double(io_dot_krak_scp_toPrint) ;
	io_dot_krak_scp_println() ;
}
void mem_dot_krak_scp_delete_lessthan_char_greaterthan__char_P__(char* mem_dot_krak_scp_toDelete)
{
	mem_dot_krak_scp_free_lessthan_char_greaterthan__char_P__(mem_dot_krak_scp_toDelete) ;
}
void mem_dot_krak_scp_delete_lessthan_char_greaterthan__char_P___int(char* mem_dot_krak_scp_toDelete, int mem_dot_krak_scp_itemCount)
{
	mem_dot_krak_scp_delete_lessthan_char_greaterthan__char_P__(mem_dot_krak_scp_toDelete) ;
}
void mem_dot_krak_scp_free_lessthan_char_greaterthan__char_P__(char* mem_dot_krak_scp_memPtr)
{
	{
		char* memPtr = mem_dot_krak_scp_memPtr;

			free(memPtr);
		;
	};
}
char* mem_dot_krak_scp_malloc_lessthan_char_greaterthan__int(int mem_dot_krak_scp_size)
{
	char* mem_dot_krak_scp_memPtr;;
	{
		int size = mem_dot_krak_scp_size;
char* memPtr = mem_dot_krak_scp_memPtr;

			memPtr = malloc(size);
		mem_dot_krak_scp_memPtr = memPtr;
;
	};
	return mem_dot_krak_scp_memPtr;
}
char* mem_dot_krak_scp_mem_scopeop_new_lessthan_char_greaterthan__int(int mem_dot_krak_scp_count)
{
	return mem_dot_krak_scp_malloc_lessthan_char_greaterthan__int(((mem_dot_krak_scp_sizeof_lessthan_char_greaterthan_() )*(mem_dot_krak_scp_count))) ;
}
char* mem_dot_krak_scp_new_lessthan_char_greaterthan__int(int mem_dot_krak_scp_count)
{
	return mem_dot_krak_scp_malloc_lessthan_char_greaterthan__int(((mem_dot_krak_scp_sizeof_lessthan_char_greaterthan_() )*(mem_dot_krak_scp_count))) ;
}
int mem_dot_krak_scp_sizeof_lessthan_char_greaterthan_()
{
	char mem_dot_krak_scp_testObj;;
	int mem_dot_krak_scp_result;;
	{
		char testObj = mem_dot_krak_scp_testObj;

			int result = sizeof(testObj);
		mem_dot_krak_scp_result = result;
;
	};
	return mem_dot_krak_scp_result;
}/* Method Definitions for string */

string_dot_krak_scp_string* string_dot_krak_scp_string__construct(string_dot_krak_scp_string* this)
{
	vector_dot_krak_scp_vector_lessthan_char_greaterthan___construct(&this->string_dot_krak_scp_data) ;
	return this;
}

string_dot_krak_scp_string* string_dot_krak_scp_string__construct_char_P__(string_dot_krak_scp_string* this, char* string_dot_krak_scp_str)
{
	vector_dot_krak_scp_vector_lessthan_char_greaterthan___construct(&this->string_dot_krak_scp_data) ;
	while (*(string_dot_krak_scp_str))
		{
		vector_dot_krak_scp_vector_lessthan_char_greaterthan___addEnd_char(&this->string_dot_krak_scp_data,*(string_dot_krak_scp_str)) ;
		string_dot_krak_scp_str = ((string_dot_krak_scp_str)+(1));
	};
;
	return this;
}

char* string_dot_krak_scp_string__toCharArray(string_dot_krak_scp_string* this)
{
	char* string_dot_krak_scp_out = mem_dot_krak_scp_mem_scopeop_new_lessthan_char_greaterthan__int(((this->string_dot_krak_scp_data).vector_dot_krak_scp_size)) ;;
	for (	int string_dot_krak_scp_i = 0;((string_dot_krak_scp_i)<(((this->string_dot_krak_scp_data).vector_dot_krak_scp_size)));	string_dot_krak_scp_i++)
		(string_dot_krak_scp_out)[string_dot_krak_scp_i] = vector_dot_krak_scp_vector_lessthan_char_greaterthan___get_int(&this->string_dot_krak_scp_data,string_dot_krak_scp_i) ;
;
	return string_dot_krak_scp_out;
}
/* Done with string */

int util_dot_krak_scp_lesser_lessthan_int_greaterthan__int_int(int util_dot_krak_scp_a, int util_dot_krak_scp_b)
{
	if (((util_dot_krak_scp_a)>(util_dot_krak_scp_b)))
	{ 	return util_dot_krak_scp_b;
 };
	return util_dot_krak_scp_a;
}/* Method Definitions for vector<char> */

vector_dot_krak_scp_vector_lessthan_char_greaterthan_* vector_dot_krak_scp_vector_lessthan_char_greaterthan___construct(vector_dot_krak_scp_vector_lessthan_char_greaterthan_* this)
{
	this->vector_dot_krak_scp_size = 0;
	this->vector_dot_krak_scp_available = 8;
	this->vector_dot_krak_scp_data = mem_dot_krak_scp_new_lessthan_char_greaterthan__int(8) ;
	return this;
}

void vector_dot_krak_scp_vector_lessthan_char_greaterthan___destruct(vector_dot_krak_scp_vector_lessthan_char_greaterthan_* this)
{
	mem_dot_krak_scp_delete_lessthan_char_greaterthan__char_P__(this->vector_dot_krak_scp_data) ;
}

bool vector_dot_krak_scp_vector_lessthan_char_greaterthan___resize_int(vector_dot_krak_scp_vector_lessthan_char_greaterthan_* this, int vector_dot_krak_scp_newSize)
{
	char* vector_dot_krak_scp_newData = mem_dot_krak_scp_new_lessthan_char_greaterthan__int(vector_dot_krak_scp_newSize) ;;
	if (!(vector_dot_krak_scp_newData))
	{ 	return false;
 };
	for (	int vector_dot_krak_scp_i = 0;((vector_dot_krak_scp_i)<(util_dot_krak_scp_lesser_lessthan_int_greaterthan__int_int(this->vector_dot_krak_scp_size, vector_dot_krak_scp_newSize) ));	vector_dot_krak_scp_i++)
		(vector_dot_krak_scp_newData)[vector_dot_krak_scp_i] = (this->vector_dot_krak_scp_data)[vector_dot_krak_scp_i];
;
	mem_dot_krak_scp_delete_lessthan_char_greaterthan__char_P___int(this->vector_dot_krak_scp_data, 0) ;
	this->vector_dot_krak_scp_data = vector_dot_krak_scp_newData;
	this->vector_dot_krak_scp_available = vector_dot_krak_scp_newSize;
	return true;
}

char vector_dot_krak_scp_vector_lessthan_char_greaterthan___at_int(vector_dot_krak_scp_vector_lessthan_char_greaterthan_* this, int vector_dot_krak_scp_index)
{
	return vector_dot_krak_scp_vector_lessthan_char_greaterthan___get_int(this,vector_dot_krak_scp_index) ;
}

char vector_dot_krak_scp_vector_lessthan_char_greaterthan___get_int(vector_dot_krak_scp_vector_lessthan_char_greaterthan_* this, int vector_dot_krak_scp_index)
{
	if (((((vector_dot_krak_scp_index)<(0)))||(((vector_dot_krak_scp_index)>=(this->vector_dot_krak_scp_size)))))
	{
		io_dot_krak_scp_println_char_P__("Vector access out of bounds! Retuning 0th element as sanest option") ;
		io_dot_krak_scp_print_char_P__("Vector tried to access element: ") ;
		io_dot_krak_scp_println_int(vector_dot_krak_scp_index) ;
		io_dot_krak_scp_print_char_P__("Max Index of vector: ") ;
		io_dot_krak_scp_println_int(((this->vector_dot_krak_scp_size)-(1))) ;
		return (this->vector_dot_krak_scp_data)[0];
	};
	return (this->vector_dot_krak_scp_data)[vector_dot_krak_scp_index];
}

char* vector_dot_krak_scp_vector_lessthan_char_greaterthan___getBackingMemory(vector_dot_krak_scp_vector_lessthan_char_greaterthan_* this)
{
	return this->vector_dot_krak_scp_data;
}

void vector_dot_krak_scp_vector_lessthan_char_greaterthan___set_int_char(vector_dot_krak_scp_vector_lessthan_char_greaterthan_* this, int vector_dot_krak_scp_index, char vector_dot_krak_scp_dataIn)
{
	if (((((vector_dot_krak_scp_index)<(0)))||(((vector_dot_krak_scp_index)>=(this->vector_dot_krak_scp_size)))))
	{ 	return;
 };
	(this->vector_dot_krak_scp_data)[vector_dot_krak_scp_index] = vector_dot_krak_scp_dataIn;
}

void vector_dot_krak_scp_vector_lessthan_char_greaterthan___addEnd_char(vector_dot_krak_scp_vector_lessthan_char_greaterthan_* this, char vector_dot_krak_scp_dataIn)
{
	this->vector_dot_krak_scp_size++;
	if (((this->vector_dot_krak_scp_size)>=(this->vector_dot_krak_scp_available)))
	{ 	vector_dot_krak_scp_vector_lessthan_char_greaterthan___resize_int(this,((this->vector_dot_krak_scp_size)*(2))) ;
 };
	(this->vector_dot_krak_scp_data)[((this->vector_dot_krak_scp_size)-(1))] = vector_dot_krak_scp_dataIn;
}
/* Done with vector<char> */
