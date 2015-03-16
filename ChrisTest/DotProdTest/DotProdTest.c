#include "DotProdTest.h"

/*unknown declaration named translation_unit*/
/*unknown declaration named translation_unit*/
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

/**
 * Function Definitions
 */


double dot_vector_lessthan_double_greaterthan__vector_lessthan_double_greaterthan_(vector_lessthan_double_greaterthan_ a, vector_lessthan_double_greaterthan_ b)
{
	if (((((a).size))<(((b).size))))
		{
		println_char_P__("Error: Vectors not of same size") ;
		print_char_P__("Vector a is of size: ") ;
		println_int(((a).size)) ;
		print_char_P__("Vector b is of size: ") ;
		println_int(((b).size)) ;
		println_char_P__("Did you know that Grover Cleveland served two seperate terms as President of the United States, four years apart?") ;
	};
;
	double ans = 0;;
	for (	int i = 0;((i)<(lesser_lessthan_int_greaterthan__int_int(((a).size), ((b).size)) ));	i++)
		{
		ans = ((ans)+(((vector_lessthan_double_greaterthan___at_int(&a,i) )*(vector_lessthan_double_greaterthan___at_int(&b,i) ))));
	};
;
	println_double(ans) ;
	return ans;
}
int main()
{
	vector_lessthan_double_greaterthan_ aVec; vector_lessthan_double_greaterthan___construct(&aVec) /*Init Position Call*/;
	vector_lessthan_double_greaterthan_ bVec; vector_lessthan_double_greaterthan___construct(&bVec) /*Init Position Call*/;
	double j;;
	for (	int i = 0;((i)<(4));	i++)
		{
		j = ((i)+(0.0));
		vector_lessthan_double_greaterthan___addEnd_double(&aVec,j) ;
		vector_lessthan_double_greaterthan___addEnd_double(&bVec,j) ;
	};
;
	vector_lessthan_double_greaterthan___addEnd_double(&bVec,12.3) ;
	double dotProd = dot_vector_lessthan_double_greaterthan__vector_lessthan_double_greaterthan_(aVec, bVec) ;;
	return 0;
	vector_lessthan_double_greaterthan___destruct(&aVec);
	vector_lessthan_double_greaterthan___destruct(&bVec);
}
void print_char_P__(char* toPrint)
{
	{
		
			printf(toPrint);
		;
	};
	return;
}
void print_string(string toPrint)
{
	print_char_P__(string__toCharArray(&toPrint) ) ;
}
void print_int(int toPrint)
{
	{
		
			printf("%d", toPrint);
		;
	};
	return;
}
void print_float(float toPrint)
{
	{
		
			printf("%f", toPrint);
		;
	};
	return;
}
void print_double(double toPrint)
{
	{
		
			printf("%f", toPrint);
		;
	};
	return;
}
void println()
{
	print_char_P__("\n") ;
}
void println_char_P__(char* toPrint)
{
	print_char_P__(toPrint) ;
	println() ;
}
void println_string(string toPrint)
{
	println_char_P__(string__toCharArray(&toPrint) ) ;
}
void println_int(int toPrint)
{
	print_int(toPrint) ;
	println() ;
}
void println_float(float toPrint)
{
	print_float(toPrint) ;
	println() ;
}
void println_double(double toPrint)
{
	print_double(toPrint) ;
	println() ;
}
void delete_lessthan_char_greaterthan__char_P__(char* toDelete)
{
	free(toDelete) ;
}
void delete_lessthan_char_greaterthan__char_P___int(char* toDelete, int itemCount)
{
	delete_lessthan_char_greaterthan__char_P__(toDelete) ;
}
void delete_lessthan_double_greaterthan__double_P__(double* toDelete)
{
	free(toDelete) ;
}
void delete_lessthan_double_greaterthan__double_P___int(double* toDelete, int itemCount)
{
	delete_lessthan_double_greaterthan__double_P__(toDelete) ;
}
char* malloc_lessthan_char_greaterthan__int(int size)
{
	char* memPtr = 0;;
	{
		
			memPtr = malloc(size);
		;
	};
	return memPtr;
}
double* malloc_lessthan_double_greaterthan__int(int size)
{
	double* memPtr = 0;;
	{
		
			memPtr = malloc(size);
		;
	};
	return memPtr;
}
char* new_lessthan_char_greaterthan__int(int count)
{
	return malloc_lessthan_char_greaterthan__int(((sizeof_lessthan_char_greaterthan_() )*(count))) ;
}
double* new_lessthan_double_greaterthan__int(int count)
{
	return malloc_lessthan_double_greaterthan__int(((sizeof_lessthan_double_greaterthan_() )*(count))) ;
}
int sizeof_lessthan_char_greaterthan_()
{
	int result = 0;;
	char testObj;;
	{
		
			result = sizeof(testObj);
		;
	};
	return result;
}
int sizeof_lessthan_double_greaterthan_()
{
	int result = 0;;
	double testObj;;
	{
		
			result = sizeof(testObj);
		;
	};
	return result;
}/* Method Definitions for string */

string* string__construct(string* this)
{
	vector_lessthan_char_greaterthan___construct(&this->data) ;
	return this;
}

string* string__construct_char_P__(string* this, char* str)
{
	vector_lessthan_char_greaterthan___construct(&this->data) ;
	while (*(str))
		{
		vector_lessthan_char_greaterthan___addEnd_char(&this->data,*(str)) ;
		str = ((str)+(1));
	};
;
	return this;
}

char* string__toCharArray(string* this)
{
	char* out = new_lessthan_char_greaterthan__int(((this->data).size)) ;;
	for (	int i = 0;((i)<(((this->data).size)));	i++)
		(out)[i] = vector_lessthan_char_greaterthan___get_int(&this->data,i) ;
;
	return out;
}
/* Done with string */

int lesser_lessthan_int_greaterthan__int_int(int a, int b)
{
	if (((a)>(b)))
		return b;
;
	return a;
}/* Method Definitions for vector<char> */

vector_lessthan_char_greaterthan_* vector_lessthan_char_greaterthan___construct(vector_lessthan_char_greaterthan_* this)
{
	this->size = 0;
	this->available = 8;
	this->data = new_lessthan_char_greaterthan__int(8) ;
	return this;
}

void vector_lessthan_char_greaterthan___destruct(vector_lessthan_char_greaterthan_* this)
{
	delete_lessthan_char_greaterthan__char_P__(this->data) ;
}

bool vector_lessthan_char_greaterthan___resize_int(vector_lessthan_char_greaterthan_* this, int newSize)
{
	char* newData = new_lessthan_char_greaterthan__int(newSize) ;;
	if (!(newData))
		return false;
;
	for (	int i = 0;((i)<(lesser_lessthan_int_greaterthan__int_int(this->size, newSize) ));	i++)
		(newData)[i] = (this->data)[i];
;
	delete_lessthan_char_greaterthan__char_P___int(this->data, 0) ;
	this->data = newData;
	this->available = newSize;
	return true;
}

char vector_lessthan_char_greaterthan___at_int(vector_lessthan_char_greaterthan_* this, int index)
{
	return vector_lessthan_char_greaterthan___get_int(this,index) ;
}

char vector_lessthan_char_greaterthan___get_int(vector_lessthan_char_greaterthan_* this, int index)
{
	if (((((index)<(0)))||(((index)>=(this->size)))))
		{
		return (this->data)[0];
	};
;
	return (this->data)[index];
}

char* vector_lessthan_char_greaterthan___getBackingMemory(vector_lessthan_char_greaterthan_* this)
{
	return this->data;
}

void vector_lessthan_char_greaterthan___set_int_char(vector_lessthan_char_greaterthan_* this, int index, char dataIn)
{
	if (((((index)<(0)))||(((index)>=(this->size)))))
		return;
;
	(this->data)[index] = dataIn;
}

void vector_lessthan_char_greaterthan___addEnd_char(vector_lessthan_char_greaterthan_* this, char dataIn)
{
	this->size++;
	if (((this->size)>=(this->available)))
		vector_lessthan_char_greaterthan___resize_int(this,((this->size)*(2))) ;
;
	(this->data)[((this->size)-(1))] = dataIn;
}
/* Done with vector<char> */
/* Method Definitions for vector<double> */

vector_lessthan_double_greaterthan_* vector_lessthan_double_greaterthan___construct(vector_lessthan_double_greaterthan_* this)
{
	this->size = 0;
	this->available = 8;
	this->data = new_lessthan_double_greaterthan__int(8) ;
	return this;
}

void vector_lessthan_double_greaterthan___destruct(vector_lessthan_double_greaterthan_* this)
{
	delete_lessthan_double_greaterthan__double_P__(this->data) ;
}

bool vector_lessthan_double_greaterthan___resize_int(vector_lessthan_double_greaterthan_* this, int newSize)
{
	double* newData = new_lessthan_double_greaterthan__int(newSize) ;;
	if (!(newData))
		return false;
;
	for (	int i = 0;((i)<(lesser_lessthan_int_greaterthan__int_int(this->size, newSize) ));	i++)
		(newData)[i] = (this->data)[i];
;
	delete_lessthan_double_greaterthan__double_P___int(this->data, 0) ;
	this->data = newData;
	this->available = newSize;
	return true;
}

double vector_lessthan_double_greaterthan___at_int(vector_lessthan_double_greaterthan_* this, int index)
{
	return vector_lessthan_double_greaterthan___get_int(this,index) ;
}

double vector_lessthan_double_greaterthan___get_int(vector_lessthan_double_greaterthan_* this, int index)
{
	if (((((index)<(0)))||(((index)>=(this->size)))))
		{
		return (this->data)[0];
	};
;
	return (this->data)[index];
}

double* vector_lessthan_double_greaterthan___getBackingMemory(vector_lessthan_double_greaterthan_* this)
{
	return this->data;
}

void vector_lessthan_double_greaterthan___set_int_double(vector_lessthan_double_greaterthan_* this, int index, double dataIn)
{
	if (((((index)<(0)))||(((index)>=(this->size)))))
		return;
;
	(this->data)[index] = dataIn;
}

void vector_lessthan_double_greaterthan___addEnd_double(vector_lessthan_double_greaterthan_* this, double dataIn)
{
	this->size++;
	if (((this->size)>=(this->available)))
		vector_lessthan_double_greaterthan___resize_int(this,((this->size)*(2))) ;
;
	(this->data)[((this->size)-(1))] = dataIn;
}
/* Done with vector<double> */
