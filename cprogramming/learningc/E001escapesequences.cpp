#include <cstdio>

// KNK 2.1 example
// KNR 1.1 example

int main(){
	// first example of KNK
	printf("To C, or not to C: that is the question.\n");
	
	// first example and exercise of KNR
	printf("hello, world%c",0x0a);
	
    printf("a: \a f\n");
	printf("b: erase\b f\n");
	printf("e: \e f\n");
	printf("f: \f f\n");
	printf("n: \n f\n");
	printf("r: \r \n");
	printf("t: \t f\n");
	printf("u: \u0167 f\n");
	printf("v: ff\vff f\n");
	printf("x: \x41 f\n");
	printf("nnn: \101");
	
	return 0;
}
