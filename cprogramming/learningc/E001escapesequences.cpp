#include <cstdio>

// KNK 2.1 example
// KNR 1.1 example

int main(){
	// first example of KNK
	printf("To C, or not to C: that is the question.\n");
	
	// first example and exercise of KNR
	printf("hello, world%c",0x0a);	// linefeed by ASCII code
	
    	printf("a: \a f\n");		// alert
	printf("b: erase\b f\n");	// backspace
	printf("e: \e f\n");		// escape characters (non-standard)
	printf("f: \f f\n");		// formfeed page break
	printf("n: \n f\n");		// linefeed line break
	printf("r: \r \n");		// cursor return
	printf("t: \t f\n");		// tab
	printf("u: \u0167 f\n");	// unicode
	printf("v: ff\vff f\n");	// vertical tab
	printf("x: \x41 f\n");		// hexadecimal character code
	printf("nnn: \101");		// octal character code
	
	return 0;
}
