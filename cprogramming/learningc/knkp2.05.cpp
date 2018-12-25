// KNK Chapter 2 Programming Project 5
// by orangecalculator

#include <cstdio>

int main(){
	float x;
	
	printf("Write the Value: ");
	scanf("%f",&x);
	printf("%f\n",3.0f*x*x*x*x*x+2.0f*x*x*x*x-5.0f*x*x*x-x*x+7.0f*x-6.0f);
	printf("%f\n",((((3.0f*x+2.0f)*x-5.0f)*x-1.0f)*x+7.0f)*x-6.0f);
}
