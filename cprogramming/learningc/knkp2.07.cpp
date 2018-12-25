// KNK Chapter 2 Programming Project 5
// by orangecalculator

#include <cstdio>

int main(){
	int dollars;
	
	printf("Enter a dollar amount: ");
	scanf("%d",&dollars);
	
	printf("\n$20 bills: %d",dollars/20);
	dollars=dollars%20;
	printf("\n$10 bills: %d",dollars/10);
	dollars=dollars%10;
	printf("\n $5 bills: %d",dollars/5);
	dollars=dollars%5;
	printf("\n $1 bills: %d",dollars);
}
