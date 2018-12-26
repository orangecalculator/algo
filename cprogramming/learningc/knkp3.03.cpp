// KNK Chapter 3 Programming Project 3
// by orangecalculator

#include <cstdio>

int main(){
	int prefix,identifier,code,number,check;

	printf("Enter ISBN: ");
	scanf("%d - %d - %d - %d - %d",&prefix,&identifier,&code,&number,&check);

	printf("GSI prefix: %d\n",prefix);
	printf("Group identifier: %d\n",identifier);
	printf("Publisher code: %d\n",code);
	printf("Item number: %d\n",number);
	printf("Check digit: %d\n",check);

}
