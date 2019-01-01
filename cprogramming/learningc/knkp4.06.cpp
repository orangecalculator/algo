// KNK Chapter 4 Programming Project 6
// by orangecalculator

#include <cstdio>

int main(){
	int digit[12];

	int i,sum[2]={0};
	printf("Enter the first 12 digits of an EAN: ");
	for(i=0;i<12;++i) scanf("%1d",digit+i);

	for(i=1;i<12;i=i+2) sum[0]+=digit[i];
	for(i=0;i<12;i=i+2) sum[1]+=digit[i];
	printf("Check digit: %d\n",(9-(3*sum[0]+sum[1]-1)%10));
}
