// KNK Chapter 2 Programming Project 1
// by orangecalculator

#include <cstdio>

int main(){
	for(int j=8;j>=3;--j){
	for(int i=1;i<=j;++i)	if(i==j|i==6-j) printf("*"); else printf(" ");
	printf("\n");
}
}
