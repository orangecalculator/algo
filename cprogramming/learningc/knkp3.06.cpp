// KNK Chapter 3 Programming Project 6
// by orangecalculator

#include <cstdio>

int main(){
	int denom1,nom1,denom2,nom2;

	printf("Enter two fractions separated by a plus sign: ");
	scanf("%d / %d + %d / %d",&denom1,&nom1,&denom2,&nom2);
    printf("The sum is %d/%d",denom1*nom2+denom2*nom1,nom1*nom2);
}
