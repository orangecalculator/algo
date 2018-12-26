// KNK Chapter 3 Programming Project 5
// by orangecalculator

#include <cstdio>

int main(){
	int num[16],i,j;

	printf("Enter the numbers from 1 to 16 in any order:\n");
	for(i=0;i<16;++i) scanf("%d",num+i);

	for(j=0;j<4;++j){
        for(i=0;i<4;++i) printf("%2d ",num[4*j+i]);
        printf("\n");
	}
    printf("\n");

	printf("Row sums: %d %d %d %d\n",
                num[0]+num[1]+num[2]+num[3],
                num[4]+num[5]+num[6]+num[7],
                num[8]+num[9]+num[10]+num[11],
                num[12]+num[13]+num[14]+num[15]);

    printf("Column sums: %d %d %d %d\n",
                num[0]+num[4]+num[8]+num[12],
                num[1]+num[5]+num[9]+num[13],
                num[2]+num[6]+num[10]+num[14],
                num[3]+num[7]+num[11]+num[15]);
    printf("Diagonal sums: %d %d\n",
                num[0]+num[5]+num[10]+num[15],
                num[3]+num[6]+num[9]+num[12]);

}
