#include <cstdio>

int main() {
	char x[100];
	int i,t,j;
	while(t>=0){
	//start getstr
	i=0;
	do t=scanf("%c",x+i);	while(x[i++]!='\n');
	x[i]='\0';
	//end of getstr

	//end if no input
	if(t==-1) break;

	//str modify algorithm
	if(x[0]=='T' & x[1]=='e'){
	    printf("[");
	    for(j=0;j<i-1;j++)
	    printf("%c",x[j]);
	    printf("]",);

	}
	if(x[0]=='P' & x[1]=='r')

	//print out string
	printf("%s",x);
	}
}
