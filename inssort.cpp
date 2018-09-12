#include <cstdio>
#include <time.h>

int inssort(int (*sor),int i,int f){
    int index,j,k,temp;
    for(j=i+1;j<=f;j++){
         index=i;
        while(sor[index]<sor[j]) index++;
        temp=sor[j];
        for(k=j;k>index;k--) sor[k]=sor[k-1];
        sor[index]=temp;
        }
    return 1;
}

int main(){
    clock_t begin = clock();
    int N,i;
    scanf("%d",&N);
    int s[1000000];
    for(i=0;i<N;i++) scanf("%d",s+i);
    inssort(s,0,N-1);
    for(i=0;i<N;i++) printf("%4d %d\n",i+1,s[i]);
    clock_t end = clock();
    printf("%f\n",(double)(end-begin));
    int yes=1;
    for(i=0;i<N-1;i++) if(s[i+1]<s[i]) yes=0;
    if(yes==1) printf("CLEAR\n"); else printf("ERROR\n");
    return 0;
}
