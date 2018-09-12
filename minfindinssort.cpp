#include <cstdio>
#include <time.h>

int minfindinssort(int (*sor),int i,int f){
    int index,j,k,temp;
    for(j=i;j<f;j++){
         index=j;
        for(k=j+1;k<=f;k++) if(sor[index]>sor[k]) index=k;
        if(index!=j){
            temp=sor[j];
            sor[j]=sor[index];
            sor[index]=temp;
        }
        }
    return 1;
}

int main(){
    clock_t begin = clock();
    int N,i;
    scanf("%d",&N);
    int s[1000000];
    for(i=0;i<N;i++) scanf("%d",s+i);
    minfindinssort(s,0,N-1);
    for(i=0;i<N;i++) printf("%4d %d\n",i+1,s[i]);
    clock_t end = clock();
    printf("%f\n",(double)(end-begin));
    return 0;
}
