#include <cstdio>
#include <time.h>

int countingsort(int *sor,int i,int j){
        int num[10001]={0};
        int k,l;
        for(k=i;k<=j;k++) num[sor[k]]++;

        int lim=i-1;
        k=i;
        for(l=0;l<=10000;l++){
        lim+=num[l];
        while(k<=lim){
        sor[k]=l;
        k++;
        }}

        return 1;
}

int main(){
    int N,i;
    scanf("%d",&N);
    int s[1000000];
    for(i=0;i<N;i++) scanf("%d",s+i);
    clock_t begin=clock();
    countingsort(s,0,N-1);
    clock_t end=clock();
    for(i=0;i<N;i++) printf("%d\n",s[i]);
    printf("time: %f\n",(double)(end-begin));
    return 0;
}
