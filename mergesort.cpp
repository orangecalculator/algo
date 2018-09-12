#include <cstdio>
#include <time.h>

int tem[1000000];

int inssort(int (*sor),int i,int f){
    int index,j,k,temp;
    for(j=i+1;j<=f;j++){
         index=i;
        while(sor[index]<sor[j]) index++;
        if(index==j) continue;
        temp=sor[j];
        for(k=j;k>index;k--) sor[k]=sor[k-1];
        sor[index]=temp;
        }
    return 1;
}

int mergesort(int (*sor),int i,int f){

    if(f-i<3) {inssort(sor,i,f);return 1;}

    int m=(f+i-1)/2;
    mergesort(sor,i,m);
    mergesort(sor,m+1,f);

    int p=i,q=m+1,u;
    for(u=i;u<=f;u++){
        if(p>m) {tem[u]=sor[q];q++;}
        else if(q>f) {tem[u]=sor[p];p++;}
        else if(sor[p]<sor[q]) {tem[u]=sor[p];p++;}
        else {tem[u]=sor[q];q++;}
    }
    for(u=i;u<=f;u++) sor[u]=tem[u];
    return 1;
}

int main(){
    int N,i;
    scanf("%d",&N);
    int s[1000000];
    for(i=0;i<N;i++) scanf("%d",s+i);
    clock_t begin=clock();
    mergesort(s,0,N-1);
    clock_t end=clock();
    for(i=0;i<N;i++) printf("%d\n",s[i]);
    printf("time: %f\n",(double)(end-begin));
    return 0;
}
