#include <cstdio>
#include <time.h>

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

int tem[1000000];

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

    int t1[1000000],t2[1000000];
    for(i=0;i<N;i++) {scanf("%d",t1+i);t2[i]=t1[i];}

    clock_t begin,end;
    double time1,time2;

    begin = clock();
    inssort(t1,0,N-1);
    end = clock();
    time1=(double)(end-begin);

    begin = clock();
    mergesort(t2,0,N-1);
    end = clock();
    time2=(double)(end-begin);

    for(i=0;i<N;i++) printf("%4d %6d %6d\n",i+1,t1[i],t2[i]);
    printf("first time:%f\nsecond time:%f\n",time1,time2);

    int yes1=1,yes2=1;
    for(i=0;i<N-1;i++) {
    if(t1[i+1]<t1[i]) yes1=0;
    if(t2[i+1]<t2[i]) yes2=0;
    }
    if(yes1==1) printf("CLEAR\n"); else printf("ERROR\n");
    if(yes2==1) printf("CLEAR\n\n"); else printf("ERROR\n\n");
    return 0;
}
