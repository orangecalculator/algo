#include <cstdio>
#include <time.h>

int stdsort(int (*sor),int i,int f){
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

    int N,i;
    scanf("%d",&N);
    int s[1000000],t[1000000];
    for(i=0;i<N;i++) {scanf("%d",s+i);t[i]=s[i];}
    clock_t begin = clock();
    stdsort(s,0,N-1);
    clock_t end = clock();
    double stdsorttime=(double)(end-begin);
    begin = clock();
    inssort(t,0,N-1);
    end = clock();
    double inssorttime=(double)(end-begin);
    for(i=0;i<N;i++) printf("%4d %d\n",i+1,s[i]);
    printf("minfindinssorttime:%f\ninssorttime:%f\n",stdsorttime,inssorttime);
        int yes=1;
    for(i=0;i<N-1;i++) if(s[i+1]<s[i]) yes=0;
    if(yes==1) printf("CLEAR\n"); else printf("ERROR\n");
    return 0;
}
