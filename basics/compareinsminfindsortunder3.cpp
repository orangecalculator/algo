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
        if(index==j) continue;
        temp=sor[j];
        for(k=j;k>index;k--) sor[k]=sor[k-1];
        sor[index]=temp;
        }
    return 1;
}

int swap(int *a,int *b){
int temp=*a;
*a=*b;
*b=temp;
return 1;
}

int iterate3(int (*sor),int *N){
    if(*N==1) return 0;
    else if(*N==2&(sor[0]<sor[1])) swap(sor,sor+1);
    else if(*N==2&(sor[0]>sor[1])) {swap(sor,sor+1);*N=3;}
    else if(sor[1]<sor[2]) swap(sor+1,sor+2);
    else if(sor[1]>sor[2]) {
    swap(sor,sor+2);
    if(sor[0]==2) swap(sor+1,sor+2);
    else swap(sor,sor+1);
    }
    else if(sor[1]>sor[2]&sor[0]>sor[1]) *N=1;
    return 1;
    }

int main(){
    int N=2,i,yes1,yes2,k;
    clock_t begin,end;
    int s[3]={1,2,3},t1[3],t2[3];
    for(int j=0;j<10;j++)
    {
    for(k=0;k<3;k++) {t1[k]=s[k];t2[k]=s[k];}
    for(i=0;i<N;i++) printf("%2d %2d %2d\n",i+1,t1[i],t2[i]);
    begin = clock();
    stdsort(t1,0,N-1);
    end = clock();
    double stdsorttime=(double)(end-begin);
    begin = clock();
    inssort(t2,0,N-1);
    end = clock();
    double inssorttime=(double)(end-begin);
    for(i=0;i<N;i++) printf("%2d %2d %2d\n",i+1,t1[i],t2[i]);
    printf("minfindinssorttime:%f\ninssorttime:%f\n",stdsorttime,inssorttime);
    yes1=1;yes2=1;
    for(i=0;i<N-1;i++) {
    if(t1[i+1]<t1[i]) yes1=0;
    if(t2[i+1]<t2[i]) yes2=0;
    }
    if(yes1==1) printf("CLEAR\n"); else printf("ERROR\n");
    if(yes2==1) printf("CLEAR\n\n"); else printf("ERROR\n\n");
    iterate3(s,&N);
    }
    return 0;
}
