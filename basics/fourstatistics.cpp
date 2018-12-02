#include <cstdio>

int main(){
    int N,i,sum=0,num;
    int count[8001]={0};
    scanf("%d",&N);
    for(i=0;i<N;i++) {
        scanf("%d",&num);
        sum+=num;
        count[num+4000]++;
    }

    //print the mean
    sum+=N/2;
    if(sum%N<0) printf("%d\n",sum/N-1);
    else printf("%d\n",sum/N);

    //print the median
    i=0;
    int stack=0;
    while(stack<(1+N/2)){
        stack+=count[i];
        i++;
    }
    printf("%d\n",i-4001);

    int mod=0,modeq,max=0,min=-1;
    stack=0;

    if(count[0]!=0) min=0;
    for(i=1;i<8001;i++){
        if(count[i]>count[mod]) {mod=i;stack=0;}
        else if(count[i]==count[mod]&stack==0){modeq=i;stack++;}

        if(count[i]!=0) max=i;
        if(count[i]!=0 &min==-1) min=i;
    }

    //print mod
    if(stack!=0) printf("%d\n",modeq-4000);
    else printf("%d\n",mod-4000);

    //print range
    printf("%d",max-min);
}
