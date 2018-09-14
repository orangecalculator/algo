#include <cstdio>
#include <time.h>

int swap(int *swap1,int *swap2){
        int temp=*swap2;
        *swap2=*swap1;
        *swap1=temp;
        return 1;
}

int downheap(int *sor, int i, int j, int p){
        int u=p+1,left,right;
        while(p!=u){
            u=p;
            left=2*p-i+1;
            right=left+1;

        if(j>=right){
            if(sor[left]>=sor[right])
                {if(sor[left]>sor[p]) u=left;}
            else if(sor[right]>sor[p]) u=right;
            }

        else if(j<left);
        else if(sor[left]>sor[p]) u=left;;

        if(p!=u)swap(sor+p,sor+u);
        }
        return 1;
}

int heapsort(int *sor,int i,int j){
        int s;
        for(s=(j+i-1)/2;s>=i;s--)downheap(sor,i,j,s);
        for(s=j;s>i;s--){
            swap(sor+s,sor+i);
            downheap(sor,i,s-1,i);
        }
        return 1;
}

int main(){
    int N,i;
    scanf("%d",&N);
    int s[1000000];
    for(i=0;i<N;i++) scanf("%d",s+i);
    clock_t begin=clock();
    heapsort(s,0,N-1);
    clock_t end=clock();
    for(i=0;i<N;i++) printf("%d\n",s[i]);
    printf("time: %f\n",(double)(end-begin));
    return 0;
}
