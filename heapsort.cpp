#include <cstdio>
#include <time.h>

int swap(int *swap1,int *swap2){
        int temp=*swap2;
        *swap2=*swap1;
        *swap1=temp;
        return 1;
}

int nodeheap(int *sor, int i, int j, int p){
        int u=p,left=2*p-i+1;
        int right=left+1;

        if(j>=right){
            if(sor[left]>=sor[right])
                {if(sor[left]>sor[p]) u=left;}
            else if(sor[right]>sor[p]) u=right;
            }
            
        else if(j<left);
        else if(sor[left]>sor[p]) u=left;;

        if(p!=u)swap(sor+p,sor+u);
        return u;
        }

int downheap(int *sor, int i, int j, int p){
        int next=p;
        p++;
        while(p!=next){
            p=next;
            next=nodeheap(sor,i,j,p);
        }
        return 1;
}

int iniheapify(int *sor,int i,int j){

        int s;
        for(s=(j+i-1)/2;s>=i;s--)downheap(sor,i,j,s);
        return 0;
}

int heapsort(int *sor,int i,int j){
        int s;
        for(s=(j+i-1)/2;s>=i;s--)downheap(sor,i,j,s);
int k;
        for(s=j;s>i;s--){
            for(k=i;k<=j;k++)printf("%d ",sor[k]);printf("\n");
            swap(sor+s,sor+i);
            for(k=i;k<=j;k++)printf("%d ",sor[k]);printf("\n\n");
            downheap(sor,i,s-1,i);
        }
        return 1;
}

int main(){
    int N,i;
    scanf("%d",&N);
    int s[1000000];
    for(i=0;i<N;i++) scanf("%d",s+i);
    for(int k=0;k<N;k++)printf("%d ",s[k]);printf("\n\n");
    clock_t begin=clock();
    heapsort(s,0,N-1);
    clock_t end=clock();
    for(i=0;i<N;i++) printf("%d\n",s[i]);
    printf("time: %f\n",(double)(end-begin));
    return 0;
}
