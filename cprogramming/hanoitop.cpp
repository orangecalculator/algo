#include <cstdio>

int stones[3][1000],stonetop[3]={-1,-1,-1};

void printstate(){
    int maxIndex=-1;
    int i;
    for(i=0;i<3;++i) if(maxIndex<stonetop[i]) maxIndex = stonetop[i];
    
    while(maxIndex>=0){
        for(i=0;i<3;++i)
            if(maxIndex<=stonetop[i]) printf("%-4d",stones[i][maxIndex]);
            else printf("    ");
        printf("\n");
        --maxIndex;
    }
    printf("\n");
}

void transfer(int depth, int origin,int dest, int help){
    if(depth<=0) return ;
    else if(depth==1){
        stones[dest][++stonetop[dest]] = stones[origin][stonetop[origin]--];
        printstate();
        return ;
    }
    
    transfer(depth-1,origin,help,dest);
    
    stones[dest][++stonetop[dest]] = stones[origin][stonetop[origin]--];
    printstate();
    
    transfer(depth-1,help,dest,origin);
}

void initStone(int N){
    int i;
    for(i=0;i<N;++i) stones[0][i] = N - i ;
    stonetop[0] = N-1;
}

int main(){
    
    initStone(5);
    transfer(5,0,2,1);
}
