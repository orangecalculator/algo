#include <cstdio>

int bubsort(int (*sor),int i,int j){
    int index,temp,move;
    while(1){
        move=0;
        for(index=i;index<j;index++){
            if(sor[index+1]<sor[index]){
                temp=sor[index];
                sor[index]=sor[index+1];
                sor[index+1]=temp;
                move++;
            }
        }
        if(move==0) break;
    }
    return 1;
}

int main(){
    int N,i;
    scanf("%d",&N);
    int s[1000];
    for(i=0;i<N;i++) scanf("%d",s+i);
    bubsort(s,0,N-1);
    for(i=0;i<N;i++) printf("%d\n",s[i]);

    int c=1;
    for(i=0;i<N-1;i++) if(s[i]>s[i+1]) {c=0;break;}
    if(c==1) printf("CLEAR\n");
    else printf("ERROR\n");

    return 0;
}
