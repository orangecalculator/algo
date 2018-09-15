#include <cstdio>
#include <time.h>

int main(){
    int N,i,l;
    scanf("%d",&N);

    int count[10001]={0};
    for(i=0;i<N;i++) {scanf("%d",&l);count[l]++;}

    int lim=-1;l=0;
    for(i=0;i<10001;i++){
    lim+=count[i];
    while(l<=lim){
    printf("%d\n",i);
    l++;
    }
    }
    return 0;
}
