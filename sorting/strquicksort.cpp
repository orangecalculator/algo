#include <cstdio>

    char sor[20000][51];
    int ind[20000];

int strcomp(char *str1,char *str2){
    int i=0;
    
    while(str1[i]!='\0'&str2[i]!='\0') i++;
    
    if(str1[i]!='\0'&str2[i]=='\0') return -1;
    else if(str1[i]=='\0'&str2[i]!='\0') return 1;
    
    i=0;
    //-1 means str1 is lagging
    // 1 means str1 is leading
    while(str1[i]!='\0'&str2[i]!='\0'){
        if(str1[i]>str2[i]) return -1;
        else if(str1[i]<str2[i]) return 1;
        else i++;
    }
    return 0;
    // 0 means same string
}

int quicksortstr(int i,int j){
    if(i>=j) return 0;

    int p=i,q=j,temp;
    int pivot=ind[(i+j)/2];
    while(p<=q){
    
    //stop at index p in half front
    while(strcomp(sor[ind[p]],sor[pivot])>0) p++;
    //stop at index q in half back
    while(strcomp(sor[ind[q]],sor[pivot])<0) q--;
    
    if(p<=q){
    temp=ind[p];
    ind[p]=ind[q];
    ind[q]=temp;
    p++;q--;
    }
    
    }

    quicksortstr(i,q);
    quicksortstr(p,j);

    return 1;
}

int main(){
    int N;
    scanf("%d\n",&N);
    

    
    int i;
    for(i=0;i<N;i++){
        scanf("%s\n",sor[i]);
        ind[i]=i;
    }
    
    quicksortstr(0,N-1);
    
    
    for(i=0;i<(N-1);i++){
        if((strcomp(sor[ind[i]],sor[ind[i+1]]))) printf("%s\n",sor[ind[i]]);
    }
    printf("%s",sor[ind[N-1]]);
}
