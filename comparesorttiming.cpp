#include <cstdio>
#include <time.h>

int minfindinssort(int (*sor),int i,int f){
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

int swap(int *swap1,int *swap2){
        int temp=*swap2;
        *swap2=*swap1;
        *swap1=temp;
        return 1;
}

int downheap(int *sor, int i, int j, int p){
        int u=p,left,right;p++;
        while(p!=u){
            p=u;
            left=2*p-i+1;
            right=left+1;

        if(j>=right){
            if(sor[left]>=sor[right])
                {if(sor[left]>sor[p]) u=left;}
            else if(sor[right]>sor[p]) u=right;
            }
        else if(j<left);
        else if(sor[left]>sor[p]) u=left;

        if(p!=u)swap(sor+p,sor+u);
        }
        return 1;
}

int heapsort(int *sor,int i,int j){
        int s;int k;
        for(s=(j+i-1)/2;s>=i;s--)downheap(sor,i,j,s);
        for(s=j;s>i;s--){
            swap(sor+s,sor+i);
            downheap(sor,i,s-1,i);
        }
        return 1;
}

int countingsort(int *sor,int i,int j){
        int num[10001]={0};
        int k,l;
        for(k=i;k<=j;k++) num[sor[k]]++;

        int lim=i-1;
        k=i;
        for(l=0;l<=10000;l++){
        lim+=num[l];
        while(k<=lim){
        sor[k]=l;
        k++;
        }}

        return 1;
}

int quicksort(int *sor,int i,int j){
    if(i==j) return 1;
    else if((j-i)<3) {
    inssort(sor,i,j);
    return 1;
    }

    int p=i,q=j-1,temp;
    while(1){
    while(!(sor[p]>=sor[j]&sor[q]<sor[j])){
            if(!(sor[p]>=sor[j])) p++;
            if(!(sor[q]<sor[j])) q--;
            if(p>q) break;
    }
    if(p>q) break;
    temp=sor[p];
    sor[p]=sor[q];
    sor[q]=temp;
    }

    temp=sor[p];
    sor[p]=sor[j];
    sor[j]=temp;

    quicksort(sor,i,p-1);
    quicksort(sor,p,j);

    return 1;
}

int main(){
    int N,i;
    scanf("%d",&N);
    int t[1000000];
    for(i=0;i<N;i++) scanf("%d",t+i);
    int s[1000000];
    int c;
    clock_t begin,end;

    for(i=0;i<N;i++) s[i]=t[i];
    printf("\nPROCEED MINFINDINSSORT\n");
    begin=clock();
    minfindinssort(s,0,N-1);
    end=clock();
    printf("FINISH MINFINDINSSORT\n");
    c=1;
    for(i=0;i<N-1;i++) if(s[i]>s[i+1]) {c=0;break;}
    if(c==1) printf("CLEAR\n");
    else printf("ERROR\n");
    printf("ELAPSED TIME:%f\n",(double)(end-begin));

    for(i=0;i<N;i++) s[i]=t[i];
    printf("\nPROCEED INSSORT\n");
    begin=clock();
    inssort(s,0,N-1);
    end=clock();
    printf("FINISH INSSORT\n");
    c=1;
    for(i=0;i<N-1;i++) if(s[i]>s[i+1]) {c=0;break;}
    if(c==1) printf("CLEAR\n");
    else printf("ERROR\n");
    printf("ELAPSED TIME:%f\n",(double)(end-begin));

    for(i=0;i<N;i++) s[i]=t[i];
    printf("\nPROCEED BUBSORT\n");
    begin=clock();
    bubsort(s,0,N-1);
    end=clock();
    printf("FINISH BUBSORT\n");
    c=1;
    for(i=0;i<N-1;i++) if(s[i]>s[i+1]) {c=0;break;}
    if(c==1) printf("CLEAR\n");
    else printf("ERROR\n");
    printf("ELAPSED TIME:%f\n",(double)(end-begin));

    for(i=0;i<N;i++) s[i]=t[i];
    printf("\nPROCEED MERGESORT\n");
    begin=clock();
    mergesort(s,0,N-1);
    end=clock();
    printf("FINISH MERGESORT\n");
    c=1;
    for(i=0;i<N-1;i++) if(s[i]>s[i+1]) {c=0;break;}
    if(c==1) printf("CLEAR\n");
    else printf("ERROR\n");
    printf("ELAPSED TIME:%f\n",(double)(end-begin));

    for(i=0;i<N;i++) s[i]=t[i];
    printf("\nPROCEED HEAPSORT\n");
    begin=clock();
    heapsort(s,0,N-1);
    end=clock();
    printf("FINISH HEAPSORT\n");
    c=1;
    for(i=0;i<N-1;i++) if(s[i]>s[i+1]) {c=0;break;}
    if(c==1) printf("CLEAR\n");
    else printf("ERROR\n");
    printf("ELAPSED TIME:%f\n",(double)(end-begin));

    for(i=0;i<N;i++) s[i]=t[i];
    printf("\nPROCEED COUNTINGSORT\n");
    begin=clock();
    countingsort(s,0,N-1);
    end=clock();
    printf("FINISH COUNTINGSORT\n");
    c=1;
    for(i=0;i<N-1;i++) if(s[i]>s[i+1]) {c=0;break;}
    if(c==1) printf("CLEAR\n");
    else printf("ERROR\n");
    printf("ELAPSED TIME:%f\n",(double)(end-begin));

    for(i=0;i<N;i++) s[i]=t[i];
    printf("\nPROCEED QUICKSORT\n");
    begin=clock();
    quicksort(s,0,N-1);
    end=clock();
    printf("FINISH QUICKSORT\n");
    c=1;
    for(i=0;i<N-1;i++) if(s[i]>s[i+1]) {c=0;break;}
    if(c==1) printf("CLEAR\n");
    else printf("ERROR\n");
    printf("ELAPSED TIME:%f\n",(double)(end-begin));


    return 0;
}
