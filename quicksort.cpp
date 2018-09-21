#include <cstdio>
#include <time.h>

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

int quicksort(int *sor,int i,int j){
    if(i>=j) return 0;

    int p=i,q=j,temp;
    int pivot=sor[(i+j)/2];
    while(p<=q){
    while(sor[p]<pivot) p++;
    while(sor[q]>pivot) q--;
    if(p<=q){
    temp=sor[p];
    sor[p]=sor[q];
    sor[q]=temp;
    p++;q--;
    }
    }

    quicksort(sor,i,q);
    quicksort(sor,p,j);

    return 1;
}

//by wikipedia
void quickSort(int arr[], int left, int right) {
      int i = left, j = right;
      int pivot = arr[(left + right) / 2];
      int temp;
      do
      {
        while (arr[i] < pivot)
            i++;
        while (arr[j] > pivot)
            j--;
        if (i<= j)
        {
            temp = arr[i];
            arr[i] = arr[j];
            arr[j] = temp;
            i++;
            j--;
        }
      } while (i<= j);

    /* recursion */
    if (left < j)
        quickSort(arr, left, j);

    if (i < right)
        quickSort(arr, i, right);
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
    printf("\nPROCEED QUICKSORT\n");
    begin=clock();
    quicksort(s,0,N-1);
    end=clock();
    printf("FINISH QUICKSORT\n");
    for(i=0;i<N;i++) {if(i==N-1) printf("%3d %d\n",i,s[i]); else if(s[i]>s[i+1]) printf("%3d %d ERR\n",i,s[i]);else printf("%3d %d\n",i,s[i]);}
    c=1;
    for(i=0;i<N-1;i++) if(s[i]>s[i+1]) {c=0;break;}
    if(c==1) printf("CLEAR\n");
    else printf("ERROR\n");
    printf("ELAPSED TIME:%f\n",(double)(end-begin));
    return 0;
}
