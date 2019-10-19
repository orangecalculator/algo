/*

백준 1413 https://www.acmicpc.net/problem/1413
변형문제

사실 문제를 잘못 알아들어서 엄청 어려운 부분을 추가로 구현해버렸다.

만약 다못이가 하나씩 박스를 폭파시키지 않고 차례로 박스를 폭파시키지 않고
무작위로 처음에 상자를 뽑아서 일단 상자를 폭파시키고 열쇠를 사용한다면 성공확률이 얼마나 될 것인가?

*/

#include <cstdio>

int N,M;

//probability fraction calculation part

long long int factorial[21];

long long int gcd(long long int a, long long int b){
    static long long int temp;
    if(a<b){
        temp = b;
        b = a;
        a = temp;
    }
    while(b>0){
        a %= b;
        
        //swap
        temp = b;
        b = a;
        a = temp;
    }
    
    return a;
}

struct fraction{
    long long int numer,denom;
};

void reduce(fraction & base){
    if(base.denom==0) return;
    else if(base.numer==0) {
        base.denom=1;
        return;
    }
    static long long int div;
    div = gcd(base.numer,base.denom);
    base.numer /= div;
    base.denom /= div;
}

void add(fraction & base,const fraction & diff){
    static long long int multforbase,multfordiff;
    multforbase = gcd(base.denom,diff.denom);
    multfordiff = base.denom / multforbase;
    multforbase = diff.denom / multforbase;
    
    base.numer = base.numer * multforbase + diff.numer * multfordiff;
    base.denom *= multforbase;
    reduce(base);
}

// specific case part
int partition[20],partitionsize,calcsize;
fraction delta;

int calcattrib[20],trivial;
void getcalcattrib(int modify=calcsize){
    static int adder[20];
    if(modify>=0){
        adder[modify]=1;
        getcalcattrib(modify-1);
        adder[modify]=0;
        getcalcattrib(modify-1);
        return;
    }
    static int groupcount,addcount,i;
    groupcount = addcount = 0;
    for(i=0;i<=calcsize;++i)if(adder[i]){
        ++addcount;
        groupcount += partition[i];
    }
    if(groupcount+M>N) return;
    if(addcount%2) --calcattrib[groupcount];
    else ++calcattrib[groupcount];
}

void getprob(){
    static int i;
    calcsize=partitionsize;
    while(calcsize>0 && partition[calcsize]==1) --calcsize;
    trivial = partitionsize - calcsize;
    
    for(i=0;i<20;++i) calcattrib[i]=0;
    getcalcattrib(calcsize);
    
    static long long int numerdelta;
    delta.numer = 1,delta.denom = 1,numerdelta = 1;
    static int Nspe;
    Nspe = N - trivial;
    for(i=1;i<=Nspe-M;++i){
        numerdelta *= (N-M-i+1);
        delta.numer *= (Nspe-i+1);
        delta.denom *= (Nspe-i+1);
        if(calcattrib[i]) delta.numer += calcattrib[i] * numerdelta;
    }
    reduce(delta);
    
    for(i=0;i<trivial;++i){
        delta.numer *= M-i;
        delta.denom *= N-i;
    }
    reduce(delta);
}

// permutation part
fraction totalprob,outerprob;
int setpart[21];//
void partitiongenerate(int margin,int upperlimit,int pointsize=0){
    if(pointsize<partitionsize){
        int i=( margin-partitionsize+pointsize<upperlimit ? margin-partitionsize+pointsize : upperlimit );
        for(;i>=margin/(partitionsize-pointsize+1);--i){
            partition[pointsize]=i;
            partitiongenerate(margin-i,i,pointsize+1);
        }
        return;
    } else if(margin>upperlimit) return;
    else partition[pointsize]=margin;
    
    //calculate
    getprob();
    
    int i,j;
    outerprob.numer=outerprob.denom=factorial[N];
    for(i=1;i<=N;++i) setpart[i]=0;
    for(i=0;i<=partitionsize;++i) ++setpart[partition[i]];
    for(i=1;i<=N;++i){
        for(j=0;j<setpart[i];++j) outerprob.numer /= i;
        if(setpart[i]>1) outerprob.numer /= factorial[setpart[i]];
    }
    
    reduce(outerprob);
    
    static long long int temp;
    temp = outerprob.numer;
    outerprob.numer = delta.numer;
    delta.numer = temp;
    
    reduce(outerprob);
    reduce(delta);
    
    outerprob.numer *= delta.numer;
    outerprob.denom *= delta.denom;
    
    add(totalprob,outerprob);
    
}

int main(){
    int i,j;
    scanf("%d %d",&N,&M);
    
    factorial[0]=factorial[1]=1;
    for(i=2;i<=20;++i) factorial[i] = factorial[i-1] * i;
    
    totalprob.numer = 0;
    totalprob.denom = 1;
    
    for(i=M;i>=1;--i){
        partitionsize = i-1;
        partitiongenerate(N,N);
    }
    
    
    printf("%lld/%lld",totalprob.numer,totalprob.denom);
    
}
