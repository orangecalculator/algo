#include <iostream>
#include <stack>
#include <functional>

using namespace std;

void showstatus(stack<int> out);
void stackpermgen(stack<int>& in,stack<int>& out,stack<int>& store,function<void(stack<int>)> action);

int main(){
    stack<int> in;
    stack<int> out;
    stack<int> store;

    unsigned size;
    cin >> size;
    
    for(int k=size;k>0;--k) in.push(k);
    
    stackpermgen(in,out,store,showstatus);
}

void showstatus(stack<int> out){
    stack<int> temp;
    while(!out.empty()){
        temp.push(out.top());
        out.pop();
    }
    
    while(!temp.empty()){
        cout << temp.top() << ' ';
        temp.pop();
    }
    cout << endl;
}

void stackpermgen(stack<int>& in,stack<int>& out,stack<int>& store,function<void(stack<int>)> action){
    if(!store.empty()){
        out.push(store.top());
        store.pop();
        stackpermgen(in,out,store,action);
        store.push(out.top());
        out.pop();
    }
    if(!in.empty()){
        unsigned pushcount = 0;
        while(!in.empty()){
            ++pushcount;
            out.push(in.top());
            in.pop();
            stackpermgen(in,out,store,action);
            store.push(out.top());
            out.pop();
        }
        while(pushcount>0){
            --pushcount;
            in.push(store.top());
            store.pop();
        }
    }
    if(in.empty() && store.empty()) action(out);
}

// a_n is number of stackpermutations
// a_0 = a_1 = 1
// a_n = /sum_0^(n-1)a_k*a_(n-k-1)
// a_n = C(2n,n)/(n+1)

/*
#python3

import operator as op
from functools import reduce

def ncr(n, r):
    r = min(r, n-r)
    numer = reduce(op.mul, range(n, n-r, -1), 1)
    denom = reduce(op.mul, range(1, r+1), 1)
    return numer // denom

def stackpermcount(n):
    return ncr(2*n,n) // (n+1)

for k in range(100):
    print("{:>3}: {:>8}".format(k,stackpermcount(k)))

*/