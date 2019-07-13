from itertools import combinations_with_replacement as cwr
from math import factorial

#assuming p=1/2

def comb(N,K):
    ans = 1
    for k in range(K):
        ans *= N-k
    for k in range(1,K+1):
        ans /= k
    return ans

def multi_comb(K):
    Nfact = factorial(sum(K))
    for k in K:
        Nfact /= factorial(k)
        #print("multicomb",K,Nfact)
    return Nfact

def combrep(L,K):
    sum = 0
    choose = lambda L,K : cwr(list(range(L)),K)
    for coordinate in choose(L,K):
        #print("coordinate",coordinate)
        occurence = [coordinate.count(k) for k in range(L)]
        #print("occurence",occurence)
        if all(occurence):
            sum += multi_comb(occurence)
    return sum
            
def moment(K):
    M_k=0
    for L in range(1,K+1):
        #print(L,K,combrep(L,K))
        M_k += 2**L * comb(-2,L) * combrep(L,K)
    return M_k * (-1)**(K%2)
    
for K in range(1,8):
    M_K = moment(K)
    print(K,':',M_K,M_K/factorial(K))
