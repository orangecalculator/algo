/*
Permutation Generator base code
by orangecalculator

permutation following the list in the form of linkedlist
n-times for loop by dynamic programming

*/

#include <cstdio>
#include <vector>

using namespace std;

struct linkboard{
    int front,number,back;
};
struct linkboard board[12];
vector<int> trace;
void permutation(int size,int stage=1){
    if(stage<size){
        int location=board[0].back;
        while(location!=size+1){
            board[board[location].front].back = board[location].back;
            board[board[location].back].front = board[location].front;
            trace.push_back(location);
            
            permutation(size,stage+1);
            
            trace.pop_back();
            board[board[location].front].back = location;
            board[board[location].back].front = location;
            
            location = board[location].back;
        }
        return;
    }
    
    trace.push_back(board[0].back);
    
    for(int permnum : trace) printf("%d",permnum);
    printf("\n");
    
    trace.pop_back();
    return;
}

int main(){
    for(int i=0;i<12;++i){
        board[i].front=i-1;
        board[i].number=i;
        board[i].back=i+1;
    }
    board[0].back=1;
    board[11].front=10;
    
    permutation(5);
}
