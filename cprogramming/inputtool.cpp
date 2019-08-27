// written by orangecalculator
// written on 20190827

// simulation of main on boj 2941 from COCI 2008/2009 contest #5 problem 1

//#define DBG

#include <cstdio>
#include <queue>

using namespace std;

//#define NULL ((void*) 0)
#define possible_chars 28

typedef struct _trie{
    char character='\0';
    int depth;
    bool end = false;
    struct _trie * next_node[possible_chars] = {};
    struct _trie * reduce_node = NULL;
} trie;

int encode_char(const char character){
    if(character>=0x61 && character <(0x61 + 26)) return character - 0x61;
    else if(character == '-') return 26;
    else if(character == '=') return 27;
    return 28;
}
/*
char decode_index(const int index){
    if(index<26) return (char)(index + 0x61);
    else if(index==26) return '-';
    else if(index==27) return '=';
    return '\0';
}
*/

trie * new_trie(void){
    trie * new_parent = new _trie;
    new_parent->character = '\0';
    new_parent->depth = 0;
    return new_parent;
}

int load_pattern(_trie const * parent, const char pattern[]){
    trie * trace=(trie *)parent;
    for(int k=0;pattern[k];++k){
        const char & next_char = pattern[k];
        const int char_index = encode_char(next_char);
        trie * next_node = trace->next_node[char_index];
        if(next_node == NULL){
            next_node = trace->next_node[char_index] = new trie;
            next_node->character = pattern[k];
            next_node->depth = trace->depth + 1;
        }
        trace = next_node;
    }
    trace->end = true;
    return 0;
}

int destruct_trie(trie * node){
    for(int k=0;k<possible_chars;++k){
        if(node->next_node[k] != NULL) destruct_trie(node->next_node[k]);
    }
    delete node;
    return 0;
}

int construct_back_trace(trie * const parent){
    // preprocess on parent
    parent->reduce_node = (trie *) parent;

    queue<trie *> process_line;
    for(int k=0;k<possible_chars;++k){
        if(parent->next_node[k] != NULL){
            process_line.push(parent->next_node[k]);
            parent->next_node[k]->reduce_node = parent;
        }
    }

    while(!process_line.empty()){
         trie * proc_node = process_line.front();
         process_line.pop();

         for(int k=0;k<possible_chars;++k){
             if(proc_node->next_node[k] != NULL){
                 process_line.push(proc_node->next_node[k]);

                 // find reduce_node
                 trie * const edit_node = proc_node->next_node[k];
                 trie * find_node = proc_node->reduce_node;
                 while(find_node->next_node[k] == NULL && find_node !=parent) find_node = find_node->reduce_node;
                 if(find_node->next_node[k] != NULL) edit_node->reduce_node = find_node->next_node[k];
                 else edit_node->reduce_node = parent;
             }
         }
    }
    
    return 0;
}

int DBG_view(trie * parent){

        for(int i=0;i<parent->depth;++i) printf("\t");
        printf("%p character: %c depth: %d end: %d",parent,parent->character,parent->depth,parent->end ? 1 : 0);
        for(int k=0;k<possible_chars;++k) if(parent->next_node[k] != NULL) printf(" %d",k);
        printf(" reduce: %c on %d",parent->reduce_node->character,parent->reduce_node->depth);
        printf("\n");
        
        for(int k=0;k<possible_chars;++k) if(parent->next_node[k] != NULL) DBG_view(parent->next_node[k]);
    
    return 0;
}

int main(){
    trie * parent = new_trie();
    load_pattern(parent,"c=");
    load_pattern(parent,"c-");
    load_pattern(parent,"dz=");
    load_pattern(parent,"d-");
    load_pattern(parent,"lj");
    load_pattern(parent,"nj");
    load_pattern(parent,"s=");
    load_pattern(parent,"z=");

    construct_back_trace(parent);

#ifdef DBG

    DBG_view(parent);

    return 0;
#endif

    char analyze_pattern[101];
    scanf("%s",analyze_pattern);
    
    trie * current_node = parent;
    int alphabet_count = 0;
    for(char * ptit = analyze_pattern; *ptit;++ptit){
        trie * travel_node = current_node;
        int next_index = encode_char(*ptit);
        
        if(current_node->next_node[next_index] != NULL){
            current_node = current_node->next_node[next_index];
            if(current_node->end){
                 alphabet_count += 1;
                 current_node = parent;
            }
        } else{
            while(travel_node->next_node[next_index] == NULL && travel_node != parent) travel_node = travel_node->reduce_node;
            if(travel_node->next_node[next_index]!=NULL) travel_node = travel_node->next_node[next_index];
            alphabet_count += current_node->depth + 1 - travel_node->depth;
            current_node = travel_node;
        }
    }
    
    alphabet_count += current_node->depth;
    current_node = parent;

    printf("%d",alphabet_count);

    destruct_trie(parent);
}
