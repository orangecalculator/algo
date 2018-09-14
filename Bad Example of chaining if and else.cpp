Bad Example of chaining if and else

int nodeheap(int *sor, int i, int j, int p){
        int u=p,left=2*p-i+1;
        int right=left+1;

        if(j>=right)
            if(sor[left]>=sor[right])
                if(sor[left]>sor[p]) u=left;
            else if(sor[right]>sor[p]) u=right;
            
            
        else if(j<left);
        else if(sor[left]>sor[p]) u=left;;

        if(p!=u)swap(sor+p,sor+u);
        return u;
        }

