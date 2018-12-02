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
/*
The original intention of chaining was grouping if and else level shown by indent
However, C or C++ does not interpret in that way
Rather, as follows
*/

int nodeheap(int *sor, int i, int j, int p){
        int u=p,left=2*p-i+1;
        int right=left+1;

        if(j>=right)
            if(sor[left]>=sor[right])
                if(sor[left]>sor[p]) u=left;
        	else if(sor[right]>sor[p]) u=right;
        	else if(j<left);
        	else if(sor[left]>sor[p]) u=left;

        if(p!=u)swap(sor+p,sor+u);
        return u;
        }
/*
In conclusion, in the wrong script, the intended job wouldnt work if index is small or right value is bigger
Therefore, the wrong script is the same as follows
*/
int nodeheap(int *sor, int i, int j, int p){
        int u=p,left=2*p-i+1;
        int right=left+1;

        if(j>=right){
            if(sor[left]>=sor[right])
                if(sor[left]>sor[p]) u=left;

        if(p!=u)swap(sor+p,sor+u);
        return u;
        }

//The intended code is as follows

int nodeheap(int *sor, int i, int j, int p){
        int u=p,left=2*p-i+1;
        int right=left+1;

        if(j>=right){
            if(sor[left]>=sor[right])
                {if(sor[left]>sor[p]) u=left;}
            else if(sor[right]>sor[p]) u=right;
            }
            
        else if(j<left);
        else if(sor[left]>sor[p]) u=left;;

        if(p!=u)swap(sor+p,sor+u);
        return u;
        }
