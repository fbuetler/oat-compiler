#include <stdio.h>

int main(int argc, char **argv)
{
    int a[5] = { 1, 3, 5, 7, 9 };
    printf("%d\n", binSearch(3, a, sizeof(a)/sizeof(int)));
    return 0;
}

int binSearch(int val, int a[], int len){
    int l = 0;
    int r = len;
    while (l <= r){
        int m = (l+r)/2;
        if (val < a[m]){
            r = m-1;
        } else if (a[m] < val) {
            l = m+1;
        } else {
            return m;
        }
    }
    return -1;
}