#include <stdio.h>

int main(int argc, char **argv)
{
    printf("%d\n", ackermann(3, 1));
    return 0;
}

int ackermann(int m, int n) {
  if(m==0)
    return n+1;
  else if(m>0 && n==0)
    return ackermann(m-1, 1);
  else if(m>0 && n>0)
    return ackermann(m-1, ackermann(m, n-1));
  else
    return -1;
}