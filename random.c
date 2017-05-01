#include <stdio.h>
#include <stdlib.h>

int main()
{

printf ("Enter number of elements to be created\n");
int n;
scanf ("%d",&n);

printf ("Enter the modulo\n");
int modulo;
scanf ("%d",&modulo);

FILE *fp;

fp=fopen ("input.txt","w");


for(int i=0;i<n;i++) 
{
fprintf(fp,"%d\n",rand()%modulo);
}

printf ("Done");
   
return(0);
}
