#include <stdio.h>
#include <stdlib.h>

int main()
{
printf ("Enter number of hashtables to be created\n");
int j;
scanf ("%d",&j);

printf ("Enter number of elements to be created\n");
int n;
scanf ("%d",&n);

printf ("Enter the modulo\n");
int modulo;
scanf ("%d",&modulo);

FILE *fp;

fp=fopen ("input.txt","w");
fprintf(fp,"%d\n",j);	
int one=1;
for(int i=0;i<n;i++) 
{
	
fprintf(fp,"%d\n",one);	
fprintf(fp,"%d\n",rand()%modulo);
}
int three=3;
fprintf(fp,"%d",three);	
printf ("Done");
   
return(0);
}
