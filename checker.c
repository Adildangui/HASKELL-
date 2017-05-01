#include <stdio.h>
int main()
{
	FILE *fp;
	fp=fopen ("input.txt","r");
	int count=0;
	while(!feof(fp))
	{
		int n;
		fscanf (fp,"%d",&n);
		if (n==45)
		count++;
	} 
	printf ("count:%d",count);
}
