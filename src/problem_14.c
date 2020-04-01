#include <stdio.h>
#include <stdlib.h>

int main() {
	int *x = malloc(sizeof(int));
	int a[15];

	for(int i = 0; i < 15; i++)
		a[i] = 0;

	*x = 7;
	a[*x] = *x + 4;

	printf("%d\n", *x);
        printf("%d\n", a[*x]);

	return 0;
}
