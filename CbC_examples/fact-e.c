#define __environment _CbC_environment
#define __return _CbC_return

#include "stdio.h"
#include "stdlib.h"

struct F { int n; int r; int o; void *env; 
       __code (*next)( struct F );
       __code (*exit)( struct F );
   };

__code factorial(struct F arg)
{
    if (arg.n<0) {
	printf("#0008:err %d!\n",arg.n);
        exit(1);
	// goto (*exit1)(0,exit1env);
    }
    if (arg.n==0)
	goto arg.next(arg);
    else {
	arg.r *= arg.n;
	arg.n--;
	goto factorial(arg);
    }
}

__code print(struct F arg);

int main( int ac, char *av[])
{
    struct F arg;
    arg.n = atoi(av[1]);
    // arg.n = 10;
    arg.r = 1;
    arg.o = 1;
    arg.next = print;
    arg.exit = 0;
    arg.env = 0;
    goto factorial(arg);
}

__code print(struct F arg)
{
    printf("#0032:%d! = %d\n",arg.o, arg.r);
    exit(0);
}

