#include "stdio.h"

#define __environment _CbC_environment
#define __return _CbC_return


struct arg {
   int a0;int a1;int a2;int a3;int a4;
};

extern void exit(int); 

void *exit_env;
__code (*exit___code)();

// #ifndef __llvm__ 
__code carg2(int arg0,int arg1,int arg2,int arg3,int arg4,__code(*exit1)(int, void*),void *env);
__code cargs(struct arg args0,__code exit1(int, void*),void *env);
__code carg4(struct arg args0,struct arg args1,int i, int j,int k,int l);
__code carg5(struct arg args0,struct arg args1,int i, int j,int k,int l);
__code carg6(int i, int j,int k,int l,struct arg args0);
// #endif

__code carg1(int arg0,int arg1,int arg2,int arg3,int arg4,__code(*exit1)(int, void*),void *env)
{
    printf("#0017:arg1: %d %d %d %d %d : %x %x\n",arg0,arg1,arg2,arg3,arg4,exit1==exit___code,env==exit_env);
    goto carg2(arg1,arg2,arg3,arg4,arg0,exit1,env);
}

__code carg2(int arg0,int arg1,int arg2,int arg3,int arg4,__code(*exit1)(int, void*),void *env)
{
    struct arg args0;
    printf("#0024:arg1: %d %d %d %d %d : %x %x\n",arg0,arg1,arg2,arg3,arg4,exit1==exit___code,env==exit_env );
    args0.a0 = arg0;
    args0.a1 = arg1;
    args0.a2 = arg2;
    args0.a3 = arg3;
    args0.a4 = arg4;
    goto cargs(args0,exit1,env);
}

__code cargs(struct arg args0,__code exit1(int, void*),void *env)
{
    printf("#0035:args: %d %d %d %d %d : %x %x\n",
	args0.a0,args0.a1,args0.a2,args0.a3,args0.a4,
    exit1==exit___code,env==exit_env);
    // goto exit1(321),env;
    goto (*exit1)(0,env);
}


__code carg3(struct arg args0,struct arg args1,int i, int j,int k,int l)
{
    printf("#0045:args3: %d %d %d %d %d : %x %x %x %x\n",
	args0.a0,args0.a1,args0.a2,args0.a3,args0.a4,i,j,k,l);
    printf("#0047:args3: args0 %d %d %d %d %d : args1 %d %d %d %d %d : %x %x %x %x\n",
	args0.a0,args0.a1,args0.a2,args0.a3,args0.a4,
	args1.a0,args1.a1,args1.a2,args1.a3,args1.a4,
	i,j,k,l);
    if (args0.a0==args1.a0) exit(0);
    goto carg4(args0,args1,j,k,l,i);
}

__code carg4(struct arg args0,struct arg args1,int i, int j,int k,int l)
{
    printf("#0057:args4: %d %d %d %d %d : %x %x %x %x\n",
	args0.a0,args0.a1,args0.a2,args0.a3,args0.a4,i,j,k,l);
    goto carg5(args1,args0,j,k,l,i);
}

__code carg5(struct arg args0,struct arg args1,int i, int j,int k,int l)
{
    printf("#0064:args5: %d %d %d %d %d : %x %x %x %x\n",
	args0.a0,args0.a1,args0.a2,args0.a3,args0.a4,i,j,k,l);
    goto carg6(i,j,k,l,args0);
}

__code carg6(int i, int j,int k,int l,struct arg args0)
{
    printf("#0071:args6: %d %d %d %d %d : %x %x %x %x\n",
	args0.a0,args0.a1,args0.a2,args0.a3,args0.a4,i,j,k,l);
    goto carg3(args0,args0,i,j,k,l);
}

int main1(int n)
{
    goto carg1(0,1,2,3,4,exit___code=__return,exit_env=__environment);
    return n;
}

struct arg a00;
struct arg a01;

int main( int ac, char *av[])
{
    int n;
    n = main1(123);
    printf("#0089:321=%d\n",n);

    a00.a0 = 11;
    a00.a1 = 22;
    a00.a2 = 33;
    a00.a3 = 44;
    a00.a4 = 55;
    a01.a0 = 66;
    a01.a1 = 77;
    a01.a2 = 88;
    a01.a3 = 99;
    a01.a4 = 10;
    goto carg3(a00,a01,1,2,3,4);
}

