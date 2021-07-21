#include "stdio.h"

extern int atoi (const char *);

static int loop;

#if 1 // def __micro_c__
#define CC_ONLY 0
#else
#define CC_ONLY 1
#endif

/* classical function call case (0) */
int g0(int);
int h0(int);

int
f0(int i) {
    int k,j;
    k = 3+i;
    j = g0(i+3);
    return k+4+j;
}

int
g0(int i) {
    return h0(i+4)+i;
}

int
h0(int i) {
    return i+4;
}

#if !CC_ONLY

/* straight conversion case (1) */

typedef void *stack;

struct cont_interface { // General Return Continuation
    __code (*ret)(int, void *);
};

//#ifndef __llvm__ 
__code f_g0(int i,int k,stack sp);
__code f_g1(int j,stack sp);
__code g(int i,stack sp);
__code h(int i,stack sp);
__code f2(int i,char *sp);
__code g2(int i,int k,int j,char *sp);
__code h2(int i,int k,char *sp);
__code main_return2(int i,stack sp);
__code g2_1(int k,int i,stack *sp);
__code h2_11(int i,int k,stack *sp);
//#endif

__code f(int i,stack sp) {
    int k,j;
    k = 3+i;
    goto f_g0(i,k,sp);
}

struct f_g0_interface {  // Specialized Return Continuation
    __code (*ret)(int, void *);
    int i_,k_,j_;
};

__code f_g1(int j,stack sp);

__code f_g0(int i,int k,stack sp) { // Caller
    struct f_g0_interface *c = 
	(struct f_g0_interface *)(sp -= sizeof(struct f_g0_interface));

    c->ret = f_g1;
    c->k_ = k;
    c->i_ = i;

    goto g(i+3,sp);
}

__code f_g1(int j,stack sp) {  // Continuation 
    struct f_g0_interface *c = (struct f_g0_interface *)sp;
    int k = c->k_;
    sp+=sizeof(struct f_g0_interface);
    c = (struct f_g0_interface *)sp;
    goto (c->ret)(k+4+j,sp);
}

__code g_h1(int j,stack sp);

__code g(int i,stack sp) { // Caller
    struct f_g0_interface *c = 
	(struct f_g0_interface *)(sp -= sizeof(struct f_g0_interface));

    c->ret = g_h1;
    c->i_ = i;

    goto h(i+3,sp);
}

__code g_h1(int j,stack sp) {  // Continuation 
    struct f_g0_interface *c = (struct f_g0_interface *)sp;
    int i = c->i_;
    sp+=sizeof(struct f_g0_interface);
    c = (struct f_g0_interface *)sp;
    goto (c->ret)(j+i,sp);
}

__code h(int i,stack sp) {
    struct f_g0_interface *c = (struct f_g0_interface *)sp;
    goto (c->ret)(i+4,sp);
}

struct main_continuation { // General Return Continuation
    __code (*ret)(int, void*);
    __code (*main_ret)(int, void*);
    void *env;
};

__code main_return(int i,stack sp) {
    if (loop-->0)
	goto f(233,sp);
    printf("#0103:%d\n",i);
    goto (( (struct main_continuation *)sp)->main_ret)(0,
           ((struct main_continuation *)sp)->env);
}

/* little optimzation without stack continuation (2) */

__code f2(int i,char *sp) {
    int k,j;
    k = 3+i;
    goto g2(i,k,i+3,sp);
}

__code g2(int i,int k,int j,char *sp) {
    j = j+4;
    goto h2(i,k+4+j,sp);
}

__code h2_1(int i,int k,int j,char *sp) {
    goto main_return2(i+j,sp);
}

__code h2(int i,int k,char *sp) {
    goto h2_1(i,k,i+4,sp);
}

__code main_return2(int i,stack sp) {
    if (loop-->0)
	goto f2(233,sp);
    printf("#0132:%d\n",i);
    goto (( (struct main_continuation *)sp)->main_ret)(0,
           ((struct main_continuation *)sp)->env);
}

/* little optimizaed case (3) */

__code f2_1(int i,stack *sp) {
    int k,j;
    k = 3+i;
    goto g2_1(k,i+3,sp);
}

__code g2_1(int k,int i,stack *sp) {
    goto h2_11(k,i+4,sp);
}

__code f2_0_1(int k,int j,stack *sp);
__code h2_1_1(int i,int k,int j,stack *sp) {
    goto f2_0_1(k,i+j,sp);
}

__code h2_11(int i,int k,stack *sp) {
    goto h2_1_1(i,k,i+4,sp);
}

__code f2_0_1(int k,int j,stack *sp) {
    goto (( (struct cont_interface *)sp)->ret)(k+4+j,sp);
}

__code main_return2_1(int i,stack sp) {
    if (loop-->0)
        goto f2_1(233,sp);
    printf("#0165:%d\n",i);
    goto (( (struct main_continuation *)sp)->main_ret)(0,
           ((struct main_continuation *)sp)->env);
}

#define STACK_SIZE 2048
char main_stack[STACK_SIZE];
#define stack_last (main_stack+STACK_SIZE)

#endif

#define LOOP_COUNT 10000000

int
main(int ac,char *av[])
{
#if !CC_ONLY
    struct main_continuation *cont;
    stack sp = stack_last;
#endif
    int sw;
    int j;
    if (ac==2) sw = atoi(av[1]);
    else sw=3;

    if (sw==0) {
	for(loop=0;loop<LOOP_COUNT;loop++) {
	   j = f0(233);
	}
	printf("#0193:%d\n",j);
#if !CC_ONLY
    } else if (sw==1) {
	loop = LOOP_COUNT;
	sp -= sizeof(*cont);
	cont = (struct main_continuation *)sp;
	cont->ret = main_return;
	cont->main_ret = _CbC_return;
	cont->env = _CbC_environment;
	goto f(233,sp);
    } else if (sw==2) {
	loop = LOOP_COUNT;
	sp -= sizeof(*cont);
	cont = (struct main_continuation *)sp;
	cont->ret = main_return2;
	cont->main_ret = _CbC_return;
	cont->env = _CbC_environment;
	goto f2(233,sp);
    } else if (sw==3) {
	loop = LOOP_COUNT;
	sp -= sizeof(*cont);
	cont = (struct main_continuation *)sp;
	cont->ret = main_return2_1;
	cont->main_ret = _CbC_return;
	cont->env = _CbC_environment;
	goto f2_1(233,sp);
#endif
    }
return 0;
}

/* end */
