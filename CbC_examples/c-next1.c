#define NEXT_OP(i) (i->op = *(MVMuint16 *)(i->cur_op), i->cur_op += 2, i->op)

typedef unsigned short MVMuint16;
typedef unsigned char MVMuint8;
typedef long* MVMRegister;
typedef void* MVMCompUnit;
typedef void* MVMCallsite;
//typedef void* MVMThreadContext;

typedef struct MVMThreadContext {
	MVMuint8 **interp_cur_op;
	MVMuint8 **interp_bytecode_start;
	MVMRegister **interp_reg_base;
	MVMCompUnit **interp_cu;  
} MVMThreadContext;


typedef struct interp {
     MVMuint16 op;
     /* Points to the place in the bytecode right after the current opcode. */
     /* See the NEXT_OP macro for making sense of this */
     MVMuint8 *cur_op;

     /* The current frame's bytecode start. */
     MVMuint8 *bytecode_start;

     /* Points to the base of the current register set for the frame we
 *       * are presently in. */
     MVMRegister *reg_base;

     /* Points to the current compilation unit. */
     MVMCompUnit *cu;

     /* The current call site we're constructing. */
     MVMCallsite *cur_callsite;

     MVMThreadContext *tc;
    
    //__code (*ret)();
    //__code (*main_ret)();
    __code (*ret)(int, void*);
    __code (*main_ret)(int, void*);
    void *env;

 } INTER,*INTERP;

__code cbc_no_op(INTERP);
__code cbc_exit(INTERP);

__code (* CODES[])(INTERP) = {
   cbc_no_op,
   cbc_no_op,
   cbc_exit,
};

__code cbc_next(INTERP i){
    __code (*c)(INTERP);
    c = CODES[NEXT_OP(i)];
    c(i);
    goto c(i);
}

__code cbc_no_op(INTERP i){
   goto cbc_next(i);
}

__code cbc_exit(INTERP i){
   goto i->main_ret(0,i->env);
}

//__code main_return(int i,stack sp) {
//    if (loop-->0)
//        goto f(233,sp);
//    printf("#0103:%d\n",i);
//    goto (( (struct main_continuation *)sp)->main_ret)(0,
//           ((struct main_continuation *)sp)->env);
//}

int interp_run(MVMThreadContext *tc){
	INTER inter = {0,0,0,0,0,0,0,0,0};
	INTERP i  = &inter;
	MVMuint8 cur_op[] = {0,1,1,0,1,2};
//	i->ret =  main_return;
        i->main_ret = _CbC_return;
        i->env = _CbC_environment;
	i->cur_op = cur_op;

	tc->interp_cur_op         = &i->cur_op;
	tc->interp_bytecode_start = &i->bytecode_start;
	tc->interp_reg_base       = &i->reg_base;
	tc->interp_cu             = &i->cu;
        goto cbc_next(i);
	return 0;
}

int main(int argc, char **argv){
   MVMThreadContext tct = {0,0,0,0};
   MVMThreadContext* tc = &tct;
   interp_run(tc);
}
