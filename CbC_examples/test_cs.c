//#include<stdio.h>
//#include<stdlib.h>

typedef unsigned long size_t;
extern int printf(const char*, ...);
extern     void exit(int status);

__code cs_exit(int , double , char );
__code cs1(int, int, int, int);
__code cs2(double, double, int, double);
void test_goto(void);

#define ALLOCATE_SIZE 20000000
#define NEW(type) (type*)(calloc(1, sizeof(type)))
#define NEWN(n, type) (type*)(calloc(n, sizeof(type)))

extern void *calloc(size_t count, size_t size);
struct Conext { char c[100]; };
void (**v) (struct Context*);
__code (**code) (struct Context*);

int main(int argc, char **argv){
        v = (void (**) (struct Context*)) NEWN(ALLOCATE_SIZE, void*);
        code = (__code(**) (struct Context*)) NEWN(ALLOCATE_SIZE, void*);
	printf("main start\n");
	//goto cs2(2.22, 3.33, 4, 5.55);
	test_goto();
	return 0;
}

void test_goto(){
	goto cs1(10, 20, 30, 40);
}

__code cs1(int a, int b, int c, int d){
	printf("%4d, %4d, %4d, %4d\n", a, b, c, d);
	a += 40, b += 40, c += 40, d += 40;
	goto cs2((double)a, (double)b, c, (double)d);
}

__code cs2(double a, double b, int c, double d){
	printf("%4d, %4d, %4d, %4d\n", (int)a, (int)b, (int)c, (int)d);
	a += 40, b += 40, c += 40, d += 40;
	goto cs_exit((int)a, b, (char)c);
}

__code cs_exit(int a, double b, char c){
	printf("%4d, %4d, %4d\n", (int)a, (int)b, (int)c);
	printf("cs_exit was called!\n");
	exit(0);
}


void caller(int a, double b){
	cs2(b,20.0,a, 40.4);
	cs1(10,20,30, 40);
}

