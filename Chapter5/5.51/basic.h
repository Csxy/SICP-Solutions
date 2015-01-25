#ifndef PAIR_H
#define PAIR_H
#include <stdbool.h>
typedef void(*label)();

typedef struct Pair pair;
typedef struct Item item;
struct Item{

	enum{ t_quoted, t_number, t_symbol, t_pair, t_null, t_proc, t_broken_heart, t_goto } type;
	union contents{
		void(*label)();
		item(*function)(item);
		int number;
		char *string;
	} content;
};

void free_all_string();

// vector
item vector_ref(item *vector, item reg);
void vector_set(item *vector, item reg1, item reg2);
void show_item(const item p);
void copy_item(item *a, item b);
item make_item(char * str);

// pair
item cons(const item a, const item b);
item car(const item p);
item cdr(const item p);
void set_car(item p, item n);
void set_cdr(item p, item n);
item nil();

//stack
void save(item i);
item restore();
void initialize_stack();

//other
bool is_pair(item i);
bool is_null(item i);
item append(item x, item y);
item list(int lim, ...);
int length(item i);

bool eq(item x, item y);

item map(item(*proc)(item), item);

#endif