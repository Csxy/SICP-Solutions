#include "basic.h"
#include "others.h"
#include "gc.h"



item *the_cars = memory1;
item *the_cdrs = memory2;
item *new_cars = memory3;
item *new_cdrs = memory4;

item *temp;
extern item val, exp, unev, argl, env, proc, the_stack, the_continue, next, alist, result;
extern item the_free;
item the_scan, root;
item older, newer,oldcr;

void update(item *reg){
	older = *reg;
	relocate_old_result_in_new();
	*reg = newer;
}

void update_register(){
	update(&val);
	update(&exp);
	update(&unev);
	update(&argl);
	update(&env);
	update(&proc);
	update(&the_stack);
	update(&alist);
	update(&result);
	update(&the_continue);
	update(&next);
}

void begin_garbage_collection()
{
	the_free = (item){ t_pair, 0 };
	the_scan = (item){ t_pair, 0 };
	update_register();
	while (the_scan.content.number != the_free.content.number)
	{
		older = vector_ref(new_cars, the_scan);
		relocate_old_result_in_new();
		vector_set(new_cars, the_scan, newer);

		older = vector_ref(new_cdrs, the_scan);
		relocate_old_result_in_new();
		vector_set(new_cdrs, the_scan, newer);
		++the_scan.content.number;
	}
	
	temp = the_cdrs;
	the_cdrs = new_cdrs;
	new_cdrs = temp;
	temp = the_cars;
	the_cars = new_cars;
	new_cars = temp;
}

void relocate_old_result_in_new()
{
	if (!is_pair(older)){
		newer = older;
		return;
	}
	oldcr = car(older);
	if (oldcr.type == t_broken_heart){
		newer = vector_ref(the_cdrs, older);
		return;
	}
	newer = the_free;
	++the_free.content.number;
	vector_set(new_cars, newer, oldcr);
	oldcr = vector_ref(the_cdrs, older);
	vector_set(new_cdrs, newer, oldcr);

	item broken;
	broken.type = t_broken_heart;
	vector_set(the_cars, older, broken);
	vector_set(the_cdrs, older, newer);
	return;
}