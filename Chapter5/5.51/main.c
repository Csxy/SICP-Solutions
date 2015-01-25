#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "label.h"
#include "basic.h"
#include "parsing.h"
#include "others.h"
#include "gc.h"



item val, exp, unev, argl,alist, result;
item env, proc, the_stack;
item the_free = { t_pair, 0 };
item the_continue, next;


int main(void)
{
	env = setup_environment();
	while (1){
		initialize_stack();
		puts("\n;;; EC-Eval input:");
		char *str = read();
		alist = tokenized(str);
		result = parse(&alist);
		exp = result;
		next.type = t_goto;
		next.content.label = eval_dispatch;
		the_continue.type = t_goto;
		the_continue.content.label = NULL;
		while (next.content.label != NULL)
		{
			if (the_free.content.number > MAX_MEMORY - 20){
				begin_garbage_collection();
			}
			(*next.content.label)();
		}
		puts(";;; EC-Eval value:");
		show_item(val);
		free(str);
	}
	free_all_string();
}