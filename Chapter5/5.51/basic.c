#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include "basic.h"
#include "others.h"
#include "gc.h"
#define STRING_LEN 100

extern item *the_cars;
extern item *the_cdrs;
extern item the_stack;
extern item the_free ;

char * string_list[STRING_LEN];
int string_free = 0;

char *make_string(const char * str){
	for (int i = 0; i < string_free; ++i){
		if (strcmp(string_list[i], str) == 0){
			return string_list[i];
		}
	}
	string_list[string_free] = (char *)malloc(strlen(str) + 1);
	strcpy(string_list[string_free], str);
	return string_list[string_free++];
}

void free_all_string(){
	for (int i = 0; i < string_free; ++i){
		free(string_list[i]);
	}
}

item vector_ref(item *vector, item reg){
	return vector[reg.content.number];
}
void vector_set(item *vector, item reg1, item reg2){
	vector[reg1.content.number] = reg2;
}

//
//输入一个字符串
//返回一个scheme类型
item make_item(char * str)
{
	char * ptr = str;
	if (isdigit(*ptr) || (isdigit(*(ptr + 1)) && (*ptr == '-' || *ptr == '+'))) {

		while (*++ptr != '\0'){
			if (!isdigit(*ptr)){
				break;
			}
		}
		if (*ptr == '\0'){
			item temp;
			temp.type = t_number;
			temp.content.number = atoi(str);
			return temp;
		}
	}
	if (str[0] == '\''){
		char *temp = make_string(str + 1);
		item t;
		t.type = t_quoted;
		t.content.string = temp;
		return t;
	}
	char * temp = make_string(str);
	item t;
	t.type = t_symbol;
	t.content.string = temp;
	return t;
}


item cons(const item a, const item b)
{
	vector_set(the_cars, the_free, a);
	vector_set(the_cdrs, the_free, b);
	item temp = the_free;
	++the_free.content.number;
	return temp;
}

item car(const item p)
{
	if (p.type != t_pair){
		fprintf(stderr, "car : contract violation");
		exit(1);
	}
	return vector_ref(the_cars, p);
}
item cdr(const item p)
{
	if (p.type != t_pair){
		fprintf(stderr, "cdr : contract violation");
		exit(1);
	}
	return  vector_ref(the_cdrs, p);
}

void set_car(item p, item n){
	vector_set(the_cars, p, n);
}
void set_cdr(item p, item n){
	vector_set(the_cdrs, p, n);
}


void copy_item(item *a, item b)
{
	if (b.type != t_symbol && b.type != t_quoted){
		*a = b;
	}
	else{
		a->type = b.type;
		a->content.string = b.content.string;
		strcpy(a->content.string, b.content.string);
	}
}

//返回空表
item nil()
{
	item temp;
	temp.type = t_null;
	return temp;
}


//stack
void save(item i){
	the_stack = cons(i, the_stack);
}
item restore(){
	item temp = car(the_stack);
	the_stack = cdr(the_stack);
	return temp;
}
void initialize_stack(){
	the_stack = nil();
}



//


void show_pair(const item p)
{
	show_item(car(p));
	if (is_null(cdr(p))){
		printf(" )");
	}
	else if (!is_pair(cdr(p))) {
		printf(" .");
		show_item(cdr(p));
		printf(" )");
	}
	else
		show_pair(cdr(p));
}

void show_item(const item p)
{
	if (p.type == t_number){
		printf(" %d", p.content.number);
	}
	else if (p.type == t_proc){
		printf(" <proc>");
	}
	else if (is_pair(p)){
		if (is_compound_procedure(p)){
			printf(" ( \"procedure\" ");
			show_item(car(cdr(p)));
			printf(" )");
		}
		else {
			printf(" (");
			show_pair(p);
		}
	}
	else if (is_null(p)){
		printf(" ()");
	}
	else printf(" %s", p.content.string);
}





bool is_pair(item i){
	return i.type == t_pair;
}

bool is_null(item i){
	return i.type == t_null;
}

item append(item x, item y){
	if (is_null(x)){
		return y;
	}
	else{
		return cons(car(x), append(cdr(x), y));
	}
}
item list_helper(va_list ap, int lim){
	if (lim == 0){
		return nil();
	}
	else{
		item first = va_arg(ap, item);
		return cons(first, list_helper(ap, lim - 1));
	}
}
item list(int lim, ...){
	va_list ap;
	va_start(ap, lim);
	return list_helper(ap, lim);
}




int length(item i)
{
	int num = 0;
	while (!is_null(i)){
		i = cdr(i);
		++num;
	}
	return num;
}

bool eq(item x, item y){
	if (x.type == y.type){
		switch (x.type)
		{
		case t_null:return true;
		case t_number:
		case t_pair : return x.content.number == y.content.number;
		case t_symbol:
		case t_quoted: return x.content.string == y.content.string;
		default:
			return false;
		}
	}
	return false;
}


item map(item(*proc)(item), item items){
	if (is_null(items)){
		return items;
	}
	else
		return cons((*proc)(car(items)), map(proc, cdr(items)));
}