#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include "others.h"
#include "basic.h"



bool tagged_list(item exp, const char *tag)
{
	if (exp.type == t_pair){
		return eq(car(exp), make_item((char *)tag));
	}
	else return false;
}

bool is_self_evaluating(item exp)
{
	return exp.type == t_number;
}

bool is_variable(item exp)
{
	return exp.type == t_symbol;
}

bool is_quoted(item exp)
{
	return exp.type == t_quoted;
}

bool is_assignment(item exp)
{
	return tagged_list(exp, "set!");
}

bool is_definition(item exp)
{
	return tagged_list(exp, "define");
}

bool is_if(item exp)
{
	return tagged_list(exp, "if");
}
bool is_lambda(item exp)
{
	return tagged_list(exp, "lambda");
}
bool is_begin(item exp)
{
	return tagged_list(exp, "begin");
}
bool is_application(item exp)
{
	return exp.type == t_pair;
}


item text_of_quotation(item exp)
{
	exp.type = t_symbol;
	return exp;
}

item lambda_parameters(item exp){
	return car(cdr(exp));
}

item lambda_body(item exp){
	return cdr(cdr(exp));
}
item make_lambda(item para, item body){
	return cons(make_item("lambda") , cons(para, body));
}
item make_procedure(item unev, item exp, item env){
	return list(4, make_item("procedure"), unev, exp, env);
}

item operands(item exp){
	return cdr(exp);
}
item the_operator(item exp){
	return car(exp);
}
bool no_operands(item unev){
	return is_null(unev);
}
item first_operand(item unev){
	return car(unev);
}
bool last_operand(item unev){
	return is_null(cdr(unev));
}

bool last_exp(item exp){
	return is_null(cdr(exp));
}

item adjoin_arg(item val, item argl){
	return append(argl, cons(val, nil()));
}

item rest_operands(item unev){
	return cdr(unev);
}
bool is_primitive_procedure(item proc){
	return tagged_list(proc, "primitive");
}
bool is_compound_procedure(item proc){
	return tagged_list(proc, "procedure");
}
item apply_primitive_procedure(item proc, item argl){
	return (*car(cdr(proc)).content.function)(argl);
}
item procedure_parameters(item proc){
	return car(cdr(proc));
}
item procedure_environment(item proc){
	return car(cdr(cdr(cdr(proc))));
}

item procedure_body(item proc){
	return car(cdr(cdr(proc)));
}
item begin_actions(item exp){
	return cdr(exp);
}
item first_exp(item unev){
	return car(unev);
}
item rest_exps(item unev){
	return cdr(unev);
}
item if_predicate(item exp){
	return car(cdr(exp));
}
item if_alternative(item exp){
	if (!is_null(cdr(cdr(cdr(exp))))){
		return car(cdr(cdr(cdr(exp))));
	}
	else{
		return nil();
	}
}
item if_consequent(item exp){
	return car(cdr(cdr(exp)));
}
item assignment_variable(item exp){
	return car(cdr(exp));
}
item assignment_value(item exp){
	return car(cdr(cdr(exp)));
}



item definition_variable(item exp){
	if (car(cdr(exp)).type == t_symbol){
		return car(cdr(exp));
	}
	else
		return car(car(cdr(exp)));
}
item definition_value(item exp){
	if (car(cdr(exp)).type == t_symbol){
		return car(cdr(cdr(exp)));
	}
	else
		return make_lambda(cdr(car(cdr(exp))),
		cdr(cdr(exp)));
}

// environment
item enclosing_environment(item env){
	return cdr(env);
}
item first_frame(item env){
	return car(env);
}
item the_empty_environment(){
	return nil();
}

item make_frame(item variables, item values){
	return cons(variables, values);
}

item frame_variables(item frame){
	return car(frame);
}

item frame_value(item frame){
	return cdr(frame);
}

void add_binding_to_frame(item var, item val, item frame){
	set_car(frame, cons(var, car(frame)));
	set_cdr(frame, cons(val, cdr(frame)));
}

item extend_environment(item unev, item argl, item env){
	if (length(unev) == length(argl)){
		return cons(make_frame(unev, argl), env);
	}
	else{
		fprintf(stderr, "Too many or too few arguments supplied");
		exit(1);
	}
}


item lookup_variable_value(item exp, item env){
	item vars, vals;
	while (!eq(env, the_empty_environment())){
		vars = frame_variables(first_frame(env));
		vals = frame_value(first_frame(env));
		while (1){
			if (is_null(vars)){
				env = enclosing_environment(env);
				break;
			}
			else if (eq(exp, car(vars))){
				return car(vals);
			}
			else {
				vars = cdr(vars);
				vals = cdr(vals);
			}
		}
	}
	fprintf(stderr, "Unbound variable");
	exit(1);
}

void set_variable_value(item unev, item val, item env){
	item vars, vals;
	while (!eq(env, the_empty_environment())){
		vars = frame_variables(first_frame(env));
		vals = frame_value(first_frame(env));
		while (1){
			if (is_null(vars)){
				env = enclosing_environment(env);
				break;
			}
			else if (eq(unev, car(vars))){
				set_car(vals, val);
			}
			else {
				vars = cdr(vars);
				vals = cdr(vals);
			}
		}
	}
}
void define_variable(item unev, item val, item env){
	item frame = first_frame(env);
	item vars, vals;
	vars = frame_variables(frame);
	vals = frame_value(frame);
	while (1){
		if (is_null(vars)){
			add_binding_to_frame(unev, val, frame);
			break;
		}
		else if (eq(unev, car(vars))){
			set_car(vals, val);
			break;
		}
		else{
			vars = cdr(vars);
			vals = cdr(vals);
		}
	}
}

item base_car(item argl){
	return car(argl);
}
item base_cdr(item argl){
	return cdr(argl);
}
item base_cons(item argl){
	return cons(car(argl), car(cdr(argl)));
}

item base_list(item argl){
	if (!is_null(argl)){
		return cons(car(argl), base_list(cdr(argl)));
	}
	else
		return nil();
}

item base_set_car(item argl){
	set_car(car(argl), car(cdr(argl)));
	return make_item("ok");
}

item base_set_cdr(item argl){
	set_cdr(car(argl), car(cdr(argl)));
	return make_item("ok");
}

item base_equal(item argl){
	item temp;
	temp.type = t_number;
	temp.content.number = ((car(argl).content.number) == (car(cdr(argl)).content.number));
	return temp;
}

item base_multiply(item argl){
	int num = 1;
	while (!is_null(argl)){
		num = num * car(argl).content.number;
		argl = cdr(argl);
	}
	item temp;
	temp.type = t_number;
	temp.content.number = num;
	return temp;
}

item base_sub(item argl){
	item temp;
	temp.type = t_number;
	temp.content.number = car(argl).content.number - car(cdr(argl)).content.number;
	return temp;
}

item base_add(item argl){
	int mul = 0;
	while (!is_null(argl)){
		mul = mul + car(argl).content.number;
		argl = cdr(argl);
	}
	item temp;
	temp.type = t_number;
	temp.content.number = mul;
	return temp;
}

item make_func(item(*func)(item)){
	item temp;
	temp.type = t_proc;
	temp.content.function = func;
	return temp;
}

item primitive_procedures(){
	item temp = list(10,
		list(2, make_item("car"), make_func(base_car)),
		list(2, make_item("cdr"), make_func(base_cdr)),
		list(2, make_item("cons"), make_func(base_cons)),
		list(2, make_item("list"), make_func(base_list)),
		list(2, make_item("set-car!"), make_func(base_set_car)),
		list(2, make_item("set-cdr!"), make_func(base_set_cdr)),
		list(2, make_item("+"), make_func(base_add)),
		list(2, make_item("-"), make_func(base_sub)),
		list(2, make_item("="), make_func(base_equal)),
		list(2, make_item("*"), make_func(base_multiply)));
	return temp;
}

item primitive_procedure_names(){
	return map(car, primitive_procedures());
}

item helper_pri(item i){
	return list(2, make_item("primitive"), car(cdr(i)));
}
item primitive_procedure_objects(){
	return map(helper_pri, primitive_procedures());
}

item setup_environment(){
	item initial_env = extend_environment(primitive_procedure_names(), primitive_procedure_objects(), the_empty_environment());
	item t, f;
	t.type = f.type = t_number;
	t.content.number = 1;
	f.content.number = 0;
	define_variable(make_item("true"), t, initial_env);
	define_variable(make_item("false"), f, initial_env);
	return initial_env;
}