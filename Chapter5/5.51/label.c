#include <stdlib.h>
#include <stdio.h>
#include "basic.h"
#include "others.h"
#include "label.h"

extern item val, exp, unev, argl, env, proc;
extern item the_continue;
extern item next;

//from SICP

void eval_dispatch()
{
	if (is_self_evaluating(exp)){
		next = (item){ t_goto, ev_self_eval };
	}
	else if (is_variable(exp)){
		next = (item){ t_goto, ev_variable };
	}
	else if (is_quoted(exp)){
		next = (item){ t_goto, ev_quoted };
	}
	else if (is_assignment(exp)){
		next = (item){ t_goto, ev_assignment };
	}
	else if (is_definition(exp)){
		next = (item){ t_goto, ev_definition };
	}
	else if (is_if(exp)){
		next = (item){ t_goto, ev_if };
	}
	else if (is_lambda(exp)){
		next = (item){ t_goto, ev_lambda };
	}
	else if (is_begin(exp)){
		next = (item){ t_goto, ev_begin };
	}
	else if (is_application(exp)){
		next = (item){ t_goto, ev_application };
	}
	else
		next = (item){ t_goto, unknow_expression_type };
}

void ev_self_eval()
{
	val = exp;
	next =  the_continue;
}

void ev_variable()
{
	val = lookup_variable_value(exp, env);
	next =  the_continue ;
}

void ev_quoted()
{
	val = text_of_quotation(exp);
	next = the_continue;
}

void ev_lambda()
{
	unev = lambda_parameters(exp);
	exp = lambda_body(exp);
	val = make_procedure(unev, exp, env);
	next = the_continue;
}

void ev_application()
{
	save(the_continue);
	save(env);
	unev = operands(exp);
	save(unev);
	exp = the_operator(exp);
	the_continue = (item){ t_goto, ev_appl_did_operator };
	next = (item){ t_goto, eval_dispatch };
}

void ev_appl_did_operator()
{
	unev = restore();
	env = restore();
	argl = nil();
	proc = val;
	if (no_operands(unev)){
		next = (item){ t_goto, apply_dispatch };
	}
	else {
		save(proc);
		next = (item){ t_goto, ev_appl_operand_loop };
	}
}

void ev_appl_operand_loop()
{
	save(argl);
	exp = first_operand(unev);
	if (last_operand(unev)){
		next = (item){ t_goto, ev_appl_last_arg };
	}
	else {
		save(env);
		save(unev);
		the_continue = (item){ t_goto, ev_appl_accumulate_arg };
		next = (item){ t_goto, eval_dispatch };
	}
}

void ev_appl_accumulate_arg()
{
	unev = restore();
	env = restore();
	argl = restore();
	argl = adjoin_arg(val, argl);
	unev = rest_operands(unev);
	next = (item){ t_goto, ev_appl_operand_loop };
}

void ev_appl_last_arg()
{
	the_continue = (item){ t_goto, ev_appl_accum_last_arg };
	next = (item){ t_goto, eval_dispatch };
}

void ev_appl_accum_last_arg()
{
	argl = restore();
	argl = adjoin_arg(val, argl);
	proc = restore();
	next = (item){ t_goto, apply_dispatch };
}

void apply_dispatch()
{
	if (is_primitive_procedure(proc)){
		next = (item){ t_goto, primitive_apply };
	}
	else if (is_compound_procedure(proc)){
		next = (item){ t_goto, compound_apply };
	}
	else
		next = (item){ t_goto, unknown_procedure_type };
}

void primitive_apply()
{
	val = apply_primitive_procedure(proc, argl);
	the_continue = restore();
	next = the_continue;
}

void compound_apply()
{
	unev = procedure_parameters(proc);
	env = procedure_environment(proc);
	env = extend_environment(unev, argl, env);
	unev = procedure_body(proc);
	next = (item){ t_goto, ev_sequence };
}

void ev_begin()
{
	unev = begin_actions(exp);
	save(the_continue);
	next = (item){ t_goto, ev_sequence };
}

void ev_sequence()
{
	exp = first_exp(unev);
	if (last_exp(unev)){
		next = (item){ t_goto, ev_sequence_last_exp };
	}
	else {
		save(unev);
		save(env);
		the_continue = (item){ t_goto, ev_sequence_continue };
		next = (item){ t_goto, eval_dispatch };
	}
}

void ev_sequence_continue()
{
	env = restore();
	unev = restore();
	unev = rest_exps(unev);
	next = (item){ t_goto, ev_sequence };
}

void ev_sequence_last_exp()
{
	the_continue = restore();
	next = (item){ t_goto, eval_dispatch };
}

void ev_if()
{
	save(exp);
	save(env);
	save(the_continue);
	the_continue = (item){ t_goto, ev_if_decide };
	exp = if_predicate(exp);
	next = (item){ t_goto, eval_dispatch };
}

void ev_if_decide()
{
	the_continue = restore();
	env = restore();
	exp = restore();
	if (val.content.number){                         //
		next = (item){ t_goto, ev_if_consequent };
	}
	else
		next = (item){ t_goto, ev_if_alternative };
}

void ev_if_alternative()
{
	exp = if_alternative(exp);
	next = (item){ t_goto, eval_dispatch };
}

void ev_if_consequent()
{
	exp = if_consequent(exp);
	next = (item){ t_goto, eval_dispatch };
}

void ev_assignment()
{
	unev = assignment_variable(exp);
	save(unev);
	exp = assignment_value(exp);
	save(env);
	save(the_continue);
	the_continue = (item){ t_goto, ev_assignment_l };
	next = (item){ t_goto, eval_dispatch };
}

void ev_assignment_l()
{
	the_continue = restore();
	env = restore();
	unev = restore();
	set_variable_value(unev, val, env);
	val = make_item("ok!");
	next = the_continue;
}

void ev_definition()
{
	unev = definition_variable(exp);
	save(unev);
	exp = definition_value(exp);
	save(env);
	save(the_continue);
	the_continue = (item){ t_goto, ev_definition_l };
	next = (item){ t_goto, eval_dispatch };
}

void ev_definition_l()
{
	the_continue = restore();
	env = restore();
	unev = restore();
	define_variable(unev, val, env);
	val = make_item("ok");
	next = the_continue;
}


void unknow_expression_type(){
	fprintf(stderr, "unknow_expression_type");
	next.type = t_goto;
	next.content.label = NULL;
}
void unknown_procedure_type(){
	fprintf(stderr, "unknown_procedure_type");
	next.type = t_goto;
	next.content.label = NULL;
}


