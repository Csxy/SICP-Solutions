#ifndef OTHERS_H
#define OTHERS_H
#include <stdbool.h>
#include "basic.h"


bool tagged_list(item exp, const char * tag);

bool is_self_evaluating(item exp);
bool is_variable(item exp);
bool is_quoted(item exp);
bool is_assignment(item exp);
bool is_definition(item exp);
bool is_if(item exp);
bool is_lambda(item exp);
bool is_begin(item exp);
bool is_application(item exp);

item lookup_variable_value(item exp, item env);
item text_of_quotation(item exp);
item lambda_parameters(item exp);
item lambda_body(item exp);
item make_procedure(item unev,item exp,item env);
item operands(item exp);
item the_operator(item exp);
bool no_operands(item unev);
item first_operand(item unev);
bool last_operand(item unev);
bool last_exp(item exp);
item adjoin_arg(item val, item argl);
item rest_operands(item unev);
bool is_primitive_procedure(item proc);
bool is_compound_procedure(item proc);
item apply_primitive_procedure(item proc, item argl);
item procedure_parameters(item proc);
item procedure_environment(item proc);
item extend_environment(item unev, item argl, item env);
item procedure_body(item proc);
item begin_actions(item exp);
item first_exp(item unev);
item rest_exps(item unev);
item if_predicate(item exp);
item if_alternative(item exp);
item if_consequent(item exp);
item assignment_variable(item exp);
item assignment_value(item exp);
void set_variable_value(item unev, item val, item env);
item definition_variable(item exp);
item definition_value(item exp);
void define_variable(item unev, item val, item env);
item setup_environment();

#endif