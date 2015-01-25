#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdbool.h>
#include "basic.h"
#include "others.h"

//给( , )旁边加上空格
char * read()
{
	char ch;
	char temp[81];
	char * ptr = temp;
	
	int parentheses = 0;
	bool flag = true;
	do{
		ch = getchar();
		if (ch == '('){
			*ptr++ = ch;
			*ptr++ = ' ';
			++parentheses;
			flag = false;
		}
		else if (ch == ')'){
			*ptr++ = ' ';
			*ptr++ = ch;
			--parentheses;
		}
		else if (isspace(ch)){
			*ptr++ = ' ';
			flag = false;
		}
		else{
			*ptr++ = ch;
		}
	} while (flag || parentheses);
	fflush(stdin);
	*ptr = '\0';
	char * str = (char *)malloc(strlen(temp) + 1);
	strcpy(str, temp);
	return str;
}

item tokenized(char *str)
{
	char *pt = strtok(str," ");
	if (pt){
		item first = make_item(pt);
		item rest = tokenized(NULL);
		return cons(first, rest);
	}
	return nil();
}

item parse(item *alist)
{
	if (!is_null(*alist) ){

		item first;
		copy_item(&first , car(*alist));
		*alist = cdr(*alist);

		if (first.type == t_symbol && first.content.string[0] == '('){
			item f;
			copy_item(&f, parse(alist));

			if (!is_null(*alist)){
				return cons(f, parse(alist));
			}
			else
				return f;
		}
		else if (first.type == t_symbol && first.content.string[0] == ')'){
			return nil();
		}
		else{
			if (!is_null(*alist)){
				return cons(first, parse(alist));
			}
			else
				return first;
		}
	}
	else
		return nil();
}
