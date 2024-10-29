#ifndef _RULE_H
#define _RULE_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <glib.h>

#define FOREACH_VP_NODES(F) \
	F(VP_ROOT) \
	F(VP_RULE) \
	F(VP_FILTER) \
	F(VP_UNARY_EXPRESSION) \
	F(VP_REDUCE) \
    F(VP_MATH_EXPRESSION) \
	F(VP_EVENT_EXPRESSION) \

enum vp_node_type {
#define ENTRY(S)	S,
	FOREACH_VP_NODES(ENTRY)
#undef ENTRY
};

static const char *vpnode_type_to_str[] = {
#define ENTRY(S)	[S] = #S,
	FOREACH_VP_NODES(ENTRY)
#undef ENTRY
};

struct PathScanner {
  char *top, *cur, *ptr, *pos;
  int line;  
  GNode *root;
  GStringChunk *chunk;
};

enum vp_compare_type {
    FILTER_LT,
    FILTER_GT,
    FILTER_LTEQ,
    FILTER_GTEQ,
    FILTER_EQ,
    FILTER_NOTEQ,
    FILTER_IN,
    FILTER_CONTANINS,
};

enum vp_math_type {
	VP_MATH_ADD,
	VP_MATH_SUB,
	VP_MATH_PLUS,
	VP_MATH_MINUS,				
	VP_MATH_NOT,
	VP_MATH_MULTIPLY,
	VP_MATH_DIV,
	VP_MATH_MOD,
	VP_MATH_SL,
	VP_MATH_SR,
	VP_MATH_LT,
	VP_MATH_GT,
	VP_MATH_LTEQ,
	VP_MATH_GTEQ,
	VP_MATH_EQEQ,
	VP_MATH_NOTEQ,
	VP_MATH_CONCAT,
	VP_MATH_ANDAND,
	VP_MATH_OROR,
	VP_MATH_CONDITIONAL,
	VP_MATH_IN,
	VP_MATH_CONTANINS,
};

struct vp_ast {
    enum vp_node_type type;
    union
    {
		struct {
			enum {
				VP_UNARY_STRING,
				VP_UNARY_SIGNED_CONSTANT,
				VP_UNARY_UNSIGNED_CONSTANT,
				VP_UNARY_FLOAT_CONSTANT,
				VP_UNARY_SBRAC,
				VP_UNARY_ARRAY,
                VP_UNARY_BRACE,
                VP_UNARY_PARENTHESES,
				VP_UNARY_LITERAL,
                VP_UNARY_LITERAL_SINGLE,
                VP_UNARY_PARENT,
                VP_UNARY_CURRENT,
                VP_UNARY_ROOT,
                VP_UNARY_WILDCARD,
                VP_UNARY_DECENDANT,
				VP_UNARY_SUM,
				VP_UNARY_AVERAGE,
				VP_UNARY_MIN,
				VP_UNARY_MAX,
				VP_UNARY_COLLECTION,
			} type;
			union {
				/*
				 * string for identifier, id_type, keywords,
				 * string literals and character constants.
				 */
				char *string;
				int64_t signed_constant;
				uint64_t unsigned_constant;
				double float_constant;
				//struct ctf_ast *sbrac_exp;
			} u;
		} unary_expression;
        struct
        {
			enum vp_math_type type;			
        } math_expression;
        struct
        {
			enum vp_compare_type type;	
        } filter;       
        struct
        {
            char *name;            
        } rule;   
		struct
		{
			char *name;
			uint64_t code; 
		} event;
		  
    } u;
    
};

#endif 