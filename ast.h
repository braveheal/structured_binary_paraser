#ifndef _SBP_AST_H
#define _SBP_AST_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <glib.h>

#define FOREACH_SBP_NODES(F) \
	F(NODE_UNKNOWN) \
	F(NODE_ROOT) \
	F(NODE_ERROR) \
	F(NODE_EVENT) \
	F(NODE_CTF_EXPRESSION) \
	F(NODE_UNARY_EXPRESSION) \
	F(NODE_TYPEALIAS) \
	F(NODE_TYPE_SPECIFIER) \
	F(NODE_TYPE_DECLARATOR) \
	F(NODE_ENUMERATOR) \
	F(NODE_STRUCT_OR_VARIANT_DECLARATION) \
	F(NODE_SKIP) \
    F(NODE_MATH_EXPRESSION) \
	F(NODE_STATEMENT) \


enum node_type {
#define ENTRY(S)	S,
	FOREACH_SBP_NODES(ENTRY)
#undef ENTRY
	NR_NODE_TYPES,
};

static const char *node_type_to_str[] = {
#define ENTRY(S)	[S] = #S,
	FOREACH_SBP_NODES(ENTRY)
#undef ENTRY
};

struct sbp_ast {

	unsigned int lineno;
	/*
	 * We mark nodes visited in the generate-ir phase (last
	 * phase). We only mark the 1-depth level nodes as visited
	 * (never the root node, and not their sub-nodes). This allows
	 * skipping already visited nodes when doing incremental
	 * metadata append.
	 */
	int visited;

	enum node_type type;
	union {
		struct {
			enum {
				UNARY_UNKNOWN = 0,
				UNARY_STRING,
				UNARY_SIGNED_CONSTANT,
				UNARY_UNSIGNED_CONSTANT,
				UNARY_FLOAT_CONSTANT,
				UNARY_SBRAC,
				UNARY_LITERAL
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
				//struct sbp_ast *sbrac_exp;
			} u;
			enum {
				UNARY_LINK_UNKNOWN = 0,
				UNARY_DOTLINK,
			} link;
		} unary_expression;
		struct {
			//struct sbp_ast *field_class_specifier_list;
			//struct bt_list_head field_class_declarators;
			char *alias;
		} field_class_alias;
		struct {
			uint64_t length;
		} skip_expression;	
		struct {
			enum {
				TYPESPEC_UNKNOWN = 0,
				TYPESPEC_ID_TYPE,
				TYPESPEC_FLOATING_POINT,
				TYPESPEC_INTEGER,
				TYPESPEC_STRING,
				TYPESPEC_STRUCT,
				TYPESPEC_VARIANT,
				TYPESPEC_ENUM,
			} type;
			/* For struct, variant and enum */
			//struct sbp_ast *node;
			const char *id_type;
		} type_specifier;
		struct {
			gboolean show_only_if;
			gboolean formula;
		} struct_or_variant_declaration;
		struct {
			enum {
				EXP_ASSIGN = 0,
				EXP_TYPE_ASSIGN,
			} type;
		} ctf_expression;
		struct {
			char *id;
		} enumerator;
		struct {
			enum {
				NORMAL = 0,
				_DEFAULT
			} type;
			uint64_t constant;
		} label_statement;
		struct 
		{
			enum{
				MATH_ADD,
				MATH_SUB,
				MATH_PLUS,
				MATH_MINUS,				
				MATH_NOT,
				MATH_MULTIPLY,
				MATH_DIV,
				MATH_MOD,
				MATH_SL,
				MATH_SR,
				MATH_LT,
				MATH_GT,
				MATH_LTEQ,
				MATH_GTEQ,
				MATH_EQEQ,
				MATH_NOTEQ,
				MATH_AND,
				MATH_OR,
				MATH_ANDAND,
				MATH_OROR,
				MATH_CONDITIONAL,
				MATH_SELECTION,
				MATH_LOGARITHM,
				MATH_POW,
				MATH_LENVAR,
			} type;			
		} math_expression;
		
	} u;
};

#endif /* _SBP_AST_H */