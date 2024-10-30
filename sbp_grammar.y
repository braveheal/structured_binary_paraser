%token_type { char* }
%default_type { GNode* }
/*%default_destructor  { type_destructor($$); }*/
%type keyword {char*}
%type struct_or_variant_declaration_list { GSList * }
%type assignment_expression_list { GSList * }
%type enumerator_list { GSList * }
%type statement_list { GSList * }

%include {
#include <assert.h>
#include "ast.h"
#include <glib.h>
#include "scanner.h"

typedef struct sbp_ast* ast;

GNode *make_node(enum node_type type)
{
	ast node;
	node = malloc(sizeof(struct sbp_ast));
	if(!node) {
		printf("error construction.\n");
		return NULL;
	} else {
		node->type = type;
		//printf("%s\n", node_type_to_str[type]); 
		return g_node_new((gpointer)node); 
	}
}

void removeChar(char *str, char garbage) {

    char *src, *dst;
    for (src = dst = str; *src != '\0'; src++) {
        *dst = *src;
        if (*dst != garbage) dst++;
    }
    *dst = '\0';
}

void init_scope(struct ctf_scanner_scope *scope,
		       struct ctf_scanner_scope *parent)
{
	scope->parent = parent;
	scope->classes = g_hash_table_new_full(g_str_hash, g_str_equal,
					     NULL, NULL);
}

void finalize_scope(struct ctf_scanner_scope *scope)
{
	g_hash_table_destroy(scope->classes);
}

void push_scope(Scanner *scanner)
{
	struct ctf_scanner_scope *ns;

	//BT_LOGT("Pushing scope: scanner-addr=%p", scanner);
	ns = malloc(sizeof(struct ctf_scanner_scope));
	init_scope(ns, scanner->cs);
	scanner->cs = ns;
}

void pop_scope(Scanner *scanner)
{
	struct ctf_scanner_scope *os;

	//BT_LOGT("Popping scope: scanner-addr=%p", scanner);
	os = scanner->cs;
	scanner->cs = os->parent;
	finalize_scope(os);
	free(os);
}

int lookup_type(struct ctf_scanner_scope *s, const char *id)
{
	int ret;

	ret = GPOINTER_TO_INT(g_hash_table_lookup(s->classes, id));
	//BT_LOGT("Looked up type: scanner-addr=%p, id=\"%s\", ret=%d",
	//	s, id, ret);
	return ret;
}

  gchar* name_str;  

int is_type(Scanner *scanner)
{
    name_str = g_string_chunk_insert_len (scanner->chunk, scanner->top, scanner->cur - scanner->top);
  
	struct ctf_scanner_scope *it;
	int ret = 0;

	for (it = scanner->cs; it; it = it->parent) {
		if (lookup_type(it, name_str)) {
			ret = 1;
			break;
		}
	}
	//BT_LOGT("Found if ID is type: scanner-addr=%p, id=\"%s\", ret=%d",
	//	scanner, id, ret);
	return ret;
}

void add_type(Scanner *scanner, char *id)
{
	//BT_LOGT("Adding type: scanner-addr=%p, id=\"%s\"",
	//	scanner, id);
	if (lookup_type(scanner->cs, id))
		return;
	g_hash_table_insert(scanner->cs->classes, id, id);
}

}

%extra_argument { Scanner *scanner }

%parse_accept { printf("The parser has completed successfully.\n"); }

%syntax_error { printf("Error\n"); }

%parse_failure { fprintf(stderr, "Parse failure\n"); }

%stack_size 20000

%start_symbol program

postfix_expression(A) ::= IDENTIFIER(B).
{
	A = make_node(NODE_UNARY_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.unary_expression.type = UNARY_STRING;
	a->u.unary_expression.u.string = B;
	//printf("%s\n",B);
}
postfix_expression(A) ::= ID_TYPE(B).
{
	A = make_node(NODE_UNARY_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.unary_expression.type = UNARY_STRING;
	a->u.unary_expression.u.string = B;
	//printf("%s\n",B);
}
postfix_expression(A) ::= INT_LITERAL(B).
{
	A = make_node(NODE_UNARY_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.unary_expression.type = UNARY_UNSIGNED_CONSTANT;
	uint64_t u;
	if(strncmp(B, "0x", strlen("0x")) == 0 || strncmp(B, "0X", strlen("0X")) == 0 ){
		u = strtoull(B, NULL, 16);
	} else if(strncmp(B, "0", strlen("0")) == 0) {
		u = strtoull(B, NULL, 8);
	} else {
		u = strtoull(B, NULL, 10);
	}
	a->u.unary_expression.u.unsigned_constant = u;
	//printf("%s\n",B);
}
postfix_expression(A) ::= FLOAT_LITERAL(B).
{
	A = make_node(NODE_UNARY_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.unary_expression.type = UNARY_FLOAT_CONSTANT;
	a->u.unary_expression.u.float_constant = strtod(B, NULL);
	//printf("B %s  val %f\n",B, a->u.unary_expression.u.float_constant);
}
postfix_expression(A) ::= STRING_LITERAL(B).
{
	A = make_node(NODE_UNARY_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.unary_expression.type = UNARY_LITERAL;
	removeChar(B, '"');
	a->u.unary_expression.u.string = B;
	//printf("%s\n",B);
}
postfix_expression(A) ::= CHARACTER_LITERAL(B).
{
	A = B;
}
postfix_expression(A) ::= LPAREN selection_expression(B) RPAREN.
{
	A = B;
}

postfix_expression(A) ::= postfix_expression(B) LSBRAC selection_expression(C) RSBRAC.
{
	A = B;
	GNode *nd, *left_most;
	nd = make_node(NODE_UNARY_EXPRESSION);
	ast a = (ast)(nd->data);
	a->u.unary_expression.type = UNARY_SBRAC;
	left_most = A;
	while(left_most) {
	  if(G_NODE_IS_LEAF(left_most))
		break;
	  else
		left_most = g_node_first_child(left_most);
	}  
	g_node_append(left_most, nd);
	g_node_append(left_most, C);
}
postfix_expression(A) ::= postfix_expression(B) DOT IDENTIFIER(C).
{
	A = B;
	GNode *nd, *left_most;
	nd = make_node(NODE_UNARY_EXPRESSION);
	ast a = (ast)(nd->data);
	a->u.unary_expression.type = UNARY_STRING;
	a->u.unary_expression.u.string = C;
	left_most = A;
	while(left_most) {
	  if(G_NODE_IS_LEAF(left_most))
		break;
	  else
		left_most = g_node_first_child(left_most);
	}  
	g_node_append(left_most, nd);	
}

/* postfix_expression ::= postfix_expression DOT ID_TYPE. */

unary_expression(A) ::= postfix_expression(B).
{
	A = B;
}

unary_expression(A) ::= PLUS postfix_expression(B).
{
	ast b = (ast)(B->data);
	if(b->type == NODE_MATH_EXPRESSION){
		A = B;
	} else if(b->type == NODE_UNARY_EXPRESSION) {
		if(b->u.unary_expression.type == UNARY_STRING){
			A = make_node(NODE_MATH_EXPRESSION);
			ast a = (ast)(A->data);
			a->u.math_expression.type = MATH_PLUS;
			g_node_append(A, B);
		} else if(b->u.unary_expression.type == UNARY_UNSIGNED_CONSTANT
			|| b->u.unary_expression.type != UNARY_SIGNED_CONSTANT){
			A = B;
		} else {
			fprintf(stderr, "expecting numeric constant\n");
		}
	} else {
		fprintf(stderr, "expecting numeric constant or math expression\n");
	}
}
unary_expression(A) ::= MINUS postfix_expression(B).
{
	ast b = (ast)(B->data);
	if(b->type == NODE_MATH_EXPRESSION){
		A = make_node(NODE_MATH_EXPRESSION);
		ast a = (ast)(A->data);
		a->u.math_expression.type = MATH_MINUS;
		g_node_append(A, B);
	} else if(b->type == NODE_UNARY_EXPRESSION) {	
		if(b->u.unary_expression.type == UNARY_STRING){
			A = make_node(NODE_MATH_EXPRESSION);
			ast a = (ast)(A->data);
			a->u.math_expression.type = MATH_MINUS;
			g_node_append(A, B);
		} else if (b->u.unary_expression.type == UNARY_UNSIGNED_CONSTANT) {
			A = B;
			b->u.unary_expression.type = UNARY_SIGNED_CONSTANT;
			b->u.unary_expression.u.signed_constant =
				-(b->u.unary_expression.u.unsigned_constant);
		} else if (b->u.unary_expression.type == UNARY_SIGNED_CONSTANT) {
			A = B;
			b->u.unary_expression.u.signed_constant =
				-(b->u.unary_expression.u.signed_constant);
		} else {
			fprintf(stderr, "expecting numeric constant or math expression\n");
		}			
	} else {
		fprintf(stderr, "expecting numeric constant\n");
	}
}
unary_expression(A) ::= NOT postfix_expression(B).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_NOT;
	g_node_append(A, B);
}

unary_expression(A) ::= LOGARITHM LPAREN selection_expression(B) RPAREN.
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_LOGARITHM;
	g_node_append(A, B);
}

unary_expression(A) ::= LENVAR LPAREN selection_expression(B) RPAREN.
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_LENVAR;
	g_node_append(A, B);
}

unary_expression(A) ::= POW LPAREN selection_expression(B) COMMA selection_expression(C) RPAREN.
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_POW;
	g_node_append(A, B);
	g_node_append(A, C);
}


// MULTIPLICATIVE EXPRESSION
multiplicative_expression(A) ::= unary_expression(B).
{
	A = B;
}
multiplicative_expression(A) ::= multiplicative_expression(B) MULTIPLY unary_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_MULTIPLY;	
	g_node_append(A, B);
	g_node_append(A, C);
}
multiplicative_expression(A) ::= multiplicative_expression(B) DIV unary_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_DIV;	
	g_node_append(A, B);
	g_node_append(A, C);	
}
multiplicative_expression(A) ::= multiplicative_expression(B) MOD unary_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_MOD;	
	g_node_append(A, B);
	g_node_append(A, C);		
}

// ADDITIVE EXPRESSION
additive_expression(A) ::= multiplicative_expression(B).
{
	A = B;
}
additive_expression(A) ::= additive_expression(B) PLUS multiplicative_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_ADD;	
	g_node_append(A, B);
	g_node_append(A, C);	
}
additive_expression(A) ::= additive_expression(B) MINUS multiplicative_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_SUB;	
	g_node_append(A, B);
	g_node_append(A, C);	
}

// SHIFT EXPRESSION
shift_expression(A) ::= additive_expression(B).
{
	A = B;
}
shift_expression(A) ::= shift_expression(B) SL additive_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_SL;	
	g_node_append(A, B);
	g_node_append(A, C);	
}
shift_expression(A) ::= shift_expression(B) SR additive_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_SR;	
	g_node_append(A, B);
	g_node_append(A, C);	
}

// RELATIONAL EXPRESSION
relational_expression(A) ::= shift_expression(B).
{
	A = B;
}
relational_expression(A) ::= relational_expression(B) LT shift_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_LT;	
	g_node_append(A, B);
	g_node_append(A, C);	
}
relational_expression(A) ::= relational_expression(B) GT shift_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_GT;	
	g_node_append(A, B);
	g_node_append(A, C);	
}
relational_expression(A) ::= relational_expression(B) LTEQ shift_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_LTEQ;	
	g_node_append(A, B);
	g_node_append(A, C);	
}
relational_expression(A) ::= relational_expression(B) GTEQ shift_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_GTEQ;	
	g_node_append(A, B);
	g_node_append(A, C);	
}

// EQUALITY EXPRESSION
equality_expression(A) ::= relational_expression(B).
{
	A = B;
}
equality_expression(A) ::= equality_expression(B) EQEQ relational_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_EQEQ;	
	g_node_append(A, B);
	g_node_append(A, C);	
}
equality_expression(A) ::= equality_expression(B) NOTEQ relational_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_NOTEQ;	
	g_node_append(A, B);
	g_node_append(A, C);	
}

// INCLUSIVE AND EXPRESSION
and_expression(A) ::= equality_expression(B).
{
	A = B;
}
and_expression(A) ::= and_expression(B) AND equality_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_AND;	
	g_node_append(A, B);
	g_node_append(A, C);	
}

// INCLUSIVE OR EXPRESSION
inclusive_or_expression(A) ::= and_expression(B).
{
	A = B;
}
inclusive_or_expression(A) ::= inclusive_or_expression(B) OR and_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_OR;	
	g_node_append(A, B);
	g_node_append(A, C);	
}

// LOGICAL AND EXPRESSION
logical_and_expression(A) ::= inclusive_or_expression(B).
{
	A = B;
}
logical_and_expression(A) ::= logical_and_expression(B) ANDAND inclusive_or_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_ANDAND;	
	g_node_append(A, B);
	g_node_append(A, C);	
}

// LOGICAL OR EXPRESSION
logical_or_expression(A) ::= logical_and_expression(B).
{
	A = B;
}
logical_or_expression(A) ::= logical_or_expression(B) OROR logical_and_expression(C).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_OROR;	
	g_node_append(A, B);
	g_node_append(A, C);	
}

// CONDITIONAL EXPRESSION
conditional_expression(A) ::=	logical_or_expression(B).
{
	A = B;
}
conditional_expression(A) ::= logical_or_expression(B) QUESTION conditional_expression(C) COLON conditional_expression(D).
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_CONDITIONAL;	
	g_node_append(A, B);
	g_node_append(A, C);	
	g_node_append(A, D);	
}

labeled_statement(A) ::= CASE INT_LITERAL(B) COLON selection_expression(C).
{
	A = make_node(NODE_STATEMENT);
	ast a = (ast)(A->data);
	a->u.label_statement.type = NORMAL;	
	uint64_t u;
	if(strncmp(B, "0x", strlen("0x")) == 0 || strncmp(B, "0X", strlen("0X")) == 0 ){
		u = strtoull(B, NULL, 16);
	} else if(strncmp(B, "0", strlen("0")) == 0) {
		u = strtoull(B, NULL, 8);
	} else {
		u = strtoull(B, NULL, 10);
	}	
	a->u.label_statement.constant = u;
	g_node_append(A, C);
}
labeled_statement(A) ::= DEFAULT COLON selection_expression(B).
{
	A = make_node(NODE_STATEMENT);
	ast a = (ast)(A->data);
	a->u.label_statement.type = _DEFAULT;	
	g_node_append(A, B);
}

statement_list(A) ::= labeled_statement(B) SEMICOLON.
{
	GSList *list = NULL;
	A = g_slist_append(list, B);
}
statement_list(A) ::= statement_list(B) labeled_statement(C) SEMICOLON.
{
	A = g_slist_append(B, C);
}

selection_expression(A) ::= conditional_expression(B).
{
	A = B;
}
selection_expression(A) ::= SWITCH LPAREN conditional_expression(B) RPAREN LBRAC statement_list(C) RBRAC.
{
	A = make_node(NODE_MATH_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.math_expression.type = MATH_SELECTION;	
	g_node_append(A, B);
	for (GSList *l = C; l != NULL; l = l->next)
    {
	  g_node_append(A, l->data);
    }
  	g_slist_free(C);			
}


type_specifier(A) ::= INTEGER.
{
	A = make_node(NODE_TYPE_SPECIFIER);
	ast a = (ast)(A->data);
	a->u.type_specifier.type = TYPESPEC_INTEGER;
}
type_specifier(A) ::= FLOAT.
{
	A = make_node(NODE_TYPE_SPECIFIER);
	ast a = (ast)(A->data);
	a->u.type_specifier.type = TYPESPEC_FLOATING_POINT;
}
type_specifier(A) ::= ID_TYPE(B).
{
	//printf("type_specifier %s\n",B);
	A = make_node(NODE_TYPE_SPECIFIER);
	ast a = (ast)(A->data);
	a->u.type_specifier.type = TYPESPEC_ID_TYPE;
	a->u.type_specifier.id_type = B;
}
type_specifier(A) ::= FLOATING_POINT LBRAC assignment_expression_list(B) RBRAC.
{
	A = make_node(NODE_TYPE_SPECIFIER);
	ast a = (ast)(A->data);
	a->u.type_specifier.type = TYPESPEC_FLOATING_POINT;
	for (GSList *l = B; l != NULL; l = l->next)
    {
	  g_node_append(A, l->data);
    }
	g_slist_free(B);
}
type_specifier(A) ::= INTEGER LBRAC assignment_expression_list(B) RBRAC.
{
	A = make_node(NODE_TYPE_SPECIFIER);
	ast a = (ast)(A->data);
	a->u.type_specifier.type = TYPESPEC_INTEGER;
	for (GSList *l = B; l != NULL; l = l->next)
    {
	  g_node_append(A, l->data);
    }
	g_slist_free(B);
}
type_specifier(A) ::= STRUCT struct_declaration_begin struct_or_variant_declaration_list(B) struct_declaration_end.
{
	A = make_node(NODE_TYPE_SPECIFIER);
	ast a = (ast)(A->data);
	a->u.type_specifier.type = TYPESPEC_STRUCT;
	for (GSList *l = B; l != NULL; l = l->next)
    {
	  g_node_append(A, l->data);
    }
	g_slist_free(B);
}
type_specifier(A) ::= VARIANT LT IDENTIFIER(B) GT variant_declaration_begin struct_or_variant_declaration_list(C) variant_declaration_end.
{
	A = make_node(NODE_TYPE_SPECIFIER);
	ast a = (ast)(A->data);
	a->u.type_specifier.type = TYPESPEC_VARIANT;
	a->u.type_specifier.id_type = B;
	for (GSList *l = C; l != NULL; l = l->next)
    {
	  g_node_append(A, l->data);
    }
  	g_slist_free(C);
}
type_specifier(A) ::= ENUM COLON ID_TYPE(C) LBRAC enumerator_list(B) RBRAC.
{
	A = make_node(NODE_TYPE_SPECIFIER);
	ast a = (ast)(A->data);
	a->u.type_specifier.type = TYPESPEC_ENUM;
	a->u.type_specifier.id_type = C;
	for (GSList *l = B; l != NULL; l = l->next)
    {
	  g_node_append(A, l->data);
    }
  	g_slist_free(B);
}

enumerator_list(A) ::= enumerator(B).
{
	GSList *list = NULL;
	A = g_slist_append(list, B);
}
enumerator_list(A) ::= enumerator_list(B) COMMA enumerator(C).
{
	A = g_slist_append(B, C);
}
enumerator(A) ::= IDENTIFIER(B) EQUAL unary_expression(C).
{
	A = make_node(NODE_ENUMERATOR);
	ast a = (ast)(A->data);
	a->u.enumerator.id = B;
	g_node_append(A, C);
	//printf("%s\n", B);
}
struct_or_variant_declaration_list(A) ::= struct_or_variant_declaration(B) SEMICOLON.
{
	GSList *list = NULL;
	A = g_slist_append(list, B);
}
struct_or_variant_declaration_list(A) ::= struct_or_variant_declaration_list(B) struct_or_variant_declaration(C) SEMICOLON.
{
	A = g_slist_append(B, C);
}
struct_or_variant_declaration(A) ::= SKIP INT_LITERAL(B).
{
	A = make_node(NODE_SKIP);
	ast a = (ast)(A->data);
	uint64_t u;
	if(strncmp(B, "0x", strlen("0x")) == 0 || strncmp(B, "0X", strlen("0X")) == 0 ){
		u = strtoull(B, NULL, 16);
	} else if(strncmp(B, "0", strlen("0")) == 0) {
		u = strtoull(B, NULL, 8);
	} else {
		u = strtoull(B, NULL, 10);
	}
	a->u.skip_expression.length = u;
}

struct_or_variant_declaration(A) ::= type_specifier(B) postfix_expression(C).
{
	A = make_node(NODE_STRUCT_OR_VARIANT_DECLARATION);
	g_node_append(A, B);
	g_node_append(A, C);
	ast a = (ast)(A->data);
	a->u.struct_or_variant_declaration.show_only_if = false;
	a->u.struct_or_variant_declaration.formula = false;
}
struct_or_variant_declaration(A) ::= type_specifier(B) postfix_expression(C) QUESTIONQUESTION selection_expression(D).
{
	A = make_node(NODE_STRUCT_OR_VARIANT_DECLARATION);
	g_node_append(A, B);
	g_node_append(A, C);
	g_node_append(A, D);
	ast a = (ast)(A->data);
	a->u.struct_or_variant_declaration.show_only_if = true;
	a->u.struct_or_variant_declaration.formula = false;
}
struct_or_variant_declaration(A) ::= struct_or_variant_declaration(B) EQUAL selection_expression(C).
{
	A = B;
	g_node_append(A, C);
	ast a = (ast)(A->data);
	a->u.struct_or_variant_declaration.formula = true;
	//a->u.struct_or_variant_declaration.show_only_if = false;
}

struct_declaration_begin ::= LBRAC.
{	
	push_scope(scanner);	
}
struct_declaration_end ::= RBRAC.
{	
	pop_scope(scanner);	
}

variant_declaration_begin ::= LBRAC.
{	
	push_scope(scanner);	
}
variant_declaration_end ::= RBRAC.
{	
	pop_scope(scanner);	
}
assignment_expression_list(A) ::= assignment_expression(B) SEMICOLON.
{
	GSList *list = NULL;
	A = g_slist_append(list, B);
}
assignment_expression_list(A) ::= assignment_expression_list(B) assignment_expression(C) SEMICOLON.
{
	A = g_slist_append(B, C);
}

assignment_expression(A) ::= unary_expression(B) EQUAL unary_expression(C).
{
	A = make_node(NODE_CTF_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.ctf_expression.type = EXP_ASSIGN;
	g_node_append(A, B);
	g_node_append(A, C);
}

assignment_expression(A) ::= unary_expression(B) TYPEASSIGN type_specifier(C).
{
	/*only struct*/
	A = make_node(NODE_CTF_EXPRESSION);
	ast a = (ast)(A->data);
	a->u.ctf_expression.type = EXP_TYPE_ASSIGN;
	g_node_append(A, B);
	g_node_append(A, C);
}

declaration(A) ::= TYPEALIAS type_specifier(B) TYPEASSIGN IDENTIFIER(C).
{
	A = make_node(NODE_TYPEALIAS);
	ast a = (ast)(A->data);
	a->u.field_class_alias.alias = C;
	g_node_append(A, B);
	add_type(scanner, C);
	//printf("%s\n",C);
}

declaration(A) ::= EVENT event_declaration_begin assignment_expression_list(B) event_declaration_end.
{
	A = make_node(NODE_EVENT);
	for (GSList *l = B; l != NULL; l = l->next)
    {
	  g_node_append(A, l->data);
    }
  	g_slist_free(B);
}

declaration_list(A) ::= declaration(B) SEMICOLON.{
	A = make_node(NODE_ROOT);
	g_node_append(A, B);
}
declaration_list(A) ::= declaration_list(B) declaration(C) SEMICOLON.{
	A = B;
	g_node_append(A, C);
}

event_declaration_begin ::= LBRAC.{ push_scope(scanner); }

event_declaration_end ::= RBRAC.{	pop_scope(scanner);	}

program ::= declaration_list(B) END_TOKEN. { 
	scanner->root = B;
}