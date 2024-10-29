%name rule
%token_type { char* }
%default_type { GNode* }
%extra_argument { struct PathScanner *ps }
%type compare_operator { enum vp_compare_type }
%type filter_expression_list { GSList * }
%type reduce_list { GSList * }
%type rule_expression_list { GSList * }
%type expression_list { GSList * }
%type collection_expression { GSList * }


%include {
#include <assert.h>
#include "rule_ast.h"
#include <glib.h>

typedef struct vp_ast* vpast;

GNode *make_vp_node(enum vp_node_type type)
{
	vpast node;
	node = malloc(sizeof(struct vp_ast));
	if(!node) {
		printf("error construction.\n");
		return NULL;
	} else {
		node->type = type;
		//printf("%s\n", vpnode_type_to_str[type]); 
		return g_node_new((gpointer)node); 
	}
}

void removeChar_vp(char *str, char garbage) {

    char *src, *dst;
    for (src = dst = str; *src != '\0'; src++) {
        *dst = *src;
        if (*dst != garbage) dst++;
    }
    *dst = '\0';
}
}

%parse_accept { printf("The parser has completed successfully.\n"); }

%syntax_error { fprintf(stderr, "Error\n"); }

%parse_failure { fprintf(stderr, "Parse failure\n"); }

%start_symbol program

// UNARY OPERATOR
unary_operator ::= P_ADD.
unary_operator ::= P_SUB.
unary_operator ::= P_NOT.

// COMPARATION OPERATOR
compare_operator(A) ::= P_CONTAINS.
{
  A = FILTER_CONTANINS;
}
compare_operator(A) ::= P_IN.
{
  A = FILTER_IN;
}
compare_operator(A) ::= P_LT.
{
  A = FILTER_LT;
}
compare_operator(A) ::= P_GT.
{
  A = FILTER_GT;
}
compare_operator(A) ::= P_LTEQ.
{
  A = FILTER_LTEQ;
}
compare_operator(A) ::= P_GTEQ.
{
  A = FILTER_GTEQ;
}
compare_operator(A) ::= P_EQEQ.
{
  A = FILTER_EQ;
}
compare_operator(A) ::= P_NOTEQ.
{
  A = FILTER_NOTEQ;
}

// POSTFIX EXPRESSION
postfix_expression(A) ::= P_NAME(B).
{
  A = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.unary_expression.type = VP_UNARY_STRING;
	a->u.unary_expression.u.string = B;
}
postfix_expression(A) ::= P_TILDE.
{
  A = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.unary_expression.type = VP_UNARY_PARENT;
}
postfix_expression(A) ::= P_STAR.
{
  A = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.unary_expression.type = VP_UNARY_WILDCARD; 
}
postfix_expression(A) ::= P_STARSTAR.
{
  A = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.unary_expression.type = VP_UNARY_DECENDANT;   
}
postfix_expression(A) ::= P_DOLLAR.
{
  A = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.unary_expression.type = VP_UNARY_CURRENT;
}
postfix_expression(A) ::= P_DOLLARDOLLAR.
{
  A = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.unary_expression.type = VP_UNARY_ROOT;
}
postfix_expression(A) ::= P_INT_LITERAL(B).
{
	A = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.unary_expression.type = VP_UNARY_UNSIGNED_CONSTANT;
	uint64_t u;
	if(strncmp(B, "0x", strlen("0x")) == 0 || strncmp(B, "0X", strlen("0X")) == 0 ){
		u = strtoull(B, NULL, 16);
	} else if(strncmp(B, "0", strlen("0")) == 0) {
		u = strtoull(B, NULL, 8);
	} else {
		u = strtoull(B, NULL, 10);
	}
	a->u.unary_expression.u.unsigned_constant = u;
}
postfix_expression(A) ::= P_FLOAT_LITERAL(B).
{
  A = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.unary_expression.type = VP_UNARY_FLOAT_CONSTANT;
	a->u.unary_expression.u.float_constant = strtod(B, NULL);
}
postfix_expression(A) ::= P_STRING_LITERAL(B).
{	
	A = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.unary_expression.type = VP_UNARY_LITERAL;
	removeChar_vp(B, '"');
	a->u.unary_expression.u.string = B;
}
postfix_expression(A) ::= P_STRING_LITERAL_SIGNLE(B).
{
	A = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.unary_expression.type = VP_UNARY_LITERAL_SINGLE;
	removeChar_vp(B, '\'');
	a->u.unary_expression.u.string = B;
}
postfix_expression(A) ::= P_LPAREN conditional_expression(B) P_RPAREN.
{
	A = B;
}

postfix_expression(A) ::= P_SUM P_LPAREN unary_expression(B) P_RPAREN.
{
	A = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.unary_expression.type = VP_UNARY_SUM;
	g_node_append(A, B);
}

postfix_expression(A) ::= P_AVERAGE P_LPAREN unary_expression(B) P_RPAREN.
{
	A = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.unary_expression.type = VP_UNARY_AVERAGE;
	g_node_append(A, B);
}

postfix_expression(A) ::= P_MIN P_LPAREN unary_expression(B) P_RPAREN.
{
	A = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.unary_expression.type = VP_UNARY_MIN;
	g_node_append(A, B);
}

postfix_expression(A) ::= P_MAX P_LPAREN unary_expression(B) P_RPAREN.
{
	A = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.unary_expression.type = VP_UNARY_MAX;
	g_node_append(A, B);
}

postfix_expression(A) ::= postfix_expression(B) P_LBRACKET conditional_expression(C) P_RBRACKET.
{
	A = B;
	GNode *nd, *left_most;
	nd = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(nd->data);
	a->u.unary_expression.type = VP_UNARY_SBRAC;
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
postfix_expression(A) ::= postfix_expression(B) P_DOT P_LBRACKET reduce_list(C) P_RBRACKET.
{
	A = B;
	GNode *nd, *left_most;
	nd = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(nd->data);
	a->u.unary_expression.type = VP_UNARY_ARRAY;
	left_most = A;
	while(left_most) {
	  if(G_NODE_IS_LEAF(left_most))
		  break;
	  else
		  left_most = g_node_first_child(left_most);
	}  
	g_node_append(left_most, nd);
	for (GSList *l = C; l != NULL; l = l->next)
    {
	  g_node_append(left_most, l->data);
    }
  	g_slist_free(C);
}
postfix_expression(A) ::= postfix_expression(B) P_DOT P_LBRACE reduce_list(C) P_RBRACE.
{
	/*children nodes are reduce expression except the first one*/
	A = B;
	GNode *nd, *left_most;
	nd = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(nd->data);
	a->u.unary_expression.type = VP_UNARY_BRACE;
	left_most = A;
	while(left_most) {
	  if(G_NODE_IS_LEAF(left_most))
		  break;
	  else
		  left_most = g_node_first_child(left_most);
	}  
	g_node_append(left_most, nd);
	for (GSList *l = C; l != NULL; l = l->next)
    {
	  g_node_append(left_most, l->data);
    }
  	g_slist_free(C);
}

postfix_expression(A) ::= postfix_expression(B) P_DOT P_NAME(C).
{
	A = B;
	GNode *nd, *left_most;
	nd = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(nd->data);
	a->u.unary_expression.type = VP_UNARY_STRING;
	
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

postfix_expression(A) ::= postfix_expression(B) P_DOT P_STRING_LITERAL_SIGNLE(C).
{
	A = B;
	GNode *nd, *left_most;
	nd = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(nd->data);
	a->u.unary_expression.type = VP_UNARY_LITERAL_SINGLE;
	removeChar_vp(C, '\'');
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

postfix_expression(A) ::= postfix_expression(B) P_DOT P_LPAREN conditional_expression(C) P_RPAREN.
{
	A = B;
	GNode *nd, *left_most;
	nd = make_vp_node(VP_UNARY_EXPRESSION);
	vpast a = (vpast)(nd->data);
	a->u.unary_expression.type = VP_UNARY_PARENTHESES;
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

// UNARY EXPRESSION
unary_expression(A) ::= postfix_expression(B).
{
	A = B;
}
unary_expression ::= unary_operator unary_expression.
unary_expression(A) ::= P_LBRACKET collection_expression(B) P_RBRACKET.
{
	A = make_vp_node(VP_UNARY_EXPRESSION);	
	vpast a = (vpast)(A->data); 
    a->u.unary_expression.type = VP_UNARY_COLLECTION;	
	for (GSList *l = B; l != NULL; l = l->next)
    {
	  g_node_append(A, l->data);
    }
  	g_slist_free(B);		
}

//COLLECTION EXPRESSION
collection_expression(A) ::= unary_expression(B).
{
    GSList *list = NULL;
    A = g_slist_append(list, B);		
}
collection_expression(A) ::= collection_expression(B) P_COMMA unary_expression(C).
{
	A = g_slist_append(B, C);
}

// CONCACT EXPRESSION
concat_expression(A) ::= unary_expression(B).
{
	A = B;
}
concat_expression(A) ::= concat_expression(B) P_AND unary_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_CONCAT;	
	g_node_append(A, B);
	g_node_append(A, C);	
}

// MULTIPLICATIVE EXPRESSION
multiplicative_expression(A) ::= concat_expression(B).
{
	A = B;
}
multiplicative_expression(A) ::= multiplicative_expression(B) P_STAR concat_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_MULTIPLY;	
	g_node_append(A, B);
	g_node_append(A, C);	  
}
multiplicative_expression(A) ::= multiplicative_expression(B) P_DIV concat_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_DIV;	
	g_node_append(A, B);
	g_node_append(A, C);	   
}
multiplicative_expression(A) ::= multiplicative_expression(B) P_MOD concat_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_MOD;	
	g_node_append(A, B);
	g_node_append(A, C);	  
}

// ADDITIVE EXPRESSION
additive_expression(A) ::= multiplicative_expression(B).
{
	A = B;
}
additive_expression(A) ::= additive_expression(B) P_ADD multiplicative_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_ADD;	
	g_node_append(A, B);
	g_node_append(A, C);	  
}
additive_expression(A) ::= additive_expression(B) P_SUB multiplicative_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_SUB;	
	g_node_append(A, B);
	g_node_append(A, C);	  
}

// SHIFT EXPRESSION
shift_expression(A) ::= additive_expression(B).
{
	A = B;
}
shift_expression(A) ::= shift_expression(B) P_SL additive_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_SL;	
	g_node_append(A, B);
	g_node_append(A, C);	  
}
shift_expression(A) ::= shift_expression(B) P_SR additive_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_SR;	
	g_node_append(A, B);
	g_node_append(A, C);  
}

// RELATIONAL EXPRESSION
relational_expression(A) ::= shift_expression(B).
{
	A = B;
}
relational_expression(A) ::= relational_expression(B) P_LT shift_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_LT;	
	g_node_append(A, B);
	g_node_append(A, C);    
}
relational_expression(A) ::= relational_expression(B) P_GT shift_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_GT;	
	g_node_append(A, B);
	g_node_append(A, C);      
}
relational_expression(A) ::= relational_expression(B) P_LTEQ shift_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_LTEQ;	
	g_node_append(A, B);
	g_node_append(A, C);   
}
relational_expression(A) ::= relational_expression(B) P_GTEQ shift_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_GTEQ;	
	g_node_append(A, B);
	g_node_append(A, C);     
}
relational_expression(A) ::= relational_expression(B) P_IN shift_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_IN;	
	g_node_append(A, B);
	g_node_append(A, C);  
}
relational_expression(A) ::= relational_expression(B) P_CONTAINS shift_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_CONTANINS;	
	g_node_append(A, B);
	g_node_append(A, C);    
}

// EQUALITY EXPRESSION
equality_expression(A) ::= relational_expression(B).
{
	A = B;
}
equality_expression(A) ::= equality_expression(B) P_EQEQ relational_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_EQEQ;	
	g_node_append(A, B);
	g_node_append(A, C);      
}
equality_expression(A) ::= equality_expression(B) P_NOTEQ relational_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_NOTEQ;	
	g_node_append(A, B);
	g_node_append(A, C);   
}
// LOGICAL AND EXPRESSION
logical_and_expression(A) ::= equality_expression(B).
{
	A = B;
}
logical_and_expression(A) ::= logical_and_expression(B) P_ANDAND equality_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_ANDAND;	
	g_node_append(A, B);
	g_node_append(A, C);   
}

// LOGICAL OR EXPRESSION
logical_or_expression(A) ::= logical_and_expression(B).
{
	A = B;
}
logical_or_expression(A) ::= logical_or_expression(B) P_OROR logical_and_expression(C).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_OROR;	
	g_node_append(A, B);
	g_node_append(A, C);     
}

// CONDITIONAL EXPRESSION
conditional_expression(A) ::= logical_or_expression(B).
{
	A = B;
}
conditional_expression(A) ::= logical_or_expression(B) P_QUESTION conditional_expression(C) P_COLON conditional_expression(D).
{
	A = make_vp_node(VP_MATH_EXPRESSION);
	vpast a = (vpast)(A->data);
	a->u.math_expression.type = VP_MATH_CONDITIONAL;	
	g_node_append(A, B);
	g_node_append(A, C);
	g_node_append(A, D);     
}

// FILTER EXPRESSION
filter_expression(A) ::= unary_expression(B) compare_operator(C) unary_expression(D).
{
	A = make_vp_node(VP_FILTER);
	vpast a = (vpast)(A->data); 
    a->u.filter.type = C;	
	g_node_append(A, B);
	g_node_append(A, D);   
}

//FILTER EXPRESSION LIST
filter_expression_list(A) ::= filter_expression(B).
{
    GSList *list = NULL;
    A = g_slist_append(list, B);
}
filter_expression_list(A) ::= filter_expression_list(B) P_COMMA filter_expression(C).
{
    A = g_slist_append(B, C);
}

// REDUCE EXPRESSION
reduce_expression(A) ::= concat_expression(B) P_COLON conditional_expression(C).
{
	A = make_vp_node(VP_REDUCE);
	g_node_append(A, B);
	g_node_append(A, C);   
}

// REDUCE LIST
reduce_list(A) ::= reduce_expression(B).
{
    GSList *list = NULL;
    A = g_slist_append(list, B);	
}
reduce_list(A) ::= reduce_list(B) P_COMMA reduce_expression(C).
{
    A = g_slist_append(B, C);	
}

// RULE EXPRESSION
rule_expression(A) ::= P_LPAREN P_RPAREN P_INDICATE concat_expression(B).
{
	A = make_vp_node(VP_RULE);
	g_node_append(A, B);	
}
rule_expression(A) ::= P_NAME(B) P_LPAREN filter_expression_list(C) P_RPAREN P_INDICATE concat_expression(D).
{
	A = make_vp_node(VP_RULE);	
	vpast a = (vpast)(A->data); 
    a->u.rule.name = B;	
	for (GSList *l = C; l != NULL; l = l->next)
    {
	  g_node_append(A, l->data);
    }
  	g_slist_free(C);
	/*last node is the result expression*/
	g_node_append(A, D);	
}

// RULE EXPRESSION LIST
rule_expression_list(A) ::= rule_expression(B) P_SEMICOLON.
{
	GSList *list = NULL;
    A = g_slist_append(list, B);
}
rule_expression_list(A) ::= rule_expression_list(B) rule_expression(C) P_SEMICOLON.
{
    A = g_slist_append(B, C);		
}

// EXPRESSION
expression(A) ::= P_NAME(B) P_LPAREN P_INT_LITERAL(C) P_RPAREN P_BINDING P_LBRACE rule_expression_list(D) P_RBRACE.
{
	A = make_vp_node(VP_EVENT_EXPRESSION);	
	vpast a = (vpast)(A->data); 
    a->u.event.name = B;	
	uint64_t u;
	if(strncmp(C, "0x", strlen("0x")) == 0 || strncmp(C, "0X", strlen("0X")) == 0 ){
		u = strtoull(C, NULL, 16);
	} else if(strncmp(C, "0", strlen("0")) == 0) {
		u = strtoull(C, NULL, 8);
	} else {
		u = strtoull(C, NULL, 10);
	}
	a->u.event.code = u;
	for (GSList *l = D; l != NULL; l = l->next)
    {
	  g_node_append(A, l->data);
    }
  	g_slist_free(D);		
}

// EXPRESSION LIST
expression_list(A) ::= expression(B) P_SEMICOLON.
{
	GSList *list = NULL;
    A = g_slist_append(list, B);	
}
expression_list(A) ::= expression_list(B) expression(C) P_SEMICOLON.
{
    A = g_slist_append(B, C);		
}

// PROGRAM
program ::= expression_list(B) END_TOKEN.
{
	ps->root = make_vp_node(VP_ROOT);	
	for (GSList *l = B; l != NULL; l = l->next)
    {
	  g_node_append(ps->root, l->data);
    }
  	g_slist_free(B);				
}