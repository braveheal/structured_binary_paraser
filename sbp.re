#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <glib.h>
#include "sbp_grammar.h"
#include "ast.h"
#include "meta.h"
#include "scanner.h"
#include "bitfield.h"
#include "rete.h"
#include <json-glib/json-glib.h>

#define   YYCTYPE     char
#define   YYCURSOR    s->cur
#define   YYMARKER    s->ptr

/* functions to interface the lemon parser */
void *ParseAlloc();
void Parse();
void ParseFree();
void ParseTrace();

GHashTable *decl;
GHashTable *event_decl;
GHashTable *rules;
enum byte_order bo = BYTE_ORDER_LITTLE;

int scan(Scanner* s, char *buff_end) {
  
regular:
  if (s->cur >= buff_end) {
    return END_TOKEN;
  }
  s->top = s->cur;

/*!re2c
  re2c:yyfill:enable = 0;
 
  whitespace = [ \t\v\f]+;
  any = [\000-\377];
  
  INTEGER_SUFFIX = [uUlL]*;
  DIGIT = [0-9];
  NONDIGIT = [a-zA-Z_];
  HEXDIGIT = [0-9A-Fa-f];
  OCTALDIGIT = [0-7];
  UCHARLOWERCASE = "u" HEXDIGIT{4};
  UCHARUPPERCASE = "U" HEXDIGIT{8};
  ID_NONDIGIT = NONDIGIT|UCHARLOWERCASE|UCHARUPPERCASE;
  IDENTIFIER = (ID_NONDIGIT (ID_NONDIGIT|DIGIT)*) | "^" | "@";
*/

/*!re2c
  "/*"            { goto comment; }
  "="             { return EQUAL; }
  "["             { return LSBRAC; }
  "]"             { return RSBRAC; }
  "("             { return LPAREN; }
  ")"             { return RPAREN; }
  "{"             { return LBRAC; }
  "}"             { return RBRAC; }
  "<"             { return LT; }
  ">"             { return GT; }
  "+"			  { return PLUS; }
  "-"			  { return MINUS; }
  ":="			  { return TYPEASSIGN; }
  ";"			  { return SEMICOLON; }
  "."			  { return DOT; }
  ","			  { return COMMA; }
  ":"			  { return COLON; }
  "!"			  { return NOT; }
  "*"			  { return MULTIPLY; }
  "/"			  { return DIV; }
  "%"			  { return MOD; }
  "<<"			  { return SL; }
  ">>"			  { return SR; }
  "=="            { return EQEQ; }
  "!="            { return NOTEQ; }
  "<"             { return LT; }
  ">"             { return GT; }
  "<="            { return LTEQ; }
  ">="            { return GTEQ; }  
  "&"			  { return AND; }
  "|"			  { return OR; }
  "&&"			  { return ANDAND; }
  "||"			  { return OROR; }  
  "?"			  { return QUESTION; }  
  "??"			  { return QUESTIONQUESTION; }  
  "floating_point" { return FLOATING_POINT; }
  "integer"       { return INTEGER; }
  "event"		  { return EVENT; }
  "typealias"     { return TYPEALIAS; }
  "struct"		  { return STRUCT; }
  "enum"		  { return ENUM; }
  "lenvar"	  	  { return LENVAR; }
  "variant"       { return VARIANT; }
  "case"		  { return CASE; }
  "switch"		  { return SWITCH; }
  "default"		  { return DEFAULT; }
  "log10" 		  { return LOGARITHM; }  
  "pow"			  { return POW; }
  "skip"		  { return SKIP; }
  IDENTIFIER      { if(is_type(s)) return ID_TYPE; else return IDENTIFIER; }
  ["]([^"]+)["]   { return STRING_LITERAL; }
  [']([^']+)[']   { return CHARACTER_LITERAL; }
  whitespace      { goto regular; }


  ("0" [xX] HEXDIGIT+ INTEGER_SUFFIX?) | ("0" OCTALDIGIT* INTEGER_SUFFIX?) | ([1-9] DIGIT* INTEGER_SUFFIX?)    { return INT_LITERAL; }
  (DIGIT* "." DIGIT+)	{return FLOAT_LITERAL;}  
  "\r\n"|"\n"
  {
    s->pos = s->cur;
    s->line++;
    goto regular;
  }

  any
  {
    printf("unexpected character: %c\n", *s->cur);
    goto regular;
  }
*/

comment:
/*!re2c
  "*/"          { goto regular; }
  "\n"          { s->line++; goto comment; }
  any           { goto comment; }
*/
}

static inline
void read_signed_bitfield(const uint8_t *buf, size_t at,
		unsigned int field_size, enum byte_order bo, int64_t *v)
{
	switch (bo) {
	case BYTE_ORDER_BIG:
		bt_bitfield_read_be(buf, uint8_t, at, field_size, v);
		break;
	case BYTE_ORDER_LITTLE:
		bt_bitfield_read_le(buf, uint8_t, at, field_size, v);
		break;
	default:
		break;
	}
}

static inline
void read_unsigned_bitfield(const uint8_t *buf, size_t at,
		unsigned int field_size, enum byte_order bo,
		uint64_t *v)
{
	switch (bo) {
	case BYTE_ORDER_BIG:
		bt_bitfield_read_be(buf, uint8_t, at, field_size, v);
		break;
	case BYTE_ORDER_LITTLE:
		bt_bitfield_read_le(buf, uint8_t, at, field_size, v);
		break;
	default:
		break;
	}
}

static gboolean
node_build_int (GNode    *node,
                   gpointer  data)
{
  GNode *left_nd, *right_nd;
  struct ctf_ast *left, *right;
  struct field_class_int *c = (struct field_class_int *)data;
  struct ctf_ast *ast = (struct ctf_ast*)(node->data);
  if(ast->type == NODE_CTF_EXPRESSION && ast->u.ctf_expression.type == EXP_ASSIGN) {
    /*get left and right then assign to ctx*/
	left_nd = g_node_first_child(node);
	left = (struct ctf_ast*)(left_nd->data);
	right_nd = g_node_last_child(node);
	right = (struct ctf_ast*)(right_nd->data);
	if(!left_nd || !left || !right_nd || !right){
	  goto error;
	}
	if(left->type == NODE_UNARY_EXPRESSION && left->u.unary_expression.type == UNARY_STRING) {
	  if(strcmp(left->u.unary_expression.u.string,"size") == 0) {
	    /*get the right value and assign it to ctx*/
		if(right->type == NODE_UNARY_EXPRESSION && right->u.unary_expression.type == UNARY_UNSIGNED_CONSTANT) {
		  c->base.size = right->u.unary_expression.u.unsigned_constant;
		} else {
		  goto error;
		}
	  } else if(strcmp(left->u.unary_expression.u.string, "signed") == 0) {
	  	if(right->type == NODE_UNARY_EXPRESSION && right->u.unary_expression.type == UNARY_STRING) {
		  char *str = right->u.unary_expression.u.string;
		  gboolean ret;
		  if (strcmp(str, "true") == 0 || strcmp(str, "TRUE") == 0) {
			ret = TRUE;
		  } else if (strcmp(str, "false") == 0 || strcmp(str, "FALSE") == 0) {
			ret = FALSE;
		  } else {
			goto error;
		  }
		  c->is_signed = ret;
		} else {
		  goto error;
		}
	  } else {
	    goto error;
	  }
    } else {
	  goto error;
    }		  
  } else {
    goto error;
  }
  return TRUE; 
  
  error:
    printf("error for the build int\n");
	return FALSE;
}

static gboolean
node_build_float (GNode    *node,
                   gpointer  data)
{
  GNode *left_nd, *right_nd;
  struct ctf_ast *left, *right;
  
  struct field_class_float *c = (struct field_class_float *)data;
  struct ctf_ast *ast = (struct ctf_ast*)(node->data);
  if(ast->type == NODE_CTF_EXPRESSION && ast->u.ctf_expression.type == EXP_ASSIGN) {
    /*get left and right then assign to ctx*/
	left_nd = g_node_first_child(node);
	left = (struct ctf_ast*)(left_nd->data);
	right_nd = g_node_last_child(node);
	right = (struct ctf_ast*)(right_nd->data);
	if(!left_nd || !left || !right_nd || !right)
	  goto error;
	if(left->type == NODE_UNARY_EXPRESSION && left->u.unary_expression.type == UNARY_STRING) {
	  if(strcmp(left->u.unary_expression.u.string,"size") == 0) {
	    /*get the right value and assign it to ctx*/
		if(right->type == NODE_UNARY_EXPRESSION && right->u.unary_expression.type == UNARY_UNSIGNED_CONSTANT) {
		  c->base.size = right->u.unary_expression.u.unsigned_constant;
		} else {
		  goto error;
		}
	  } else {
	    goto error;
	  }
    } else {
	  goto error;
    }		  
  } else {
    goto error;
  }
  return TRUE; 
  
  error:
    printf("error for the build float\n");
	return FALSE;
}

static gboolean
node_build_enum (GNode    *node,
                   gpointer  data)
{
  GNode *nd;
  struct ctf_ast *ast, *child_ast;
  
  struct field_class_enum *c = (struct field_class_enum *)data;
  ast = (struct ctf_ast*)(node->data);
  if(ast->type == NODE_ENUMERATOR) {
    /*get enumerator name, and get child node for the value*/
	/*first check the enum type if signed*/
	gboolean _signed = c->base.is_signed;
	nd = g_node_first_child(node);
	child_ast = (struct ctf_ast*)(nd->data);

	if(!nd || !child_ast)
	  goto error;
	if(child_ast->type == NODE_UNARY_EXPRESSION) {
	  if(child_ast->u.unary_expression.type == UNARY_UNSIGNED_CONSTANT && !_signed) {
	    struct field_class_enum_mapping *map = g_new0(struct field_class_enum_mapping, 1);
		map->label = g_string_new(ast->u.enumerator.id);
		map->value.u = child_ast->u.unary_expression.u.unsigned_constant;
		c->mappings = g_list_append(c->mappings, map);
	  } else if(child_ast->u.unary_expression.type == UNARY_STRING) {
	    if(strcmp(child_ast->u.unary_expression.u.string, "Default") == 0)
		{
			c->default_tag = g_string_new(ast->u.enumerator.id);
		} else {
			printf("not supported string.");
			goto error;
		}
	  } else {
		  goto error;
	  }
    } else {
	  goto error;
    }		  
  } else {
    goto error;
  }
  return TRUE; 
  
  error:
    printf("error for the build enum: %s, %s\n", node_type_to_str[ast->type],ast->u.enumerator.id);
	return FALSE;
}

static void
array_append_child (struct named_field_class *fc,
                   struct named_field_class *new_fc) {	   
  switch(fc->fc->type) {
    case FIELD_CLASS_TYPE_STRUCT:
	  ((struct field_class_struct *)(fc->fc))->members = g_list_append(((struct field_class_struct *)(fc->fc))->members, new_fc);
	  break;
	case FIELD_CLASS_TYPE_VARIANT:
	  ((struct field_class_variant *)(fc->fc))->options = g_list_append(((struct field_class_variant *)(fc->fc))->options, new_fc);	
	  break;
	default:
	  break;
  }
}

static gboolean
node_build_struct_or_variant (GNode    *node,
                   gpointer  data);

static struct named_field_class *
visit_specifier (GNode    *specifier)
{
  /*this will visit the specifier and generate a field class with the name*/
  struct named_field_class *new_fc = g_new0(struct named_field_class, 1);
  struct ctf_ast *ast = (struct ctf_ast*)(specifier->data);
  
  switch(ast->u.type_specifier.type) {
    case TYPESPEC_ID_TYPE:
	{
		/*find the type in hashtable and copy one*/
		struct field_class *fc = g_hash_table_lookup(decl,ast->u.type_specifier.id_type);
		if(fc) {	
		  new_fc->fc = fc;  
		  //new_fc->name = g_string_new(ast->u.type_specifier.id_type);
		} else {
		  goto error;
		}
	  break;
	}
	case TYPESPEC_ENUM:
	{
		/*visit the enum specifier, first copy the field from id_type, then assign the name from right, then iterate child to add enumator*/
		struct field_class *fc = g_hash_table_lookup(decl,ast->u.type_specifier.id_type);
		if(fc) {
		  struct field_class_enum *enum_fc = g_new0(struct field_class_enum, 1);
		  _field_class_copy(&((struct field_class_int *)fc)->base, &enum_fc->base.base);
		  enum_fc->type_name = g_string_new(ast->u.type_specifier.id_type);
		  enum_fc->base.base.type = FIELD_CLASS_TYPE_ENUM;
		  enum_fc->base.is_signed = ((struct field_class_int *)fc)->is_signed;	  	
		  new_fc->fc = enum_fc;
		  /*iterate child*/
		  g_node_children_foreach (specifier, G_TRAVERSE_ALL, (GNodeForeachFunc)node_build_enum, new_fc->fc);				  
		} else {
		  goto error;
		}
	  break;
	}
	/*
	case TYPESPEC_LENVAR:
	{
		//visit the enum specifier, first copy the field from id_type, then assign the name from right, then iterate child to add enumator
		struct field_class *fc = g_hash_table_lookup(decl,ast->u.type_specifier.id_type);
		if(fc) {
		  struct field_class_lenvar *len_fc = g_new0(struct field_class_lenvar, 1);
		  _field_class_copy(&((struct field_class_int *)fc)->base, &len_fc->base.base);
		  len_fc->base.base.type = FIELD_CLASS_TYPE_LENVAR;
		  len_fc->base.is_signed = ((struct field_class_int *)fc)->is_signed;	  	
		  new_fc->fc = len_fc;			  
		} else {
		  goto error;
		}
	  break;
	}
	*/
	case TYPESPEC_VARIANT:
	{
		/*init a variant field class and iterate children*/
		struct field_class_variant *variant_fc =  g_new0(struct field_class_variant, 1);
		variant_fc->base.type = FIELD_CLASS_TYPE_VARIANT;
		variant_fc->tag_ref = g_string_new(ast->u.type_specifier.id_type);
		new_fc->fc = variant_fc;
		/*iterate child*/
		g_node_children_foreach (specifier, G_TRAVERSE_ALL, (GNodeForeachFunc)node_build_struct_or_variant, new_fc);		
	  break;
	}
	case TYPESPEC_STRUCT:
	{
		struct field_class_struct *struct_fc = g_new0(struct field_class_struct, 1);
		struct_fc->base.type = FIELD_CLASS_TYPE_STRUCT;
		new_fc->fc = struct_fc;
		/*iterate child*/
		g_node_children_foreach (specifier, G_TRAVERSE_ALL, (GNodeForeachFunc)node_build_struct_or_variant, new_fc);
	  break;
	}
	default:
	  break;
  }
  return new_fc;
  error:
    printf("error for the build specifier\n");
	g_free(new_fc);
	return NULL;
}	

static struct named_field_class *
visit_declator (GNode    *specifier_nd, GNode    *declator_nd)
{
  struct ctf_ast *specifier, *declator; 
  specifier = (struct ctf_ast*)(specifier_nd->data);
  declator = (struct ctf_ast*)(declator_nd->data);  
  struct named_field_class *new_fc, *nested_fc;
  if(!specifier || !declator){
    goto error;
  }
  g_assert(specifier->type == NODE_TYPE_SPECIFIER);
  g_assert(declator->type == NODE_UNARY_EXPRESSION);
  /*visit the declator until the no nested*/
  if(G_NODE_IS_LEAF(declator_nd)) {
	/*a named field class*/
	new_fc = visit_specifier(specifier_nd);
  } else {
    /*visit this declator, and generate array or sequence*/
	GNode *nested_declator = g_node_first_child(declator_nd);	
	GNode *unary_exp = g_node_last_child(declator_nd);
	struct ctf_ast *nested_ast = (struct ctf_ast*)(nested_declator->data);
	g_assert(nested_ast->type == NODE_UNARY_EXPRESSION && nested_ast->u.unary_expression.type == UNARY_SBRAC);
	struct ctf_ast *unary_ast = (struct ctf_ast*)(unary_exp->data);
	//printf("the length of nested %i\n", unary_ast->u.unary_expression.u.unsigned_constant);	
	if(!nested_declator || !unary_exp || !nested_ast || !unary_ast){
      goto error;
    }

	g_assert(unary_ast->type == NODE_UNARY_EXPRESSION || unary_ast->type == NODE_MATH_EXPRESSION);
	new_fc = g_new0(struct named_field_class, 1);
	if(unary_ast->type == NODE_UNARY_EXPRESSION) {
		switch(unary_ast->u.unary_expression.type) {
		case UNARY_UNSIGNED_CONSTANT:
			{
			/*make an array field*/
			struct field_class_array *array_fc =  g_new0(struct field_class_array, 1);
			array_fc->base.type = FIELD_CLASS_TYPE_ARRAY;
			array_fc->arrType = ARRAY;			
			array_fc->u.length = unary_ast->u.unary_expression.u.unsigned_constant;
			array_fc->item = visit_declator(specifier_nd, nested_declator);
			array_fc->item->in_array = TRUE;
			new_fc->fc = array_fc;
			break;
			}
		case UNARY_STRING:
			{
			/*make and sequence field*/
			struct field_class_array *sequence_fc =  g_new0(struct field_class_array, 1);
			sequence_fc->base.type = FIELD_CLASS_TYPE_ARRAY;
			sequence_fc->arrType = SEQUENCE;			
			sequence_fc->u.length_ref = unary_exp; 
			sequence_fc->item = visit_declator(specifier_nd, nested_declator);
			sequence_fc->item->in_array = TRUE;
			new_fc->fc = sequence_fc;		
			break;
			}
		default:
			goto error;
			break;
		}
	} else if (unary_ast->type == NODE_MATH_EXPRESSION) {
		if(unary_ast->u.math_expression.type == MATH_LENVAR) {
			struct field_class_array *while_fc =  g_new0(struct field_class_array, 1);
			while_fc->base.type = FIELD_CLASS_TYPE_ARRAY;
			while_fc->arrType = LENCONDITION;			
			while_fc->u.lenvar = g_node_first_child(unary_exp); 
			while_fc->item = visit_declator(specifier_nd, nested_declator);
			while_fc->item->in_array = TRUE;
			new_fc->fc = while_fc;		
		} else {
	
			//struct field_class *fc = g_hash_table_lookup(decl,"Uint32");
			struct field_class_int *fc = g_new0(struct field_class_int, 1);
			fc->base.type = FIELD_CLASS_TYPE_INT;		
			fc->base.size = 32;
			fc->is_signed = FALSE;

			struct named_field_class *tmp_fc = g_new0(struct named_field_class, 1);
			tmp_fc->fc = fc;		

			struct field_class_formula *formula_fc =  g_new0(struct field_class_formula, 1);
			formula_fc->base.type = FIELD_CLASS_TYPE_FORMULA; 
			formula_fc->class = tmp_fc;
			formula_fc->node = unary_exp;

			struct field_class_array *array_fc =  g_new0(struct field_class_array, 1);
			array_fc->base.type = FIELD_CLASS_TYPE_ARRAY;
			array_fc->arrType = FORMULA;	
			array_fc->u.formula = formula_fc;
			array_fc->item = visit_declator(specifier_nd, nested_declator);
			array_fc->item->in_array = TRUE;

			new_fc->fc = array_fc;
		}

	} 
	else {
		goto error;		
	}
  } 
  return new_fc;	
  error:
    printf("error for the build struct\n");
	return FALSE;
}	


static gboolean
node_build_struct_or_variant (GNode    *node,
                   gpointer  data)
{
	struct ctf_ast *ast = (struct ctf_ast*)(node->data);
	struct named_field_class *cx = (struct named_field_class *)data;	
	if(ast->type == NODE_STRUCT_OR_VARIANT_DECLARATION) {
		GNode *left_nd, *right_nd; 
		GNode *showonlyif_nd = NULL;
		GNode *formula_nd = NULL;
		struct ctf_ast *left, *right, *declator;

		/*could be a struct or variant*/		
		left_nd = g_node_first_child(node);
		left = (struct ctf_ast*)(left_nd->data);
		right_nd = g_node_nth_child(node, 1);
		right = (struct ctf_ast*)(right_nd->data);
		if(!left_nd || !left || !right_nd || !right){
			goto error;
		}
		if(ast->u.struct_or_variant_declaration.show_only_if){
			showonlyif_nd = g_node_nth_child(node, 2);
		}
		if(ast->u.struct_or_variant_declaration.formula){
			if(g_node_n_children(node) > 3) {
			formula_nd = g_node_nth_child(node, 3);
			} else {
			formula_nd = g_node_nth_child(node, 2);
			}
		}

		declator = (struct ctf_ast*)(right_nd->data);
		g_assert(declator->u.unary_expression.type == UNARY_STRING);

		struct named_field_class *new_fc = visit_declator(left_nd, right_nd);
		new_fc->name = g_string_new(declator->u.unary_expression.u.string);

		struct field_class_formula *formula_fc = NULL;
		struct field_class_showonlyif *showonlyif_fc = NULL;

		if(formula_nd && !showonlyif_nd){
			struct field_class_formula *formula_fc =  g_new0(struct field_class_formula, 1);
			formula_fc->base.type = FIELD_CLASS_TYPE_FORMULA; 
			formula_fc->class = new_fc;
			formula_fc->node = formula_nd;
					
			struct named_field_class *tmp_fc = g_new0(struct named_field_class, 1);
			tmp_fc->fc = formula_fc;
			array_append_child(cx, tmp_fc);
		} else if(formula_nd && showonlyif_nd){
			struct field_class_formula *formula_fc =  g_new0(struct field_class_formula, 1);
			formula_fc->base.type = FIELD_CLASS_TYPE_FORMULA; 
			formula_fc->class = new_fc;
			formula_fc->node = formula_nd;

			struct named_field_class *tmp_fc1 = g_new0(struct named_field_class, 1);
			tmp_fc1->fc = formula_fc;

			struct field_class_showonlyif *showonlyif_fc =  g_new0(struct field_class_showonlyif, 1);
			showonlyif_fc->base.type = FIELD_CLASS_TYPE_SHOWONLYIF;	
			showonlyif_fc->class = tmp_fc1;
			showonlyif_fc->node = showonlyif_nd;

			struct named_field_class *tmp_fc = g_new0(struct named_field_class, 1);
			tmp_fc->fc = showonlyif_fc;	

			array_append_child(cx, tmp_fc);  
		} else if(!formula_nd && showonlyif_nd) {
			struct field_class_showonlyif *showonlyif_fc =  g_new0(struct field_class_showonlyif, 1);
			showonlyif_fc->base.type = FIELD_CLASS_TYPE_SHOWONLYIF;	
			showonlyif_fc->class = new_fc;	
			showonlyif_fc->node = showonlyif_nd;

			struct named_field_class *tmp_fc = g_new0(struct named_field_class, 1);
			tmp_fc->fc = showonlyif_fc;	
			array_append_child(cx, tmp_fc);  	  
		} else {
			array_append_child(cx, new_fc);
		}
		
		return TRUE;   
		error:
			printf("error for the build struct\n");
			return FALSE;
	} else if (ast->type == NODE_SKIP){
		//skip node
		struct field_class_skip *skip = NULL;
		skip = g_new0(struct field_class_skip, 1);
		skip->base.type = FIELD_CLASS_TYPE_SKIP; 
		skip->length = ast->u.skip_expression.length;

		struct named_field_class *tmp_fc = g_new0(struct named_field_class, 1);
		tmp_fc->fc = skip;	
		array_append_child(cx, tmp_fc);
		return TRUE;  
	} else {
		printf("error not a valid type\n");
		return FALSE;
	}
}

static gboolean
node_build_ctx (GNode    *node,
                   gpointer  data)
{
  GNode *child_node;
  struct ctf_ast *child;
  
  //struct ctx *c = (struct ctx *)data;
  struct ctf_ast *ast = (struct ctf_ast*)(node->data);
  switch(ast->type) {
	case NODE_TYPEALIAS:
	{
	  /*visit first child to know which ctf type to create*/
	  child_node = g_node_first_child(node);
	  child = (struct ctf_ast*)(child_node->data);
	  if(child) {
	    if(child->type == NODE_TYPE_SPECIFIER) {		  
		  if(child->u.type_specifier.type == TYPESPEC_INTEGER){
		    struct field_class_int *fc = g_new0(struct field_class_int, 1);
			fc->base.type = FIELD_CLASS_TYPE_INT;		
			g_hash_table_insert(decl, ast->u.field_class_alias.alias, fc);
			g_node_children_foreach (child_node, G_TRAVERSE_ALL, (GNodeForeachFunc)node_build_int, fc);
		  } else if(child->u.type_specifier.type == TYPESPEC_FLOATING_POINT) {
		  	struct field_class_float *fc = g_new0(struct field_class_float, 1);
			fc->base.type = FIELD_CLASS_TYPE_FLOAT;
			g_hash_table_insert(decl, ast->u.field_class_alias.alias, fc);
			g_node_children_foreach (child_node, G_TRAVERSE_ALL, (GNodeForeachFunc)node_build_float, fc);
		  } else {
		    goto error;
		  }
		} else {
		  goto error;
		}
	  } else {
		goto error;
	  }
	  break;
	}
	case NODE_EVENT:
	{
	  //ec = g_new0(struct named_field_class, 1);
	  GNode *left_nd, *right_nd;
      struct ctf_ast *left, *right;
	  uint32_t id;
	  struct named_field_class *ec = g_new0(struct named_field_class, 1);
	  /*visit event*/
	  for(guint i=0;i< g_node_n_children(node);i++) {
	    /*child must be ctf_expressions*/
		child_node = g_node_nth_child(node, i);
		if(child_node) { 
		  child = (struct ctf_ast*)(child_node->data);
		  if(child->type == NODE_CTF_EXPRESSION) {		    
		    left_nd = g_node_first_child(child_node);
		    left = (struct ctf_ast*)(left_nd->data);
		    right_nd = g_node_last_child(child_node);
		    right = (struct ctf_ast*)(right_nd->data);
		    if(!left_nd || !left || !right_nd || !right) {
			  goto error;
			}
		    if(child->u.ctf_expression.type == EXP_ASSIGN) {
			  /*visit left and right and assign the name to event*/
			  if(left->type == NODE_UNARY_EXPRESSION && left->u.unary_expression.type == UNARY_STRING) {
			    if(strcmp(left->u.unary_expression.u.string,"name") == 0) {
			  	  /*get the right value and assign it to ctx*/
			      if(right->type == NODE_UNARY_EXPRESSION && right->u.unary_expression.type == UNARY_LITERAL) {
			  	    ec->name = g_string_new(right->u.unary_expression.u.string);
			  	  } else {
			  	    goto error;
			  	  }
			    } else if(strcmp(left->u.unary_expression.u.string,"id") == 0) {
			  	  /*get the right value and append it to ctx hasp*/
			      if(right->type == NODE_UNARY_EXPRESSION && right->u.unary_expression.type == UNARY_UNSIGNED_CONSTANT) {
					id = right->u.unary_expression.u.unsigned_constant;
					//printf("id %i\n",id);
			  	  } else {
			  	    goto error;
			  	  }					
				}else {
			 	  goto error;
		  	    }
		  	  } else {
		  	    goto error;
		  	  }		  
			} else if(child->u.ctf_expression.type == EXP_TYPE_ASSIGN) {
			  if(left->type == NODE_UNARY_EXPRESSION && left->u.unary_expression.type == UNARY_STRING) {
			  	if(strcmp(left->u.unary_expression.u.string,"fields") == 0) {
			  	  /*get the right value and assign it to ctx*/
			      if(right->type == NODE_TYPE_SPECIFIER && right->u.type_specifier.type == TYPESPEC_STRUCT) {
			  	    struct field_class_struct *stru = g_new0(struct field_class_struct, 1);
					stru->base.type = FIELD_CLASS_TYPE_STRUCT;
					ec->fc = stru;
					/*iterate child node*/
					g_node_children_foreach (right_nd, G_TRAVERSE_ALL, (GNodeForeachFunc)node_build_struct_or_variant, ec);
			  	  } else {
			  	    goto error;
			  	  }
			    } else {
			 	  goto error;
		  	    }
		  	  } else {
		  	    goto error;
		  	  }	
			} else {
			  goto error;
			}
		  } else {
		    goto error;
		  }
		} else {
		  goto error;
		}
	  }
	  g_hash_table_insert(event_decl, GUINT_TO_POINTER(id), ec);
	  break;
	}	
	default:
	  break;
  }
  return FALSE;
  error:
    printf("error event\n");
    return TRUE;
}

static GNode * 
find_root_unary(GNode *node) {
	GNode *parent, *current;	
	current = node;	
	parent = current->parent;
	while(parent) {
		struct ctf_ast *a = (struct ctf_ast*)(parent->data);
		if(a->type != NODE_UNARY_EXPRESSION){
			break;
		} else {
			current = parent;
			parent = parent->parent;
		}
	}
	return current;
}

static gboolean
find_parsed_node (GNode    *node,
             gpointer  data)
{
  struct parsed_nd_cb *pcb = data;
  struct parsed_node *pn = node->data; 
  if(pn) {
	if(pn->type == COMPOUND){
	  struct compound_node *cn = pn->u.cpn;
	  if(cn && cn->label && strcmp(pcb->label->str, cn->label->str) == 0){
		  pcb->nd = node;
		  //printf("found the parsed compound unary node %s\n", pcb->label->str);
		  return TRUE;
	  }
    } else {
	  #ifdef DEBUG
	  GVariant *var = pn->u.sn->var;
	  #else
	  GVariant *var = pn->u.var;  
	  #endif
	  //printf("%s\n", g_variant_get_type_string(var));
	  if(var && strcmp(g_variant_get_type_string(var),"{sv}")==0) {
		  const gchar *str = NULL;
		  g_variant_get_child (var, 0, "&s", &str);
		  if(strcmp(str, pcb->label->str) == 0)
		  {
			pcb->nd = node;
			//printf("found the parsed node %s\n", str);
			//GVariant *v = g_variant_get_variant(g_variant_get_child_value(var, 1));
			//if(g_variant_is_of_type(v, G_VARIANT_TYPE_UINT32))
			//	printf("value: %i\n", g_variant_get_uint32(v));
				
			return TRUE;
		  }  
	  }
    }
  }
  return FALSE;
}

const GVariantType *
top_type(const GVariantType *a, const GVariantType *b){
	#ifdef TOP
	#undef TOP // avoid a conflict with AIX 6.1 system include files
	#endif
	#define TOP(x, y, z) do { if (g_variant_type_equal(a, x) && g_variant_type_equal(b, y)) { return (z); } } while (false)
		TOP(G_VARIANT_TYPE_INT32, G_VARIANT_TYPE_INT32, G_VARIANT_TYPE_INT64);
		TOP(G_VARIANT_TYPE_INT32, G_VARIANT_TYPE_INT64, G_VARIANT_TYPE_INT64);
		TOP(G_VARIANT_TYPE_INT32, G_VARIANT_TYPE_UINT32, G_VARIANT_TYPE_INT64);
		TOP(G_VARIANT_TYPE_INT32, G_VARIANT_TYPE_UINT64, G_VARIANT_TYPE_INT64);	
		TOP(G_VARIANT_TYPE_INT32, G_VARIANT_TYPE_DOUBLE, G_VARIANT_TYPE_DOUBLE);
		TOP(G_VARIANT_TYPE_INT32, G_VARIANT_TYPE_STRING, G_VARIANT_TYPE_STRING);						
		TOP(G_VARIANT_TYPE_INT64, G_VARIANT_TYPE_INT32, G_VARIANT_TYPE_INT64);
		TOP(G_VARIANT_TYPE_INT64, G_VARIANT_TYPE_INT64, G_VARIANT_TYPE_INT64);
		TOP(G_VARIANT_TYPE_INT64, G_VARIANT_TYPE_UINT32, G_VARIANT_TYPE_INT64);
		TOP(G_VARIANT_TYPE_INT64, G_VARIANT_TYPE_UINT64, G_VARIANT_TYPE_INT64);	
		TOP(G_VARIANT_TYPE_INT64, G_VARIANT_TYPE_DOUBLE, G_VARIANT_TYPE_DOUBLE);
		TOP(G_VARIANT_TYPE_INT64, G_VARIANT_TYPE_STRING, G_VARIANT_TYPE_STRING);			
		TOP(G_VARIANT_TYPE_UINT32, G_VARIANT_TYPE_INT32, G_VARIANT_TYPE_INT64);
		TOP(G_VARIANT_TYPE_UINT32, G_VARIANT_TYPE_INT64, G_VARIANT_TYPE_INT64);
		TOP(G_VARIANT_TYPE_UINT32, G_VARIANT_TYPE_UINT32, G_VARIANT_TYPE_INT64);
		TOP(G_VARIANT_TYPE_UINT32, G_VARIANT_TYPE_UINT64, G_VARIANT_TYPE_INT64);	
		TOP(G_VARIANT_TYPE_UINT32, G_VARIANT_TYPE_DOUBLE, G_VARIANT_TYPE_DOUBLE);	
		TOP(G_VARIANT_TYPE_UINT32, G_VARIANT_TYPE_STRING, G_VARIANT_TYPE_STRING);				
		TOP(G_VARIANT_TYPE_UINT64, G_VARIANT_TYPE_INT32, G_VARIANT_TYPE_INT64);
		TOP(G_VARIANT_TYPE_UINT64, G_VARIANT_TYPE_INT64, G_VARIANT_TYPE_INT64);
		TOP(G_VARIANT_TYPE_UINT64, G_VARIANT_TYPE_UINT32, G_VARIANT_TYPE_INT64);
		TOP(G_VARIANT_TYPE_UINT64, G_VARIANT_TYPE_UINT64, G_VARIANT_TYPE_INT64);		
		TOP(G_VARIANT_TYPE_UINT64, G_VARIANT_TYPE_DOUBLE, G_VARIANT_TYPE_DOUBLE);
		TOP(G_VARIANT_TYPE_UINT64, G_VARIANT_TYPE_STRING, G_VARIANT_TYPE_STRING);			
		TOP(G_VARIANT_TYPE_DOUBLE, G_VARIANT_TYPE_INT32, G_VARIANT_TYPE_DOUBLE);	
		TOP(G_VARIANT_TYPE_DOUBLE, G_VARIANT_TYPE_INT64, G_VARIANT_TYPE_DOUBLE);	
		TOP(G_VARIANT_TYPE_DOUBLE, G_VARIANT_TYPE_UINT32, G_VARIANT_TYPE_DOUBLE);	
		TOP(G_VARIANT_TYPE_DOUBLE, G_VARIANT_TYPE_UINT64, G_VARIANT_TYPE_DOUBLE);	
		TOP(G_VARIANT_TYPE_DOUBLE, G_VARIANT_TYPE_DOUBLE, G_VARIANT_TYPE_DOUBLE);
		TOP(G_VARIANT_TYPE_DOUBLE, G_VARIANT_TYPE_STRING, G_VARIANT_TYPE_STRING);
		TOP(G_VARIANT_TYPE_STRING, G_VARIANT_TYPE_INT32, G_VARIANT_TYPE_STRING);
		TOP(G_VARIANT_TYPE_STRING, G_VARIANT_TYPE_INT64, G_VARIANT_TYPE_STRING);
		TOP(G_VARIANT_TYPE_STRING, G_VARIANT_TYPE_UINT32, G_VARIANT_TYPE_STRING);
		TOP(G_VARIANT_TYPE_STRING, G_VARIANT_TYPE_UINT64, G_VARIANT_TYPE_STRING);
		TOP(G_VARIANT_TYPE_STRING, G_VARIANT_TYPE_DOUBLE, G_VARIANT_TYPE_STRING);
		TOP(G_VARIANT_TYPE_STRING, G_VARIANT_TYPE_STRING, G_VARIANT_TYPE_STRING);
		TOP(G_VARIANT_TYPE_BOOLEAN, G_VARIANT_TYPE_UINT32, G_VARIANT_TYPE_INT64);
		TOP(G_VARIANT_TYPE_UINT32, G_VARIANT_TYPE_BOOLEAN, G_VARIANT_TYPE_INT64);	
		TOP(G_VARIANT_TYPE_BOOLEAN, G_VARIANT_TYPE_BOOLEAN, G_VARIANT_TYPE_BOOLEAN);			
	#undef TOP

	return G_VARIANT_TYPE_ANY;
}

GVariant *
cast_to(GVariant *src, const GVariantType *to_type) {
	const GVariantType *a = g_variant_get_type(src);
	GVariant *b;
	if(g_variant_type_equal(a, G_VARIANT_TYPE_INT32)) {
		if(g_variant_type_equal(to_type, G_VARIANT_TYPE_INT64)) {
			b = g_variant_new_int64(g_variant_get_int32(src));
		} else if(g_variant_type_equal(to_type, G_VARIANT_TYPE_DOUBLE)) {
			b = g_variant_new_double((double)g_variant_get_int32(src));
		} else if(g_variant_type_equal(to_type, G_VARIANT_TYPE_BOOLEAN)){
			b = g_variant_get_int32(src) == 0? g_variant_new_boolean(FALSE) : g_variant_new_boolean(TRUE);
		} else if(g_variant_type_equal(to_type, G_VARIANT_TYPE_STRING)) {
			gchar *my_string = g_strdup_printf("%i", g_variant_get_int32(src));
			b = g_variant_new_string(my_string);
			g_free(my_string);
		} else{ 
			b = src;
		}
	} else if(g_variant_type_equal(a, G_VARIANT_TYPE_UINT32)) {
		if(g_variant_type_equal(to_type, G_VARIANT_TYPE_INT64)) {
			b = g_variant_new_int64(g_variant_get_uint32(src));
		} else if(g_variant_type_equal(to_type, G_VARIANT_TYPE_DOUBLE)) {
			b = g_variant_new_double((double)g_variant_get_uint32(src));
		} else if(g_variant_type_equal(to_type, G_VARIANT_TYPE_BOOLEAN)){
			b = g_variant_get_uint32(src) == 0? g_variant_new_boolean(FALSE) : g_variant_new_boolean(TRUE);		
		} else if(g_variant_type_equal(to_type, G_VARIANT_TYPE_STRING)) {
			gchar *my_string = g_strdup_printf("%i", g_variant_get_uint32(src));
			b = g_variant_new_string(my_string);
			g_free(my_string);				
		} else {
			b = src;
		}
	} else if(g_variant_type_equal(a, G_VARIANT_TYPE_INT64)) {
		if(g_variant_type_equal(to_type, G_VARIANT_TYPE_DOUBLE)) {
			b = g_variant_new_double((double)g_variant_get_int64(src));
		} else if(g_variant_type_equal(to_type, G_VARIANT_TYPE_BOOLEAN)){
			b = g_variant_get_int64(src) == 0? g_variant_new_boolean(FALSE) : g_variant_new_boolean(TRUE);	
		} else if(g_variant_type_equal(to_type, G_VARIANT_TYPE_STRING)) {
			gchar *my_string = g_strdup_printf("%i", g_variant_get_int64(src));
			b = g_variant_new_string(my_string);
			g_free(my_string);					
		} else {
			b = src;
		}		
	} else if(g_variant_type_equal(a, G_VARIANT_TYPE_UINT64)) {
		if(g_variant_type_equal(to_type, G_VARIANT_TYPE_DOUBLE)) {
			b = g_variant_new_double((double)g_variant_get_uint64(src));
		} else if(g_variant_type_equal(to_type, G_VARIANT_TYPE_INT64)) {
			b = g_variant_new_int64(g_variant_get_uint64(src));
		} else if(g_variant_type_equal(to_type, G_VARIANT_TYPE_BOOLEAN)){
			b = g_variant_get_uint64(src) == 0? g_variant_new_boolean(FALSE) : g_variant_new_boolean(TRUE);	
		} else if(g_variant_type_equal(to_type, G_VARIANT_TYPE_STRING)) {
			gchar *my_string = g_strdup_printf("%i", g_variant_get_uint64(src));
			b = g_variant_new_string(my_string);
			g_free(my_string);							
		} else {
			b = src;
		}			
	} else if(g_variant_type_equal(a, G_VARIANT_TYPE_DOUBLE)) {
		if(g_variant_type_equal(to_type, G_VARIANT_TYPE_BOOLEAN)){
			b = g_variant_get_int64(src) == 0? g_variant_new_boolean(FALSE) : g_variant_new_boolean(TRUE);	
		} else if(g_variant_type_equal(to_type, G_VARIANT_TYPE_STRING)) {
			gchar *my_string = g_strdup_printf("%f", g_variant_get_double(src));
			b = g_variant_new_string(my_string);
			g_free(my_string);							
		} else {		
			b = src;
		}
	} else if(g_variant_type_equal(a, G_VARIANT_TYPE_BOOLEAN)) {
		if(g_variant_type_equal(to_type, G_VARIANT_TYPE_INT64)) {
			b = g_variant_new_int64(g_variant_get_boolean(src) ? 1 : 0);
		} else if(g_variant_type_equal(to_type, G_VARIANT_TYPE_STRING)) {
			gchar *my_string = g_variant_get_boolean(src) ? "true" : "false";
			b = g_variant_new_string(my_string);
			g_free(my_string);			
		} else {
			b = src;
		}
	} else if(g_variant_type_equal(a, G_VARIANT_TYPE_STRING)) {
		if(g_variant_type_equal(to_type, G_VARIANT_TYPE_STRING)) {
			b = src;		
		} else {
			printf("cast not implemented\n");
		}		
	} else {
		const gchar *ts = g_variant_get_type_string(src);
		const gchar *ss = g_variant_type_peek_string(to_type);
		printf("cast not supported %s to %s\n", ts, ss);
	}

	return b;
}

static GVariant *compute_formula(GNode *node, GNode *parse_tree, GHashTable **cond_set);

static void
node_build_formula (GNode    *node,
                   GNode *scope, GHashTable *set, GHashTable **cond_set)
{
	struct ctf_ast *a = (struct ctf_ast*)(node->data);
	if(a->type == NODE_MATH_EXPRESSION) {
		/*add to hash*/
		if(node->children) {		
			GNode *child;		
			child = node->children;
			if(a->u.math_expression.type == MATH_SELECTION || a->u.math_expression.type == MATH_CONDITIONAL) {
				node_build_formula (child, scope, set, cond_set);	
			} else {
				while (child)
				{
					GNode *current;
					
					current = child;
					child = current->next;
					node_build_formula (current, scope, set, cond_set);				
				}
			}
			switch(a->u.math_expression.type) {
				case MATH_LENVAR:
				{
					GNode *left_nd = g_node_first_child(node);
					struct ctf_ast *b = (struct ctf_ast*)(left_nd->data);
					g_assert(b->type == NODE_UNARY_EXPRESSION);
					GNode *left_most = left_nd;
					while(left_most) {
						if(G_NODE_IS_LEAF(left_most))
							break;
						else
							left_most = g_node_first_child(left_most);
					}
					struct ctf_ast *c = (struct ctf_ast*)(left_most->data);
					struct len_struct* lenstru = g_hash_table_lookup(*cond_set, left_nd);
					g_assert (lenstru != NULL);
					g_hash_table_remove(set, left_nd);
					g_hash_table_insert(set, node, g_variant_new_uint32(lenstru->pos));
					//printf("current pos %i, length: %i\n",lenstru->pos, lenstru->length);				
					break;
				}
				case MATH_LOGARITHM:
				{
					GNode *left_nd = g_node_first_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);				
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					GVariant *a = cast_to(left, G_VARIANT_TYPE_DOUBLE);
					double d = g_variant_get_double(a);
					g_hash_table_remove(set, left_nd);
					g_hash_table_insert(set, node, g_variant_new_double(log10(d)));					
					break;
				}
				case MATH_NOT:
				{
					GNode *left_nd = g_node_first_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);				
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					GVariant *a = cast_to(left, G_VARIANT_TYPE_BOOLEAN);
					gboolean d = g_variant_get_boolean(a);
					g_hash_table_remove(set, left_nd);
					g_hash_table_insert(set, node, g_variant_new_boolean(!d));					
					break;
				}
				case MATH_POW:
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}					
					const GVariantType *top = G_VARIANT_TYPE_DOUBLE;
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c;
					c = g_variant_new_double(pow(g_variant_get_double(a) , g_variant_get_double(b)));

					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);
					break;
				}				
				case MATH_ADD:
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}					
					const GVariantType *top = top_type(g_variant_get_type(left), g_variant_get_type(right));
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c;
					if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
						c = g_variant_new_int64(g_variant_get_int64(a) + g_variant_get_int64(b));
					} else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
						c = g_variant_new_double(g_variant_get_double(a) + g_variant_get_double(b));
					} else {
						printf("top type not supported\n");
					}

					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);
					break;
				}
				case MATH_SUB:
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}						
					const GVariantType *top = top_type(g_variant_get_type(left), g_variant_get_type(right));
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c;
					if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
						c = g_variant_new_int64(g_variant_get_int64(a) - g_variant_get_int64(b));
					} else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
						c = g_variant_new_double(g_variant_get_double(a) - g_variant_get_double(b));
					} else {
						printf("top type not supported\n");
					}

					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);					
					break;
				}
				case MATH_MULTIPLY:				
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}						
					const GVariantType *top = G_VARIANT_TYPE_DOUBLE;
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c = g_variant_new_double(g_variant_get_double(a) * g_variant_get_double(b));
					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);					
					break;
				}
				case MATH_DIV:				
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}						
					const GVariantType *top = G_VARIANT_TYPE_DOUBLE;
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c = g_variant_new_double(g_variant_get_double(a) / g_variant_get_double(b));

					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);							
					break;
				}
				case MATH_MOD:				
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}						
					const GVariantType *top = G_VARIANT_TYPE_INT64;
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c = g_variant_new_int64(g_variant_get_int64(a) % g_variant_get_int64(b));

					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);							
					break;
				}
				case MATH_SL:
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}						
					const GVariantType *top = G_VARIANT_TYPE_INT64;
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c = g_variant_new_int64(g_variant_get_int64(a) << g_variant_get_int64(b));

					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);						
					break;
				}	
				case MATH_SR:
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}						
					const GVariantType *top = G_VARIANT_TYPE_INT64;
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c = g_variant_new_int64(g_variant_get_int64(a) >> g_variant_get_int64(b));

					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);						
					break;
				}
				case MATH_LT:
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}						
					const GVariantType *top = top_type(g_variant_get_type(left), g_variant_get_type(right));
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c;
					if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
						c = g_variant_new_boolean(g_variant_get_int64(a) < g_variant_get_int64(b));
					} else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
						c = g_variant_new_boolean(g_variant_get_double(a) < g_variant_get_double(b));
					} else {
						printf("top type not supported\n");
					}

					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);						
					break;
				}	
				case MATH_GT:
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}						
					const GVariantType *top = top_type(g_variant_get_type(left), g_variant_get_type(right));
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c;
					if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
						c = g_variant_new_boolean(g_variant_get_int64(a) > g_variant_get_int64(b));
					} else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
						c = g_variant_new_boolean(g_variant_get_double(a) > g_variant_get_double(b));
					} else {
						printf("top type not supported\n");
					}

					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);						
					break;
				}
				case MATH_LTEQ:
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}						
					const GVariantType *top = top_type(g_variant_get_type(left), g_variant_get_type(right));
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c;
					if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
						c = g_variant_new_boolean(g_variant_get_int64(a) <= g_variant_get_int64(b));
					} else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
						c = g_variant_new_boolean(g_variant_get_double(a) <= g_variant_get_double(b));
					} else {
						printf("top type not supported\n");
					}

					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);						
					break;
				}	
				case MATH_GTEQ:
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}						
					const GVariantType *top = top_type(g_variant_get_type(left), g_variant_get_type(right));
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c;
					if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
						c = g_variant_new_boolean(g_variant_get_int64(a) >= g_variant_get_int64(b));
					} else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
						c = g_variant_new_boolean(g_variant_get_double(a) >= g_variant_get_double(b));
					} else {
						printf("top type not supported\n");
					}

					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);						
					break;
				}
				case MATH_EQEQ:
				{	
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);									
					if(g_variant_is_of_type(left, G_VARIANT_TYPE_STRING) || g_variant_is_of_type(right, G_VARIANT_TYPE_STRING)) {
						if(g_variant_is_container(left)) {
							left = g_variant_get_child_value(left, 0);
						}
						if(g_variant_is_container(right)) {
							right = g_variant_get_child_value(right, 0);
						}							
						g_assert(strcmp(g_variant_get_type_string(left), g_variant_get_type_string(right)) == 0);
						GVariant *c; 

						const gchar *a = g_variant_get_string(left, NULL);
						const gchar *b = g_variant_get_string(right, NULL);
						c = g_variant_new_boolean(strcmp(a, b)==0?TRUE:FALSE);
						g_hash_table_remove(set, left_nd);
						g_hash_table_remove(set, right_nd);
						g_hash_table_insert(set, node, c);							
					} else {
						if(g_variant_is_container(left)) {
							left = g_variant_get_child_value(left, 1);
						}
						if(g_variant_is_container(right)) {
							right = g_variant_get_child_value(right, 1);
						}						
						const GVariantType *top = top_type(g_variant_get_type(left), g_variant_get_type(right));
						GVariant *a = cast_to(left, top);
						GVariant *b = cast_to(right, top);
						GVariant *c;
						if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
							c = g_variant_new_boolean(g_variant_get_int64(a) == g_variant_get_int64(b));
						} else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
							c = g_variant_new_boolean(g_variant_get_double(a) == g_variant_get_double(b));
						} else {
							printf("top type not supported\n");
						}

						g_hash_table_remove(set, left_nd);
						g_hash_table_remove(set, right_nd);
						g_hash_table_insert(set, node, c);	
					}					
					break;
				}
				case MATH_NOTEQ:
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_of_type(left, G_VARIANT_TYPE_STRING) || g_variant_is_of_type(right, G_VARIANT_TYPE_STRING)) {
						if(g_variant_is_container(left)) {
							left = g_variant_get_child_value(left, 0);
						}
						if(g_variant_is_container(right)) {
							right = g_variant_get_child_value(right, 0);
						}						
						g_assert(strcmp(g_variant_get_type_string(left), g_variant_get_type_string(right)) == 0);
						GVariant *c; 

						const gchar *a = g_variant_get_string(left, NULL);
						const gchar *b = g_variant_get_string(right, NULL);
						c = g_variant_new_boolean(strcmp(a, b)==0?FALSE:TRUE);
						g_hash_table_remove(set, left_nd);
						g_hash_table_remove(set, right_nd);
						g_hash_table_insert(set, node, c);							
					} else {
						if(g_variant_is_container(left)) {
							left = g_variant_get_child_value(left, 1);
						}
						if(g_variant_is_container(right)) {
							right = g_variant_get_child_value(right, 1);
						}						
						const GVariantType *top = top_type(g_variant_get_type(left), g_variant_get_type(right));
						GVariant *a = cast_to(left, top);
						GVariant *b = cast_to(right, top);
						GVariant *c;
						if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
							c = g_variant_new_boolean(g_variant_get_int64(a) != g_variant_get_int64(b));
						} else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
							c = g_variant_new_boolean(g_variant_get_double(a) != g_variant_get_double(b));
						} else {
							printf("top type not supported\n");
						}

						g_hash_table_remove(set, left_nd);
						g_hash_table_remove(set, right_nd);
						g_hash_table_insert(set, node, c);	
					}					
					break;
				}
				case MATH_AND:
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}					
					const GVariantType *top = G_VARIANT_TYPE_INT64;
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c = g_variant_new_int64(g_variant_get_int64(a) & g_variant_get_int64(b));

					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);						
					break;
				}	
				case MATH_OR:
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}					
					const GVariantType *top = G_VARIANT_TYPE_INT64;
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c = g_variant_new_int64(g_variant_get_int64(a) | g_variant_get_int64(b));

					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);						
					break;
				}
				case MATH_ANDAND:
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}					
					const GVariantType *top = G_VARIANT_TYPE_BOOLEAN;
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c = g_variant_new_boolean(g_variant_get_boolean(a) && g_variant_get_boolean(b));

					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);						
					break;
				}	
				case MATH_OROR:
				{
					GNode *left_nd = g_node_first_child(node);
					GNode *right_nd = g_node_last_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);
					GVariant* right = g_hash_table_lookup(set, right_nd);					
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					if(g_variant_is_container(right)) {
						right = g_variant_get_child_value(right, 1);
					}					
					const GVariantType *top = G_VARIANT_TYPE_BOOLEAN;
					GVariant *a = cast_to(left, top);
					GVariant *b = cast_to(right, top);
					GVariant *c = g_variant_new_boolean(g_variant_get_boolean(a) || g_variant_get_boolean(b));

					g_hash_table_remove(set, left_nd);
					g_hash_table_remove(set, right_nd);
					g_hash_table_insert(set, node, c);						
					break;
				}
				case MATH_CONDITIONAL:
				{
					GNode *left_nd = g_node_first_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);

					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					GVariant *selval = cast_to(left, G_VARIANT_TYPE_BOOLEAN);
					gboolean sval = g_variant_get_boolean(selval);	
					g_hash_table_remove(set, left_nd);
					GNode *selNd = NULL;
					GVariant *retNd = NULL;
					if(sval) {
						selNd = g_node_nth_child(node, 1);
					} else {
						selNd = g_node_last_child(node);
					}
					retNd = compute_formula(selNd, scope, cond_set);	
					g_hash_table_insert(set, node, retNd);				
					break;
				}	
				case MATH_SELECTION:
				{
					GNode *left_nd = g_node_first_child(node);
					GVariant* left = g_hash_table_lookup(set, left_nd);	
			
					if(g_variant_is_container(left)) {
						left = g_variant_get_child_value(left, 1);
					}
					GVariant *selval = cast_to(left, G_VARIANT_TYPE_INT64);
					gint64 sval = g_variant_get_int64(selval);	
					g_hash_table_remove(set, left_nd);

					GVariant *retNd = NULL;
					GVariant *defaultNd = NULL;
					for(int j=1;j<g_node_n_children(node);j++) {
						GNode *ch = g_node_nth_child(node, j);
						struct ctf_ast *cht = (struct ctf_ast *)(ch->data);
						if(cht->type == NODE_STATEMENT){ 
							if(cht->u.label_statement.type == NORMAL && cht->u.label_statement.constant == sval) {
								retNd = compute_formula(g_node_first_child(ch), scope, cond_set);															
								break;
							} else {
								if(cht->u.label_statement.type == _DEFAULT && !defaultNd) {
									defaultNd = compute_formula(g_node_first_child(ch), scope, cond_set);	
								} else {
									printf("Error: Multiple default node\n");
								}
							}
						} else {
							printf("Not a valid node type\n");
						}
					}
					if(retNd) {
						g_hash_table_insert(set, node, retNd);
					} else if(defaultNd) {
						g_hash_table_insert(set, node, defaultNd);
					} else {
						printf("error selection statement\n");
					}
					
					break;
				}																																																	
				default:
					break;
			}
		} else {
			printf("error of math node, must have a child\n");
		}
	} else if(a->type == NODE_UNARY_EXPRESSION) {
		/*find the root unary expression, and update hash*/
		switch(a->u.unary_expression.type){
			case UNARY_LITERAL:
			{
				/*add to hash*/
				g_hash_table_insert(set, node, g_variant_new_string(a->u.unary_expression.u.string));
				break;
			}			
			case UNARY_SIGNED_CONSTANT:
			{
				/*add to hash*/
				g_hash_table_insert(set, node, g_variant_new_int64(a->u.unary_expression.u.signed_constant));
				break;
			}
			case UNARY_FLOAT_CONSTANT:
			{
				g_hash_table_insert(set, node, g_variant_new_double(a->u.unary_expression.u.float_constant));
				break;				
			}
			case UNARY_UNSIGNED_CONSTANT:
			{
				/*add to hash*/
				g_hash_table_insert(set, node, g_variant_new_uint64(a->u.unary_expression.u.unsigned_constant));
				break;
			}
			case UNARY_SBRAC:
			{
				/*get the next node value, must be an integer, then update hash*/
				GNode *next = node->next;

				GVariant *access =  compute_formula(next, scope, cond_set);
				GVariant *sizeval = cast_to(access, G_VARIANT_TYPE_INT64);
				gint64 size = g_variant_get_int64(sizeval);

				GNode *root_unary = find_root_unary(node);
				GNode *parsed_nd = g_hash_table_lookup(set, root_unary);

				//printf("array size: %i\n", g_node_n_children(parsed_nd));
				GNode *arritem = g_node_nth_child(parsed_nd, size);
				g_hash_table_remove(set, root_unary);
				g_hash_table_insert(set, root_unary, arritem);		
				//printf("type: %s\n", g_variant_get_type_string(arritem->data));														

				GNode *child;		
				child = node->children;
				if (child)
				{
					node_build_formula (child, scope, set, cond_set);				
				} else {
					struct parsed_node *pn = arritem->data; 
					g_assert(pn->type == SIMPLE);
					#ifdef DEBUG
					GVariant *var = pn->u.sn->var;
					#else
					GVariant *var = pn->u.var;  
					#endif				 
					if(strcmp(g_variant_get_type_string(var),"{sv}")==0) {
						var = g_variant_get_variant(g_variant_get_child_value (var, 1));
					}					
					g_hash_table_remove(set, root_unary);
					g_hash_table_insert(set, root_unary, var);
				}		
				break;
			}
			case UNARY_STRING:
			{
				/*get the value and update to hash*/
				GNode *root_unary = find_root_unary(node);
				GNode *parsed_nd = g_hash_table_lookup(set, root_unary);				
				struct parsed_nd_cb *pcb = g_new0(struct parsed_nd_cb, 1);
				pcb->label = g_string_new(a->u.unary_expression.u.string);	
				//printf("%s\n",a->u.unary_expression.u.string);
				if(!parsed_nd) {
					if(strcmp(a->u.unary_expression.u.string, "^") == 0) {
						pcb->nd = scope->parent;
						g_hash_table_insert(set, root_unary, pcb->nd);						
						//printf("%s", cpn->label->str);					
					} else if(strcmp(a->u.unary_expression.u.string, "@") == 0) {
						
						GNode *nd_ptr, *chd_ptr;
						nd_ptr = chd_ptr = scope;
						//struct compound_node *scp_nd = nd_ptr->data;
						struct parsed_node *scp_nd = nd_ptr->data;
						while(scp_nd->type != COMPOUND || !scp_nd->u.cpn->isArray) {
							chd_ptr = nd_ptr;
							nd_ptr = nd_ptr->parent;
							scp_nd = nd_ptr->data;
						}
						int ind = g_node_child_position(nd_ptr, chd_ptr);
						//printf("the array index: %i\n", ind);
						g_hash_table_insert(set, root_unary, g_variant_new_int64(ind));
						break;
					} else {
						//find the Gnode and insert to hash
						g_node_traverse (scope, G_POST_ORDER, G_TRAVERSE_ALL, 2, find_parsed_node, pcb);
						g_hash_table_insert(set, root_unary, pcb->nd);
					}
				} else {
					if(strcmp(a->u.unary_expression.u.string, "^") == 0) {
						GNode *fnd = g_hash_table_lookup(set, root_unary);	
						pcb->nd = fnd->parent;	
						g_hash_table_remove(set, root_unary);
						g_hash_table_insert(set, root_unary, pcb->nd);
					} else if(strcmp(a->u.unary_expression.u.string, "@") == 0) {
						
						GNode *nd_ptr, *chd_ptr;
						nd_ptr = chd_ptr = scope;
						struct parsed_node *scp_nd = nd_ptr->data;
						while(scp_nd->type != COMPOUND || !scp_nd->u.cpn->isArray) {
							chd_ptr = nd_ptr;
							nd_ptr = nd_ptr->parent;
							scp_nd = nd_ptr->data;
						}
						int ind = g_node_child_position(nd_ptr, chd_ptr);
						//printf("the array index: %i\n", ind);
						GNode *arritem = g_node_nth_child(parsed_nd, ind);
						g_hash_table_remove(set, root_unary);
						g_hash_table_insert(set, root_unary, g_variant_new_int64(ind));
						break;
					} else {																	
						g_node_traverse (parsed_nd, G_POST_ORDER, G_TRAVERSE_ALL, 2, find_parsed_node, pcb);
						GNode *ptr = parsed_nd;
						while(!pcb->nd) {
							ptr = ptr->parent;
							g_node_traverse (ptr, G_POST_ORDER, G_TRAVERSE_ALL, 2, find_parsed_node, pcb);
						}
						g_hash_table_remove(set, root_unary);
						g_hash_table_insert(set, root_unary, pcb->nd);
					}
				}
				
				GNode *child;		
				child = node->children;
				if (child)
				{
					node_build_formula (child, scope, set, cond_set);				
				} else {
					struct parsed_node *pn = pcb->nd->data;
					g_assert(pn->type == SIMPLE);
					#ifdef DEBUG
					GVariant *var = pn->u.sn->var;
					#else
					GVariant *var = pn->u.var;  
					#endif					
					//GVariant *var = pcb->nd->data;  
					if(strcmp(g_variant_get_type_string(var),"{sv}")==0) {
						g_hash_table_remove(set, root_unary);
						g_hash_table_insert(set, root_unary, g_variant_get_variant(g_variant_get_child_value (var, 1)));
					}
				}
				break;
			}
			default:
				break;
		}		
	} else {
		printf("error of node type\n");
	}
}

static gboolean
find_tagref (GNode    *node,
             gpointer  data)
{
  struct TagRefData *d = data;
  struct parsed_node *pn = node->data;
  if(pn && pn->type == SIMPLE){
	  #ifdef DEBUG
	  GVariant *var = pn->u.sn->var;
	  #else
	  GVariant *var = pn->u.var;  
	  #endif
    if(strcmp(g_variant_get_type_string(var),"{sv}")==0) {
	  const gchar *str = NULL;
	  g_variant_get_child (var, 0, "&s", &str);
	  if(strcmp(str, d->tag->str) == 0)
	  {
		GVariant *a = g_variant_get_variant(g_variant_get_child_value (var, 1));	
		const gchar *ret_val = g_variant_get_string(g_variant_get_child_value (a, 0), NULL);	
		d->value = g_string_new(ret_val);
					
		//printf("found variant tag %s\n",d->value->str);
		return TRUE;
	  }  
    }
  }
  return FALSE;
}

static GVariant*
variant_traverse(GNode* node) {
  GVariant *var = NULL;
  struct parsed_node *pn = node->data;  
  if(node->children) {
	GNode *child;
	GVariantBuilder builder;
	gboolean is_empty = TRUE;	
	g_variant_builder_init (&builder, G_VARIANT_TYPE_ARRAY);
    child = node->children;
    while (child)
	{	
	  GNode *current;  	  
	  current = child;
	  child = current->next;
	  var = variant_traverse(current);
	  if(var) {
		  is_empty = FALSE;
		  g_variant_builder_add_value(&builder, var);
	  }
	}
	if(is_empty) {
		return NULL;
	} else {
		var = g_variant_builder_end (&builder);
		if(pn && pn->u.cpn && pn->u.cpn->label) {			
			return g_variant_new("{sv}", pn->u.cpn->label->str, var);
		} else if(!pn) {
			return g_variant_new("{sv}", "event", var);
		}
		return var;
	}
  } else {
	  if(pn && pn->type == SIMPLE) {
		#ifdef DEBUG
			//printf("variant type %s \n", g_variant_get_type_string(pn->u.sn->var));
			if(strcmp(g_variant_get_type_string(pn->u.sn->var),"{sv}")==0) {
				GVariantBuilder newb;	
				g_variant_builder_init (&newb, G_VARIANT_TYPE_ARRAY);				
				const gchar *str = NULL;
				g_variant_get_child (pn->u.sn->var, 0, "&s", &str);
				g_variant_builder_add (&newb, "{sv}", "value", g_variant_get_child_value(pn->u.sn->var, 1));	
				g_variant_builder_add (&newb, "{sv}", "pos", g_variant_new_uint32(pn->u.sn->pos));
				g_variant_builder_add (&newb, "{sv}", "size", g_variant_new_uint32(pn->u.sn->size));

				return g_variant_new("{sv}", str, g_variant_builder_end(&newb));
			} else {
				GVariantBuilder newb;	
				g_variant_builder_init (&newb, G_VARIANT_TYPE_ARRAY);				
				g_variant_builder_add (&newb, "{sv}", "value", pn->u.sn->var);	
				g_variant_builder_add (&newb, "{sv}", "pos", g_variant_new_uint32(pn->u.sn->pos));
				g_variant_builder_add (&newb, "{sv}", "size", g_variant_new_uint32(pn->u.sn->size));

				return g_variant_builder_end(&newb);
			}
		#else
			return pn->u.var;
		#endif		
	  } 
	  return NULL;
  }
}

static GVariant *compute_formula(GNode *node, GNode *parse_tree, GHashTable **cond_set) {
	GHashTable *set;
	set = g_hash_table_new(g_direct_hash, g_direct_equal);
	node_build_formula(node, parse_tree, set, cond_set);
	g_assert(g_hash_table_size(set) == 1);
	GList* l = g_hash_table_get_values(set);
	GVariant *v = (GVariant *)l->data;
	//printf("variant type %s \n", g_variant_get_type_string(v));
	g_hash_table_unref(set);
	return v;
}

static void length_cb(GHashTable **hash, uint32_t size) {
	GHashTableIter iter;
    gpointer k, v;
	g_hash_table_iter_init (&iter, *hash);
	while (g_hash_table_iter_next (&iter, &k, &v))
	{
		struct len_struct* lenstru = (struct len_struct*)v;
		// && lenstru->started
		if(lenstru)
			lenstru->pos += size;
	}
}

static GNode*
visit_event_and_parse_node(struct named_field_class *class, size_t at, size_t length, char *buff, GNode *root) {
	GQueue *q;
	GHashTable *lenhash;
	struct named_field_class compound_end;
	unsigned int field_size;
	int skip = 0;
	GNode *node;
	if(root) {
		node = root;
	} else {
		node = g_node_new(NULL);
	}
	size_t total = at + length;
	q = g_queue_new();
	g_queue_push_tail(q, class);
	//lenhash = g_hash_table_new(g_str_hash, g_str_equal);
	lenhash = g_hash_table_new(NULL, NULL);
	while(!g_queue_is_empty (q)) {
		//printf("at %i\n", at);
		struct named_field_class * nfc = (struct named_field_class *)g_queue_pop_tail(q);
		if(nfc == &compound_end) {
			node = node->parent;
			continue;
		}	
		switch(nfc->fc->type) {
			case FIELD_CLASS_TYPE_LENCONDITION:
			{
				GVariant *result;
				struct field_class_lencondition *len_f = (struct field_class_lencondition *)(nfc->fc);
				if(g_hash_table_contains(lenhash, len_f->condition)) {
					struct len_struct* lenstru = g_hash_table_lookup(lenhash, len_f->condition);
					if(lenstru) {
						if(lenstru->pos < lenstru->length * 8) {
							g_queue_push_tail(q, nfc);
							g_queue_push_tail(q, len_f->class);
						} else {
							g_hash_table_remove(lenhash, len_f->condition);
							//free(lenstru);
						}
					}

				}else{
					GVariant *condlen = compute_formula(len_f->condition, node, &lenhash);
					GVariant *tmp = cast_to(condlen, G_VARIANT_TYPE_INT64);
					int64_t tsize = g_variant_get_int64(tmp);	
					
					if(tsize > 0){
						struct len_struct* lenstru = g_new0(struct len_struct,1);
						lenstru->name = len_f->condition;
						lenstru->length = tsize;
						lenstru->pos = 0;	

						g_hash_table_insert(lenhash, len_f->condition, lenstru);	

						g_queue_push_tail(q, nfc);
						g_queue_push_tail(q, len_f->class);
					}
				}
				break;
			}
			case FIELD_CLASS_TYPE_SKIP:
			{
				struct field_class_skip *skip_f = (struct field_class_skip *)(nfc->fc);
				uint64_t length = skip_f->length;
				at+=length;
				if(g_hash_table_size(lenhash)>0)
					length_cb(&lenhash, length);
				#ifdef DEBUG
				struct parsed_node *pn = g_new0(struct parsed_node, 1);
				pn->type = SIMPLE;				
				struct simple_node *sn = g_new0(struct simple_node, 1);
				// Allocates storage
				char *tmp = (char*)malloc(12 * sizeof(char));
				sprintf(tmp, "skip_%i", skip++);
				sn->var = g_variant_new("{sv}", tmp, g_variant_new_int32(0));;
				sn->pos = at-length;
				sn->size = length;
				pn->u.sn = sn;
				g_node_append_data(node, pn);	
				#endif							
				break;
			}
			case FIELD_CLASS_TYPE_INT:
			{
			struct field_class_int *int_f = (struct field_class_int *)(nfc->fc);
			GVariant *result;
			if(int_f->is_signed) {
				int64_t v;
				field_size = int_f->base.size;
				read_signed_bitfield(buff, at,
						field_size, bo,
						&v);
				at+=field_size;
				if(g_hash_table_size(lenhash)>0)
					length_cb(&lenhash, field_size);
				if(field_size > 32){
					result = g_variant_new_int64(v);		
				} else {
					result = g_variant_new_int32(v);	
				}

			} else {
				uint64_t v;
				field_size = int_f->base.size;
				read_unsigned_bitfield(buff, at,
						field_size, bo,
						&v);
				at+=field_size;
				if(g_hash_table_size(lenhash)>0)
					length_cb(&lenhash, field_size);
				if(field_size > 32){
					result = g_variant_new_uint64(v);			
				} else {
					result = g_variant_new_uint32(v);			
				}					
			}
			if(!nfc->in_array){
				result = g_variant_new("{sv}", nfc->name->str, result);
			}
			struct parsed_node *pn = g_new0(struct parsed_node, 1);
			pn->type = SIMPLE;			
			#ifdef DEBUG
			struct simple_node *sn = g_new0(struct simple_node, 1);
			sn->var = result;
			sn->pos = at-field_size;
			sn->size = field_size;
			pn->u.sn = sn;
			#else
			pn->u.var = result;	
			#endif			
			g_node_append_data(node, pn);		
						
			break;
			}
			case FIELD_CLASS_TYPE_ENUM:
			{	
			struct field_class_enum *enum_f = (struct field_class_enum *)(nfc->fc);
			GVariant *result;
			if(enum_f->base.is_signed) {
				int64_t v;
				field_size = enum_f->base.base.size;
				read_signed_bitfield(buff, at,
						field_size, bo,
						&v);
				at+=field_size;
				if(g_hash_table_size(lenhash)>0)
					length_cb(&lenhash, field_size);
				/*find the value in enum mapping*/
				gboolean found = FALSE;
				for (GList *l = enum_f->mappings; l != NULL; l = l->next)
				{
				struct field_class_enum_mapping * map = (struct field_class_enum_mapping *)(l->data);
				if(map->value.i == v) {
					found = TRUE;
					//result = g_variant_new_string(map->label->str);
					result = g_variant_new("{sx}", map->label->str, v);
					//printf("enum str %s\n",map->label->str);
					if(!nfc->in_array){
						result = g_variant_new("{sv}", nfc->name->str, result);
					}
					struct parsed_node *pn = g_new0(struct parsed_node, 1);
					pn->type = SIMPLE;
					#ifdef DEBUG
					struct simple_node *sn = g_new0(struct simple_node, 1);
					sn->var = result;
					sn->pos = at-field_size;
					sn->size = field_size;
					pn->u.sn = sn;
					#else
					pn->u.var = result;	
					#endif			
					g_node_append_data(node, pn);													  
					break;
				}
				}
				if(!found){
					if(enum_f->default_tag) {
					//result = g_variant_new_string(enum_f->default_tag->str);
					result = g_variant_new("{sx}", enum_f->default_tag->str, -1);
					//printf("enum str %s\n",enum_f->default_tag->str);
					if(!nfc->in_array){
						result = g_variant_new("{sv}", nfc->name->str, result);
					}
					struct parsed_node *pn = g_new0(struct parsed_node, 1);
					pn->type = SIMPLE;
					#ifdef DEBUG
					pn->u.sn = g_new0(struct simple_node, 1);
					pn->u.sn->var = result;
					#else
					pn->u.var = result;	
					#endif			
					g_node_append_data(node, pn);	
					} else {
						result = g_variant_new_int64(v);
						//printf("enum str %i\n",v);
						if(!nfc->in_array){
							result = g_variant_new("{sv}", nfc->name->str, result);
						}
						struct parsed_node *pn = g_new0(struct parsed_node, 1);
						pn->type = SIMPLE;
						#ifdef DEBUG
						struct simple_node *sn = g_new0(struct simple_node, 1);
						sn->var = result;
						sn->pos = at-field_size;
						sn->size = field_size;
						pn->u.sn = sn;
						#else
						pn->u.var = result;	
						#endif				
						g_node_append_data(node, pn);							
					}

				}				
				//printf("enum %i\n",v);
			} else {
				uint64_t v;
				field_size = enum_f->base.base.size;
				read_unsigned_bitfield(buff, at,
						field_size, bo,
						&v);
				at+=field_size;
				if(g_hash_table_size(lenhash)>0)
					length_cb(&lenhash, field_size);
				/*find the value in enum mapping*/
				gboolean found = FALSE;
				for (GList *l = enum_f->mappings; l != NULL; l = l->next)
				{
				struct field_class_enum_mapping * map = (struct field_class_enum_mapping *)(l->data);
				if(map->value.u == v) {
					found = TRUE;
					//result = g_variant_new_string(map->label->str);
					result = g_variant_new("{st}", map->label->str, v);
					//printf("enum str %s\n",map->label->str);
					if(!nfc->in_array){
						result = g_variant_new("{sv}", nfc->name->str, result);
					}
					struct parsed_node *pn = g_new0(struct parsed_node, 1);
					pn->type = SIMPLE;
					#ifdef DEBUG
					struct simple_node *sn = g_new0(struct simple_node, 1);
					sn->var = result;
					sn->pos = at-field_size;
					sn->size = field_size;
					pn->u.sn = sn;
					#else
					pn->u.var = result;	
					#endif				
					g_node_append_data(node, pn);				  
					break;
				}
				}
				if(!found){
					if(enum_f->default_tag) {
						result = g_variant_new("{sx}", enum_f->default_tag->str, -1);
						//printf("enum str %s\n",enum_f->default_tag->str);
						if(!nfc->in_array){
							result = g_variant_new("{sv}", nfc->name->str, result);
						}
						struct parsed_node *pn = g_new0(struct parsed_node, 1);
						pn->type = SIMPLE;
						#ifdef DEBUG
						struct simple_node *sn = g_new0(struct simple_node, 1);
						sn->var = result;
						sn->pos = at-field_size;
						sn->size = field_size;
						pn->u.sn = sn;
						#else
						pn->u.var = result;	
						#endif					
						g_node_append_data(node, pn);
					} else {
						result = g_variant_new_uint64(v);
						//printf("enum str %i\n",v);
						if(!nfc->in_array){
							result = g_variant_new("{sv}", nfc->name->str, result);
						}
						struct parsed_node *pn = g_new0(struct parsed_node, 1);
						pn->type = SIMPLE;
						#ifdef DEBUG
						struct simple_node *sn = g_new0(struct simple_node, 1);
						sn->var = result;
						sn->pos = at-field_size;
						sn->size = field_size;
						pn->u.sn = sn;
						#else
						pn->u.var = result;	
						#endif				
						g_node_append_data(node, pn);						
					}		
				}			
				//printf("enum %i\n",v);
			}
			break;
			}
			case FIELD_CLASS_TYPE_FLOAT:
			{
			struct field_class_float *float_f = (struct field_class_float *)(nfc->fc);
			GVariant *result;
			double dblval;
			field_size = float_f->base.size;
			if(field_size == 32){
				uint64_t v;
				union {
					uint32_t u;
					float_t f;
				} f32;

				read_unsigned_bitfield(buff, at,
						field_size, bo,
						&v);
				at+=field_size;
				if(g_hash_table_size(lenhash)>0)
					length_cb(&lenhash, field_size);
				f32.u = (uint32_t) v;
				dblval = (double_t) f32.f;
				result = g_variant_new_double(dblval);
		
				//printf("float %f\n",dblval);
			}
			else if(field_size == 64)
			{
				union {
					uint64_t u;
					double_t d;
				} f64;

				read_unsigned_bitfield(buff, at, field_size, bo, &f64.u);
				at+=field_size;
				if(g_hash_table_size(lenhash)>0)
					length_cb(&lenhash, field_size);
				dblval = f64.d;
				result = g_variant_new_double(dblval);

				//printf("float %f\n",dblval);
			} else {
				at+=field_size;
				printf("unsupported\n");
				return FALSE;
			}
			if(!nfc->in_array){
				result = g_variant_new("{sv}", nfc->name->str, result);
			}
			struct parsed_node *pn = g_new0(struct parsed_node, 1);
			pn->type = SIMPLE;
			#ifdef DEBUG
			struct simple_node *sn = g_new0(struct simple_node, 1);
			sn->var = result;
			sn->pos = at-field_size;
			sn->size = field_size;
			pn->u.sn = sn;
			#else
			pn->u.var = result;	
			#endif			
			g_node_append_data(node, pn);	
			break;
			}
			case FIELD_CLASS_TYPE_STRING:
			{
			struct field_class_string *string_f = (struct field_class_string *)(nfc->fc);
			GVariant *result;
			field_size = string_f->base.size;
			GString *str = g_string_new_len(&buff[at], field_size);
			at+=field_size;
			if(g_hash_table_size(lenhash)>0)
					length_cb(&lenhash, field_size);
			result = g_variant_new_string(str->str);
			if(!nfc->in_array){
				result = g_variant_new("{sv}", nfc->name->str, result);
			}
			struct parsed_node *pn = g_new0(struct parsed_node, 1);
			pn->type = SIMPLE;
			#ifdef DEBUG
			struct simple_node *sn = g_new0(struct simple_node, 1);
			sn->var = result;
			sn->pos = at-field_size;
			sn->size = field_size;
			pn->u.sn = sn;
			#else
			pn->u.var = result;	
			#endif				
			g_node_append_data(node, pn);	
		  		  
			break;
			}
			case FIELD_CLASS_TYPE_STRUCT:
			{
				/*compound begin, called when a compound begin. create a new parser_scope*/
				g_queue_push_tail(q, &compound_end);
				struct field_class_struct *stru = (struct field_class_struct *)(nfc->fc);
				for (GList *l = g_list_last(stru->members); l != NULL; l = l->prev)
				{
					g_queue_push_tail(q, l->data);
				}
				struct compound_node* cn = g_new0(struct compound_node, 1);
				cn->label = nfc->name;
				cn->isArray = FALSE;

				struct parsed_node* pn = g_new0(struct parsed_node, 1);
				pn->type = COMPOUND;
				pn->u.cpn = cn;																	
				node = g_node_append_data(node, pn);
				break;
			}
			case FIELD_CLASS_TYPE_ARRAY:
			{				
				struct field_class_array *arr = (struct field_class_array *)(nfc->fc);				
				if(arr->arrType == LENCONDITION) {
					struct field_class_lencondition *cond_fc = g_new0(struct field_class_lencondition, 1);
					cond_fc->base.type = FIELD_CLASS_TYPE_LENCONDITION; 
					cond_fc->class = arr->item;
					cond_fc->condition = arr->u.lenvar;

					struct named_field_class *tmp_fc = g_new0(struct named_field_class, 1);
					tmp_fc->fc = cond_fc;	

					g_queue_push_tail(q, &compound_end);
					g_queue_push_tail(q, tmp_fc);

					struct compound_node* cn = g_new0(struct compound_node, 1);
					cn->label = nfc->name;
					cn->isArray = TRUE;
					struct parsed_node* pn = g_new0(struct parsed_node, 1);
					pn->type = COMPOUND;
					pn->u.cpn = cn;																	
					node = g_node_append_data(node, pn);		

				} else {
					gint64 size = 0;
					switch(arr->arrType) {
						case ARRAY:
						{
							size = arr->u.length;					
							break;
						}
						case SEQUENCE:
						{
							/*found the sequence_ref in hash*/
							GVariant *seq_variant = compute_formula(arr->u.length_ref, node, &lenhash);
							GVariant *tmp = cast_to(seq_variant, G_VARIANT_TYPE_INT64);
							size = g_variant_get_int64(tmp);	
							break;
						}
						case FORMULA:
						{
							GVariant *v = compute_formula(arr->u.formula->node, node, &lenhash);					
							GVariant *tmp = cast_to(v, G_VARIANT_TYPE_INT64);
							size = g_variant_get_int64(tmp);											
							break;
						}
						default:
							break;
					}
					//printf("array size : %i\n", size);
					if(size >0) {
						g_queue_push_tail(q, &compound_end);
						for(int i=0;i<size;i++) {
							g_queue_push_tail(q, arr->item);
						}						
						struct compound_node* cn = g_new0(struct compound_node, 1);
						cn->label = nfc->name;
						cn->isArray = TRUE;
						struct parsed_node* pn = g_new0(struct parsed_node, 1);
						pn->type = COMPOUND;
						pn->u.cpn = cn;																	
						node = g_node_append_data(node, pn);	
					} 
				}
				break;
			}
			case FIELD_CLASS_TYPE_VARIANT:
			{
				g_queue_push_tail(q, &compound_end);
				struct field_class_variant *var = (struct field_class_variant *)(nfc->fc);
				/*find the target ref and the value(should be a string)*/
				struct TagRefData ref;
				ref.tag = var->tag_ref;
	  			g_node_traverse (node, G_POST_ORDER, G_TRAVERSE_LEAVES, 2, find_tagref, &ref);
				GNode *tmp = node;
				while(!ref.value){
					tmp = tmp->parent;
					g_node_traverse (tmp, G_POST_ORDER, G_TRAVERSE_LEAVES, 2, find_tagref, &ref);
				}
				const gchar *ret_val = ref.value->str;
				//printf("found the variant %s\n", ret_val);	  
				for (GList *l = var->options; l != NULL; l = l->next)
				{
					struct named_field_class *tmp = (struct named_field_class *)l->data;
					if(strcmp(ret_val, tmp->name->str) == 0) {
						g_queue_push_tail(q, tmp);
						struct compound_node* cn = g_new0(struct compound_node, 1);
						cn->label = nfc->name;
						cn->isArray = FALSE;
						struct parsed_node* pn = g_new0(struct parsed_node, 1);
						pn->type = COMPOUND;
						pn->u.cpn = cn;																	
						node = g_node_append_data(node, pn);
						break;
					}			
				}	
		
				break;
			}
			case FIELD_CLASS_TYPE_FORMULA:
			{
				struct field_class_formula *formula = (struct field_class_formula *)(nfc->fc);
				/*only int, float, string, enum, sequence(0 or 1) supported*/
				/*visit gnode the parse the formula*/
				GVariant *v = compute_formula(formula->node, node, &lenhash);
				//const GVariantType *t = g_variant_get_type(v);
				
				GVariant *ret;
				switch(formula->class->fc->type){
					case FIELD_CLASS_TYPE_INT:
					{
						if(g_variant_is_container(v)) {
							v = g_variant_get_child_value(v, 1);
						}						
						ret = cast_to(v, G_VARIANT_TYPE_INT64);
					break;
					}
					case FIELD_CLASS_TYPE_FLOAT:
					{
						if(g_variant_is_container(v)) {
							v = g_variant_get_child_value(v, 1);
						}						
						ret = cast_to(v, G_VARIANT_TYPE_DOUBLE);
					break;
					}
					case FIELD_CLASS_TYPE_STRING:
					{
					break;
					}
					case FIELD_CLASS_TYPE_ENUM:
					{
						struct field_class_enum *enum_f = (struct field_class_enum *)(formula->class->fc);
						if(g_variant_is_container(v)) {
							v = g_variant_get_child_value(v, 1);
						}											
						GVariant *tmp = cast_to(v, G_VARIANT_TYPE_INT64);
						int64_t i = g_variant_get_int64(tmp);
						gboolean found = FALSE;
						//cast the value to enum
						if(enum_f->base.is_signed) {							
							for (GList *l = enum_f->mappings; l != NULL; l = l->next)
							{
								struct field_class_enum_mapping * map = (struct field_class_enum_mapping *)(l->data);
								if(map->value.i == i) {
									found = TRUE;								
									ret = g_variant_new("{sx}", map->label->str, i);						  
									break;
								}
							}
						} else {
							for (GList *l = enum_f->mappings; l != NULL; l = l->next)
							{
								struct field_class_enum_mapping * map = (struct field_class_enum_mapping *)(l->data);
								if(map->value.u == i) {
									found = TRUE;
									ret = g_variant_new("{sx}", map->label->str, i);								  
									break;
								}
							}						
						}
						if(!found && enum_f->default_tag){
							ret = g_variant_new("{sx}", enum_f->default_tag->str, i);
						}												
					break;
					}
					default:
					printf("error formula type\n");
					break;
				}
				if(!nfc->in_array){
					ret = g_variant_new("{sv}", formula->class->name->str, ret);
				}
				struct parsed_node *pn = g_new0(struct parsed_node, 1);
				pn->type = SIMPLE;
				#ifdef DEBUG
				struct simple_node *sn = g_new0(struct simple_node, 1);
				sn->var = ret;
				pn->u.sn = sn;
				#else
				pn->u.var = ret;	
				#endif				
				g_node_append_data(node, pn);										
				break;
			}
			case FIELD_CLASS_TYPE_SHOWONLYIF:
			{
				struct field_class_showonlyif *showonlyif = (struct field_class_showonlyif *)(nfc->fc);		
				GVariant *v = compute_formula(showonlyif->node, node, &lenhash);
				GVariant *x = cast_to(v, G_VARIANT_TYPE_BOOLEAN);
				gboolean ret = g_variant_get_boolean(x);
				if(ret) {
					g_queue_push_tail(q, showonlyif->class);						
				}
				break;
			}
			default:
			break;
		}
	}
	if(at != total) {
		printf("length not match %i\n", total-at);
		//at = total;		
	}
	g_queue_free(q);
	return node;
}

static gboolean
find_logcode (GNode    *node,
             gpointer  data)
{
  struct parsed_node *pn = node->data;
  if(pn && pn->type == SIMPLE) {
	#ifdef DEBUG
	GVariant *var = pn->u.sn->var;
	#else
	GVariant *var = pn->u.var;  
	#endif	
	
	if(var && strcmp(g_variant_get_type_string(var),"{sv}")==0) {
		const gchar *str = NULL;
		g_variant_get_child (var, 0, "&s", &str);
		if(strcmp(str, "logcode") == 0)
		{
			GVariant *a = g_variant_get_variant(g_variant_get_child_value (var, 1));
			guint32 *ret = data;
			*ret = g_variant_get_uint32(a);
			printf("found log code %i\n",*ret);
			return TRUE;
		}  
	}
  }
  return FALSE;
}

static gboolean
find_loglength (GNode    *node,
             gpointer  data)
{
  struct parsed_node *pn = node->data;
  if(pn && pn->type == SIMPLE) {	
	  #ifdef DEBUG
	  GVariant *var = pn->u.sn->var;
	  #else
	  GVariant *var = pn->u.var;  
	  #endif
	if(var && strcmp(g_variant_get_type_string(var),"{sv}")==0) {
		const gchar *str = NULL;
		g_variant_get_child (var, 0, "&s", &str);
		if(strcmp(str, "len") == 0)
		{
			GVariant *a = g_variant_get_variant(g_variant_get_child_value (var, 1));
			guint32 *ret = data;
			*ret = g_variant_get_uint32(a);
			printf("found log len %i\n",*ret);
			return TRUE;
		}  
	}
  }
  return FALSE;
}

typedef struct{
	size_t pos;
	uint32_t len;
	uint32_t logcode;
	char *buff;
	GNode *header;
} ParseData;

static void
pool_func (gpointer data, gpointer user_data)
{
  ParseData *m = data;
  GHashTable *events = user_data;
  struct named_field_class * event_context = g_hash_table_lookup(events,GUINT_TO_POINTER(m->logcode));
  if(!event_context){
	/*this log code not supported, just skip to next log*/
	printf("LogCode not found: %i\n", m->logcode);
	return;
  }

	GNode *nd = visit_event_and_parse_node(event_context, m->pos, m->len, m->buff, m->header);
	GVariant *output = variant_traverse(nd);

	if(rules) {
	struct rule_node *rn = g_hash_table_lookup(rules, GUINT_TO_POINTER(m->logcode));
	if(!rn) {
		printf("No rule associate with event %i\n", m->logcode);
	} else {
		GVariant* outp = evaluate(rn, output);
		gsize len;
		gchar *ostr = json_gvariant_serialize_data (outp,&len);
		printf("%s\n", ostr);
		g_free(ostr); 
		g_variant_unref(outp);
	}
	}

	#ifdef DEBUG
	gsize length;
	gchar *jstring = json_gvariant_serialize_data (output,&length);
	printf("%s\n", jstring);
	printf("Logcode %i finished. position %i. length %i\n", m->logcode, m->pos/8, m->len/8);
	g_free(jstring); 
	#endif
	g_variant_unref(output);
	g_node_destroy(nd);
	g_free(data);
}

static void get_int_type(unsigned int t_size, gboolean is_signed, char** tname) {	
	int len;
	if(t_size<=8)
		len = 8;
	else if (t_size <= 16) {
		len = 16;
	}
	else if (t_size <= 32) {
		len = 32;
	} else {
		len = 64;
	}

	snprintf(*tname, 10, "%s%d_t", is_signed ? "int":"uint", len);	
	//return tname;
}

static void get_float_type(unsigned int t_size, char** tname) {	
	snprintf(*tname, 10, "%s",  t_size<=32?"float":"double");	
	//return tname;
}

static char* replaceWord(const char* s, const char* oldW,
                  const char* newW)
{
    char* result;
    int i, cnt = 0;
    int newWlen = strlen(newW);
    int oldWlen = strlen(oldW);
  
    // Counting the number of times old word
    // occur in the string
    for (i = 0; s[i] != '\0'; i++) {
        if (strstr(&s[i], oldW) == &s[i]) {
            cnt++;
  
            // Jumping to index after the old word.
            i += oldWlen - 1;
        }
    }
  
    // Making new string of enough length
    result = (char*)malloc(i + cnt * (newWlen - oldWlen) + 1);
  
    i = 0;
    while (*s) {
        // compare the substring with the result
        if (strstr(s, oldW) == s) {
            strcpy(&result[i], newW);
            i += newWlen;
            s += oldWlen;
        }
        else
            result[i++] = *s++;
    }
  
    result[i] = '\0';
    return result;
}

static gboolean write_decl(GHashTable *hash) {
	FILE *fp = fopen("ctfparser.h", "w");
	if (fp == NULL) 
	{   
		printf("Error! Could not open file\n"); 
		exit(-1); // must include stdlib.h 
	} 
	fprintf(fp, "#ifndef WRAP_NAMESPACE\n \
		#define WRAP_NAMESPACE(ns, x) ns ## _ ## x\n \
		#endif\n");

	fprintf(fp, "#include \"bitfield.h\"\n");
	GHashTableIter iter;
    gpointer k, v;
	char *tname = (char *)malloc(10*sizeof(char));
	g_hash_table_iter_init (&iter, hash);
	while (g_hash_table_iter_next (&iter, &k, &v))
	{
		struct field_class* fc = (struct field_class*)v;
		char* aname = (char*)k;
		if(fc){
			if(fc->type == FIELD_CLASS_TYPE_INT) {
				struct field_class_int* ifc = (struct field_class_int*)v;
				get_int_type(fc->size, ifc->is_signed, &tname);
				//fprintf(fp, "typedef %s %s;\n", tname, k);
				
				if(ifc->is_signed) {					
					fprintf(fp, "static inline %s read_%s(const uint8_t *buf, size_t* at) {\n \
						int64_t v;\n \
						bt_bitfield_read(buf, uint8_t, *at, %d, &v);\n \
						*at+=%d;\n \
						return (%s)v;\n \
						}\n", tname, aname, fc->size, fc->size, tname);
				} else{
						fprintf(fp, "static inline %s read_%s(const uint8_t *buf, size_t* at) {\n \
						uint64_t v;\n \
						bt_bitfield_read(buf, uint8_t, *at, %d, &v);\n \
						*at+=%d;\n \
						return (%s)v;\n \
						}\n", tname, aname, fc->size, fc->size, tname);
				
				}
			} else if(fc->type == FIELD_CLASS_TYPE_FLOAT) {
				//fprintf(fp, "typedef %s %s;\n", fc->size<=32 ? "float":"double", k);
				if(fc->size<=32) {
					fprintf(fp, "static inline double read_%s(const uint8_t *buf, size_t* at) {\n \
						uint64_t v;\n \
						double dblval;\n \
						union {\n \
						uint32_t u;\n \
						float f;\n \
						} f32;\n \
						bt_bitfield_read(buf, uint8_t, *at, %d, &v);\n \
						*at+=%d;\n \
						f32.u = (uint32_t) v;\n \
						dblval = (double) f32.f;\n \
						return dblval;\n \
						}\n", aname, fc->size, fc->size);
				} else {
					fprintf(fp, "static inline double read_%s(const uint8_t *buf, size_t* at) {\n \
						uint64_t v;\n \
						double dblval;\n \
						union {\n \
						uint64_t u;\n \
						double d;\n \
						} f64;\n \
						bt_bitfield_read(buf, uint8_t, *at, %d, &v);\n \
						*at+=%d;\n \
						f64.u = (uint64_t) v;\n \
						dblval = (double) f64.d;\n \
						return dblval;\n \
						}\n", aname, fc->size, fc->size);

				}
			}
		}
	}

    /* Close file to save file data */
	free(tname);
    fclose(fp);
	return TRUE;
}

static char* safe_name(const char *name) {
	char *sname = replaceWord(name, " ","_");
	return sname;
}

static gboolean write_to_file(FILE *fp, GList *strs) {
	for (GList *l = strs; l != NULL; l = l->next)
	{
		GString *s = (GString*)l->data;
		fprintf(fp, "%s\n", s->str);
		g_string_free(s, TRUE);
	}		
}

void init_prefix(struct node_scope *scope,
		       struct node_scope *parent)
{
	scope->parent = parent;
	scope->nodename = g_string_new(NULL);
}

void finalize_prefix(struct node_scope *scope)
{
	g_string_free(scope->nodename, TRUE);
}

void push_prefix(struct NodePrefix *prefix)
{
	struct node_scope *ns;

	ns = malloc(sizeof(struct node_scope));
	init_prefix(ns, prefix->ns);
	prefix->ns = ns;
}

void pop_prefix(struct NodePrefix *prefix)
{
	struct node_scope *ns;

	ns = prefix->ns;
	prefix->ns = ns->parent;
	finalize_prefix(ns);
	free(ns);
}

GString *build_prefix(struct node_scope *prefix) {
	GString *ret = g_string_new(NULL);
	struct node_scope *scope = prefix;
	while(scope) {
		GString *tmp = g_string_new(NULL);
		g_string_printf(tmp, "%s_",scope->nodename->str);
		g_string_prepend(ret, tmp->str);
		g_string_free(tmp, TRUE);
		scope = scope->parent;
	}
	g_string_set_size(ret, ret->len-1);
	return ret;
}

static gboolean write_struct(struct field_class_struct *fc,char *sname, GList **strs, struct NodePrefix *prefix);

static gboolean write_variant(struct field_class_variant *fc, char *sname, GList **strs, struct NodePrefix *prefix) {
	GString *fields = g_string_new(NULL);
	GString *parser = g_string_new(NULL);
	GString *freer = g_string_new(NULL);

	GString *oldprefix = build_prefix(prefix->ns);
	g_string_append_printf(oldprefix, "_%s", fc->tag_ref->str);

	push_prefix(prefix);
	g_string_append(prefix->ns->nodename, sname);
	prefix->ns->nodetype = NODEVARIANT;

	GString *newprefix = build_prefix(prefix->ns);

	g_string_append_printf(fields, "union %s {\n", newprefix->str);
	g_string_append(fields, "void *parent;\n");
	g_string_append_printf(parser, "union %s *read_%s(const uint8_t *buf, size_t* at, enum %s _%s, void *parent) {\n", newprefix->str, newprefix->str, fc->tag_ref->str, fc->tag_ref->str);
	g_string_append_printf(parser, "union %s *un = (union %s *)malloc(sizeof(union %s));\n", newprefix->str, newprefix->str, newprefix->str);
	g_string_append_printf(parser, "un->parent = parent;\n");
	g_string_append_printf(parser, "switch(_%s) {\n", fc->tag_ref->str);

	g_string_append_printf(freer, "void free_%s(union %s *un) {\n", newprefix->str, newprefix->str);
	//fields = g_list_append(fields, g_string_free(str, FALSE));

	for (GList *l = fc->options; l != NULL; l = l->next) {
		struct named_field_class *nfc = (struct named_field_class *)l->data;
		char *name = safe_name(nfc->name->str);
		switch(nfc->fc->type) {
			case FIELD_CLASS_TYPE_STRUCT:
			{
				
				GString *subprefix = g_string_new(newprefix->str);
				g_string_append_printf(subprefix, "_%s", name);
				g_string_append_printf(fields, "struct %s *%s;\n", subprefix->str, name);

				g_string_append_printf(parser, "case %s_%s:\n{\n", oldprefix->str, name);
				g_string_append_printf(parser, "un->%s = read_%s(buf,at,parent);\n",name, subprefix->str);
				g_string_append_printf(parser, "break;\n}\n");
				write_struct((struct field_class_struct *)nfc->fc, name, strs, prefix);

				g_string_free(subprefix,TRUE);
				//pop_prefix(prefix);
				break;
			}
			default:
				break;
		}
	}
	g_string_append_printf(fields, "};\n");
	
	g_string_append_printf(parser, "default:\nbreak;\n}\n");
	g_string_append_printf(parser, "return un;\n}\n");
	g_string_append_printf(freer, "free(un);\n}\n");

	GString *f = (GString*)g_list_nth_data(*strs, 0);
	GString *p = (GString*)g_list_nth_data(*strs, 1);
	GString *r = (GString*)g_list_nth_data(*strs, 2);
	g_string_append(f, fields->str);
	g_string_append(p, parser->str);
	g_string_append(r, freer->str);

	g_string_free(fields,TRUE);
	g_string_free(parser,TRUE);
	g_string_free(freer,TRUE);

	g_string_free(newprefix,TRUE);
	g_string_free(oldprefix,TRUE);
	pop_prefix(prefix);

	return TRUE;
}

static gboolean
eval_cb (const GMatchInfo *info,
         GString          *res,
         gpointer          data)
{
  gchar *match;

   match = g_match_info_fetch (info, 0);
   struct node_level *s = (struct node_level *)data;
   //remove the last '_' 

   //get the parent of the scope 
   int i = 0;
   struct node_scope* ns = s->scope;
   while(i < s->level){
	   i++;
	   ns = ns->parent;
   }
   if(ns) {
	   GString *tmp = build_prefix(ns);
	   g_string_append_printf (res, "((%s %s *)%s)", ns->nodetype == NODESTRUCT? "struct" : "union", tmp->str, match);	   
	   g_string_free(tmp, TRUE);
   } else {
	   g_string_append_printf (res, "((struct Header *)%s)", match);	 
   }
   g_free (match);

   return FALSE;
}

static GString *conversion(struct node_scope* a, char* b) {
	GRegex *reg;
	GString *res = g_string_new(b);
	GString *tmp = build_prefix(a);
	GMatchInfo *match_info;
    GError *error = NULL;
	reg = g_regex_new("^.*?parent(?=->)", 0, 0, NULL);
	
	struct node_level lev;
	lev.scope = a;
	lev.level = 0;
	
	while(g_regex_match(reg, res->str, 0, &match_info)) {		
		lev.level++;
		gchar *ret = g_regex_replace_eval (reg, res->str, -1, 0, 0, eval_cb, &lev, NULL);		
		g_string_assign(res, ret);		
	}
	gchar* headpos = g_strrstr(res->str, "->Header");
	if(headpos) {
		g_string_erase(res, headpos-res->str, 8);
	}
	g_free(match_info);
	g_regex_unref(reg);
	g_string_free(tmp, TRUE);
	return res;
}

static gboolean is_booltype(GNode *nd) {
	struct ctf_ast *ast = (struct ctf_ast *)nd->data;
	if(ast->type == NODE_MATH_EXPRESSION) {
		switch(ast->u.math_expression.type) {
			case MATH_NOT:
			case MATH_EQEQ:
			case MATH_ANDAND:
			case MATH_OROR:
			case MATH_GT:
			case MATH_LT:
			case MATH_GTEQ:
			case MATH_LTEQ:
				return TRUE;
		}
	}
	return FALSE;
}

static GString* build_unary(GNode *nd, struct node_scope *a) {
	/*build the unary string from node*/
	GString *t = g_string_new(NULL);
	GString* left_str = NULL;
	GString* right_str = NULL;
	struct ctf_ast *ast = (struct ctf_ast *)nd->data;
	g_assert(ast->type == NODE_MATH_EXPRESSION || ast->type == NODE_UNARY_EXPRESSION);
	if(ast->type == NODE_MATH_EXPRESSION) {
		switch(ast->u.math_expression.type) {
			case MATH_NOT:
				left_str = build_unary(g_node_first_child(nd), a);
			    g_string_printf(t,"(!%s)", left_str->str);
				break;
			case MATH_SL:
				left_str = build_unary(g_node_first_child(nd), a);
				right_str = build_unary(g_node_last_child(nd), a);
			    g_string_printf(t,"(%s << %s)", left_str->str, right_str->str);
				break;
			case MATH_SR:
				left_str = build_unary(g_node_first_child(nd), a);
				right_str = build_unary(g_node_last_child(nd), a);
			    g_string_printf(t,"(%s >> %s)", left_str->str, right_str->str);
				break;
			case MATH_ADD:
				left_str = build_unary(g_node_first_child(nd), a);
				right_str = build_unary(g_node_last_child(nd), a);
			    g_string_printf(t,"(%s + %s)", left_str->str, right_str->str);
				break;
			case MATH_SUB:
				left_str = build_unary(g_node_first_child(nd), a);
				right_str = build_unary(g_node_last_child(nd), a);
			    g_string_printf(t,"(%s - %s)", left_str->str, right_str->str);
				break;
			case MATH_MULTIPLY:
				left_str = build_unary(g_node_first_child(nd), a);
				right_str = build_unary(g_node_last_child(nd), a);
			    g_string_printf(t,"(%s * %s)", left_str->str, right_str->str);
				break;
			case MATH_DIV:
				left_str = build_unary(g_node_first_child(nd), a);
				right_str = build_unary(g_node_last_child(nd), a);
			    g_string_printf(t,"(%s / %s)", left_str->str, right_str->str);
				break;
			case MATH_OROR:
				left_str = build_unary(g_node_first_child(nd), a);
				right_str = build_unary(g_node_last_child(nd), a);
			    g_string_printf(t,"(%s || %s)", left_str->str, right_str->str);
				break;
			case MATH_EQEQ:
				left_str = build_unary(g_node_first_child(nd), a);
				right_str = build_unary(g_node_last_child(nd), a);
			    g_string_printf(t,"(%s == %s)", left_str->str, right_str->str);
				break;
			case MATH_NOTEQ:
				left_str = build_unary(g_node_first_child(nd), a);
				right_str = build_unary(g_node_last_child(nd), a);
			    g_string_printf(t,"(%s != %s)", left_str->str, right_str->str);
				break;
			case MATH_LT:
				left_str = build_unary(g_node_first_child(nd), a);
				right_str = build_unary(g_node_last_child(nd), a);
			    g_string_printf(t,"(%s < %s)", left_str->str, right_str->str);
				break;
			case MATH_LTEQ:
				left_str = build_unary(g_node_first_child(nd), a);
				right_str = build_unary(g_node_last_child(nd), a);
			    g_string_printf(t,"(%s <= %s)", left_str->str, right_str->str);
				break;
			case MATH_GT:
				left_str = build_unary(g_node_first_child(nd), a);
				right_str = build_unary(g_node_last_child(nd), a);
			    g_string_printf(t,"(%s > %s)", left_str->str, right_str->str);
				break;
			case MATH_GTEQ:
				left_str = build_unary(g_node_first_child(nd), a);
				right_str = build_unary(g_node_last_child(nd), a);
			    g_string_printf(t,"(%s >= %s)", left_str->str, right_str->str);
				break;
			case MATH_AND:
				left_str = build_unary(g_node_first_child(nd), a);
				right_str = build_unary(g_node_last_child(nd), a);
			    g_string_printf(t,"(%s & %s)", left_str->str, right_str->str);
				break;
			case MATH_OR:
				left_str = build_unary(g_node_first_child(nd), a);
				right_str = build_unary(g_node_last_child(nd), a);
			    g_string_printf(t,"(%s | %s)", left_str->str, right_str->str);
				break;
			default:
				break;
		}
	} else {
		switch(ast->u.unary_expression.type) {
			case UNARY_STRING:
				g_string_printf(t, "%s", "stru->");
				g_string_append_printf(t, "%s", ast->u.unary_expression.u.string);
				GNode *sub = nd;
				while(!G_NODE_IS_LEAF(sub)) {
					sub = g_node_first_child(sub);
					struct ctf_ast *subast = (struct ctf_ast *)sub->data;
					g_string_append_printf(t, "->%s", subast->u.unary_expression.u.string);
				}
				//g_string_replace(t, "^.","parent->", 0);
				char *tmp = replaceWord(t->str, "^","parent");
				if(strstr(tmp, "parent->")) {
					GString *converted = conversion(a, tmp);
					g_string_assign(t, converted->str);
					g_string_free(converted, TRUE);
				} else {
					g_string_assign(t, tmp);
				}
				free(tmp);
				break;
			case UNARY_SIGNED_CONSTANT:
				g_string_printf(t, "%d", ast->u.unary_expression.u.signed_constant);
				break;
			case UNARY_UNSIGNED_CONSTANT:
				g_string_printf(t, "%d", ast->u.unary_expression.u.unsigned_constant);
				break;
			default:
				break;
		}
	}
	if(left_str)
		g_string_free(left_str,TRUE);
	if(right_str)
		g_string_free(right_str,TRUE);
	return t;
}

static char* 
find_first_key(GHashTable *hash, 
		struct field_class *value)
{
	GHashTableIter iter;
    gpointer k, v;
	g_hash_table_iter_init (&iter, hash);
	while (g_hash_table_iter_next (&iter, &k, &v))
	{
		if(v == value) {
			return (char*)k;
		}
	}
	return NULL;

}

static gboolean write_struct(struct field_class_struct *fc,char *sname, GList **strs, struct NodePrefix *prefix) {
	GString *fields = g_string_new(NULL);
	GString *parser = g_string_new(NULL);
	GString *freer = g_string_new(NULL);
	char *tname = (char *)malloc(10*sizeof(char));		

	push_prefix(prefix);
	g_string_append(prefix->ns->nodename, sname);
	prefix->ns->nodetype = NODESTRUCT;

	GString *newprefix = build_prefix(prefix->ns);

	g_string_append_printf(fields, "struct %s {\n", newprefix->str);
	g_string_append(fields, "void *parent;\n");

	g_string_append_printf(parser, "struct %s* read_%s(const uint8_t *buf, size_t* at, void *parent) {\n", newprefix->str, newprefix->str);	
	g_string_append_printf(parser, "struct %s *stru = malloc(sizeof(struct %s));\n", newprefix->str, newprefix->str);
	g_string_append_printf(parser, "stru->parent = parent;\n");

	g_string_append_printf(freer, "void free_%s(struct %s *stru) {\n", newprefix->str, newprefix->str);



	for (GList *l = fc->members; l != NULL; l = l->next) {
		struct named_field_class *nfc = (struct named_field_class *)l->data;
		char *name;
		if(nfc->name == NULL)
			name = NULL;
		else
			name = safe_name(nfc->name->str);
		//struct field_class *fc = (struct field_class *)nfc->fc;
		
		GString *subprefix = g_string_new(newprefix->str);
		g_string_append_printf(subprefix, "_%s", name);
		
		switch(nfc->fc->type) {
			case FIELD_CLASS_TYPE_SKIP:
			{
				struct field_class_skip* sfc = (struct field_class_skip*)nfc->fc;
				g_string_append_printf(parser, "*at += %d;\n",sfc->length); 			
				break;
			}
			case FIELD_CLASS_TYPE_INT:
			{
				struct field_class_int *ifc = (struct field_class_int *)nfc->fc;
				char *p = find_first_key(decl, ifc);
				get_int_type(ifc->base.size, ifc->is_signed, &tname);
				g_string_append_printf(fields, "%s %s;\n", tname, name); 

				g_string_append_printf(parser, "stru->%s = read_%s(buf, at);\n", name, p); 
				
				break;
			}
			case FIELD_CLASS_TYPE_FORMULA:
			{
				struct field_class_formula *ifc = (struct field_class_formula *)nfc->fc;
				if(ifc->class->fc->type == FIELD_CLASS_TYPE_INT) {
					get_int_type(ifc->class->fc->size, TRUE, &tname);
				} else if(ifc->class->fc->type == FIELD_CLASS_TYPE_FLOAT) {
					get_float_type(ifc->class->fc->size, &tname);
				}
				
				g_string_append_printf(fields, "%s %s;\n", tname, ifc->class->name->str); 
				GString *fstr = build_unary(ifc->node, prefix->ns);		
		
				g_string_append_printf(parser, "stru->%s = %s;\n", ifc->class->name->str, fstr->str); 
				g_string_free(fstr, TRUE);
				break;
			}
			case FIELD_CLASS_TYPE_ENUM:
			{
				struct field_class_enum *c = (struct field_class_enum *)nfc->fc;
				GString *maps = g_string_new("enum ");
				g_string_append_printf(maps, " %s {", subprefix->str);
				for (GList *l = c->mappings; l != NULL; l = l->next)
				{
					struct field_class_enum_mapping *map = (struct field_class_enum_mapping *)l->data;
					g_string_append_printf(maps, "%s_%s = %u,", subprefix->str, map->label->str, map->value.u);
				}	
				if(c->default_tag) {
					g_string_append_printf(maps, "%s_%s = -1,", subprefix->str, c->default_tag->str);
				}
				maps = g_string_set_size(maps, maps->len-1);
				g_string_append_printf(maps, "} %s;\n", name);
				fields = g_string_append(fields, maps->str);
				g_string_free(maps, TRUE);
				g_string_append_printf(parser, "stru->%s = read_%s(buf, at);\n", name, c->type_name->str);
				break;
			}	
			case FIELD_CLASS_TYPE_VARIANT:
			{
				struct field_class_variant *var = (struct field_class_variant *)nfc->fc;
				g_string_append_printf(fields, "union %s *%s;\n", subprefix->str, name);	

				g_string_append_printf(parser, "stru->%s = read_%s(buf, at, stru->%s, stru);\n", name, subprefix->str, var->tag_ref->str);

				g_string_append_printf(freer, "free_%s(stru->%s);\n", subprefix->str, name);
				write_variant(var,name, strs, prefix);
				break;
			}		
			case FIELD_CLASS_TYPE_STRUCT:
			{
				g_string_append_printf(fields, "struct %s *%s;\n", subprefix->str, name);

				g_string_append_printf(parser, "stru->%s = read_%s(buf, at, stru);\n", name, subprefix->str);

				g_string_append_printf(freer, "free_%s(stru->%s);\n", subprefix->str, name);		
				write_struct((struct field_class_struct *)nfc->fc,name , strs, prefix);		
				break;
			}
			case FIELD_CLASS_TYPE_ARRAY:
			{			
				struct field_class_array *arr = (struct field_class_array *)nfc->fc;
				if(arr->item) {
					char *aname;
					char *bname = find_first_key(decl, arr->item->fc);
					gboolean isStruct = FALSE;
					if(arr->item->name)
						aname = safe_name(arr->item->name->str);
					else {
						/*get the type name of the array item*/
						if(arr->item->fc->type == FIELD_CLASS_TYPE_INT) {
							char *xname = (char*)malloc(10*sizeof(char));
							struct field_class_int *arr_ifc = (struct field_class_int *)arr->item->fc;
							get_int_type(arr_ifc->base.size, arr_ifc->is_signed, &xname);
							aname = xname;
						} else if(arr->item->fc->type == FIELD_CLASS_TYPE_STRUCT) {
							aname = subprefix->str;
							write_struct((struct field_class_struct *)arr->item->fc,name , strs, prefix);	
							isStruct = TRUE;
							//g_string_append_printf(fields, "struct ");
						}
					}
					//g_string_append_printf(fields, "%s%s %s*%s;\n", isStruct?"struct ":"",aname, isStruct?"*":"",name);

					if(arr->arrType == ARRAY) {
						g_string_append_printf(fields, "%s%s %s*%s;\n", isStruct?"struct ":"",aname, isStruct?"*":"",name);
						g_string_append_printf(parser, "stru->%s = (%s *)malloc(%d*sizeof(%s));\n", name, aname, arr->u.length, aname);
						g_string_append_printf(parser, "for(int i=0;i<%d;i++) {\n", arr->u.length);
						g_string_append_printf(parser, "stru->%s[i] = read_%s(buf, at%s);\n}\n", name, bname?bname:aname, bname?"":", stru");
					} else if(arr->arrType == SEQUENCE) {
						g_string_append_printf(fields, "%s%s %s*%s;\n", isStruct?"struct ":"",aname, isStruct?"*":"",name);
						struct ctf_ast *lenref = (struct ctf_ast *)arr->u.length_ref->data;
						g_assert(lenref->type == NODE_UNARY_EXPRESSION);
						GString *lengthref = build_unary(arr->u.length_ref, prefix->ns);	

						g_string_append_printf(parser, "stru->%s = (%s%s %s*)malloc(%s*sizeof(%s%s));\n", name,isStruct?"struct ":"", aname,isStruct?"*":"", lengthref->str, isStruct?"struct ":"",aname);
						g_string_append_printf(parser, "for(int i=0;i<%s;i++) {\n", lengthref->str);
						g_string_append_printf(parser, "stru->%s[i] = read_%s(buf, at%s);\n}\n", name, bname?bname:aname, bname?"":", stru");
						//free(tmp);
						g_string_free(lengthref, TRUE);
					} else if(arr->arrType == FORMULA) {
						struct field_class_formula *formula = (struct field_class_formula *)arr->u.formula;
						GString *ustr = build_unary(formula->node, prefix->ns);		
						gboolean btype = is_booltype(formula->node);
						if(btype) {
							g_string_append_printf(fields, "%s%s %s%s;\n", isStruct?"struct ":"",aname, isStruct?"*":"",name);
							g_string_append_printf(parser, "if(%s) {\n", ustr->str);
							g_string_append_printf(parser, "stru->%s = read_%s(buf, at%s);\n", name, bname?bname:aname, bname?"":", stru");
							g_string_append_printf(parser, "}\n", ustr->str);
						} else {							
							g_string_append_printf(fields, "%s%s %s*%s;\n", isStruct?"struct ":"",aname, isStruct?"*":"",name);
							g_string_append_printf(parser, "stru->%s = (%s%s %s*)malloc(%s*sizeof(%s%s));\n", name,isStruct?"struct ":"", aname,isStruct?"*":"", ustr->str, isStruct?"struct ":"",aname);						
							g_string_append_printf(parser, "for(int i=0;i<%s;i++) {\n", ustr->str);
							g_string_append_printf(parser, "stru->%s[i] = read_%s(buf, at%s);\n}\n", name, bname?bname:aname, bname?"":", stru");	
						}
						g_string_free(ustr, TRUE);

					} else if(arr->arrType == LENCONDITION) {
						g_string_append_printf(fields, "%s%s %s*%s;\n", isStruct?"struct ":"",aname, isStruct?"*":"",name);
						GString *lengvarstr = build_unary(arr->u.lenvar, prefix->ns);
						g_string_append_printf(parser, "size_t z = *at;\nint i = 0;\n");
						g_string_append_printf(parser, "stru->%s = (%s%s %s*)malloc(%s*sizeof(%s%s));\n", name,isStruct?"struct ":"", aname,isStruct?"*":"", lengvarstr->str, isStruct?"struct ":"",aname);						
						g_string_append_printf(parser, "while (*at - z < %s*8) {\n", lengvarstr->str);
						g_string_append_printf(parser, "stru->%s[i++] = read_%s(buf, at%s);\n}\n", name, bname?bname:aname, bname?"":", stru");	

						g_string_free(lengvarstr, TRUE);

					} else {
						
					}
				} 				
				break;
			}
			default:
				break;
		}
		free(name);
		g_string_free(subprefix,TRUE);
		//pop_prefix(prefix);
	}

	g_string_append_printf(fields, "};\n");

	g_string_append_printf(fields, "struct %s* read_%s(const uint8_t *buf, size_t* at, void *parent);", newprefix->str, newprefix->str);	

	g_string_append_printf(parser, "return stru;\n}\n");

	g_string_append_printf(freer, "free(stru);\n}\n");				

	free(tname);


	GString *f = (GString*)g_list_nth_data(*strs, 0);
	GString *p = (GString*)g_list_nth_data(*strs, 1);
	GString *r = (GString*)g_list_nth_data(*strs, 2);
	g_string_append(f, fields->str);
	g_string_append(p, parser->str);
	g_string_append(r, freer->str);

	g_string_free(fields,TRUE);
	g_string_free(parser,TRUE);
	g_string_free(freer,TRUE);
	g_string_free(newprefix,TRUE);

	pop_prefix(prefix);
	return TRUE;
}


static gboolean write_events_decl(GHashTable *hash) {
	GHashTableIter iter;
    gpointer k, v;
	g_hash_table_iter_init (&iter, hash);
	while (g_hash_table_iter_next (&iter, &k, &v))
	{
		struct named_field_class* nfc = (struct named_field_class*)v;
		if(nfc){
			char hname[100];
			char fname[100];
			char *name = safe_name(nfc->name->str);
			snprintf(hname, 100, "%s_%04x.h", name, GPOINTER_TO_UINT(k));
			snprintf(fname, 100, "%s_%04x.c", name, GPOINTER_TO_UINT(k));
			FILE *hp = fopen(hname, "w");
			FILE *fp = fopen(fname, "w");
			if (fp == NULL) 
			{   
				printf("Error! Could not open file\n"); 
				exit(-1); // must include stdlib.h 
			} 	

			fprintf(hp, "#include <stdint.h>\n");
			fprintf(fp, "#include <stdint.h>\n\
#include \"stdlib.h\"\n\
#include \"ctfparser.h\"\n", hname);
			if(!strstr(name, "Header")) {
				fprintf(fp, "#include \"Header_0000.h\"\n\
#include \"%s\"\n", hname);
			} else {
				fprintf(fp, "#include \"%s\"\n", hname);
			}
			struct field_class_struct *stru = (struct field_class_struct *)nfc->fc;
			GList *strs = NULL;
			GString *fields = g_string_new(NULL);
			GString *parser = g_string_new(NULL);
			GString *freer = g_string_new(NULL);
			strs = g_list_append (strs, fields);
			strs = g_list_append (strs, parser);
			strs = g_list_append (strs, freer);

			struct NodePrefix prefix;
			prefix.ns = NULL;

			write_struct(stru,name, &strs, &prefix);
			//write_to_file(fp, strs);
			fprintf(hp, "%s\n", fields->str);
			g_string_free(fields, TRUE);

			fprintf(fp, "%s\n", parser->str);
			g_string_free(parser, TRUE);

			fprintf(fp, "%s\n", freer->str);
			g_string_free(freer, TRUE);

			g_list_free(strs);
			//g_string_free(sname,TRUE);
			free(name);
			fclose(hp);
			fclose(fp);		
		}
	}
	return TRUE;
}

static gboolean parse_trace(char* rawfile) {
  GThreadPool *pool;
  GError *err = NULL;
  gboolean success;
  FILE *fp;
  long size;
  char *buff, *buff_end; 
  size_t bytes;
  size_t at = 0;
  fp = fopen(rawfile, "rb");
  //fp = fopen("dd1.dlf", "rb");
  if(fp == NULL) {
    fprintf(stderr, "Can't open test raw file\n");
    exit(-1);
  }
    /* Get file size */
  fseek(fp, 0, SEEK_END);
  size = ftell(fp);
  rewind(fp);

  /* Allocate buffer and read */
  buff = (char*) malloc(size * sizeof(char));
  bytes = fread(buff, 1, size, fp);  
  if (bytes != size) {
    fprintf(stderr, "Error reading input file\n");
    printf("bytes: %d, size: %d\n", bytes, size);
    exit(-1);
  }

  struct named_field_class *nfc;
  pool = g_thread_pool_new (pool_func, event_decl, g_get_num_processors(), TRUE, &err);
  uint32_t index, logcode, loglen;
  while(at < bytes*8) {
	  /*push a event header to the stack and parse it*/
	  struct named_field_class * event_header = g_hash_table_lookup(event_decl,GUINT_TO_POINTER(0));
	  /*return a gvariant for a later use*/
	  GNode* header = visit_event_and_parse_node(event_header, at, 12*8, buff, NULL);
	  at += 12*8;
	  g_node_traverse (header, G_POST_ORDER, G_TRAVERSE_LEAVES, -1, find_logcode, &logcode);
	  g_node_traverse (header, G_POST_ORDER, G_TRAVERSE_LEAVES, -1, find_loglength, &loglen);
	  printf("LogCode %i\n", logcode);

	  ParseData *p = g_new0(ParseData, 1);
	  p->len = (loglen-12)*8;
	  p->pos = at;
	  p->logcode = logcode;
	  p->buff = buff;
	  p->header = header;

	success = g_thread_pool_push (pool, p, &err);
	g_assert_no_error (err);
	g_assert_true (success);
	at += (loglen-12)*8;

  }
  printf("reach to end\n");
g_thread_pool_free (pool, FALSE, TRUE);
  fclose(fp);
  free(buff);
  return TRUE;	
}
  
int main(int argc, char *argv[]) {

   char * schema, *raw, *rule_file = NULL;;
   if( argc >= 3 ) {
      schema = argv[1];
	  raw = argv[2];
	  if( argc == 4 ) {
		rule_file = argv[3];
	  } 
	  /*
	  else {
		printf("Too many arguments supplied.\n");
		exit(-1);
	  }
	  */
   } else {
      printf("more argument expected.\n");
	  exit(-1);
   }

  if(rule_file) {
	rules = parse_rule(rule_file);
  }

  FILE *fp, *traceFile;
  long size;
  char *buff, *buff_end;
  
  size_t bytes;
  int token;
  Scanner scanner;
  void *parser;
  //mergefiles();
  /* Open input file */

  fp = fopen(schema, "rb");
  if(fp == NULL) {
    fprintf(stderr, "Can't open test file\n");
    exit(-1);
  }

  /* Open trace file */
 /*
  traceFile = fopen("trace.out", "w");
  if(traceFile == NULL) {
    fprintf(stderr, "Can't open trace file\n");
    exit(-1);
  }
  */

  /* Get file size */
  fseek(fp, 0, SEEK_END);
  size = ftell(fp);
  rewind(fp);

  /* Allocate buffer and read */
  buff = (char*) malloc(size * sizeof(char));
  bytes = fread(buff, 1, size, fp);  
  if (bytes != size) {
    fprintf(stderr, "Error reading input file\n");
    printf("bytes: %d, size: %d\n", bytes, size);
    exit(-1);
  }

  /* Initialize scanner */
  scanner.top = buff;
  scanner.cur = buff;
  scanner.pos = buff;  
  scanner.line = 1;
  init_scope(&scanner.root_scope, NULL);
  scanner.cs = &scanner.root_scope;
  /* scanner.root = g_node_new(NULL); */
  scanner.chunk = g_string_chunk_new (2048);
  
  /* Pointer to the end of the buffer */
  buff_end = (char*) (((char*)buff) + size);

  /* Create parser and set up tracing */
  parser = ParseAlloc(malloc);
  //ParseTrace(traceFile, "parser >> ");

  gchar* name_str;  
  while(token = scan(&scanner, buff_end)) {
    // Send strings to the parser with IDENTIFIER tokens
	if(token == IDENTIFIER || token == ID_TYPE || token == INT_LITERAL || token == FLOAT_LITERAL || token == STRING_LITERAL) {	
      name_str = g_string_chunk_insert_len (scanner.chunk, scanner.top, scanner.cur - scanner.top);	  
	  
	  //printf("name_str %s. token %d\n", name_str, token);	  
      Parse(parser, token, name_str, &scanner);
    }  else {
      Parse(parser, token, "", &scanner);
    }

    // Execute Parse for the last time
    if(token == END_TOKEN) {
      Parse(parser, 0, NULL, &scanner);
      break;
    }
  }

    /* Deallocate parser */
  ParseFree(parser, free);
  /* Close files and deallocate */
  fclose(fp);
  //fclose(traceFile);
  free(buff);

    /*traverse the tree*/
  decl = g_hash_table_new(g_str_hash, g_str_equal);
  event_decl = g_hash_table_new(g_direct_hash, g_direct_equal);
  g_node_children_foreach (scanner.root, G_TRAVERSE_ALL, (GNodeForeachFunc)node_build_ctx, NULL);
  write_decl(decl);
  write_events_decl(event_decl);
  parse_trace(raw);
  g_hash_table_unref(decl);
  g_hash_table_unref(event_decl); 
  printf("#################################\n");

  return(0);
}