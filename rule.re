#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "rule_grammar.h"
#include "rete.h"
#include "rule_ast.h"
#include <glib.h>

#define   YYCTYPE     char
#define   YYCURSOR    s->cur
#define   YYMARKER    s->ptr

/* functions to interface the lemon parser */
void *ruleAlloc();
void rule();
void ruleFree();
void ruleTrace();

typedef struct vp_ast* vpast;

int pscan(struct PathScanner* s, char *buff_end) {
  
regular:
  if (s->cur >= buff_end) {
    return END_TOKEN;
  }
  s->top = s->cur;

/*!re2c
  re2c:yyfill:enable = 0;
 
  ALPHANUMS = [a-zA-Z0-9]+;
  whitespace = [ \t\v\f]+;
  dig = [0-9];
  let = [a-zA-Z_];
  hex = [a-fA-F0-9];
  int_des = [uUlL]*;
  any = [\000-\377];
*/

/*!re2c
  "/*"                  { goto comment; }
  "."                   { return P_DOT; }
  "("                   { return P_LPAREN; }
  ")"                   { return P_RPAREN; }
  "{"                   { return P_LBRACE; }
  "}"                   { return P_RBRACE; }
  "["                   { return P_LBRACKET; }
  "]"                   { return P_RBRACKET; }  
  ";"                   { return P_SEMICOLON; }
  ","                   { return P_COMMA; }
  "?"                   { return P_QUESTION; }
  ":"                   { return P_COLON; }
  "~"                   { return P_TILDE; }  
  "$"                   { return P_DOLLAR; }
  "$$"                  { return P_DOLLARDOLLAR; }
  ":="                  { return P_BINDING; }
  "in"|"IN"             { return P_IN; }
  "contains"|"CONTAINS" { return P_CONTAINS; }
  "$sum"                { return P_SUM; }
  "$avg"                { return P_AVERAGE; }
  "$min"                { return P_MIN; }
  "$max"                { return P_MAX; }
  "+"                   { return P_ADD; }
  "-"                   { return P_SUB; }
  "*"                   { return P_STAR; }
  "**"                  { return P_STARSTAR; }  
  "/"                   { return P_DIV; }
  "%"                   { return P_MOD; }
  "=>"                  { return P_INDICATE; }
  "=="                  { return P_EQEQ; }
  "!="                  { return P_NOTEQ; }
  "<"                   { return P_LT; }
  ">"                   { return P_GT; }
  "<="                  { return P_LTEQ; }
  ">="                  { return P_GTEQ; }

  "!"                   { return P_NOT; }
	"&"                   { return P_AND; }
	"and"|"AND"			      { return P_ANDAND; }
	"or"|"OR"			        { return P_OROR; }  

  ">>"                  { return P_SR; }
  "<<"                  { return P_SL; }
  
  ["]([^"]+)["]         { return P_STRING_LITERAL; }
  [']([^']+)[']         { return P_STRING_LITERAL_SIGNLE; }
  let (let|dig)*        { return P_NAME; }
  whitespace            { goto regular; }

	("0" [xX] hex+ int_des?) | ("0" dig+ int_des?) | (dig+ int_des?)
  { return P_INT_LITERAL; }
  ("0"|([1-9] dig*))("." dig+)?([Ee][-+]?dig+)? { return P_FLOAT_LITERAL; }

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

GHashTable* build_rete(GNode *root);

GHashTable* parse_rule(char* rule_file) {
  FILE *fp, *traceFile;
  long size;
  char *buff, *buff_end;
  gchar* name_str;
  size_t bytes;
  int token;
  struct PathScanner scanner;
  void *parser;
  /* Open input file */
  fp = fopen(rule_file, "rb");
  if(fp == NULL) {
    fprintf(stderr, "Can't open test file\n");
    exit(-1);
  }

  /* Open trace file */
  traceFile = fopen("rule_trace.out", "w");
  if(traceFile == NULL) {
    fprintf(stderr, "Can't open trace file\n");
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
    exit(-1);
  }

  /* Initialize scanner */
  scanner.top = buff;
  scanner.cur = buff;
  scanner.pos = buff;  
  scanner.line = 1;
  scanner.chunk = g_string_chunk_new (2048);
  
  /* Initialize the parser state structure */
  
  /* Pointer to the end of the buffer */
  buff_end = (char*) (((char*)buff) + size);

  /* Create parser and set up tracing */
  parser = ruleAlloc(malloc);
  ruleTrace(traceFile, "parser >> ");

  while(token = pscan(&scanner, buff_end)) {
    
    // Send strings to the parser with NAME tokens
    if(token == P_NAME || token == P_STRING_LITERAL || token == P_STRING_LITERAL_SIGNLE || token == P_INT_LITERAL || token == P_FLOAT_LITERAL) {
      name_str = g_string_chunk_insert_len (scanner.chunk, scanner.top, scanner.cur - scanner.top);	 
      rule(parser, token, name_str, &scanner);
      //printf("%s\n", name_str);
    } 
    else {
      if(token == END_TOKEN) {
        rule(parser, token, "", &scanner);    
        // Execute Parse for the last time  
        rule(parser, 0, NULL, &scanner);
        break;
      } else {
        rule(parser, token, "", &scanner);
      }
    }
  }

  /* Deallocate parser */
  ruleFree(parser, free);

  /* Close files and deallocate */
  fclose(fp);
  fclose(traceFile);
  free(buff);

  /* Print results of parsing */
  GHashTable *h = build_rete(scanner.root);
  return h;
}

static void debugbreak(){
	printf("break\n");
}

gboolean condition_equal(struct condition_node *a, struct condition_node *b) {
  if(g_string_equal(a->left->path, b->left->path) && a->type == b->type && 
  g_variant_equal(a->right, b->right)) {
    return TRUE;
  }
  return FALSE;
}

GVariant *getvariant(GNode *nd) {
  GVariant *ret = NULL;
  vpast a = (vpast)(nd->data);
  g_assert(a->type == VP_UNARY_EXPRESSION && (a->u.unary_expression.type == VP_UNARY_SIGNED_CONSTANT || a->u.unary_expression.type == VP_UNARY_UNSIGNED_CONSTANT
  || a->u.unary_expression.type == VP_UNARY_FLOAT_CONSTANT || a->u.unary_expression.type == VP_UNARY_LITERAL || 
  a->u.unary_expression.type == VP_UNARY_COLLECTION));  
  switch(a->u.unary_expression.type){
    case VP_UNARY_SIGNED_CONSTANT:
    {
      ret = g_variant_new_int64(a->u.unary_expression.u.signed_constant);
      break;
    }
    case VP_UNARY_UNSIGNED_CONSTANT:
    {
      ret = g_variant_new_int64(a->u.unary_expression.u.signed_constant);
      break;
    }
    case VP_UNARY_FLOAT_CONSTANT:
    {
      ret = g_variant_new_double(a->u.unary_expression.u.float_constant);
      break;
    }
    case VP_UNARY_LITERAL:
    {
      ret = g_variant_new_string(a->u.unary_expression.u.string);
      break;
    }
    case VP_UNARY_COLLECTION:
    {
      GNode *child;
      GVariantBuilder builder;
      g_variant_builder_init (&builder, G_VARIANT_TYPE_ARRAY);      
      child = nd->children;
      while (child)
      {
        g_variant_builder_add_value (&builder, getvariant(child));
        child = child->next;
      }   
      ret = g_variant_builder_end (&builder);
      break;
    }
    default:    
      break;
  }
  return ret;
}

char* get_unary_name(vpast a) {
  g_assert(a->type == VP_UNARY_EXPRESSION && (a->u.unary_expression.type == VP_UNARY_STRING || a->u.unary_expression.type == VP_UNARY_LITERAL_SINGLE
  || a->u.unary_expression.type == VP_UNARY_ROOT));
  if(a->u.unary_expression.type == VP_UNARY_STRING || a->u.unary_expression.type == VP_UNARY_LITERAL_SINGLE) 
    return a->u.unary_expression.u.string;
  else
    return "$$";
}

GString *getpath(GNode *nd) {
  GNode *child;
  GString *path;
  char* name;
  child = nd;
  vpast a = (vpast)(child->data);
  name =  get_unary_name(a);
  path = g_string_new (name);
	while(child) {
    a = (vpast)(child->data);
    g_string_append_c(path, '/'+get_unary_name(a));
	  if(G_NODE_IS_LEAF(child)) {      
		  break;
    } else {
		  child = g_node_first_child(child);
    }
	}
  return path;  
}

GHashTable* build_rete(GNode *root) {
    GNode *event_node, *rule_node, *condition_nd, *left, *right;
    GHashTable *h;
    h = g_hash_table_new (g_direct_hash, g_direct_equal);
    for(guint i=0;i< g_node_n_children(root);i++) {
      event_node = g_node_nth_child(root, i);     
      if(event_node) { 
        vpast e = (vpast)(event_node->data); 
        g_assert(e->type == VP_EVENT_EXPRESSION);
        uint64_t ec = e->u.event.code;
        GSList *alpha_nodes = NULL;
        GSList *beta_nodes = NULL;
        for(guint j=0;j< g_node_n_children(event_node);j++) {
          rule_node = g_node_nth_child(event_node, j);
          vpast r = (vpast)(rule_node->data); 
          /*last node is the expression, others are conditions*/
          guint k = g_node_n_children(rule_node);
          g_assert(k >= 1);

          struct beta_node *b = g_new0(struct beta_node, 1);
          b->name = r->u.rule.name;
          b->enabled = true;
          b->alpha_nodes = NULL;
          b->expression_node = g_node_nth_child(rule_node, k-1);
          beta_nodes = g_slist_append(beta_nodes,b);

          if(k > 1) {
            b->is_default = false;
            for(guint m=0;m<k-1;m++) {
              condition_nd = g_node_nth_child(rule_node, m);
              vpast bc = (vpast)(condition_nd->data);
              g_assert(bc->type == VP_FILTER);
              left = g_node_first_child(condition_nd);
              right = g_node_last_child(condition_nd);

              struct unary_node *lun = g_new0(struct unary_node, 1);
              lun->path = getpath(left);
              lun->node = left;

              struct condition_node * cn = g_new0(struct condition_node, 1);
              cn->left = lun;
              cn->right = getvariant(right);
              cn->type = bc->u.filter.type;

              gboolean found = FALSE;
              for (GSList *l = alpha_nodes; l != NULL; l = l->next)
              {
                struct alpha_node *a = l->data;
                struct condition_node *cd = a->condition;
                if(strcmp(cn->left->path->str, cd->left->path->str) == 0 && g_variant_equal(cn->right, cd->right) &&
                cn->type == cd->type) {
                  found = TRUE;
                  a->beta_nodes = g_slist_append(a->beta_nodes, b);
                  break;
                }
              }
              if(!found) {
                struct alpha_node *a = g_new0(struct alpha_node, 1);
                a->condition = cn;
                a->enabled = true;
                a->beta_nodes = NULL;
                a->beta_nodes = g_slist_append(a->beta_nodes, b);

                alpha_nodes = g_slist_append(alpha_nodes,a);
              }

            }
          } else {
            b->is_default = true;
          }
        }
        struct rule_node *rn = g_new0(struct rule_node, 1);
        rn->name = e->u.event.name;
        rn->alpha = alpha_nodes;
        rn->beta = beta_nodes;

        g_hash_table_insert(h, GUINT_TO_POINTER(ec), rn);
      }
    }
    return h;
}

GVariant *compute_math(GVariant *left, enum vp_math_type type, GVariant *right) {
    const GVariantType *top;
    GVariant *left_t;
    if(type == VP_MATH_CONCAT) {
      top = G_VARIANT_TYPE_STRING;
      left_t = left;
    } else {
      if(g_variant_is_container(left)) {						
        if(g_variant_is_of_type(right, G_VARIANT_TYPE_STRING)) {
          left_t = g_variant_get_child_value(left, 0);
        } else {
          left_t = g_variant_get_child_value(left, 1);
        }           
			} else {
        left_t = left;
      }

      top = top_type(g_variant_get_type(left_t), g_variant_get_type(right));
    }
    
    GVariant *a = cast_to(left_t, top);
    GVariant *b = cast_to(right, top);
    GVariant *c;
    switch(type) {
      case VP_MATH_CONCAT:
      {
        const char* astr = g_variant_get_string(a, NULL);
        const char* bstr = g_variant_get_string(b, NULL);
        char* cstr = g_strconcat(astr, bstr, NULL);
        c = g_variant_new_string(cstr);
        g_free(cstr);
        break;
      }
      case VP_MATH_ADD:
      {
        if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
          c = g_variant_new_int64(g_variant_get_int64(a) + g_variant_get_int64(b));
        } else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
          c = g_variant_new_double(g_variant_get_double(a) + g_variant_get_double(b));
        } else {
          printf("top type not supported\n");
        }          
        break;
      }
      case VP_MATH_SUB:
      {
        if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
          c = g_variant_new_int64(g_variant_get_int64(a) - g_variant_get_int64(b));
        } else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
          c = g_variant_new_double(g_variant_get_double(a) - g_variant_get_double(b));
        } else {
          printf("top type not supported\n");
        }          
        break;
      }
      case VP_MATH_MULTIPLY:
      {
        if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
          c = g_variant_new_int64(g_variant_get_int64(a) * g_variant_get_int64(b));
        } else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
          c = g_variant_new_double(g_variant_get_double(a) * g_variant_get_double(b));
        } else {
          printf("top type not supported\n");
        }        
        break;
      }
      case VP_MATH_DIV:
      {
        if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
          c = g_variant_new_double(g_variant_get_int64(a) / g_variant_get_int64(b));
        } else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
          c = g_variant_new_double(g_variant_get_double(a) / g_variant_get_double(b));
        } else {
          printf("top type not supported\n");
        }              
        break;
      }
      case VP_MATH_MOD:
      {
        if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
          c = g_variant_new_int64(g_variant_get_int64(a) % g_variant_get_int64(b));
        } else {
          printf("top type not supported\n");
        }            
        break;
      }
      case VP_MATH_SL:
      {
        if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
          c = g_variant_new_int64(g_variant_get_int64(a) << g_variant_get_int64(b));
        } else {
          printf("top type not supported\n");
        }         
        break;
      }
      case VP_MATH_SR:
      {
        if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
          c = g_variant_new_int64(g_variant_get_int64(a) >> g_variant_get_int64(b));
        } else {
          printf("top type not supported\n");
        }          
        break;
      }
      case VP_MATH_LT:
      {
        if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
          c = g_variant_new_boolean(g_variant_get_int64(a) < g_variant_get_int64(b));
        } else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
          c = g_variant_new_boolean(g_variant_get_double(a) < g_variant_get_double(b));
        } else {
          printf("top type not supported\n");
        }         
        break;
      }
      case VP_MATH_GT:
      {
        if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
          c = g_variant_new_boolean(g_variant_get_int64(a) > g_variant_get_int64(b));
        } else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
          c = g_variant_new_boolean(g_variant_get_double(a) > g_variant_get_double(b));
        } else {
          printf("top type not supported\n");
        }             
        break;
      }
      case VP_MATH_LTEQ:
      {
        if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
          c = g_variant_new_boolean(g_variant_get_int64(a) <= g_variant_get_int64(b));
        } else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
          c = g_variant_new_boolean(g_variant_get_double(a) <= g_variant_get_double(b));
        } else {
          printf("top type not supported\n");
        }        
        break;
      }
      case VP_MATH_GTEQ:
      {
        if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
          c = g_variant_new_boolean(g_variant_get_int64(a) >= g_variant_get_int64(b));
        } else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
          c = g_variant_new_boolean(g_variant_get_double(a) >= g_variant_get_double(b));
        } else {
          printf("top type not supported\n");
        }           
        break;
      }
      case VP_MATH_EQEQ:
      {
        if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
          c = g_variant_new_boolean(g_variant_get_int64(a) == g_variant_get_int64(b));
        } else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
          c = g_variant_new_boolean(g_variant_get_double(a) == g_variant_get_double(b));
        } else if(g_variant_type_equal(top, G_VARIANT_TYPE_BOOLEAN)) {
          c = g_variant_new_boolean(g_variant_get_boolean(a) == g_variant_get_boolean(b));
        } else if(g_variant_type_equal(top, G_VARIANT_TYPE_STRING)) {
          c = g_variant_new_boolean(strcmp(g_variant_get_string(a, NULL), g_variant_get_string(b, NULL)) == 0? TRUE:FALSE);          
        } else {
          printf("top type not supported\n");
        }         
        break;
      }
      case VP_MATH_NOTEQ:
      {
        if(g_variant_type_equal(top, G_VARIANT_TYPE_INT64)) {
          c = g_variant_new_boolean(g_variant_get_int64(a) != g_variant_get_int64(b));
        } else if(g_variant_type_equal(top, G_VARIANT_TYPE_DOUBLE)) {
          c = g_variant_new_boolean(g_variant_get_double(a) != g_variant_get_double(b));
        } else if(g_variant_type_equal(top, G_VARIANT_TYPE_BOOLEAN)) {
          c = g_variant_new_boolean(g_variant_get_boolean(a) != g_variant_get_boolean(b));
        } else if(g_variant_type_equal(top, G_VARIANT_TYPE_STRING)) {
          c = g_variant_new_boolean(strcmp(g_variant_get_string(a, NULL), g_variant_get_string(b, NULL)) == 0? FALSE:TRUE);           
        } else {
          printf("top type not supported\n");
        }         
        break;
      }
      case VP_MATH_ANDAND:
      {
        if(g_variant_type_equal(top, G_VARIANT_TYPE_BOOLEAN)) {
          c = g_variant_new_boolean(g_variant_get_boolean(a) && g_variant_get_boolean(b));
        } else {
          printf("top type not supported\n");
        }         
        break;
      }
      case VP_MATH_OROR:
      {
        if(g_variant_type_equal(top, G_VARIANT_TYPE_BOOLEAN)) {
          c = g_variant_new_boolean(g_variant_get_boolean(a) || g_variant_get_boolean(b));
        } else {
          printf("top type not supported\n");
        }          
        break;
      }
      default:
        break;
    }
    return c;
}

GVariant *evaluate_expression(GNode *nd, GVariant *v);
GVariant *recursive_concat(GNode *nd, GVariant *v);
GVariant *evaluate_math(GNode *nd, enum vp_math_type type, GVariant *v);

GVariant *
iterate_container_recursive (GVariant *v, char* name, GNode *nd)
{
  GVariantIter iter;
  GVariant *child, *value, *outp, *tmp;
  gchar *key;
  gboolean found = FALSE;
  g_variant_iter_init (&iter, v);  

  const GVariantType *childType = g_variant_type_element(g_variant_get_type(v));
  if(g_variant_type_is_array(childType)) {
    GVariantBuilder builder;
    g_variant_builder_init (&builder, G_VARIANT_TYPE_ARRAY);
    while ((child = g_variant_iter_next_value (&iter)))
    {
      tmp = iterate_container_recursive(child, name, nd);
      if(tmp)
        g_variant_builder_add_value (&builder, tmp);
      g_variant_unref (child);
    }
    return g_variant_builder_end (&builder);
  } else if(g_variant_type_is_dict_entry(childType)) {
    while (g_variant_iter_next (&iter, "{sv}", &key, &value))
    {
      if(strcmp(key, name) == 0) {
        if(nd->children)
          outp = evaluate_expression(g_node_first_child(nd), value);
        else {
          /*unbox the child and return the variant*/
          outp = value;
          #ifdef DEBUG
          outp = g_variant_get_child_value(outp, 0);
          outp = g_variant_get_variant(g_variant_get_child_value(outp, 1));      
          if(g_variant_type_is_variant(g_variant_get_type(outp)))
            outp = g_variant_get_variant(outp);           
          #endif         
        }
        found = TRUE;
        break;
      }

      // must free data for ourselves
      g_variant_unref (value);
      g_free (key);
    } 
    if(!found) {
      printf("error not found in array %s\n", name);
      return NULL;          
    } else {
      return outp;
    }    
  } else {
        printf("error container type\n");
        return NULL;
  }
}

GVariant *
iterate_array_recursive (GVariant *v, GNode *nd)
{
  GVariantIter iter;
  GVariant *child, *tmp;
  GVariant *left, *right, *reduce;  

  g_variant_iter_init (&iter, v);  
  const GVariantType *childType = g_variant_type_element(g_variant_get_type(v));
  if(g_variant_type_is_array(childType)) {
    GVariantBuilder builder;
    g_variant_builder_init (&builder, G_VARIANT_TYPE_ARRAY);
    while ((child = g_variant_iter_next_value (&iter)))
    {
      tmp = iterate_array_recursive(child, nd);
      if(tmp)
        g_variant_builder_add_value (&builder, tmp);
      g_variant_unref (child);
    }
    return g_variant_builder_end (&builder);
  } else if(g_variant_type_is_dict_entry(childType)) {

      GNode *next = nd;
      GVariantBuilder builder;
      g_variant_builder_init (&builder, G_VARIANT_TYPE_ARRAY);         
      while(next = g_node_next_sibling(next)) {
        vpast nx = (vpast)(next->data);
        g_assert(nx->type == VP_REDUCE);
        GNode* leftnd = g_node_first_child(next);
        GNode* rightnd = g_node_last_child(next);
        const char* leftname;

        //left is a string
        left = recursive_concat(leftnd, v);
        leftname = g_variant_get_string(left, NULL);        

        //right is an unary or math expression
        vpast nr = (vpast)(rightnd->data);
        if(nr->type == VP_UNARY_EXPRESSION){    
          right = evaluate_expression(rightnd, v);
        } else if(nr->type == VP_MATH_EXPRESSION) {
          right = evaluate_math(rightnd, nr->u.math_expression.type, v);
        } else {
          printf("reduce not supported\n");
          return NULL;
          break;          
        }

        reduce = g_variant_new("{sv}", leftname, right);       
        g_variant_builder_add_value (&builder, reduce);    
      }
      return g_variant_builder_end(&builder);
  } else {
      printf("error array type\n");
      return NULL;
  }
}

GVariant *recursive_concat(GNode *nd, GVariant *v) {
  if(!nd->children) {
    vpast ast = (vpast)(nd->data);
    g_assert(ast->type == VP_UNARY_EXPRESSION);
    GVariant *cg = evaluate_expression(nd, v);
    GVariant *cgstr = cast_to(cg, G_VARIANT_TYPE_STRING); 
    return cgstr;
  } else {
    GNode *left = g_node_first_child(nd);
    GNode *right = g_node_last_child(nd);
    GVariant *lstr, *rstr, *outstr, *ltmp, *rtmp, *childleft;
    const char *lstring, *rstring;
    //right is a unary
    vpast rx = (vpast)(right->data);
    g_assert(rx->type == VP_UNARY_EXPRESSION);
    
    rtmp = evaluate_expression(right, v);
    rstr = cast_to(rtmp, G_VARIANT_TYPE_STRING); 
    rstring = g_variant_get_string(rstr, NULL);   
    
    vpast lx = (vpast)(left->data);
    if(lx->type == VP_UNARY_EXPRESSION) {
      ltmp = evaluate_expression(left, v);
      lstr = cast_to(ltmp, G_VARIANT_TYPE_STRING);
      lstring = g_variant_get_string(lstr, NULL);
      outstr = g_variant_new_string(g_strconcat(lstring, rstring, NULL));
    } else if(lx->type == VP_MATH_EXPRESSION && lx->u.math_expression.type == VP_MATH_CONCAT) {
      childleft = recursive_concat(left, v);
      lstring = g_variant_get_string(childleft, NULL);
      outstr = g_variant_new_string(g_strconcat(lstring, rstring, NULL));
    } else {
      printf("concat type not supported\n");
      return NULL;
    }   
    return outstr;
  }
  
}

GVariant *evaluate_math(GNode *nd, enum vp_math_type type, GVariant *v) {
  if(!v)
    return NULL;
  switch(type){
    case VP_MATH_ADD:
    case VP_MATH_SUB:
    case VP_MATH_MULTIPLY:
    case VP_MATH_DIV:
    case VP_MATH_MOD:
    case VP_MATH_SL:
    case VP_MATH_SR:
    case VP_MATH_LT:
    case VP_MATH_GT:
    case VP_MATH_LTEQ:
    case VP_MATH_GTEQ:
    case VP_MATH_EQEQ:
    case VP_MATH_NOTEQ:         
    {
      GVariant *left = evaluate_expression(g_node_first_child(nd), v);
      GVariant *right = evaluate_expression(g_node_last_child(nd), v);
      return compute_math(left, type, right);
    }
    case VP_MATH_ANDAND:
    case VP_MATH_OROR: 
    {
      GNode *leftnd = g_node_first_child(nd);
      GNode *rightnd = g_node_last_child(nd);
      vpast lnd = (vpast)leftnd->data;
      vpast rnd = (vpast)rightnd->data;
      g_assert(lnd->type == VP_MATH_EXPRESSION);
      g_assert(rnd->type == VP_MATH_EXPRESSION);
      GVariant *leftout = evaluate_math(leftnd, lnd->u.math_expression.type, v);
      GVariant *rightout = evaluate_math(rightnd, rnd->u.math_expression.type, v);

      if(type == VP_MATH_ANDAND) {
        return g_variant_new_boolean(g_variant_get_boolean(leftout) && g_variant_get_boolean(rightout));
      } else {
        return g_variant_new_boolean(g_variant_get_boolean(leftout) || g_variant_get_boolean(rightout));
      }
      break;
    }
    case VP_MATH_CONDITIONAL:
    {
      GNode *outnd;
      GNode *leftnd = g_node_first_child(nd);
      vpast lnd = (vpast)leftnd->data;
      g_assert(lnd->type == VP_MATH_EXPRESSION);
      GVariant *leftout = evaluate_math(leftnd, lnd->u.math_expression.type, v);
      g_assert(g_variant_type_equal(g_variant_get_type(leftout),G_VARIANT_TYPE_BOOLEAN));
      if(g_variant_get_boolean(leftout)) {
        //process middle
        outnd = leftnd->next;
      } else {
        //process right
        outnd = g_node_last_child(nd);
      }

      vpast b = (vpast)outnd->data;
      if(b->type == VP_UNARY_EXPRESSION) {
        return evaluate_expression(outnd, v);
      } else if(b->type == VP_MATH_EXPRESSION) {
        return evaluate_math(outnd, b->u.math_expression.type, v);
      } else {
        printf("not supported\n");
        return NULL;            
      }
      break;
    }    
    default:
    {
      printf("not implemented\n");
      return NULL;          
    }
  }
}

GVariant *evaluate_expression(GNode *nd, GVariant *v) {
  if(!v)
    return NULL;
  vpast a = (vpast)(nd->data);
  g_assert(a->type == VP_UNARY_EXPRESSION);
  switch(a->u.unary_expression.type) 
  {
    case VP_UNARY_SIGNED_CONSTANT:
    {
      return g_variant_new_int64(a->u.unary_expression.u.signed_constant);
      break;
    }
    case VP_UNARY_UNSIGNED_CONSTANT:
    {
      return g_variant_new_int64(a->u.unary_expression.u.unsigned_constant);
      break;
    }
    case VP_UNARY_FLOAT_CONSTANT:
    {
      return g_variant_new_double(a->u.unary_expression.u.float_constant);
      break;
    }
    case VP_UNARY_COLLECTION:
    {
      GNode *child;
      GVariantBuilder builder;
      g_variant_builder_init (&builder, G_VARIANT_TYPE_ARRAY);      
      child = nd->children;
      while (child)
      {
        g_variant_builder_add_value (&builder, evaluate_expression(child, v));
        child = child->next;
      }   
      return g_variant_builder_end (&builder);
      break;
    }
    case VP_UNARY_SUM:
    {
      GVariantIter iter;
      GVariant *sumv, *child;
      float f;

      GNode *sumnd = g_node_first_child(nd);
      vpast nx = (vpast)(sumnd->data);
      g_assert(nx->type == VP_UNARY_EXPRESSION);

      sumv = evaluate_expression(sumnd, v);
      g_assert(g_variant_type_is_array(g_variant_get_type(sumv)));

      g_variant_iter_init (&iter, sumv);
      while ((child = g_variant_iter_next_value (&iter)))
      {
        GVariant *ctmp = cast_to(child, G_VARIANT_TYPE_DOUBLE);
        f += g_variant_get_double(ctmp);
        g_variant_unref (child);
      }  
      g_variant_unref (sumv);
      return g_variant_new_double(f);    
      break;
    }
    case VP_UNARY_AVERAGE:
    {
      GVariantIter iter;
      GVariant *avgv, *child;
      float f;
      int i;

      GNode *avgnd = g_node_first_child(nd);
      vpast nx = (vpast)(avgnd->data);
      g_assert(nx->type == VP_UNARY_EXPRESSION);

      avgv = evaluate_expression(avgnd, v);
      g_assert(g_variant_type_is_array(g_variant_get_type(avgv)));

      g_variant_iter_init (&iter, avgv);
      while ((child = g_variant_iter_next_value (&iter)))
      {
        GVariant *ctmp = cast_to(child, G_VARIANT_TYPE_DOUBLE);
        f += g_variant_get_double(ctmp);
        g_variant_unref (child);
        i++;
      }  
      g_variant_unref (avgv);
      return g_variant_new_double(f/i);          
      break;
    }
    case VP_UNARY_MIN:
    {
      GVariantIter iter;
      GVariant *minv, *child;
      float f;
      gboolean is_initial = FALSE;
      const GVariantType *topt;

      GNode *minnd = g_node_first_child(nd);
      vpast nx = (vpast)(minnd->data);
      g_assert(nx->type == VP_UNARY_EXPRESSION);

      minv = evaluate_expression(minnd, v);
      g_assert(g_variant_type_is_array(g_variant_get_type(minv)));
      g_variant_iter_init (&iter, minv);
      while ((child = g_variant_iter_next_value (&iter)))
      {
        if(g_variant_type_equal(g_variant_get_type(child), G_VARIANT_TYPE_DOUBLE))
          topt = G_VARIANT_TYPE_DOUBLE;
        GVariant *ctmp = cast_to(child, G_VARIANT_TYPE_DOUBLE);
        if(is_initial) {
          f = f < g_variant_get_double(ctmp) ? f : g_variant_get_double(ctmp);
        } else {
          f = g_variant_get_double(ctmp);
          is_initial = TRUE;
        }
        
        g_variant_unref (child);
      }  
      g_variant_unref (minv);
      if(topt && g_variant_type_equal(topt, G_VARIANT_TYPE_DOUBLE)) {
        return g_variant_new_double(f);       
      } else {
        return g_variant_new_int64((int)f);
      }
       
      break;
    }
    case VP_UNARY_MAX:
    {
      GVariantIter iter;
      GVariant *maxv, *child;
      float f;
      gboolean is_initial = FALSE;
      const GVariantType *topt = NULL;

      GNode *maxnd = g_node_first_child(nd);
      vpast nx = (vpast)(maxnd->data);
      g_assert(nx->type == VP_UNARY_EXPRESSION);

      maxv = evaluate_expression(maxnd, v);
      g_assert(g_variant_type_is_array(g_variant_get_type(maxv)));

      g_variant_iter_init (&iter, maxv);
      while ((child = g_variant_iter_next_value (&iter)))
      {
        if(g_variant_type_equal(g_variant_get_type(child), G_VARIANT_TYPE_DOUBLE))
          topt = G_VARIANT_TYPE_DOUBLE;
        GVariant *ctmp = cast_to(child, G_VARIANT_TYPE_DOUBLE);
        if(is_initial) {
          f = f > g_variant_get_double(ctmp) ? f : g_variant_get_double(ctmp);
        } else {
          f = g_variant_get_double(ctmp);
          is_initial = TRUE;
        }
        
        g_variant_unref (child);
      }  
      g_variant_unref (maxv);
      if(topt && g_variant_type_equal(topt, G_VARIANT_TYPE_DOUBLE)) {
        return g_variant_new_double(f);       
      } else {
        return g_variant_new_int64((int)f);
      }
      break;
    }
    case VP_UNARY_PARENTHESES:
    {
      GNode *next = g_node_next_sibling(nd);
      vpast nx = (vpast)(next->data);
      if(nx->type == VP_UNARY_EXPRESSION){
        return evaluate_expression(next, v);
      } else if(nx->type == VP_MATH_EXPRESSION) {
        return evaluate_math(next, nx->u.math_expression.type, v);
      } else {
        printf("Unkown\n");
        return NULL;
      }     
      break;
    }
    case VP_UNARY_BRACE:
    {
      GNode *next = nd;
      GVariant *left, *right, *reduce;   
      GVariantBuilder builder;
      g_variant_builder_init (&builder, G_VARIANT_TYPE_ARRAY);         
      while(next = g_node_next_sibling(next)) {
        vpast nx = (vpast)(next->data);
        g_assert(nx->type == VP_REDUCE);
        GNode* leftnd = g_node_first_child(next);
        GNode* rightnd = g_node_last_child(next);
        const char* leftname;

        //left is a string
        left = recursive_concat(leftnd, v);
        leftname = g_variant_get_string(left, NULL);        

        //right is an unary or math expression
        vpast nr = (vpast)(rightnd->data);
        if(nr->type == VP_UNARY_EXPRESSION){    
          right = evaluate_expression(rightnd, v);
        } else if(nr->type == VP_MATH_EXPRESSION) {
          right = evaluate_math(rightnd, nr->u.math_expression.type, v);
        } else {
          printf("reduce not supported\n");
          return NULL;
          break;          
        }

        reduce = g_variant_new("{sv}", leftname, right);       
        g_variant_builder_add_value (&builder, reduce);    
      }
      return g_variant_builder_end(&builder);
    }
    case VP_UNARY_ARRAY:
    {
      g_assert(g_variant_type_is_array(g_variant_get_type(v)));
      return iterate_array_recursive(v, nd);
    }    
    case VP_UNARY_SBRAC:
    {
      /*parse the next node, node type maybe filter or select*/
      g_assert(g_variant_type_is_array(g_variant_get_type(v)));
      GNode *next = g_node_next_sibling(nd);
      vpast nx = (vpast)(next->data);
      if(nx->type == VP_UNARY_EXPRESSION){
        /*select from array*/
        GVariant *ret = evaluate_expression(next, v);
        GVariant *tmp = cast_to(ret,G_VARIANT_TYPE_INT64);
        if(tmp) {
          gint64 ind = g_variant_get_int64(tmp);
          if(ind>=0 && ind <g_variant_n_children(v)) {
            return evaluate_expression(g_node_first_child(nd), g_variant_get_child_value(v, ind));
          } else {
            printf("size must be positive and less than the actual size\n");
            return NULL;
          }
        } else {
          printf("not a valid size of an array\n");
          return NULL;
        }
      } else if(nx->type == VP_MATH_EXPRESSION) {
        /*if logical then filter, otherwise select*/
        switch(nx->u.math_expression.type){
          case VP_MATH_ADD:
          case VP_MATH_SUB:
          case VP_MATH_MULTIPLY:
          case VP_MATH_DIV:
          case VP_MATH_MOD:
          case VP_MATH_SL:
          case VP_MATH_SR:
          case VP_MATH_CONDITIONAL:
          {
            /*GVariant *left = evaluate_expression(g_node_first_child(next), v);
            GVariant *right = evaluate_expression(g_node_last_child(next), v);
            GVariant *x = compute_math(left, nx->u.math_expression.type, right);*/
            GVariant *x = evaluate_math(next, nx->u.math_expression.type, v);
            if(x) {
              int64_t i64t;
              const GVariantType *x_type = g_variant_get_type(x);
              if(g_variant_type_equal(x_type, G_VARIANT_TYPE_INT64)) {
                i64t = g_variant_get_int64(x);            
              } else {
                printf("top type not supported\n");
                return NULL;
              }
              if(i64t >= 0 && i64t <g_variant_n_children(v)) {
                return evaluate_expression(g_node_first_child(nd), g_variant_get_child_value(v, i64t));
              } else {
                printf("size must be positive and less than the actual size\n");
                return NULL;
              }                                
            } else {
              printf("not a valid index value\n");
              return NULL;
            }           
            break;
          }
          case VP_MATH_LT:
          case VP_MATH_GT:
          case VP_MATH_LTEQ:
          case VP_MATH_GTEQ:
          case VP_MATH_EQEQ:
          case VP_MATH_NOTEQ:
          case VP_MATH_IN:
          case VP_MATH_CONTANINS:         
          {
            GVariantIter iter;
            GVariant *child, *left, *right, *outb, *tempt;
            const GVariantType *b_type;
            GVariantBuilder builder;
            gboolean is_empty = TRUE;
            g_variant_builder_init (&builder, G_VARIANT_TYPE_ARRAY);
            g_variant_iter_init (&iter, v);
            while ((child = g_variant_iter_next_value (&iter)))
              {                
                left = evaluate_expression(g_node_first_child(next), child);
                right = evaluate_expression(g_node_last_child(next), child);  
                

                //printf("left type %s, right type %s\n", g_variant_get_type_string(left), g_variant_get_type_string(right));  
                if(nx->u.math_expression.type == VP_MATH_IN) {
                  g_assert(g_variant_type_is_array(g_variant_get_type(right)));
                  GVariantIter in_iter;
                  GVariant *in_child, *in_tmp;
                  gboolean in_found = FALSE;
                  g_variant_iter_init (&in_iter, right);
                  while ((in_child = g_variant_iter_next_value (&in_iter)))
                  {
                    in_tmp = compute_math(left, VP_MATH_EQEQ, in_child);
                    if(g_variant_get_boolean(in_tmp)) {
                      in_found = TRUE;
                      g_variant_unref (in_tmp);
                      g_variant_unref (in_child);                      
                      break;
                    }
                    g_variant_unref (in_tmp);
                    g_variant_unref (in_child);
                  }
                  if(in_found) {
                    is_empty = FALSE;
                    g_variant_builder_add_value (&builder, child);
                  } 
              } else if(nx->u.math_expression.type == VP_MATH_CONTANINS) {
                  g_assert(g_variant_type_is_array(g_variant_get_type(left)));
                  GVariantIter in_iter;
                  GVariant *in_child, *in_tmp;
                  gboolean in_found = FALSE;
                  g_variant_iter_init (&in_iter, left);
                  while ((in_child = g_variant_iter_next_value (&in_iter)))
                  {
                    in_tmp = compute_math(in_child, VP_MATH_EQEQ, right);
                    if(g_variant_get_boolean(in_tmp)) {
                      in_found = TRUE;
                      g_variant_unref (in_tmp);
                      g_variant_unref (in_child);                      
                      break;
                    }
                    g_variant_unref (in_tmp);
                    g_variant_unref (in_child);
                  }
                  if(in_found) {
                    is_empty = FALSE;
                    g_variant_builder_add_value (&builder, child);
                  }                   
                } else {
                  outb = compute_math(left, nx->u.math_expression.type, right);
                  b_type = g_variant_get_type(outb);
                  if(g_variant_type_equal(b_type, G_VARIANT_TYPE_BOOLEAN)) {
                    if(g_variant_get_boolean(outb)) {
                      is_empty = FALSE;
                      g_variant_builder_add_value (&builder, child);
                      //printf("child type %s\n", g_variant_get_type_string(child));  
                    }           
                  } else {
                    printf("not a boolean type\n");
                    return NULL;
                  }                      
                }               
                                          
                g_variant_unref (child);
              }
              if(!is_empty)
                tempt = g_variant_builder_end (&builder);
              else
                return NULL;
            //printf("tempt type %s\n", g_variant_get_type_string(tempt));  
            
            if(g_variant_type_is_array(g_variant_get_type(tempt))) {
              GVariantIter xiter;
              GVariant *xchild;
              GVariantBuilder xbuilder;
              g_variant_builder_init (&xbuilder, G_VARIANT_TYPE_ARRAY);     
              g_variant_iter_init (&xiter, tempt);
              while ((xchild = g_variant_iter_next_value (&xiter)))
              {
                g_variant_builder_add_value (&xbuilder, evaluate_expression(g_node_first_child(nd), xchild));     
                g_variant_unref (xchild);
              }  
              return g_variant_builder_end(&xbuilder);      
            } else {
              return evaluate_expression(g_node_first_child(nd), tempt);
            }
            break;
          } 
          case VP_MATH_ANDAND:
          case VP_MATH_OROR: 
          {
            GVariantIter iter;
            GVariant *child, *outb, *tempt;
            GVariantBuilder builder;
            g_variant_builder_init (&builder, G_VARIANT_TYPE_ARRAY);
            g_variant_iter_init (&iter, v);
            gboolean is_empty = TRUE;
            while ((child = g_variant_iter_next_value (&iter)))
              { 
                if(nx->u.math_expression.type == VP_MATH_ANDAND || nx->u.math_expression.type == VP_MATH_OROR) {
                  outb = evaluate_math(next, nx->u.math_expression.type, child);
                  if(g_variant_get_boolean(outb)) {
                    g_variant_builder_add_value (&builder, child);
                    if(is_empty) {
                      is_empty = FALSE;
                    }
                  }
                  g_variant_unref (outb);
                }
                g_variant_unref (child);
              }
            if(!is_empty) {
              tempt =  g_variant_builder_end(&builder);
              //printf("tempt type %s\n", g_variant_get_type_string(tempt));  
              if(g_variant_type_is_array(g_variant_get_type(tempt))) {
                GVariantIter xiter;
                GVariant *xchild;
                GVariantBuilder xbuilder;
                g_variant_builder_init (&xbuilder, G_VARIANT_TYPE_ARRAY);     
                g_variant_iter_init (&xiter, tempt);
                while ((xchild = g_variant_iter_next_value (&xiter)))
                {
                  g_variant_builder_add_value (&xbuilder, evaluate_expression(g_node_first_child(nd), xchild));     
                  g_variant_unref (xchild);
                }  
                return g_variant_builder_end(&xbuilder);      
              } else {
                return evaluate_expression(g_node_first_child(nd), tempt);
              } 
            } else {
              return NULL;
            } 
                          
            break;
          }                           
          default:
          {
            printf("not implemented\n");
            return NULL;
            break;
          }
        }
      }
      break;
    }
    case VP_UNARY_LITERAL:
    {
      return g_variant_new_string(a->u.unary_expression.u.string);
    }
    case VP_UNARY_STRING:
    case VP_UNARY_LITERAL_SINGLE:
    {
      if(v) {
        const GVariantType *type = g_variant_get_type(v);
        if(g_variant_type_is_dict_entry(type)){
          const gchar *str = NULL;
          g_variant_get_child (v, 0, "&s", &str);
          if(strcmp(str, a->u.unary_expression.u.string) == 0) {
            GVariant *childv = g_variant_get_variant(g_variant_get_child_value (v, 1));
            if(nd->children)
              return evaluate_expression(g_node_first_child(nd), childv);
            else {
              /*unbox the child and return the variant*/
              #ifdef DEBUG
              childv = g_variant_get_child_value(childv, 0);
              childv = g_variant_get_variant(g_variant_get_child_value(childv, 1));
              if(g_variant_type_is_variant(g_variant_get_type(childv)))
                childv = g_variant_get_variant(childv);
              #endif
              return childv;            
            }
          } else {
            printf("error not found %s\n", a->u.unary_expression.u.string);
            return NULL;
          }      
        } else if(g_variant_type_is_array(type)) {
          return iterate_container_recursive (v, a->u.unary_expression.u.string, nd);
        } else {
          printf("unexpected type %s\n", g_variant_get_type_string(v));
          return NULL;
        }
      } else {
        return NULL;
      }
      break;
    }
    default:
      break;
  }
  return NULL;
}

bool evaluate_condition(struct condition_node *cn, GVariant* v) {
  GVariant *left = evaluate_expression(cn->left->node, v);
  if(!left)
    return false;
  if(g_variant_is_container(left)) {
    left = g_variant_get_child_value(left, 1);
  }  		    
  GVariant* right = cn->right;
  //printf("left type %s, right type %s\n", g_variant_get_type_string(left), g_variant_get_type_string(right)); 
  bool c = false;
  switch(cn->type) {
    case FILTER_EQ:
    {        
      GVariant *tmp = compute_math(left, VP_MATH_EQEQ, right);
      if(g_variant_get_boolean(tmp)) {
        c = true;
      }
      g_variant_unref (tmp);        
      break;
    }
    case FILTER_LT:
    {	
      GVariant *tmp = compute_math(left, VP_MATH_LT, right);
      if(g_variant_get_boolean(tmp)) {
        c = true;
      }
      g_variant_unref (tmp);      
      break;
    }
    case FILTER_GT:
    {	
      GVariant *tmp = compute_math(left, VP_MATH_GT, right);
      if(g_variant_get_boolean(tmp)) {
        c = true;
      }
      g_variant_unref (tmp);     
      break;
    }
    case FILTER_LTEQ:
    {		
      GVariant *tmp = compute_math(left, VP_MATH_LTEQ, right);
      if(g_variant_get_boolean(tmp)) {
        c = true;
      }
      g_variant_unref (tmp);       
      break;
    }
    case FILTER_GTEQ:
    {
      GVariant *tmp = compute_math(left, VP_MATH_GTEQ, right);
      if(g_variant_get_boolean(tmp)) {
        c = true;
      }
      g_variant_unref (tmp);            
      break;
    }
    case FILTER_NOTEQ:
    {	
      GVariant *tmp = compute_math(left, VP_MATH_NOTEQ, right);
      if(g_variant_get_boolean(tmp)) {
        c = true;
      }
      g_variant_unref (tmp);          
      break;
    }
    case FILTER_IN:
    {
      GVariantIter iter;
      GVariant *child, *in_tmp;

      g_variant_iter_init (&iter, right);
      while ((child = g_variant_iter_next_value (&iter)))
        {
          in_tmp = compute_math(left, VP_MATH_EQEQ, child);
          if(g_variant_get_boolean(in_tmp)) {
            c = true;
            g_variant_unref (in_tmp);
            g_variant_unref (child);            
            break;
          }
          g_variant_unref (in_tmp);
          g_variant_unref (child);
        }      
      break;
    }
    case FILTER_CONTANINS:
    {
      GVariantIter iter;
      GVariant *child, *con_tmp;

      g_variant_iter_init (&iter, left);
      while ((child = g_variant_iter_next_value (&iter)))
        {
          con_tmp = compute_math(child, VP_MATH_EQEQ, right);
          if(g_variant_get_boolean(con_tmp)) {
            c = true;
            break;
          }
          g_variant_unref (con_tmp);
          g_variant_unref (child);
        }         
      break;
    }
    default:
      break;
  }
  return c;
}

GVariant* evaluate(struct rule_node *rn, GVariant *source) {
  GSList *list = NULL;
  for (GSList *l = rn->alpha; l != NULL; l = l->next)
  {
    struct alpha_node *a = l->data;
    struct condition_node *cd = a->condition;
    if(!evaluate_condition(cd, source)) {
      for (GSList *s = a->beta_nodes; s != NULL; s = s->next) 
      {
        struct beta_node *b = s->data;
        b->enabled = false;
      }
    }
  }
  gboolean matched = FALSE;
  struct beta_node *default_bn = NULL;
  for (GSList *x = rn->beta; x != NULL; x = x->next) 
  {
    struct beta_node *bn = x->data;
    if(bn->enabled && !bn->is_default) {
        matched = TRUE;
        /*process this rule*/     
        GVariant *tmp = evaluate_expression(bn->expression_node, source);
        if(tmp)
          list = g_slist_append(list, g_variant_new("{sv}", bn->name, tmp));
    }
    if(bn->is_default) {
      default_bn = bn;
    }
  }
  if(!matched) {
    /*process the default*/
    list = g_slist_append(list, g_variant_new("{sv}", "default rule", evaluate_expression(default_bn->expression_node, source)));
  }
  GVariantBuilder builder;
  g_variant_builder_init (&builder, G_VARIANT_TYPE_ARRAY);
	for (GSList *l = list; l != NULL; l = l->next)
  {
    g_variant_builder_add_value (&builder, l->data);    
  }    
  g_slist_free(list);
  return  g_variant_new("{sv}", rn->name,g_variant_builder_end(&builder));
}





