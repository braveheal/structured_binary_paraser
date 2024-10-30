#ifndef _SBP_SCANNER_H
#define _SBP_SCANNER_H
#include "ast.h"
#include <glib.h>

enum NodeType { NODESTRUCT = 1, NODEVARIANT};

struct node_scope;
struct node_scope {
	struct node_scope* parent;
	GString* nodename;
	enum NodeType nodetype;
};

struct node_level;
struct node_level {
	struct node_scope* scope;
	int level;
};

struct NodePrefix {
	struct node_scope* ns;
};

GString* build_prefix(struct NodePrefix* prefix);

struct ctf_scanner_scope;
struct ctf_scanner_scope {
	struct ctf_scanner_scope *parent;
	GHashTable *classes;
};

void init_scope(struct ctf_scanner_scope *scope,
		       struct ctf_scanner_scope *parent);
           
typedef struct Scanner {
  char *top, *cur, *ptr, *pos;
  int line;  
  GNode *root;
  struct ctf_scanner_scope root_scope;
  struct ctf_scanner_scope *cs;
  GStringChunk *chunk;
} Scanner;

int is_type(Scanner *scanner);
#endif