#ifndef _SBP_RULE_RETE_H
#define _SBP_RULE_RETE_H
#include <glib.h>
#include <stdbool.h>
#include "rule_ast.h"

struct unary_node
{
    GString* path;
    GNode* node;
};

struct condition_node
{
    struct unary_node *left;
    GVariant *right;
    enum vp_compare_type type;
};

struct alpha_node
{
    struct condition_node *condition;
    bool enabled;
    GSList *beta_nodes; 
};

struct beta_node
{
    bool enabled;
    bool is_default;
    char* name;
    GSList *alpha_nodes; 
    GNode *expression_node;
};

struct rule_node
{
    char* name;
    GSList *alpha;
    GSList *beta;
};

const GVariantType *
top_type(const GVariantType *a, const GVariantType *b);

GVariant *
cast_to(GVariant *src, const GVariantType *to_type);

GHashTable* parse_rule(char *rule_file);

GVariant* evaluate(struct rule_node *rn, GVariant *source);
#endif