#ifndef _SBP_META_H
#define _SBP_META_H

#include <glib.h>

struct compound_node
{
	/* data */
	GString *label;
	gboolean isArray;
};

#ifdef DEBUG
struct simple_node
{
	GVariant *var;
	uint32_t pos;
	uint32_t size;
};
#endif


struct len_struct {
	GNode *name;
	uint32_t length;
	uint32_t pos;
};

struct parsed_node {
	enum {
		COMPOUND=0,
		SIMPLE
	} type; 
	union
	{
		struct compound_node *cpn;
		#ifdef DEBUG
		struct simple_node *sn;
		#else
		GVariant *var;
		#endif
	} u;	
};

struct TagRefData{
  GString *tag;
  GString *value;
};

struct parsed_nd_cb
{
	GString *label;
	GNode *nd;
};

enum field_class_type {
	FIELD_CLASS_TYPE_INT,
	FIELD_CLASS_TYPE_ENUM,
	FIELD_CLASS_TYPE_FLOAT,
	FIELD_CLASS_TYPE_STRING,
	FIELD_CLASS_TYPE_STRUCT,
	FIELD_CLASS_TYPE_ARRAY,
	FIELD_CLASS_TYPE_VARIANT,
	FIELD_CLASS_TYPE_FORMULA,
	FIELD_CLASS_TYPE_SHOWONLYIF,
	FIELD_CLASS_TYPE_SKIP,
	FIELD_CLASS_TYPE_LENCONDITION,
};

enum byte_order {
	BYTE_ORDER_BIG,
	BYTE_ORDER_LITTLE,
};

struct field_class {
	enum field_class_type type;
	unsigned int size;
};

struct field_class_int {
	struct field_class base;
	gboolean is_signed;
};

struct field_class_float {
	struct field_class base;
};

struct field_class_string {
	struct field_class base;
};

struct field_class_struct {
	struct field_class base;

	/* Array of `struct named_field_class` */
	GList *members;
};

struct field_class_enum_mapping {
	GString *label;

	union {
		uint64_t u;
		int64_t i;
	} value;
};

struct field_class_enum {
	struct field_class_int base;
	GString* default_tag;
	GString* type_name;
	/* Array of `struct field_class_enum_mapping` */
	GList *mappings;
};

struct field_class_lencondition {
	/*this type not occupy fields, the base->size always 0*/
	struct field_class base;
	/*this is a wrapper of the parent class*/
	struct named_field_class *class;
	GNode *condition;
};

struct field_class_variant {
	struct field_class base;
	GString *tag_ref;
	/* Array of `struct named_field_class`  key is the name*/
	GList *options;
};

struct named_field_class;

struct field_class_array {
	struct field_class base;

	/* Array of `struct named_field_class` */
	struct named_field_class *item;
	enum {
		ARRAY,
		SEQUENCE,
		FORMULA,
		LENCONDITION
	} arrType;
	union
	{
		uint64_t length;
		GNode *length_ref;
		struct field_class_formula *formula;
		GNode *lenvar;
	} u;
	
	//uint64_t length;
};

struct field_class_skip {
	/*this type not occupy fields, the base->size always 0*/
	struct field_class base;
	uint64_t length;
};

struct field_class_formula {
	/*this type not occupy fields, the base->size always 0*/
	struct field_class base;
	/*the returned field class of this formula*/
	struct named_field_class *class;
	/*the root node to use in IR*/
	GNode *node;
};

struct field_class_showonlyif {
	/*this type not occupy fields, the base->size always 0*/
	struct field_class base;
	//this class can be formula or other type
	struct named_field_class *class;	
	/*the root node to use in IR*/
	GNode *node;
};

struct named_field_class {
	/* Name as translated to trace IR (leading `_` removed) */
	GString *name;

	/* Owned by this */
	struct field_class *fc;
	/*this only make sense during the ir*/
	gboolean in_array;
};

static inline
void *_field_class_copy(struct field_class *src_fc, struct field_class *dst_fc)
{
	dst_fc->type = src_fc->type;
	dst_fc->size = src_fc->size;
}
#endif /* _META_H */