#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "ast.h"
#include "alloc.h"
#include "array.h"
#include "common.h"
#include "debug_print.h"
#include "error.h"          // log()
                            // WARNING: error.h gives access to the global `loc`
#include "hash_table.h"
#include "llvm.h"
#include "str_intern.h"

extern config_t config;

/* Errors
 */
// TODO: Stop typechecking altogether after a threshold of errors.
static int num_global_typecheck_errors;
template <typename... Args> 
void typecheck_error_no_ln(location_t __loc, Args... args) {
    ++num_global_typecheck_errors;
    log(__loc);
    yellow_on();
    bold_on();
    log(" Semantic Error: ");
    bold_off();
    yellow_off();
    log(args...);
}

template <typename... Args> 
static void typecheck_error(location_t __loc, Args... args) {
    ++num_global_typecheck_errors;
    log(__loc);
    yellow_on();
    bold_on();
    log(" Semantic Error: ");
    bold_off();
    yellow_off();
    log(args...);
    printf("\n");
}
/* End of errors */

// Used only in virtual table generation. It contains
// the roots of inheritance trees.
static Buf<IdType *> inher_roots;

void typecheck_init() {
    set_indent_char('*');
}

/// Fill the type_table.
TypeTable install_type_declarations(Goal *goal) {
    DeclarationVisitor decl_visitor(goal->type_decls.len);
    goal->accept(&decl_visitor);

    // Free memory that was used for allocating objects
    // for parsing that doenot persist in pass 2 of type-checking.
    deallocate(MEM::PARSE_TEMP);

    return decl_visitor.type_table;
}

void full_typecheck(Goal *goal, TypeTable type_table) {
    MainTypeCheckVisitor main_visitor(type_table);
    goal->accept(&main_visitor);
}

static bool params_match(Method *m1, Method *m2) {
    if (m1->param_len != m2->param_len) return false;
    size_t it = 0;
    for (Local *p1 : m1->locals) {
        if (it == m1->param_len) break;
        Local *p2 = m2->locals[it];
        if (p1->type != p2->type) return false;
        ++it;
    }
    return true;
}

bool methods_have_same_signature(Method *m1, Method *m2) {
    if (m1->ret_type != m2->ret_type)
      return false;
    return params_match(m1, m2);
}

static void print_param_types(Method *method) {
    size_t param_len = method->param_len;
    for (size_t i = 0; i != param_len; ++i) {
        Local *param = method->locals[i];
        printf("%s", param->type->name());
        if (i + 1 != param_len) {
            printf(", ");
        }
    }
}

constexpr ssize_t bucket_cap = 8;
struct Bucket {
    const char *method_ids[bucket_cap];
    ssize_t indexes[bucket_cap];
    Bucket *next;

    Bucket() {
      next = NULL;
    }

    static void *operator new(size_t size) {
        return allocate(size, MEM::VTABLE);
    }

    void insert(const char *method_id, ssize_t index_in_serial_buf, ssize_t index_in_bucket) {
        assert(index_in_bucket >= 0 && index_in_bucket < bucket_cap);
        method_ids[index_in_bucket] = method_id;
        indexes[index_in_bucket] = index_in_serial_buf;
    }

    ssize_t find(const char *id, ssize_t lim) const {
        for (ssize_t i = 0; i <= lim; ++i) {
            if (method_ids[i] == id) {
              return indexes[i];
            }
        }
        return -1;
    }
};

struct FirstBucket {
    Bucket b;
    ssize_t nelems = 0;
    Bucket *last_bucket;

    FirstBucket() {
        clear();
    }

    void insert(const char *method_id, ssize_t index_in_serial_buf) {
        ssize_t index_in_bucket = nelems % bucket_cap;
        if (nelems != 0 && index_in_bucket == 0) {
            // It may have been allocated from a previous downward
            // tree traversal.
            Bucket *b;
            if (last_bucket->next == NULL) {
                b = new Bucket;
                last_bucket->next = b;
            } else {
                b = last_bucket->next;
            }
            last_bucket = b;
        }
        last_bucket->insert(method_id, index_in_serial_buf, index_in_bucket);
        nelems++;
    }

    ssize_t find(const char *id) const {
        const Bucket *runner = &b;
        ssize_t lim = bucket_cap;
        ssize_t i = nelems;
        while (i > 0) {
            assert(runner);
            ssize_t lim = bucket_cap - 1;
            if (i < bucket_cap) {
                lim = i - 1;
            }
            ssize_t ndx = runner->find(id, lim);
            if (ndx >= 0) {
              return ndx;
            }
            runner = runner->next;
            i -= bucket_cap;
        }
        return -1;
    }

    void clear() {
        nelems = 0;
        b.next = NULL;
        last_bucket = &b;
    }
};

struct VMethod {
    Method *method;
    const char *enclosing;
};

struct RestorePair {
    Bucket *last_bucket;
    ssize_t nelems;
};

struct VirtualTable {
    constexpr static size_t nbuckets = 32;
    size_t nelems;
    FirstBucket buckets[nbuckets];
    Buf<VMethod> vmethods;

    VirtualTable() {
        vmethods.reserve(32);
        nelems = 0;
    }

    inline ssize_t hash(const char *key) const {
        // Note: We have asserted that the key is not NULL,
        // by definition, which leads to a fast and good
        // multiplicative hasing.

        // Prime factor
        uint32_t factor = 100001029;
        // Non-zero key gets hashed
        uint64_t h = (uint32_t) ((uintptr_t)key * factor);
        h = ((h * (uint64_t) this->nbuckets) >> 32);
        return (ssize_t) h;
    }

    // NOTE: We don't test for duplicates.
    void insert(Method *method, const char *enclosing) {
        assert(method);
        // Keep index that it will be pushed in the serial buffer.
        ssize_t index = vmethods.len;
        vmethods.push({method, enclosing});
        // Insert it into the table (giving it an index to keep
        // in the serial buffer)
        const char *method_id = method->id;
        ssize_t val = this->hash(method_id);
        buckets[val].insert(method_id, index);
    }

    // Note that for the specific usage of this
    // table, when we search for something, if it is
    // found, we want to replace it. But only want
    // to replace the "value" part, i.e. the VMethod.
    // That's why we return a pointer to that.
    VMethod *find(const char *id) {
        ssize_t val = this->hash(id);
        ssize_t index = buckets[val].find(id);
        if (index == -1) {
            return NULL;
        }
        return &vmethods[index];
    }

    void clear() {
        nelems = 0;
        vmethods.clear();
        for (ssize_t i = 0; i < nbuckets; ++i) {
            buckets[i].clear();
        }
        deallocate(MEM::VTABLE);
    }

    void save_first_buckets(RestorePair arr[VirtualTable::nbuckets]) {
        for (ssize_t i = 0; i < VirtualTable::nbuckets; ++i) {
            arr[i].nelems = buckets[i].nelems;
            arr[i].last_bucket = buckets[i].last_bucket;
        }
    }

    void restore_first_buckets(RestorePair arr[VirtualTable::nbuckets]) {
        for (ssize_t i = 0; i < VirtualTable::nbuckets; ++i) {
            buckets[i].nelems = arr[i].nelems;
            buckets[i].last_bucket = arr[i].last_bucket;
        }
    }
};

bool check_method_for_overriding(Method *method, IdType *type, VirtualTable *vtable) {
    VMethod *vmethod = vtable->find(method->id);
    if (!vmethod) {
      return false;
    }
    Method *method_parent = vmethod->method;
    assert(method_parent);
    if (!methods_have_same_signature(method, method_parent)) {
        typecheck_error_no_ln(method->loc, method->ret_type->name(), " ", method->id, "(");
        print_param_types(method);
        printf(") in `%s` can't override %s %s(", type->id, method_parent->ret_type->name(),
               method_parent->id);
        print_param_types(method_parent);
        printf(") in `%s`\n", vmethod->enclosing);
        return false;
    }
    vmethod->enclosing = type->id;
    method->offset = method_parent->offset;
    return true;
}

void TypeTable::compute_and_print_offsets_for_type(IdType *type, size_t start_fields, size_t start_methods, VirtualTable *vtable) {
    if (type == NULL) {
      return;
    }
    IdType *parent = type->parent;
    if (parent) {
        // Inherits, so include the parent at the start of the type.
        emit("%%class.%s = type { %%class.%s", type->id, parent->id);
    } else {
        // The first parent in the inheritance tree - create
        // a vptr.
        emit("%%class.%s = type { i8 (...)**", type->id);
    }

    // Process methods first to know how many methods we have so we
    // can emit the respective virtual pointer.
    int num_methods = start_methods / 8;
    size_t running_offset = start_methods;
    for (Method *method: type->methods) {
        if (!check_method_for_overriding(method, type, vtable)) {
            if (config.offsets) {
                printf("%s.%s: %zd\n", type->id, method->id, running_offset);
            }
            method->offset = running_offset;
            running_offset += 8;
            ++num_methods;
            vtable->insert(method, type->id);
        }
    }
    size_t methods_size = running_offset - start_methods;

    // Generate offsets for fields and also emit the fields.
    running_offset = start_fields;
    for (Local *field : type->fields) {
        field->offset = running_offset;
        if (config.offsets) {
            printf("%s.%s: %zd\n", type->id, field->id, field->offset);
        }
        emit(", ");
        if (field->type == this->bool_type) {
            emit("i1");
            running_offset += 1;
        } else if (field->type == this->int_type) {
            emit("i32");
            running_offset += 4;
        } else if (field->type == this->int_arr_type) {
            emit("i32*");
            running_offset += 8;
        } else {
            IdType *idtype = field->type->is_IdType();
            assert(idtype);
            emit("%%class.%s*", idtype->id);
            running_offset += 8;
        }
    }
    emit(" }\n");

    size_t fields_size = running_offset - start_fields;

    start_fields = start_fields + fields_size;
    type->__sizeof = start_fields;
    start_methods = start_methods + methods_size;

    Buf<VMethod> vmethods = vtable->vmethods;
    type->vmethods_len = vmethods.len;
    emit("@.%s = global [%d x i8*]\n[\n", type->id, vmethods.len);
    ssize_t i = 0;
    for (ssize_t i = 0; i < vmethods.len; ++i) {
        VMethod vmethod = vmethods[i];
        emit("i8* bitcast (");
        emit_vmethod_signature(type, vmethod.method); 
        emit(" @%s__%s to i8*)", vmethod.enclosing, vmethod.method->id);
        if (i != vmethods.len - 1) {
            emit(",");
        }
        emit("\n");
    }
    emit("]\n");
    
    assert(type->children);
    auto children = *(type->children);
    size_t len = children.len;
    if (len) {
        Buf<VMethod> deep_copy;
        // We only need deep copy for more than one children.
        RestorePair save[VirtualTable::nbuckets];
        if (len > 1) {
            deep_copy = vtable->vmethods.deep_copy();
            vtable->save_first_buckets(save);
        }
        compute_and_print_offsets_for_type(children[0], start_fields, start_methods, vtable);
        if (len > 1) {
            for (ssize_t index = 1; index < len; ++index) {
                vtable->vmethods = deep_copy;
                vtable->restore_first_buckets(save);
                IdType *child = children[index];
                compute_and_print_offsets_for_type(child, start_fields, start_methods, vtable);
                vtable->vmethods.free();
                if (index != len - 1) {
                    deep_copy = vtable->vmethods.deep_copy();
                }
            }
        }
    }

    type->invalidate_children();
}

void TypeTable::offset_computation() {
    VirtualTable vtable;
    for (IdType *type : inher_roots) {
        //printf("-- Inheritance root: %s -- \n\n", type->id);
        constexpr size_t start_fields = 0;
        constexpr size_t start_methods = 0;
        compute_and_print_offsets_for_type(type, start_fields, start_methods, &vtable);
        vtable.clear();
        //printf("\n");
    }
    deallocate(MEM::CHILDREN);
    emit("\n");
}

bool typecheck(Goal *goal) {
    typecheck_init();
    constexpr int approximated_num_of_classes_per_inheritance_tree = 8;
    // +1 for the main class
    int type_decls_len = goal->type_decls.len + 1;
    inher_roots.reserve(type_decls_len /
                        approximated_num_of_classes_per_inheritance_tree);
    // Pass 1
    TypeTable type_table = install_type_declarations(goal);
    if (config.log) {
        printf("\n");
    }

    // TODO: How able we are to continue?
    // Print types that could not be inserted. This can
    // happen if the types used are more than the type
    // declarations. See TypeTable.
    // TODO: The types in `could_not_be_inserted` might actually have a
    // declaration. Consider that if say we have space in the table for 2
    // type declarations. The declarations that _exist_ are for A and D.
    // In A, let's say we use A, B, C and D in that order. D will be inserted
    // last and it won't be in the type table. It will be in the
    // `could_not_be_inserted` because of lack of space. But D
    // actually has a declaration.
    for (IdType *type : type_table.could_not_be_inserted) {
        typecheck_error(type->loc, "Type `", type->id, "` has not been ",
                        "defined");
    }

    // Offset computation
    type_table.offset_computation();

    // Print calloc declarations etc.
    cgen_init();

    // Pass 2
    full_typecheck(goal, type_table);
    
    if (num_global_typecheck_errors == 0) {
        return true;
    }
    return false;
}

/* Declaration Visitor
 */
IdType* DeclarationVisitor::id_to_type(const char *id, location_t loc) {
    // Check if it already exists.
    IdType *type = this->type_table.find(id);
    if (type) {
        return type;
    }
    // Otherwise construct a new one and
    // save it in the table.
    type = new IdType(id);
    type->loc = loc;
    this->type_table.insert(type->id, type);
    return type;
}

// This is member of the visitor so that we have
// access to the type table.
Type* DeclarationVisitor::typespec_to_type(Typespec tspec, location_t loc) {
    switch (tspec.kind) {
    case TYSPEC::UNDEFINED: assert(0);
    case TYSPEC::INT: return type_table.int_type;
    case TYSPEC::ARR: return type_table.int_arr_type;
    case TYSPEC::BOOL: return type_table.bool_type;
    case TYSPEC::ID: return this->id_to_type(tspec.id, loc);
    default: assert(0);
    }
}

/* Type Table
 */
void TypeTable::initialize(size_t n) {
    type_table.reserve(n);
    undefined_type = new Type(TY::UNDEFINED);
    bool_type = new Type(TY::BOOL);
    int_type = new Type(TY::INT);
    int_arr_type = new Type(TY::ARR);
}


void TypeTable::insert(const char *id, IdType* v) {
    if (!type_table.insert(id, v)) {
        // Search in the auxiliary buffer
        for (IdType *type : could_not_be_inserted) {
            if (type->id == id) {
                return;
            }
        }
        could_not_be_inserted.push(v);
    }
}

DeclarationVisitor::DeclarationVisitor(size_t ntype_decls) {
    // IMPORTANT: Before installing a type, the user
    // is responsible for checking if it exists.
    // +1 for the main class
    this->type_table.initialize(ntype_decls + 1);
}

void DeclarationVisitor::visit(Goal *goal) {
    LOG_SCOPE;
    debug_print("Pass1::Goal\n");
    goal->main_class.accept(this);
    for (TypeDeclaration *type_decl : goal->type_decls) {
        type_decl->accept(this);
    }
}

void DeclarationVisitor::visit(MainClass *main_class) {
    LOG_SCOPE;
    debug_print("Pass1::MainClass: %s\n", main_class->id);
    // Assert that we visit this first i.e., no types have been created
    assert(this->type_table.len_inserted() == 0);
    IdType *main_cls_type = new IdType(main_class->id,
                                       /* nfields = */ 0,
                                       /* nmethods = */ 0);
    main_cls_type->loc = main_class->loc;

    this->type_table.main_cls_type = main_cls_type;
    this->type_table.insert(main_class->id, main_cls_type);
    inher_roots.push(main_cls_type);

    // Construct a custom method for `main()`
    Method *main_method = Method::construct_main_method(this, main_class);
    this->type_table.main_method = main_method;
}

// Note - IMPORTANT: 
// A lot of IdTypes just start undefined (i.e. kind == TY::UNDEFINED)
// if we have seen a declaration with this type but we have
// not yet seen a definition. For example:
//     int test(A a)
// if we have not yet have seen the definition of A.
// We install (i.e allocate) a type (if one does not exist already for id `A`)
// but we just keep it undefined (and keep a pointer to it).
// If we then _see_
//     class A { ...
// we don't allocate a new type (note below that we're searching the table).
// We do the changes in the same memory that was allocated when we were in `test(A a)`.
// And we change the type from undefined.

// If we never see the definition of `A`, it will be caught in pass 2 as described below.

// Essentially, this almost eliminates searches in the type table in the pass 2.
// We just check the pointer and if the type was never defined, it will have
// remained undefined and we issue an error. Otherwise, the definition of this
// type will be in the same memory location.

void DeclarationVisitor::visit(TypeDeclaration *type_decl) {
    LOG_SCOPE;
    assert(!type_decl->is_undefined());
    print_indentation();
    debug_log(type_decl->loc, "Pass1::TypeDeclaration: ", type_decl->id, "\n");
    IdType *type = this->type_table.find(type_decl->id);
    // If it exists and it's declared (i.e. we have processed
    // a type with the same `id`), then we have redeclaration error.
    if (type) {
        if (type->is_defined()) {
            typecheck_error(type_decl->loc, "Type with id: `", type_decl->id,
                            "` has already been declared");
            return;
        } else {
            // Overwrite the location. If the type is already inserted, its
            // loc is its first usage.
            type->loc = type_decl->loc;
            type->set_sizes(type_decl->vars.len, type_decl->methods.len);
        }
    } else {
        type = new IdType(type_decl->id, type_decl->vars.len, type_decl->methods.len);
        type->loc = type_decl->loc;
        this->type_table.insert(type->id, type);
    }
    // Handle inheritance
    if (type_decl->extends) {
        IdType *parent = this->id_to_type(type_decl->extends, type_decl->loc);
        type->set_parent(parent);
        parent->add_child(type);
    } else {
        inher_roots.push(type);
    }
    for (LocalDeclaration *ld : type_decl->vars) {
        // Check redefinition in fields.
        // IMPORTANT:
        // - A field must not conflict with a field of the current class.
        // - A field can conflict with a field of the parent class and
        //   this is called shadowing. And it's correct (when we refer
        //   to the field, we refer to the most immediate in the
        //   inheritance tree).
        // - A field can't possibly conflict with a method
        //   of the current class because first we define all the fields,
        //   then all the methods (so, only a method can conflict with a field).
        // - A field can conflict with a method of a parent class, but that's
        //   NOT an error. Because it is disambiguated in what we refer to
        //   by the way we dereference.

        Field *field = type->fields.find(ld->id);
        if (field) {
            typecheck_error(ld->loc, "In class with id: `", type_decl->id,
                            "`, redefinition of field with id: `", field->id, "`");
        } else {
            field = ld->accept(this);
            field->kind = (int)LOCAL_KIND::FIELD;
            type->fields.insert(field->id, field);
        }
    }
    for (MethodDeclaration *md : type_decl->methods) {
        // Check redefinition
        // IMPORTANT:
        // - A method must not conflict with a method of the current class.
        // - A method must not conflict with a field of the current class.
        // - A method can conflict with a method of the parent class:
        //   -- If the methods have the same return type and the _same_
        //      parameters, then it's valid overriding.
        //   -- Otherwise, it's an error.
        //   We CAN'T check this in the first pass as it is possible that
        //   we have not processed the parent type yet. Note that we don't
        //   check this at the typecheck visitor but at the offset computation.
        // - A method can conflict with a field of the parent 
        //   but that's NOT an error.  Because it is disambiguated in what
        //   we refer to by the way we dereference.

        Method *method = type->methods.find(md->id);
        if (method) {
            typecheck_error(md->loc, "In class with id: `", type_decl->id,
                            "`, redefinition of method with id: `", method->id,
                            "`. Note that you can override but not overload ",
                            "a method");
        } else {
            method = md->accept(this);
            assert(method);
            assert(method->id);
            type->methods.insert(method->id, method);
        }
    }
}

Local *DeclarationVisitor::visit(LocalDeclaration *local_decl) {
    // Note: It's responsibility of the one who calls this visit
    // to have assured that a local declaration with the same
    // id does not exist.
    LOG_SCOPE;
    assert(!local_decl->is_undefined());
    print_indentation();
    debug_log(local_decl->loc, "LocalDeclaration: ", local_decl->id, "\n");
    if (local_decl->typespec.id != NULL
        && local_decl->typespec.id == this->type_table.main_cls_type->id)
    {
        typecheck_error(local_decl->loc, "You can't declare a local of type of",
                        " the main class.");
    }
    Type *type = typespec_to_type(local_decl->typespec, local_decl->loc);
    Local *local = new Local(local_decl->id, type);
    return local;
}

const char *DeclarationVisitor::gen_id() {
    static int count = 0;
    char buf[64];
    sprintf(buf, "a%d", count);
    count++;
    return str_intern(buf);
}

Method *DeclarationVisitor::visit(MethodDeclaration *method_decl) {
    // Note: It's responsibility of the one who calls this visit
    // to have assured that a method with the same id does not exist.
    LOG_SCOPE;
    assert(!method_decl->is_undefined());
    print_indentation();
    debug_log(method_decl->loc, "MethodDeclaration: ", method_decl->id, "\n");
    Method *method = new Method(method_decl);
    assert(method);
    method->ret_type = this->typespec_to_type(method_decl->typespec, method_decl->ret->loc);
    for (LocalDeclaration *par : method_decl->params) {
        // We accept the param anyway. But in the case that there is
        // another param with the same name, we still insert this
        // (so that the rest of type-checking can sort of continue) but
        // we have to generate a (dummy) id for it.
        Param *param = par->accept(this);
        if (method->locals.find(par->id)) {
            typecheck_error(par->loc, "Parameter `", par->id, "` is already defined",
                            " in method `", method_decl->id, "`");
            param->id = gen_id();
        }
        method->locals.insert(param->id, param);
    }
    for (LocalDeclaration *var : method_decl->vars) {
        if (method->locals.find(var->id)) {
            typecheck_error(var->loc, "Variable `", var->id, "` is already defined",
                            " in method `", method_decl->id, "`");
        } else {
            Var *v = var->accept(this);
            method->locals.insert(v->id, v);
        }
    }
    return method;
}

/* Main TypeCheck Visitor (Pass 2)
 */
void MainTypeCheckVisitor::visit(Goal *goal) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::Goal\n");
    goal->main_class.accept(this);
    for (IdType *type : this->type_table) {
        if (type->is_defined()) {
            type->accept(this);
        } else {
            // type->loc is going to be the location of its first usage (Check
            // id_to_type()). Ideally, we would like to have all the locations
            // it was used, or some, but this will at least make this a not very
            // bad message.
            typecheck_error(type->loc, "Type `", type->id, "` has not been ",
                            "defined");
        }
    }
}

void MainTypeCheckVisitor::visit(MainClass *main_class) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::MainClass\n");
    this->curr_class = this->type_table.main_cls_type;
    this->type_table.main_method->accept(this, this->curr_class->id);
    this->curr_class = NULL;
}

void MainTypeCheckVisitor::visit(IdType *type) {
    LOG_SCOPE;
    assert(type->is_IdType());
    assert(type->is_defined());
    debug_print("MainTypeCheck::IdType %s\n", type->id);

    this->curr_class = type;
    for (Method *method : type->methods) {
        method->accept(this, type->id);
    }
    this->curr_class = NULL;
}

// True if `rhs` is child of `lhs` (or they're equal)
static bool compatible_types(Type *lhs, Type *rhs) {
    if (lhs == rhs) return true;
    IdType *ty1 = lhs->is_IdType();
    IdType *ty2 = rhs->is_IdType();
    if (ty1 && ty2) {
        IdType *runner = ty2;
        while (runner->parent) {
            runner = runner->parent;
            if (runner == ty1) {
                return true;
            }
            // Cyclic inheritance, we issue error elsewhere.
            if (runner == ty2) break;
        }
        return false;
    }
    return false;
}

extern ExprContext __expr_context;

static bool type_transitively_extends_Main(MainTypeCheckVisitor *visitor,
                                           const char *class_name) {
  IdType *type = visitor->type_table.find(class_name);
  const char *main_cls_id = visitor->type_table.main_cls_type->id;
  assert(type);
  IdType *parent = type->parent;
  while (parent != NULL) {
    if (parent->id == main_cls_id) {
      return true;
    }
    parent = parent->parent;
  }
  return false;
}

void MainTypeCheckVisitor::visit(Method *method, const char *class_name) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::Method %s\n", method->id);
    this->curr_method = method;

    __expr_context.method = method;

    bool is_main_method = method->id == main_method_string;

    cgen_start_method(method, class_name, is_main_method);

    if (is_main_method && type_transitively_extends_Main(this, class_name))
    {
        typecheck_error(method->loc, "In type: `", class_name,
                        "` you can't name a method `main` because "
                        "the type transitively extends `Main` class.");
    }

    for (int i = 0; i < __expr_context.nesting_level; ++i) {
      for (int j = 0; j < __expr_context.if_bufs[i].len; ++j) {
        __expr_context.if_bufs[i][j].assigned = false;
      }
      for (int j = 0; j < __expr_context.else_bufs[i].len; ++j) {
        __expr_context.else_bufs[i][j].assigned = false;
      }
    }
    __expr_context.nesting_level = 0;


    for (Statement *stmt : method->stmts) {
        stmt->accept(this);
    }
    // An undefined return expression may actually end up here.
    // Check parse.cpp
    // A null expression may also come here because of the `main()` method.
    if (!is_main_method && !method->ret_expr->is_undefined()) {
        assert(method->ret_expr);
        Type *ret_type = method->ret_expr->accept(this);
        if (ret_type->kind != TY::UNDEFINED) {
          assert(ret_type);
          assert(method->ret_type);
          llvalue_t ret_val = __expr_context.llval;
          if (!compatible_types(method->ret_type, ret_type)) {
              location_t loc_here = method->ret_expr->loc;
              typecheck_error(loc_here, "The type: `", ret_type->name(),
                              "` of the return expression does not match the ",
                              "return type: `", method->ret_type->name(),
                              "` of method: `", method->id, "`");
          } else {
              llvm_ret(ret_type, ret_val);
          }
        }
    }
    this->curr_method = NULL;

    cgen_end_method(is_main_method);

    // Free arena for objects of function lifetime
    deallocate(MEM::FUNC);
}

static Local *lookup_local(const char *id, Method *method, IdType *cls) {
    // Check current method's locals (vars and params).
    Local *local = method->locals.find(id);
    if (local) {
        return local;
    }
    // Check current class's fields.
    local = cls->fields.find(id);
    if (local) {
        return local;
    }
    // Check parent's fields (account for cyclic inheritance).
    IdType *runner = cls;
    while (runner->parent) {
        runner = runner->parent;
        local = runner->fields.find(id);
        if (local) {
            return local;
        }
        // Cyclic inheritance, we issue error elsewhere.
        if (runner == cls) break;
    }
    return NULL;
}

static bool check_expr_list_against_method(FuncArr<Type*> expr_list, Method *method) {
    if (expr_list.len != method->param_len) {
        return false;
    }

    size_t formal_param_counter = 0;
    for (Type *ety : expr_list) {
        Type *formal_type = method->locals[formal_param_counter]->type;
        if (!compatible_types(formal_type, ety)) {
            return false;
        }
        ++formal_param_counter;
    }
    assert(formal_param_counter == method->param_len);
    return true;
}

static Method *lookup_method_parent(const char *id, IdType *cls, IdType **ret_parent = NULL) {
    IdType *runner = cls;
    while (runner->parent) {
        runner = runner->parent;
        Method *method = runner->methods.find(id);
        if (method) {
            if (ret_parent) {
                *ret_parent = runner;
            }
            return method;
        }
        // Cyclic inheritance, we issue error elsewhere.
        if (runner == cls) break;
    }
    return NULL;
}

static Method *lookup_method(const char *id, IdType *cls) {
    // Check current class's methods
    Method *method = cls->methods.find(id);
    if (method) {
        return method;
    }
    // Check parent's methods (account for cyclic inheritance).
    return lookup_method_parent(id, cls);
}

static Method *deduce_method(FuncArr<Type*> expr_list, const char *method_id, IdType *cls) {
    IdType *runner = cls;
    do {
        Method *method = lookup_method(method_id, runner);
        if (method) {
            if (check_expr_list_against_method(expr_list, method)) {
                return method;
            }
        }
        runner = runner->parent;
        // Cyclic inheritance, we issue error elsewhere.
        if (runner == cls) break;
    } while (runner);
    return NULL;
}

// Only used for EXPR::AND
static Type *typecheck_and_helper(bool is_correct, Expression *expr,
                                  MainTypeCheckVisitor *v, Type *boolty, Type *undefinedty) {
    Type *ty = expr->accept(v);
    if (ty->kind == TY::UNDEFINED) {
        return undefinedty;
    }
    if (ty != boolty) {
        typecheck_error(expr->loc, "Bad right operand for binary operator `&&`. Operand ",
                        "of boolean type was expected, found: `", ty->name(), "`");
        is_correct = false;
    }
    if (is_correct) {
        return boolty;
    }
    return undefinedty;
}

#define poison_expr(t) \
  if (t->kind == TY::UNDEFINED) { \
    return this->type_table.undefined_type; \
  } \

// IMPORTANT: DO NOT check if a type is undefined with equality test with
// type_table.undefined_type. Check a note above on a full explanation.
// A lot of types can have remained undefined (either because we never saw a definition
// or the definition was invalid) but are not pointing to
// type_table.undefined_type. type_table.undefined_type is only for denoting
// an undefined type in general (e.g. an expression has undefined type, return
// type_table.undefined_type as its type. The caller checks for equality
// with type_table.undefined_type to see if it was valid).
// TODO: Maybe pass a second argument which is the expected return type. This is
// because a lot of times, when we find an error, we don't want to introduce useless
// cascading errors by return e.g. undefined_type. But currently, we can't know
// what the caller expects so we can just return that.
Type* MainTypeCheckVisitor::visit(Expression *expr) {
    LOG_SCOPE;
    assert(!expr->is_undefined());

    // Used in binary expression cases.
    BinaryExpression *be = (BinaryExpression *) expr;

    switch (expr->kind) {
    case EXPR::BOOL_LIT:
    {
        debug_print("MainTypeCheck::BoolExpression\n");
        /// Codegen ///
        __expr_context.llval.kind = LLVALUE::CONST;
        __expr_context.llval.val = expr->lit_val;
        /// End of Codegen ///
        return this->type_table.bool_type;
    } break;
    case EXPR::ID:
    {
        debug_print("MainTypeCheck::IdExpression: %s\n", expr->id);
        assert(this->curr_method);
        assert(this->curr_class);
        Local *local = lookup_local(expr->id, this->curr_method, this->curr_class);
        if (!local) {
            typecheck_error(expr->loc, "In identifier expression, Identifier: `",
                            expr->id, "` is not defined.");
            return this->type_table.undefined_type;
        }

        /// Codegen ///

        // We assume that we have correct type-checking (although we might not).
        // Given that, the 3 categories are handled as follows:
        // Params:
        //    Params have an automatically-assigned register and we just we use that.
        // Fields:
        //    For fields, we just load from register %0, which we assume corresponds
        //    to the `this` implicit parameter. Normally, we should use `getelementptr`
        //    (correctly) but this is kind of complicated, because the
        //    id can be in an arbitrary depth in the inheritance tree.
        //    What is more, the current offsets would
        //    not work (because they're absolute offsets, like the pointer is char*,
        //    not like getelementptr's). So, we assume %0 is i8*, move to the right position
        //    with `getelementptr` and we just bitcast it.
        // Vars:
        //    For vars, if the type-checking is correct, the will have been initialized.
        //    And thus, they should have a register assigned to them.
        LOCAL_KIND kind = (LOCAL_KIND)local->kind;
        switch (kind) {
        case LOCAL_KIND::PARAM:
        case LOCAL_KIND::VAR:
        {
            // Just use its llvalue.
            __expr_context.llval = local->llval;
        } break;
        case LOCAL_KIND::FIELD:
        {
            llvalue_t ptr = cgen_get_field_ptr(local);
            llvalue_t value = llvm_load(local->type, ptr);
            __expr_context.llval = value;
        } break;
        default: printf("id: %s\n", local->id); assert(0);
        }
        /// End of Codegen ///
        return local->type;
    } break;
    case EXPR::INT_LIT:
    {
        debug_print("MainTypeCheck::IntegerExpression: %d\n", expr->lit_val);
        /// Codegen ///
        __expr_context.llval.kind = LLVALUE::CONST;
        __expr_context.llval.val = expr->lit_val;
        /// End of Codegen ///
        return this->type_table.int_type;
    } break;
    case EXPR::THIS:
    {
        debug_print("MainTypeCheck::ThisExpression\n");
        /// Codegen ///

        // Assume that every function has as a first argument
        // (so, register %0) a pointer to the calling class.
        // Assume also that this pointer is typed properly,
        // so no need to bitcast.
        __expr_context.llval.kind = LLVALUE::REG;
        __expr_context.llval.reg = 0;

        /// End of Codegen ///

        assert(this->curr_class);
        return this->curr_class;
    } break;
    case EXPR::ALLOC:
    {
        debug_print("MainTypeCheck::AllocationExpression\n");
        // Find the type of the Identifier.
        IdType *type = this->type_table.find(expr->id);
        if (!type || type->kind == TY::UNDEFINED) {
            typecheck_error(expr->loc, "In allocation expression, Identifier: `",
                            expr->id, "` does not denote a known type");
            return this->type_table.undefined_type;
        }
        if (!type->is_IdType()) {
            typecheck_error(expr->loc, "In allocation expression, Identifier: `",
                            expr->id, "` does not denote a user-defined type");
            return this->type_table.undefined_type;
        }

        if (type->id == this->type_table.main_cls_type->id) {
            typecheck_error(expr->loc, "You can't create an object of the main class.");
            return this->type_table.undefined_type;
        }
        /// Codegen ///
        llvalue_t i8p = llvm_calloc(const_llv((int)type->sizeof_(), {}));
        llvalue_t i8ppp = llvm_bitcast_i8p_to_i8ppp(i8p);
        cgen_store_vptr(i8ppp, type);

        __expr_context.llval = i8p;
        /// End of Codegen ///
        return type;
    } break;
    case EXPR::ARR_ALLOC:
    {
        debug_print("MainTypeCheck::ArrayAllocationExpression\n");
        assert(expr->e1);
        Type *index_type = expr->e1->accept(this);
        poison_expr(index_type);
        if (index_type != this->type_table.int_type) {
            typecheck_error(expr->loc, "In array allocation expression, ",
                            "the index expression must be of integer type.");
        }
        llvalue_t len = __expr_context.llval;
        // Check for negative size
        // TODO: Do a check when the value is not const.
        if (len.kind == LLVALUE::CONST && len.val < 0) {
            typecheck_error(expr->loc, "Length of array allocation with ",
                            "constant value: ", len.val, " is negative.");
        }
        /// Codegen ///
        // Save the len
        constexpr int sizeof_int = 4;
        Type *int_type = this->type_table.int_type;
        Type *int_arr_type = this->type_table.int_arr_type;
        llvalue_t calloc_ptr;
        if (len.kind == LLVALUE::CONST) {
            // Add 1 more element which is used to save the length.
            len.val += 1;
            calloc_ptr = llvm_calloc(const_llv((int)(len.val * sizeof_int), {}));
        } else {
            // Add 1 more element which is used to save the length.
            len = llvm_op('+', len, const_llv(1, {}));
            llvalue_t size = llvm_op('*', len, const_llv(sizeof_int, {}));
            calloc_ptr = llvm_calloc(size);
        }
        llvalue_t i32p = llvm_bitcast_from_i8p(int_type, calloc_ptr);
        llvm_store(int_type, len, i32p);
        __expr_context.llval = i32p;
        /// End of Codegen ///
        return this->type_table.int_arr_type;
    } break;
    case EXPR::ARR_LEN:
    {
        debug_print("MainTypeCheck::LengthExpression\n");
        assert(expr->e1);
        Type *arr = expr->e1->accept(this);
        poison_expr(arr);
        llvalue_t ptr = __expr_context.llval;
        if (arr != this->type_table.int_arr_type) {
            typecheck_error(expr->loc, "In array length expression, ",
                            "the dereferenced id must be of integer array type.");
        }
        /// Codegen ///

        assert(ptr.kind == LLVALUE::REG);
        // Just loading a 4-byte value from `the` ptr will give us the length
        // as the first 4 bytes of arrays are the length.
        __expr_context.llval = llvm_load(this->type_table.int_type, ptr);

        /// End of Codegen ///

        return this->type_table.int_type;
    } break;
    case EXPR::NOT:
    {
        debug_print("MainTypeCheck::NotExpression\n");
        assert(expr->e1);
        Type *ty = expr->e1->accept(this);
        poison_expr(ty);
        llvalue_t res = __expr_context.llval;
        if (ty != this->type_table.bool_type) {
            typecheck_error(expr->loc, "Bad operand for unary operator `!`. Operand ",
                            "of boolean type was expected, found: `", ty->name(), "`");
        }
        /// Codegen ///

        if (res.kind == LLVALUE::CONST) {
            __expr_context.llval.kind = LLVALUE::CONST;
            __expr_context.llval.val = !res.val;
        } else {
            __expr_context.llval = not_llvalue(res);
        }
        /// End of Codegen ///

        return this->type_table.bool_type;
    } break;

/*----------- BINARY EXPRESSIONS ----------------*/

    case EXPR::AND:
    {
        debug_print("MainTypeCheck::AndExpression\n");
        assert(be->e1);
        assert(be->e2);
        Type *ty1 = be->e1->accept(this);
        poison_expr(ty1);
        llvalue_t res = __expr_context.llval;
        bool is_correct = true;

        llvm_label_t origin_lbl = __expr_context.curr_lbl;

        if (ty1 != this->type_table.bool_type) {
            typecheck_error(expr->loc, "Bad left operand for binary operator `&&`. Operand ",
                            "of boolean type was expected, found: `", ty1->name(), "`");
            is_correct = false;
        }

        // TODO: Currently, we can do constant-folding on the left expression. If it
        // is constant, we will know when it returns and it won't have emitted anything.
        // If not, it will have emitted and again we will know.
        // _However_, we can't constant fold the right expression. This is because
        // if it is not, we have to print a branch _before_ we process the right expression.
        // Possible solutions:
        // 1) The current one: constant fold only the left plus check if the right
        //    one is a trivial constant expression that we can know if it is constant
        //    a priori. Those are the BOOL_LIT expressions.
        // 2) Provide the ability to codegen in a buffer. That way, we don't print, but
        //    we output to the buffer that we can then use accordingly when we know
        //    what kind of expr we have.
        // 3) Disable the `config.codegen` (so, disable printing) and process the expr,
        //    learn if it is constant and if it's not, re-process it. That may be
        //    faster than it seems (and faster than 2) ).

        // Note: Read carefully the following code, it's crafted subtly.

        bool do_branching = true;
        if (res.kind == LLVALUE::CONST) {
            // Constant left `false` value, don't run (i.e. codegen) the right expression.
            // `res` remains and is the result of the left expr.
            // We still have to type-check the right expr.
            if (res.val == 0) {
                // Turn off the codegen (if it was on), we only need to typecheck.
                bool codegen = config.codegen;
                config.codegen = false;
                Type *ty = typecheck_and_helper(is_correct, be->e2, this,
                                                this->type_table.bool_type,
                                                this->type_table.undefined_type);
                // We're done, restore it.
                config.codegen = codegen;
                // Restore
                __expr_context.llval = res;
                return ty;
            } else {
                // The result of the expr will be what the right expr is
                // and will be generated with the code following. No need
                // to do branching, phi etc.
                do_branching = false;
            }
        }
        
        lbl_pair_t and_lbls;
        if (do_branching) {
            // TODO: Here we need something like "insert BB", "insert BB after"
            // like LDC has.
            and_lbls.construct("and");
            
            // To reach this point, certainly res1.kind == LLVALUE::REG.
            assert(res.kind == LLVALUE::REG);
            llvm_branch_cond(res, and_lbls.start, and_lbls.end);
            llvm_gen_lbl(and_lbls.start);
        }

        Type *ret_ty = typecheck_and_helper(is_correct, be->e2, this,
                                    this->type_table.bool_type,
                                    this->type_table.undefined_type);
        llvm_label_t save_curr_lbl = __expr_context.curr_lbl;
        
        if (do_branching) {
            llvm_branch(and_lbls.end);
            llvm_gen_lbl(and_lbls.end);
            __expr_context.llval = llvm_phi_node(this->type_table.bool_type,
                                                 const_llv(0, {}), __expr_context.llval,
                                                 origin_lbl, save_curr_lbl);
        } /* else {
            `__expr_context.llval` has the value generated
            when type-checking to receive `ret_ty`.
        }
        */
        return ret_ty;
    } break;

    case EXPR::CMP:
    {
        debug_print("MainTypeCheck::CmpExpression\n");
        assert(be->e1);
        assert(be->e2);
        Type *ty1 = be->e1->accept(this);
        poison_expr(ty1);
        llvalue_t res1 = __expr_context.llval;
        Type *ty2 = be->e2->accept(this);
        poison_expr(ty2);
        llvalue_t res2 = __expr_context.llval;

        if (ty1 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad left operand for binary operator `<`. Operand ",
                            "of int type was expected, found: `", ty1->name(), "`");
        }
        if (ty2 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad right operand for binary operator `<`. Operand ",
                            "of int type was expected, found: `", ty2->name(), "`");
        }
        /// Codegen  ///
        __expr_context.llval = llvm_op('<', res1, res2);
        /// End of Codegen  ///
        return this->type_table.bool_type;
    } break;
    case EXPR::PLUS:
    case EXPR::MINUS:
    case EXPR::TIMES:
    {
        assert(be->e1);
        assert(be->e2);

        int op;
        if (be->kind == EXPR::PLUS) {
            op = '+';
            debug_print("MainTypeCheck::PlusExpression\n");
        } else if (be->kind == EXPR::MINUS) {
            op = '-';
            debug_print("MainTypeCheck::MinusExpression\n");
        } else {
            op = '*';
            debug_print("MainTypeCheck::TimesExpression\n");
        }

        Type *ty1 = be->e1->accept(this);
        poison_expr(ty1);
        llvalue_t res1 = __expr_context.llval;
        Type *ty2 = be->e2->accept(this);
        poison_expr(ty2);
        llvalue_t res2 = __expr_context.llval;

        if (ty1 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad left operand for binary operator `", (char)op ,
                            "`. Operand of int type was expected, found: `",
                            ty1->name(), "`");
        }
        if (ty2 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad right operand for binary operator `", (char)op ,
                            "`. Operand of int type was expected, found: `",
                            ty2->name(), "`");
        }
        /// Codegen ///
        __expr_context.llval = llvm_op(op, res1, res2);
        /// End of Codegen ///
        return this->type_table.int_type;
    } break;
    case EXPR::ARR_LOOK:
    {
        debug_print("MainTypeCheck::ArrayLookupExpression\n");
        assert(be->e1);
        assert(be->e2);
        Type *ty1 = be->e1->accept(this);
        poison_expr(ty1);
        llvalue_t ptr = __expr_context.llval;
        Type *ty2 = be->e2->accept(this);
        poison_expr(ty2);
        llvalue_t index = __expr_context.llval;

        if (ty1 != this->type_table.int_arr_type) {
            typecheck_error(expr->loc, "Bad left operand for index operator `[]`. ",
                            "Operand of int array type was expected, found: `",
                            ty1->name(), "`");
        }
        if (ty2 != this->type_table.int_type) {
            typecheck_error(expr->loc, "Bad index expression for index operator `[]`. ",
                            "Operand of int type was expected, found: `",
                            ty1->name(), "`");
        }
        // Check for negative index 
        if (index.kind == LLVALUE::CONST && index.val < 0) {
            typecheck_error(expr->loc, "Index with constant value: ", index.val,
                            " is out of bounds (negative)");
        }

        if (ptr.kind != LLVALUE::REG) {
            return this->type_table.int_type;
        }
        /// Codegen ///

        // Check if index < len
        // Note: The length is never constant, so we can't
        // fully constant-fold this even if the index is constant (
        // we only partially constant-fold this in that the constant
        // value is used inline).
        lbl_pair_t lbls_len_check;
        lbls_len_check.construct("bounds_len");
        llvalue_t len = llvm_load(this->type_table.int_type, ptr);
        llvalue_t cmp_res = llvm_op('<', index, len);
        // Mind the order of labels
        llvm_branch_cond(cmp_res, lbls_len_check.end, lbls_len_check.start);

        llvm_gen_lbl(lbls_len_check.start);
        emit("    call void @throw_oob()\n");
        emit("    unreachable\n");
        llvm_gen_lbl(lbls_len_check.end);


        // You can't have a constant of pointer value.
        assert(ptr.kind == LLVALUE::REG);
        llvalue_t el;
        if (index.kind == LLVALUE::CONST) {
            // If index is constant and negative, we check it above.
            // Take into consideration the 4 bytes of the length.
            // Note: Because we're indexing an i32*, the 4 bytes are
            // +1 offset.
            index.val += 1;
        } else {
            lbl_pair_t lbls_neg_check;
            lbls_neg_check.construct("bounds_neg");
            llvalue_t cmp_res = llvm_op('<', index, const_llv(0, {}));
            llvm_branch_cond(cmp_res, lbls_neg_check.start, lbls_neg_check.end);

            llvm_gen_lbl(lbls_neg_check.start);
            emit("    call void @throw_oob()\n");
            emit("    unreachable\n");
            llvm_gen_lbl(lbls_neg_check.end);
            // Note: As above, because we're indexing an i32*, the 4 bytes are
            // +1 offset.
            index = llvm_op('+', index, const_llv(1, {}));
        }
        ptr = llvm_getelementptr_i32(ptr, index);
        __expr_context.llval = llvm_load(this->type_table.int_type, ptr);

        /// End of Codegen ///

        return this->type_table.int_type;
    } break;
    case EXPR::MSG_SEND:
    {
        debug_print("MainTypeCheck::MessageSendExpression\n");
        assert(be->e1);
        IdType *type = (IdType*) be->e1->accept(this);
        poison_expr(type);
        llvalue_t base_obj = __expr_context.llval;

        if (!type->is_IdType()) {
            typecheck_error(expr->loc, "Bad dereferenced operand for message",
                            "send operator `.`. ", "Operand of user-defined ",
                            "type was expected, found: `", type->name(), "`");
            return this->type_table.undefined_type;
        }

        FuncArr<Type*> expr_list_types(be->msd->expr_list.len);
        FuncArr<llvalue_t> args(be->msd->expr_list.len);
        for (Expression *e : be->msd->expr_list) {
            Type *type = e->accept(this);
            args.push(__expr_context.llval);
            expr_list_types.push(type);
        }
        Method *method = deduce_method(expr_list_types, be->msd->id, type);
        if (!method) {
            typecheck_error(be->loc, "No matching method with id: `", be->msd->id, "`");
            // TODO: Could we return something better here? Like the type of the first
            // method in the inheritance tree?
            return this->type_table.undefined_type;
        }
        Type *ret_type = method->ret_type;
        // Load the virtual method pointer.
        llvalue_t vmethod = cgen_get_virtual_method(type, base_obj, method);
        __expr_context.llval = llvm_call(ret_type, type, base_obj,
                                         vmethod, expr_list_types, args);
        return ret_type;
    } break;
    default: assert(0); return this->type_table.undefined_type;
    }
}

#define poison_stmt(t) \
  if (t->kind == TY::UNDEFINED) { \
    return;\
  } \

/* Statements
 */
void MainTypeCheckVisitor::visit(BlockStatement *block_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::BlockStatement\n");
    for (Statement *stmt : block_stmt->block) {
        stmt->accept(this);
    }
}

void MainTypeCheckVisitor::visit(AssignmentStatement *asgn_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::AssignmentStatement\n");
    assert(this->curr_method);
    assert(this->curr_class);
    Local *lhs = lookup_local(asgn_stmt->id, this->curr_method, this->curr_class);
    if (!lhs) {
        // Of course, with variable we mean also parameter and field.
        typecheck_error(asgn_stmt->loc, "In assignment statement, variable: `",
                        asgn_stmt->id, "` is not defined");
        return;
    }
    poison_stmt(lhs->type);
    assert(asgn_stmt->rhs);
    Type *rhs_type = asgn_stmt->rhs->accept(this);
    poison_stmt(rhs_type);
    if (!compatible_types(lhs->type, rhs_type)) {
        typecheck_error(asgn_stmt->loc, "Incompatible types: `",
                        rhs_type->name(), "` can't be converted to `",
                        lhs->type->name(), "`");
        return;
    }
    /// Codegen ///
    llvalue_t rhs_val = __expr_context.llval;
    llvalue_t ptr;
    if (lhs->kind == (int)LOCAL_KIND::FIELD) {
        ptr = cgen_get_field_ptr(lhs);
    }
    // Bitcast the value to the type of the lhs
    if (lhs->type != rhs_type) {
        rhs_val = cgen_cast_value(rhs_val , rhs_type, lhs->type);
    }
    // Store the value: Handle fields separately.
    if (lhs->kind == (int)LOCAL_KIND::FIELD) {
        llvm_store(lhs->type, rhs_val, ptr);
    } else {
        // Just set `rhs_val` as the new llvalue.
        lhs->llval = rhs_val;
    }
    // Track assignment only for parameters and variables
    if (__expr_context.nesting_level && lhs->kind != (int)LOCAL_KIND::FIELD) {
        TrackAssignment tasgn;
        tasgn.assigned = true;
        tasgn.llval = lhs->llval;
        if (__expr_context.in_if) {
            __expr_context.if_bufs[__expr_context.nesting_level - 1][lhs->index] = tasgn;
        } else {
            __expr_context.else_bufs[__expr_context.nesting_level - 1][lhs->index] = tasgn;
        }
    }
    /// End of Codegen ///
}

void MainTypeCheckVisitor::visit(ArrayAssignmentStatement *arr_asgn_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::ArrayAssignmentStatement\n");
    assert(this->curr_method);
    assert(this->curr_class);
    Local *arr = lookup_local(arr_asgn_stmt->id, this->curr_method, this->curr_class);
    bool is_correct = true;
    if (!arr) {
        typecheck_error(arr_asgn_stmt->loc, "In array assignment statement, array: `",
                        arr_asgn_stmt->id, "` is not defined");
        is_correct = false;
    }
    assert(arr_asgn_stmt->index);
    Type *index_type = arr_asgn_stmt->index->accept(this);
    poison_stmt(index_type);
    llvalue_t index = __expr_context.llval;
    if (index_type != this->type_table.int_type) {
        typecheck_error(arr_asgn_stmt->loc, "In array assignment statement, the ",
                        "index expression must have `int` type but has: `",
                        index_type->name(), "`");
        is_correct = false;
    }
    assert(arr_asgn_stmt->rhs);
    Type *rhs_type = arr_asgn_stmt->rhs->accept(this);
    poison_stmt(rhs_type);
    llvalue_t rhs_val = __expr_context.llval;
    if (rhs_type != this->type_table.int_type) {
        typecheck_error(arr_asgn_stmt->loc, "In array assignment statement, the ",
                        "right-hand-side expression must have `int` type but has: `",
                        rhs_type->name(), "`");
        is_correct = false;
    }
    if (!is_correct) {
        return;
    }
    /// Codegen ///
    // Get the pointer
    llvalue_t ptr;
    if (arr->kind == (int)LOCAL_KIND::FIELD) {
        ptr = cgen_get_field_ptr(arr);
        ptr = llvm_load(this->type_table.int_arr_type, ptr);
    } else {
        ptr = arr->llval;
    }
    // Store the value (by taking into consideration
    // that the first element is used to save the length).
    if (index.kind == LLVALUE::CONST) {
        index.val += 1;
    } else {
        index = llvm_op('+', index, const_llv(1, {}));
    }
    ptr = llvm_getelementptr_i32(ptr, index);
    assert(rhs_type == this->type_table.int_type);
    llvm_store(rhs_type, rhs_val, ptr);
    /// End of Codegen ///
}

void MainTypeCheckVisitor::visit(IfStatement *if_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::IfStatement:\n");
    Type *condty = if_stmt->cond->accept(this);
    poison_stmt(condty);
    if (condty != this->type_table.bool_type) {
        typecheck_error(if_stmt->loc, "In if statement, the ",
                        "condition expression must have `boolean` type but has: `",
                        condty->name(), "`");
    }
    /// Codegen ///
    llvalue_t cond = __expr_context.llval;
    // Handle constant condition. Eliminate the branch
    // and elide the never-executed body.
    if (cond.kind == LLVALUE::CONST) {
        // Elide either the `if` body or the `else` body
        // depending on the condition constant value.
        // Assume that the user does not want to type-
        // check the elided body (IIRC if(0) in Java
        // works like #ifdef 0 in C).
        if (cond.val) {
            assert(if_stmt->then);
            if_stmt->then->accept(this);
        } else {
            assert(if_stmt->else_);
            if_stmt->else_->accept(this);
        }
        return;
    }
    
    // Generate the 3 labels used. According to the condition,
    // we either go to if_lbl or else_lbl. Both bodies at the end
    // should jump to end_lbl.
    llvm_label_t if_lbl, else_lbl, end_lbl;
    long lbl = gen_lbl();
    if_lbl.construct("if", lbl);
    else_lbl.construct("else", lbl);
    end_lbl.construct("if_end", lbl);
    
    // Emit a comparison and branch
    /*
    llvalue_t cmp_res;
    cmp_res.kind = LLVALUE::REG;
    cmp_res.reg = gen_reg();
    emit("    %%%ld = icmp eq i1 %%%ld, 1\n", cmp_res.reg, cond.reg);
    */
    llvm_branch_cond(cond, if_lbl, else_lbl);

    /*
    //// Algorithm for emission of phi nodes ////
    
    -- A couple of important info --
    Note that this compiler does not use the classic load / store model.
    Clang / LDC and generally most of the LLVM front-ends, whenever we
    use a variable, they load it from memory. And when they assign to
    a variable, they store to. Implementing this model is simple - just
    `alloca` every local variable and use its memory. For parameters, because
    they come in registers, we just `alloca` a copy of them as well.
    This is simple and effective for all kinds of reasons:
    1) First and foremost, because of its simplicity, bugs are less likely to happen.
    2) Debuggers have an easier time when they know that the memory for a variable
       is in a specific place in memory.
    3) LLVM will remove them anyway in optimized builds.

    But I tried to do something better, for locals (variables and params), I tried
    to use registers only by wrapping them in llvalue_t. That scheme makes
    constant-folding and constant-propagation easy.
    An example:
    a = 2;  // underneath, an llvalue is created, which is of kind CONST
    b = a;  // now, `llval` for the local `b` is the previous CONST llvalue and so on.

    However, with this scheme, problems are introduced with conditional code.
    Let's say we have this:
    if (cond) {
        a = 2;
    } else {
        a = 3;
    }

    What llvalue should `a` get? 2 or 3 ? We can't know at compile-time (assuming
    that `cond` is not known at compile-time).
    What we have to do is emit a phi node, i.e. this code above should translate to:
    
        icmp cond, 1
        br i1 cond, if1, else1
    if1:
        br if_end1
    else1:
        br if_end1
    if_end1:
        %1 = phi [ 2, if1 ], [ 3, else1 ]

    Now, the `llval` for the local `a` is the llvalue of kind REG, where its `reg`
    is 1 (i.e. %1).
    So, now the question is how do we generate these phi nodes.
    
    -- The algorithm --
    We have 2 places where we need this scheme: ifs and whiles. Let's explain
    `if` and you'll get the point.
    Note that an `if` is always followed by an `else`. Thus, we always will go either
    to the `if` body or the `else` body. As such, let's say we want to correctly
    emit code for local `a`. There are 4 cases:
    1) Neither of the 2 bodies assignes to `a`. This is the simplest case, keep
       its current `llval`.
    2) The `if` body _only_ assigns to it. Then, keep the (last) value that it
       assigns and then emit a phi node between this assigned value and the
       previous / initial value. For example:
       a = 2;
       if (cond) {
           a = 3;
       } else {
           // No assignment to `a`
       }
       emit a phi node between 2 and 3 for `a`.

    3) The `else` body _only_ assigns to it. Just as case 2)
    4) Both assign to it. Then, emit a phi node between the 2 assigned values.

    So, there are a couple of things we have to do. First of all, track for each
    `if` statement the assignments in its bodies. We keep a TrackAssignment
    for each local of the function, so a TrackAssignmentBuf.
    We use a TrackAssignmentBuf for each nesting level of the function, _not_ for
    every if. For example:
    if () {
        if () {
        }
        ...
        if () {  <-- We can reuse the same buffer for this if as we're finished with
                     with the above. However we can't use it for the outer if because
                     that's still on the run.
        }
    }

    Actually, we keep 2 buffers for each nesting level. One for the if body and
    one for the else. Because we want to save them separately so we can then
    apply the 4 cases above.
    
    Assuming we have those buffers, on the _return_ of processing the
    `if` and `else` bodies, we apply the 4 cases below.
    */

    assert(if_stmt->then);
    assert(if_stmt->else_);

    __expr_context.nesting_level += 1;
    debug_print("IfStatement nesting level: %d\n", __expr_context.nesting_level);
    
    int nesting_level = __expr_context.nesting_level;
    size_t locals_len = __expr_context.method->locals.len;
    // If we surpassed max nesting level, allocate 2 new buffers.
    if (nesting_level > __expr_context.max_nesting_level) {
        __expr_context.max_nesting_level = nesting_level;
        TrackAssignmentBuf if_buf, else_buf;
        if_buf.reserve(locals_len);
        else_buf.reserve(locals_len);
        __expr_context.if_bufs.push(if_buf);
        __expr_context.else_bufs.push(else_buf);
    }
    Method *method = __expr_context.method;
      
    // Note: You'll see the `nesting_level - 1` a lot below. The reason
    // for the -1 is that if you think about, we don't need to keep
    // assignment info for level 0. So, the 0th buffers are for nesting
    // level 1 etc.

    // Clear the 2 buffers in the current nesting level
    for (size_t i = 0; i != locals_len; ++i) {
        TrackAssignment tasgn;
        tasgn.assigned = false;
        // TODO: There might be a better way to decide about this.
        // Essentially, if we're in the first nesting level, we can just get
        // the local value. But if not, then we have to take from the previous level
        // as the `llval` of the local might have changed but we don't want this new value:
        /*
        if (cond) {
            a = new A();  // let's say %5
        } else {
            // Now, `llval` of `a` is %5 but we don't want that.
            if (cond) {
            } else {
                a = new A();
            }
        }
        */
        if (nesting_level > 1) {
            if (__expr_context.in_if) {
                tasgn.llval = __expr_context.if_bufs[nesting_level - 2][i].llval;
            } else {
                tasgn.llval = __expr_context.else_bufs[nesting_level - 2][i].llval;
            }
        } else {
            tasgn.llval = method->locals[i]->llval;
        }
        Local *local = method->locals[i];
        __expr_context.if_bufs[nesting_level - 1][i] = tasgn;
        __expr_context.else_bufs[nesting_level - 1][i] = tasgn;
    }
    
    // Save if we're currently inside an if or an else.
    bool save_in_if = __expr_context.in_if;
    llvm_gen_lbl(if_lbl);
    __expr_context.in_if = true;
    if_stmt->then->accept(this);

    // 2 bufs that track assignments for the 2 bodies we just processed (if and else)
    TrackAssignmentBuf if_buf = __expr_context.if_bufs[nesting_level - 1];
    TrackAssignmentBuf else_buf = __expr_context.else_bufs[nesting_level - 1];
    
    // Before moving to `else`, if `if` assigned, we want to _revert_ the choice,
    // because the `else` may use a variable before assign it. Consider this:
    // a = 0;
    // if (...) {
    //   a = 2;  // This was visited first and now `a->llval` is 2.
    // } else {
    //   b = a;  // Here, we use `a` before we assign to it, so `b` will get `2`,
    //           // which is wrong.
    // }
    // TODO: That was a hacky choice as I found the bug. This bug might mean
    // that we want to revisit a bigger part of the algorithm.
    for (size_t i = 0; i < method->locals.len; ++i) {
        bool assigned_if = if_buf[i].assigned;
        if (assigned_if) {
            Local *local = method->locals[i];
            local->llval = else_buf[i].llval;
        }
    }

    // Save the last label in if, because this is the label / basic block
    // from which we jump to the `end_lbl`. And we need this info
    // for the emission of the phi node.
    llvm_label_t save_last_lbl_in_if = __expr_context.curr_lbl;
    llvm_branch(end_lbl);
    llvm_gen_lbl(else_lbl);
    __expr_context.in_if = false;
    if_stmt->else_->accept(this);
    // Save the last label in `else` for the same reason as above.
    llvm_label_t save_last_lbl_in_else = __expr_context.curr_lbl;
    llvm_branch(end_lbl);
    llvm_gen_lbl(end_lbl);
    __expr_context.in_if = save_in_if;
    
    // Go through all the locals and apply the 4 cases above.
    for (size_t i = 0; i < method->locals.len; ++i) {
        bool assigned_if = if_buf[i].assigned;
        bool assigned_else = else_buf[i].assigned;
        if (assigned_if || assigned_else) {
            Local *local = method->locals[i];
            // If only one of them assigned to it, then the initial
            // value is on the _other_ buffer. If both bodies
            // assigned to it, then just emit the nodes between the 2.
            // So, in any case, emit a phi node between the 2.
            // Note: Because there's no `if` without `else`, we will
            // always go to either one of the 2.
            llvalue_t if_value = if_buf[i].llval;
            llvalue_t else_value = else_buf[i].llval;
            llvalue_t new_val = llvm_phi_node(local->type, if_value, else_value,
                                              save_last_lbl_in_if,
                                              save_last_lbl_in_else);
            // Save the new val for the local
            local->llval = new_val;
            // Pass on the previous nesting level. If we have an assignment
            // in an enclosed level, then we have in its outer as well.
            if (nesting_level > 1) {
                if (__expr_context.in_if) {
                    __expr_context.if_bufs[nesting_level - 2][i].assigned = true;
                    __expr_context.if_bufs[nesting_level - 2][i].llval = new_val;
                } else {
                    __expr_context.else_bufs[nesting_level - 2][i].assigned = true;
                    __expr_context.else_bufs[nesting_level - 2][i].llval = new_val;
                }
            }
        } /* else {
            // Do nothing, no assignment was involved.
        } */
    }
    __expr_context.nesting_level -= 1;
}

struct WhileAssign {
    Local *local;
    unsigned int assigned : 2;
    unsigned int used : 2;
};

// track_while_assignment_for_stmt() and track_while_assignment_for_expr()
// track assignments and usage of locals for while loops for statements
// and expressions respectively.
// There are 3 cases where we can see a local:
// 1) IdExpression
// 2) AssignmentStatement
// 1) ArrayAssignmentStatement
// For each of the 3, we get the id and we loop over the track_buf
// to see if it is a local in the buffer. If so, we track usage and assignment
// by enforcing an ordering. For example, an IdExpression denotes a usage (while
// the other 2 an assignment). For a usage, if we have already seen another usage,
// we do nothing. If not and we have also not seen an assignment, we save 1.
// Otherwise, we save 2 (assuming that `assigned` is 1).
// That 1 and 2 tracks what appears first.

bool track_while_assignment_for_expr(Expression *expr, FuncArr<WhileAssign> track_buf) {
    switch (expr->kind) {
    case EXPR::ID:
    {
        for (WhileAssign& wa : track_buf) {
            Local *local = wa.local;
            if (expr->id == local->id) {
                if (!wa.used) {
                    if (!wa.assigned) {
                        wa.used = 1;
                    } else {
                        wa.used = 2;
                    }
                }
                break;
            }
        }
    } break;
    case EXPR::ARR_ALLOC:
    case EXPR::ARR_LEN:
    case EXPR::NOT:
    {
        track_while_assignment_for_expr(expr->e1, track_buf);
    } break;
    case EXPR::AND:
    case EXPR::CMP:
    case EXPR::PLUS:
    case EXPR::MINUS:
    case EXPR::TIMES:
    case EXPR::ARR_LOOK:
    {
        BinaryExpression *be = (BinaryExpression *) expr;
        track_while_assignment_for_expr(be->e1, track_buf);
        track_while_assignment_for_expr(be->e2, track_buf);
    } break;
    case EXPR::MSG_SEND:
    {
        BinaryExpression *be = (BinaryExpression *) expr;
        track_while_assignment_for_expr(be->e1, track_buf);
        for (Expression *e : be->msd->expr_list) {
            track_while_assignment_for_expr(e, track_buf);
        }
    } break;
    default: break;
    }
    return false;
}

bool track_while_assignment_for_stmt(Statement *stmt, FuncArr<WhileAssign> track_buf) {
    // IMPORTANT: Be sure to first visit the usage, then
    // the assignment in assigments. For example, in
    // AssignmentStatement, we first want to visit the RHS,
    // then track the assignment because if we have say:
    // a = a + 1;
    // then, it counts as a usage before an assignment.
    switch (stmt->kind) {
    case STMT::UNDEFINED: assert(0);
    case STMT::BLOCK:
    {
        BlockStatement *block = (BlockStatement *)stmt;
        for (Statement *s : block->block) {
            track_while_assignment_for_stmt(s, track_buf);
        }
    } break;
    case STMT::ASGN:
    {
        AssignmentStatement *asgn = (AssignmentStatement *)stmt;
        track_while_assignment_for_expr(asgn->rhs, track_buf);
        for (WhileAssign& wa : track_buf) {
            Local *local = wa.local;
            if (asgn->id == local->id) {
                if (!wa.assigned) {
                    if (!wa.used) {
                        wa.assigned = 1;
                    } else {
                        wa.assigned = 2;
                    }
                }
                break;
            }
        }
    } break;
    case STMT::ARR_ASGN:
    {
        ArrayAssignmentStatement *arr_asgn = (ArrayAssignmentStatement *)stmt;
        track_while_assignment_for_expr(arr_asgn->index, track_buf);
        track_while_assignment_for_expr(arr_asgn->rhs, track_buf);

        for (WhileAssign& wa : track_buf) {
            Local *local = wa.local;
            if (arr_asgn->id == local->id) {
                if (!wa.assigned) {
                    if (!wa.used) {
                        wa.assigned = 1;
                    } else {
                        wa.assigned = 2;
                    }
                }
                break;
            }
        }
    } break;
    case STMT::IF:
    {
        IfStatement *if_stmt = (IfStatement *)stmt;
        track_while_assignment_for_expr(if_stmt->cond, track_buf);
        track_while_assignment_for_stmt(if_stmt->then, track_buf);
        track_while_assignment_for_stmt(if_stmt->else_, track_buf);
    } break;
    case STMT::WHILE:
    {
        WhileStatement *while_stmt = (WhileStatement *)stmt;
        track_while_assignment_for_expr(while_stmt->cond, track_buf);
        track_while_assignment_for_stmt(while_stmt->body, track_buf);
    } break;
    case STMT::PRINT:
    {
        PrintStatement *print_stmt = (PrintStatement *)stmt;
        track_while_assignment_for_expr(print_stmt->to_print, track_buf);
    } break;
    }
    return false;
}

llvalue_t while_phi_node(Type *type, llvalue_t prev_value,
                    llvalue_t new_value, llvm_label_t pred_lbl,
                    llvm_label_t loop_lbl, long reg) {
    return llvm_phi_node(type, prev_value, new_value, pred_lbl, loop_lbl, reg);
}

FuncArr<WhileAssign> get_track_buf(SerializedHashTable<Local*> locals) {
    FuncArr<WhileAssign> track_buf(locals.len);
    size_t i = 0;
    for (Local *lo : locals) {
        // Initialize its entry with its local and dumb
        // values for `assigned` and `used` (they will be overriden).
        track_buf.push({lo, 0, 0});
        ++i;
    }
    return track_buf;
}

void MainTypeCheckVisitor::visit(WhileStatement *while_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::WhileStatement\n");
    Type *cond = while_stmt->cond->accept(this);
    poison_stmt(cond);
    llvalue_t cond_val = __expr_context.llval;
    if (cond != this->type_table.bool_type) {
        typecheck_error(while_stmt->loc, "In while statement, the ",
                        "condition expression must have `boolean` type but has: `",
                        cond->name(), "`");
    }

    if (cond_val.kind == LLVALUE::CONST && cond_val.val == 0) {
        // Elide while body because of constant false condition.
        // Assume that the user does not even wants to typecheck it.
        return;
    }
    
    /// Codegen ///

    // Codegen loops as:
    /*
    predecessor:
        cond test code (branch to loop start or after_loop)
    loop_start:
        loop_body
        cond test code (again, branch to loop_start or after_loop)
    after_loop:

    That way, when we're in loop_body, we know whether we came from predecessor
    or from loop_start.
    Note: Here we have already generated the initial cond code when we
    typechecked it.
    */
    
    const char *pred_lbl_str = (while_stmt->pred_lbl == NULL) ?
                                __expr_context.curr_lbl.lbl : while_stmt->pred_lbl;
    while_stmt->pred_lbl = pred_lbl_str;
    llvm_label_t pred_lbl = {pred_lbl_str};
    llvm_label_t loop_lbl, after_loop_lbl;
    long lbl = gen_lbl();
    loop_lbl.construct("while", lbl);
    after_loop_lbl.construct("while_end", lbl);
    
    // Note: We have already tested if the `cond_val` is const with
    // false value. If we reach here and we have a const, then it's
    // definitely true in which case we don't generate the branch, we
    // just get to the loop.
    // However, note that we have to remember that for the while
    // so that we remember if we need to create a phi or not.
    bool generated_cond_branch = false;
    if (cond_val.kind != LLVALUE::CONST) {
        generated_cond_branch = true;
        llvm_branch_cond(cond_val, loop_lbl, after_loop_lbl);
    } else {
        llvm_branch(loop_lbl);
    }
    llvm_gen_lbl(loop_lbl);

    // There are 3 cases for a local inside a loop:
    // 1) No assignment to the local.
    // 2) We have at least one assignment and we have a use _after_ the _first_ assignment.
    // 3) We have at least one assignment and we have a use _before_ the _first_ assignment.

    // We want to insert a phi node for locals in case 3. To do that, we use the `track_buf`.
    // This keeps an `assigned` and `used` for every local. We then "visit" the whole
    // loop body and condition to track in what case we are. To do that, we use
    // an ordering in `assigned` and `used`. After finishing, either they're both
    // 0, or one is bigger than the other denoting the order.

    SerializedHashTable<Local*> locals = __expr_context.method->locals;
    FuncArr<WhileAssign> track_buf = get_track_buf(locals);

    // Track first the body and then the cond,
    // as the condition will be put on the bottom.
    track_while_assignment_for_stmt(while_stmt->body, track_buf);
    track_while_assignment_for_expr(while_stmt->cond, track_buf);

    /* -- Main algorithm for insert phi nodes --

    Assuming that we have an ordering now, we need to handle case 3) (from above)
    locals. For those we will insert a phi node at the start of the loop
    that decides between the previous value (the value before the loop) and
    the value inside the loop. Let's note some problems:
    1) We have to know the value inside the loop _before_ we actually generate
       the loop. This is kind of impossible so there are 2 ways:
       a) Generate the loop (say into a buffer), then read this IR, find the values
          and then regenerate the loop with the suitable phi nodes on the start.
       b) Generate the loop (but by disabling actual codegen, i.e. printing), find
          the values, generate the phi nodes and then regenerate the loop as it was
          never generated.
       We will do b) as it is of course way easier and faster. But that comes later.
       Before that we have to solve another problem:
    2) To do the first fake generation that was described 1b) above, we have to know
       the register that was assigned to the phi node, _before_ we can generate
       the phi node.
    
    To see both 1) and 2) into action, consider this:
        
       ```
       a = 3;
       while (a < 4) {
          a = a + 1;
       }
       ```
        
       That has to generate something like this:
       ```
           // ... (`a` has gotten constant value 3 here)
       while1:
           %1 = phi i32 [ 3, entry ], [ %2, while1 ]
           %2 = add i32 %1, 1
           %3 = icmp slt i32 %2, 4
           br i1 %3, label while1, label while_end1
       
       while_end1:
           // ...
       ```

       Note that to generate the phi node in %1, we have to know that `a`
       gets the llvalue %2. But, we can't know that _before_ we generate the body.
       So, let's say that we go to generate it. Well, to generate it, when we
       go to generate code for %2, we have to know that `a` has the llvalue %1.
       If we _don't_ somehow do that and do the generation normally, then when we
       generate code for %2, the value that will be assigned to `a` will be the
       constant value 3. And that's of course wrong code generation.

       To solve that we decide the register in which we will save the phi node
       before we actually. Because of the ordering (which remember, comes before all that),
       we know e.g. that we have to generate a phi node for `a`. At this point we can't
       generate the phi node, but we can decide that its value will be saved e.g. in %1.
       Then, we save the llvalue of `a` to be %1 (e.g. local->llval = ...). In that way,
       when we do the first fake generation of, when we go to generate %2, it uses %1
       as the llvalue of `a`. Then of course `a` gets as llvalue the %2.
       And _now_, after finishing the fake generation, we know that `a` got assigned
       as llvalue the %2. And so, we generate the phi node between the constant value
       3 (the previous value i.e. that before the loop) and %2 (the new value).

       Ok, to do all that we need 2 buffers:
       1) `previous_values` holds the values of the locals _before_ the loop.
       2) `new_assigned_values` holds the values that we assign a priori to the phi
          nodes that _will_ be generated.
    */

    FuncArr<llvalue_t> previous_values, new_assigned_values;
    previous_values.reserve(locals.len);
    new_assigned_values.reserve(locals.len);
    // Initialize the 2 buffers.
    for (Local *local : locals) {
        previous_values.push(local->llval);
        // DUMB VALUE
        new_assigned_values.push(const_llv(0, {}));
    }

    /// Decide for which locals we have to make a phi node
    /// and get a register for the phi node (that will be generated later).
    size_t i = 0;
    for (WhileAssign wa : track_buf) {
        assert(i < locals.len);
        // Either no use and no assignment or they are not equal (since
        // we should have ordering).
        assert((!wa.assigned && !wa.used) || (wa.used != wa.assigned));
        if (wa.assigned && wa.used && wa.used < wa.assigned) {
            Local *local = locals[i];
            new_assigned_values[i].kind = LLVALUE::REG;
            new_assigned_values[i].reg = gen_reg();
            local->llval = new_assigned_values[i];
        }
        ++i;
    }

    /// Do the fake generation (by disabling codegen).
    /// That requires some boilterplate to save and restore the state of registers
    /// and labels.

    assert(while_stmt->body);
    bool save_codegen = config.codegen;
    // TODO: We should have a save state / restore state.
    // Get the last register but we actually have to reset it immediately.
    long save_reg = gen_reg();
    set_reg(save_reg);
    long save_lbl = gen_lbl();
    set_lbl(save_lbl);
    config.codegen = false;
    while_stmt->body->accept(this);
    while_stmt->cond->accept(this);
    const char *last_lbl_str = (while_stmt->last_lbl == NULL) ?
                                __expr_context.curr_lbl.lbl : while_stmt->last_lbl;
    while_stmt->last_lbl = last_lbl_str;
    llvm_label_t last_lbl = {last_lbl_str};
    set_reg(save_reg);
    set_lbl(save_lbl);
    config.codegen = save_codegen;

    
    /// Now that we know the llvalue that the locals got in the loop,
    /// generate the phi nodes. Note that they're locals that might have been
    /// assigned but either never used or used after the assignment. In these
    /// cases we don't want to insert a phi node but we do want to restore their
    /// llvalue so that the actual code generation is correct.
    i = 0;
    for (WhileAssign wa : track_buf) {
        Local *local = wa.local;
        if (wa.assigned && wa.used && wa.used < wa.assigned) {
            long reg = new_assigned_values[i].reg;
            local->llval = while_phi_node(local->type, previous_values[i], local->llval,
                                          pred_lbl, last_lbl, reg);
        } else if (wa.assigned) {
            // Restore in the general case
            local->llval = previous_values[i];
        }
        ++i;
    }
    
    /// Do the actual code generation (note that we generate the cond on the
    /// bottom etc. - as described above on how while loops are generated / transformed).
    while_stmt->body->accept(this);
    while_stmt->cond->accept(this);
    cond_val = __expr_context.llval;
    llvm_branch_cond(cond_val, loop_lbl, after_loop_lbl);
    llvm_gen_lbl(after_loop_lbl);
    
    /// Last but not least, we want to generate a phi nodes for all
    /// the locals that were assigned inside the loop (regardless of the usage).
    /// Of course the phi will be between their before-loop-llvalue and the
    /// value they get assigned inside the loop.

    // Generate a phi node after the while. Do that only if the condition
    // was not known and so a conditional branch was inserted.
    if (generated_cond_branch) {
      i = 0;
      for (WhileAssign wa : track_buf) {
          Local *local = wa.local;
          llvalue_t prev_value = previous_values[i];
          llvalue_t new_value = local->llval;
          if (wa.assigned) {
              local->llval = llvm_phi_node(local->type, prev_value, new_value,
                                           pred_lbl, last_lbl);
          }
          ++i;
      }
    }
}

void MainTypeCheckVisitor::visit(PrintStatement *print_stmt) {
    LOG_SCOPE;
    debug_print("MainTypeCheck::PrintStatement\n");
    // A print statement can have only integer.
    Type *ty = print_stmt->to_print->accept(this);
    poison_stmt(ty);
    if (ty != this->type_table.int_type) {
        typecheck_error(print_stmt->to_print->loc,
                        "Print statement accepts only integer expressions.");
    }
    /// Codegen ///
    llvalue_t to_print = __expr_context.llval;
    cgen_print_stmt(to_print);
}

const char *Type::name() const {
    switch (kind) {
    case TY::UNDEFINED: return "Undefined Type (Error)";
    case TY::INT: return "int";
    case TY::ARR: return "int[]";
    case TY::BOOL: return "boolean";
    case TY::ID: return ((IdType *)this)->id; 
    default: assert(0);
    }
}

Method::Method(MethodDeclaration *method_decl) {
    id = method_decl->id;
    size_t locals_size = method_decl->params.len + method_decl->vars.len;
    locals.reserve(locals_size);
    param_len = method_decl->params.len;
    stmts = method_decl->stmts;
    ret_expr = method_decl->ret;
    loc = method_decl->loc;
}


Method *Method::construct_main_method(DeclarationVisitor *visitor, MainClass *main_class) {
    Method *method = (Method *) allocate_zero(sizeof(Method), MEM::TYPECHECK);
    method->id = main_method_string;
    size_t locals_size = main_class->vars.len;
    method->locals.reserve(locals_size);

    for (LocalDeclaration *var : main_class->vars) {
        if (method->locals.find(var->id)) {
            typecheck_error(var->loc, "Variable `", var->id, "` is already defined",
                            " in method `", method->id, "`");
        } else {
            Var *v = var->accept(visitor);
            method->locals.insert(v->id, v);
        }
    }

    method->param_len = 0;
    method->stmts = main_class->stmts;
    method->ret_type = visitor->type_table.int_type;
    method->ret_expr = NULL;
    method->loc = main_class->loc;
    return method;
}
