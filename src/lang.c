/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 2 "lang.y" /* yacc.c:339  */


/* DO NOT EDIT!
 *
 * This file is created automatically by make_func from
 * the template prolang.y according to the specs in func_spec.
 */

#line 3 "prolang.y"
#line 4 "prolang.y"
/*---------------------------------------------------------------------------
 * LPC compiler
 *
 *---------------------------------------------------------------------------
 * TODO: Some code parts 'know' which instructions are xcodes and which normal.
 * TODO:: Conditional compiles would be nice there.
 * TODO: The handling of virtual inherits is a bit vague, too.
 *
 * This is the grammar definition and bytecode compiler of LPC. However, this
 * file is not passed to byacc directly, but first preprocessed by make_func,
 * among other things to synchronise the tokens with the other bytecodes
 * (reason being that yacc doesn't know an include construct). The following
 * keywords are recognized and replaced by make_func:
 *
 *    %line:  generates a #line statement to synchronize the C compiler.
 *
 *    %hookmap <hookname>:<value>,...,<hookname>:<value>
 *       Generates a lookup table <hookname> -> <value>. Unspecified
 *       driverhook entries are given the value 0.
 *
 * In addition, make_func implements a simple preprocessor using the
 * keywords %if, %elif, %else and %endif so that parsing rules can be
 * activated/deactivated from config.h defines.
 *---------------------------------------------------------------------------
 * To compile a file, open the file  to yield a filedescriptor 'fd', then call
 *
 *     compile_file(fd, <filename>, <isMasterObj>);
 *
 * then close the file again. The compiled program is 'returned' in
 * the global compiled_prog - on error, this variable is returned as NULL.
 * If after the compilation the variable inherit_file is
 * not NULL, the compilation failed because it encountered an
 * "inherit 'name'" statement for which no object could be found: the
 * 'name' was stored in inherit_file and has to be compiled first.
 *
 * It is the task of the caller to make sure that the compiler is not called
 * recursively.
 *
 * If there is any initialization of a global variable, a function '__INIT'
 * is generated with the initialization code. The code is generated in
 * fragments whenever a variable initialization is encountered; the fragments
 * are therefore potentially spread over the whole program code. The fragments
 * are linked by JUMP instructions with jump to the next fragment, just
 * the last fragment ends in a RETURN0.
 *
 * When inheriting from another object, a call will automatically be made
 * to call __INIT in that code from the current __INIT.
 *---------------------------------------------------------------------------
 * The compiler is a simple one-pass compiler with immediate code generation.
 * The problem of forward references is solved with various backpatching
 * structures (explained where declared).
 *
 * The most tricky part is that of lvalue (and with it reference) generation
 * in contexts where rvalues are sensible as well. The approach is to
 * generate rvalues, but keep the position, and size and alternatives of the
 * instruction(s) in a struct lrvalue, so that a later change into lvalues is
 * possible.
 *
 * Another challenge is the compilation of inline closures, as they
 * are implemented as separate lfuns (with synthetic names), but encountered
 * in the middle of a regular lfun or even another inline closure. The
 * compiler therefore stores his state when it recognizes another
 * inline closures, and then resets its state as if a normal lfun is
 * compiled. When the inline closure is complete, its data is moved into
 * a backup storage area, and the compiler restores its previous state.
 *---------------------------------------------------------------------------
 */

#undef lint  /* undef so that precompiled headers can be used */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>

#include "prolang.h"

#include "array.h"
#include "backend.h"
#include "closure.h"
#include "exec.h"
#include "gcollect.h"
#include "interpret.h"
#include "instrs.h"
#include "lex.h"
#include "main.h"
#include "mapping.h"
#include "mstrings.h"
#include "object.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stdstrings.h"
#include "structs.h"
#include "svalue.h"
#include "swap.h"
#include "switch.h"
#include "types.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "pkg-python.h"
#include "i-eval_cost.h"

#include "../mudlib/sys/driver_hook.h"

#undef DEBUG_INLINES
  /* Define this to activate lots of debugging output during the compilation
   * of inline closures.
   */

#define lint  /* redef again to prevent spurious warnings */

#define YYMAXDEPTH        600

/*-------------------------------------------------------------------------*/

typedef struct block_scope_s       block_scope_t;
typedef struct const_list_s        const_list_t;
typedef struct const_list_svalue_s const_list_svalue_t;
typedef struct struct_init_s       struct_init_t;
typedef struct efun_shadow_s       efun_shadow_t;
typedef struct mem_block_s         mem_block_t;

/*-------------------------------------------------------------------------*/
/* Exported result variables */

int32 current_id_number = 0;
  /* The id-number of the compiled program.
   */

int num_virtual_variables;
  /* Number of virtual variables.
   * When creating the bytecode, the non-virtual variable indices
   * are offset by this value, in effect collecting the virtual
   * variables at the start of the variable indices.
   */

program_t *compiled_prog;
  /* After yyparse(), the finished program.
   */

string_t *inherit_file;
  /* Used as a flag: if it is set to a tabled string after yyparse(),
   * this string should be loaded as an object, and the original object
   * must be loaded again.
   */

int num_parse_error;
  /* Number of errors in the compile.
   */

Bool variables_defined;
  /* TRUE: Variable definitions have been encountered.
   */

/*-------------------------------------------------------------------------*/
/* Table which hook may be of which type.
 * It is here because make_func has to touch this file anyway, but
 * it will be used by simulate:f_set_driver_hook().
 */

#define SH(x) - -(1 << (x))

short hook_type_map[NUM_DRIVER_HOOKS] =
{
 0,
 0,
      SH(T_CLOSURE),
     SH(T_CLOSURE),
                 SH(T_STRING),
                    SH(T_STRING),
                 SH(T_STRING),
                        SH(T_STRING),
       SH(T_CLOSURE) SH(T_STRING),
 SH(T_CLOSURE) SH(T_STRING) SH(T_MAPPING),
    SH(T_CLOSURE) SH(T_STRING),
                  SH(T_STRING),
   SH(T_CLOSURE)              SH(T_POINTER),
     SH(T_CLOSURE) SH(T_STRING),
         SH(T_CLOSURE) SH(T_STRING),
       SH(T_CLOSURE),
 SH(T_STRING),
        SH(T_CLOSURE) SH(T_STRING),
 SH(T_CLOSURE) SH(T_STRING),
   SH(T_CLOSURE) SH(T_STRING),
 SH(T_CLOSURE) SH(T_STRING),
 SH(T_CLOSURE) SH(T_STRING),
   SH(T_CLOSURE) SH(T_STRING),
 SH(T_NUMBER),
  SH(T_CLOSURE) SH(T_STRING),
};
#undef SH

/*-------------------------------------------------------------------------*/
/* Types */

/* --- struct const_list_s: One element in a constant list ---
 *
 * When initializing variables statically with arrays ({ ... }),
 * a list of these structures is used to collect the information about
 * the array content.
 */

struct const_list_s
#line 212 "prolang.y"
{
    const_list_t *next;
    svalue_t val;
    string_t * member; /* NULL, or the member name to initialize */
};

/* --- struct const_list_svalue_s: Head of a constant list ---
 *
 * When initializing variables statically with arrays ({ ... }),
 * the initializer-svalue_t* will point to an instance of this c_l_svalue_s.
 * In fact, the initializer points to the .head member.
 *
 * The .head svalue_t is a T_ERROR_HANDLER pointing to a deallocation
 * function for the list.
 */

struct const_list_svalue_s
#line 229 "prolang.y"
{
    svalue_t     head;  /* the error handler */
    const_list_t list;  /* First element of the list */
    const_list_t *last_member; /* For nested struct initialisations */
};

/* --- struct struct_init_s: Descriptor for one struct literal member
 *
 * When createing struct literals at runtime, a list of these structures
 * keeps the information about the order and type of members encountered.
 */

struct struct_init_s
#line 242 "prolang.y"
{
    struct_init_t * next;  /* Next member entry */
    fulltype_t      type;  /* Type of expression */
    string_t      * name;  /* Member name, or NULL if unnamed */
};

/* --- struct efun_shadow_s: Store info about masked efuns ---
 *
 * This structure is used when global identifiers shadow efun names.
 */

struct efun_shadow_s
#line 254 "prolang.y"
{
    efun_shadow_t *next;    /* Linkpointer for the list of shadows */
    ident_t       *shadow;  /* Identifier of the shadow */
};


/*-------------------------------------------------------------------------*/
/* Macros */

#define NON_VIRTUAL_OFFSET_TAG 0x4000
   /* Tag or'ed on inherit.variable_index_offset for non-virtual
    * inherits for the duration of the compilation.
    * The variable_index_offsets of such marked variables do not
    * yet the the num_virtual_variables offset into account.
    */

#define align(x) (((x) + (sizeof(char*)-1) ) & ~(sizeof(char*)-1) )

#define defined_function(s) \
    ((s)->type == I_TYPE_GLOBAL ? (s)->u.global.function : -1)
  /* Return the index of the function <s> if global (and therefore existing),
   * and -1 otherwise.
   */

#define set_fulltype(t, flags, ptr) \
    ( t.t_flags = (flags), t.t_type = (ptr) )
  /* Set the fulltype <t> to type <ptr> with modifiers <flags>.
   */

#define NEW_INHERITED_INDEX (0xfffff)
  /* While inserting a new inherit, this marks the newly inherited
   * things.
   */

/* Values for %type <number> foreach_expr
 */
#define FOREACH_LOOP  0  /* Normal foreach loop value */
#define FOREACH_REF   1  /* Referenced foreach loop value */
#define FOREACH_RANGE 2  /* Integer range as loop value */

/*-------------------------------------------------------------------------*/
/* The generated information (code and otherwise) is kept in several
 * memory areas, each of which can grow dynamically and independent
 * from the others.
 *
 * The first NUMPAREAS are save with the program code after compilation,
 * the others are of internal use for the compiler only.
 */

enum e_saved_areas {
   A_PROGRAM = 0
    /* (bytecode_t): Program code.
     */
 , A_STRINGS
    /* (string_t*) Strings used by the program, all tabled.
     */
 , A_VARIABLES
    /* (variable_t) The information for all non-virtual variables.
     */
 , A_VIRTUAL_VAR
    /* (variable_t) The information for all virtual variables.
     */
 , A_LINENUMBERS
    /* (char) The linenumber information.
     */
 , A_INHERITS
    /* (inherit_t) The information for the inherited programs.
     */
 , A_UPDATE_INDEX_MAP
    /* (unsigned short) New variable and function indices for the
     * variables and functions of an obsolete virtually inherited
     * program.
     */
 , A_ARGUMENT_TYPES
    /* (lpctype_t*) Types of the arguments of all functions with
     * typechecking. The argument types for a specific function
     * can be found using the ARGUMENT_INDEX. All entries
     * are counted references.
     */
 , A_ARGUMENT_INDEX
    /* (unsigned short) Index of the first argument type of function <n>.
     * INDEX_START_NONE is used for functions with no type information.
     */

 , A_INCLUDES
    /* (include_t) Tabled descriptors of all included files, in the order
     * of appearance.
     */

 , A_STRUCT_DEFS
    /* (struct_def_t) Tabled descriptors of all struct definitions.
     */

 , NUMPAREAS  /* Number of saved areas */
};

typedef bytecode_t     A_PROGRAM_t;
typedef string_t*      A_STRINGS_t;
typedef variable_t     A_VARIABLES_t;
typedef variable_t     A_VIRTUAL_VAR_t;
typedef char           A_LINENUMBERS_t;
typedef inherit_t      A_INHERITS_t;
typedef unsigned short A_UPDATE_INDEX_MAP_t;
typedef lpctype_t*     A_ARGUMENT_TYPES_t;
typedef unsigned short A_ARGUMENT_INDEX_t;
typedef include_t      A_INCLUDES_t;
typedef struct_def_t   A_STRUCT_DEFS_t;

enum e_internal_areas {
   A_FUNCTIONS = NUMPAREAS
     /* (function_t): Function definitions
      */

 , A_STRING_NEXT
   /* (int) During compilation, the strings in A_STRINGS are organized
    * in a hash table (prog_string_indizes/_tags). The hash chains are
    * linked together using the indizes in this area. The end of
    * a chain is marked by a negative next-index.
    */

 , A_LOCAL_TYPES
   /* (lpctype_t*) The full types of local and context variables.
    * For normal functions, only the beginning of the area is used.
    * The rest is used stack-wise for nested inline closures.
    */

 , A_INLINE_PROGRAM
    /* (bytecode_t, char): Program and linenumbers saved from the compiled
     * but not yet inserted inline closures.
     */
 , A_INLINE_CLOSURE
    /* (inline_closure_t): The currently pending inline closures. The lexical
     * nesting is achieved with the .prev/.next pointers in the
     * inline_closure_t structures.
     */

 , A_STRUCT_MEMBERS
    /* (struct_member_t) While a struct definition is parsed, the member
     * descriptors are collected here.
     */

 , A_LVALUE_CODE
    /* (bytecode_t): Area where to put lvalue bytecodes.
     * Used for <expr4> which compile rvalue and lvalue code simultaneously.
     * Also used for <lvalue> which doesn't put the lvalue code directly
     * into the program, because the lvalue code need to be there
     * after the code of the rhs expression.
     */
 , NUMAREAS  /* Total number of areas */
};

typedef struct inline_closure_s inline_closure_t;

typedef function_t       A_FUNCTIONS_t;
typedef int              A_STRING_NEXT_t;
typedef fulltype_t       A_LOCAL_TYPES_t;
typedef bytecode_t       A_INLINE_PROGRAM_t;
typedef inline_closure_t A_INLINE_CLOSURE_t;
typedef struct_member_t  A_STRUCT_MEMBERS_t;
typedef bytecode_t       A_LVALUE_CODE_t;

/* --- struct mem_block_s: One memory area ---
 * Every mem_block keeps one memory area. As it grows by using realloc(),
 * no pointers should be kept into such an area (offsets are ok).
 */

struct mem_block_s
#line 421 "prolang.y"
{
    char    *block;         /* Pointer to the allocated memory */
    mp_uint  current_size;  /* Used size of the mem_block */
    mp_uint  max_size;      /* Allocated size of the mem_block */
};


#define START_BLOCK_SIZE  2048
  /* Initial size of an area/mem_block.
   */

static mem_block_t mem_block[NUMAREAS];
  /* All memory areas.
   */

#define GET_BLOCK(BLOCK_NAME)            ((BLOCK_NAME##_t*)(mem_block[BLOCK_NAME].block))
#define GET_BLOCK_COUNT(BLOCK_NAME)      (mem_block[BLOCK_NAME].current_size / sizeof(BLOCK_NAME##_t))
#define GET_BLOCK_SIZE(BLOCK_NAME)       (mem_block[BLOCK_NAME].current_size)

#define PROGRAM_BLOCK           GET_BLOCK(A_PROGRAM)
  /* The current program block, properly typed.
   */

#define CURRENT_PROGRAM_SIZE    GET_BLOCK_SIZE(A_PROGRAM)
  /* The current program size.
   */


#define LINENUMBER_BLOCK        GET_BLOCK(A_LINENUMBERS)
  /* The current linenumber block, properly typed.
   */

#define LINENUMBER_SIZE         GET_BLOCK_SIZE(A_LINENUMBERS)
  /* The current linenumber data size.
   */


#define FUNCTION(n)             (GET_BLOCK(A_FUNCTIONS) + (n))
  /* Return the function_t* for function number <n>.
   */

#define FUNCTION_COUNT          GET_BLOCK_COUNT(A_FUNCTIONS)
  /* Number of function_t stored so far in A_FUNCTIONS.
   */


#define INHERIT_COUNT           GET_BLOCK_COUNT(A_INHERITS)
  /* Number of inherit_t stored so far in A_INHERITS.
   */

#define UPDATE_INDEX_MAP_COUNT  GET_BLOCK_COUNT(A_UPDATE_INDEX_MAP)
  /* Offset of the next free entry in A_INDEX_MAP.
   */

#define ARGUMENT_INDEX(n)       GET_BLOCK(A_ARGUMENT_INDEX)[n]
  /* Lookup the start index of the types for function number <n>.
   */


#define ARGTYPE_COUNT           GET_BLOCK_COUNT(A_ARGUMENT_TYPES)
  /* Number of lpctype_t* stored so far in A_ARGUMENT_TYPES.
   */

#define ARGUMENT_TYPE(n)        GET_BLOCK(A_ARGUMENT_TYPES)[n]
  /* Index the lpctype_t* <n>.
   */

#define NV_VARIABLE(n)          (GET_BLOCK(A_VARIABLES) + (n))
  /* Return the variable_t* for the non-virtual variable <n>.
   */

#define NV_VARIABLE_COUNT       GET_BLOCK_COUNT(A_VARIABLES)
#define V_VARIABLE_COUNT        GET_BLOCK_COUNT(A_VIRTUAL_VAR)
  /* Number of variables stored so var in A_VARIABLES resp. A_VIRTUAL_VAR.
   */

#define V_VARIABLE(n)           (GET_BLOCK(A_VIRTUAL_VAR) + (n) - VIRTUAL_VAR_TAG)
  /* Return the variable_t* for the virtual variable <n> (still including
   * the offset).
   */

#define VARIABLE(n)             ((n) & VIRTUAL_VAR_TAG ? V_VARIABLE(n) : NV_VARIABLE(n))
  /* Return the variable_t* for the variable <n>, virtual or not.
   */

#define INHERIT(n)              GET_BLOCK(A_INHERITS)[n]
  /* Index the inherit_t <n>.
   */

#define INHERIT_COUNT           GET_BLOCK_COUNT(A_INHERITS)
  /* Return the number of inherits encountered so far.
   */

#define STRUCT_DEF(n)           GET_BLOCK(A_STRUCT_DEFS)[n]
  /* Index the struct_def_t <n>.
   */

#define STRUCT_COUNT            GET_BLOCK_COUNT(A_STRUCT_DEFS)
  /* Return the number of structs encountered so far.
   */

#define STRUCT_MEMBER(n)        GET_BLOCK(A_STRUCT_MEMBERS)[n]
  /* Index the struct_member_t <n>.
   */

#define STRUCT_MEMBER_COUNT     GET_BLOCK_COUNT(A_STRUCT_MEMBERS)
  /* Return the number of struct members stored.
   */

#define PROG_STRING(n)          GET_BLOCK(A_STRINGS)[n]
  /* Index the pointer for program string <n>.
   */

#define STRING_COUNT            GET_BLOCK_COUNT(A_STRINGS)
  /* Return the number of program strings encountered so far.
   */

#define PROG_STRING_NEXT(n)     GET_BLOCK(A_STRING_NEXT)[n]
  /* Index the chain-index for program string <n>.
   */

#define STRING_NEXT_COUNT       GET_BLOCK_COUNT(A_STRING_NEXT)
  /* Return the number of entries in A_STRING_NEXT.
   */

#define INCLUDE(n)              GET_BLOCK(A_INCLUDES)[n]
  /* Returns the include_t <n>.
   */

#define INCLUDE_COUNT           GET_BLOCK_COUNT(A_INCLUDES)
  /* Return the total number of include files encountered so far.
   */

#define LOCAL_TYPE_COUNT        GET_BLOCK_COUNT(A_LOCAL_TYPES)
  /* Return the total number of types.
   */

#define LOCAL_TYPE(n)           GET_BLOCK(A_LOCAL_TYPES)[n]
  /* Return the local/context var type at index <n>.
   */

#define INLINE_PROGRAM_BLOCK(n) (GET_BLOCK(A_INLINE_PROGRAM) + (n))
  /* Return the inline-closure program block at address <n>, properly typed.
   */

#define INLINE_PROGRAM_SIZE     GET_BLOCK_SIZE(A_INLINE_PROGRAM)
  /* The current program size.
   */

#define INLINE_CLOSURE(n)       GET_BLOCK(A_INLINE_CLOSURE)[n]
  /* Return the inline-closure program block at address <n>, properly typed.
   */

#define INLINE_CLOSURE_COUNT    GET_BLOCK_COUNT(A_INLINE_CLOSURE)
  /* Return the number of saved inline-closures.
   */

#define LVALUE_BLOCK            GET_BLOCK(A_LVALUE_CODE)
  /* The current block for lvalue code, propertly typed.
   */

#define LVALUE_BLOCK_SIZE       GET_BLOCK_SIZE(A_LVALUE_CODE)
  /* The current size of the lvalue code.
   */

#if MAX_LOCAL > 256
  /* There are only 8 bit opcodes for accessing
   * local variables.
   */
#  error "Maximum number of local variables too high."
#endif

#define CONTEXT_VARIABLE_BASE 256
  /* Indicates context variables in ident->u.local.num
   * when deriving new context variables from them.
   * Should be greater or equal to MAX_LOCAL.
   */



/*-------------------------------------------------------------------------*/
/* Information describing nested local blocks (scopes).
 */
struct block_scope_s
#line 605 "prolang.y"
{
    int     first_local;  /* Number of first local defined in this scope */
    int     num_locals;   /* Number of locals defined in this scope */
    int     num_cleared;
      /* Number of locals that have been cleared by earlier CLEAR_LOCALS */
    Bool    clobbered;
      /* Local variables beyond num_locals may be clobbered */
    mp_uint addr;
      /* Address of CLEAR_LOCALS instruction, needed for backpatching */
    Bool    accessible;   /* True if variables of this block are accessible */
};

static block_scope_t block_scope[COMPILER_STACK_SIZE];
  /* A static stack of block scopes, indexed by <block_depth>-1.
   * TODO: This should be dynamic.
   */

static int block_depth;
  /* The nesting depth of blocks ( '{ ... }' ), used to distinguish
   * local variable definitions.
   * block_depth = 0: not used, would mean 'global'
   *             = 1: function arguments
   *             = 2: function local variables
   *             > 2: vars of nested blocks within the function
   */

/*-------------------------------------------------------------------------*/
/* Information describing inline closures.
*/

struct inline_closure_s
#line 636 "prolang.y"
{
    mp_int prev;
      /* Index of the enclosing inline closure, or -1 if none.
       */

    mp_int next;
      /* Index of an enclosed inline closure.
       * This is only used temporarily.
       */

    /* --- Compilation information --- */
    mp_uint end;
      /* While compiling the closure: end of the program code before
       * the closure. It is not identical to .start because of alignment.
       */
    mp_uint start;
      /* While compiling the closure: start address of the code in A_PROGRAM.
       * For pending closures: start address of the code in A_INLINE_PROGRAM.
       */
    mp_uint length;
      /* Length of the compiled code.
       */
    mp_uint li_start;
      /* While compiling the closure: start address of the data in
       * A_LINENUMBERS.
       * For pending closures: start address of the data in A_INLINE_PROGRAM.
       */
    mp_uint li_length;
      /* Length of the linenumber data.
       */
    int function;
      /* Function index
       */
    fulltype_t returntype;
      /* The return type (uncounted reference).
       */
    ident_t * ident;
      /* The ident entry with the function name.
       */
    int num_args;
      /* Number of arguments.
       */
    Bool parse_context;
      /* TRUE if the context variable definitions are parsed.
       */
    int start_line;
      /* Starting line number, used to adjust the generated linenumbers.
       */
    int end_line;
      /* Ending line number, used to adjust the generated linenumbers.
       */

    /* --- Saved Globals --- */
    void * include_handle;
      /* Current include state.
       */
    lpctype_t * exact_types;
      /* The enclosing return type setting (reference not counted).
       */
    int block_depth;
      /* Block depth at definition point.
       * +1: Context depth
       * +2: Argument depth
       */
    int num_locals;
    int max_num_locals;
      /* Current and max number of locals at definition point.
       */
    int break_stack_size;
    int max_break_stack_size;
      /* Current and max break stack size at definition point.
       */
    mp_uint full_local_type_start;
    mp_uint full_context_type_start;
      /* Start indices of the local/context variable type information
       * in A_LOCAL_TYPES.
       */
    mp_uint full_local_type_size;
      /* Current size of the A_LOCAL_TYPES memblocks.
       */
};

static inline_closure_t * current_inline;
  /* NULL, or pointer to the current inline_closure_t structure while
   * compiling an inline closure.
   * This variable is also used as flag that we're currently compiling
   * an inline_closure.
   */

static unsigned int inline_closure_id;
  /* ID Number for the inline closure name.
   */

/*-------------------------------------------------------------------------*/
/* Other Variables */

static Bool disable_sefuns;
  /* TRUE: Sefuns will be ignored.
   */

static char *last_yalloced = NULL;
  /* Head of blocklist allocated with yalloc().
   */

static program_t NULL_program;
  /* Empty program_t structure for initialisations.
   */

static p_int comp_stack[COMPILER_STACK_SIZE];
  /* A stack of addresses (offsets) in the generated program code for
   * later backpatching.
   */

static size_t comp_stackp;
  /* Index of the next unused entry in <comp_stack>.
   */

static p_int last_initializer_end;
  /* Address of the argument of the final JUMP instruction of the
   * previous INIT fragment.
   * A negative value if there is no previous fragment (this also means
   * that the INIT functions hasn't been created yet).
   */

static p_int first_initializer_start;
  /* Address of the 'num_arg' byte in the function header of the first
   * INIT fragment.
   */

static Bool variables_initialized;
  /* TRUE if the code for all variables has been created.
   */

static fulltype_t def_function_returntype;
static ident_t *  def_function_ident;
static int        def_function_num_args;
  /* Globals to keep the state while a function is parsed:
   *   _returntype: the returntype (uncounted reference)
   *   _ident:      the function's identifier.
   *   _num_args:   number of formal arguments.
   */

static mem_block_t type_of_arguments;
  /* The fulltypes of arguments when calling functions must be saved,
   * to be used afterwards for checking. And because function calls
   * can be done as an argument to a function calls, a stack of argument types
   * is needed. This stack does not need to be freed between compilations,
   * but will be reused.
   */

static A_LOCAL_TYPES_t* type_of_locals = NULL;
  /* The full types of the local variables.
   * Points to a location in mem_block A_LOCAL_TYPES, it is NULL between
   * compilations.
   */

static A_LOCAL_TYPES_t* type_of_context = NULL;
  /* The full types of the context variables.
   * Points to a location in mem_block A_LOCAL_TYPES, it is NULL between
   * compilations.
   */

static int current_number_of_locals = 0;
  /* Current (active) number of local variables at this point in the
   * function.
   */

static int max_number_of_locals = 0;
  /* Total number of local variables used in this function so far.
   */

static int max_number_of_init_locals = 0;
  /* Total number of local variables used in the __INIT function so far.
   * (These are context variables of inline closures used for initialization.)
   */

static ident_t *all_locals = NULL;
  /* List of defined local variables, listed in reverse order of definition.
   * This also means that the variables are listed in reverse order of
   * nested block scopes.
   */

static lpctype_t * exact_types;
  /* If NULL, don't check nor require argument and function types.
   * Otherwise it's the return type of the function. (Reference
   * is not counted, a counted reference is held in the parser tokens.)
   */

static funflag_t default_varmod;
static funflag_t default_funmod;
  /* Default visibility modifiers for variables resp. function.
   */

static int heart_beat;
  /* Number of the heart_beat() function, or < 0 if none.
   */

static int call_other_sefun;
  /* Index of the call_other() sefun, or < 0 if none;
   */

static ident_t *all_globals = NULL;
  /* List of all created global identifiers (variables and functions).
   */

static efun_shadow_t *all_efun_shadows = NULL;
  /* List of all shadow markers for efuns shadowed by global identifiers.
   */

static p_int switch_pc;
  /* When compiling a switch, this is the address of the first byte
   * after the SWITCH instruction.
   */

static bc_offset_t current_break_address;
  /* If != 0, the compiler is in a break-able environment and this
   * variable points to the first offset-part of a series of LBRANCHes
   * which implement the break statement. Stored in every offset-part
   * is the address of the offset of the next LBRANCH in the series. The
   * last FBRANCH is marked by having a negative offset value.
   *
   * There are a few special values/flags for this variable:
   */
#define BREAK_ADDRESS_MASK   0x0003ffff
  /* Mask for the offset-address part of the variable.
   */
#define BREAK_ON_STACK        (0x04000000)
  /* Bitflag: true when the break-address is stored on the break stack,
   * and therefore the BREAK instruction has to be used.
   */
#define BREAK_FROM_SWITCH     (0x08000000)
  /* TODO: We are compiling a switch instruction.
   */
#define CASE_LABELS_ENABLED   (0x10000000)
  /* The "case" and "default" statements are allowed since we're
   * compiling a switch(). This flag is turned off for loops or
   * conditions embedded in a switch().
   */
#define BREAK_DELIMITER       (-0x20000000)
  /* Special value: no break encountered (yet).
   */

static bc_offset_t current_continue_address;
  /* If != 0, the compiler is in a continue-able environment and this
   * variable points to the first offset-part of a series of  FBRANCHes
   * which implement the continue statement. Stored in every offset-part
   * is the address of the offset of the next FBRANCH in the series. The
   * last FBRANCH is marked by having a negative offset value.
   *
   * A special case are continues inside a switch, as for these the
   * switch()es have to be terminated too using the BREAK_CONTINUE
   * instructions (which also have an offset-part). The c_c_a therefore
   * also encodes the switch()-nesting depth in the top bits of the
   * variable.
   */
#define CONTINUE_ADDRESS_MASK   0x0003ffff
  /* Mask for the offset-address part of the variable.
   */
#define SWITCH_DEPTH_UNIT       0x00040000
  /* The switch depth is encoded in multiples of this value.
   * This way we don't have to shift.
   */
#define SWITCH_DEPTH_MASK       0x3ffc0000
  /* Mask for the switch-nesting depth part of the variable.
   */
#define CONTINUE_DELIMITER     -0x40000000
  /* Special value: no continue encountered (yet).
   */

static int current_struct;
  /* Index of the current structure to be defined.
   */

static p_uint last_expression;
  /* If >= 0, the address of the last instruction which by itself left
   * a value on the stack. If there is no such instruction, the value
   * is (unsigned)-1.
   */

static Bool last_string_is_new;
  /* TRUE: the last string stored with store_prog_string() was indeed
   * a new string.
   */

static int prog_string_indizes[0x100];
  /* Hash table for the program strings holding the initial indices
   * for the hash chains.
   */

static string_t *last_string_constant = NULL;
  /* The current (last) string constant, a tabled string.
   * It is also used to optimize "foo"+"bar" constructs.
   */

static int current_break_stack_need = 0;
  /* Current depth of the required switch/break stack at this point
   * in a function.
   */

static int max_break_stack_need = 0;
  /* Total depth of the required switch/break stack for this function.
   * This information is required when computing the 'num_locals'
   * for the function header.
   */

static p_int stored_bytes;
  /* Size of the program at the last time of store_line_number_info().
   */

static p_int stored_lines;
  /* Current linenumber at the last time of store_line_number_info().
   */

static int simple_includes;
  /* Number of simple includes since the last real one.
   */

static p_uint last_include_start;
  /* Address in A_LINENUMBERS of the last include information.
   * It is used to remove information about includes which do
   * not generate information ('simple includes').
   */

static int argument_level;
  /* Nesting level of function call arguments.
   * Used to detect nested function calls, like foo( bar () ).
   */

static Bool got_ellipsis[COMPILER_STACK_SIZE];
  /* Flags indexed by <argument_level>, telling if the current function
   * arguments used the L_ELLIPSIS operator.
   * TODO: This should be dynamic.
   */

static const char * compiled_file;
  /* The name of the program to be compiled. While current_loc.file reflects
   * the name of the source file currently being read, this name is always
   * the program's name. Set by prolog().
   */

  /* A few standard types we often need.
   * We'll initialize them later (using the type functions, so all pointers
   * are correctly set) and then put them into a static storage (and set
   * their ref count to zero).
   */

static bool _lpctypes_initialized = false;
lpctype_t _lpctype_unknown_array, _lpctype_any_array,    _lpctype_int_float,
          _lpctype_int_array,     _lpctype_string_array, _lpctype_object_array;
lpctype_t *lpctype_unknown_array = &_lpctype_unknown_array,
          *lpctype_any_array     = &_lpctype_any_array,
          *lpctype_int_float     = &_lpctype_int_float,
          *lpctype_int_array     = &_lpctype_int_array,
          *lpctype_string_array  = &_lpctype_string_array,
          *lpctype_object_array  = &_lpctype_object_array;


/*-------------------------------------------------------------------------*/
/* Forward declarations */

struct lvalue_s; /* Defined within YYSTYPE aka %union */

static void define_local_variable (ident_t* name, lpctype_t* actual_type, struct lvalue_s *lv, Bool redeclare, Bool with_init);
static void init_local_variable (ident_t* name, struct lvalue_s *lv, int assign_op, fulltype_t type2);
static Bool add_lvalue_code ( struct lvalue_s * lv, int instruction);
static void insert_pop_value(void);
static int insert_inherited(char *, string_t *, program_t **, function_t *, int, bytecode_p);
  /* Returnvalues from insert_inherited(): */
#  define INHERITED_NOT_FOUND            (-1)
#  define INHERITED_WILDCARDED_ARGS      (-2)
#  define INHERITED_WILDCARDED_NOT_FOUND (-3)
static void store_line_number_relocation(int relocated_from);
int yyparse(void);
static void add_new_init_jump(void);
static void transfer_init_control(void);
static void copy_structs(program_t *, funflag_t);
static void new_inline_closure (void);
static int inherit_program(program_t *from, funflag_t funmodifier, funflag_t varmodifier);
static void fix_variable_index_offsets(program_t *);
static short store_prog_string (string_t *str);

/*-------------------------------------------------------------------------*/
void
yyerror (const char *str)

/* Raise the parse error <str>: usually generate the error message and log it.
 * If this is the first error in this file, account the wizard with an error.
 * If too many errors occurred already, do nothing.
 */

#line 1026 "prolang.y"
{
    char *context;

    if (num_parse_error > 5)
        return;
    context = lex_error_context();
    fprintf(stderr, "%s %s line %d: %s%s.\n"
                  , time_stamp(), current_loc.file->name, current_loc.line
                  , str, context);
    /* TODO: lex should implement a function get_include_stack() which
     * TODO:: returns an svalue-array with the current include stack.
     * TODO:: This could be printed, and also passed to parse_error().
     */
    fflush(stderr);
    parse_error(MY_FALSE, current_loc.file->name, current_loc.line
               , str, context);
    if (num_parse_error == 0)
        save_error(str, current_loc.file->name, current_loc.line);
    num_parse_error++;
} /* yyerror() */

/*-------------------------------------------------------------------------*/
void
yyerrorf (const char *format, ...)

/* Generate an yyerror() using printf()-style arguments.
 */

#line 1054 "prolang.y"
{
    va_list va;
    char buff[5120];
    char fixed_fmt[1000];

    format = limit_error_format(fixed_fmt, sizeof(fixed_fmt), format);
    va_start(va, format);
    vsprintf(buff, format, va);
    va_end(va);
    yyerror(buff);
} /* yyerrorf() */

/*-------------------------------------------------------------------------*/
void
yywarn (const char *str)

/* Raise the parse warning <str>: usually generate the warning message and
 * log it.
 */

#line 1074 "prolang.y"
{
    char *context;

    context = lex_error_context();
    fprintf(stderr, "%s %s line %d: Warning: %s%s.\n"
                  , time_stamp(), current_loc.file->name, current_loc.line
                  , str, context);
    /* TODO: lex should implement a function get_include_stack() which
     * TODO:: returns an svalue-array with the current include stack.
     * TODO:: This could be printed, and also passed to parse_error().
     */
    fflush(stderr);
    parse_error(MY_TRUE, current_loc.file->name, current_loc.line
               , str, context);
    if (master_ob && num_parse_error == 0)
        save_error(str, current_loc.file->name, current_loc.line);
    /* TODO: Introduce a 'master_is_loading' flag to prevent this call while
     * TODO:: the master is inactive.
     */
} /* yywarn() */

/*-------------------------------------------------------------------------*/
void
yywarnf (const char *format, ...)

/* Generate an yywarn() using printf()-style arguments.
 */

#line 1102 "prolang.y"
{
    va_list va;
    char buff[5120];
    char fixed_fmt[1000];

    format = limit_error_format(fixed_fmt, sizeof(fixed_fmt), format);
    va_start(va, format);
    vsprintf(buff, format, va);
    va_end(va);
    yywarn(buff);
} /* yywarnf() */

/*-------------------------------------------------------------------------*/
static void *
yalloc (size_t size)

/* Allocate a block of <size>, add it at the head of the last_yalloced
 * list, and return the pointer.
 *
 * Together with yfree(), this allocator is able to free intermediate
 * results in the epilog() which were thrown away due to an error.
 * TODO: A stack'ish mempool could do this?
 */

#line 1126 "prolang.y"
{
    char **p;

    p = xalloc(size+sizeof(char*));
    if (!p)
#line 1131 "prolang.y"
    {
        fatal("Out of memory in compiler.\n");
        return NULL;
    }
    *p++ = last_yalloced;
    last_yalloced = (char *)p;
    return p;
} /* yalloc() */

/*-------------------------------------------------------------------------*/
static void
yfree (void *block)

/* Free the block last allocated by yalloc().
 */

#line 1147 "prolang.y"
{
    char **p;

    p = (char **)block;
    if (p != (char **)last_yalloced)
#line 1152 "prolang.y"
    {
        debug_message("%s Block mismatch", time_stamp());
        return;
    }
    last_yalloced = *--p;
    xfree(p);
} /* yfree() */

/*-------------------------------------------------------------------------*/
static char *
ystring_copy (char *str)

/* Duplicate the string <str> using yalloc() and return the new one.
 */

#line 1167 "prolang.y"
{
    char *p;

    p = yalloc(strlen(str)+1);
    strcpy(p, str);
    return p;
} /* ystring_copy() */

/*-------------------------------------------------------------------------*/
static void
add_string_constant (void)

/* Add the string <last_lex_string> to the string in <last_string_constant>.
 * This is used to optimize "foo" + "bar" constructs.
 */

#line 1183 "prolang.y"
{
    string_t *tmp;

    tmp = mstr_add(last_string_constant, last_lex_string);
    if (!tmp)
#line 1188 "prolang.y"
    {
        yyerrorf("Out of memory for string literal (%zu bytes)"
                , (mstrsize(last_string_constant)
                         +mstrsize(last_lex_string))
                );
        return;
    }
    free_mstring(last_string_constant);
    free_mstring(last_lex_string); last_lex_string = NULL;
    last_string_constant = make_tabled(tmp);
    if (!last_string_constant)
#line 1199 "prolang.y"
    {
        yyerrorf("Out of memory for string literal (%zu bytes)",
                mstrsize(tmp));
    }
} /* add_string_constant() */

/*-------------------------------------------------------------------------*/
static char *
realloc_mem_block (mem_block_t *mbp, mp_int size)

/* Resize memblock <mbp> to hold at least <size> bytes, but at least
 * double its current size.
 *
 * Return NULL when out of memory, or a pointer to the newly allocated
 * memory area (ie. mbp->block).
 */

#line 1216 "prolang.y"
{
    mp_uint max_size;
    char *p;

    max_size = mbp->max_size;
    do {
        max_size *= 2;
    } while ((mp_uint)size > max_size);

    p = rexalloc(mbp->block, max_size);
    if (!p)
#line 1227 "prolang.y"
    {
        lex_close("Out of memory");
        return NULL;
    }
    mbp->block = p;
    mbp->max_size = max_size;
    return p;
} /* realloc_mem_block() */

/*-------------------------------------------------------------------------*/
static INLINE bool
reserve_mem_block (int n, size_t size)

/* Reserve <size> bytes at the current position in memory area <n>.
 * This does not increase the .current_size. Returns true, when successful,
 * false otherwise (usually an out-of-memory condition).
 * (If false, then an error message was already emitted.)
 */

#line 1246 "prolang.y"
{
    mem_block_t *mbp = &mem_block[n];

    if (size && mbp->current_size + size > mbp->max_size)
        return realloc_mem_block(mbp, mbp->current_size + size);

    return true;
} /* reserve_mem_block() */

/*-------------------------------------------------------------------------*/
static INLINE bool
extend_mem_block (int n, size_t size)

/* Reserve <size> bytes at the current position in memory area <n>.
 * This does increase the .current_size. Returns true, when successful,
 * false otherwise (usually an out-of-memory condition).
 * (If false, then an error message was already emitted.)
 */

#line 1265 "prolang.y"
{
    if (reserve_mem_block(n, size))
#line 1267 "prolang.y"
    {
        mem_block[n].current_size += size;
        return true;
    }

    return false;
} /* extend_mem_block() */

/*-------------------------------------------------------------------------*/
static INLINE bool
add_to_mem_block (int n, void *data, size_t size)

/* Add the <data> block of <size> bytes to the memory area <n>.
 * Returns true, when successful, false otherwise
 * (usually an out-of-memory condition).
 * (If false, then an error message was already emitted.)
 */

#line 1285 "prolang.y"
{
    mem_block_t *mbp = &mem_block[n];

    if (!size)
        return true;

    if (!reserve_mem_block(n, size))
        return false;

    memcpy(mbp->block + mbp->current_size, data, size);
    mbp->current_size += size;

    return true;
} /* add_to_mem_block() */

/*-------------------------------------------------------------------------*/

/* Define functions like add_to_mem_block() but with the correct
 * type for the corresponding block.
 */

#define DEFINE_ADD_TO_BLOCK_BY_PTR(FUN, BLOCK_NAME)               \
    static INLINE void                                            \
    FUN (BLOCK_NAME##_t *element)                                 \
    {                                                             \
        add_to_mem_block(BLOCK_NAME, element, sizeof(*element));  \
    }
#define DEFINE_ADD_TO_BLOCK_BY_VALUE(FUN, BLOCK_NAME)             \
    static INLINE void                                            \
    FUN (BLOCK_NAME##_t element)                                  \
    {                                                             \
        add_to_mem_block(BLOCK_NAME, &element, sizeof(element));  \
    }
#define DEFINE_RESERVE_MEM_BLOCK(FUN, BLOCK_NAME)                 \
    static INLINE bool                                            \
    FUN (size_t count)                                            \
    {                                                             \
        return reserve_mem_block(BLOCK_NAME,                      \
            sizeof(BLOCK_NAME##_t) * count);                      \
    }

DEFINE_ADD_TO_BLOCK_BY_VALUE(ADD_ARGUMENT_TYPE, A_ARGUMENT_TYPES)
DEFINE_ADD_TO_BLOCK_BY_VALUE(ADD_ARGUMENT_INDEX, A_ARGUMENT_INDEX)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_FUNCTION, A_FUNCTIONS)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_VIRTUAL_VAR, A_VIRTUAL_VAR)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_VARIABLE, A_VARIABLES)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_STRUCT_DEF, A_STRUCT_DEFS)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_STRUCT_MEMBER, A_STRUCT_MEMBERS)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_INLINE_CLOSURE, A_INLINE_CLOSURE)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_INCLUDE, A_INCLUDES)
DEFINE_ADD_TO_BLOCK_BY_PTR(ADD_INHERIT, A_INHERITS)

DEFINE_RESERVE_MEM_BLOCK(RESERVE_FUNCTIONS, A_FUNCTIONS);
DEFINE_RESERVE_MEM_BLOCK(RESERVE_UPDATE_INDEX_MAP, A_UPDATE_INDEX_MAP);
DEFINE_RESERVE_MEM_BLOCK(RESERVE_INHERITS, A_INHERITS);

#define byte_to_mem_block(n, b) \
    ((void)((mem_block[n].current_size == mem_block[n].max_size \
             ? !!realloc_mem_block(&mem_block[n],0) : 1) \
            ? (mem_block[n].block[mem_block[n].current_size++] = (char)(b)) \
            : 0)\
    )
  /* Add the byte <b> to the memory area <n>, which is resized
   * if necessary.
   */

/* ==============================   TYPES   ============================== */

/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/

static size_t
get_f_visibility_buf (typeflags_t flags, char *buf, size_t bufsize)

/* Write a textual representation of the visibility flags in <flags>
 * into <buf> with maximum size <bufsize>.
 */

#line 1363 "prolang.y"
{
    size_t len;

    if (bufsize <= 0)
        return 0;

    buf[0] = '\0';
    if (flags & TYPE_MOD_STATIC)
        strncat(buf, "static ", bufsize);
    if (flags & TYPE_MOD_NO_MASK)
        strncat(buf, "nomask ", bufsize);
    if (flags & TYPE_MOD_PRIVATE)
        strncat(buf, "private ", bufsize);
    if (flags & TYPE_MOD_PROTECTED)
        strncat(buf, "protected ", bufsize);
    if (flags & TYPE_MOD_PUBLIC)
        strncat(buf, "public ", bufsize);
    if (flags & TYPE_MOD_VISIBLE)
        strncat(buf, "visible ", bufsize);
    if (flags & TYPE_MOD_VARARGS)
        strncat(buf, "varargs ", bufsize);
    if (flags & TYPE_MOD_DEPRECATED)
        strncat(buf, "deprecated ", bufsize);

    len = strlen(buf);
    if (len && buf[len-1] == ' ')
        buf[--len] = '\0';

    return len;
} /* get_f_visibility_buf() */

/*-------------------------------------------------------------------------*/
static char *
get_f_visibility (typeflags_t flags)

/* Return (in a static buffer) a textual representation of the visibility
 * flags in <flags>.
 */

#line 1402 "prolang.y"
{
    static char buff[120];
    get_f_visibility_buf(flags, buff, sizeof(buff));
    return buff;
} /* get_f_visibility() */

/*-------------------------------------------------------------------------*/
static char *
get_visibility (fulltype_t type)

/* Return (in a static buffer) a textual representation of the visibility
 * portion of <type>.
 */

#line 1416 "prolang.y"
{
    return get_f_visibility(type.t_flags);
} /* get_visibility() */

/*-------------------------------------------------------------------------*/
size_t
get_lpctype_name_buf (lpctype_t *type, char *buf, size_t bufsize)

/* Write a textual representation of <type> into <buf>.
 * At most <bufsize> bytes (including the trailing '\0') are written.
 * Returns the number of bytes written (excluding the trailing '\0').
 */
#line 1428 "prolang.y"
{
    static char *type_name[] = { "unknown", "int", "string", "void",
                                 "object", "mapping", "float", "mixed",
                                 "closure", "symbol", "quoted_array", "struct" };

    if (bufsize <= 0)
        return 0;

    if (type == NULL)
        type = lpctype_mixed;

    switch(type->t_class)
#line 1440 "prolang.y"
    {
    case TCLASS_PRIMARY:
#line 1442 "prolang.y"
        {
            char* name;
            size_t len;

            if (type->t_primary >= sizeof type_name / sizeof type_name[0])
                fatal("Bad type %"PRIu32": %s line %d\n"
                     , type->t_primary,  current_loc.file->name
                     , current_loc.line);

            name = type_name[type->t_primary];
            len = strlen(name);

            if(len < bufsize)
#line 1455 "prolang.y"
            {
                memcpy(buf, name, len+1);
                return len;
            }
            else
#line 1460 "prolang.y"
            {
                buf[0] = '\0';
                return 0;
            }
        }

    case TCLASS_STRUCT:
#line 1467 "prolang.y"
        {
            if (type->t_struct.name)
#line 1469 "prolang.y"
            {
                size_t len = 7 + mstrsize(type->t_struct.name->name);
                if(len < bufsize)
#line 1472 "prolang.y"
                {
                    memcpy(buf, "struct ", 7);
                    memcpy(buf+7, get_txt(type->t_struct.name->name), len-7);
                    buf[len] = 0;
                    return len;
                }
                else
#line 1479 "prolang.y"
                {
                    buf[0] = '\0';
                    return 0;
                }
            }
            else // no struct
#line 1485 "prolang.y"
            {
                snprintf(buf, bufsize, "unknown struct");
                return strlen(buf);
            }

        }

    case TCLASS_ARRAY:
#line 1493 "prolang.y"
        {
            size_t sublen;
            lpctype_t *basetype;

            basetype = type->t_array.base;
            if(basetype->t_class == TCLASS_UNION)
#line 1499 "prolang.y"
            {
                buf[0] = '<';
                sublen = get_lpctype_name_buf(basetype, buf + 1, bufsize - type->t_array.depth - 2);
                if(sublen)
#line 1503 "prolang.y"
                {
                    buf[sublen+1] = '>';
                    sublen += 2;
                }
            }
            else
                sublen = get_lpctype_name_buf(basetype, buf, bufsize - type->t_array.depth);

            if(sublen)
#line 1512 "prolang.y"
            {
                memset(buf + sublen, '*', type->t_array.depth);
                sublen += type->t_array.depth;
            }

            buf[sublen] = '\0';
            return sublen;
        }

    case TCLASS_UNION:
#line 1522 "prolang.y"
        {
            lpctype_t *curtype = type;
            char* curbuf = buf;
            size_t sublen;

            if (bufsize < 5)
#line 1528 "prolang.y"
            {
                buf[0] = 0;
                return 0;
            }

            while (curtype->t_class == TCLASS_UNION)
#line 1534 "prolang.y"
            {
                sublen = get_lpctype_name_buf(curtype->t_union.member, curbuf, bufsize - 4);
                if(!sublen)
                    break;

                curbuf[sublen] = '|';
                curbuf += sublen + 1;
                curtype = curtype->t_union.head;
            }

            if(sublen)
#line 1545 "prolang.y"
            {
                sublen = get_lpctype_name_buf(curtype, curbuf, bufsize);
                curbuf += sublen;
            }

            if(!sublen)
#line 1551 "prolang.y"
            {
                if(curbuf == buf)
                    return 0;
                memcpy(curbuf, "...", 4);
                return curbuf - buf + 3;
            }

            return curbuf - buf;
        }
    }

    /* Not reached. */
    buf[0] = '\0';
    return 0;
} /* get_lpctype_name_buf() */

/*-------------------------------------------------------------------------*/
char *
get_lpctype_name (lpctype_t *type)

/* Return (in a static buffer) a textual representation of <type>.
 */

#line 1574 "prolang.y"
{
    static char buff[512];

    get_lpctype_name_buf(type, buff, sizeof(buff));
    return buff;
} /* get_lpctype_name() */

/*-------------------------------------------------------------------------*/
size_t
get_fulltype_name_buf (fulltype_t type, char *buf, size_t bufsize)

/* Write a textual representation of <type> into <buf>.
 * At most <bufsize> bytes (including the trailing '\0') are written.
 * Returns the number of bytes written (excluding the trailing '\0').
 */
#line 1589 "prolang.y"
{
    size_t len;

    if (bufsize <= 0)
        return 0;

    if (type.t_flags & TYPE_MOD_REFERENCE)
#line 1596 "prolang.y"
    {
        if(bufsize < 4)
#line 1598 "prolang.y"
        {
            buf[0] = '\0';
            return 0;
        }

        bufsize -= 2;
    }

    len = get_f_visibility_buf(type.t_flags, buf, bufsize);
    if(len && len + 1 < bufsize)
        buf[len++] = ' ';
    len += get_lpctype_name_buf(type.t_type, buf + len, bufsize - len);

    if (type.t_flags & TYPE_MOD_REFERENCE)
#line 1612 "prolang.y"
    {
        memcpy(buf + len, " &", 3);
        len += 2;
    }

    return len;
} /* get_fulltype_name_buf() */

/*-------------------------------------------------------------------------*/
char *
get_fulltype_name (fulltype_t type)

/* Return (in a static buffer) a textual representation of <type>.
 */

#line 1627 "prolang.y"
{
    static char buff[1024];

    get_fulltype_name_buf(type, buff, sizeof(buff));
    return buff;
} /* get_fulltype_name() */

/*-------------------------------------------------------------------------*/
static char *
get_two_fulltypes (fulltype_t type1, fulltype_t type2)

/* Return (in a static buffer) the text "(<type1> vs. <type2>)".
 */
#line 1640 "prolang.y"
{
    static char buff[1024];
    size_t len, len2;

    buff[0] = '(';
    len = 1 + get_fulltype_name_buf(type1, buff+1, sizeof(buff)-10);

    memcpy(buff + len, " vs ", 4);
    len += 4;

    len2 = get_fulltype_name_buf(type2, buff+len, sizeof(buff)-len-1);
    if(!len2)
        memcpy(buff + len, "...)", 5);
    else
        memcpy(buff + len + len2, ")", 2);

    return buff;
} /* get_two_fulltypes() */

/*-------------------------------------------------------------------------*/
static char *
get_two_lpctypes (lpctype_t *type1, lpctype_t *type2)

/* Return (in a static buffer) the text "(<type1> vs. <type2>)".
 */
#line 1665 "prolang.y"
{
    static char buff[1024];
    size_t len, len2;

    buff[0] = '(';
    len = 1 + get_lpctype_name_buf(type1, buff+1, sizeof(buff)-10);

    memcpy(buff + len, " vs ", 4);
    len += 4;

    len2 = get_lpctype_name_buf(type2, buff+len, sizeof(buff)-len-1);
    if(!len2)
        memcpy(buff + len, "...)", 5);
    else
        memcpy(buff + len + len2, ")", 2);

    return buff;
} /* get_two_lpctypes() */

/*-------------------------------------------------------------------------*/
static void
fulltype_error (char *str, fulltype_t type)

/* Generate an yyerror with the message "<str>: <type>".
 */
#line 1690 "prolang.y"
{
    char *p;

    p = get_fulltype_name(type);
    yyerrorf("%s: \"%s\"", str, p);
} /* fulltype_error() */

/*-------------------------------------------------------------------------*/
static void
lpctype_error (char *str, lpctype_t *type)

/* Generate an yyerror with the message "<str>: <type>".
 */
#line 1703 "prolang.y"
{
    char *p;

    p = get_lpctype_name(type);
    yyerrorf("%s: \"%s\"", str, p);
} /* lpctype_error() */

/*-------------------------------------------------------------------------*/
static void
argument_type_error (int instr, lpctype_t *type)

/* Generate an yyerror with the message "Bad argument to <instr>: <type>".
 */

#line 1717 "prolang.y"
{
    char *p;

    p = get_lpctype_name(type);
    yyerrorf("Bad argument to %s: \"%s\"", instrs[instr].name, p);
} /* argument_type_error() */

/*-------------------------------------------------------------------------*/
static void
efun_argument_error(int arg, int instr
                   , fulltype_t * expected, fulltype_t got
                   )
#line 1729 "prolang.y"
{
    char msg[1024];

    msg[0] = '\0';
    for (; expected->t_type; expected++)
#line 1734 "prolang.y"
    {
        if (msg[0] != '\0')
            strcat(msg, "|");
        strcat(msg, get_fulltype_name(*expected));
    }
    yyerrorf("Bad arg %d type to %s(): got %s, expected %s"
            , arg, instrs[instr].name, get_fulltype_name(got), msg);
} /* efun_argument_error() */

/*-------------------------------------------------------------------------*/
typedef struct unary_op_types_s unary_op_types_t;
typedef struct binary_op_types_s binary_op_types_t;

struct unary_op_types_s
#line 1748 "prolang.y"
{
    lpctype_t* t;       /* argument to the operator. */
    lpctype_t* result;  /* result type for this operation. */

    lpctype_t* (*resultfun)(lpctype_t* t);
      /* if <resultfun> is not NULL it is called with the argument
       * type, and only if it returns a non-NULL value, this entry
       * matches. If <result> is NULL then the function's result
       * will be regarded as the operator result type.
       * (otherwise <result> will be taken).
       */
};

struct binary_op_types_s
#line 1762 "prolang.y"
{
    lpctype_t* t1;      /* first argument to the operator. */
    lpctype_t* t2;      /* second argument to the operator. */
    lpctype_t* result;  /* result type for this type combination. */

    lpctype_t* (*resultfun)(lpctype_t* t1, lpctype_t* t2);
      /* If <resultfun> is not NULL it is called with the argument
       * types, and only if it returns a non-NULL value, this
       * entry matches. If <result> is NULL then the function's
       * result will be regarded as the operator result type
       * (otherwise <result> will be taken).
       */

    lpctype_t* (*expfun)(lpctype_t* t1, lpctype_t* t2);
      /* If <expfun> is not NULL it is called with the argument
       * types when this entry didn't match. It can then
       * return a suggested type, that will be printed in the
       * error message.
       */
};

static lpctype_t*
get_array_member_type (lpctype_t* array)

/* <array> is an array or a union of arrays.
 * Determine the type of its members.
 */

#line 1790 "prolang.y"
{
    lpctype_t *result = NULL;
    lpctype_t *head = array;

    while (true)
#line 1795 "prolang.y"
    {
        lpctype_t *member = head->t_class == TCLASS_UNION ? head->t_union.member : head;

        if(member->t_class == TCLASS_ARRAY)
#line 1799 "prolang.y"
        {
            lpctype_t *oldresult = result;
            result = get_union_type(result, member->t_array.element);
            free_lpctype(oldresult);
        }
        else /* must be mixed or unknown */
#line 1805 "prolang.y"
        {
            free_lpctype(result);
            return ref_lpctype(member);
        }

        if (head->t_class == TCLASS_UNION)
            head = head->t_union.head;
        else
            break;
    }

    return result;
}

static lpctype_t*
get_union_array_type (lpctype_t* t1, lpctype_t* t2)

/* <t1> and <t2> are arrays. This function creates an array type
 * of the union of the members of both types.
 */

#line 1826 "prolang.y"
{
    lpctype_t *member, *result;
    lpctype_t *element1 = get_array_member_type(t1),
              *element2 = get_array_member_type(t2);

    member = get_union_type(element1, element2);
    result = get_array_type(member);

    free_lpctype(member);
    free_lpctype(element1);
    free_lpctype(element2);

    return result;
}

static lpctype_t*
get_sub_array_type (lpctype_t* t1, lpctype_t* t2)

/* <t1> and <t2> are arrays. This function returns <t1> if
 * there are any common member types, otherwise this functions fails.
 */

#line 1848 "prolang.y"
{
    lpctype_t *common;
    lpctype_t *element1 = get_array_member_type(t1),
              *element2 = get_array_member_type(t2);

    common = get_common_type(element1, element2);

    free_lpctype(element1);
    free_lpctype(element2);

    if(common == NULL)
        return NULL;

    free_lpctype(common);
    return ref_lpctype(t1);
}


static lpctype_t*
get_first_type (lpctype_t* t1, lpctype_t* t2)

/* Return <t1> as the result type in the type table.
 */

#line 1872 "prolang.y"
{
    return ref_lpctype(t1);
}


static lpctype_t*
get_second_type (lpctype_t* t1, lpctype_t* t2)

/* Return <t2> as the result type in the type table.
 */

#line 1883 "prolang.y"
{
    return ref_lpctype(t2);
}

static lpctype_t*
get_argument_type (lpctype_t* t)

/* Return <t> as the result type in the type table.
 * This is the intersection between the type in
 * the table and the type of the argument.
 */

#line 1895 "prolang.y"
{
    return ref_lpctype(t);
}


/* Operator type table for assignment with addition.
 */
binary_op_types_t types_add_assignment[] = {
    { &_lpctype_string,    &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                  },
    { &_lpctype_string,    &_lpctype_int,       &_lpctype_string,  NULL                  , NULL                  },
    { &_lpctype_string,    &_lpctype_float,     &_lpctype_string,  NULL                  , NULL                  },
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                  },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                  },
    { &_lpctype_any_array, &_lpctype_any_array, NULL,              &get_union_array_type , NULL                  },
    { &_lpctype_mapping,   &_lpctype_mapping,   &_lpctype_mapping, NULL                  , NULL                  },
    { NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for assignment with subtraction.
 */
binary_op_types_t types_sub_assignment[] = {
    { &_lpctype_string,    &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                  },
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                  },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                  },
    { &_lpctype_mapping,   &_lpctype_mapping,   &_lpctype_mapping, NULL                  , NULL                  },
    { &_lpctype_any_array, &_lpctype_any_array, NULL,              &get_sub_array_type   , &get_first_type       },
    { NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for assignment with multiplication.
 */
binary_op_types_t types_mul_assignment[] = {
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                  },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                  },
    { &_lpctype_string,    &_lpctype_int,       &_lpctype_string,  NULL                  , NULL                  },
    { &_lpctype_any_array, &_lpctype_int,       NULL,              &get_first_type       , NULL                  },
    { NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for assignment with division.
 */
binary_op_types_t types_div_assignment[] = {
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                  },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                  },
    { NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for assignment with the binary and.
 */
binary_op_types_t types_binary_and_assignment[] = {
    { &_lpctype_mapping,   &_lpctype_mapping,   &_lpctype_mapping, NULL                  , NULL                  },
    { &_lpctype_mapping,   &_lpctype_any_array, &_lpctype_mapping, NULL                  , NULL                  },
    { &_lpctype_any_array, &_lpctype_mapping,   NULL,              &get_first_type       , NULL                  },
    { &_lpctype_any_array, &_lpctype_any_array, NULL,              &get_common_type      , NULL                  },
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                  },
    { &_lpctype_string,    &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                  },
    { NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for the binary or and xor,
 * allowing <int>|<int> and <mixed*>|<mixed*>.
 */
binary_op_types_t types_binary_or[] = {
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                  },
    { &_lpctype_any_array, &_lpctype_any_array, NULL,              &get_union_array_type , NULL                  },
    { NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for the binary and.
 */
binary_op_types_t types_binary_and[] = {
    { &_lpctype_mapping,   &_lpctype_mapping,   &_lpctype_mapping, NULL                  , NULL                  },
    { &_lpctype_mapping,   &_lpctype_any_array, &_lpctype_mapping, NULL                  , NULL                  },
    { &_lpctype_any_array, &_lpctype_mapping,   NULL,              &get_first_type       , NULL                  },
    { &_lpctype_any_array, &_lpctype_any_array, NULL,              &get_common_type      , NULL                  },
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                  },
    { &_lpctype_string,    &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                  },
    { NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for equality tests.
 * Basically there must be a common type, but ints can be compared with floats also.
 */
binary_op_types_t types_equality[] = {
    { &_lpctype_mixed,     &_lpctype_mixed,     &_lpctype_int,     &get_common_type      , NULL                  },
    { &_lpctype_int,       &_lpctype_float,     &_lpctype_int,     NULL                  , NULL                  },
    { &_lpctype_float,     &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                  },
    { NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for shift operations.
 * Only ints are allowed.
 */
binary_op_types_t types_shift[] = {
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                  },
    { NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for addition.
 */
binary_op_types_t types_addition[] = {
    { &_lpctype_string,    &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                  },
    { &_lpctype_string,    &_lpctype_int,       &_lpctype_string,  NULL                  , NULL                  },
    { &_lpctype_string,    &_lpctype_float,     &_lpctype_string,  NULL                  , NULL                  },
    { &_lpctype_int,       &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                  },
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                  },
    { &_lpctype_int,       &_lpctype_float,     &_lpctype_float,   NULL                  , NULL                  },
    { &_lpctype_float,     &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                  },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                  },
    { &_lpctype_any_array, &_lpctype_any_array, NULL,              &get_union_array_type , NULL                  },
    { &_lpctype_mapping,   &_lpctype_mapping,   &_lpctype_mapping, NULL                  , NULL                  },
    { NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for subtraction.
 */
binary_op_types_t types_subtraction[] = {
    { &_lpctype_string,    &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                  },
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                  },
    { &_lpctype_int,       &_lpctype_float,     &_lpctype_float,   NULL                  , NULL                  },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                  },
    { &_lpctype_mapping,   &_lpctype_mapping,   &_lpctype_mapping, NULL                  , NULL                  },
    { &_lpctype_any_array, &_lpctype_any_array, NULL,              &get_sub_array_type   , &get_first_type       },
    { NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for multiplication.
 */
binary_op_types_t types_multiplication[] = {
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                  },
    { &_lpctype_int,       &_lpctype_float,     &_lpctype_float,   NULL                  , NULL                  },
    { &_lpctype_int,       &_lpctype_string,    &_lpctype_string,  NULL                  , NULL                  },
    { &_lpctype_int,       &_lpctype_any_array, NULL,              &get_second_type      , NULL                  },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                  },
    { &_lpctype_string,    &_lpctype_int,       &_lpctype_string,  NULL                  , NULL                  },
    { &_lpctype_any_array, &_lpctype_int,       NULL,              &get_first_type       , NULL                  },
    { NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for the modulus operation.
 */
binary_op_types_t types_modulus[] = {
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                  },
    { NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for division.
 */
binary_op_types_t types_division[] = {
    { &_lpctype_int,       &_lpctype_int,       &_lpctype_int,     NULL                  , NULL                  },
    { &_lpctype_int,       &_lpctype_float,     &_lpctype_float,   NULL                  , NULL                  },
    { &_lpctype_float,     &_lpctype_int_float, &_lpctype_float,   NULL                  , NULL                  },
    { NULL, NULL, NULL, NULL, NULL }
};

/* Operator type table for the unary minus, increment and decrement operation.
 */
unary_op_types_t types_unary_math[] = {
    { &_lpctype_int,                            &_lpctype_int,     NULL                  },
    { &_lpctype_float,                          &_lpctype_float,   NULL                  },
    { NULL, NULL, NULL }
};

/* Operator type table for the range index operation (arg[x..y])
 */
unary_op_types_t types_range_index[] = {
    { &_lpctype_string,                         &_lpctype_string,  NULL                  },
    { &_lpctype_any_array,                      NULL,              &get_argument_type    },
    { NULL, NULL, NULL }
};

/*-------------------------------------------------------------------------*/
static lpctype_t*
check_unary_op_type (lpctype_t* t, const char* op_name, unary_op_types_t* type_table, lpctype_t *error_type)

/* Checks the argument <t> against the possible combinations in type_table.
 * The result is the union of all result types of matching entries.
 * If there are no matching entries, a compile error is thrown
 * (unless <op_name> is NULL) and <error_type> is returned.
 */

#line 2076 "prolang.y"
{
    lpctype_t* result = NULL;

    /* No type info? Use the fallback. */
    if (t == NULL)
        return ref_lpctype(error_type);

    for (unary_op_types_t* entry = type_table; entry->t; ++entry)
#line 2084 "prolang.y"
    {
        /* Is there any commonality between our
         * argument type and the entry in the table?
         */
        lpctype_t *m = get_common_type(entry->t, t);
        lpctype_t *mresult, *oldresult;
        if (m == NULL)
            continue;

        mresult = ref_lpctype(entry->result);

        /* Then check resultfun, if it exists.
         * It is an additional condition (must return != NULL)
         * and returns the result type unless given in entry->result.
         */
        if (entry->resultfun != NULL)
#line 2100 "prolang.y"
        {
            lpctype_t *funresult = (*entry->resultfun)(m);
            if (funresult == NULL)
#line 2103 "prolang.y"
            {
                free_lpctype(m);
                free_lpctype(mresult);
                continue;
            }

            if (mresult == NULL)
                mresult = funresult;
            else
                free_lpctype(funresult);
        }

        oldresult = result;
        result = get_union_type(result, mresult);
        free_lpctype(m);
        free_lpctype(mresult);
        free_lpctype(oldresult);
    }

    if (result != NULL || op_name == NULL)
        return result;

    /* Now get the error message:
     * "Bad argument to <op_name>: <t>, expected ..."
     * and show the unions of all first types in the table.
     */
    if (exact_types)
#line 2130 "prolang.y"
    {
        lpctype_t* expected = NULL;
        char expbuff[1024];

        for (unary_op_types_t* entry = type_table; entry->t; ++entry)
#line 2135 "prolang.y"
        {
            lpctype_t *oldexpected = expected;
            expected = get_union_type(expected, entry->t);
            free_lpctype(oldexpected);
        }

        get_lpctype_name_buf(expected, expbuff, sizeof(expbuff));
        free_lpctype(expected);

        yyerrorf("Bad argument to %s: got %s, expected %s"
            , op_name, get_lpctype_name(t), expbuff);
    }

    return ref_lpctype(error_type);

} /* check_unary_op_type() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
check_binary_op_types (lpctype_t* t1, lpctype_t* t2, const char* op_name, binary_op_types_t* type_table, lpctype_t *error_type)

/* Checks the arguments <t1> and <t2> against the possible combinations
 * in type_table. The result is the union of all result types of
 * matching entries. If there are no matching entries, a compile error
 * is thrown (unless <op_name> is NULL) and <error_type> is returned.
 */

#line 2162 "prolang.y"
{
    Bool t1_matched = MY_FALSE;
    lpctype_t* result = NULL;

    /* No type info? Use the fallback as a shortcut. */
    if (t1 == NULL && t2 == NULL)
        return error_type ? ref_lpctype(error_type) : lpctype_mixed;

    if (t1 == NULL)
        t1 = lpctype_mixed;

    if (t2 == NULL)
        t2 = lpctype_mixed;

    for (binary_op_types_t* entry = type_table; entry->t1; ++entry)
#line 2177 "prolang.y"
    {
        /* Is there any commonality between our first
         * argument type and the entry in the table?
         */
        lpctype_t *m1 = get_common_type(entry->t1, t1);
        lpctype_t *m2, *mresult, *oldresult;
        if (m1 == NULL)
            continue;

        t1_matched = MY_TRUE;
        /* And if so, is there some intersection
         * between <t2> and the table entry?
         */
        m2 = get_common_type(entry->t2, t2);
        if (m2 == NULL)
#line 2192 "prolang.y"
        {
            free_lpctype(m1);
            continue;
        }

        mresult = ref_lpctype(entry->result);

        /* Then check resultfun, if it exists.
         * It is an additional condition (must return != NULL)
         * and returns the result type unless given in entry->result.
         */
        if (entry->resultfun != NULL)
#line 2204 "prolang.y"
        {
            lpctype_t *funresult = (*entry->resultfun)(m1,m2);
            if (funresult == NULL)
#line 2207 "prolang.y"
            {
                free_lpctype(m1);
                free_lpctype(m2);
                free_lpctype(mresult);
                continue;
            }

            if (mresult == NULL)
                mresult = funresult;
            else
                free_lpctype(funresult);
        }

        oldresult = result;
        result = get_union_type(result, mresult);
        free_lpctype(m1);
        free_lpctype(m2);
        free_lpctype(mresult);
        free_lpctype(oldresult);
    }

    if (result != NULL || op_name == NULL)
        return result;

    /* For the error message, remember whether at least <t1> matched somewhere.
     * If so, "Bad argument 2 to <op_name>: <t2>, expected ..." and show the
     * unions of all second types for each match for t1. Otherwise
     * "Bad argument 1 to <op_name>: <t1>, expected ..." and show the unions
     * of all first types in the table.
     */
    if (!exact_types)
#line 2238 "prolang.y"
    {
        /* No strong type check, don't print error messages. */
    }
    else if (t1_matched)
#line 2242 "prolang.y"
    {
        lpctype_t* expected = NULL;
        char expbuff[1024];

        for (binary_op_types_t* entry = type_table; entry->t1; ++entry)
#line 2247 "prolang.y"
        {
            lpctype_t *m1 = get_common_type(entry->t1, t1);
            lpctype_t *oldexpected, *newexpected;
            if (m1 == NULL)
                continue;

            oldexpected = expected;
            if (entry->expfun)
#line 2255 "prolang.y"
            {
                lpctype_t *m2 = get_common_type(entry->t2, t2);
                if (m2 == NULL)
                    newexpected = ref_lpctype(entry->t2);
                else
#line 2260 "prolang.y"
                {
                    newexpected = (*entry->expfun)(m1, m2);
                    free_lpctype(m2);
                }
            }
            else
                newexpected = ref_lpctype(entry->t2);

            expected = get_union_type(expected, newexpected);

            free_lpctype(m1);
            free_lpctype(oldexpected);
            free_lpctype(newexpected);
        }

        get_lpctype_name_buf(expected, expbuff, sizeof(expbuff));
        free_lpctype(expected);

        yyerrorf("Bad argument 2 to %s: got %s, expected %s"
            , op_name, get_lpctype_name(t2), expbuff);
    }
    else
#line 2282 "prolang.y"
    {
        lpctype_t* expected = NULL;
        char expbuff[1024];

        for (binary_op_types_t* entry = type_table; entry->t1; ++entry)
#line 2287 "prolang.y"
        {
            lpctype_t *oldexpected = expected;
            expected = get_union_type(expected, entry->t1);
            free_lpctype(oldexpected);
        }

        get_lpctype_name_buf(expected, expbuff, sizeof(expbuff));
        free_lpctype(expected);

        yyerrorf("Bad argument 1 to %s: got %s, expected %s"
            , op_name, get_lpctype_name(t1), expbuff);
    }

    return ref_lpctype(error_type);

} /* check_binary_op_types() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
get_index_result_type (lpctype_t* aggregate, fulltype_t index, int inst, lpctype_t* error_type)

/* Determines the result type of a single index operation on <aggregate>.
 * Also checks whether the <index> and the index operation <inst> are
 * legal for <aggregate>.
 *
 * The result is the union of all possible result types. If there are no
 * possible results, a compile erorr is thrown (and <error_type> is
 * returned, reference added).
 */
#line 2316 "prolang.y"
{
    lpctype_t* result = NULL;
    lpctype_t* head = aggregate;
    bool can_be_mapping = false;
    bool can_be_array_or_string = false;

    if (index.t_flags & TYPE_MOD_REFERENCE)
        yyerror("Reference used as index");

    /* No type information, then it might be anything... */
    if (aggregate == NULL)
        return lpctype_mixed;

    /* Treat index as anything, if we don't know what it is. */
    if (index.t_type == NULL)
        index.t_type = lpctype_mixed;

    /* [<n] or [>n] can only index an array or string,
     * [n] (F_INDEX) can also index a mapping.
     */
    if (inst == F_INDEX)
        can_be_mapping = true;

    /* Array or string indexing need an integer as an index. */
    if (lpctype_contains(lpctype_int, index.t_type))
        can_be_array_or_string = true;
    else if(inst != F_INDEX)
#line 2343 "prolang.y"
    {
        /* [<n] or [>n] need an integer. */
        if (exact_types)
            lpctype_error("Bad type of index", index.t_type);
        return ref_lpctype(error_type);
    }

    /* Now walk through <aggregate>. */
    while (true)
#line 2352 "prolang.y"
    {
        lpctype_t *member = head->t_class == TCLASS_UNION ? head->t_union.member : head;

        switch (member->t_class)
#line 2356 "prolang.y"
        {
        case TCLASS_PRIMARY:
            switch (member->t_primary)
#line 2359 "prolang.y"
            {
            case TYPE_UNKNOWN:
            case TYPE_ANY:
                /* Can't do anything here... */
                return ref_lpctype(member);

            case TYPE_STRING:
                if (can_be_array_or_string)
#line 2367 "prolang.y"
                {
                    /* Indexing a string, add int to the result. */
                    lpctype_t *oldresult = result;
                    result = get_union_type(result, lpctype_int);
                    free_lpctype(oldresult);
                }
                break;

            case TYPE_MAPPING:
                if (can_be_mapping)
#line 2377 "prolang.y"
                {
                    /* Values can be anything... */
                    free_lpctype(result);
                    result = lpctype_mixed;
                }
                break;

            default:
                /* All other cases we ignore, they cannot be indexed. */
                break;
            }
            break;

        case TCLASS_ARRAY:
            if (can_be_array_or_string)
#line 2392 "prolang.y"
            {
                /* Add the array member type to the result. */
                lpctype_t *oldresult = result;
                result = get_union_type(result, member->t_array.element);
                free_lpctype(oldresult);
            }
            break;
        }

        if (head->t_class == TCLASS_UNION)
            head = head->t_union.head;
        else
            break;
    }

    if (!result)
#line 2408 "prolang.y"
    {
        if (exact_types)
            lpctype_error("Bad type to index", aggregate);
        return ref_lpctype(error_type);
    }

    return result;
} /* get_index_result_type() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
get_flattened_type (lpctype_t* t)

/* Determines the result type of argument flattening.
 *
 * The result is the union of all non-array types in <t> and the type
 * of all elements of arrays in <t>.
 */
#line 2426 "prolang.y"
{
    lpctype_t *result = NULL;
    lpctype_t* head = t;

    if (t == NULL)
        return NULL;

    /* Let's walk through all types in <t>. */
    while (true)
#line 2435 "prolang.y"
    {
        lpctype_t *oldresult = result;
        lpctype_t *member = head->t_class == TCLASS_UNION ? head->t_union.member : head;
        lpctype_t *add = NULL;

        if (member->t_class == TCLASS_ARRAY)
            add = member->t_array.element;
        else
            add = member;

        result = get_union_type(result, add);
        free_lpctype(oldresult);

        if (head->t_class == TCLASS_UNION)
            head = head->t_union.head;
        else
            break;
    }

    return result;

} /* check_binary_op_types() */

/*-------------------------------------------------------------------------*/
static funflag_t
check_visibility_flags (funflag_t flags, funflag_t default_vis, bool function)

/* Checks the given visibility flags.
 *
 * Checks whether only one visibility modifier was given.
 * If no modifier was given use <default_vis> instead.
 * TYPE_MOD_VISIBLE will be removed.
 * If <function> is true, then TYPE_MOD_STATIC is also considered.
 */
#line 2469 "prolang.y"
{
    funflag_t mask = TYPE_MOD_PRIVATE | TYPE_MOD_PROTECTED | TYPE_MOD_PUBLIC | TYPE_MOD_VISIBLE;
    int num_modifier = 0;

    if(function)
        mask |= TYPE_MOD_STATIC;

    for (funflag_t i = 1; i < mask; i <<= 1)
        if (flags & mask & i)
            num_modifier++;

    if (num_modifier == 0)
        flags |= default_vis;
    else if (num_modifier > 1)
#line 2483 "prolang.y"
    {
        yyerrorf("Multiple visibility modifier given: %s"
            , get_f_visibility(flags & mask));

        /* Remove them all to avoid confusion till compilation aborts. */
        flags &= ~mask;
    }

    return flags & ~TYPE_MOD_VISIBLE;
} /* check_visibility_flags() */

/*-------------------------------------------------------------------------*/
static INLINE void
i_add_arg_type (fulltype_t type)

/* Add another function argument type to the argument type stack.
 * The reference of <type> is adopted.
 */

#line 2502 "prolang.y"
{
    mem_block_t *mbp = &type_of_arguments;

    if (mbp->current_size + sizeof type > mbp->max_size)
#line 2506 "prolang.y"
    {
        mbp->max_size *= 2;
        mbp->block = rexalloc((char *)mbp->block, mbp->max_size);
    }

    *(fulltype_t*)(mbp->block + mbp->current_size) = type;
    mbp->current_size += sizeof(fulltype_t);
} /* i_add_arg_type() */

#define add_arg_type(t) i_add_arg_type(t)

/*-------------------------------------------------------------------------*/
static INLINE void
pop_arg_stack (int n)

/* Pop (remove) the last <n> types from the argument stack.
 */

#line 2524 "prolang.y"
{
    fulltype_t * vp;

    vp = (fulltype_t*)(type_of_arguments.block + type_of_arguments.current_size);
    while (n > 0)
#line 2529 "prolang.y"
    {
        type_of_arguments.current_size -= sizeof (fulltype_t);
        n--;
        vp--;

        free_fulltype(*vp);
    }
} /* pop_arg_stack() */

/*-------------------------------------------------------------------------*/
static INLINE fulltype_t *
get_argument_types_start (int n)

/* Get the type of the <n>th last argument from the stack.
 * <n> must be >= 1.
 */

#line 2546 "prolang.y"
{
    return
        &((fulltype_t *)
         (type_of_arguments.block + type_of_arguments.current_size))[ - n];
} /* get_arguments_type_start() */

/*-------------------------------------------------------------------------*/
static INLINE lpctype_t *
get_aggregate_type (int n)

/* The last <n> types on the argument stack are an aggregate type.
 * Combine the single types to a union type and remove them
 * from the argument stack.
 */

#line 2561 "prolang.y"
{
    fulltype_t *argp;
    lpctype_t *result = NULL;
    typeflags_t mask = 0;

    argp = (fulltype_t *) (type_of_arguments.block +
          (type_of_arguments.current_size -= sizeof (fulltype_t) * n) );

    while (--n >= 0 )
#line 2570 "prolang.y"
    {
        lpctype_t *oldresult = result;

        result = get_union_type(result, argp->t_type);
        mask |= argp->t_flags;

        free_lpctype(oldresult);
        free_fulltype(*argp);
        argp++;
    }

    if (result)
#line 2582 "prolang.y"
    {
        lpctype_t *arrtype = get_array_type(result);
        free_lpctype(result);
        return arrtype;
    }

    return lpctype_any_array;

} /* get_aggregate_type() */


/*-------------------------------------------------------------------------*/
static INLINE void
warn_function_shadow ( const string_t *pubProg,  string_t * pubFun
                     , const string_t *privProg, string_t * privFun
                     )

/* Issue a warning that the public function <pubProg>::<pubFun>() shadows the
 * private function <privProg>::<privFun>().
 * Both <pubProg> and <privProg> can be NULL.
 * If the function is __INIT(), no warning is printed.
 */

#line 2605 "prolang.y"
{
    string_t *pubCProg = NULL;
    string_t *privCProg = NULL;

    if (mstreq(pubFun, STR_VARINIT)
     && mstreq(privFun, STR_VARINIT))
        return;

    if (pubProg != NULL)  pubCProg = cvt_progname(pubProg);
    if (privProg != NULL) privCProg = cvt_progname(privProg);

    if (pubCProg != NULL)
#line 2617 "prolang.y"
    {
        if (privCProg != NULL)
            yywarnf("public %s::%s() shadows private %s::%s()"
                  , get_txt(pubCProg), get_txt(pubFun)
                  , get_txt(privCProg), get_txt(privFun)
                );
        else
            yywarnf("public %s::%s() shadows private %s()"
                  , get_txt(pubCProg), get_txt(pubFun)
                  , get_txt(privFun)
                );
    }
    else if (privCProg != NULL)
        yywarnf("public %s() shadows private %s::%s()"
              , get_txt(pubFun)
              , get_txt(privCProg), get_txt(privFun)
            );
    else
        yywarnf("public %s() shadows private %s()"
              , get_txt(pubFun)
              , get_txt(privFun)
            );
    
    if (pubCProg != NULL)  free_mstring(pubCProg);
    if (privCProg != NULL) free_mstring(privCProg);
} /* warn_function_shadow() */

/* =============================   CODEGEN   ============================= */

/*-------------------------------------------------------------------------*/
static INLINE char *
realloc_a_program (size_t size)

/* If necessary, increase the allocated size of the A_PROGRAM area so that at
 * least <size> more bytes can be stored in it.
 *
 * Return NULL when out of memory, or a pointer to the (possibly newly
 * allocated) memory area (ie. mem_block[A_PROGRAM].block).
 */

#line 2657 "prolang.y"
{
    mem_block_t * mbp = &mem_block[A_PROGRAM];
    mp_uint new_size = mbp->current_size + size;

    if (new_size <= mbp->max_size)
        return mbp->block;
    return realloc_mem_block(mbp, new_size);
} /* realloc_a_program() */

/*-------------------------------------------------------------------------*/

#define ins_byte(b) byte_to_mem_block(A_PROGRAM, b)

#ifndef ins_byte

static INLINE void
ins_byte (bytecode_t b)

/* Add the byte <b> to the A_PROGRAM area.
 */

#line 2678 "prolang.y"
{
    if (mem_block[A_PROGRAM].current_size == mem_block[A_PROGRAM].max_size )
#line 2680 "prolang.y"
    {
        if (!realloc_a_program(1))
#line 2682 "prolang.y"
        {
            yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                    , mem_block[A_PROGRAM].current_size + 1);
            return;
        }
    }
    mem_block[A_PROGRAM].block[mem_block[A_PROGRAM].current_size++] = b;
} /* ins_byte() */

#endif

/*-------------------------------------------------------------------------*/
static void
ins_f_code (unsigned int b)

/* Add the instruction <b> to the A_PROGRAM area, taking care of encoding
 * multi-byte instructions properly.
 */

#line 2701 "prolang.y"
{
    if (instrs[b].prefix)
        ins_byte(instrs[b].prefix);
    ins_byte(instrs[b].opcode);
} /* ins_f_code() */

/*-------------------------------------------------------------------------*/
static int
ins_f_code_buf (unsigned int b, bytecode_p buf)

/* Add the instruction <b> to the <buf>, taking care of encoding
 * multi-byte instructions properly. Returns the number of
 * bytes written. The buffer must have space for at least 2 bytes.
 */

#line 2716 "prolang.y"
{
    int pos = 0;
    if (instrs[b].prefix)
        buf[pos++] = instrs[b].prefix;
    buf[pos++] = instrs[b].opcode;

    return pos;
} /* ins_f_code_buf() */

/*-------------------------------------------------------------------------*/
static void
ins_short (long l)

/* Add the 2-byte number <l> to the A_PROGRAM area in a fixed byteorder.
 */

#line 2732 "prolang.y"
{
    if (l > (long)USHRT_MAX || l < SHRT_MIN)
        yyerrorf("Compiler error: too large number %lx passed to ins_short()"
                , l);

    if (realloc_a_program(sizeof(short)))
#line 2738 "prolang.y"
    {
        put_short(PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE, l);
        CURRENT_PROGRAM_SIZE += sizeof(short);
    }
    else
#line 2743 "prolang.y"
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                , CURRENT_PROGRAM_SIZE + sizeof(short));
    }
} /* ins_short() */

/*-------------------------------------------------------------------------*/
static void
upd_short (bc_offset_t offset, long l)

/* Store the 2-byte number <l> at <offset> in the A_PROGRAM area in
 * a fixed byteorder.
 */

#line 2757 "prolang.y"
{
    if (l > (long)USHRT_MAX || l < SHRT_MIN)
        yyerrorf("Compiler error: too large number %ld passed to upd_short()"
                , l);

    put_short(PROGRAM_BLOCK + offset, l);

} /* upd_short() */

/*-------------------------------------------------------------------------*/
static short
read_short (bc_offset_t offset)

/* Return the 2-byte number stored at <offset> in the A_PROGRAM area.
 */

#line 2773 "prolang.y"
{
    return get_short(PROGRAM_BLOCK + offset);

} /* read_short() */

/*-------------------------------------------------------------------------*/
static void
ins_jump_offset (bc_offset_t l)
/* Add the jump offset <l> to the A_PROGRAM area in a fixed byteorder.
 */
#line 2783 "prolang.y"
{
    if (realloc_a_program(sizeof(l)))
#line 2785 "prolang.y"
    {
        put_bc_offset(PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE, l);
        CURRENT_PROGRAM_SIZE += sizeof(l);
    }
    else
#line 2790 "prolang.y"
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                 , CURRENT_PROGRAM_SIZE + sizeof(l));
    }
} /* ins_jump_offset() */

/*-------------------------------------------------------------------------*/
static void
upd_jump_offset (bc_offset_t offset, bc_offset_t l)
/* Store the new offset <l> at <offset> in the A_PROGRAM area in
 * a fixed byteorder.
 */
#line 2802 "prolang.y"
{
    put_bc_offset(PROGRAM_BLOCK + offset, l);
} /* upd_jump_offset() */

/*-------------------------------------------------------------------------*/
static bc_offset_t
read_jump_offset (bc_offset_t offset)
/* Return the jump offset stored at <offset> in the A_PROGRAM area.
 */
#line 2811 "prolang.y"
{
    return get_bc_offset(PROGRAM_BLOCK + offset);
} /* read_jump_offset() */

/*-------------------------------------------------------------------------*/
#ifndef FLOAT_FORMAT_2
static void
ins_uint32 (uint32_t l)
 // Add the uint32_t <l> to the A_PROGRAM area.
 // Currently only used when not using FLOAT_FORMAT_2.
#line 2821 "prolang.y"
{
    if (realloc_a_program(sizeof(uint32_t)))
#line 2823 "prolang.y"
    {
        put_uint32(PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE, l);
        CURRENT_PROGRAM_SIZE += sizeof(uint32_t);
    }
    else
#line 2828 "prolang.y"
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                , CURRENT_PROGRAM_SIZE + sizeof(uint32_t));
    }
 } // ins_uint32()
#endif // FLOAT_FORMAT_2
/*-------------------------------------------------------------------------*/
static void
ins_double (double d)
/* Add the double <d> to the A_PROGRAM area.
 */
#line 2839 "prolang.y"
{
    if (realloc_a_program(sizeof(double)))
#line 2841 "prolang.y"
    {
        put_double(PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE, d);
        CURRENT_PROGRAM_SIZE += sizeof(double);
    }
    else
#line 2846 "prolang.y"
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                 , CURRENT_PROGRAM_SIZE + sizeof(double));
    }
} /* ins_double() */
/*-------------------------------------------------------------------------*/
static void upd_uint32 (bc_offset_t offset, uint32_t l) UNUSED;
static void
upd_uint32 (bc_offset_t offset, uint32_t l)
/* Store the uint32_t <l> at <offset> in the A_PROGRAM are.
 */
#line 2857 "prolang.y"
{
    put_uint32(PROGRAM_BLOCK + offset, l);
} /* upd_uint32() */

/*-------------------------------------------------------------------------*/
static uint32 read_uint32 (bc_offset_t offset) UNUSED;
static uint32
read_uint32 (bc_offset_t offset)
// Return the uint32_t stored at <offset> in the A_PROGRAM area.
#line 2866 "prolang.y"
{
    return get_uint32(PROGRAM_BLOCK + offset);
} /* read_uint32() */

/*-------------------------------------------------------------------------*/
#ifndef FLOAT_FORMAT_2
static void
ins_uint16 (uint16_t l)
 // Add the uint16_t <l> to the A_PROGRAM area.
 // Currently only used when not using FLOAT_FORMAT_2.
#line 2876 "prolang.y"
{
    if (realloc_a_program(sizeof(l)))
#line 2878 "prolang.y"
    {
        put_uint16(PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE, l);
        CURRENT_PROGRAM_SIZE += sizeof(l);
    }
    else
#line 2883 "prolang.y"
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                 , CURRENT_PROGRAM_SIZE + sizeof(l));
    }
 } // ins_uint16()
#endif // FLOAT_FORMAT_2

/*-------------------------------------------------------------------------*/
static INLINE void
ins_p_int (p_int num)

/* Add the number <num> to the A_PROGRAM area in a fixed byteorder.
 */
#line 2896 "prolang.y"
{
    if (realloc_a_program(sizeof(num)))
#line 2898 "prolang.y"
    {
        /* F_NUMBER expects the number in the host format. Therefore memcpy()
         * is OK. interpret.c will read the number with memcpy() as well.
         * TODO: use a suitable PUT_ from bytecode.h (change in interpret.c as well!)
         */
        memcpy(mem_block[A_PROGRAM].block + CURRENT_PROGRAM_SIZE, &num, sizeof(num));
        CURRENT_PROGRAM_SIZE += sizeof(num);
    }
    else
#line 2907 "prolang.y"
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                 , mem_block[A_PROGRAM].current_size + sizeof(num));
    }
} /* ins_p_int() */

/*-------------------------------------------------------------------------*/
static INLINE void
upd_p_int (mp_uint offset, p_int num)

/* Store the number <num> at <offset> in the A_PROGRAM area in a fixed byteorder.
 */
#line 2919 "prolang.y"
{
    /* F_NUMBER expects the number in the host format. Therefore memcpy()
     * is OK. interpret.c will read the number with memcpy() as well.
     * TODO: use a suitable PUT_ from bytecode.h (change in interpret.c as well!)
     */
    memcpy(mem_block[A_PROGRAM].block + offset, &num, sizeof(num));

} /* upd_p_int() */

/*-------------------------------------------------------------------------*/
static p_int
read_p_int (mp_uint offset)

/* Return the <number> stored at <offset> in the A_PROGRAM area.
 */
#line 2934 "prolang.y"
{
    p_int number;
    /* TODO: use GET_ function from bytecode.h */
    memcpy(&number, mem_block[A_PROGRAM].block + offset, sizeof(number));

    return number;
} /* read_p_int() */

/*-------------------------------------------------------------------------*/
static void
ins_number (p_int num)

/* Insert code to push number <num> onto the stack.
 * The function tries to find the shortest sequence to do so.
 */

#line 2950 "prolang.y"
{
    if (num == 0)
        ins_f_code(F_CONST0);
    else if (num == 1)
#line 2954 "prolang.y"
    {
        ins_f_code(F_CONST1);
    }
    else if (num == -1)
        ins_f_code(F_NCONST1);
    else if (num >= 0 && num <= 0x0FF)
#line 2960 "prolang.y"
    {
        ins_f_code(F_CLIT);
        ins_byte((num & 0xFF));
    }
    else if (num < 0 && num >= -0x0FF)
#line 2965 "prolang.y"
    {
        ins_f_code(F_NCLIT);
        ins_byte(((-num) & 0xFF));
    }
    else
#line 2970 "prolang.y"
    {
        ins_f_code(F_NUMBER);
        ins_p_int(num);
    }
} /* ins_number() */

/*-------------------------------------------------------------------------*/
/* The following macros are used for a speedy codegeneration within bigger
 * functions.
 *
 * To insert at max <n> bytes, the function has to declare
 *
 *    PREPARE_INSERT(n)
 *
 * among the variables and can the use the following macros to add bytes:
 *
 *    add_f_code(i): to add instruction <i> to the program
 *    add_byte(b):   to add byte <b> to the program
 *    add_short(s):  to add short <s> to the program
 *
 * Except for add_f_code(), none of the macros adapts CURRENT_PROGRAM_SIZE,
 * and add_f_code() increments the _SIZE only for the prefix byte if any.
 */

#define PREPARE_INSERT(n) \
    bytecode_p __PREPARE_INSERT__p = (\
      realloc_a_program(n) ? (PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE) : NULL);

#define add_byte(b)   (void) STORE_UINT8(__PREPARE_INSERT__p, (b))

#define add_short(s) STORE_SHORT(__PREPARE_INSERT__p, (s))

#define add_f_code(i) \
    do{ if (instrs[i].prefix) { \
            add_byte(instrs[i].prefix); \
            CURRENT_PROGRAM_SIZE++; \
        }\
        add_byte(instrs[i].opcode); \
    }while(0)

/*-------------------------------------------------------------------------*/
static void
push_address (void)

/* Push the current program size as address onto the compiler stack.
 */

#line 3017 "prolang.y"
{
    if (comp_stackp >= COMPILER_STACK_SIZE)
#line 3019 "prolang.y"
    {
        yyerror("Compiler stack overflow");
        /* Don't store the address, but keep proper track of the depth.
         */
        comp_stackp++;
        return;
    }
    comp_stack[comp_stackp++] = mem_block[A_PROGRAM].current_size;
} /* push_address() */

/*-------------------------------------------------------------------------*/
static void
push_explicit (p_int address)

/* Push the program <address> onto the compiler stack.
 */

#line 3036 "prolang.y"
{
    if (comp_stackp >= COMPILER_STACK_SIZE)
#line 3038 "prolang.y"
    {
        yyerror("Compiler stack overflow");
        /* Don't store the address, but keep proper track of the depth.
         */
        comp_stackp++;
        return;
    }
    comp_stack[comp_stackp++] = address;
} /* push_explicit() */

/*-------------------------------------------------------------------------*/
static p_int
pop_address (void)

/* Pop the most recent stored address from the compiler stack and return
 * it.
 */

#line 3056 "prolang.y"
{
    if (comp_stackp == 0)
        fatal("Compiler stack underflow.\n");
    if (comp_stackp > COMPILER_STACK_SIZE)
#line 3060 "prolang.y"
    {
        /* Nothing to retrieve, but keep track of the depth */
        --comp_stackp;
        return 0;
    }
    return comp_stack[--comp_stackp];
} /* pop_address() */

/*-------------------------------------------------------------------------*/
static Bool
fix_branch (int ltoken, p_int dest, p_int loc)

/* Backpatch a branch instruction at <loc> to jump to <dest>.
 * If the offset exceeds the 255 range, the branch instruction is changed
 * into its long-branch variant <ltoken>.
 *
 * Return TRUE if the long branch had to be used, FALSE otherwise.
 * TODO: This really confuses the line number detection code, as suddenly
 * TODO:: the recorded offset are no longer accurate.
 */

#line 3081 "prolang.y"
{
    p_int offset;  /* The branch offset */

    offset = dest - (loc +1);

    if (offset > 0xff)
#line 3087 "prolang.y"
    {
        /* We need a long branch. That also means that we have to
         * move the following code and adapt remembered addresses.
         */
        bc_offset_t i, j;
        bytecode_p p;

        mem_block[A_PROGRAM].block[loc] = 0; /* Init it */

        /* Update the break address */
        if ( current_break_address > loc
         && !(current_break_address & (BREAK_ON_STACK|BREAK_DELIMITER) ) )
#line 3099 "prolang.y"
        {
            for (i = current_break_address & BREAK_ADDRESS_MASK
                ; (j = read_jump_offset(i)) > loc; )
#line 3102 "prolang.y"
            {
                upd_jump_offset(i, j+1);
                i = j;
            }
            current_break_address++;
        }

        /* Update the continue address */
        if ( (current_continue_address & CONTINUE_ADDRESS_MASK) > loc
         && !(current_continue_address & CONTINUE_DELIMITER ) )
#line 3112 "prolang.y"
        {
            for(i = current_continue_address & CONTINUE_ADDRESS_MASK;
              (j=read_jump_offset(i)) > loc; )
#line 3115 "prolang.y"
            {
                upd_jump_offset(i, j+1);
                i = j;
            }
            current_continue_address++;
        }

        ins_byte(0); /* Just to make sure the memory is there */

        /* Move the code */
        p = PROGRAM_BLOCK + mem_block[A_PROGRAM].current_size-1;
        i = mem_block[A_PROGRAM].current_size - loc;
        for( ; --i >= 0; --p )
#line 3128 "prolang.y"
        {
            PUT_CODE(p, GET_CODE(p-1));
        }

        /* Store the new branch instruction */
        PUT_CODE(p, ltoken);
        upd_short(loc, offset+2);

        if (offset > 0x7ffd)
            yyerrorf("Compiler limit: Too much code to branch over: %"
                      PRIdPINT" bytes", offset);

        return MY_TRUE;
    }
    else
#line 3143 "prolang.y"
    {
        /* Just update the offset */
        mem_block[A_PROGRAM].block[loc] = offset;
        return MY_FALSE;
    }
} /* fix_branch() */

/*-------------------------------------------------------------------------*/
static bytecode_p
yyget_space (p_int size)

/* Callback function for switch: return a pointer to <size> more bytes
 * in the program area.
 */

#line 3158 "prolang.y"
{
    if (realloc_a_program(size))
#line 3160 "prolang.y"
    {
        CURRENT_PROGRAM_SIZE += size;
        return PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE - size;
    }
    yyerrorf("Out of memory: program size %"PRIdMPINT"\n"
            , mem_block[A_PROGRAM].current_size + size);
    return NULL;
} /* yyget_space() */

/*-------------------------------------------------------------------------*/
static void
yymove_switch_instructions (int len, p_int blocklen)

/* Callback function for switch: move the <blocklen> bytecodes at <switch_pc>
 * back by <len> bytes to <switch_pc>+<len>. A continue address in the
 * affected area is corrected.
 */

#line 3178 "prolang.y"
{
    bc_offset_t i, j;

    if (realloc_a_program(len))
#line 3182 "prolang.y"
    {
        CURRENT_PROGRAM_SIZE += len;

        /* Adjust the continue address, if any */
        if ( (current_continue_address & CONTINUE_ADDRESS_MASK) > switch_pc
         && !(current_continue_address & CONTINUE_DELIMITER ) )
#line 3188 "prolang.y"
        {
            for(i = current_continue_address & CONTINUE_ADDRESS_MASK;
              (j=read_jump_offset(i)) > switch_pc; )
#line 3191 "prolang.y"
            {
                    upd_jump_offset(i, j+len);
                    i = j;
            }
            current_continue_address += len;
        }

        memmove(
          mem_block[A_PROGRAM].block + switch_pc + len,
          mem_block[A_PROGRAM].block + switch_pc,
          blocklen
        );
    }
    else
#line 3205 "prolang.y"
    {
        yyerrorf("Out of memory: program size %"PRIdMPINT"\n"
                , mem_block[A_PROGRAM].current_size + len);
    }
} /* yymove_switch_instructions() */

/*-------------------------------------------------------------------------*/
static void
yycerrorl (const char *s1, const char *s2, int line1, int line2)

/* Callback function for switch: Raise an error <s1> in file <s2> at
 * lines <line1> and <line2>.
 * <s1> may contain one '%s' to insert s2, <s2> may contain one or
 * or two '%d' to insert line1 and line2.
 */

#line 3221 "prolang.y"
{
    char buff[100];

    sprintf(buff, s2, line1, line2);
    yyerrorf(s1, buff);
} /* yycerrorl() */

/*-------------------------------------------------------------------------*/
static void
update_lop_branch ( p_uint address, int instruction )

/* <address> points to the branch offset value of an LAND/LOR operation,
 * currently set to 0. Update that offset to branch to the current end
 * of the program.
 *
 * If that branch is too long, the code is rewritten:
 *
 *     Original:             Rewritten:
 *
 *      <expr1>                <expr1>
 *      LOR/LAND l             DUP
 *      <expr2>                LBRANCH_<instruction>
 *   l:                        POP_VALUE
 *                             <expr2>
 *                          l:
 *
 * The extra DUP compensates the svalue the LBRANCH eats.
 * The LBRANCH_<instruction> needs to be passed suiting the logical
 * operator: LBRANCH_WHEN_ZERO for LAND, LBRANCH_WHEN_NON_ZERO for LOR.
 */

#line 3252 "prolang.y"
{
    p_int offset;

    last_expression = -1;

    offset = mem_block[A_PROGRAM].current_size - ( address + 1);
    if (offset > 0xff)
#line 3259 "prolang.y"
    {
        /* A long branch is needed */

        int i;
        bytecode_p p;

        ins_short(0);
        ins_byte(0);
        p = PROGRAM_BLOCK + mem_block[A_PROGRAM].current_size-1;
        for (i = offset; --i >= 0; --p )
            *p = p[-3];
        p[-4] = F_DUP;
        p[-3] = instruction;
        upd_short(address+1, offset+3);
        if (offset > 0x7ffc)
            yyerrorf("Compiler limit: Too much code to skip for ||/&&:"
                     " %"PRIdPINT" bytes" , offset);
        p[0]  = F_POP_VALUE;
    }
    else
#line 3279 "prolang.y"
    {
        mem_block[A_PROGRAM].block[address] = offset;
    }
} /* update_lop_branch() */

/*-------------------------------------------------------------------------*/
static void
shuffle_code (p_uint start1, p_uint start2, p_uint end)

/* Reverse the order of the program blocks [start1..start2[ and [start2..end[
 */

#line 3291 "prolang.y"
{
    p_uint len1 = start2 - start1;
    p_uint len2 = end - start2;

    bytecode_p pStart1 = PROGRAM_BLOCK + start1;
    bytecode_p pStart2 = PROGRAM_BLOCK + start2;

    bytecode_p * pTmp;

    if (!len1 || !len2)
        return;

    pTmp = xalloc(len1);
    if (!pTmp)
#line 3305 "prolang.y"
    {
        yyerror("(shuffle_code) Out of memory");
        return;
    }
    memmove(pTmp, pStart1, len1);
    memmove(pStart1, pStart2, len2);
    memmove(pStart1+len2, pTmp, len1);
    xfree(pTmp);
} /* shuffle_code() */

/* ========================   LVALUE CODE   ======================== */
lvalue_block_t
alloc_lvalue_block (p_int size)

/* Creates an empty lvalue block of the given size.
 */

#line 3322 "prolang.y"
{
    lvalue_block_t result = { LVALUE_BLOCK_SIZE, size };
    if (!extend_mem_block(A_LVALUE_CODE, size))
        result.size = 0;

    return result;
} /* alloc_lvalue_block() */

/*-------------------------------------------------------------------------*/
lvalue_block_t
compose_lvalue_block (lvalue_block_t previous_block, int post_lvalue_instruction, p_int argument_start, int final_instruction)

/* Creates an lvalue code block with the following elements:
 *
 * 1. A previous lvalue block <previous_block>.
 *    If its .size is 0, then it is ignored.
 * 2. Following an instruction (if 0, then ignored)
 * 3. Code from the PROGRAM_BLOCK starting from <argument_start>
 *    to its end (if -1, then ignored).
 * 4. A final instruction (if 0, then ignored)
 */

#line 3344 "prolang.y"
{
    lvalue_block_t result = previous_block;

    p_int size_to_add = 0;

    if (post_lvalue_instruction)
#line 3350 "prolang.y"
    {
        size_to_add++;
        if (instrs[post_lvalue_instruction].prefix)
            size_to_add++;
    }

    if (final_instruction)
#line 3357 "prolang.y"
    {
        size_to_add++;
        if (instrs[final_instruction].prefix)
            size_to_add++;
    }

    if (argument_start >= 0)
         size_to_add += CURRENT_PROGRAM_SIZE - argument_start;

    if (previous_block.size == 0)
#line 3367 "prolang.y"
    {
        /* No previous block, start a new one at the end. */
        result.start = LVALUE_BLOCK_SIZE;
    }
    else if(previous_block.start + previous_block.size != LVALUE_BLOCK_SIZE)
#line 3372 "prolang.y"
    {
        /* If we can't append, we need to copy <previous_block> */
        if (!reserve_mem_block(A_LVALUE_CODE, previous_block.size + size_to_add))
            return result;

        result.start = LVALUE_BLOCK_SIZE;
        memcpy(LVALUE_BLOCK + LVALUE_BLOCK_SIZE, LVALUE_BLOCK + previous_block.start, previous_block.size);
        LVALUE_BLOCK_SIZE += previous_block.size;
    }
    else
#line 3382 "prolang.y"
    {
        /* We can append. */
    }

    if (!reserve_mem_block(A_LVALUE_CODE, size_to_add))
        return result;

    /* Add the first instruction. */
    if (post_lvalue_instruction)
        LVALUE_BLOCK_SIZE += ins_f_code_buf(post_lvalue_instruction, LVALUE_BLOCK + LVALUE_BLOCK_SIZE);

    /* Add the arguments. */
    if (argument_start >= 0)
#line 3395 "prolang.y"
    {
        p_int len = CURRENT_PROGRAM_SIZE - argument_start;
        memcpy(LVALUE_BLOCK + LVALUE_BLOCK_SIZE, PROGRAM_BLOCK + argument_start, len);
        LVALUE_BLOCK_SIZE += len;
    }

    /* Add the final instruction. */
    if (final_instruction)
        LVALUE_BLOCK_SIZE += ins_f_code_buf(final_instruction, LVALUE_BLOCK + LVALUE_BLOCK_SIZE);

    result.size += size_to_add;
    return result;
} /* make_lvalue_block() */

/*-------------------------------------------------------------------------*/
void
free_lvalue_block (lvalue_block_t block)

/* If the given block is at the end of the mempool for lvalue code,
 * then remove that block. Otherwise just ignore it, the whole
 * mempool will be freed at the end of the compilation.
 */

#line 3418 "prolang.y"
{
    if (block.start + block.size == LVALUE_BLOCK_SIZE)
        LVALUE_BLOCK_SIZE -= block.size;
} /* free_lvalue_block() */


/* ========================   LOCALS and SCOPES   ======================== */

/*-------------------------------------------------------------------------*/
void
free_all_local_names (void)

/* Free all local names, and reset the counters.
 */

#line 3433 "prolang.y"
{
    ident_t *p,*q;

    for (q = all_locals; NULL != (p = q);)
#line 3437 "prolang.y"
    {
        q = p->next_all;
        free_shared_identifier(p);
    }

    while (current_number_of_locals > 0 && type_of_locals)
#line 3443 "prolang.y"
    {
        current_number_of_locals--;
        free_fulltype(type_of_locals[current_number_of_locals]);
    }

    /* Free also types of context variables. */
    if (type_of_context && type_of_context != &(LOCAL_TYPE(0)))
#line 3450 "prolang.y"
    {
        int i;
        for (i=0; i<MAX_LOCAL; i++)
            free_fulltype(type_of_context[i]);
    }

    all_locals = NULL;
    current_number_of_locals = 0;
    max_number_of_locals = 0;
    current_break_stack_need = 0;
    max_break_stack_need = 0;
    def_function_ident = NULL;
} /* free_all_local_names() */

/*-------------------------------------------------------------------------*/
static void
free_local_names (int depth)

/* Free all locals in the all_locals list which are of higher or
 * the same <depth>, and adjust the counters.
 * A <depth> of 0 is equivalent to calling free_all_local_names().
 */

#line 3473 "prolang.y"
{
    ident_t *q;

    if (!depth)
#line 3477 "prolang.y"
    {
        free_all_local_names();
        return;
    }

    /* Are the locals of the given depth? */
    if (!all_locals || all_locals->u.local.depth < depth)
        return;

    if (all_locals->u.local.depth > depth)
        fatal("List of locals clobbered: depth %d, block_depth %d\n"
             , all_locals->u.local.depth, depth);

    while (all_locals != NULL && all_locals->u.local.depth >= depth)
#line 3491 "prolang.y"
    {
        q = all_locals;
        all_locals = q->next_all;
        free_shared_identifier(q);
        if (q->u.local.context >= 0)
#line 3496 "prolang.y"
        {
            free_fulltype(type_of_context[q->u.local.context]);
        }
        else
#line 3500 "prolang.y"
        {
            current_number_of_locals--;
            free_fulltype(type_of_locals[current_number_of_locals]);
        }
    }
} /* free_local_names() */

/*-------------------------------------------------------------------------*/
static ident_t *
add_local_name (ident_t *ident, fulltype_t type, int depth)

/* Declare a new local variable <ident> with the type <type> on
 * the scope depth <depth>. The references of <type> ARE adopted.
 * Return the (adjusted) ident for the new variable.
 */

#line 3516 "prolang.y"
{
    if (type.t_type == lpctype_void)
#line 3518 "prolang.y"
    {
        yyerrorf( "Illegal to define variable '%s' as type 'void'"
                , get_txt(ident->name));
    }

    if (current_number_of_locals >= MAX_LOCAL) /* size of type recording array */
        yyerror("Too many local variables");

    else
#line 3527 "prolang.y"
    {
        if (ident->type != I_TYPE_UNKNOWN)
#line 3529 "prolang.y"
        {
            /* We're overlaying some other definition.
             * If it's a global, or if we are in an inline-closure arg list,
             * it's ok.
             */
#ifdef DEBUG_INLINES
if (current_inline && current_inline->block_depth+2 == block_depth 
    && ident->type != I_TYPE_GLOBAL)
    printf("DEBUG: redeclare local '%s' as inline arg, depth %d\n", 
           get_txt(ident->name), block_depth);
#endif /* DEBUG_INLINES */
            if (ident->type != I_TYPE_GLOBAL
             && !(current_inline && current_inline->block_depth+2 == block_depth)
               )
#line 3543 "prolang.y"
            {
                yywarnf( "Variable '%s' shadows previous declaration"
                       , get_txt(ident->name));
            }
            ident = make_shared_identifier_mstr(ident->name, I_TYPE_LOCAL, depth);
        }

        /* Initialize the ident */
        ident->type = I_TYPE_LOCAL;
        ident->u.local.num = current_number_of_locals;
        ident->u.local.depth = depth;
        ident->u.local.context = -1;

        /* Put the ident into the list of all locals */
        if (all_locals && all_locals->u.local.depth > depth)
            fatal("List of locals clobbered: depth %d, adding depth %d\n"
                 , all_locals->u.local.depth, depth);
        ident->next_all = all_locals;
        all_locals = ident;

        /* Record the type */
        type_of_locals[current_number_of_locals++] = type;

        /* And update the scope information */
        if (current_number_of_locals > max_number_of_locals)
            max_number_of_locals = current_number_of_locals;
        block_scope[depth-1].num_locals++;
    }

    return ident;
} /* add_local_name() */

/*-------------------------------------------------------------------------*/
static ident_t *
redeclare_local (ident_t *ident, fulltype_t type, int depth)

/* Redeclare a local name <ident>, to <type> at <depth>; the references
 * of <type> ARE adopted (and freed on error, except fatal ones).
 * If this happens on a deeper level, it is legal: the new declaration
 * is added and the new identifier is returned.
 * If it is illegal, an yyerror() is risen and the ident of the older
 * declaration is returned for error recovery.
 */

#line 3587 "prolang.y"
{
    if (all_locals && all_locals->u.local.depth > depth)
#line 3589 "prolang.y"
    {
        fatal("List of locals clobbered: list depth %d, "
              "block depth %d\n"
              , all_locals->u.local.depth, depth);
    }


    if (ident->u.local.depth >= depth
     || (ident->u.local.depth == 1 && depth == 2)
     || (current_inline && ident->u.local.depth == current_inline->block_depth+2
                        && depth == current_inline->block_depth+3)
       )
#line 3601 "prolang.y"
    {
        yyerrorf("Illegal to redeclare local name '%s'", get_txt(ident->name));
        free_fulltype(type);
    }
    else
#line 3606 "prolang.y"
    {
        ident = add_local_name(ident, type, depth);
    }

    return ident;
} /* redeclare_local() */

/*-------------------------------------------------------------------------*/
static ident_t *
add_context_name (inline_closure_t *closure, ident_t *ident, lpctype_t *type, int num)

/* Declare a new context variable <ident> with the type <type> for the
 * currently compiled inline closure. The references of <type> are NOT adopted.
 * <num> is -1 for independent context
 * variables, or the index of the inherited local variable.
 * Return the (adjusted) ident for the new variable.
 */

#line 3624 "prolang.y"
{
    int depth;
    block_scope_t * block;

    depth = closure->block_depth+1;
    block = & block_scope[depth-1]; /* The context block scope. */

#ifdef DEBUG_INLINES
printf("DEBUG: add_context_name('%s', num %d) depth %d, context %d\n", 
       get_txt(ident->name), num, depth, block->num_locals);
#endif /* DEBUG_INLINES */
    if (block->num_locals >= MAX_LOCAL) /* size of type recording array */
#line 3636 "prolang.y"
    {
        yyerror("Too many context variables");
    }
    else if (num < 0
     && block->first_local + block->num_locals >= MAX_LOCAL
        )
#line 3642 "prolang.y"
    {
        yyerror("Too many local variables");
    }
    else
#line 3646 "prolang.y"
    {
        if (ident->type != I_TYPE_UNKNOWN)
#line 3648 "prolang.y"
        {
            /* We're overlaying some other definition, but that's ok.
             */
            ident = make_shared_identifier_mstr(ident->name, I_TYPE_LOCAL, depth);
            assert (ident->type == I_TYPE_UNKNOWN);
        }

        /* Initialize the ident */
        ident->type = I_TYPE_LOCAL;
        ident->u.local.depth = depth;
        if (num < 0)
#line 3659 "prolang.y"
        {
            /* First initialize it as a local variable
             * of the outer function, so they can be
             * referenced during initialization.
             */
            ident->u.local.num = block->first_local + block->num_locals;
            ident->u.local.context = -1;
        }
        else
#line 3668 "prolang.y"
        {
            ident->u.local.num = num;
            ident->u.local.context = block->num_locals;
        }

        /* Put the ident into the list of all locals.
         */
        if (all_locals && all_locals->u.local.depth > depth)
#line 3676 "prolang.y"
        {
            /* This context variable was detected after we already
             * added locals - find the proper insertion point.
             */
            ident_t * prev, *this;

            for ( prev = all_locals, this = all_locals->next_all
                ; this && this->u.local.depth > depth
                ; prev = this, this = this->next_all) NOOP;

            ident->next_all = this;
            prev->next_all = ident;
        }
        else
#line 3690 "prolang.y"
        {
            ident->next_all = all_locals;
            all_locals = ident;
        }

        /* Record the type */
        type_of_context[block->num_locals] = get_fulltype(ref_lpctype(type));

        if (num < 0)
            type_of_locals[ident->u.local.num] = get_fulltype(ref_lpctype(type));

        block->num_locals++;
    }

    return ident;
} /* add_context_name() */

/*-------------------------------------------------------------------------*/
static ident_t *
check_for_context_local (ident_t *ident, lpctype_t ** pType)

/* The LPC code uses local variable <ident>. If we're compiling
 * an inline closure, check if it is an inherited local for which
 * no context variable has been created yet. If yes, create the context
 * variable.
 * Return the (possibly updated) ident, and store the variables type
 * in *<pType> (the reference count is not increased).
 */

#line 3719 "prolang.y"
{
    int depth = ident->u.local.depth;

    if (!block_scope[depth-1].accessible)
        yyerrorf("Variable '%s' is in scope but inaccessible.\n",
                 get_txt(ident->name));

    if (current_inline
     && depth <= current_inline->block_depth
       )
#line 3729 "prolang.y"
    {
        inline_closure_t *closure;
        mp_int closure_nr;
        lpctype_t* type;

        closure = current_inline;
        closure_nr = closure - &(INLINE_CLOSURE(0));
        closure->next = -1;

        /* Go through all inline closures to our local variable
         * and record this path using the ->next pointers.
         *
         * Stop at the last inline closure before the variable's
         * scope, because the information about that scope was
         * saved there.
         */
        for (;;)
#line 3746 "prolang.y"
        {
            inline_closure_t *outer_closure;
            mp_int outer_closure_nr;

            outer_closure_nr = closure->prev;
            if (outer_closure_nr == -1)
                /* It is a local variable of the current function. */
                break;

            outer_closure = &(INLINE_CLOSURE(outer_closure_nr));
            outer_closure->next = closure_nr;

            if (outer_closure->block_depth < depth)
                /* The variable belongs to outer_closure. */
                break;

            closure = outer_closure;
            closure_nr = outer_closure_nr;
        }

        if (ident->u.local.context >= 0)
#line 3767 "prolang.y"
        {
            /* It's a context variable. */
            type = LOCAL_TYPE(closure->full_context_type_start
                              + ident->u.local.context
                             ).t_type;
        }
        else
#line 3774 "prolang.y"
        {
            /* It's a local variable. */
            type = LOCAL_TYPE(closure->full_local_type_start
                              + ident->u.local.num
                             ).t_type;
        }

        /* Now pass this context variable through
         * all surrounding inline closures.
         */
        while (MY_TRUE)
#line 3785 "prolang.y"
        {
            /* Skip closures whose context is being parsed,
             * because current_inline is not created
             * in their runtime.
             */
            if (!closure->parse_context)
#line 3791 "prolang.y"
            {
                ident = add_context_name(closure, ident, type,
                     ident->u.local.context >= 0
                     ? CONTEXT_VARIABLE_BASE + ident->u.local.context
                     : ident->u.local.num);
            }

            if (closure->next == -1)
                break;

            closure = &(INLINE_CLOSURE(closure->next));
        }

        *pType = type;
    }
    else if (ident->u.local.context >= 0)
        *pType = type_of_context[ident->u.local.context].t_type;
    else
        *pType = type_of_locals[ident->u.local.num].t_type;

    return ident;
} /* check_for_context_local() */

/*-------------------------------------------------------------------------*/
static void
adapt_context_names (void)

/* Convert all explicit context variables
 * from local variables to context variables.
 */

#line 3822 "prolang.y"
{
    int depth;
    block_scope_t *scope;

    depth = current_inline->block_depth+1;
    scope = block_scope + depth - 1;

    /* Are there locals of the given depth? */
    if (all_locals && all_locals->u.local.depth >= depth)
#line 3831 "prolang.y"
    {
        ident_t *q = all_locals;

        while (q != NULL && q->u.local.depth > depth)
            q = q->next_all;

        while (q != NULL && q->u.local.depth == depth)
#line 3838 "prolang.y"
        {
            free_fulltype(type_of_locals[q->u.local.num]);

            q->u.local.context = q->u.local.num - scope->first_local;
            q->u.local.num = -1;

            q = q->next_all;
        }
    }
} /* adapt_context_names() */

/*-------------------------------------------------------------------------*/
static void
init_scope (int depth)

/* Initialize the block_scope entry for block_depth <depth>.
 */

#line 3856 "prolang.y"
{
    block_scope[depth-1].num_locals = 0;
    block_scope[depth-1].first_local = current_number_of_locals;
    block_scope[depth-1].num_cleared = 0;
    block_scope[depth-1].clobbered = MY_FALSE;
    block_scope[depth-1].addr = 0;
    block_scope[depth-1].accessible = MY_TRUE;
} /* init_scope() */

/*-------------------------------------------------------------------------*/
static void
enter_block_scope (void)

/* Enter a new scope and initialize it.
 */

#line 3872 "prolang.y"
{
    if (block_depth == COMPILER_STACK_SIZE)
        yyerror("Too deep nesting of local blocks.\n");

    block_depth++;
    init_scope(block_depth);

} /* enter_block_scope() */

/*-------------------------------------------------------------------------*/
static void
leave_block_scope (Bool dontclobber)

/* Leave the current scope, freeing all local names defined in that scope.
 *
 * <dontclobber> should be MY_TRUE if the stack of the to-be-left scope
 * is independent of the outer scope (i.e. the scope of closures).
 */

#line 3891 "prolang.y"
{
        free_local_names(block_depth);
        block_depth--;
        if (block_depth && !dontclobber
         && (block_scope[block_depth].num_locals
          || block_scope[block_depth].clobbered))
#line 3897 "prolang.y"
        {
            /* the block we just left may have clobbered local variables */
            block_scope[block_depth-1].clobbered = MY_TRUE;
        }
} /* leave_block_scope() */


/* ======================   GLOBALS and FUNCTIONS   ====================== */

/*-------------------------------------------------------------------------*/
const char *
get_current_function_name()

/* Get the name of the function currently being defined. If there is
 * no such function, return NULL.
 */
#line 3913 "prolang.y"
{
    if (def_function_ident)
        return get_txt(def_function_ident->name);
    else
        return NULL;
}

/*-------------------------------------------------------------------------*/
static unsigned short
store_argument_types ( int num_arg )

/* Store the <num_arg> argument types from global type_of_locals[] into
 * the proper memblock and return the new argument start index.
 * It is task of the caller to store this start index where it belongs.
 *
 * If exact_types are not required, the function just returns
 * INDEX_START_NONE.
 */

#line 3932 "prolang.y"
{
    unsigned short argument_start_index;

    /* Store the function arguments, if required.
     */
    if (!exact_types)
#line 3938 "prolang.y"
    {
        argument_start_index = INDEX_START_NONE;
    }
    else
#line 3942 "prolang.y"
    {
        int i;

        /* Save the argument types.
         */
        argument_start_index = ARGTYPE_COUNT;
        for (i = 0; i < num_arg; i++)
#line 3949 "prolang.y"
        {
            ADD_ARGUMENT_TYPE(ref_lpctype(type_of_locals[i].t_type));
        }
    }

    return argument_start_index;
} /* store_argument_types() */

/*-------------------------------------------------------------------------*/
static int
define_new_function ( Bool complete, ident_t *p, int num_arg, int num_local
                    , p_int offset, funflag_t flags, fulltype_t type)

/* Define a new function <p> with the characteristics <num_arg>, <num_local>,
 * program <offset>, <flags> and <type>.
 * The references of <type> are NOT adopted.
 * Result is the number (index) of the function.
 *
 * The function is called whenever a function header (return type, name
 * and arguments) has been parsed - <complete> is FALSE then. Additionally,
 * the function is called as well after a functionbody has been parsed,
 * <complete> is TRUE then.
 *
 * This function is called at least twice for all function definitions:
 * first as prototype (flags & NAME_PROTOTYPE) when the function def is
 * encountered, then a second time for real when the function has been
 * completed. Explicit prototypes can cause additional calls.
 */

#line 3978 "prolang.y"
{
    int num;
    function_t fun;
    unsigned short argument_start_index;

    /* Move the visibility-info into flags */
    flags |= type.t_flags & ~TYPE_MOD_MASK;

    do {
        function_t *funp;
        Bool args_differ, compare_args;

        if (p->type != I_TYPE_GLOBAL) break;
        if ((num = p->u.global.function) < 0) break;

        funp = FUNCTION(num);

        if ((funp->flags & (NAME_INHERITED|TYPE_MOD_PRIVATE|NAME_HIDDEN))
         == (NAME_INHERITED|TYPE_MOD_PRIVATE|NAME_HIDDEN))
#line 3997 "prolang.y"
        {
            break;
        }

        /* The function was already defined. It may be one of several reasons:
         *
         *   1. There has been a prototype.
         *   2. There was the same function defined by inheritance.
         *   3. This function has been called, but not yet defined.
         *   4. The function is defined twice.
         *   5. A "late" prototype has been encountered.
         */

        args_differ = MY_FALSE;
        compare_args = MY_FALSE;

        /* The following checks are useful only when done before
         * a functionbody appears, otherwise the warning/error message
         * line numbers will be misleading.
         */
        if (!complete)
#line 4018 "prolang.y"
        {
            if ((funp->flags & TYPE_MOD_NO_MASK)
             && !(funp->flags & (NAME_PROTOTYPE|NAME_UNDEFINED))
             && ((flags & (NAME_PROTOTYPE|NAME_UNDEFINED)) == (NAME_PROTOTYPE|NAME_UNDEFINED))
               )
                yyerrorf("Illegal to redefine 'nomask' function \"%s\""
                        , get_txt(p->name));

            if (!(funp->flags & (NAME_UNDEFINED|NAME_PROTOTYPE|NAME_INHERITED) ) )
#line 4027 "prolang.y"
            {
                yyerrorf("Redeclaration of function %s.", get_txt(p->name));
                if ( !(flags & NAME_PROTOTYPE) )
                    free_mstring(p->name);
                return num;
            }

            /* It was either an undefined but used function, or an inherited
             * function. In both cases, we now consider this to be THE new
             * definition. It might also have been a prototype to an already
             * defined function.
             *
             * Check arguments only when types are supposed to be tested,
             * and if this function really has been defined already.
             *
             * 'nomask' functions may not be redefined.
             */
            if (exact_types && funp->type != lpctype_unknown)
#line 4045 "prolang.y"
            {
                lpctype_t *new_type, *old_type;

                // get old+new functions flags
                funflag_t new_fflags = flags;
                funflag_t old_fflags = funp->flags;

                // We first check the return types for consistency.
                // If the new function has no type, it will be handled as lpctype_mixed.
                if (type.t_type)
                    new_type = type.t_type;
                else
                    new_type = lpctype_mixed;
                old_type = funp->type;
                if (old_fflags & NAME_INHERITED)
#line 4060 "prolang.y"
                {
                    // If the existing function was inherited, we (only) require type compatibility
                    if (!has_common_type(new_type, old_type))
#line 4063 "prolang.y"
                    {
                        if (pragma_pedantic)
                            yyerrorf("Inconsistent declaration of '%s': Return type mismatch %s", get_txt(p->name), get_two_lpctypes(old_type, new_type));
                        else if (pragma_check_overloads)
                            yywarnf("Inconsistent declaration of '%s': Return type mismatch %s", get_txt(p->name), get_two_lpctypes(old_type, new_type));
                    }
                }
                else
#line 4071 "prolang.y"
                {
                    // In all other cases we require that types are identical.
                    // (All protoypes+definitions in one file should be consistent.
                    if (new_type != old_type)
#line 4075 "prolang.y"
                    {
                        yyerrorf("Inconsistent declaration of '%s': Return type mismatch %s", get_txt(p->name), get_two_lpctypes(old_type, new_type));
                    }
                }

                // Then check the number of arguments and varargs flags and determine
                // if we should check the argument types (later).
                if (funp->num_arg > num_arg && !(funp->flags & TYPE_MOD_VARARGS))
                    yyerrorf("Incorrect number of arguments in redefinition of '%s'", get_txt(p->name));
                else if (funp->num_arg == num_arg
                      && ((funp->flags ^ flags) & TYPE_MOD_XVARARGS)
                      && !(funp->flags & TYPE_MOD_VARARGS))
                    yyerrorf("Incorrect number of arguments in redefinition of '%s'", get_txt(p->name));
                else
#line 4089 "prolang.y"
                {
                    unsigned short first_arg;

                    first_arg = ARGUMENT_INDEX(num);
                    if (first_arg == INDEX_START_NONE)
#line 4094 "prolang.y"
                    {
                        if (num_arg && !(funp->flags & NAME_TYPES_LOST) )
                            yyerrorf(
                              "Redefined function '%s' not compiled with type testing"
                            , get_txt(p->name));
                    }
                    else
#line 4101 "prolang.y"
                    {
                        /* We can compare the arguments */
                        compare_args = MY_TRUE;
                    }
                } /* cases (number of arguments) */

                /* If it's a prototype->function redefinition, check if the
                 * visibility is conserved. For redefining inherited functions
                 * different visibility is OK.
                 */
#               define TYPE_MOD_VIS \
                        ( TYPE_MOD_NO_MASK \
                        | TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC \
                        |TYPE_MOD_STATIC | TYPE_MOD_PROTECTED)

                if (!(old_fflags & (NAME_INHERITED|NAME_TYPES_LOST))
                    && ((new_fflags ^ old_fflags) & TYPE_MOD_VIS)
                    )
#line 4119 "prolang.y"
                {
                    char buff[120];
                    strncpy(buff, get_f_visibility(old_fflags), sizeof(buff)-1);
                    buff[sizeof(buff) - 1] = '\0'; // strncpy() does not guarantee NUL-termination
                    if (pragma_pedantic)
                        yyerrorf("Inconsistent declaration of '%s': Visibility changed from '%s' to '%s'"
                                , get_txt(p->name), buff, get_visibility(type));
                    else
                        yywarnf("Inconsistent declaration of '%s': Visibility changed from '%s' to '%s'"
                                , get_txt(p->name), buff, get_visibility(type));
                }
#               undef TYPE_MOD_VIS

                /* Check if the 'varargs' attribute is conserved. */
                if ((old_fflags ^ flags) & TYPE_MOD_VARARGS
                    &&  old_fflags & TYPE_MOD_VARARGS
                   )
#line 4136 "prolang.y"
                {
                    // this is a warning in case of re-defining inherited functions
                    // with pedantic, but always an error when prototype->definition
                    if (old_fflags & NAME_INHERITED)
#line 4140 "prolang.y"
                    {
                        if (pragma_check_overloads)
                            yywarnf("Redefinition of '%s' loses 'varargs' modifier"
                                    , get_txt(p->name));
                    }
                    else
                        yyerrorf("Inconsistent declaration of '%s': 'varargs' modifier lost"
                                , get_txt(p->name));
                }

                /* Check that the two argument lists are compatible */
                if (compare_args)
#line 4152 "prolang.y"
                {
                    int i;
                    unsigned short first_arg;
                    lpctype_t **argp;
                    int num_args = num_arg;

                    /* Don't check newly added arguments */
                    if (num_args > funp->num_arg)
                        num_args = funp->num_arg;

                    first_arg = ARGUMENT_INDEX(num);
                    argp = GET_BLOCK(A_ARGUMENT_TYPES) + first_arg;

                    if (old_fflags & TYPE_MOD_XVARARGS)
                        num_args--; /* last argument is ok */

                    for (i = 0; i < num_args; i++ )
#line 4169 "prolang.y"
                    {
                        new_type = type_of_locals[i].t_type;
                        old_type = argp[i];
                        if (new_type != old_type)
#line 4173 "prolang.y"
                        {
                            args_differ = MY_TRUE;
                            // If it is a redefinition of an inherited function, it might be OK,
                            // if the two arguments are at least compatible.
                            // But if it's a prototype->function redefinition, this is now a error,
                            // because prototype + definition should always be the same.
                            if (old_fflags & NAME_INHERITED)
#line 4180 "prolang.y"
                            {
                                if (!has_common_type(new_type, old_type))
#line 4182 "prolang.y"
                                {
                                    if (pragma_pedantic)
                                        yyerrorf("Argument type mismatch in redefinition of '%s': arg %d %s"
                                            , get_txt(p->name), i+1, get_two_lpctypes(new_type, old_type));
                                    else if (pragma_check_overloads)
                                        yywarnf("Argument type mismatch in redefinition of '%s': arg %d %s"
                                            , get_txt(p->name), i+1, get_two_lpctypes(new_type, old_type));
                                }
                            }
                            else
#line 4192 "prolang.y"
                            {
                                yyerrorf("Inconsistent declaration of '%s': argument type mismatch in definition: arg %d %s"
                                         , get_txt(p->name), i+1, get_two_lpctypes(new_type, old_type));
                            }

                        }
                    } /* for (all args) */

                } /* if (compare_args) */

            } /* if (exact_types && already defined) */

         } /* if (!complete) */

        /* Remember the heart_beat() function */
        if (mstreq(p->name, STR_HEART_BEAT))
            heart_beat = num;

        /* If it was yet another prototype,
         * just update its types and then return.
         */
        if (flags & NAME_PROTOTYPE)
#line 4214 "prolang.y"
        {
            if (funp->num_arg != num_arg || args_differ)
#line 4216 "prolang.y"
            {
                /* Arguments changed. The only reasonable way this can happen
                 * is if this function redefined an inherited one.
                 * For that case, we re-create the arguments, for all other cases
                 * (to be on the safe side), we turn off type
                 * checking as we have no way of deciding which definition is the
                 * correct one.
                 */
                if (funp->flags & NAME_INHERITED)
#line 4225 "prolang.y"
                {
                    funp->num_arg = num_arg;
                    ARGUMENT_INDEX(num) = store_argument_types(num_arg);
                }
                else
#line 4230 "prolang.y"
                {
                    if (exact_types && !(funp->flags & NAME_TYPES_LOST))
#line 4232 "prolang.y"
                    {
                        /* Warn about type checks being turned off. */
                        if (pragma_pedantic)
                            yyerrorf("Multiple inconsistent declarations "
                                     "of '%s' encountered: "
                                     "Deactivating argument type checks."
                                    , get_txt(p->name)
                                    );
                        else
                            yywarnf("Multiple inconsistent declarations "
                                     "of '%s' encountered: "
                                     "Deactivating argument type checks."
                                    , get_txt(p->name)
                                    );
                    }

                    funp->num_arg = num_arg;
                    ARGUMENT_INDEX(num) = INDEX_START_NONE;
                    funp->flags |= NAME_TYPES_LOST;
                }
            }

            free_lpctype(funp->type);
            // If the function has no type, it implicitly will be lpctype_mixed from
            // now on.
            if (type.t_type)
                funp->type = ref_lpctype(type.t_type);
            else
                funp->type  = lpctype_mixed; // static, no need to reference them.

            return num;
        }  // end of prototype update

        /* This is the completion of an earlier prototype:
         * now flesh out the function structure.
         */

        if (funp->flags & NAME_INHERITED) /* We didn't adopt the reference yet. */
            ref_mstring(funp->name);

        funp->num_locals = num_local;
        funp->flags = flags;
        funp->offset.pc = offset;

        /* That's it */
        return num;

    } while(0); /* Test and handle for already defined functions */

    /* It's a new function! */

    if (mstreq(p->name, STR_HEART_BEAT))
        heart_beat = FUNCTION_COUNT;

    /* Fill in the function_t */
    fun.name      = p->name;
    fun.offset.pc = offset;
    fun.flags     = flags;
    fun.num_arg   = num_arg;
    fun.num_locals= num_local; /* will be updated later */
    // If the function has no type, it implicitly will be lpctype_mixed from
    // now on. Background: fun.type being NULL is a nasty source of NULL pointer
    // dereferences because this is apparantly the instance a type can be NULL
    // and is sometimes forgotten.
    if (type.t_type)
        fun.type  = ref_lpctype(type.t_type);
    else
        fun.type  = lpctype_mixed; // static, no need to reference them.
    ref_mstring(fun.name);

    num = FUNCTION_COUNT;

    if (p->type != I_TYPE_GLOBAL)
#line 4305 "prolang.y"
    {
        /* This is the first _GLOBAL use of this identifier:
         * make an appropriate entry in the identifier table.
         */

        if (p->type != I_TYPE_UNKNOWN)
#line 4311 "prolang.y"
        {
            /* The ident has been used before otherwise, so
             * get a fresh structure.
             */
            p = make_shared_identifier_mstr(p->name, I_TYPE_GLOBAL, 0);
        }
        /* should be I_TYPE_UNKNOWN now. */

        init_global_identifier(p, /* bVariable: */ MY_TRUE);
        p->next_all = all_globals;
        all_globals = p;
    }
    else if (p->u.global.variable == I_GLOBAL_VARIABLE_FUN)
#line 4324 "prolang.y"
    {
        /* The previous _GLOBAL use is the permanent efun definition:
         * mark the efun as shadowed.
         */
        efun_shadow_t *q;

        q = xalloc(sizeof(efun_shadow_t));
        q->shadow = p;
        q->next = all_efun_shadows;
        all_efun_shadows = q;
    }
    /* else: Other cases don't need special treatment */

    p->u.global.function = num;

    /* Store the function_t in the functions area */
    ADD_FUNCTION(&fun);

    /* Store the function arguments, if required,
     * and save the position of the argument types.
     */
    argument_start_index = store_argument_types(num_arg);
    ADD_ARGUMENT_INDEX(argument_start_index);

    return num;
} /* define_new_function() */

/*-------------------------------------------------------------------------*/
static void
check_variable_redefinition (ident_t *name, typeflags_t flags)

/* Checks whether the redefinition of variable <name> with one having
 * <flags> is okay. Throws compile errors otherwise.
 */

#line 4359 "prolang.y"
{
    int n = name->u.global.variable;
    typeflags_t vn_flags = VARIABLE(n)->type.t_flags;

    /* Visible nomask variables can't be redefined */
    if ( vn_flags & TYPE_MOD_NO_MASK && !(flags & NAME_HIDDEN))
        yyerrorf( "Illegal to redefine 'nomask' variable '%s'"
                , get_txt(name->name));

    /* We can redefine inherited variables if they are private or hidden,
     * or if at least one of them is static.
     */
    if (  (   !(vn_flags & NAME_INHERITED)
           || (   !(vn_flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN))
               && !((flags | vn_flags) & TYPE_MOD_STATIC)
              )
          )
        && !(flags & NAME_INHERITED)
       )
#line 4378 "prolang.y"
    {
        if (vn_flags & NAME_INHERITED)
            yyerrorf("Illegal to redefine inherited variable '%s'"
                    , get_txt(name->name));
        else
            yyerrorf("Illegal to redefine global variable '%s'"
                    , get_txt(name->name));
    }

    if (((flags | vn_flags) & (TYPE_MOD_STATIC|TYPE_MOD_PRIVATE))
        == TYPE_MOD_STATIC
     && !(flags & NAME_INHERITED)
       )
#line 4391 "prolang.y"
    {
        yywarnf("Redefining inherited %s variable '%s' with a %s variable"
               , (vn_flags & TYPE_MOD_STATIC)
                 ? "nosave" : "non-nosave"
               , get_txt(name->name)
               , (flags & TYPE_MOD_STATIC) ? "nosave" : "non-nosave"
               );
    }
} /* check_variable_redefinition */

/*-------------------------------------------------------------------------*/
static void
define_variable (ident_t *name, fulltype_t type)

/* Define a new global variable <name> of type <type>.
 * The references of <type> are NOT adopted.
 */

#line 4409 "prolang.y"
{
    variable_t dummy;
    typeflags_t flags = type.t_flags;
    int n;

    if (type.t_type == lpctype_void)
#line 4415 "prolang.y"
    {
        yyerrorf( "Illegal to define variable '%s' as type 'void'"
                , get_txt(name->name));
    }

    if (name->type != I_TYPE_GLOBAL)
#line 4421 "prolang.y"
    {
        /* This is the first _GLOBAL use of this identifier:
         * make an appropriate entry in the identifier table.
         */

        if (name->type != I_TYPE_UNKNOWN)
#line 4427 "prolang.y"
        {
            /* The ident has been used before otherwise, so
             * get a fresh structure.
             */
            name = make_shared_identifier_mstr(name->name, I_TYPE_GLOBAL, 0);
        }

        init_global_identifier(name, /* bVariable: */ MY_TRUE);
        name->next_all = all_globals;
        all_globals = name;
    }
    else if (name->u.global.function == I_GLOBAL_FUNCTION_OTHER
          && (name->u.global.efun >= 0 || name->u.global.sim_efun >= 0
#ifdef USE_PYTHON
           || is_python_efun(name)
#endif
             )
            )
#line 4445 "prolang.y"
    {
        /* The previous _GLOBAL use is the permanent efun definition:
         * mark the efun as shadowed.
         */
        efun_shadow_t *q;

        q = xalloc(sizeof(efun_shadow_t));
        q->shadow = name;
        q->next = all_efun_shadows;
        all_efun_shadows = q;
    }

    /* Prepare the new variable_t */

    if (flags & TYPE_MOD_NOSAVE)
#line 4460 "prolang.y"
    {
        /* 'nosave' is internally saved as 'static' (historical reason) */
        flags |= TYPE_MOD_STATIC;
        flags ^= TYPE_MOD_NOSAVE;
    }

    /* If the variable already exists, make sure that we can redefine it */
    if ( (n = name->u.global.variable) >= 0)
#line 4468 "prolang.y"
    {
        typeflags_t vn_flags = VARIABLE(n)->type.t_flags;

        check_variable_redefinition(name, flags);

        /* Make sure that at least one of the two definitions is 'static'.
         * The variable which has not been inherited gets first pick.
         */
        if (flags & NAME_INHERITED)
#line 4477 "prolang.y"
        {
            flags |= ~(vn_flags) & TYPE_MOD_STATIC;
        }
        else
#line 4481 "prolang.y"
        {
            vn_flags |=   ~flags & TYPE_MOD_STATIC;
            VARIABLE(n)->type.t_flags = vn_flags;
        }
    }

    type.t_flags = flags;

    dummy.name = ref_mstring(name->name);
    dummy.type = ref_fulltype(type);

    if (flags & TYPE_MOD_VIRTUAL)
#line 4493 "prolang.y"
    {
        if (!(flags & NAME_HIDDEN))
            name->u.global.variable = VIRTUAL_VAR_TAG | V_VARIABLE_COUNT;
        ADD_VIRTUAL_VAR(&dummy);
    }
    else
#line 4499 "prolang.y"
    {
        if (!(flags & NAME_HIDDEN))
            name->u.global.variable = NV_VARIABLE_COUNT;
        ADD_VARIABLE(&dummy);
    }
} /* define_variable() */

/*-------------------------------------------------------------------------*/
static void
redeclare_variable (ident_t *name, fulltype_t type, int n)

/* The variable <name> is inherited virtually with number <n>.
 * Adjust its modifier accordingly. The pure type shouldn't
 * have changed. The references of <type> are NOT adopted.
 */

#line 4515 "prolang.y"
{
    typeflags_t flags = type.t_flags;
    typeflags_t varflags;
    variable_t *variable;

    if (name->type != I_TYPE_GLOBAL)
#line 4521 "prolang.y"
    {
        /* This is the first _GLOBAL use of this identifier:
         * make an appropriate entry in the identifier table.
         */

        if (name->type != I_TYPE_UNKNOWN)
#line 4527 "prolang.y"
        {
            /* The ident has been used before otherwise, so
             * get a fresh structure.
             */
            name = make_shared_identifier_mstr(name->name, I_TYPE_GLOBAL, 0);
        }

        init_global_identifier(name, /* bVariable: */ MY_TRUE);
        name->next_all = all_globals;
        all_globals = name;
    }
    else if (name->u.global.function == I_GLOBAL_FUNCTION_OTHER
          && (name->u.global.efun >= 0 || name->u.global.sim_efun >= 0
#ifdef USE_PYTHON
           || is_python_efun(name)
#endif
             )
            )
#line 4545 "prolang.y"
    {
        /* The previous _GLOBAL use is the permanent efun definition:
         * mark the efun as shadowed.
         */
        efun_shadow_t *q;

        q = xalloc(sizeof(efun_shadow_t));
        q->shadow = name;

        q->next = all_efun_shadows;
        all_efun_shadows = q;
    }
    /* else: the variable is inherited after it has been defined
     * in the child program.
     */

    /* The variable is hidden, do nothing else */
    if (flags & NAME_HIDDEN)
        return;

    if (flags & TYPE_MOD_NOSAVE)
#line 4566 "prolang.y"
    {
        /* 'nosave' is internally saved as 'static' (historical reason) */
        flags |= TYPE_MOD_STATIC;
        flags ^= TYPE_MOD_NOSAVE;
    }

    if (name->u.global.variable >= 0 && name->u.global.variable != n)
#line 4573 "prolang.y"
    {
        check_variable_redefinition(name, flags);
    }

    name->u.global.variable = n;

    variable = V_VARIABLE(n);
    varflags = variable->type.t_flags;

    assert(variable->name == name->name);
    assert(variable->type.t_type == type.t_type);

    /* The most visible modifier wins here. */
    if ((flags|varflags) & TYPE_MOD_PUBLIC)
#line 4587 "prolang.y"
    {
        varflags &= ~(TYPE_MOD_PRIVATE | TYPE_MOD_PROTECTED | NAME_HIDDEN);
        varflags |= TYPE_MOD_PUBLIC;
    }
    else if (!(flags&(TYPE_MOD_PRIVATE|TYPE_MOD_PROTECTED)))
#line 4592 "prolang.y"
    {
        varflags &= ~(TYPE_MOD_PRIVATE | TYPE_MOD_PROTECTED | NAME_HIDDEN);
    }
    else if (!(varflags&(TYPE_MOD_PRIVATE|TYPE_MOD_PROTECTED)))
#line 4596 "prolang.y"
    {
        /* It's already visible. */
    }
    else if (flags & TYPE_MOD_PROTECTED)
#line 4600 "prolang.y"
    {
        varflags &= ~(TYPE_MOD_PRIVATE | NAME_HIDDEN);
        varflags |= TYPE_MOD_PROTECTED;
    }

    /* Preserve nosave only, if both of them have it. */
    if (!(flags & varflags & TYPE_MOD_STATIC))
        varflags &= ~TYPE_MOD_STATIC;

    /* If either of them is nomask, the resulting var is, too. */
    varflags |= flags & TYPE_MOD_NO_MASK;

    variable->type.t_flags = varflags;
} /* redeclare_variable() */

/*-------------------------------------------------------------------------*/
static int
verify_declared (ident_t *p)

/* Check that <p> is a global variable.
 * If yes, return the index of that variable, -1 otherwise.
 */

#line 4623 "prolang.y"
{
    int r;

    if (p->type != I_TYPE_GLOBAL
     || (r = p->u.global.variable) < 0)
#line 4628 "prolang.y"
    {
        yyerrorf("Variable %s not declared !", get_txt(p->name));
        return -1;
    }

    return r;
} /* verify_declared() */

/*-------------------------------------------------------------------------*/
static int
define_global_variable (ident_t* name, fulltype_t actual_type, Bool with_init)

/* This is called directly from a parser rule: <type> <name>
 * if with_init is true, then an initialization of this variable will follow.
 * It creates the global variable and returns its index.
 */

#line 4645 "prolang.y"
{
    int i;

    variables_defined = MY_TRUE;

    actual_type.t_flags = check_visibility_flags(actual_type.t_flags, default_varmod, false);

    if (actual_type.t_flags & TYPE_MOD_VARARGS)
#line 4653 "prolang.y"
    {
        yyerror("can't declare a variable as varargs");
            actual_type.t_flags &= ~TYPE_MOD_VARARGS;
    }

    if (!pragma_share_variables)
        actual_type.t_flags |= VAR_INITIALIZED;

    define_variable(name, actual_type);
    i = verify_declared(name); /* Is the var declared? */

#ifdef DEBUG
    if (i == -1)
        fatal("Variable not declared after defining it.\n");
#endif

    /* Initialize float values with 0.0. */
    if (with_init || actual_type.t_type == lpctype_float)
#line 4671 "prolang.y"
    {

        /* Prepare the init code */
        transfer_init_control();

        /* If this is the first variable initialization and
         * pragma_share_variables is in effect, insert
         * the check for blueprint/clone initialisation:
         *    if (clonep(this_object())) return 1;
         */
        if (!variables_initialized && pragma_share_variables)
#line 4682 "prolang.y"
        {
            ins_f_code(F_THIS_OBJECT);
            ins_f_code(F_CLONEP);
            ins_f_code(F_BRANCH_WHEN_ZERO);
            ins_byte(2);
            ins_f_code(F_CONST1);
            ins_f_code(F_RETURN);
        }

        /* Initialize floats with 0.0 */
        if(!with_init)
#line 4693 "prolang.y"
        {
            PREPARE_INSERT(5)
            /* Must come after the non-local program code inserts! */

            add_f_code(F_FCONST0);

#ifdef DEBUG
            if (i & VIRTUAL_VAR_TAG)
#line 4701 "prolang.y"
            {
                /* When we want to allow 'late' initializers for
                 * inherited variables, it must have a distinct syntax,
                 * lest name clashs remain undetected, making LPC code
                 * hard to debug.
                 */
                fatal("Newly declared variable is virtual\n");
            }
#endif
            variables_initialized = MY_TRUE; /* We have __INIT code */
            if (!pragma_share_variables)
                VARIABLE(i)->type.t_flags |= VAR_INITIALIZED;

            /* Push the variable reference and create the assignment */

            if (i > 0xff)
#line 4717 "prolang.y"
            {
                add_f_code(F_PUSH_IDENTIFIER16_LVALUE);
                add_short(i);
                CURRENT_PROGRAM_SIZE += 1;
            }
            else
#line 4723 "prolang.y"
            {
                add_f_code(F_PUSH_IDENTIFIER_LVALUE);
                add_byte(i);
            }

            /* Ok, assign */
            add_f_code(F_VOID_ASSIGN);
            CURRENT_PROGRAM_SIZE += 4;
            add_new_init_jump();
        } /* PREPARE_INSERT() block */
    } /* if (float variable) */

    return i;
} /* define_global_variable() */

/*-------------------------------------------------------------------------*/
static void
init_global_variable (int i, ident_t* name, fulltype_t actual_type
                     , int assign_op, fulltype_t exprtype)

/* This is called directly from a parser rule: <type> <name> = <expr>
 * It will be called after the call to define_global_variable().
 * It assigns the result of <expr> to the variable.
 */

#line 4748 "prolang.y"
{
    PREPARE_INSERT(4)

    if (!(actual_type.t_flags & (TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC
                          | TYPE_MOD_PROTECTED)))
#line 4753 "prolang.y"
    {
        actual_type.t_flags |= default_varmod;
    }

#ifdef DEBUG
    if (i & VIRTUAL_VAR_TAG)
#line 4759 "prolang.y"
    {
        /* When we want to allow 'late' initializers for
         * inherited variables, it must have a distinct syntax,
         * lest name clashs remain undetected, making LPC code
         * hard to debug.
         */
        fatal("Newly declared variable is virtual\n");
    }
#endif
    variables_initialized = MY_TRUE; /* We have __INIT code */

    /* Push the variable reference and create the assignment */

    if (i > 0xff)
#line 4773 "prolang.y"
    {
        add_f_code(F_PUSH_IDENTIFIER16_LVALUE);
        add_short(i);
        CURRENT_PROGRAM_SIZE += 1;
    }
    else
#line 4779 "prolang.y"
    {
        add_f_code(F_PUSH_IDENTIFIER_LVALUE);
        add_byte(i);
    }

    /* Only simple assigns are allowed */
    if (assign_op != F_ASSIGN)
       yyerror("Illegal initialization");

    /* Do the types match? */
    actual_type.t_flags &= TYPE_MOD_MASK;
    if (!has_common_type(exprtype.t_type, actual_type.t_type))
#line 4791 "prolang.y"
    {
        yyerrorf("Type mismatch %s when initializing %s"
                , get_two_lpctypes(actual_type.t_type, exprtype.t_type)
                , get_txt(name->name));
    }

    /* Ok, assign */
    add_f_code(F_VOID_ASSIGN);
    CURRENT_PROGRAM_SIZE += 3;
    add_new_init_jump();
} /* init_global_variable() */

/*-------------------------------------------------------------------------*/
static void
get_function_information (function_t * fun_p, program_t * prog, int ix)

/* Read the function information for function <ix> in program <prog>
 * (which may be inherited) and store it in *<fun_p>. It is the callers
 * responsibility to set <fun_p>->flags _before_ calling this function.
 *
 * In particular, this function sets these <fun_p> fields: .name, .rtype
 * .num_args, and it modifies .flags.
 */

#line 4815 "prolang.y"
{
    const program_t *inhprogp;
    int inhfx;
    function_t * header = get_function_header_extended(prog, ix, &inhprogp, &inhfx);

    fun_p->name = header->name;
    fun_p->type = ref_lpctype(header->type);

    fun_p->num_arg = header->num_arg;
    if (is_undef_function(inhprogp->program + (inhprogp->functions[inhfx] & FUNSTART_MASK)))
        fun_p->flags |= NAME_UNDEFINED;
} /* get_function_information() */

/*-------------------------------------------------------------------------*/
static void
def_function_typecheck (fulltype_t returntype, ident_t * ident, Bool is_inline)

/* Called after parsing the '<type> <functionname>' part of a function
 * definition, this function performs the typecheck, makes sure that
 * the function name is put into the list of globals, and initialises
 * the block scoping.
 *
 * If <is_inline> is TRUE, the function to be compiled is an inline closure,
 * which requires a slightly different handling. This function is called
 * after 'func <type>' has been parsed, and is provided with a synthetic
 * function name.
 */

#line 4843 "prolang.y"
{
    if (is_inline)
#line 4845 "prolang.y"
    {
        new_inline_closure();
        enter_block_scope(); /* Scope for context */
        enter_block_scope(); /* Argument scope */
    }
    else
#line 4851 "prolang.y"
    {
        block_depth = 1;
        init_scope(block_depth);
    }

    returntype.t_flags = check_visibility_flags(returntype.t_flags, default_funmod, true);

    /* Require exact types? */
    exact_types = returntype.t_type;
    if (exact_types == NULL)
#line 4861 "prolang.y"
    {
        if (pragma_strict_types != PRAGMA_WEAK_TYPES)
            yyerrorf("\"#pragma %s_types\" requires type of function"
                    , pragma_strict_types == PRAGMA_STRICT_TYPES
                      ? "strict" : "strong" );
    }

    if (returntype.t_flags & TYPE_MOD_NOSAVE)
#line 4869 "prolang.y"
    {
        yyerror("can't declare a function as nosave");
        returntype.t_flags &= ~TYPE_MOD_NOSAVE;
    }

    if (ident->type == I_TYPE_UNKNOWN)
#line 4875 "prolang.y"
    {
        /* prevent freeing by exotic name clashes */
        init_global_identifier(ident, /* bVariable: */ MY_TRUE);
        ident->next_all = all_globals;
        all_globals = ident;
    }

    /* Store the data */
    if (is_inline)
#line 4884 "prolang.y"
    {
        current_inline->ident = ident;
        current_inline->returntype = returntype;
    }
    else
#line 4889 "prolang.y"
    {
        def_function_returntype = returntype;
        def_function_ident = ident;
    }
} /* def_function_typecheck() */

/*-------------------------------------------------------------------------*/
static void
def_function_prototype (int num_args, Bool is_inline)

/* Called after parsing '<type> <name> ( <args> ) of a function definition,
 * this function creates the function prototype entry.
 *
 * If <is_inline> is TRUE, the function to be compiled is an inline closure,
 * which requires a slightly different handling. This function is called
 * after 'func <type> <arguments> <context>' has been parsed.
 */

#line 4907 "prolang.y"
{
    ident_t * ident;
    fulltype_t * returntype;
    int fun;

    if (is_inline)
#line 4913 "prolang.y"
    {
        ident = current_inline->ident;
        returntype = &current_inline->returntype;
    }
    else
#line 4918 "prolang.y"
    {
        ident = def_function_ident;
        returntype = &def_function_returntype;
    }

    /* We got the complete prototype: define it */

    if ( current_number_of_locals
     && (type_of_locals[current_number_of_locals-1].t_flags
         & TYPE_MOD_VARARGS)
       )
#line 4929 "prolang.y"
    {
        /* The last argument has to allow an array. */
        fulltype_t *t;

        returntype->t_flags |= TYPE_MOD_XVARARGS;

        t = type_of_locals + (current_number_of_locals-1);
        if (!lpctype_contains(lpctype_unknown_array, t->t_type))
#line 4937 "prolang.y"
        {
            yyerror("varargs parameter must be declared array or mixed");

            /* Keep the visibility, but change the type to 'mixed'.
             */
            free_lpctype(t->t_type);
            t->t_type = lpctype_mixed;
        }
    }

    /* Define a prototype. If it is a real function, then the
     * prototype will be updated below.
     */
    fun = define_new_function( MY_FALSE, ident, num_args, 0, 0
                             , NAME_UNDEFINED|NAME_PROTOTYPE
                             , *returntype);

    /* Store the data */
    if (is_inline)
#line 4956 "prolang.y"
    {
        current_inline->num_args = num_args;
        current_inline->function = fun;
    }
    else
#line 4961 "prolang.y"
    {
        def_function_num_args = num_args;
    }
} /* def_function_prototype() */

/*-------------------------------------------------------------------------*/
static void
def_function_complete ( p_int body_start, Bool is_inline)

/* Called after completely parsing a function definition,
 * this function updates the function header and closes all scopes..
 * Argument is the program index where the space for the function header
 * was made, or -1 if there was no body.
 *
 * If <is_inline> is TRUE, the function to be compiled is an inline closure,
 * which requires a slightly different handling. This function is called
 * after the complete closure has been parsed.
 */

#line 4980 "prolang.y"
{
    ident_t    * ident;
    fulltype_t   returntype;
    int          num_args;

    if (is_inline)
#line 4986 "prolang.y"
    {
        ident = current_inline->ident;
        returntype = current_inline->returntype;
        num_args = current_inline->num_args;
    }
    else
#line 4992 "prolang.y"
    {
        ident = def_function_ident;
        returntype = def_function_returntype;
        num_args = def_function_num_args;
    }

    if (body_start < 0)
#line 4999 "prolang.y"
    {
        /* function_body was a ';' -> prototype
         * Just norm the visibility flags unless it is a prototype
         * for an already inherited function.
         */

        funflag_t *flagp;

        flagp = (funflag_t *)(&FUNCTION(ident->u.global.function)->flags);
        if (!(*flagp & NAME_INHERITED))
#line 5009 "prolang.y"
        {
            *flagp |= returntype.t_flags
                      & (*flagp & TYPE_MOD_PUBLIC
                        ? (TYPE_MOD_NO_MASK)
                        : (TYPE_MOD_NO_MASK|TYPE_MOD_PRIVATE
                          |TYPE_MOD_STATIC|TYPE_MOD_PROTECTED
                          |TYPE_MOD_PUBLIC)
                        );
        }
    }
    else
#line 5020 "prolang.y"
    {
        /* function_body was a block: generate the
         * function header and update the ident-table entry.
         */
        int num_vars = max_number_of_locals - num_args
                                            + max_break_stack_need;

        define_new_function(MY_TRUE, ident
                            , num_args
                            , num_vars
                            , body_start + FUNCTION_PRE_HDR_SIZE
                            , 0, returntype);

        /* Catch a missing return if the function has a return type */
        if (returntype.t_type != lpctype_void
         && (   returntype.t_type != lpctype_unknown
             || pragma_strict_types
            )
           )
#line 5039 "prolang.y"
        {
            /* Check if the previous instruction is a RETURN, or
             * at least a non-continuing instruction.
             */
            bytecode_t last = F_ILLEGAL;

            if (CURRENT_PROGRAM_SIZE > body_start + FUNCTION_HDR_SIZE)
                last = PROGRAM_BLOCK[CURRENT_PROGRAM_SIZE-1];

            if (F_RETURN == last || F_RETURN0 == last
             || F_RAISE_ERROR == last || F_THROW == last
               )
#line 5051 "prolang.y"
            {
                /* Good, the last instruction seems to be a 'return'.
                 * But just in case we're looking at the data field
                 * of a different opcode or a conditional return: insert a
                 * proper default return as well.
                 */
                if (pragma_warn_missing_return)
                    ins_f_code(F_DEFAULT_RETURN);
                else
                    ins_f_code(F_RETURN0);
            }
            else
#line 5063 "prolang.y"
            {
                /* There is no 'return' here: most likely it is missing
                 * altogether.
                 * If warn_missing_return is enabled, issue the warning,
                 * but always insert a normal F_RETURN0: with the pragma
                 * active it's no use to warn again at runtime, and without
                 * the pragma no warning is desired anyway.
                 */
                if (pragma_warn_missing_return)
                    yywarnf("Missing 'return <value>' statement");

                ins_f_code(F_RETURN0);
            }
        }
        else
#line 5078 "prolang.y"
        {
            ins_f_code(F_RETURN0);
        }
    }

    /* Clean up for normal functions.
     * Do not free the function returntype - it is still held in A_FUNCTIONS
     * and freed after the compile.
     * Inline closures need some of the information for some more processing.
     */
    if (is_inline)
#line 5089 "prolang.y"
    {
        /* Keep block_depth, and local names */
    }
    else
#line 5093 "prolang.y"
    {
        free_all_local_names();
        block_depth = 0;
    }

} /* def_function_complete() */

/* =============================   STRUCTS   ============================= */

/*-------------------------------------------------------------------------*/
static int
define_new_struct ( Bool proto, ident_t *p, const char * prog_name, funflag_t flags)

/* Define a new struct <p> with the visibility <flags>.
 * If <proto> is TRUE, the function is called for a struct forward
 * declaration; if <proto> is FALSE, the struct is about to be defined.
 *
 * Result is the index (id) of the struct in the struct_defs table.
 * If the struct would be a duplicate, -1 is returned instead of the index.
 *
 * If a prototype is encountered, the struct definition is stored
 * with an additional visibility flag of NAME_PROTOTYPE.
 *
 * If NAME_HIDDEN is set in flags, the struct is added to the program
 * but no visibility checks occur - this is for inherited structs
 * which are no longer visible, but have to be kept in order to
 * keep the struct ids intact.
 */

#line 5122 "prolang.y"
{
    int          num;
    struct_def_t sdef;

    /* If this is a redeclaration, check for consistency. */
    if (p->type == I_TYPE_GLOBAL && (num = p->u.global.struct_id) >= 0
     && !(flags & NAME_HIDDEN)
      )
#line 5130 "prolang.y"
    {
        struct_def_t *pdef;

        pdef = &STRUCT_DEF(num);

        /* Check if the visibility is conserved.
         */
#line 5137 "prolang.y"
        {
#            define TYPE_MOD_VIS \
            ( TYPE_MOD_NO_MASK \
            | TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC \
            | TYPE_MOD_PROTECTED)
            funflag_t f1 = pdef->flags;
            funflag_t f2 = flags;

            /* Smooth out irrelevant differences */
            if (f1 & TYPE_MOD_STATIC) f1 |= TYPE_MOD_PROTECTED;
            if (f2 & TYPE_MOD_STATIC) f2 |= TYPE_MOD_PROTECTED;

            if ( ((f1 ^ f2) & TYPE_MOD_VIS) )
#line 5150 "prolang.y"
            {
                char buff[120];

                strncpy(buff, get_f_visibility(pdef->flags), sizeof(buff)-1);
                buff[sizeof(buff)-1] = '\0'; // strncpy() does not guarantee NUL termination
                yywarnf("Inconsistent declaration of struct %s: "
                        "Visibility changed from '%s' to '%s'"
                       , get_txt(p->name), buff, get_f_visibility(flags));
            }
#           undef TYPE_MOD_VIS
        }

        /* If this is just another prototype, return */
        if (proto)
            return num;

        /* If this is a redefinition of a completed struct, complain
         * and return.
         */
        if (!proto && !(pdef->flags & NAME_PROTOTYPE))
#line 5170 "prolang.y"
        {
            yyerrorf("Duplicate definition of struct %s"
                    , get_txt(p->name));
            return -1;
        }

        /* At this point, we have in our hands the definition of a
         * previously just declared struct.
         * Update the stored information and return its index.
         */
        pdef->flags = flags & ~NAME_PROTOTYPE;

        return num;
    }

    /* This is a new struct! */
    flags = check_visibility_flags(flags, 0, false);
    if (flags & TYPE_MOD_STATIC)
#line 5188 "prolang.y"
    {
        yyerror("Can't declare a struct as static");
        flags &= ~TYPE_MOD_STATIC;
    }
    if (flags & TYPE_MOD_VARARGS)
#line 5193 "prolang.y"
    {
        yyerror("Can't declare a struct as varargs");
        flags &= ~TYPE_MOD_VARARGS;
    }

    /* Fill in the struct_def_t */
    sdef.type  = struct_new_prototype(ref_mstring(p->name)
                                    , new_tabled(prog_name));
    sdef.flags = proto ? (flags | NAME_PROTOTYPE)
                       : (flags & ~NAME_PROTOTYPE);
    sdef.inh = -1;

    update_struct_type(sdef.type->name->lpctype, sdef.type);

    num = STRUCT_COUNT;

    if (p->type != I_TYPE_GLOBAL)
#line 5210 "prolang.y"
    {
        /* This is the first _GLOBAL use of this identifier:
         * make an appropriate entry in the identifier table.
         */

        if (p->type != I_TYPE_UNKNOWN)
#line 5216 "prolang.y"
        {
            /* The ident has been used before otherwise, so
             * get a fresh structure.
             */
            p = make_shared_identifier_mstr(p->name, I_TYPE_GLOBAL, 0);
        }
        /* should be I_TYPE_UNKNOWN now. */

        init_global_identifier(p, /* bVariable: */ MY_FALSE);
        p->next_all = all_globals;
        all_globals = p;
    }

    if  (!(flags & NAME_HIDDEN))
        p->u.global.struct_id = num;

    /* Store the function_t in the functions area */
    ADD_STRUCT_DEF(&sdef);

    return num;
} /* define_new_struct() */

/*-------------------------------------------------------------------------*/
static int
find_struct ( string_t * name )

/* Find the struct <name> and return its index. Return -1 if not found.
 */

#line 5245 "prolang.y"
{
    ident_t * p;

    p = find_shared_identifier_mstr(name, I_TYPE_GLOBAL, 0);

    /* Find the global struct identifier */
    while (p != NULL && p->type != I_TYPE_GLOBAL)
        p = p->inferior;

    if (p == NULL || p->u.global.struct_id < 0)
        return -1;
    if (STRUCT_DEF(p->u.global.struct_id).flags & NAME_HIDDEN)
        return -1;
    return p->u.global.struct_id;
} /* find_struct() */

/*-------------------------------------------------------------------------*/
static void
add_struct_member ( string_t *name, lpctype_t *type
                  , struct_type_t * from_struct )

/* Add a new member <name> with type <type> to A_STRUCT_MEMBERS for the
 * to the most recently defined struct <current_struct>.
 * If <from_struct> is not NULL, it is the type of the struct from
 * which the member is inherited.
 * Raise an error if a member of the same name already exists.
 * The references of <type> are NOT adopted.
 */

#line 5274 "prolang.y"
{
    struct_def_t *pdef;

    pdef = &STRUCT_DEF(current_struct);

    if (STRUCT_MEMBER_COUNT != 0)
#line 5280 "prolang.y"
    {
        /* Not the first member: check if the name already occurred */
        int i;

        for ( i = STRUCT_MEMBER_COUNT-1 ; i >= 0 ; i--)
#line 5285 "prolang.y"
        {
            if (mstreq(name, STRUCT_MEMBER(i).name))
#line 5287 "prolang.y"
            {
                if (pdef->type->base
                 && pdef->type->base->num_members > i
                   )
                    yyerrorf("Duplicate member '%s' in struct '%s', "
                             "inherited from struct '%s'"
                            , get_txt(name)
                            , get_txt(struct_t_name(pdef->type))
                            , get_txt(struct_t_name(pdef->type->base))
                            );
                else
                    yyerrorf("Duplicate member '%s' in struct '%s'"
                            , get_txt(name)
                            , get_txt(struct_t_name(pdef->type))
                            );
                return;
            }
        }
    }

    /* Member is ok: add it */
    if (STRUCT_MEMBER_COUNT == STRUCT_MAX_MEMBERS)
#line 5309 "prolang.y"
    {
        yyerrorf("Too many members for struct '%s'"
                , get_txt(struct_t_name(pdef->type)));
    }
    else
#line 5314 "prolang.y"
    {
        struct_member_t member;

        member.name = ref_mstring(name);
        member.type = ref_lpctype(type);
        ADD_STRUCT_MEMBER(&member);
    }
} /* add_struct_member() */

/*-------------------------------------------------------------------------*/
static void
finish_struct ( int32 prog_id)

/* The definition for struct <current_struct> has been parsed completely,
 * now complete the struct type object with the A_STRUCT_MEMBERS data.
 */

#line 5331 "prolang.y"
{
    struct_def_t *pdef;
    struct_type_t *base;
    string_t *name, *prog_name;

    pdef = &STRUCT_DEF(current_struct);

    /* Retrieve the .base pointer so that the error handling won't
     * get confused about it.
     * Also get a safety copy of the name.
     */
    base = pdef->type->base;
    pdef->type->base = NULL;
    name = ref_mstring(struct_t_name(pdef->type));
    prog_name = ref_mstring(struct_t_pname(pdef->type));

    /* Fill in the prototype */
    pdef->type = struct_fill_prototype(pdef->type
                                      , prog_id
                                      , base
                                      , STRUCT_MEMBER_COUNT
                                      , &STRUCT_MEMBER(0)
                                      );

    if (pdef->type)
#line 5356 "prolang.y"
    {
        /* Success: Free the safety copies */
        free_mstring(name);
        free_mstring(prog_name);
    }
    else
#line 5362 "prolang.y"
    {
        /* Failure: Recreate the prototype as the old one got deleted */
        pdef->type = struct_new_prototype(name, prog_name);
    }

    /* Clear the STRUCT_MEMBER block - the definitions have already
     * been adopted or cleared by the struct_fill_prototype().
     */
    mem_block[A_STRUCT_MEMBERS].current_size = 0;
} /* finish_struct() */

/*-------------------------------------------------------------------------*/
static Bool
create_struct_literal ( struct_def_t * pdef, int length, struct_init_t * list)

/* The compiler has created code for <length> expressions in order
 * to create a struct literal of struct <pdef>.
 * Analyze the <list> of member descriptions and generate the appropriate
 * bytecode.
 *
 * Return TRUE on success, and FALSE if an error occurred (the caller will
 * then clean up the bytecode).
 */

#line 5386 "prolang.y"
{
    struct_init_t * p;
    struct_member_t * pmember;
    void * block;    /* Allocation block for flags and index */
    Bool * flags;    /* Flag: which struct members have been set */
    int  * ix;       /* For each expr in order, list the struct member index */
    int    consumed; /* To check if we used all elements */
    int    count, member;
    int    i;
    Bool got_error = MY_FALSE;

    /* Check if there is one member assigned by name */
    for (p = list; p != NULL; p = p->next)
        if (p->name != NULL)
            break;

    if (length == 0 || p == NULL)
#line 5403 "prolang.y"
    {
        /* Simplest case: all members assigned by position. */

        /* Check the types */
        if (exact_types && length > 0)
#line 5408 "prolang.y"
        {
            for (member = 0, pmember = pdef->type->member, p = list
                ; member < length && member < struct_t_size(pdef->type)
                ; member++, pmember++, p = p->next
                )
#line 5413 "prolang.y"
            {
                if (!has_common_type(pmember->type, p->type.t_type) )
#line 5415 "prolang.y"
                {
                    yyerrorf("Type mismatch %s for member '%s' "
                             "in struct '%s'"
                            , get_two_lpctypes(pmember->type, p->type.t_type)
                            , get_txt(pmember->name)
                            , get_txt(struct_t_name(pdef->type))
                            );
                    got_error = MY_TRUE;
                }
            }

            if (got_error)
                return MY_FALSE;
        }

        /* The types check out - create the bytecode */
        ins_f_code(F_S_AGGREGATE);
        ins_short(pdef - &STRUCT_DEF(0));
        ins_byte(length);

        return MY_TRUE;
    }

    /* We have named members in there - sort them out */

    consumed = 0;

    block = xalloc( struct_t_size(pdef->type) * sizeof(*flags)
                  + length * sizeof(*ix));
    flags = (Bool *)block;
    ix = (int *)((char *)block + struct_t_size(pdef->type) * sizeof(*flags));

    for (i = 0; i < struct_t_size(pdef->type); i++)
#line 5448 "prolang.y"
    {
        flags[i] = MY_FALSE;
    }

    for (i = 0; i < length; i++)
#line 5453 "prolang.y"
    {
        ix[i] = -1;
    }

    /* Loop through list: assign the named members.
     */
    for (p = list, count = 0; p != NULL; p = p->next, count++)
#line 5460 "prolang.y"
    {

        if (p->name == NULL)
#line 5463 "prolang.y"
        {
            if (!got_error)
#line 5465 "prolang.y"
            {
                yyerrorf( "Can't mix named and unnamed initializers "
                          "in struct '%s'"
                        , get_txt(struct_t_name(pdef->type))
                        );
                got_error = MY_TRUE;
            }
            continue;
        }

        consumed++;
        pmember = NULL; /* avoids a warning */
        member = struct_find_member(pdef->type, p->name);
        if (member >= 0)
            pmember = &pdef->type->member[member];

        if (member < 0)
#line 5482 "prolang.y"
        {
            yyerrorf( "No such member '%s' in struct '%s'"
                    , get_txt(p->name)
                    , get_txt(struct_t_name(pdef->type))
                    );
            got_error = MY_TRUE;
        }
        else if (flags[member])
#line 5490 "prolang.y"
        {
            yyerrorf( "Multiple initializations of member '%s' "
                      "in struct '%s'"
                    , get_txt(p->name)
                    , get_txt(struct_t_name(pdef->type))
                    );
            got_error = MY_TRUE;
        }
        else if (exact_types
              && !has_common_type( pmember->type , p->type.t_type) )
#line 5500 "prolang.y"
        {
            yyerrorf("Type mismatch %s when initializing member '%s' "
                     "in struct '%s'"
                    , get_two_lpctypes(pmember->type, p->type.t_type)
                    , get_txt(p->name)
                    , get_txt(struct_t_name(pdef->type))
                    );
            got_error = MY_TRUE;
        }
        else
#line 5510 "prolang.y"
        {
            flags[member] = MY_TRUE;
            ix[count] = member;
        }
    } /* for() */

    if (got_error)
#line 5517 "prolang.y"
    {
        xfree(block);
        return MY_FALSE;
    }

    /* Sanity checks */

    if (consumed < length)
#line 5525 "prolang.y"
    {
        yyerrorf("Too many elements for struct '%s'"
                , get_txt(struct_t_name(pdef->type))
                );
        xfree(block);
        return MY_FALSE;
    }

    for (i = 0; i < length; i++)
#line 5534 "prolang.y"
    {
        if (ix[i] < 0)
#line 5536 "prolang.y"
        {
            fatal("struct literal: expression %d not assigned to any member.\n"
                 , i);
            /* NOTREACHED */
        }
    }

    /* Finally, create the code */
    ins_f_code(F_S_M_AGGREGATE);
    ins_short(pdef - &STRUCT_DEF(0));
    ins_byte(length);
    for (i = length-1; i >= 0; i--)
        ins_byte(ix[i]);

    /* Done */
    xfree(block);

    return MY_TRUE;
} /* create_struct_literal() */

/*-------------------------------------------------------------------------*/
static short
get_struct_index (struct_name_t * pName)

/* Return the index of struct name <pName> in this program's A_STRUCT_DEFS.
 * Return -1 if not found.
 */

#line 5564 "prolang.y"
{
    short i;

    for (i = 0; (size_t)i < STRUCT_COUNT; i++)
#line 5568 "prolang.y"
    {
        if (STRUCT_DEF(i).type->name == pName)
            return i;
    }
    return -1;
} /* get_struct_index() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
get_struct_member_result_type (lpctype_t* structure, string_t* member_name, short* struct_index, int* member_index)

/* Determines the result type of a struct member lookup operation
 * <structure> -> <member_name>. It checks whether <structure> is a valid
 * struct type for access to <member_name>.
 *
 * <member_name> is NULL for runtime lookups, otherwise it contains the
 * name of the struct member to lookup at compile time.
 *
 * If <member_name> is non-NULL, then the corresponding struct index will
 * be written into <struct_index> and the member index into <member_index>.
 * If the name is ambiguous then the compiler must downgrade to a runtime
 * lookup and this function will return -1 (FSM_AMBIGUOUS) in <struct_index>
 * and -1 in <member_index>. If the name is not found -2 (FSM_NO_STRUCT)
 * will be returned in <struct_index> and -1 in <member_index>.
 * If <member_name> is NULL, FSM_AMBIGUOUS will be returned in <struct_index>
 * and -1 in <member_index>.
 *
 * Returns the member type (or a union of all possible member types)
 * or NULL upon a compile error.
 */

#define FSM_AMBIGUOUS (-1)
#define FSM_NO_STRUCT (-2)

#line 5602 "prolang.y"
{
    lpctype_t* head = structure;    /* Used to walk over the type.                   */
    lpctype_t* result = NULL;       /* Result type.                                  */
    struct_type_t* fstruct = NULL;  /* Found structure definition.                   */
    bool is_struct = false;         /* There was at least one struct in <structure>. */

    *struct_index = FSM_NO_STRUCT;
    *member_index = -1;

    while (true)
#line 5612 "prolang.y"
    {
        /* Let's go through <structure> and see, if there are any structs in it. */
        lpctype_t *unionmember = head->t_class == TCLASS_UNION ? head->t_union.member : head;

        switch (unionmember->t_class)
#line 5617 "prolang.y"
        {
        case TCLASS_PRIMARY:
            switch (unionmember->t_primary)
#line 5620 "prolang.y"
            {
            case TYPE_UNKNOWN:
            case TYPE_ANY:
                /* Can be anything, even a struct... */
                is_struct = true;

                if (member_name != NULL)
#line 5627 "prolang.y"
                {
                    /* Let's look for all known structs if the have
                     * a corresponding member.
                     */

                    for (size_t i = 0; i < STRUCT_COUNT; i++)
#line 5633 "prolang.y"
                    {
                        struct_def_t * pdef = &STRUCT_DEF(i);

                        /* We just look at the direct members, so we automatically
                         * get the smallest struct that defines the member.
                         */
                        int idx = struct_find_direct_member(pdef->type, member_name);
                        if (idx < 0)
                            continue;

                        switch (*struct_index)
#line 5644 "prolang.y"
                        {
                            case FSM_NO_STRUCT:
                                fstruct = pdef->type;
                                *struct_index = i;
                                *member_index = idx;
                                break;

                            case FSM_AMBIGUOUS:
                                break;

                            default:
                                /* We did already found a struct.
                                 * This other struct is different from the current one
                                 * (because 'mixed' can't be in a union with other types
                                 * and all we don't have a struct twice in STRUCT_DEF),
                                 * so we have ambiguity here.
                                 */
                                *struct_index = FSM_AMBIGUOUS;
                                *member_index = -1;
                                fstruct = NULL;
                                break;
                        }

                        lpctype_t *oldresult = result;
                        result = get_union_type(result, pdef->type->member[idx].type);
                        free_lpctype(oldresult);
                    } /* for (all structs) */
                }
                else
#line 5673 "prolang.y"
                {
                    /* Struct not known, member name not known, can be anything... */
                    *struct_index = FSM_AMBIGUOUS;
                    *member_index = -1;
                    free_lpctype(result);
                    return lpctype_mixed;
                }
                break;

            default:
                /* All other cases we ignore, they cannot be indexed. */
                break;
            }
            break;

        case TCLASS_STRUCT:
#line 5689 "prolang.y"
            {
                struct_type_t *pdef = unionmember->t_struct.def;
                if (pdef == NULL)
                    break;

                int midx = -1;

                is_struct = true;

                if (member_name != NULL)
#line 5699 "prolang.y"
                {
                    /* Let's see, if this one has <member>. */
                    midx = struct_find_member(pdef, member_name);
                    if (midx < 0)
                        break;
                }

                switch (*struct_index)
#line 5707 "prolang.y"
                {
                    case FSM_NO_STRUCT:
                        fstruct = pdef;
                        *struct_index = get_struct_index(fstruct->name);
                        *member_index = midx;
                        if (*struct_index == -1)
#line 5713 "prolang.y"
                        {
                            yyerrorf("Unknown type in struct dereference: struct %s\n"
                                    , get_txt(fstruct->name->name));
                            *struct_index = FSM_AMBIGUOUS;
                        }
                        break;

                    case FSM_AMBIGUOUS:
                        break;

                    default:
                        /* Check whether this struct and <fstruct> are related. */
                        if (struct_baseof(fstruct, pdef))
                            break;
                        if (struct_baseof(pdef, fstruct))
#line 5728 "prolang.y"
                        {
                            fstruct = pdef;
                            *struct_index = get_struct_index(fstruct->name);
                            *member_index = midx;
                            break;
                        }

                        /* Not related, fall back to runtime lookup. */
                        *struct_index = FSM_AMBIGUOUS;
                        *member_index = -1;
                        fstruct = NULL;
                        break;
                }

                if (midx < 0) /* This also means that member_name == NULL */
#line 5743 "prolang.y"
                {
                    /* This is a runtime lookup. We can't guess the type,
                     * because at runtime we might have a derived structs
                     * with additional members.
                     */
                    free_lpctype(result);

                    /* It doesn't get any better, so return... */
                    return lpctype_mixed;
                }
                else
#line 5754 "prolang.y"
                {
                    lpctype_t *oldresult = result;
                    result = get_union_type(result, pdef->member[midx].type);
                    free_lpctype(oldresult);
                }
                break;
            }
        }

        if (head->t_class == TCLASS_UNION)
            head = head->t_union.head;
        else
            break;
    }

    if (*struct_index == FSM_NO_STRUCT)
#line 5770 "prolang.y"
    {
        if (is_struct)
            yyerrorf("No such member '%s' for struct '%s'"
                    , get_txt(member_name)
                    , get_lpctype_name(structure)
                    );
        else
            yyerrorf("Bad type for struct lookup: %s"
                    , get_lpctype_name(structure));

        return NULL;
    }

    return result;
} /* get_struct_member_result_type() */

/*-------------------------------------------------------------------------*/
static void
struct_epilog (void)

/* After a successful parse, make sure that all structs are defined,
 * try to reactivate existing structs, and publish the new ones.
 *
 * If an error occures, num_parse_error will be incremented.
 */

#line 5796 "prolang.y"
{
    int i;

    /* Check that all structs are defined.
     */
    for (i = 0; (size_t)i < STRUCT_COUNT; i++)
#line 5802 "prolang.y"
    {
        if (STRUCT_DEF(i).flags & NAME_PROTOTYPE)
#line 5804 "prolang.y"
        {
            yyerrorf("struct '%s' defined just as prototype"
                    , get_txt(struct_t_name(STRUCT_DEF(i).type))
                    );
            return;
        }
    }

    /* For all structs defined in this program, check if they just
     * replicate an existing older struct.
     */
    for (i = 0; (size_t)i < STRUCT_COUNT; i++)
#line 5816 "prolang.y"
    {
        struct_type_t *pSType = STRUCT_DEF(i).type;
        struct_type_t *pOld;

        if (STRUCT_DEF(i).inh >= 0)
            continue;

        pOld = struct_lookup_type(pSType);
        if (!pOld || !struct_type_equivalent(pSType, pOld))
            continue;

        /* pOld has the same structure as pSType, so lets
         * replace the latter with the former.
         */

        free_struct_type(pSType);
        STRUCT_DEF(i).type = ref_struct_type(pOld);
    } /* for(i) */

    /* Publish all struct types defined in this program.
     * It is safe to publish types twice.
     */
    for (i = 0; (size_t)i < STRUCT_COUNT; i++)
#line 5839 "prolang.y"
    {
        if (STRUCT_DEF(i).inh < 0)
            struct_publish_type(STRUCT_DEF(i).type);
    } /* for(i) */

} /* struct_epilog() */


/* =========================   Inline Closures   =-======================= */

/*-------------------------------------------------------------------------*/
static void
new_inline_closure (void)

/* Create a new inline closure structure and push it on top of the stack.
 */

#line 5856 "prolang.y"
{
    inline_closure_t ict;

    if (current_inline == NULL)
#line 5860 "prolang.y"
    {
        ict.prev = -1;
    }
    else
#line 5864 "prolang.y"
    {
        ict.prev = current_inline - &(INLINE_CLOSURE(0));
    }
#ifdef DEBUG_INLINES
printf("DEBUG: new inline #%"PRIuMPINT": prev %"PRIdMPINT"\n", INLINE_CLOSURE_COUNT, ict.prev);
#endif /* DEBUG_INLINES */

    /* Initialize the other fields */
    ict.end = CURRENT_PROGRAM_SIZE;
    ict.start = CURRENT_PROGRAM_SIZE;
    ict.length = 0;
    ict.li_start = LINENUMBER_SIZE;
    ict.li_length = 0;
    ict.function = -1;
    ict.ident = NULL;
    ict.returntype.t_flags = 0;
    ict.returntype.t_type = NULL;
    ict.num_args = 0;
    ict.parse_context = MY_FALSE;
    ict.start_line = stored_lines;
    ict.end_line = stored_lines;

#ifdef DEBUG_INLINES
printf("DEBUG:   start: %"PRIuMPINT", depth %d, locals: %d/%d, break: %d/%d\n", 
       CURRENT_PROGRAM_SIZE, block_depth, current_number_of_locals, 
       max_number_of_locals, current_break_stack_need, max_break_stack_need);
#endif /* DEBUG_INLINES */
    ict.block_depth          = block_depth;
    ict.break_stack_size     = current_break_stack_need;
    ict.max_break_stack_size = max_break_stack_need;
    ict.num_locals           = current_number_of_locals;
    ict.max_num_locals       = max_number_of_locals;
    ict.exact_types          = exact_types;
    ict.include_handle       = get_include_handle();
    ict.full_local_type_start   = type_of_locals - &(LOCAL_TYPE(0));
    ict.full_context_type_start = type_of_context - &(LOCAL_TYPE(0));
    ict.full_local_type_size    = mem_block[A_LOCAL_TYPES].current_size;
#ifdef DEBUG_INLINES
printf("DEBUG:   local types: %"PRIuMPINT", context types: %"PRIuMPINT"\n", 
       ict.full_local_type_start, ict.full_context_type_start);
#endif /* DEBUG_INLINES */

    /* Extend the type memblocks */
#line 5907 "prolang.y"
    {
        mp_uint type_count = LOCAL_TYPE_COUNT;

        extend_mem_block(A_LOCAL_TYPES, 2 * MAX_LOCAL * sizeof(A_LOCAL_TYPES_t));
        memset(&LOCAL_TYPE(type_count), 0
              , (LOCAL_TYPE_COUNT - type_count) * sizeof(A_LOCAL_TYPES_t));

        type_of_context = &(LOCAL_TYPE(type_count));
        type_of_locals = &(LOCAL_TYPE(type_count+MAX_LOCAL));
#ifdef DEBUG_INLINES
printf("DEBUG:   type ptrs: %p, %p\n", 
       type_of_locals, type_of_context );
#endif /* DEBUG_INLINES */
    }

    max_break_stack_need = current_break_stack_need = 0;
    max_number_of_locals = current_number_of_locals = 0;

    /* Add the structure to the memblock */
    ADD_INLINE_CLOSURE(&ict);
    current_inline = &(INLINE_CLOSURE(INLINE_CLOSURE_COUNT-1));
} /* new_inline_closure() */

/*-------------------------------------------------------------------------*/
static void
finish_inline_closure (Bool bAbort)

/* The compilation of the current inline closure is finished - move
 * everything out of the way of the ongoing compilation.
 * Note that only the codeblock .start/.length is saved; if there is
 * already code generated afterwards, it is moved forward. Ditto for
 * the linenumbers.
 *
 * If <bAbort> is TRUE, the closure is just finished, but not stored.
 */

#line 5943 "prolang.y"
{
    mp_uint backup_start, start, length, end;
    int offset;
    
#ifdef DEBUG_INLINES
#line 5948 "prolang.y"
{
    mp_int index = current_inline - &(INLINE_CLOSURE(0));
printf("DEBUG: %s inline #%"PRIdMPINT": prev %"PRIdMPINT", end %"PRIuMPINT
       ", start %"PRIuMPINT", length %"PRIuMPINT", function %d pc %"PRIu32"\n", 
       bAbort ? "abort" : "finish", index, current_inline->prev, 
       current_inline->end, current_inline->start, current_inline->length, 
       current_inline->function, FUNCTION(current_inline->function)->offset.pc);
printf("DEBUG:   depth %d, locals: %d/%d, break: %d/%d\n", 
       current_inline->block_depth, current_inline->num_locals, 
       current_inline->max_num_locals, current_inline->break_stack_size, 
       current_inline->max_break_stack_size);
}
#endif /* DEBUG_INLINES */

    /* Move the program code into the backup storage */
    start = current_inline->start;
    length = current_inline->length;
    end = current_inline->end;

    if (!bAbort)
#line 5968 "prolang.y"
    {
        backup_start = INLINE_PROGRAM_SIZE;
#ifdef DEBUG_INLINES
printf("DEBUG:   move code to backup %"PRIuMPINT"\n", backup_start);
#endif /* DEBUG_INLINES */
        add_to_mem_block( A_INLINE_PROGRAM, PROGRAM_BLOCK+start, length);
        current_inline->start = backup_start;
    }
    else
#line 5977 "prolang.y"
    {
        current_inline->length = 0; /* Marks this one invalid */
    }

    if (start + length < CURRENT_PROGRAM_SIZE)
#line 5982 "prolang.y"
    {
#ifdef DEBUG_INLINES
printf("DEBUG:   move code forward: from %"PRIuMPINT", length %"PRIuMPINT
       ", to %"PRIuMPINT"\n", 
       start+length, CURRENT_PROGRAM_SIZE - length - start, end);
#endif /* DEBUG_INLINES */
        memmove( PROGRAM_BLOCK+end
               , PROGRAM_BLOCK+start+length
               , CURRENT_PROGRAM_SIZE - length - start
               );
    }
    CURRENT_PROGRAM_SIZE -= length + (start - end);
    stored_bytes -= length + (start - end);

    /* Update last_expression, so it doesn't point into
     * the inline closure code block that we just removed.
     */
    if (bAbort)
        last_expression = -1;
    else
        last_expression = end; /* There is the F_CONTEXT_CLOSURE. */

#ifdef DEBUG_INLINES
printf("DEBUG:   program size: %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */

    /* Move the linenumber data into the backup storage */
    start = current_inline->li_start;
    length = current_inline->li_length;
    if (!bAbort)
#line 6012 "prolang.y"
    {
        backup_start = INLINE_PROGRAM_SIZE;
#ifdef DEBUG_INLINES
printf("DEBUG:   move li data to %"PRIuMPINT", from %"PRIuMPINT" length %"
        PRIuMPINT"\n", 
        backup_start, start, length);
#endif /* DEBUG_INLINES */
        add_to_mem_block( A_INLINE_PROGRAM, LINENUMBER_BLOCK+start, length);
        current_inline->li_start = backup_start;
    }

    /* Skip the lines with the closure. */
    offset = current_inline->end_line - current_inline->start_line;
    while (offset > 0)
#line 6026 "prolang.y"
    {
        int lines;

        lines = offset;
        if (lines > LI_MAXEMPTY)
            lines = LI_MAXEMPTY;
        offset -= lines;
        LINENUMBER_BLOCK[start++] = (char)(256 - lines);
        length--;
    }

    if (start + length < LINENUMBER_SIZE)
#line 6038 "prolang.y"
    {
#ifdef DEBUG_INLINES
printf("DEBUG:   move li data forward: from %"PRIuMPINT", length %"PRIuMPINT
       ", to %"PRIuMPINT"\n", 
       start+length, LINENUMBER_SIZE - length - start, start);
#endif /* DEBUG_INLINES */
        memmove( LINENUMBER_BLOCK+start
               , LINENUMBER_BLOCK+start+length
               , LINENUMBER_SIZE - length - start
               );
    }
    LINENUMBER_SIZE -= length;

    free_local_names(current_inline->block_depth+1);



    /* Restore the globals */
    block_depth              = current_inline->block_depth;
    current_number_of_locals = current_inline->num_locals;
    max_number_of_locals     = current_inline->max_num_locals;
    current_break_stack_need = current_inline->break_stack_size;
    max_break_stack_need     = current_inline->max_break_stack_size;
    exact_types              = current_inline->exact_types;

#ifdef DEBUG_INLINES
printf("DEBUG:   local types: %"PRIuMPINT", context types: %"PRIuMPINT"\n", 
       current_inline->full_local_type_start, current_inline->full_context_type_start);
#endif /* DEBUG_INLINES */
    type_of_locals = &(LOCAL_TYPE(current_inline->full_local_type_start));
    type_of_context = &(LOCAL_TYPE(current_inline->full_context_type_start));
#ifdef DEBUG_INLINES
printf("DEBUG:   type ptrs: %p, %p\n", type_of_locals, type_of_context );
#endif /* DEBUG_INLINES */

    /* Don't free the current_inline->returntype as it's not counted. */

    mem_block[A_LOCAL_TYPES].current_size = current_inline->full_local_type_size;

    /* Remove the structure from the lexical nesting stack */
    if (current_inline->prev == -1)
        current_inline = NULL;
    else
        current_inline = &(INLINE_CLOSURE(current_inline->prev));
} /* finish_inline_closure() */

/*-------------------------------------------------------------------------*/
static void
insert_pending_inline_closures (void)

/* The compilation is a point where pending inline closures can be
 * inserted. Do that now.
 */

#line 6092 "prolang.y"
{
    mp_int ix;
#ifdef DEBUG_INLINES
if (INLINE_CLOSURE_COUNT != 0) printf("DEBUG: insert_inline_closures(): %"
                                      PRIuMPINT" pending\n", 
                                      INLINE_CLOSURE_COUNT);
#endif /* DEBUG_INLINES */

    for (ix = 0; (size_t)ix < INLINE_CLOSURE_COUNT; ix++)
#line 6101 "prolang.y"
    {
        inline_closure_t * ict = &(INLINE_CLOSURE(ix));
#ifdef DEBUG_INLINES
printf("DEBUG:   #%"PRIdMPINT": start %"PRIuMPINT", length %"PRIuMPINT
       ", function %d: new start %"PRIuMPINT"\n", 
       ix, ict->start, ict->length, ict->function, CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */
        if (ict->length != 0)
#line 6109 "prolang.y"
        {
            CURRENT_PROGRAM_SIZE = align(CURRENT_PROGRAM_SIZE);

            store_line_number_info();
            if (stored_lines > ict->start_line)
                store_line_number_backward(stored_lines - ict->start_line);
            else 
                while (stored_lines < ict->start_line)
#line 6117 "prolang.y"
                {
                    int lines;

                    lines = ict->start_line - stored_lines;
                    if (lines > LI_MAXEMPTY)
                        lines = LI_MAXEMPTY;
                    stored_lines += lines;
                    byte_to_mem_block(A_LINENUMBERS, 256 - lines);
                }

            FUNCTION(ict->function)->offset.pc = CURRENT_PROGRAM_SIZE + FUNCTION_PRE_HDR_SIZE;
            add_to_mem_block(A_PROGRAM, INLINE_PROGRAM_BLOCK(ict->start)
                            , ict->length);
#ifdef DEBUG_INLINES
printf("DEBUG:        li_start %"PRIuMPINT", li_length %"PRIuMPINT
       ", new li_start %"PRIuMPINT"\n", 
       ict->li_start, ict->li_length, LINENUMBER_SIZE);
#endif /* DEBUG_INLINES */

            add_to_mem_block(A_LINENUMBERS, INLINE_PROGRAM_BLOCK(ict->li_start)
                            , ict->li_length);
            stored_lines = ict->end_line;
            stored_bytes += ict->length;
        }
    }

    /* Empty the datastorages */
    mem_block[A_INLINE_CLOSURE].current_size = 0;
    mem_block[A_INLINE_PROGRAM].current_size = 0;
} /* insert_pending_inline_closure() */

/*-------------------------------------------------------------------------*/
static Bool
prepare_inline_closure (lpctype_t *returntype)

/* Called after parsing 'func <type>', this creates the identifier
 * with the synthetic function name. The function also sets up the inline
 * closure structure and block scope.
 *
 * If the name can't be generated, FALSE is returned, otherwise TRUE.
 */

#line 6159 "prolang.y"
{
    char name[256+MAXPATHLEN+1];
    fulltype_t funtype;
    ident_t * ident;

    /* Create the name of the new inline function.
     * We have to make sure the name is really unique.
     */
    do
#line 6168 "prolang.y"
    {
        char * start;

        sprintf(name, "__inline_%s_%d_#%04x", current_loc.file->name
                     , current_loc.line, inline_closure_id++);

        /* Convert all non-alnums (but '#') to '_' */
        for (start = name; *start != '\0'; start++)
#line 6176 "prolang.y"
        {
            if (!isalnum((unsigned char)(*start)) && '#' != *start)
                *start = '_';
        }
    } while (    find_shared_identifier(name, 0, 0)
              && inline_closure_id != 0);
    if (inline_closure_id == 0)
#line 6183 "prolang.y"
    {
        yyerror("Can't generate unique name for inline closure");
        return MY_FALSE;
    }

    ident = make_shared_identifier(name, I_TYPE_UNKNOWN, 0);

    /* The lfuns implementing the inline closures should not
     * be callable directly (without the CLOSURE svalue), and also not
     * overrideable.
     */
    funtype = get_fulltype(returntype);
    funtype.t_flags |= TYPE_MOD_NO_MASK | TYPE_MOD_PRIVATE;

    def_function_typecheck(funtype, ident, MY_TRUE);
#ifdef DEBUG_INLINES
printf("DEBUG: New inline closure name: '%s'\n", name);
printf("DEBUG:   current_inline->depth: %d\n", current_inline->block_depth);
printf("DEBUG:           context depth: %d\n", current_inline->block_depth+1);
printf("DEBUG:               arg depth: %d\n", current_inline->block_depth+2);
printf("DEBUG:           current depth: %d\n", block_depth);
#endif /* DEBUG_INLINES */

    return MY_TRUE;
} /* prepare_inline_closure() */

/*-------------------------------------------------------------------------*/
static Bool
inline_closure_prototype (int num_args)

/* Called after parsing 'func <type> <arguments> <context>', this function
 * creates the function prototype entry.
 *
 * Return FALSE if out of memory (the internal structures have been cleaned
 * up then), TRUE otherwise.
 *
 * TODO: This function shares a lot of code with the generic function
 * TODO:: setup. To do this, use entry#0 for gathering the normal
 * TODO:: function information, and entries #1.. for the actual inlines.
 * TODO:: Or use a handful of globals, and save the in the closure entries
 * TODO:: as needed.
 */

#line 6226 "prolang.y"
{
#ifdef DEBUG_INLINES
printf("DEBUG: inline_closure_prototype(%d)\n", num_args);
#endif /* DEBUG_INLINES */
    def_function_prototype(num_args, MY_TRUE);

#ifdef DEBUG_INLINES
printf("DEBUG:   current_inline->depth: %d: %d\n", current_inline->block_depth, block_scope[current_inline->block_depth-1].num_locals);
printf("DEBUG:           context depth: %d: %d\n", current_inline->block_depth+1, block_scope[current_inline->block_depth+1-1].num_locals);
printf("DEBUG:               arg depth: %d: %d\n", current_inline->block_depth+2, block_scope[current_inline->block_depth+2-1].num_locals);
printf("DEBUG:           current depth: %d: %d\n", block_depth, block_scope[block_depth].num_locals);
printf("DEBUG:   Function index: %d\n", current_inline->function);
#endif /* DEBUG_INLINES */

    store_line_number_info();

    /* A function with code: align the function and
     * make space for the function header.
     */
    current_inline->end = CURRENT_PROGRAM_SIZE;
#ifdef DEBUG_INLINES
printf("DEBUG:   program size: %"PRIuMPINT" align to %"PRIuMPINT"\n", 
       CURRENT_PROGRAM_SIZE, align(CURRENT_PROGRAM_SIZE));
#endif /* DEBUG_INLINES */
    CURRENT_PROGRAM_SIZE = align(CURRENT_PROGRAM_SIZE);
    current_inline->start = CURRENT_PROGRAM_SIZE;
    current_inline->li_start = LINENUMBER_SIZE;
    current_inline->start_line = stored_lines;
    stored_bytes = CURRENT_PROGRAM_SIZE; /* Ignore the alignment. */
    
    if (realloc_a_program(FUNCTION_HDR_SIZE))
#line 6257 "prolang.y"
    {
        CURRENT_PROGRAM_SIZE += FUNCTION_HDR_SIZE;
    }
    else
#line 6261 "prolang.y"
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                , mem_block[A_PROGRAM].current_size + FUNCTION_HDR_SIZE);
        finish_inline_closure(MY_TRUE);
        return MY_FALSE;
    }

    return MY_TRUE;
} /* inline_closure_prototype() */

/*-------------------------------------------------------------------------*/
static void
complete_inline_closure ( void )

/* Called after parsing 'func <type> <arguments> <block>', this function
 * updates the function header and moves the closure into the pending
 * area. After that, the function also completes the generation of
 * the F_CONTEXT_CLOSURE instruction.
 *
 * TODO: This function shares a lot of code with the generic function
 * TODO:: setup. To do this, use entry#0 for gathering the normal
 * TODO:: function information, and entries #1.. for the actual inlines.
 * TODO:: Or use a handful of globals, and save the in the closure entries
 * TODO:: as needed.
 */

#line 6287 "prolang.y"
{
    p_int start, li_start;
#ifdef DEBUG_INLINES
printf("DEBUG: Generate inline closure function:\n");
#endif /* DEBUG_INLINES */
    if  (current_inline->include_handle != get_include_handle())
#line 6293 "prolang.y"
    {
        yyerror("Implementation restriction: Inline closure must not span "
                "include file limits");
        /* Clean up */
        leave_block_scope(MY_TRUE);  /* Argument scope */
        leave_block_scope(MY_TRUE);  /* Context scope */
        finish_inline_closure(MY_TRUE);
        return;
    }

    start = current_inline->start;
    li_start = current_inline->li_start;

#ifdef DEBUG_INLINES
printf("DEBUG:   current_inline->depth: %d: %d\n", current_inline->block_depth, block_scope[current_inline->block_depth-1].num_locals);
printf("DEBUG:           context depth: %d: %d\n", current_inline->block_depth+1, block_scope[current_inline->block_depth+1-1].num_locals);
printf("DEBUG:               arg depth: %d: %d\n", current_inline->block_depth+2, block_scope[current_inline->block_depth+2-1].num_locals);
printf("DEBUG:           current depth: %d: %d\n", block_depth, block_scope[block_depth-1].num_locals);
#endif /* DEBUG_INLINES */

    /* Generate the function header and update the ident-table entry.
     */
    def_function_complete(start, MY_TRUE);

    current_inline->length = CURRENT_PROGRAM_SIZE - start;

    store_line_number_info();
    current_inline->li_length = LINENUMBER_SIZE - li_start;
    current_inline->end_line = stored_lines;

    /* Add the code to push the values of the inherited local
     * variables onto the stack, followed by the F_CONTEXT_CLOSURE
     * instruction. Since this code is after the recorded .length,
     * the finish_inline_closure() call will move it backward into
     * its rightful place.
     */
#line 6329 "prolang.y"
    {
        int num_explicit_context = 0;
        int depth = current_inline->block_depth+1;
        block_scope_t * context = &(block_scope[depth-1]);

#ifdef DEBUG_INLINES
printf("DEBUG:   %d context vars, depth %d\n", context->num_locals, depth);
#endif /* DEBUG_INLINES */
        if (context->num_locals != 0)
#line 6338 "prolang.y"
        {
            Bool got_mapped;

            /* To get the context->local information in the right order
             * we read the locals as they are and store the information
             * in an array.
             */
            int * lcmap = alloca(context->num_locals * sizeof(int));
            ident_t * id;
            int i;

            for (i = 0; i < context->num_locals; i++)
                lcmap[i] = -1;

            for (id = all_locals
                ; id && id->u.local.depth >= depth
                ; id = id->next_all)
#line 6355 "prolang.y"
            {
#ifdef DEBUG_INLINES
if (id->u.local.depth == depth) printf("DEBUG:     '%s': local %d, context %d\n", 
                                       get_txt(id->name), id->u.local.num, id->u.local.context);
#endif /* DEBUG_INLINES */
                if (id->u.local.depth == depth
                 && id->u.local.context >= 0
                 && id->u.local.num >= 0
                   )
#line 6364 "prolang.y"
                {
                    lcmap[id->u.local.context] = id->u.local.num;
                }
            }

            /* Got all context->local mappings, now create the bytecode */
            got_mapped = MY_FALSE;
            for (i = 0; i < context->num_locals; i++)
#line 6372 "prolang.y"
            {
                if (lcmap[i] != -1)
#line 6374 "prolang.y"
                {
                    if (lcmap[i] >= CONTEXT_VARIABLE_BASE)
#line 6376 "prolang.y"
                    {
                        ins_f_code(F_CONTEXT_IDENTIFIER);
                        ins_byte(lcmap[i] - CONTEXT_VARIABLE_BASE);
                    }
                    else
#line 6381 "prolang.y"
                    {
                        ins_f_code(F_LOCAL);
                        ins_byte(lcmap[i]);
                    }
                    got_mapped = MY_TRUE;
#ifdef DEBUG_INLINES
printf("DEBUG:     -> F_LOCAL %d\n", lcmap[i]);
#endif /* DEBUG_INLINES */
                }
                else if (got_mapped)
#line 6391 "prolang.y"
                {
                    /* This shouldn't happen, as all explicit context
                     * variables are created before the first implicit
                     * reference can be encountered.
                     */
                    fatal("Explicit context var #%d has higher index than "
                          "implicit context variables.", i);
                }
                else
                    num_explicit_context++;
            }
        } /* Push local vars */

        /* Add the context_closure instruction */
#ifdef DEBUG_INLINES
printf("DEBUG:     -> F_CONTEXT_CLOSURE %d %d %d\n", current_inline->function
      , num_explicit_context, context->num_locals - num_explicit_context);
#endif /* DEBUG_INLINES */
        ins_f_code(F_CONTEXT_CLOSURE);
        ins_short(current_inline->function);
        ins_byte(context->first_local);
        ins_short(num_explicit_context);
        ins_short(context->num_locals - num_explicit_context);
    } /* Complete F_CONTEXT_CLOSURE instruction */


    /* Clean up */
    leave_block_scope(MY_TRUE);  /* Argument scope */
    leave_block_scope(MY_TRUE);  /* Context scope */
    finish_inline_closure(MY_FALSE);
} /* complete_inline_closure() */

/* =========================   PROGRAM STRINGS   ========================= */

/*-------------------------------------------------------------------------*/
static short
store_prog_string (string_t *str)

/* Add the tabled string <str> to the strings used by the program.
 * The function takes care that the same string is not stored twice.
 * The function adopts the reference of <str>.
 * Result is the index of the string in the table or -1 in case of errors (out of memory).
 */

#line 6435 "prolang.y"
{
    mp_uint str_size, next_size;
    hash32_t hash;
    int i, *indexp;

    /* Compute the hash and the tagmask for the hash table */
    hash = hashpointer(str) & 0xff;

    indexp = &prog_string_indizes[hash];

    if (*indexp >= 0)
#line 6446 "prolang.y"
    {
        /* There is a hash chain for this hash: search the
         * string in there.
         */
        i = *indexp;
        for(;;)
#line 6452 "prolang.y"
        {
            if ( PROG_STRING(i) == str )
#line 6454 "prolang.y"
            {
                // same string as the new one.
                free_mstring(str); /* Drop the extra ref. */
                last_string_is_new = MY_FALSE;
                return i;
            }
            if ((i = PROG_STRING_NEXT(i)) < 0)
                break;
        }

        /* Not found: re-get the initial 'next'-index. After insertation of the
         * new string its PROG_STRING_NEXT will point to i (the old *indexp)
         */
        i = *indexp;
    }
    else
#line 6470 "prolang.y"
    {
        /* The first time this hash shows up (which also implies
         * that <str> is a new string. i will be used to terminate this
         * hash chain below.
         */
        i = -1;
    }

    /* Add a totally new string */

    str_size = mem_block[A_STRINGS].current_size;
    next_size = mem_block[A_STRING_NEXT].current_size;

    /* Make sure we have enough memory */
    if (str_size + sizeof(string_t *) > mem_block[A_STRINGS].max_size
     || next_size + sizeof(int) > mem_block[A_STRING_NEXT].max_size
       )
#line 6487 "prolang.y"
    {
        if (!realloc_mem_block(&mem_block[A_STRINGS], 0)
         || !realloc_mem_block(&mem_block[A_STRING_NEXT], 0))
#line 6490 "prolang.y"
        {
            yyerrorf("Out of memory for new program string (%zu bytes).",
                    sizeof(string_t *) + sizeof(int));
            last_string_is_new = MY_FALSE;
            return -1;
        }
    }

    /* Add the string pointer to the A_STRING area. */
    PROG_STRING(STRING_COUNT) = str;
    mem_block[A_STRINGS].current_size += sizeof(A_STRINGS_t);

    /* Add the old prog_string_index[] as a new entry in A_STRING_NEXT */
    PROG_STRING_NEXT(STRING_NEXT_COUNT) = i;
    mem_block[A_STRING_NEXT].current_size += sizeof(A_STRING_NEXT_t);

    /* Store the string index in A_STRINGS as new prog_string_index[] at its
     * hash position. */
    *indexp = str_size / sizeof str;

    last_string_is_new = MY_TRUE;
    return *indexp;
} /* store_prog_string() */

/*-------------------------------------------------------------------------*/
static int
ins_prog_string (string_t *str)

/* Add the tabled string <str> to the strings used by the program
 * and inserts code to put it on the stack into the current bytecode.
 * The function adopts the reference of <str>.
 * Returns the number of bytes written to the bytecode.
 */
#line 6523 "prolang.y"
{
    PREPARE_INSERT(3);
    int string_number = store_prog_string(str);
    if ( string_number <= 0xff )
#line 6527 "prolang.y"
    {
        add_f_code(F_CSTRING0);
        add_byte(string_number);
    }
    else if ( string_number <= 0x1ff )
#line 6532 "prolang.y"
    {
        add_f_code(F_CSTRING1);
        add_byte(string_number);
    }
    else if ( string_number <= 0x2ff )
#line 6537 "prolang.y"
    {
        add_f_code(F_CSTRING2);
        add_byte(string_number);
    }
    else if ( string_number <= 0x3ff )
#line 6542 "prolang.y"
    {
        add_f_code(F_CSTRING3);
        add_byte(string_number);
    }
    else
#line 6547 "prolang.y"
    {
        add_f_code(F_STRING);
        add_short(string_number);
        CURRENT_PROGRAM_SIZE += 3;
        return 3;
    }
    CURRENT_PROGRAM_SIZE += 2;
    return 2;
} /* ins_prog_string */

/*-------------------------------------------------------------------------*/
static void
delete_prog_string (void)

/* Remove the program string last added with store_prog_string().
 */

#line 6564 "prolang.y"
{
    string_t *str;
    hash32_t hash;
    int *indexp;

    /* Remove the string from the A_STRINGS area */
    mem_block[A_STRINGS].current_size -= sizeof(A_STRINGS_t);
    str = PROG_STRING(STRING_COUNT);

    /* Remove the string from the hash table */
    hash = hashpointer(str) & 0xff;
    indexp = &prog_string_indizes[hash];

    // let *indexp in the hash table point to the former next string.
    mem_block[A_STRING_NEXT].current_size -= sizeof(int);
    *indexp = PROG_STRING_NEXT(STRING_NEXT_COUNT);
    // BTW: if that is now -1, the hash chain is empty.

    // and finally free the string
    free_mstring(str);

} /* delete_prog_string() */


/*=========================================================================*/

#if defined(__MWERKS__) && !defined(WARN_ALL)
#    pragma warn_possunwant off
#    pragma warn_implicitconv off
#endif

#line 7104 "y.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    L_ASSIGN = 258,
    L_ARROW = 259,
    L_BREAK = 260,
    L_CASE = 261,
    L_CATCH = 262,
    L_CLOSURE = 263,
    L_CLOSURE_DECL = 264,
    L_COLON_COLON = 265,
    L_CONTINUE = 266,
    L_DEC = 267,
    L_DEFAULT = 268,
    L_DO = 269,
    L_DUMMY = 270,
    L_ELLIPSIS = 271,
    L_ELSE = 272,
    L_EQ = 273,
    L_FUNC = 274,
    L_BEGIN_INLINE = 275,
    L_END_INLINE = 276,
    L_FLOAT = 277,
    L_FLOAT_DECL = 278,
    L_FOR = 279,
    L_FOREACH = 280,
    L_GE = 281,
    L_IDENTIFIER = 282,
    L_IF = 283,
    L_INC = 284,
    L_INHERIT = 285,
    L_INLINE_FUN = 286,
    L_INT = 287,
    L_LAND = 288,
    L_LE = 289,
    L_LOCAL = 290,
    L_LOR = 291,
    L_LSH = 292,
    L_MAPPING = 293,
    L_MIXED = 294,
    L_NE = 295,
    L_NO_MASK = 296,
    L_NOSAVE = 297,
    L_DEPRECATED = 298,
    L_NOT = 299,
    L_NUMBER = 300,
    L_OBJECT = 301,
    L_PARSE_COMMAND = 302,
    L_PRIVATE = 303,
    L_PROTECTED = 304,
    L_PUBLIC = 305,
    L_QUOTED_AGGREGATE = 306,
    L_RANGE = 307,
    L_RETURN = 308,
    L_RSH = 309,
    L_RSHL = 310,
    L_SSCANF = 311,
    L_STATIC = 312,
    L_STATUS = 313,
    L_STRING = 314,
    L_STRING_DECL = 315,
    L_STRUCT = 316,
    L_SWITCH = 317,
    L_SYMBOL = 318,
    L_SYMBOL_DECL = 319,
    L_VARARGS = 320,
    L_VIRTUAL = 321,
    L_VISIBLE = 322,
    L_VOID = 323,
    L_WHILE = 324,
    LOWER_THAN_ELSE = 325
  };
#endif
/* Tokens.  */
#define L_ASSIGN 258
#define L_ARROW 259
#define L_BREAK 260
#define L_CASE 261
#define L_CATCH 262
#define L_CLOSURE 263
#define L_CLOSURE_DECL 264
#define L_COLON_COLON 265
#define L_CONTINUE 266
#define L_DEC 267
#define L_DEFAULT 268
#define L_DO 269
#define L_DUMMY 270
#define L_ELLIPSIS 271
#define L_ELSE 272
#define L_EQ 273
#define L_FUNC 274
#define L_BEGIN_INLINE 275
#define L_END_INLINE 276
#define L_FLOAT 277
#define L_FLOAT_DECL 278
#define L_FOR 279
#define L_FOREACH 280
#define L_GE 281
#define L_IDENTIFIER 282
#define L_IF 283
#define L_INC 284
#define L_INHERIT 285
#define L_INLINE_FUN 286
#define L_INT 287
#define L_LAND 288
#define L_LE 289
#define L_LOCAL 290
#define L_LOR 291
#define L_LSH 292
#define L_MAPPING 293
#define L_MIXED 294
#define L_NE 295
#define L_NO_MASK 296
#define L_NOSAVE 297
#define L_DEPRECATED 298
#define L_NOT 299
#define L_NUMBER 300
#define L_OBJECT 301
#define L_PARSE_COMMAND 302
#define L_PRIVATE 303
#define L_PROTECTED 304
#define L_PUBLIC 305
#define L_QUOTED_AGGREGATE 306
#define L_RANGE 307
#define L_RETURN 308
#define L_RSH 309
#define L_RSHL 310
#define L_SSCANF 311
#define L_STATIC 312
#define L_STATUS 313
#define L_STRING 314
#define L_STRING_DECL 315
#define L_STRUCT 316
#define L_SWITCH 317
#define L_SYMBOL 318
#define L_SYMBOL_DECL 319
#define L_VARARGS 320
#define L_VIRTUAL 321
#define L_VISIBLE 322
#define L_VOID 323
#define L_WHILE 324
#define LOWER_THAN_ELSE 325

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 6687 "prolang.y" /* yacc.c:355  */

#line 6689 "prolang.y"
    p_int number;
      /* Literal numbers, or whereever a number is required.
       */

    double float_number;
      /* Literal floats */

    struct {
        p_int number;
        unsigned short inhIndex;
    } closure;
      /* A closure (#'xxx). The .number determines the exact
       * nature of the closure.
       * For lfun closures, an inhIndex > 0 determines the
       * (inheritance index + 1) of a direct reference to an
       * inherited closure.
       */

    struct {
        string_t *name;  /* The tabled string with the name */
        int   quotes;    /* Number of quotes */
    } symbol;
      /* A literal symbol.
       */

    ident_t *ident;
      /* L_IDENTIFIER, L_INLINE_FUN: The recognized identifier
       */

    typeflags_t typeflags;
      /* Just the typeflags (reference, pointer, visibility).
       */

    fulltype_t fulltype;
      /* The fulltype (datatype plus visibility) of entities.
       */

    lpctype_t *lpctype;
      /* An LPC type without modifiers.
       */

    funflag_t inh_flags[2];
      /* Inheritance: [0]: code inheritance qualifiers
       *              [1]: variable inheritance qualifiers
       */

    svalue_t *initialized;
      /* Position where to store the variable initializer.
       */

    p_int numbers[2];
      /* Often used to save the current break/continue address.
       */

    p_uint address;
      /* Address of an instruction. */

    struct {
        bytecode_p     p;       /* The condition code */
        unsigned short length;  /* Length of the condition code */
        unsigned short line;    /* Last source line of the condition code */
    } expression;
      /* Expressions are used to save the code for a loop-condition
       * while the body is compiled.
       */

    struct rvalue_s
#line 6756 "prolang.y"
    {
        fulltype_t type;   /* Type of the expression */
        uint32     start;  /* Startaddress of the expression */
    } rvalue;
      /* Just a simple expression. */

    struct lrvalue_s
#line 6763 "prolang.y"
    {
        fulltype_t     type;     /* Type of the expression */
        uint32         start;    /* Startaddress of the instruction */
        lvalue_block_t lvalue;   /* Code of the expression as an lvalue */
    }
    lrvalue;
      /* Used for expressions which may return a rvalue or lvalues.
       * This happens when this expression is followed by an
       * index or range expression to yield an lvalue.
       * Then the indexed expression should be converted to an lvalue, too.
       * (Because for strings, the string itself must be replaced,
       * when a single character changes. For arrays and strings
       * an assignment to a range expression might replace the whole
       * array/string.)
       *
       * Lvalue generation in places where either a r- or an lvalue
       * is acceptible first generates the rvalue code, but stores
       * the entire code for the lvalue generation in the LVALUE_BLOCK.
       * .lvalue contains the pointers therein.
       *
       * If .lvalue.size == 0, then the expression cannot be an lvalue.
       */

    struct
#line 6787 "prolang.y"
    {
        fulltype_t type;         /* Type of the expression         */
        uint32     start;        /* Startaddress of the expression */
        bool       might_lvalue; /* Might be an lvalue reference.  */
    } function_call_result;
      /* A function call expression. */

    struct incdec_s
#line 6795 "prolang.y"
    {
        uint32     start;  /* Current programm pointer */
        short      code;   /* The opcode (F_PRE_INC or F_PRE_DEC) */
    }
    incdec;
      /* For pre-increment or -decrement remembers which opcode to use,
       * because it must be issued after the following expression has
       * been compiled.
       */

    struct s_index
#line 6806 "prolang.y"
    {
        short      rvalue_inst;  /* Index opcode for rvalues */
        short      lvalue_inst;  /* Index opcode for lvalues */
        short      vlvalue_inst; /* Index opcode for reseating lvalues
                                    This is not used for ranges. */
        uint32     start;        /* Startaddress of the index */
        uint32     end;          /* Endaddress+1 of the index */
        fulltype_t type1;        /* Type of index, resp. lower bound */
        fulltype_t type2;        /* Type of other index, resp. upper bound */
    }
    index;
      /* This is used to parse and return the indexing operation
       * of an array or mapping.
       * .rvalue_inst gives the type of the operation:
       *   F_INDEX:     [x]
       *   F_RINDEX:    [<x]
       *   F_AINDEX:    [>x]
       *   F_RANGE:     [ x.. y]
       *   F_RN_RANGE:  [<x.. y]
       *   F_NR_RANGE:  [ x..<y]
       *   F_RR_RANGE:  [<x..<y]
       *   F_AN_RANGE:  [>x.. y]
       *   F_AR_RANGE:  [>x..<y]
       *   F_NA_RANGE:  [ x..>y]
       *   F_RA_RANGE:  [<x..>y]
       *   F_AA_RANGE:  [>x..>y]
       *   F_NX_RANGE:  [ x..  ]
       *   F_RX_RANGE:  [<x..  ]
       *   F_AX_RANGE:  [>x..  ]
       * .lvalue_inst contains the corresponding opcode for
       * lvalue generation.
       * .start and .end are the bytecode limits of the whole
       * operation.
       * .type1 and optionally .type2 are the types of the
       * index values.
       */

    struct lvalue_s {
        lpctype_t *    type;
        lvalue_block_t lvalue;
        short          vlvalue_inst;
        short          num_arg;
    } lvalue;
      /* Used in assigns to communicate how an lvalue has to be accessed
       * (by passing on the bytecode to create) and what type it is.
       *
       * An lvalue expression is not put directly in the program code,
       * because it will be evaluated after the right hand side of the
       * assignment (eg. a = b = c; will be evaluated from right to
       * left). The compiled bytecodes for the lvalue are stored in the
       * LVALUE_BLOCK and should be freed using free_lvalue_block().
       *
       * If this lvalue expression can be used for reseating assignments,
       * then vlvalue_inst (if != 0) will contain the instruction that
       * must replace the last instruction in the lvalue block.
       * (The last instruction will have .num_arg bytes following it.)
       */

    struct {
        p_int key;     /* shared string ptr, or a number */
        Bool numeric;  /* TRUE: .key is a number */
    } case_label;
      /* Used to return the value of a 'case' label.
       */

    char *string;
      /* An allocated string */

    string_t *sh_string;
      /* A shared string */

    struct {
        char    *super; /* NULL, or the allocated qualifier */
        ident_t *real;  /* The function identifier */
    } function_name;
      /* A qualified function name: "<super>::<func>" */

    struct {
        int    simul_efun;    /* -1, or index of the simul_efun */
        p_int  start;         /* Address of the function call */
        efun_override_t efun_override; /* set on (s)efun:: prefix. */
    } function_call_head;
      /* Used to save address and possible sefun-index over
       * the argument parsing in a function call.
       */

    struct {
        int length;            /* Number of initializers parsed */
        /* Description of initializers parsed: */
        struct struct_init_s * list;  /* Head of list */
        struct struct_init_s * last;  /* Tail of list */
    } struct_init_list;
      /* For runtime struct literals: head of the list describing
       * the encountered member initializers.
       */

    struct {
        string_t * name;  /* Member name, or NULL if unnamed */
        fulltype_t type;  /* Member expr type */
    } struct_init_member;
      /* For runtime struct literals: information about a single
       * member initializer.
       */


#line 7514 "y.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 7531 "y.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   3810

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  92
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  150
/* YYNRULES -- Number of rules.  */
#define YYNRULES  384
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  647

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   325

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    81,    74,     2,
      85,    86,    79,    77,    90,    78,     2,    80,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    87,    84,
      75,     2,    76,    71,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    83,     2,    91,    73,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    88,    72,    89,    82,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  7041,  7041,  7044,  7045,  7047,  7049,  7051,  7059,  7065,
    7062,  7077,  7083,  7084,  7085,  7094,  7094,  7116,  7125,  7138,
    7173,  7125,  7250,  7248,  7313,  7330,  7334,  7340,  7349,  7351,
    7354,  7356,  7359,  7361,  7362,  7367,  7372,  7374,  7388,  7393,
    7392,  7417,  7418,  7479,  7481,  7485,  7486,  7490,  7491,  7496,
    7512,  7730,  7724,  7733,  7737,  7739,  7745,  7827,  7857,  7858,
    7860,  7864,  7871,  7872,  7876,  7877,  7878,  7879,  7880,  7881,
    7882,  7883,  7884,  7889,  7890,  7894,  7895,  7899,  7900,  7910,
    7911,  7912,  7913,  7914,  7915,  7916,  7917,  7918,  7919,  7938,
    7941,  7949,  7950,  7955,  7964,  7974,  7983,  7996,  7997,  7998,
    8002,  8003,  8007,  8031,  8076,  8088,  8088,  8105,  8119,  8118,
    8148,  8153,  8153,  8181,  8183,  8184,  8189,  8193,  8200,  8199,
    8211,  8210,  8221,  8229,  8238,  8237,  8251,  8250,  8267,  8278,
    8279,  8279,  8279,  8279,  8279,  8279,  8280,  8281,  8282,  8284,
    8312,  8365,  8368,  8416,  8427,  8416,  8563,  8576,  8563,  8693,
    8707,  8720,  8763,  8697,  8897,  8902,  8908,  8910,  8909,  8918,
    8922,  8956,  8980,  8984,  9012,  9028,  9036,  9010,  9171,  9172,
    9182,  9196,  9197,  9209,  9217,  9221,  9250,  9315,  9317,  9408,
    9409,  9413,  9416,  9416,  9419,  9446,  9507,  9514,  9530,  9562,
    9608,  9641,  9646,  9645,  9669,  9670,  9671,  9672,  9673,  9674,
    9675,  9676,  9677,  9678,  9679,  9680,  9681,  9682,  9683,  9684,
    9698,  9705,  9706,  9707,  9708,  9709,  9714,  9719,  9723,  9725,
    9727,  9757,  9759,  9758,  9791,  9789,  9947,  9951,  9960,  9950,
   10058, 10059, 10084, 10083, 10108, 10122, 10136, 10150, 10172, 10193,
   10202, 10212, 10222, 10234, 10249, 10263, 10278, 10277, 10376, 10383,
   10395, 10406, 10418, 10464, 10522, 10538, 10547, 10561, 10603, 10618,
   10637, 10659, 10713, 10724, 10730, 10735, 10750, 10751, 10756, 10768,
   10774, 10780, 10787, 10796, 10812, 10866, 10912, 10938, 10954, 10964,
   10978, 11007, 11003, 11024, 11058, 11063, 11071, 11070, 11129, 11216,
   11255, 11293, 11327, 11365, 11399, 11467, 11499, 11502, 11540, 11576,
   11624, 11677, 11694, 11706, 11712, 11727, 11745, 11761, 11781, 11822,
   11863, 11904, 11917, 11931, 11945, 11959, 11973, 11987, 12001, 12015,
   12029, 12043, 12061, 12087, 12088, 12089, 12093, 12094, 12103, 12104,
   12108, 12125, 12135, 12148, 12161, 12162, 12163, 12164, 12165, 12169,
   12176, 12188, 12189, 12197, 12200, 12203, 12209, 12225, 12226, 12227,
   12234, 12236, 12241, 12254, 12270, 12275, 12290, 12289, 12888, 12930,
   13159, 13162, 13165, 13159, 13171, 13177, 13200, 13205, 13213, 13273,
   13334, 13334, 13337, 13353, 13352, 13455, 13458, 13465, 13477, 13484,
   13501, 13546, 13558, 13575, 13578
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "L_ASSIGN", "L_ARROW", "L_BREAK",
  "L_CASE", "L_CATCH", "L_CLOSURE", "L_CLOSURE_DECL", "L_COLON_COLON",
  "L_CONTINUE", "L_DEC", "L_DEFAULT", "L_DO", "L_DUMMY", "L_ELLIPSIS",
  "L_ELSE", "L_EQ", "L_FUNC", "L_BEGIN_INLINE", "L_END_INLINE", "L_FLOAT",
  "L_FLOAT_DECL", "L_FOR", "L_FOREACH", "L_GE", "L_IDENTIFIER", "L_IF",
  "L_INC", "L_INHERIT", "L_INLINE_FUN", "L_INT", "L_LAND", "L_LE",
  "L_LOCAL", "L_LOR", "L_LSH", "L_MAPPING", "L_MIXED", "L_NE", "L_NO_MASK",
  "L_NOSAVE", "L_DEPRECATED", "L_NOT", "L_NUMBER", "L_OBJECT",
  "L_PARSE_COMMAND", "L_PRIVATE", "L_PROTECTED", "L_PUBLIC",
  "L_QUOTED_AGGREGATE", "L_RANGE", "L_RETURN", "L_RSH", "L_RSHL",
  "L_SSCANF", "L_STATIC", "L_STATUS", "L_STRING", "L_STRING_DECL",
  "L_STRUCT", "L_SWITCH", "L_SYMBOL", "L_SYMBOL_DECL", "L_VARARGS",
  "L_VIRTUAL", "L_VISIBLE", "L_VOID", "L_WHILE", "LOWER_THAN_ELSE", "'?'",
  "'|'", "'^'", "'&'", "'<'", "'>'", "'+'", "'-'", "'*'", "'/'", "'%'",
  "'~'", "'['", "';'", "'('", "')'", "':'", "'{'", "'}'", "','", "']'",
  "$accept", "all", "program", "possible_semi_colon", "note_start", "def",
  "$@1", "$@2", "function_body", "@3", "inline_func", "$@4", "$@5", "$@6",
  "$@7", "inline_opt_args", "inline_opt_type", "inline_opt_context",
  "inline_semi", "inline_context_list", "context_decl",
  "inline_comma_expr", "struct_decl", "$@8", "opt_base_struct",
  "opt_member_list", "member_list", "member", "member_name_list",
  "inheritance", "inheritance_qualifiers", "inheritance_modifier",
  "inheritance_modifier_list", "inheritance_qualifier",
  "default_visibility", "optional_stars", "type", "non_void_type",
  "type_modifier_list", "type_modifier", "opt_basic_type",
  "opt_basic_non_void_type", "basic_non_void_type",
  "single_basic_non_void_type", "basic_type", "cast", "decl_cast",
  "identifier", "argument", "argument_list", "new_arg_name", "name_list",
  "@9", "@10", "block", "statements_block", "$@11", "statements",
  "local_name_list", "@12", "@13", "@14", "@15", "statement", "return",
  "while", "@16", "@17", "do", "@18", "$@19", "for", "@20", "@21", "@22",
  "@23", "for_init_expr", "comma_expr_decl", "$@24", "expr_decl",
  "for_expr", "foreach", "@25", "@26", "$@27", "foreach_vars",
  "foreach_var_decl", "foreach_var_lvalue", "foreach_in", "foreach_expr",
  "switch", "$@28", "switch_block", "switch_statements", "switch_label",
  "case", "case_label", "default", "condStart", "cond", "optional_else",
  "@29", "constant", "string_constant", "comma_expr", "$@30", "expr0",
  "@31", "@32", "@33", "@34", "@35", "@36", "lvalue_reference",
  "pre_inc_dec", "expr4", "$@37", "@38", "name_lvalue", "lvalue",
  "name_var_lvalue", "local_name_lvalue", "index_expr", "index_range",
  "expr_list", "expr_list2", "arg_expr_list", "arg_expr_list2",
  "m_expr_list", "m_expr_list2", "m_expr_values", "struct_member_name",
  "opt_struct_init", "possible_comma", "opt_struct_init2", "struct_init",
  "function_call", "@39", "@40", "call_other_name", "function_name",
  "anchestor", "catch", "@41", "opt_catch_mods", "opt_catch_mod_list",
  "opt_catch_modifier", "sscanf", "parse_command", "lvalue_list", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,    63,   124,    94,    38,    60,    62,    43,    45,    42,
      47,    37,   126,    91,    59,    40,    41,    58,   123,   125,
      44,    93
};
# endif

#define YYPACT_NINF -395

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-395)))

#define YYTABLE_NINF -380

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -395,    16,   220,  -395,  -395,   -52,  -395,  -395,    22,    85,
    -395,  -395,   125,   340,   160,    84,   139,  3129,  -395,  -395,
      87,  -395,  -395,  -395,    27,  -395,  -395,  -395,  -395,  -395,
    -395,  -395,  -395,  -395,  -395,  -395,  -395,  -395,  -395,  -395,
      94,  -395,  -395,  -395,  -395,  1202,  -395,  -395,    99,   103,
    -395,  -395,  -395,  -395,  -395,   227,   126,    87,   -46,   480,
     118,   202,   258,  -395,  -395,   183,  1202,  -395,    43,  -395,
    -395,   148,   173,  -395,   -42,  1836,  -395,   207,  -395,   103,
     310,  -395,  -395,   286,  -395,   238,  3160,   232,   260,  -395,
     348,  -395,  -395,   267,  -395,  -395,  3210,  -395,  -395,    47,
    -395,    53,  1893,  -395,  -395,  -395,  -395,   132,  -395,   146,
    1893,  1893,   178,  -395,  1893,  1893,  3350,   350,  1935,    20,
    -395,   185,  -395,  -395,   345,  -395,  -395,  -395,   331,   271,
     359,  -395,  -395,  -395,  -395,    99,  -395,  -395,  1893,   288,
    -395,  -395,  -395,  -395,  -395,  -395,   292,   922,   295,  -395,
    -395,  -395,  1977,  -395,  -395,  -395,   901,   281,  3210,  1893,
     -31,  -395,  -395,  1893,  1893,  -395,  1893,  -395,  1893,  1893,
    1893,  1893,  -395,  1893,  1893,  1893,  1893,  1893,  -395,  1893,
    1893,  1893,  1893,  1893,   -55,    80,  -395,   127,  1615,   186,
     199,  -395,  -395,  -395,   299,   284,   301,  1202,  1836,   287,
    -395,  3350,  1893,   306,  1062,  1836,  3350,   280,   304,  1836,
     317,   320,    60,   333,  -395,   595,   922,   322,   327,  3350,
    -395,  2063,   253,  1893,   253,  1893,   259,  2063,   259,   259,
    1836,  3497,  3570,  3641,   253,   253,  1893,   191,  -395,  -395,
    -395,  3350,   227,  -395,   108,  1893,   329,   201,  -395,  1691,
    1836,  1836,  2001,  1893,  1223,  -395,  -395,  -395,   332,  1202,
    -395,   184,   -14,  3350,  -395,  -395,   334,   335,   -42,  -395,
      26,   291,   342,   338,   343,   344,  1733,   347,  -395,  -395,
     409,   289,  -395,   236,  -395,   351,  -395,   368,  -395,   425,
    -395,  -395,  -395,  1144,  -395,   251,  2879,   354,   979,  2944,
    -395,  -395,   356,   357,  -395,  -395,  3039,   360,   353,   361,
     363,   370,  -395,   364,  3424,  3414,  3350,   191,   372,  3069,
     373,  1836,  1836,  2191,  2097,  2127,   426,  1836,  -395,  3350,
    3261,   374,   376,   379,  -395,  -395,  -395,  -395,  -395,   383,
     382,   385,  -395,  -395,  -395,  -395,  -395,  1893,   335,  1893,
     390,  -395,  -395,   471,   478,  -395,  -395,  -395,   399,  1144,
     474,   410,  -395,  1836,  -395,  3350,  1893,  -395,  -395,  -395,
    1836,  1836,   412,   652,   407,   694,   419,  -395,  1893,   411,
     421,  1223,  2221,  2285,  -395,   468,  -395,   553,  -395,  1836,
    1836,  -395,  2315,  2379,  -395,  -395,  1893,  -395,    98,   227,
     423,  -395,  3210,  -395,  1467,  3205,   424,   428,  -395,   837,
     504,   513,   -10,  1893,   449,  -395,  -395,  2974,  3350,  1284,
    2409,  3350,  1836,  -395,  3039,  -395,  3350,  1893,   434,  -395,
    -395,  1836,  1836,  -395,  2473,  1836,  1836,  -395,  2503,  2567,
    2597,  -395,   206,  3273,  -395,   751,   436,  -395,  -395,   448,
    -395,   443,   334,   290,   451,   446,  -395,  3350,   535,    93,
    -395,  -395,  -395,  -395,  -395,  -395,  -395,  1836,  1836,   536,
     537,   455,  -395,  1144,  1893,   211,    39,    50,   461,  3350,
     465,   462,  -395,   469,  3350,   412,  3350,  -395,  2661,  2691,
    -395,  2755,  2785,  -395,  -395,  -395,  -395,  3350,   227,  3210,
    -395,  -395,  -395,  -395,  -395,   466,  1836,  -395,  3205,  -395,
    -395,   470,  3350,  3350,   554,   561,  -395,   486,  -395,  3350,
    -395,  1935,  1893,  -395,  1345,  -395,  -395,  -395,  -395,  -395,
    -395,  -395,  -395,  1794,  1546,  3350,  -395,  1893,   193,  1836,
    1836,  1144,  1893,   237,  -395,  3350,   348,  -395,   490,   335,
    -395,   491,  3337,   163,   489,    54,  -395,  -395,  -395,  -395,
    3350,  3350,  -395,   492,  -395,  -395,  -395,  1893,   182,  -395,
     182,   182,   163,   -33,  3488,   502,  -395,  -395,  -395,  -395,
     497,  1406,  1144,  3350,   182,  -395,  -395,  -395,   203,   163,
    -395,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,  -395,   498,
    -395,  -395,   496,  2157,   512,   512,   339,  2157,   339,   339,
    3561,  3634,  3705,   512,   512,   285,   285,  -395,  -395,  -395,
    -395,  -395,  1144,  -395,    73,    62,   127,  1615,    81,    82,
      68,   115,    86,  2031,  1836,  2849,   111
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       4,     0,    62,     1,    62,     5,    12,    13,     0,    51,
      62,    14,     0,    54,     0,     0,     0,    54,     6,     3,
       0,    53,    62,    52,    56,    83,    85,    80,    86,    87,
      64,    70,    71,    82,    66,    69,    67,    65,    79,    81,
       0,    84,    68,    72,    92,     0,    63,    60,    91,    77,
      73,    11,    58,    57,    56,     0,   216,     0,     0,    55,
       0,     0,    95,    96,    88,     0,     0,    89,     0,    95,
     218,     0,     0,    50,    62,     0,    38,    41,    90,    78,
     107,    59,   220,   217,    98,     0,    76,     0,    99,   100,
       0,   373,   275,     0,   267,   301,    26,    22,   277,   288,
     266,   289,     0,   274,     7,     7,     7,   273,   276,     0,
       0,     0,     7,   269,     0,     0,   106,   260,     0,   262,
     296,     0,   268,   356,     0,   270,   271,   272,     0,     0,
       0,   219,   102,   103,    61,    75,     9,    62,     0,     0,
     366,   367,    18,    27,   113,   255,     0,     0,     0,   371,
     294,   295,     0,   263,   257,   256,     7,     7,     7,     0,
       0,   253,   252,     0,     0,   232,     0,   230,     0,     0,
       0,     0,   227,     0,     0,     0,     0,     0,   246,     0,
       0,     0,     0,     0,     7,     0,   254,     0,     0,   291,
     292,   224,   259,   258,     0,     0,     0,    43,     0,    15,
     101,   226,     0,    24,     0,     0,   326,     0,   324,     0,
       0,   268,     0,     0,     7,     0,     0,     0,   222,   221,
      93,   237,   240,     0,   242,     0,   243,   238,   244,   245,
       0,   234,   235,   236,   241,   239,     0,   248,   249,   251,
     250,   261,     7,     7,   345,     0,   343,   290,   358,     0,
       0,     0,     0,     0,     0,   368,   369,    42,     0,    44,
      45,     0,     0,   109,    17,    10,     0,     7,    62,    19,
       0,     0,     0,     0,     0,     0,     0,     0,   138,   111,
       0,     0,   137,     0,   115,     0,   131,     0,   132,     0,
     133,   134,   135,     0,   130,    37,     0,     0,     0,     0,
     264,   265,     0,     0,   286,   281,   326,   337,     0,   335,
       0,     0,   278,     0,   233,   231,   228,   247,   344,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   305,   225,
     330,     0,   329,     0,    46,    47,    58,    48,    16,   376,
       0,    28,   129,   139,   140,   149,   164,     0,   142,     0,
       0,   113,    23,   116,   117,   114,    58,   136,     0,     0,
     191,   222,   128,     0,   280,   327,     0,   285,   284,     7,
       0,     0,   339,     0,     0,     0,     0,    94,     0,     0,
     346,     0,     0,     0,   308,     0,   306,     0,   307,     0,
       0,   320,     0,     0,   331,   357,     0,    40,     0,     0,
       0,    25,    32,    20,     0,     0,   222,   222,   110,     0,
       0,     0,     0,     0,     0,   192,   190,     0,   383,     0,
       0,   341,     0,   283,     0,   279,   223,     0,     0,   309,
     310,     0,     0,   321,     0,     0,     0,   322,     0,     0,
       0,   311,   293,   332,    49,     0,   375,   378,   374,    30,
      33,    35,     0,     0,     0,   155,   156,   159,   161,     0,
     168,   170,   302,   172,   171,   189,   177,     0,     0,   122,
     123,   222,   147,     0,     0,     0,   288,   289,     0,   355,
       0,   350,   352,     0,   342,   340,   229,   359,     0,     0,
     313,     0,     0,   316,   312,   315,   333,   380,     0,    31,
      29,    21,   303,   304,   150,     0,     0,   174,     0,   173,
     165,     0,   119,   121,     0,     0,   144,     0,   193,   383,
     381,     0,     0,   287,     0,   348,   282,   314,   317,   318,
     319,   377,    34,     0,     0,   160,   169,     0,     0,     0,
       0,     0,     0,     0,   384,   354,   349,   353,     0,   163,
     158,     0,   175,     0,     0,     0,   180,   111,   182,   183,
     125,   127,   145,   222,   382,   151,   166,     0,     0,   215,
       0,     0,     0,     0,   186,   187,   188,   178,   179,   181,
       0,     0,     0,   176,     0,   213,   212,   214,     0,     0,
     184,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   148,     0,
     167,   211,     0,   197,   200,   202,   203,   198,   204,   205,
     194,   195,   196,   201,   199,   206,   207,   208,   210,   209,
     152,   185,     0,   153,   294,   295,     0,     0,   297,   299,
     288,   289,   300,     0,     0,     0,   298
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -395,  -395,  -395,  -395,   -83,  -395,  -395,  -395,  -395,  -395,
    -395,  -395,  -395,  -395,  -395,  -395,  -395,  -395,  -395,  -395,
      88,  -395,  -395,  -395,  -395,  -395,  -395,   326,  -395,  -395,
     318,  -395,  -395,  -395,  -395,  -322,   584,  -395,   283,  -395,
    -395,  -395,   -40,   -48,    -9,  -395,  -395,  -150,   352,  -395,
     457,  -395,  -395,  -395,  -256,    38,  -395,   248,  -390,  -395,
    -395,  -395,  -395,  -278,  -395,  -395,  -395,  -395,  -395,  -395,
    -395,  -395,  -395,  -395,  -395,  -395,  -395,  -395,  -395,    67,
      25,  -395,  -395,  -395,  -395,  -395,   100,  -395,  -395,  -395,
    -395,  -395,  -395,    56,  -395,  -395,    24,  -395,  -395,  -395,
    -395,  -395,  3203,   -19,  -157,  -395,   -75,  -395,  -395,  -395,
    -395,  -395,  -395,  -395,  -395,  -111,  -395,  -395,  -106,  -109,
    -395,  -394,   433,   438,   403,   406,   244,  -395,  -395,  -395,
     208,    -3,  -395,  -395,  -395,   102,   482,  -395,  -395,  -395,
    -395,  -395,  -395,  -395,  -395,  -395,   138,  -395,  -395,   122
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    19,   159,     5,    60,   199,   265,   266,
     113,   203,   341,   452,   144,   269,   142,   403,   500,   449,
     450,   280,     6,    77,   129,   258,   259,   260,   261,     7,
       8,    22,     9,    10,    11,    68,    16,    85,    86,    46,
      47,   134,    48,    49,   281,   114,   115,    64,    87,    88,
      89,    14,    61,   130,   282,   350,   351,   204,   283,   410,
     411,   514,   515,   284,   285,   286,   287,   541,   288,   289,
     517,   290,   404,   533,   581,   632,   454,   455,   505,   456,
     548,   291,   405,   537,   582,   459,   460,   461,   510,   551,
     292,   511,   555,   556,   557,   558,   573,   559,   293,   294,
     416,   473,   574,    71,   361,   313,   219,   253,   230,   379,
     225,   223,   236,   117,   118,   119,   370,   369,   120,   121,
     463,   458,   189,   190,   207,   208,   331,   332,   308,   309,
     372,   247,   480,   525,   481,   482,   122,   194,   320,   248,
     123,   124,   125,   139,   400,   446,   447,   126,   127,   475
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     116,    58,   218,   153,    50,    65,   213,   185,    50,   186,
     338,   464,   451,   337,   398,   360,     3,   469,    79,   589,
     242,   146,   147,   148,   187,   470,    84,   145,   157,   138,
    -105,    72,    18,   243,   412,   154,   155,   246,    73,   161,
     162,   185,  -294,   210,   -97,   267,   135,   295,    67,  -370,
    -294,  -294,    20,  -295,   590,   220,  -295,  -370,    66,  -294,
     553,   302,  -295,   201,   160,  -295,  -289,   554,  -294,    81,
      80,  -294,   206,   212,   215,   216,  -294,  -288,  -370,  -295,
    -294,   414,  -295,  -370,   636,  -291,  -292,   143,   221,   222,
    -290,   224,   213,   226,   227,   228,   229,  -294,   231,   232,
     233,   234,   235,   188,   237,   238,   239,   240,   241,   451,
     342,  -104,    -8,   252,   464,  -293,    65,  -104,  -295,   348,
      69,    62,    81,   263,  -364,   444,   -95,  -295,    63,    63,
     296,   305,  -364,  -294,   299,  -365,   303,   -96,  -365,  -295,
     306,   206,  -372,   577,  -295,  -289,    56,  -365,   314,   217,
     315,    21,    24,  -364,    69,   316,  -288,   262,  -364,   212,
     216,   317,    63,   637,  -291,  -292,    54,   318,    53,  -290,
     319,    66,    57,   150,   323,   324,   325,    81,   329,   330,
     507,   151,    67,   508,   339,    70,   244,    25,   191,  -297,
     406,   149,   407,  -362,  -293,   518,   501,   192,  -297,   553,
    -365,    26,  -299,    74,  -300,    75,   554,   568,   569,  -298,
      27,  -299,   245,  -300,   193,  -297,    28,    29,  -298,   262,
      -2,   591,    56,   365,    33,    72,   568,   569,  -299,   592,
    -300,   152,    83,     4,    82,  -298,    38,   593,    39,    55,
     594,   570,    41,   595,    51,   571,   382,   383,   572,   445,
      52,   392,   393,   156,    69,    66,   471,   596,   597,    78,
     570,   157,    63,   562,   571,   132,   158,   584,   335,   478,
     180,   181,   182,   133,   336,   598,   599,   600,   601,   602,
     603,   604,   605,   606,   607,    13,   419,    17,   417,   611,
     168,   418,   128,    17,   140,   420,   421,   520,   365,   462,
     424,   521,   141,   426,   610,    59,   330,   170,   171,   509,
     434,   255,   438,  -108,   439,   440,   353,   502,   136,   256,
     355,   443,    15,   564,   354,   503,   356,   521,    23,   457,
     178,   179,   180,   181,   182,   362,   178,   179,   180,   181,
     182,  -222,    76,   -39,   479,   131,   -39,   484,   445,    25,
     137,   138,   486,   183,   633,   195,   488,   489,   196,   197,
     491,   492,   198,    26,   605,   606,   607,   -74,   214,   297,
     497,   264,    27,   202,   478,   343,   549,   205,    28,    29,
     209,    30,    31,    32,   254,   563,    33,   257,    34,    35,
      36,   268,   512,   513,   298,   453,   453,    37,    38,   519,
      39,    40,   462,   300,    41,    42,   301,    43,    44,   304,
     185,   311,   544,   312,  -360,    45,   603,   604,   605,   606,
     607,   333,   279,   345,   549,  -222,   344,    90,   346,   347,
     352,   535,   349,    91,    92,   357,    93,   358,    94,   359,
     364,    95,   367,   368,   374,    96,    97,   545,    98,   479,
     373,   375,   376,    99,   378,   100,   377,  -361,   381,   457,
     395,   101,   552,   397,   560,   561,   396,   399,   401,    90,
     102,   103,   402,   104,  -118,    91,    92,   105,    93,   408,
      94,  -120,   106,    95,   413,   107,   246,    96,    97,   108,
      98,   415,   583,   423,   362,    99,   422,   100,   427,   453,
     109,   389,   390,   101,   110,   425,  -363,   467,   111,   448,
     465,   112,   102,   103,   466,   104,   468,   391,   472,   105,
     487,    30,    31,    32,   106,   453,   498,   107,    34,    35,
      36,   108,   499,   356,   575,   504,  -157,    37,   506,  -124,
    -126,   516,   109,   431,   432,    42,   110,    43,   522,   594,
     111,   523,   524,   112,    90,   526,   534,   539,   538,   433,
      91,    92,   643,    93,   540,    94,   596,   597,    95,   645,
     575,   542,    96,    97,   565,    98,   576,   566,   580,    72,
      99,   608,   100,   631,   630,   334,    12,   532,   101,   603,
     604,   605,   606,   607,   200,   579,    90,   102,   103,   409,
     104,   550,    91,    92,   105,    93,   609,    94,   536,   106,
      95,   578,   107,   612,    96,    97,   108,    98,   638,   310,
     340,   307,    99,   639,   100,   428,   547,   109,   435,   436,
     101,   110,   485,   642,   211,   111,   531,     0,   112,   102,
     103,   543,   104,     0,   437,     0,   105,     0,     0,     0,
       0,   106,     0,    90,   107,     0,     0,     0,   108,    91,
      92,     0,    93,     0,    94,     0,     0,    95,     0,   109,
       0,    96,    97,   110,    98,     0,     0,   111,     0,    99,
     112,   100,     0,     0,     0,     0,  -334,   101,     0,     0,
       0,     0,     0,     0,     0,    90,   102,   103,     0,   104,
       0,    91,    92,   105,    93,     0,    94,     0,   106,    95,
       0,   107,     0,    96,    97,   108,    98,     0,     0,     0,
       0,    99,     0,   100,     0,     0,   109,     0,     0,   101,
     110,     0,     0,     0,   111,     0,     0,   112,   102,   103,
       0,   104,     0,  -338,     0,   105,     0,     0,     0,     0,
     106,     0,    90,   107,     0,     0,     0,   108,    91,    92,
       0,    93,     0,    94,     0,     0,    95,     0,   109,     0,
      96,    97,   110,    98,     0,     0,   111,     0,   640,   112,
     100,     0,     0,     0,     0,  -336,   641,     0,     0,     0,
       0,     0,     0,     0,     0,   102,   103,     0,   104,     0,
       0,     0,   105,     0,     0,     0,     0,   106,     0,     0,
     107,     0,     0,     0,   108,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,     0,     0,     0,   110,
       0,     0,     0,   111,     0,     0,   112,  -379,   270,     0,
       0,  -379,   271,  -112,    91,    92,    25,    93,   272,    94,
    -112,  -146,    95,     0,     0,     0,    96,    97,     0,    98,
      26,   273,   274,     0,    99,   275,   100,     0,     0,    27,
       0,     0,   101,     0,     0,    28,    29,     0,     0,     0,
       0,   102,   103,    33,   104,     0,     0,     0,   105,     0,
     276,     0,     0,   106,     0,    38,   107,    39,    55,   277,
     108,    41,     0,     0,     0,    44,  -143,     0,     0,     0,
      25,   109,    45,     0,     0,   110,     0,     0,     0,   111,
       0,   278,   112,    90,    26,   279,  -112,     0,    69,    91,
      92,     0,    93,    27,    94,     0,    63,    95,     0,    28,
      29,    96,    97,     0,    98,     0,     0,    33,     0,    99,
       0,   100,     0,     0,     0,     0,     0,   101,     0,    38,
       0,    39,    55,     0,     0,    41,   102,   103,     0,   104,
       0,     0,     0,   105,     0,     0,    45,     0,   106,     0,
      90,   107,     0,     0,     0,   108,    91,    92,     0,    93,
       0,    94,     0,     0,    95,     0,   109,     0,    96,    97,
     110,    98,     0,     0,   111,     0,    99,   112,   100,     0,
       0,  -323,     0,     0,   101,     0,     0,     0,     0,     0,
       0,     0,     0,   102,   103,     0,   104,     0,     0,     0,
     105,     0,     0,     0,     0,   106,     0,     0,   107,     0,
       0,     0,   108,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,     0,     0,     0,   110,     0,     0,
       0,   111,     0,   270,   112,     0,     0,   271,  -325,    91,
      92,    25,    93,   272,    94,     0,  -146,    95,     0,     0,
       0,    96,    97,   -36,    98,    26,   273,   274,     0,    99,
     275,   100,     0,     0,    27,     0,     0,   101,     0,     0,
      28,    29,     0,     0,     0,     0,   102,   103,    33,   104,
       0,     0,     0,   105,     0,   276,     0,     0,   106,     0,
      38,   107,    39,    55,   277,   108,    41,     0,     0,     0,
      44,  -143,     0,     0,     0,     0,   109,    45,     0,     0,
     110,     0,     0,     0,   111,   270,   278,   112,     0,   271,
     279,    91,    92,     0,    93,   272,    94,     0,  -146,    95,
       0,     0,     0,    96,    97,     0,    98,     0,   273,   274,
       0,    99,   275,   100,     0,     0,     0,     0,     0,   101,
       0,     0,     0,     0,     0,     0,     0,     0,   102,   103,
       0,   104,     0,     0,     0,   105,     0,   276,     0,     0,
     106,     0,     0,   107,     0,     0,   277,   108,     0,     0,
       0,    25,     0,  -143,     0,     0,     0,     0,   109,     0,
       0,     0,   110,     0,    90,    26,   111,     0,   278,   112,
      91,    92,   279,    93,    27,    94,     0,     0,    95,     0,
      28,    29,    96,    97,     0,    98,     0,     0,    33,     0,
     640,     0,   100,     0,     0,     0,     0,     0,   641,     0,
      38,     0,    39,    55,     0,     0,    41,   102,   103,     0,
     104,     0,     0,     0,   105,     0,     0,    45,     0,   106,
       0,     0,   107,     0,     0,    90,   108,     0,     0,     0,
       0,    91,    92,     0,    93,     0,    94,   109,     0,    95,
       0,   110,     0,    96,    97,   111,    98,     0,   112,  -328,
       0,   476,     0,   100,     0,     0,     0,     0,     0,   477,
       0,     0,     0,     0,     0,     0,     0,     0,   102,   103,
       0,   104,     0,     0,     0,   105,     0,     0,     0,     0,
     106,     0,     0,   107,     0,     0,   546,   108,     0,     0,
       0,     0,    91,    92,     0,    93,     0,    94,   109,     0,
      95,     0,   110,     0,    96,    97,   111,    98,     0,   112,
    -347,     0,   476,     0,   100,     0,     0,     0,     0,     0,
     477,     0,     0,     0,     0,     0,     0,     0,     0,   102,
     103,     0,   104,     0,     0,     0,   105,     0,     0,     0,
       0,   106,     0,     0,   107,     0,     0,    90,   108,     0,
       0,     0,     0,    91,    92,     0,    93,     0,    94,   109,
       0,    95,     0,   110,     0,    96,    97,   111,    98,     0,
     112,  -351,     0,   640,     0,   100,     0,     0,     0,     0,
       0,   641,     0,     0,     0,     0,     0,     0,     0,     0,
     102,   103,     0,   104,     0,     0,     0,   105,     0,     0,
       0,     0,   106,     0,     0,   107,     0,     0,    90,   108,
       0,     0,     0,     0,    91,    92,    25,    93,     0,    94,
     109,     0,    95,     0,   110,     0,    96,    97,   111,    98,
      26,   112,  -162,     0,    99,     0,   100,     0,     0,    27,
       0,     0,   101,     0,     0,    28,    29,     0,     0,     0,
       0,   102,   103,    33,   104,     0,     0,     0,   105,     0,
       0,     0,     0,   106,     0,    38,   107,    39,    55,     0,
     108,    41,     0,     0,     0,    44,     0,     0,     0,     0,
       0,   109,    45,     0,     0,   110,     0,    90,     0,   111,
       0,  -154,   112,    91,    92,    25,    93,     0,    94,     0,
       0,    95,     0,     0,     0,    96,    97,     0,    98,    26,
       0,     0,     0,    99,     0,   100,     0,     0,    27,     0,
       0,   101,     0,     0,    28,    29,     0,     0,     0,     0,
     102,   103,    33,   104,     0,     0,     0,   105,     0,     0,
       0,     0,   106,     0,    38,   107,    39,    55,     0,   108,
      41,     0,     0,     0,    44,     0,    90,     0,     0,     0,
     109,    45,    91,    92,   110,    93,     0,    94,   111,     0,
      95,   112,     0,     0,    96,    97,     0,    98,     0,     0,
       0,     0,    99,     0,   100,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,     0,     0,     0,     0,   102,
     103,     0,   104,     0,     0,     0,   105,   249,     0,     0,
       0,   106,     0,     0,   107,     0,     0,     0,   108,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
     250,   251,    90,   110,     0,     0,     0,   111,    91,    92,
     112,    93,     0,    94,     0,     0,    95,     0,     0,     0,
      96,    97,     0,    98,     0,     0,     0,     0,    99,     0,
     100,     0,     0,     0,     0,     0,   101,     0,     0,     0,
       0,     0,     0,     0,    90,   102,   103,     0,   104,     0,
      91,    92,   105,    93,     0,    94,     0,   106,    95,     0,
     107,     0,    96,    97,   108,    98,     0,     0,     0,     0,
      99,     0,   100,     0,     0,   109,   321,   322,   101,   110,
       0,     0,     0,   111,     0,     0,   112,   102,   103,     0,
     104,     0,     0,     0,   105,     0,     0,     0,     0,   106,
       0,     0,   107,     0,     0,    90,   108,     0,     0,     0,
       0,    91,    92,     0,    93,     0,    94,   109,     0,    95,
       0,   110,     0,    96,    97,   111,    98,  -141,   112,     0,
       0,    99,     0,   100,     0,     0,     0,     0,     0,   101,
       0,     0,     0,     0,     0,     0,     0,    90,   102,   103,
       0,   104,     0,    91,    92,   105,    93,     0,    94,     0,
     106,    95,     0,   107,     0,    96,    97,   108,    98,     0,
       0,     0,     0,    99,     0,   100,     0,     0,   109,     0,
       0,   101,   110,     0,     0,     0,   111,     0,  -162,   112,
     102,   103,     0,   104,     0,     0,     0,   105,     0,     0,
       0,     0,   106,     0,    90,   107,     0,     0,     0,   108,
      91,    92,     0,    93,     0,    94,     0,     0,    95,     0,
     109,     0,    96,    97,   110,    98,     0,     0,   111,     0,
     640,   112,   100,     0,     0,     0,     0,     0,   641,     0,
       0,     0,     0,     0,     0,     0,     0,   102,   103,     0,
     104,     0,    91,    92,   105,    93,     0,     0,     0,   106,
      95,     0,   107,     0,    96,    97,   108,    98,     0,     0,
       0,     0,   634,     0,     0,     0,     0,   109,     0,     0,
     635,   110,     0,     0,     0,   111,     0,     0,   112,     0,
     103,     0,   104,     0,    91,    92,   105,    93,     0,     0,
       0,   106,    95,     0,   107,     0,    96,    97,   108,    98,
       0,     0,     0,     0,    99,     0,     0,     0,     0,     0,
       0,     0,   101,     0,     0,     0,     0,     0,     0,   163,
     184,     0,   103,     0,   104,     0,     0,   164,   105,     0,
       0,     0,     0,   106,   165,   166,   107,   167,   168,     0,
     108,   169,     0,     0,     0,     0,     0,     0,     0,   163,
       0,     0,     0,   326,     0,   170,   171,   164,     0,     0,
       0,     0,   184,     0,   165,   166,     0,   167,   168,     0,
       0,   169,   172,   173,   174,   175,   176,   177,   178,   179,
     180,   181,   182,   326,     0,   170,   171,     0,     0,   164,
       0,   327,   328,     0,     0,     0,     0,   166,     0,     0,
     168,     0,   172,   173,   174,   175,   176,   177,   178,   179,
     180,   181,   182,     0,     0,   163,     0,   170,   171,     0,
       0,   644,   328,   164,     0,     0,     0,     0,     0,     0,
     165,   166,     0,   167,   168,     0,     0,   169,   176,   177,
     178,   179,   180,   181,   182,   163,     0,     0,     0,   385,
       0,   170,   171,   164,     0,     0,     0,     0,     0,     0,
     165,   166,     0,   167,   168,     0,     0,   169,   172,   173,
     174,   175,   176,   177,   178,   179,   180,   181,   182,   387,
       0,   170,   171,   592,     0,     0,     0,     0,   386,     0,
       0,   593,     0,     0,   594,     0,     0,     0,   172,   173,
     174,   175,   176,   177,   178,   179,   180,   181,   182,   163,
       0,   596,   597,     0,     0,     0,     0,   164,   388,     0,
       0,     0,     0,     0,   165,   166,     0,   167,   168,     0,
       0,   169,   601,   602,   603,   604,   605,   606,   607,   163,
       0,     0,     0,     0,     0,   170,   171,   164,     0,     0,
       0,     0,     0,     0,   165,   166,     0,   167,   168,     0,
       0,   169,   172,   173,   174,   175,   176,   177,   178,   179,
     180,   181,   182,     0,     0,   170,   171,     0,     0,     0,
       0,     0,   384,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   172,   173,   174,   175,   176,   177,   178,   179,
     180,   181,   182,   163,     0,     0,     0,     0,     0,     0,
       0,   164,   429,     0,     0,     0,     0,     0,   165,   166,
       0,   167,   168,     0,     0,   169,     0,     0,     0,     0,
       0,     0,     0,   163,     0,     0,     0,     0,     0,   170,
     171,   164,     0,     0,     0,     0,     0,     0,   165,   166,
       0,   167,   168,     0,     0,   169,   172,   173,   174,   175,
     176,   177,   178,   179,   180,   181,   182,     0,     0,   170,
     171,     0,     0,     0,     0,     0,   430,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   172,   173,   174,   175,
     176,   177,   178,   179,   180,   181,   182,   163,     0,     0,
       0,     0,     0,     0,     0,   164,   441,     0,     0,     0,
       0,     0,   165,   166,     0,   167,   168,     0,     0,   169,
       0,     0,     0,     0,     0,     0,     0,   163,     0,     0,
       0,     0,     0,   170,   171,   164,     0,     0,     0,     0,
       0,     0,   165,   166,     0,   167,   168,     0,     0,   169,
     172,   173,   174,   175,   176,   177,   178,   179,   180,   181,
     182,     0,     0,   170,   171,     0,     0,     0,     0,     0,
     442,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     172,   173,   174,   175,   176,   177,   178,   179,   180,   181,
     182,   163,     0,     0,     0,     0,     0,     0,     0,   164,
     483,     0,     0,     0,     0,     0,   165,   166,     0,   167,
     168,     0,     0,   169,     0,     0,     0,     0,     0,     0,
       0,   163,     0,     0,     0,     0,     0,   170,   171,   164,
       0,     0,     0,     0,     0,     0,   165,   166,     0,   167,
     168,     0,     0,   169,   172,   173,   174,   175,   176,   177,
     178,   179,   180,   181,   182,     0,     0,   170,   171,     0,
       0,     0,     0,     0,   490,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   172,   173,   174,   175,   176,   177,
     178,   179,   180,   181,   182,   163,     0,     0,     0,     0,
       0,     0,     0,   164,   493,     0,     0,     0,     0,     0,
     165,   166,     0,   167,   168,     0,     0,   169,     0,     0,
       0,     0,     0,     0,     0,   163,     0,     0,     0,     0,
       0,   170,   171,   164,     0,     0,     0,     0,     0,     0,
     165,   166,     0,   167,   168,     0,     0,   169,   172,   173,
     174,   175,   176,   177,   178,   179,   180,   181,   182,     0,
       0,   170,   171,     0,     0,     0,     0,     0,   494,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   172,   173,
     174,   175,   176,   177,   178,   179,   180,   181,   182,   163,
       0,     0,     0,     0,     0,     0,     0,   164,   495,     0,
       0,     0,     0,     0,   165,   166,     0,   167,   168,     0,
       0,   169,     0,     0,     0,     0,     0,     0,     0,   163,
       0,     0,     0,     0,     0,   170,   171,   164,     0,     0,
       0,     0,     0,     0,   165,   166,     0,   167,   168,     0,
       0,   169,   172,   173,   174,   175,   176,   177,   178,   179,
     180,   181,   182,     0,     0,   170,   171,     0,     0,     0,
       0,     0,   527,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   172,   173,   174,   175,   176,   177,   178,   179,
     180,   181,   182,   163,     0,     0,     0,     0,     0,     0,
       0,   164,   528,     0,     0,     0,     0,     0,   165,   166,
       0,   167,   168,     0,     0,   169,     0,     0,     0,     0,
       0,     0,     0,   163,     0,     0,     0,     0,     0,   170,
     171,   164,     0,     0,     0,     0,     0,     0,   165,   166,
       0,   167,   168,     0,     0,   169,   172,   173,   174,   175,
     176,   177,   178,   179,   180,   181,   182,     0,     0,   170,
     171,     0,     0,     0,     0,     0,   529,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   172,   173,   174,   175,
     176,   177,   178,   179,   180,   181,   182,   163,     0,     0,
       0,     0,     0,     0,     0,   164,   530,     0,     0,     0,
       0,     0,   165,   166,     0,   167,   168,     0,     0,   169,
       0,     0,     0,     0,     0,     0,     0,   163,     0,     0,
       0,     0,     0,   170,   171,   164,     0,     0,     0,     0,
       0,     0,   165,   166,     0,   167,   168,     0,     0,   169,
     172,   173,   174,   175,   176,   177,   178,   179,   180,   181,
     182,     0,     0,   170,   171,     0,     0,     0,     0,     0,
     646,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     172,   173,   174,   175,   176,   177,   178,   179,   180,   181,
     182,     0,   163,     0,     0,     0,     0,     0,     0,   363,
     164,     0,     0,     0,     0,     0,     0,   165,   166,     0,
     167,   168,     0,     0,   169,     0,     0,     0,     0,     0,
       0,     0,   163,     0,     0,     0,     0,     0,   170,   171,
     164,     0,     0,     0,     0,     0,     0,   165,   166,     0,
     167,   168,     0,     0,   169,   172,   173,   174,   175,   176,
     177,   178,   179,   180,   181,   182,     0,     0,   170,   171,
       0,     0,     0,     0,   366,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   172,   173,   174,   175,   176,
     177,   178,   179,   180,   181,   182,     0,   163,     0,     0,
       0,     0,     0,     0,   474,   164,     0,     0,     0,     0,
       0,     0,   165,   166,     0,   167,   168,     0,     0,   169,
       0,     0,     0,     0,     0,     0,     0,   163,     0,     0,
       0,     0,     0,   170,   171,   164,     0,     0,     0,     0,
       0,     0,   165,   166,     0,   167,   168,     0,     0,   169,
     172,   173,   174,   175,   176,   177,   178,   179,   180,   181,
     182,     0,     0,   170,   171,     0,   371,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    25,     0,
     172,   173,   174,   175,   176,   177,   178,   179,   180,   181,
     182,     0,    26,     0,     0,   380,   -74,     0,     0,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,    25,
      30,    31,    32,     0,     0,    33,     0,    34,    35,    36,
       0,     0,     0,    26,     0,     0,    37,    38,     0,    39,
      55,     0,    27,    41,    42,     0,    43,    44,    28,    29,
       0,    30,    31,    32,    45,     0,    33,     0,    34,    35,
      36,     0,     0,     0,    25,     0,     0,    37,    38,    25,
      39,    55,     0,     0,    41,    42,     0,    43,    26,     0,
       0,     0,   150,    26,     0,    45,     0,    27,     0,     0,
     151,     0,    27,    28,    29,     0,     0,     0,    28,    29,
       0,    33,     0,     0,     0,     0,    33,     0,     0,     0,
       0,     0,     0,    38,     0,    39,    55,     0,    38,    41,
      39,    55,     0,    44,    41,     0,     0,   394,    44,   163,
      45,     0,     0,     0,     0,    45,     0,   164,     0,   496,
       0,   163,     0,     0,   165,   166,     0,   167,   168,   164,
       0,   169,     0,     0,     0,     0,   165,   166,     0,   167,
     168,     0,     0,   169,     0,   170,   171,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   170,   171,     0,
       0,     0,   172,   173,   174,   175,   176,   177,   178,   179,
     180,   181,   182,     0,   172,   173,   174,   175,   176,   177,
     178,   179,   180,   181,   182,   163,     0,     0,     0,     0,
       0,     0,     0,   164,     0,     0,     0,     0,   163,     0,
     165,   166,     0,   167,   168,     0,   164,   169,     0,     0,
       0,     0,     0,   165,   166,     0,   167,   168,     0,   567,
     169,   170,   171,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   170,   171,     0,     0,   172,   173,
     174,   175,   176,   177,   178,   179,   180,   181,   182,     0,
       0,   172,   173,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   163,     0,     0,     0,     0,     0,     0,     0,
     164,     0,   163,     0,     0,     0,     0,   165,   166,     0,
     164,   168,     0,     0,   169,     0,     0,     0,   166,     0,
       0,   168,     0,     0,   169,     0,     0,     0,   170,   171,
       0,     0,     0,     0,     0,     0,     0,     0,   170,   171,
       0,     0,     0,     0,     0,     0,   173,   174,   175,   176,
     177,   178,   179,   180,   181,   182,   173,   174,   175,   176,
     177,   178,   179,   180,   181,   182,   591,     0,     0,     0,
       0,     0,     0,     0,   592,   163,     0,     0,     0,     0,
       0,     0,   593,   164,     0,   594,     0,     0,   595,     0,
       0,   166,     0,     0,   168,     0,     0,   169,     0,     0,
       0,     0,   596,   597,     0,     0,     0,     0,     0,     0,
       0,   170,   171,     0,     0,     0,     0,     0,     0,     0,
     598,   599,   600,   601,   602,   603,   604,   605,   606,   607,
     174,   175,   176,   177,   178,   179,   180,   181,   182,   591,
       0,     0,     0,     0,     0,     0,     0,   592,   163,     0,
       0,     0,     0,     0,     0,   593,   164,     0,   594,     0,
       0,   595,     0,     0,   166,     0,     0,   168,     0,     0,
     169,     0,     0,     0,     0,   596,   597,     0,     0,     0,
       0,     0,     0,     0,   170,   171,     0,     0,     0,     0,
       0,     0,     0,     0,   599,   600,   601,   602,   603,   604,
     605,   606,   607,     0,   175,   176,   177,   178,   179,   180,
     181,   182,   591,     0,     0,     0,     0,     0,     0,   163,
     592,     0,     0,     0,     0,     0,     0,   164,   593,     0,
       0,   594,     0,     0,   595,   166,     0,     0,   168,     0,
       0,   169,     0,     0,     0,     0,     0,     0,   596,   597,
       0,     0,     0,     0,     0,   170,   171,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   600,   601,
     602,   603,   604,   605,   606,   607,   176,   177,   178,   179,
     180,   181,   182,   591,     0,     0,     0,     0,     0,     0,
       0,   592,     0,     0,     0,     0,     0,     0,     0,   593,
       0,     0,   594,     0,     0,   595,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   596,
     597,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   585,     0,   586,   587,   588,     0,     0,     0,     0,
     601,   602,   603,   604,   605,   606,   607,   588,     0,     0,
       0,     0,     0,     0,   613,   614,   615,   616,   617,   618,
     619,   620,   621,   622,   623,   624,   625,   626,   627,   628,
     629
};

static const yytype_int16 yycheck[] =
{
      75,    20,   159,   109,    13,    45,   156,   118,    17,   118,
     266,   405,   402,    27,   336,   293,     0,    27,    66,    52,
      75,   104,   105,   106,     4,    35,    68,   102,    83,     3,
       3,    77,    84,    88,   356,   110,   111,   187,    84,   114,
     115,   152,     3,   152,    86,   202,    86,   204,    79,    10,
       3,    12,    30,     3,    87,    86,     3,    10,    72,    12,
       6,     1,    12,   138,   112,    12,     4,    13,    29,    79,
      27,     3,   147,   156,   157,   158,    29,     4,    10,    29,
      12,   359,    29,    10,     4,     4,     4,    96,   163,   164,
       4,   166,   242,   168,   169,   170,   171,    29,   173,   174,
     175,   176,   177,    83,   179,   180,   181,   182,   183,   499,
      84,    84,    85,   188,   508,     4,   156,    90,     3,   276,
      27,    27,    79,   198,    85,    27,    87,    12,    35,    35,
     205,   214,    85,    86,   209,    85,    76,    87,    85,    86,
     215,   216,    10,    89,    29,    83,    59,    85,   223,   158,
     225,    66,    27,    85,    27,   230,    83,   197,    85,   242,
     243,   236,    35,    83,    83,    83,    27,    59,    84,    83,
     245,    72,    85,    27,   249,   250,   251,    79,   253,   254,
      87,    35,    79,    90,   267,    59,    59,     9,     3,     3,
     347,    59,   349,    85,    83,   473,   452,    12,    12,     6,
      85,    23,     3,    85,     3,     3,    13,    44,    45,     3,
      32,    12,    85,    12,    29,    29,    38,    39,    12,   259,
       0,    18,    59,   298,    46,    77,    44,    45,    29,    26,
      29,    85,    59,    13,    86,    29,    58,    34,    60,    61,
      37,    78,    64,    40,    84,    82,   321,   322,    85,   399,
      90,   326,   327,    75,    27,    72,   413,    54,    55,    76,
      78,    83,    35,   541,    82,    27,    88,    85,    84,   419,
      79,    80,    81,    35,    90,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,     2,   369,     4,   363,    86,
      37,   366,    85,    10,    27,   370,   371,    86,   373,   405,
     375,    90,    35,   378,   582,    22,   381,    54,    55,   459,
     385,    27,   387,     3,   389,   390,    27,    27,    86,    35,
      84,   396,     4,    86,    35,    35,    90,    90,    10,   404,
      77,    78,    79,    80,    81,    84,    77,    78,    79,    80,
      81,    90,    84,    85,   419,    59,    88,   422,   498,     9,
      90,     3,   427,     3,   632,    10,   431,   432,    27,    88,
     435,   436,     3,    23,    79,    80,    81,    27,    87,    89,
     445,    84,    32,    85,   524,    84,   533,    85,    38,    39,
      85,    41,    42,    43,    85,   542,    46,    86,    48,    49,
      50,    85,   467,   468,    90,   404,   405,    57,    58,   474,
      60,    61,   508,    86,    64,    65,    86,    67,    68,    76,
     521,    89,   521,    86,    85,    75,    77,    78,    79,    80,
      81,    89,    88,    85,   581,    90,    84,     1,    85,    85,
      21,   506,    85,     7,     8,    84,    10,    69,    12,    14,
      86,    15,    86,    86,    91,    19,    20,   522,    22,   524,
      90,    90,    89,    27,    90,    29,    86,    85,    85,   534,
      86,    35,   537,    84,   539,   540,    90,    84,    86,     1,
      44,    45,    87,    47,     3,     7,     8,    51,    10,    89,
      12,     3,    56,    15,    85,    59,   636,    19,    20,    63,
      22,    17,   567,    86,    84,    27,    84,    29,    87,   508,
      74,    75,    76,    35,    78,    86,    85,     3,    82,    86,
      86,    85,    44,    45,    86,    47,     3,    91,    69,    51,
      86,    41,    42,    43,    56,   534,    90,    59,    48,    49,
      50,    63,    84,    90,   553,    84,    90,    57,     3,     3,
       3,    86,    74,    75,    76,    65,    78,    67,    87,    37,
      82,    86,    90,    85,     1,    86,    90,     3,    88,    91,
       7,     8,   637,    10,     3,    12,    54,    55,    15,   644,
     589,    85,    19,    20,    84,    22,    87,    86,    86,    77,
      27,    84,    29,    87,    86,   259,     2,   499,    35,    77,
      78,    79,    80,    81,   137,   557,     1,    44,    45,   351,
      47,   534,     7,     8,    51,    10,   581,    12,   508,    56,
      15,   555,    59,   589,    19,    20,    63,    22,   185,   216,
     268,   215,    27,   185,    29,   381,   524,    74,    75,    76,
      35,    78,   424,   636,   152,    82,   498,    -1,    85,    44,
      45,   519,    47,    -1,    91,    -1,    51,    -1,    -1,    -1,
      -1,    56,    -1,     1,    59,    -1,    -1,    -1,    63,     7,
       8,    -1,    10,    -1,    12,    -1,    -1,    15,    -1,    74,
      -1,    19,    20,    78,    22,    -1,    -1,    82,    -1,    27,
      85,    29,    -1,    -1,    -1,    -1,    91,    35,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    44,    45,    -1,    47,
      -1,     7,     8,    51,    10,    -1,    12,    -1,    56,    15,
      -1,    59,    -1,    19,    20,    63,    22,    -1,    -1,    -1,
      -1,    27,    -1,    29,    -1,    -1,    74,    -1,    -1,    35,
      78,    -1,    -1,    -1,    82,    -1,    -1,    85,    44,    45,
      -1,    47,    -1,    91,    -1,    51,    -1,    -1,    -1,    -1,
      56,    -1,     1,    59,    -1,    -1,    -1,    63,     7,     8,
      -1,    10,    -1,    12,    -1,    -1,    15,    -1,    74,    -1,
      19,    20,    78,    22,    -1,    -1,    82,    -1,    27,    85,
      29,    -1,    -1,    -1,    -1,    91,    35,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    44,    45,    -1,    47,    -1,
      -1,    -1,    51,    -1,    -1,    -1,    -1,    56,    -1,    -1,
      59,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    78,
      -1,    -1,    -1,    82,    -1,    -1,    85,    86,     1,    -1,
      -1,    90,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    -1,    -1,    -1,    19,    20,    -1,    22,
      23,    24,    25,    -1,    27,    28,    29,    -1,    -1,    32,
      -1,    -1,    35,    -1,    -1,    38,    39,    -1,    -1,    -1,
      -1,    44,    45,    46,    47,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    56,    -1,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    68,    69,    -1,    -1,    -1,
       9,    74,    75,    -1,    -1,    78,    -1,    -1,    -1,    82,
      -1,    84,    85,     1,    23,    88,    89,    -1,    27,     7,
       8,    -1,    10,    32,    12,    -1,    35,    15,    -1,    38,
      39,    19,    20,    -1,    22,    -1,    -1,    46,    -1,    27,
      -1,    29,    -1,    -1,    -1,    -1,    -1,    35,    -1,    58,
      -1,    60,    61,    -1,    -1,    64,    44,    45,    -1,    47,
      -1,    -1,    -1,    51,    -1,    -1,    75,    -1,    56,    -1,
       1,    59,    -1,    -1,    -1,    63,     7,     8,    -1,    10,
      -1,    12,    -1,    -1,    15,    -1,    74,    -1,    19,    20,
      78,    22,    -1,    -1,    82,    -1,    27,    85,    29,    -1,
      -1,    89,    -1,    -1,    35,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    44,    45,    -1,    47,    -1,    -1,    -1,
      51,    -1,    -1,    -1,    -1,    56,    -1,    -1,    59,    -1,
      -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    74,    -1,    -1,    -1,    78,    -1,    -1,
      -1,    82,    -1,     1,    85,    -1,    -1,     5,    89,     7,
       8,     9,    10,    11,    12,    -1,    14,    15,    -1,    -1,
      -1,    19,    20,    21,    22,    23,    24,    25,    -1,    27,
      28,    29,    -1,    -1,    32,    -1,    -1,    35,    -1,    -1,
      38,    39,    -1,    -1,    -1,    -1,    44,    45,    46,    47,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    56,    -1,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      68,    69,    -1,    -1,    -1,    -1,    74,    75,    -1,    -1,
      78,    -1,    -1,    -1,    82,     1,    84,    85,    -1,     5,
      88,     7,     8,    -1,    10,    11,    12,    -1,    14,    15,
      -1,    -1,    -1,    19,    20,    -1,    22,    -1,    24,    25,
      -1,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,    35,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,    45,
      -1,    47,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      56,    -1,    -1,    59,    -1,    -1,    62,    63,    -1,    -1,
      -1,     9,    -1,    69,    -1,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    78,    -1,     1,    23,    82,    -1,    84,    85,
       7,     8,    88,    10,    32,    12,    -1,    -1,    15,    -1,
      38,    39,    19,    20,    -1,    22,    -1,    -1,    46,    -1,
      27,    -1,    29,    -1,    -1,    -1,    -1,    -1,    35,    -1,
      58,    -1,    60,    61,    -1,    -1,    64,    44,    45,    -1,
      47,    -1,    -1,    -1,    51,    -1,    -1,    75,    -1,    56,
      -1,    -1,    59,    -1,    -1,     1,    63,    -1,    -1,    -1,
      -1,     7,     8,    -1,    10,    -1,    12,    74,    -1,    15,
      -1,    78,    -1,    19,    20,    82,    22,    -1,    85,    86,
      -1,    27,    -1,    29,    -1,    -1,    -1,    -1,    -1,    35,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,    45,
      -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    -1,    -1,
      56,    -1,    -1,    59,    -1,    -1,     1,    63,    -1,    -1,
      -1,    -1,     7,     8,    -1,    10,    -1,    12,    74,    -1,
      15,    -1,    78,    -1,    19,    20,    82,    22,    -1,    85,
      86,    -1,    27,    -1,    29,    -1,    -1,    -1,    -1,    -1,
      35,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,
      45,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,    -1,
      -1,    56,    -1,    -1,    59,    -1,    -1,     1,    63,    -1,
      -1,    -1,    -1,     7,     8,    -1,    10,    -1,    12,    74,
      -1,    15,    -1,    78,    -1,    19,    20,    82,    22,    -1,
      85,    86,    -1,    27,    -1,    29,    -1,    -1,    -1,    -1,
      -1,    35,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      44,    45,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,
      -1,    -1,    56,    -1,    -1,    59,    -1,    -1,     1,    63,
      -1,    -1,    -1,    -1,     7,     8,     9,    10,    -1,    12,
      74,    -1,    15,    -1,    78,    -1,    19,    20,    82,    22,
      23,    85,    86,    -1,    27,    -1,    29,    -1,    -1,    32,
      -1,    -1,    35,    -1,    -1,    38,    39,    -1,    -1,    -1,
      -1,    44,    45,    46,    47,    -1,    -1,    -1,    51,    -1,
      -1,    -1,    -1,    56,    -1,    58,    59,    60,    61,    -1,
      63,    64,    -1,    -1,    -1,    68,    -1,    -1,    -1,    -1,
      -1,    74,    75,    -1,    -1,    78,    -1,     1,    -1,    82,
      -1,    84,    85,     7,     8,     9,    10,    -1,    12,    -1,
      -1,    15,    -1,    -1,    -1,    19,    20,    -1,    22,    23,
      -1,    -1,    -1,    27,    -1,    29,    -1,    -1,    32,    -1,
      -1,    35,    -1,    -1,    38,    39,    -1,    -1,    -1,    -1,
      44,    45,    46,    47,    -1,    -1,    -1,    51,    -1,    -1,
      -1,    -1,    56,    -1,    58,    59,    60,    61,    -1,    63,
      64,    -1,    -1,    -1,    68,    -1,     1,    -1,    -1,    -1,
      74,    75,     7,     8,    78,    10,    -1,    12,    82,    -1,
      15,    85,    -1,    -1,    19,    20,    -1,    22,    -1,    -1,
      -1,    -1,    27,    -1,    29,    -1,    -1,    -1,    -1,    -1,
      35,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,
      45,    -1,    47,    -1,    -1,    -1,    51,    52,    -1,    -1,
      -1,    56,    -1,    -1,    59,    -1,    -1,    -1,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      75,    76,     1,    78,    -1,    -1,    -1,    82,     7,     8,
      85,    10,    -1,    12,    -1,    -1,    15,    -1,    -1,    -1,
      19,    20,    -1,    22,    -1,    -1,    -1,    -1,    27,    -1,
      29,    -1,    -1,    -1,    -1,    -1,    35,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    44,    45,    -1,    47,    -1,
       7,     8,    51,    10,    -1,    12,    -1,    56,    15,    -1,
      59,    -1,    19,    20,    63,    22,    -1,    -1,    -1,    -1,
      27,    -1,    29,    -1,    -1,    74,    75,    76,    35,    78,
      -1,    -1,    -1,    82,    -1,    -1,    85,    44,    45,    -1,
      47,    -1,    -1,    -1,    51,    -1,    -1,    -1,    -1,    56,
      -1,    -1,    59,    -1,    -1,     1,    63,    -1,    -1,    -1,
      -1,     7,     8,    -1,    10,    -1,    12,    74,    -1,    15,
      -1,    78,    -1,    19,    20,    82,    22,    84,    85,    -1,
      -1,    27,    -1,    29,    -1,    -1,    -1,    -1,    -1,    35,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    44,    45,
      -1,    47,    -1,     7,     8,    51,    10,    -1,    12,    -1,
      56,    15,    -1,    59,    -1,    19,    20,    63,    22,    -1,
      -1,    -1,    -1,    27,    -1,    29,    -1,    -1,    74,    -1,
      -1,    35,    78,    -1,    -1,    -1,    82,    -1,    84,    85,
      44,    45,    -1,    47,    -1,    -1,    -1,    51,    -1,    -1,
      -1,    -1,    56,    -1,     1,    59,    -1,    -1,    -1,    63,
       7,     8,    -1,    10,    -1,    12,    -1,    -1,    15,    -1,
      74,    -1,    19,    20,    78,    22,    -1,    -1,    82,    -1,
      27,    85,    29,    -1,    -1,    -1,    -1,    -1,    35,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,    45,    -1,
      47,    -1,     7,     8,    51,    10,    -1,    -1,    -1,    56,
      15,    -1,    59,    -1,    19,    20,    63,    22,    -1,    -1,
      -1,    -1,    27,    -1,    -1,    -1,    -1,    74,    -1,    -1,
      35,    78,    -1,    -1,    -1,    82,    -1,    -1,    85,    -1,
      45,    -1,    47,    -1,     7,     8,    51,    10,    -1,    -1,
      -1,    56,    15,    -1,    59,    -1,    19,    20,    63,    22,
      -1,    -1,    -1,    -1,    27,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    35,    -1,    -1,    -1,    -1,    -1,    -1,    18,
      85,    -1,    45,    -1,    47,    -1,    -1,    26,    51,    -1,
      -1,    -1,    -1,    56,    33,    34,    59,    36,    37,    -1,
      63,    40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,
      -1,    -1,    -1,    52,    -1,    54,    55,    26,    -1,    -1,
      -1,    -1,    85,    -1,    33,    34,    -1,    36,    37,    -1,
      -1,    40,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    52,    -1,    54,    55,    -1,    -1,    26,
      -1,    90,    91,    -1,    -1,    -1,    -1,    34,    -1,    -1,
      37,    -1,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    -1,    -1,    18,    -1,    54,    55,    -1,
      -1,    90,    91,    26,    -1,    -1,    -1,    -1,    -1,    -1,
      33,    34,    -1,    36,    37,    -1,    -1,    40,    75,    76,
      77,    78,    79,    80,    81,    18,    -1,    -1,    -1,    52,
      -1,    54,    55,    26,    -1,    -1,    -1,    -1,    -1,    -1,
      33,    34,    -1,    36,    37,    -1,    -1,    40,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    52,
      -1,    54,    55,    26,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    34,    -1,    -1,    37,    -1,    -1,    -1,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    18,
      -1,    54,    55,    -1,    -1,    -1,    -1,    26,    91,    -1,
      -1,    -1,    -1,    -1,    33,    34,    -1,    36,    37,    -1,
      -1,    40,    75,    76,    77,    78,    79,    80,    81,    18,
      -1,    -1,    -1,    -1,    -1,    54,    55,    26,    -1,    -1,
      -1,    -1,    -1,    -1,    33,    34,    -1,    36,    37,    -1,
      -1,    40,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    -1,    -1,    54,    55,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    26,    91,    -1,    -1,    -1,    -1,    -1,    33,    34,
      -1,    36,    37,    -1,    -1,    40,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    54,
      55,    26,    -1,    -1,    -1,    -1,    -1,    -1,    33,    34,
      -1,    36,    37,    -1,    -1,    40,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    -1,    -1,    54,
      55,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    26,    91,    -1,    -1,    -1,
      -1,    -1,    33,    34,    -1,    36,    37,    -1,    -1,    40,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    54,    55,    26,    -1,    -1,    -1,    -1,
      -1,    -1,    33,    34,    -1,    36,    37,    -1,    -1,    40,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    -1,    -1,    54,    55,    -1,    -1,    -1,    -1,    -1,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    26,
      91,    -1,    -1,    -1,    -1,    -1,    33,    34,    -1,    36,
      37,    -1,    -1,    40,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    18,    -1,    -1,    -1,    -1,    -1,    54,    55,    26,
      -1,    -1,    -1,    -1,    -1,    -1,    33,    34,    -1,    36,
      37,    -1,    -1,    40,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    -1,    -1,    54,    55,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    26,    91,    -1,    -1,    -1,    -1,    -1,
      33,    34,    -1,    36,    37,    -1,    -1,    40,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,
      -1,    54,    55,    26,    -1,    -1,    -1,    -1,    -1,    -1,
      33,    34,    -1,    36,    37,    -1,    -1,    40,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    -1,
      -1,    54,    55,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    18,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    26,    91,    -1,
      -1,    -1,    -1,    -1,    33,    34,    -1,    36,    37,    -1,
      -1,    40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,
      -1,    -1,    -1,    -1,    -1,    54,    55,    26,    -1,    -1,
      -1,    -1,    -1,    -1,    33,    34,    -1,    36,    37,    -1,
      -1,    40,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    -1,    -1,    54,    55,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    26,    91,    -1,    -1,    -1,    -1,    -1,    33,    34,
      -1,    36,    37,    -1,    -1,    40,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    54,
      55,    26,    -1,    -1,    -1,    -1,    -1,    -1,    33,    34,
      -1,    36,    37,    -1,    -1,    40,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    -1,    -1,    54,
      55,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    26,    91,    -1,    -1,    -1,
      -1,    -1,    33,    34,    -1,    36,    37,    -1,    -1,    40,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    54,    55,    26,    -1,    -1,    -1,    -1,
      -1,    -1,    33,    34,    -1,    36,    37,    -1,    -1,    40,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    -1,    -1,    54,    55,    -1,    -1,    -1,    -1,    -1,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      26,    -1,    -1,    -1,    -1,    -1,    -1,    33,    34,    -1,
      36,    37,    -1,    -1,    40,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    54,    55,
      26,    -1,    -1,    -1,    -1,    -1,    -1,    33,    34,    -1,
      36,    37,    -1,    -1,    40,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    -1,    -1,    54,    55,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    26,    -1,    -1,    -1,    -1,
      -1,    -1,    33,    34,    -1,    36,    37,    -1,    -1,    40,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    54,    55,    26,    -1,    -1,    -1,    -1,
      -1,    -1,    33,    34,    -1,    36,    37,    -1,    -1,    40,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    -1,    -1,    54,    55,    -1,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     9,    -1,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    -1,    23,    -1,    -1,    86,    27,    -1,    -1,    -1,
      -1,    32,    -1,    -1,    -1,    -1,    -1,    38,    39,     9,
      41,    42,    43,    -1,    -1,    46,    -1,    48,    49,    50,
      -1,    -1,    -1,    23,    -1,    -1,    57,    58,    -1,    60,
      61,    -1,    32,    64,    65,    -1,    67,    68,    38,    39,
      -1,    41,    42,    43,    75,    -1,    46,    -1,    48,    49,
      50,    -1,    -1,    -1,     9,    -1,    -1,    57,    58,     9,
      60,    61,    -1,    -1,    64,    65,    -1,    67,    23,    -1,
      -1,    -1,    27,    23,    -1,    75,    -1,    32,    -1,    -1,
      35,    -1,    32,    38,    39,    -1,    -1,    -1,    38,    39,
      -1,    46,    -1,    -1,    -1,    -1,    46,    -1,    -1,    -1,
      -1,    -1,    -1,    58,    -1,    60,    61,    -1,    58,    64,
      60,    61,    -1,    68,    64,    -1,    -1,    16,    68,    18,
      75,    -1,    -1,    -1,    -1,    75,    -1,    26,    -1,    16,
      -1,    18,    -1,    -1,    33,    34,    -1,    36,    37,    26,
      -1,    40,    -1,    -1,    -1,    -1,    33,    34,    -1,    36,
      37,    -1,    -1,    40,    -1,    54,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    54,    55,    -1,
      -1,    -1,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    -1,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    26,    -1,    -1,    -1,    -1,    18,    -1,
      33,    34,    -1,    36,    37,    -1,    26,    40,    -1,    -1,
      -1,    -1,    -1,    33,    34,    -1,    36,    37,    -1,    52,
      40,    54,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    54,    55,    -1,    -1,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    -1,
      -1,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      26,    -1,    18,    -1,    -1,    -1,    -1,    33,    34,    -1,
      26,    37,    -1,    -1,    40,    -1,    -1,    -1,    34,    -1,
      -1,    37,    -1,    -1,    40,    -1,    -1,    -1,    54,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    54,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    26,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    34,    26,    -1,    37,    -1,    -1,    40,    -1,
      -1,    34,    -1,    -1,    37,    -1,    -1,    40,    -1,    -1,
      -1,    -1,    54,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    54,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    18,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    26,    18,    -1,
      -1,    -1,    -1,    -1,    -1,    34,    26,    -1,    37,    -1,
      -1,    40,    -1,    -1,    34,    -1,    -1,    37,    -1,    -1,
      40,    -1,    -1,    -1,    -1,    54,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    54,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    -1,    74,    75,    76,    77,    78,    79,
      80,    81,    18,    -1,    -1,    -1,    -1,    -1,    -1,    18,
      26,    -1,    -1,    -1,    -1,    -1,    -1,    26,    34,    -1,
      -1,    37,    -1,    -1,    40,    34,    -1,    -1,    37,    -1,
      -1,    40,    -1,    -1,    -1,    -1,    -1,    -1,    54,    55,
      -1,    -1,    -1,    -1,    -1,    54,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    75,
      76,    77,    78,    79,    80,    81,    75,    76,    77,    78,
      79,    80,    81,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    34,
      -1,    -1,    37,    -1,    -1,    40,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    54,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   568,    -1,   570,   571,   572,    -1,    -1,    -1,    -1,
      75,    76,    77,    78,    79,    80,    81,   584,    -1,    -1,
      -1,    -1,    -1,    -1,   591,   592,   593,   594,   595,   596,
     597,   598,   599,   600,   601,   602,   603,   604,   605,   606,
     607
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    93,    94,     0,    13,    97,   114,   121,   122,   124,
     125,   126,   128,   130,   143,   122,   128,   130,    84,    95,
      30,    66,   123,   122,    27,     9,    23,    32,    38,    39,
      41,    42,    43,    46,    48,    49,    50,    57,    58,    60,
      61,    64,    65,    67,    68,    75,   131,   132,   134,   135,
     136,    84,    90,    84,    27,    61,    59,    85,   195,   130,
      98,   144,    27,    35,   139,   134,    72,    79,   127,    27,
      59,   195,    77,    84,    85,     3,    84,   115,    76,   135,
      27,    79,    86,    59,    68,   129,   130,   140,   141,   142,
       1,     7,     8,    10,    12,    15,    19,    20,    22,    27,
      29,    35,    44,    45,    47,    51,    56,    59,    63,    74,
      78,    82,    85,   102,   137,   138,   198,   205,   206,   207,
     210,   211,   228,   232,   233,   234,   239,   240,    85,   116,
     145,    59,    27,    35,   133,   134,    86,    90,     3,   235,
      27,    35,   108,   136,   106,   198,    96,    96,    96,    59,
      27,    35,    85,   210,   198,   198,    75,    83,    88,    96,
     135,   198,   198,    18,    26,    33,    34,    36,    37,    40,
      54,    55,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,     3,    85,   207,   211,     4,    83,   214,
     215,     3,    12,    29,   229,    10,    27,    88,     3,    99,
     142,   198,    85,   103,   149,    85,   198,   216,   217,    85,
     211,   228,    96,   139,    87,    96,    96,   136,   196,   198,
      86,   198,   198,   203,   198,   202,   198,   198,   198,   198,
     200,   198,   198,   198,   198,   198,   204,   198,   198,   198,
     198,   198,    75,    88,    59,    85,   139,   223,   231,    52,
      75,    76,   198,   199,    85,    27,    35,    86,   117,   118,
     119,   120,   134,   198,    84,   100,   101,   196,    85,   107,
       1,     5,    11,    24,    25,    28,    53,    62,    84,    88,
     113,   136,   146,   150,   155,   156,   157,   158,   160,   161,
     163,   173,   182,   190,   191,   196,   198,    89,    90,   198,
      86,    86,     1,    76,    76,    96,   198,   217,   220,   221,
     216,    89,    86,   197,   198,   198,   198,   198,    59,   198,
     230,    75,    76,   198,   198,   198,    52,    90,    91,   198,
     198,   218,   219,    89,   119,    84,    90,    27,   146,    96,
     140,   104,    84,    84,    84,    85,    85,    85,   196,    85,
     147,   148,    21,    27,    35,    84,    90,    84,    69,    14,
     155,   196,    84,    90,    86,   198,    90,    86,    86,   209,
     208,    87,   222,    90,    91,    90,    89,    86,    90,   201,
      86,    85,   198,   198,    91,    52,    91,    52,    91,    75,
      76,    91,   198,   198,    16,    86,    90,    84,   127,    84,
     236,    86,    87,   109,   164,   174,   196,   196,    89,   149,
     151,   152,   127,    85,   155,    17,   192,   198,   198,    96,
     198,   198,    84,    86,   198,    86,   198,    87,   218,    91,
      91,    75,    76,    91,   198,    75,    76,    91,   198,   198,
     198,    91,    91,   198,    27,   139,   237,   238,    86,   111,
     112,   150,   105,   136,   168,   169,   171,   198,   213,   177,
     178,   179,   210,   212,   213,    86,    86,     3,     3,    27,
      35,   196,    69,   193,    90,   241,    27,    35,   139,   198,
     224,   226,   227,    91,   198,   222,   198,    86,   198,   198,
      91,   198,   198,    91,    91,    91,    16,   198,    90,    84,
     110,   146,    27,    35,    84,   170,     3,    87,    90,   139,
     180,   183,   198,   198,   153,   154,    86,   162,   155,   198,
      86,    90,    87,    86,    90,   225,    86,    91,    91,    91,
      91,   238,   112,   165,    90,   198,   178,   175,    88,     3,
       3,   159,    85,   241,   211,   198,     1,   227,   172,   196,
     171,   181,   198,     6,    13,   184,   185,   186,   187,   189,
     198,   198,   155,   196,    86,    84,    86,    52,    44,    45,
      78,    82,    85,   188,   194,   195,    87,    89,   185,   147,
      86,   166,   176,   198,    85,   194,   194,   194,   194,    52,
      87,    18,    26,    34,    37,    40,    54,    55,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    84,   172,
     155,    86,   188,   194,   194,   194,   194,   194,   194,   194,
     194,   194,   194,   194,   194,   194,   194,   194,   194,   194,
      86,    87,   167,   155,    27,    35,     4,    83,   214,   215,
      27,    35,   223,   198,    90,   198,    91
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    92,    93,    94,    94,    95,    95,    96,    98,    99,
      97,    97,    97,    97,    97,   101,   100,   100,   103,   104,
     105,   102,   106,   102,   107,   107,   108,   108,   109,   109,
     110,   110,   111,   111,   111,   112,   113,   113,   114,   115,
     114,   116,   116,   117,   117,   118,   118,   119,   120,   120,
     121,   122,   122,   123,   124,   124,   125,   126,   127,   127,
     128,   129,   130,   130,   131,   131,   131,   131,   131,   131,
     131,   131,   131,   132,   132,   133,   133,   134,   134,   135,
     135,   135,   135,   135,   135,   135,   135,   135,   135,   135,
     135,   136,   136,   137,   138,   139,   139,   140,   140,   140,
     141,   141,   142,   142,   143,   144,   143,   143,   145,   143,
     146,   148,   147,   149,   149,   149,   150,   150,   151,   150,
     152,   150,   150,   150,   153,   150,   154,   150,   155,   155,
     155,   155,   155,   155,   155,   155,   155,   155,   155,   155,
     155,   156,   156,   158,   159,   157,   161,   162,   160,   164,
     165,   166,   167,   163,   168,   168,   169,   170,   169,   171,
     171,   171,   172,   172,   174,   175,   176,   173,   177,   177,
     178,   179,   179,   180,   180,   181,   181,   183,   182,   184,
     184,   185,   186,   186,   187,   187,   188,   188,   189,   190,
     191,   192,   193,   192,   194,   194,   194,   194,   194,   194,
     194,   194,   194,   194,   194,   194,   194,   194,   194,   194,
     194,   194,   194,   194,   194,   194,   195,   195,   195,   195,
     195,   196,   197,   196,   199,   198,   198,   200,   201,   198,
     202,   198,   203,   198,   198,   198,   198,   198,   198,   198,
     198,   198,   198,   198,   198,   198,   204,   198,   198,   198,
     198,   198,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   198,   198,   205,   205,   205,   206,   206,   207,   207,
     207,   207,   207,   207,   207,   207,   207,   207,   207,   207,
     207,   208,   207,   207,   207,   207,   209,   207,   207,   207,
     207,   207,   207,   207,   210,   210,   211,   211,   211,   211,
     211,   211,   212,   213,   213,   214,   214,   214,   215,   215,
     215,   215,   215,   215,   215,   215,   215,   215,   215,   215,
     215,   215,   215,   216,   216,   216,   217,   217,   218,   218,
     219,   219,   219,   219,   220,   220,   220,   220,   220,   221,
     221,   222,   222,   223,   223,   223,   223,   224,   224,   224,
     225,   225,   226,   226,   227,   227,   229,   228,   230,   228,
     231,   231,   231,   231,   232,   232,   232,   232,   232,   232,
     233,   233,   233,   235,   234,   236,   236,   237,   237,   238,
     238,   239,   240,   241,   241
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     3,     0,     0,     1,     0,     0,     0,
       8,     2,     1,     1,     1,     0,     2,     1,     0,     0,
       0,     8,     0,     5,     0,     3,     0,     1,     0,     3,
       0,     1,     0,     1,     3,     1,     0,     1,     4,     0,
       9,     0,     3,     0,     1,     1,     2,     2,     2,     4,
       4,     1,     2,     1,     1,     3,     2,     3,     0,     2,
       2,     2,     0,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     1,     0,     1,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     2,
       3,     1,     1,     3,     5,     1,     1,     0,     1,     1,
       1,     3,     2,     2,     2,     0,     5,     4,     0,     7,
       3,     0,     2,     0,     3,     2,     2,     2,     0,     5,
       0,     5,     4,     4,     0,     7,     0,     7,     2,     2,
       1,     1,     1,     1,     1,     1,     2,     1,     1,     2,
       2,     1,     2,     0,     0,     7,     0,     0,     9,     0,
       0,     0,     0,    13,     0,     1,     1,     0,     4,     1,
       3,     1,     0,     1,     0,     0,     0,    10,     1,     3,
       1,     1,     1,     1,     1,     1,     3,     0,     8,     2,
       1,     2,     1,     1,     3,     5,     1,     1,     2,     4,
       3,     0,     0,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     2,     2,     1,     1,     3,     2,     4,
       3,     1,     0,     4,     0,     4,     3,     0,     0,     7,
       0,     4,     0,     4,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     0,     4,     3,     3,
       3,     3,     2,     2,     2,     2,     2,     2,     2,     2,
       1,     3,     1,     2,     4,     4,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     6,
       5,     0,     8,     6,     5,     5,     0,     8,     1,     1,
       3,     2,     2,     6,     1,     1,     1,     2,     6,     2,
       3,     1,     1,     2,     2,     3,     4,     4,     4,     5,
       5,     5,     6,     6,     7,     6,     6,     7,     7,     7,
       4,     5,     5,     0,     1,     2,     1,     3,     0,     1,
       1,     2,     3,     4,     0,     1,     2,     1,     2,     2,
       4,     2,     3,     1,     2,     1,     3,     0,     2,     3,
       0,     1,     1,     3,     3,     1,     0,     5,     0,     7,
       1,     2,     1,     3,     1,     1,     2,     2,     3,     3,
       1,     2,     1,     0,     7,     2,     0,     3,     1,     1,
       2,     8,    10,     0,     3
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  switch (yytype)
    {
          case 102: /* inline_func  */
#line 6923 "prolang.y" /* yacc.c:1257  */
      { free_fulltype(((*yyvaluep).rvalue).type); }
#line 9499 "y.tab.c" /* yacc.c:1257  */
        break;

    case 108: /* inline_opt_type  */
#line 6920 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lpctype));       }
#line 9505 "y.tab.c" /* yacc.c:1257  */
        break;

    case 120: /* member_name_list  */
#line 6920 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lpctype));       }
#line 9511 "y.tab.c" /* yacc.c:1257  */
        break;

    case 128: /* type  */
#line 6921 "prolang.y" /* yacc.c:1257  */
      { free_fulltype(((*yyvaluep).fulltype));      }
#line 9517 "y.tab.c" /* yacc.c:1257  */
        break;

    case 129: /* non_void_type  */
#line 6921 "prolang.y" /* yacc.c:1257  */
      { free_fulltype(((*yyvaluep).fulltype));      }
#line 9523 "y.tab.c" /* yacc.c:1257  */
        break;

    case 132: /* opt_basic_type  */
#line 6920 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lpctype));       }
#line 9529 "y.tab.c" /* yacc.c:1257  */
        break;

    case 133: /* opt_basic_non_void_type  */
#line 6920 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lpctype));       }
#line 9535 "y.tab.c" /* yacc.c:1257  */
        break;

    case 134: /* basic_non_void_type  */
#line 6920 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lpctype));       }
#line 9541 "y.tab.c" /* yacc.c:1257  */
        break;

    case 135: /* single_basic_non_void_type  */
#line 6920 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lpctype));       }
#line 9547 "y.tab.c" /* yacc.c:1257  */
        break;

    case 136: /* basic_type  */
#line 6920 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lpctype));       }
#line 9553 "y.tab.c" /* yacc.c:1257  */
        break;

    case 137: /* cast  */
#line 6920 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lpctype));       }
#line 9559 "y.tab.c" /* yacc.c:1257  */
        break;

    case 138: /* decl_cast  */
#line 6920 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lpctype));       }
#line 9565 "y.tab.c" /* yacc.c:1257  */
        break;

    case 143: /* name_list  */
#line 6921 "prolang.y" /* yacc.c:1257  */
      { free_fulltype(((*yyvaluep).fulltype));      }
#line 9571 "y.tab.c" /* yacc.c:1257  */
        break;

    case 150: /* local_name_list  */
#line 6920 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lpctype));       }
#line 9577 "y.tab.c" /* yacc.c:1257  */
        break;

    case 179: /* foreach_var_lvalue  */
#line 6922 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lvalue).type);  }
#line 9583 "y.tab.c" /* yacc.c:1257  */
        break;

    case 196: /* comma_expr  */
#line 6923 "prolang.y" /* yacc.c:1257  */
      { free_fulltype(((*yyvaluep).rvalue).type); }
#line 9589 "y.tab.c" /* yacc.c:1257  */
        break;

    case 198: /* expr0  */
#line 6923 "prolang.y" /* yacc.c:1257  */
      { free_fulltype(((*yyvaluep).rvalue).type); }
#line 9595 "y.tab.c" /* yacc.c:1257  */
        break;

    case 205: /* lvalue_reference  */
#line 6922 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lvalue).type);  }
#line 9601 "y.tab.c" /* yacc.c:1257  */
        break;

    case 207: /* expr4  */
#line 6923 "prolang.y" /* yacc.c:1257  */
      { free_fulltype(((*yyvaluep).lrvalue).type); }
#line 9607 "y.tab.c" /* yacc.c:1257  */
        break;

    case 210: /* name_lvalue  */
#line 6922 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lvalue).type);  }
#line 9613 "y.tab.c" /* yacc.c:1257  */
        break;

    case 211: /* lvalue  */
#line 6922 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lvalue).type);  }
#line 9619 "y.tab.c" /* yacc.c:1257  */
        break;

    case 212: /* name_var_lvalue  */
#line 6922 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lvalue).type);  }
#line 9625 "y.tab.c" /* yacc.c:1257  */
        break;

    case 213: /* local_name_lvalue  */
#line 6922 "prolang.y" /* yacc.c:1257  */
      { free_lpctype(((*yyvaluep).lvalue).type);  }
#line 9631 "y.tab.c" /* yacc.c:1257  */
        break;

    case 214: /* index_expr  */
#line 6924 "prolang.y" /* yacc.c:1257  */
      { free_fulltype(((*yyvaluep).index).type1);
              free_fulltype(((*yyvaluep).index).type2); }
#line 9638 "y.tab.c" /* yacc.c:1257  */
        break;

    case 215: /* index_range  */
#line 6924 "prolang.y" /* yacc.c:1257  */
      { free_fulltype(((*yyvaluep).index).type1);
              free_fulltype(((*yyvaluep).index).type2); }
#line 9645 "y.tab.c" /* yacc.c:1257  */
        break;

    case 227: /* struct_init  */
#line 6923 "prolang.y" /* yacc.c:1257  */
      { free_fulltype(((*yyvaluep).struct_init_member).type); }
#line 9651 "y.tab.c" /* yacc.c:1257  */
        break;

    case 234: /* catch  */
#line 6923 "prolang.y" /* yacc.c:1257  */
      { free_fulltype(((*yyvaluep).rvalue).type); }
#line 9657 "y.tab.c" /* yacc.c:1257  */
        break;

    case 239: /* sscanf  */
#line 6923 "prolang.y" /* yacc.c:1257  */
      { free_fulltype(((*yyvaluep).rvalue).type); }
#line 9663 "y.tab.c" /* yacc.c:1257  */
        break;

    case 240: /* parse_command  */
#line 6923 "prolang.y" /* yacc.c:1257  */
      { free_fulltype(((*yyvaluep).rvalue).type); }
#line 9669 "y.tab.c" /* yacc.c:1257  */
        break;


      default:
        break;
    }
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 6:
#line 7049 "prolang.y" /* yacc.c:1646  */
    { yywarn("Extra ';' ignored"); }
#line 9933 "y.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 7051 "prolang.y" /* yacc.c:1646  */
    { (yyval.address) = CURRENT_PROGRAM_SIZE; }
#line 9939 "y.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 7059 "prolang.y" /* yacc.c:1646  */
    {
          def_function_typecheck((yyvsp[-1].fulltype), (yyvsp[0].ident), MY_FALSE);
      }
#line 9947 "y.tab.c" /* yacc.c:1646  */
    break;

  case 9:
#line 7065 "prolang.y" /* yacc.c:1646  */
    {
          def_function_prototype((yyvsp[-1].number), MY_FALSE);
      }
#line 9955 "y.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 7071 "prolang.y" /* yacc.c:1646  */
    {
          def_function_complete((yyvsp[0].number), MY_FALSE);
          insert_pending_inline_closures();
          free_fulltype((yyvsp[-7].fulltype));
      }
#line 9965 "y.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 7078 "prolang.y" /* yacc.c:1646  */
    {
          insert_pending_inline_closures();
          free_fulltype((yyvsp[-1].fulltype));
      }
#line 9974 "y.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 7094 "prolang.y" /* yacc.c:1646  */
    {
#line 7096 "prolang.y"
          CURRENT_PROGRAM_SIZE = align(CURRENT_PROGRAM_SIZE);
          (yyval.number) = CURRENT_PROGRAM_SIZE;
          if (realloc_a_program(FUNCTION_HDR_SIZE))
#line 7099 "prolang.y"
          {
              CURRENT_PROGRAM_SIZE += FUNCTION_HDR_SIZE;
          }
          else
#line 7103 "prolang.y"
          {
              yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                      , mem_block[A_PROGRAM].current_size + FUNCTION_HDR_SIZE);
              YYACCEPT;
          }
      }
#line 9996 "y.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 7114 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-1].number); }
#line 10002 "y.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 7116 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = -1; }
#line 10008 "y.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 7125 "prolang.y" /* yacc.c:1646  */
    {
#ifdef DEBUG_INLINES
printf("DEBUG: After inline_opt_type: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */
          if (!prepare_inline_closure((yyvsp[0].lpctype)))
#line 7130 "prolang.y"
          {
              free_lpctype((yyvsp[0].lpctype));
              YYACCEPT;
          }
      }
#line 10024 "y.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 7138 "prolang.y" /* yacc.c:1646  */
    {
#ifdef DEBUG_INLINES
printf("DEBUG: After inline_opt_args: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */

          /* deactivate argument scope while parsing explicit context */
          block_scope[current_inline->block_depth+1].accessible = MY_FALSE;
          current_inline->parse_context = MY_TRUE;
          assert(block_scope[current_inline->block_depth].first_local == 0);

          /* Set the context scope as it would belong to the enclosing function. */
          if (current_inline->prev != -1 && INLINE_CLOSURE(current_inline->prev).parse_context)
#line 7150 "prolang.y"
          {
              /* If we are within the context of another closure, count from there.
               * Get its context scope and take their current variable count
               * (Its first_local was adjusted the same way, so just add its current
               * number of context variables.)
               */
              block_scope_t *scope = block_scope + INLINE_CLOSURE(current_inline->prev).block_depth;
              block_scope[current_inline->block_depth].first_local = scope->first_local + scope->num_locals;
          }
          else
              block_scope[current_inline->block_depth].first_local = current_inline->num_locals;

          /* Set type of locals back to the locals of the outer block.*/
          type_of_locals = &(LOCAL_TYPE(current_inline->full_local_type_start));

          /* Note, that type_of_context must not be reset, as add_context_name()
           * needs it where it points now. check_for_context_local() will take
           * care of finding the right type for context variables.
           */
      }
#line 10062 "y.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 7173 "prolang.y" /* yacc.c:1646  */
    {
#ifdef DEBUG_INLINES
printf("DEBUG: After inline_opt_context: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */

          /* Complete the F_CLEAR_LOCALS at the beginning of the context block. */
          block_scope_t *scope = block_scope + current_inline->block_depth;
          inline_closure_t *outer_closure;
          int * outer_max_num_locals;

          if (scope->num_locals > scope->num_cleared)
#line 7184 "prolang.y"
          {
              mem_block[A_PROGRAM].block[scope->addr+2]
                = (char)(scope->num_locals - scope->num_cleared);
          }

          /* reactivate argument scope */
          block_scope[current_inline->block_depth+1].accessible = MY_TRUE;
          current_inline->parse_context = MY_FALSE;
          adapt_context_names();
          type_of_locals = type_of_context + MAX_LOCAL;

          /* Find the correct max_num_locals to update.
           * That is the one of the last closure with parse_context set.
           */
          outer_max_num_locals = &(current_inline->max_num_locals);
          outer_closure = current_inline;

          while (outer_closure->prev != -1)
#line 7202 "prolang.y"
          {
              outer_closure = &(INLINE_CLOSURE(outer_closure->prev));
              if (!outer_closure->parse_context)
                  break;
              outer_max_num_locals = &(outer_closure->max_num_locals);
          }

          if (scope->first_local + scope->num_locals > *outer_max_num_locals)
              *outer_max_num_locals = scope->first_local + scope->num_locals;

          /* Check whether we clobbered some other local or context variables. */
          if (current_inline->block_depth > 0 && (scope->num_locals || scope->clobbered))
#line 7214 "prolang.y"
          {
              if (current_inline->prev != -1)
#line 7216 "prolang.y"
              {
                  outer_closure = &(INLINE_CLOSURE(current_inline->prev));
                  if (outer_closure->parse_context)
                      block_scope[outer_closure->block_depth].clobbered = MY_TRUE;
                  else
                      block_scope[current_inline->block_depth-1].clobbered = MY_TRUE;
              }
              else
                  block_scope[current_inline->block_depth-1].clobbered = MY_TRUE;
          }

          if (!inline_closure_prototype((yyvsp[-2].number)))
#line 7228 "prolang.y"
          {
              free_lpctype((yyvsp[-4].lpctype));
              YYACCEPT;
          }
      }
#line 10132 "y.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 7236 "prolang.y" /* yacc.c:1646  */
    {
#ifdef DEBUG_INLINES
printf("DEBUG: After inline block: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */
         (yyval.rvalue).start = current_inline->end;
         (yyval.rvalue).type = get_fulltype(lpctype_closure);

         complete_inline_closure();
         free_lpctype((yyvsp[-6].lpctype));
      }
#line 10147 "y.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 7250 "prolang.y" /* yacc.c:1646  */
    {
          int i;

#ifdef DEBUG_INLINES
printf("DEBUG: After L_BEGIN_INLINE: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */
          if (!prepare_inline_closure(lpctype_mixed))
              YYACCEPT;

          /* Synthesize $1..$9 as arguments */
          for (i = 1; i < 10; i++)
#line 7261 "prolang.y"
          {
              char name[4];
              ident_t *ident;

              sprintf(name, "$%d", i);
              ident = make_shared_identifier(name, I_TYPE_UNKNOWN, 0);
              add_local_name(ident, get_fulltype(lpctype_mixed), block_depth);
          }

          if (!inline_closure_prototype(9))
              YYACCEPT;

          /* Put the code block in its own scope apart from the
           * parameters, so that define_local_variable doesn't
           * assume that there are already 9 Variables.
           */
          enter_block_scope();
#ifdef DEBUG_INLINES
printf("DEBUG: Before comma_expr: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */
      }
#line 10185 "y.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 7287 "prolang.y" /* yacc.c:1646  */
    {
#ifdef DEBUG_INLINES
printf("DEBUG: After L_END_INLINE: program size %"PRIuMPINT"\n", CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */

         /* Complete the F_CLEAR_LOCALS at the beginning of the block. */
         block_scope_t *scope = block_scope + block_depth - 1;

         if (scope->num_locals > scope->num_cleared)
#line 7296 "prolang.y"
         {
              mem_block[A_PROGRAM].block[scope->addr+2]
                = (char)(scope->num_locals - scope->num_cleared);
         }

         leave_block_scope(MY_FALSE);

         (yyval.rvalue).start = current_inline->end;
         (yyval.rvalue).type = get_fulltype(lpctype_closure);

         complete_inline_closure();
      }
#line 10212 "y.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 7313 "prolang.y" /* yacc.c:1646  */
    {
          int i;

          /* Synthesize $1..$9 as arguments */
          for (i = 1; i < 10; i++)
#line 7318 "prolang.y"
          {
              char name[4];
              ident_t *ident;

              sprintf(name, "$%d", i);
              ident = make_shared_identifier(name, I_TYPE_UNKNOWN, 0);
              add_local_name(ident, get_fulltype(lpctype_mixed), block_depth);
          }

          (yyval.number) = 9;
      }
#line 10234 "y.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 7330 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-1].number); }
#line 10240 "y.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 7334 "prolang.y" /* yacc.c:1646  */
    {
#ifdef DEBUG_INLINES
          printf("DEBUG: inline_opt_type default: ANY\n");
#endif /* DEBUG_INLINES */
          (yyval.lpctype) = lpctype_mixed;
      }
#line 10251 "y.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 7341 "prolang.y" /* yacc.c:1646  */
    {
#ifdef DEBUG_INLINES
          printf("DEBUG: inline_opt_type: %s\n", get_lpctype_name((yyvsp[0].lpctype)));
#endif /* DEBUG_INLINES */
          (yyval.lpctype) = (yyvsp[0].lpctype);
      }
#line 10262 "y.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 7368 "prolang.y" /* yacc.c:1646  */
    { free_lpctype((yyvsp[0].lpctype)); }
#line 10268 "y.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 7375 "prolang.y" /* yacc.c:1646  */
    {
          /* Add a F_RETURN to complete the statement */
          ins_f_code(F_RETURN);
          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 10278 "y.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 7389 "prolang.y" /* yacc.c:1646  */
    {
          (void)define_new_struct(MY_TRUE, (yyvsp[-1].ident), compiled_file, (yyvsp[-3].typeflags));
      }
#line 10286 "y.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 7393 "prolang.y" /* yacc.c:1646  */
    {
          size_t i;

          /* Free any struct members left over from a previous
           * struct parse. This should happen only in case
           * of errors.
           */
          for (i = 0; i < STRUCT_MEMBER_COUNT; i++)
#line 7401 "prolang.y"
          {
              free_struct_member_data(&STRUCT_MEMBER(i));
          }
          mem_block[A_STRUCT_MEMBERS].current_size = 0;

          current_struct = define_new_struct(MY_FALSE, (yyvsp[0].ident), compiled_file, (yyvsp[-2].typeflags));
          if (current_struct < 0)
              YYACCEPT;
      }
#line 10309 "y.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 7411 "prolang.y" /* yacc.c:1646  */
    {
          finish_struct(current_id_number+1);
      }
#line 10317 "y.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 7417 "prolang.y" /* yacc.c:1646  */
    { }
#line 10323 "y.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 7419 "prolang.y" /* yacc.c:1646  */
    {
          /* Look up the struct id for the given identifier */

          int num = -1;

          if ((yyvsp[-1].ident)->type == I_TYPE_UNKNOWN)
#line 7425 "prolang.y"
          {
              /* Identifier -> no such struct encountered yet */
              yyerrorf("Unknown base struct '%s'", get_txt((yyvsp[-1].ident)->name));
          }
          else
#line 7430 "prolang.y"
          {
              ident_t *p = (yyvsp[-1].ident);

              /* Find the global struct identifier */
              while (p != NULL && p->type != I_TYPE_GLOBAL)
                  p = p->inferior;

              if (p == NULL || (num = p->u.global.struct_id) < 0)
#line 7438 "prolang.y"
              {
                  yyerrorf("Unknown base struct '%s'", get_txt((yyvsp[-1].ident)->name));
              }
              else if (STRUCT_DEF(num).flags & NAME_PROTOTYPE)
#line 7442 "prolang.y"
              {
                  yyerrorf("Undefined base struct '%s'", get_txt((yyvsp[-1].ident)->name));
              }
              else if (!struct_t_unique_name(STRUCT_DEF(num).type))
#line 7446 "prolang.y"
              {
                  yyerrorf("Incomplete base struct '%s'", get_txt((yyvsp[-1].ident)->name));
              }
              else
#line 7450 "prolang.y"
              {
                  struct_type_t *ptype = STRUCT_DEF(num).type;
                  struct_type_t *ctype = STRUCT_DEF(current_struct).type;
                  // record pointer to base struct even if the base struct has no members
                  ctype->base = ref_struct_type(ptype);

                  if (struct_t_size(ptype) > 0)
#line 7457 "prolang.y"
                  {
                      int count;
                      struct_member_t *member;

                      member = ptype->member;
                      count = struct_t_size(ptype);

                      for ( ; count > 0; count--, member++ )
                          add_struct_member(member->name, member->type, ptype);
                  }
              }
          } /* if type == UNKNOWN */
      }
#line 10386 "y.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 7484 "prolang.y" /* yacc.c:1646  */
    {
          /* The member_name_list adds the struct members. */
          free_lpctype((yyvsp[-1].lpctype));
      }
#line 10395 "y.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 7492 "prolang.y" /* yacc.c:1646  */
    {
          add_struct_member((yyvsp[0].ident)->name, (yyvsp[-1].lpctype), NULL);
          (yyval.lpctype) = (yyvsp[-1].lpctype);
      }
#line 10404 "y.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 7497 "prolang.y" /* yacc.c:1646  */
    {
          lpctype_t* type = get_array_type_with_depth((yyvsp[-3].lpctype), (yyvsp[-1].number));
          add_struct_member((yyvsp[0].ident)->name, type, NULL);
          free_lpctype(type);
          (yyval.lpctype) = (yyvsp[-3].lpctype);
      }
#line 10415 "y.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 7513 "prolang.y" /* yacc.c:1646  */
    {
#line 7515 "prolang.y"
          /* We got an inheritance: look up the name object and copy
           * the functions and variables into this program.
           *
           * If the inherited object hasn't been loaded yet, store the
           * name in inherit_file and abort the compile.
           *
           * Otherwise inherit_program() will do the rest.
           */

          object_t *ob;

          if (CURRENT_PROGRAM_SIZE
           && !(FUNCTION(FUNCTION_COUNT-1)->flags & NAME_INHERITED))
#line 7528 "prolang.y"
          {
              yyerror("illegal to inherit after defining functions");
          }

          /* Check the inheritance qualifiers.
           * A variable 'nosave' inherit is internally stored as 'static',
           * a functions 'nosave' inherit is not allowed.
           */
          if ((yyvsp[-3].inh_flags)[1] & TYPE_MOD_NOSAVE)
#line 7537 "prolang.y"
          {
              (yyvsp[-3].inh_flags)[1] |= TYPE_MOD_STATIC;
              (yyvsp[-3].inh_flags)[1] ^= TYPE_MOD_NOSAVE;
          }

          if ((yyvsp[-3].inh_flags)[0] & TYPE_MOD_NOSAVE)
#line 7543 "prolang.y"
          {
              (yyvsp[-3].inh_flags)[0] ^= TYPE_MOD_NOSAVE;
              yyerror("illegal to inherit code as 'nosave'");
          }

          if ((yyvsp[-3].inh_flags)[1] & TYPE_MOD_VARARGS)
#line 7549 "prolang.y"
          {
              (yyvsp[-3].inh_flags)[1] ^= TYPE_MOD_VARARGS;
              yyerror("illegal to inherit variables as 'varargs'");
          }

          (yyvsp[-3].inh_flags)[0] = check_visibility_flags((yyvsp[-3].inh_flags)[0], 0, true);
          (yyvsp[-3].inh_flags)[1] = check_visibility_flags((yyvsp[-3].inh_flags)[1], 0, false);

          /* First, try to call master->inherit_file().
           * Since simulate::load_object() makes sure that the master has been
           * loaded, this test can only fail when the master is compiled.
           */
          if (master_ob && !(master_ob->flags & O_DESTRUCTED)
           && !EVALUATION_TOO_LONG()
             )
#line 7564 "prolang.y"
          {
              svalue_t *res;

              push_ref_string(inter_sp, last_string_constant);

              if (!compat_mode)
#line 7570 "prolang.y"
              {
                  char * filename;
                  filename = alloca(strlen(current_loc.file->name)+2);
                  *filename = '/';
                  strcpy(filename+1, current_loc.file->name);
                  push_c_string(inter_sp, filename);
              }
              else
                  push_c_string(inter_sp, current_loc.file->name);

              res = apply_master(STR_INHERIT_FILE, 2);

              if (res && !(res->type == T_NUMBER && !res->u.number))
#line 7583 "prolang.y"
              {
                  /* We got a result - either a new name or a "reject it"
                   * value.
                   */

                  char * cp;

                  if (res->type != T_STRING)
#line 7591 "prolang.y"
                  {
                      yyerrorf("Illegal to inherit file '%s'."
                              , get_txt(last_string_constant));
                      YYACCEPT;
                  }

                  for (cp = get_txt(res->u.str); *cp == '/'; cp++) NOOP;

                  if (!legal_path(cp))
#line 7600 "prolang.y"
                  {
                      yyerrorf("Illegal path '%s'.", get_txt(res->u.str));
                      YYACCEPT;
                  }

                  /* Ok, now replace the parsed string with the name
                   * we just got.
                   */
                  free_mstring(last_string_constant);
                  last_string_constant = new_tabled(cp);
              }
              /* else: no result - use the string as it is */
          }
          else if (EVALUATION_TOO_LONG())
#line 7614 "prolang.y"
          {
              yyerrorf("Can't call master::%s for "
                       "'%s': eval cost too big"
                      , get_txt(STR_INHERIT_FILE)
                      , get_txt(last_string_constant));
              /* use the string as it is */
          }


          /* Look up the inherited object and swap it in.
           */
          ob = find_object(last_string_constant);
          if (ob == 0)
#line 7627 "prolang.y"
          {
              inherit_file = last_string_constant;
              last_string_constant = NULL;
              /* Return back to load_object() */
              YYACCEPT;
          }
          ob->time_of_ref = current_time;

          if (ob->flags & O_SWAPPED && load_ob_from_swap(ob) < 0)
#line 7636 "prolang.y"
          {
              free_mstring(last_string_constant);
              last_string_constant = NULL;
              yyerrorf("Out of memory when unswapping '%s'", get_txt(ob->name));
              YYACCEPT;
          }

          /* Legal to inherit? */
          if (ob->prog->flags & P_NO_INHERIT)
#line 7645 "prolang.y"
          {
              yyerror("Illegal to inherit an object which sets "
                      "'#pragma no_inherit'.");
              YYACCEPT;
          }

          free_mstring(last_string_constant);
          last_string_constant = NULL;

          /* Let's check whether we already have this inherit. */
#line 7655 "prolang.y"
          {
              inherit_t *inheritp = GET_BLOCK(A_INHERITS);
              int j = INHERIT_COUNT;
              bool duplicate_toplevel = false;

              for (; --j >= 0; inheritp++)
#line 7661 "prolang.y"
              {
                  if (inheritp->prog == ob->prog && inheritp->inherit_depth == 1)
#line 7663 "prolang.y"
                  {
                      duplicate_toplevel = true;
                      break;
                  }
              }

              if  (duplicate_toplevel)
#line 7670 "prolang.y"
              {
                  if (pragma_pedantic)
#line 7672 "prolang.y"
                  {
                      yyerrorf("Program '%s' already inherited"
                              , get_txt(inheritp->prog->name));
                      YYACCEPT;
                  }
                  else
                      yywarnf("Program '%s' already inherited"
                             , get_txt(inheritp->prog->name));
              }
          }

          /* Copy the functions and variables, and take
           * care of the initializer.
           */
          int initializer = inherit_program(ob->prog, (yyvsp[-3].inh_flags)[0], (yyvsp[-3].inh_flags)[1]);
          if (initializer > -1)
#line 7688 "prolang.y"
          {
              /* We inherited a __INIT() function: create a call */

              transfer_init_control();
              ins_f_code(F_SAVE_ARG_FRAME);
              ins_f_code(F_CALL_INHERITED);
              ins_short(INHERIT_COUNT-1);
              ins_short(initializer);
              ins_f_code(F_RESTORE_ARG_FRAME);
              ins_f_code(F_POP_VALUE);
              add_new_init_jump();
          }

          num_virtual_variables = V_VARIABLE_COUNT;
      }
#line 10629 "y.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 7712 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.inh_flags)[0] = (yyval.inh_flags)[1] = (yyvsp[0].typeflags);

          /* Allow 'static nosave inherit foo' as the short form
           * of 'static functions nosave variables inherit foo'; meaning
           * that we have to prevent the qualifier test in the
           * inheritance rule from triggering.
           */
          (yyval.inh_flags)[0] &= ~TYPE_MOD_NOSAVE;
          (yyval.inh_flags)[1] &= ~TYPE_MOD_VARARGS;
      }
#line 10645 "y.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 7725 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.inh_flags)[0] = (yyvsp[-1].inh_flags)[0] | (yyvsp[0].inh_flags)[0];
          (yyval.inh_flags)[1] = (yyvsp[-1].inh_flags)[1] | (yyvsp[0].inh_flags)[1];
      }
#line 10654 "y.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 7733 "prolang.y" /* yacc.c:1646  */
    { (yyval.typeflags) = TYPE_MOD_VIRTUAL; }
#line 10660 "y.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 7738 "prolang.y" /* yacc.c:1646  */
    { (yyval.typeflags) = (yyvsp[0].typeflags); }
#line 10666 "y.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 7740 "prolang.y" /* yacc.c:1646  */
    { (yyval.typeflags) = (yyvsp[-2].typeflags) | (yyvsp[-1].typeflags) | (yyvsp[0].typeflags); }
#line 10672 "y.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 7746 "prolang.y" /* yacc.c:1646  */
    {
          static ident_t    *last_identifier;
          static typeflags_t last_modifier;
#line 7750 "prolang.y"
          /* We use 'type' instead of only 'type_modifier_list'
           * to avoid parser conflicts between:
           *
           *    private functions(int i) {}
           * and
           *    private functions inherit "i";
           *
           * So this is the same right hand side as 'def'
           * for function definitions.
           * TODO: If 'functions' and 'variables' would become
           * keywords, then this wouldn't be ambiguous.
           *
           * But for now we just check that no type was given.
           */
          if ((yyvsp[-1].fulltype).t_type)
#line 7765 "prolang.y"
          {
              yyerror("syntax error");
              free_fulltype((yyvsp[-1].fulltype));
          }

          /* Check if there were any modifiers at all */
          do
#line 7772 "prolang.y"
          {
              if ( !(yyvsp[-1].fulltype).t_flags )
#line 7774 "prolang.y"
              {
                  /* take lookahead into account */
                  if ((yyvsp[0].ident) == last_identifier)
#line 7777 "prolang.y"
                  {
                      last_identifier = NULL;
                      (yyval.inh_flags)[0] = (yyval.inh_flags)[1] = 0;
                      break;
                  }
              }
              else
#line 7784 "prolang.y"
              {
                  last_modifier = (yyvsp[-1].fulltype).t_flags;
              }

              last_identifier = (yyvsp[0].ident);

              /* The L_IDENTIFIER must be one of "functions" or "variables" */
              if (mstreq(last_identifier->name, STR_FUNCTIONS))
#line 7792 "prolang.y"
              {
                    (yyval.inh_flags)[0] = last_modifier;
                    (yyval.inh_flags)[1] = 0;
              }
              else if (mstreq(last_identifier->name, STR_VARIABLES))
#line 7797 "prolang.y"
              {
                    (yyval.inh_flags)[0] = 0;
                    (yyval.inh_flags)[1] = last_modifier;
              }
              else
#line 7802 "prolang.y"
              {
                  yyerrorf("Unrecognized inheritance modifier '%s'"
                          , get_txt(last_identifier->name));
                  (yyval.inh_flags)[0] = (yyval.inh_flags)[1] = 0;
              }
          } while(0);
      }
#line 10748 "y.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 7820 "prolang.y" /* yacc.c:1646  */
    {
          if ((yyvsp[-1].inh_flags)[0] & ~( TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC | TYPE_MOD_VISIBLE
                       | TYPE_MOD_PROTECTED | TYPE_MOD_STATIC | TYPE_MOD_DEPRECATED)
             )
#line 7824 "prolang.y"
          {
              yyerror("Default visibility specification for functions "
                      "accepts only 'private', 'protected', 'visible', 'public', "
                      "'static' or 'deprecated'");
              YYACCEPT;
          }

          if ((yyvsp[-1].inh_flags)[1] & ~( TYPE_MOD_PRIVATE | TYPE_MOD_PUBLIC | TYPE_MOD_VISIBLE
                       | TYPE_MOD_PROTECTED | TYPE_MOD_DEPRECATED)
             )
#line 7834 "prolang.y"
          {
              yyerror("Default visibility specification for variables "
                      "accepts only 'private', 'protected', 'visible', 'public' "
                      "or 'deprecated'"
                      );
              YYACCEPT;
          }

          default_funmod = check_visibility_flags((yyvsp[-1].inh_flags)[0], 0, true);
          default_varmod = check_visibility_flags((yyvsp[-1].inh_flags)[1], 0, false);
      }
#line 10780 "y.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 7857 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = 0; }
#line 10786 "y.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 7858 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-1].number)+1; }
#line 10792 "y.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 7859 "prolang.y" /* yacc.c:1646  */
    {
          set_fulltype((yyval.fulltype), (yyvsp[-1].typeflags), (yyvsp[0].lpctype));
      }
#line 10800 "y.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 7865 "prolang.y" /* yacc.c:1646  */
    {
          set_fulltype((yyval.fulltype), (yyvsp[-1].typeflags), (yyvsp[0].lpctype));
      }
#line 10808 "y.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 7871 "prolang.y" /* yacc.c:1646  */
    { (yyval.typeflags) = 0; }
#line 10814 "y.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 7872 "prolang.y" /* yacc.c:1646  */
    { (yyval.typeflags) = (yyvsp[-1].typeflags) | (yyvsp[0].typeflags); }
#line 10820 "y.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 7876 "prolang.y" /* yacc.c:1646  */
    { (yyval.typeflags) = TYPE_MOD_NO_MASK; }
#line 10826 "y.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 7877 "prolang.y" /* yacc.c:1646  */
    { (yyval.typeflags) = TYPE_MOD_STATIC; }
#line 10832 "y.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 7878 "prolang.y" /* yacc.c:1646  */
    { (yyval.typeflags) = TYPE_MOD_PRIVATE; }
#line 10838 "y.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 7879 "prolang.y" /* yacc.c:1646  */
    { (yyval.typeflags) = TYPE_MOD_PUBLIC; }
#line 10844 "y.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 7880 "prolang.y" /* yacc.c:1646  */
    { (yyval.typeflags) = TYPE_MOD_VARARGS; }
#line 10850 "y.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 7881 "prolang.y" /* yacc.c:1646  */
    { (yyval.typeflags) = TYPE_MOD_PROTECTED; }
#line 10856 "y.tab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 7882 "prolang.y" /* yacc.c:1646  */
    { (yyval.typeflags) = TYPE_MOD_NOSAVE; }
#line 10862 "y.tab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 7883 "prolang.y" /* yacc.c:1646  */
    { (yyval.typeflags) = TYPE_MOD_DEPRECATED; }
#line 10868 "y.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 7884 "prolang.y" /* yacc.c:1646  */
    { (yyval.typeflags) = TYPE_MOD_VISIBLE; }
#line 10874 "y.tab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 7890 "prolang.y" /* yacc.c:1646  */
    { (yyval.lpctype) = NULL; }
#line 10880 "y.tab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 7895 "prolang.y" /* yacc.c:1646  */
    { (yyval.lpctype) = NULL; }
#line 10886 "y.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 7901 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.lpctype) = get_union_type((yyvsp[-2].lpctype), (yyvsp[0].lpctype));
          free_lpctype((yyvsp[-2].lpctype));
          free_lpctype((yyvsp[0].lpctype));
      }
#line 10896 "y.tab.c" /* yacc.c:1646  */
    break;

  case 79:
#line 7910 "prolang.y" /* yacc.c:1646  */
    { (yyval.lpctype) = lpctype_int;        }
#line 10902 "y.tab.c" /* yacc.c:1646  */
    break;

  case 80:
#line 7911 "prolang.y" /* yacc.c:1646  */
    { (yyval.lpctype) = lpctype_int;        }
#line 10908 "y.tab.c" /* yacc.c:1646  */
    break;

  case 81:
#line 7912 "prolang.y" /* yacc.c:1646  */
    { (yyval.lpctype) = lpctype_string;     }
#line 10914 "y.tab.c" /* yacc.c:1646  */
    break;

  case 82:
#line 7913 "prolang.y" /* yacc.c:1646  */
    { (yyval.lpctype) = lpctype_object;     }
#line 10920 "y.tab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 7914 "prolang.y" /* yacc.c:1646  */
    { (yyval.lpctype) = lpctype_closure;    }
#line 10926 "y.tab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 7915 "prolang.y" /* yacc.c:1646  */
    { (yyval.lpctype) = lpctype_symbol;     }
#line 10932 "y.tab.c" /* yacc.c:1646  */
    break;

  case 85:
#line 7916 "prolang.y" /* yacc.c:1646  */
    { (yyval.lpctype) = lpctype_float;      }
#line 10938 "y.tab.c" /* yacc.c:1646  */
    break;

  case 86:
#line 7917 "prolang.y" /* yacc.c:1646  */
    { (yyval.lpctype) = lpctype_mapping;    }
#line 10944 "y.tab.c" /* yacc.c:1646  */
    break;

  case 87:
#line 7918 "prolang.y" /* yacc.c:1646  */
    { (yyval.lpctype) = lpctype_mixed;      }
#line 10950 "y.tab.c" /* yacc.c:1646  */
    break;

  case 88:
#line 7920 "prolang.y" /* yacc.c:1646  */
    {
          int num;

          num = find_struct((yyvsp[0].sh_string));
          if (num < 0)
#line 7925 "prolang.y"
          {
              yyerrorf("Unknown struct '%s'", get_txt((yyvsp[0].sh_string)));
              (yyval.lpctype) = lpctype_any_struct;
          }
          else
#line 7930 "prolang.y"
          {
              (yyval.lpctype) = get_struct_type(STRUCT_DEF(num).type);
          }

          free_mstring((yyvsp[0].sh_string));
      }
#line 10973 "y.tab.c" /* yacc.c:1646  */
    break;

  case 89:
#line 7937 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.lpctype) = get_array_type((yyvsp[-1].lpctype));
          free_lpctype((yyvsp[-1].lpctype));
      }
#line 10982 "y.tab.c" /* yacc.c:1646  */
    break;

  case 90:
#line 7942 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.lpctype) = (yyvsp[-1].lpctype);
      }
#line 10990 "y.tab.c" /* yacc.c:1646  */
    break;

  case 92:
#line 7950 "prolang.y" /* yacc.c:1646  */
    { (yyval.lpctype) = lpctype_void;    }
#line 10996 "y.tab.c" /* yacc.c:1646  */
    break;

  case 93:
#line 7956 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.lpctype) = (yyvsp[-1].lpctype);
      }
#line 11004 "y.tab.c" /* yacc.c:1646  */
    break;

  case 94:
#line 7965 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.lpctype) = (yyvsp[-2].lpctype);
      }
#line 11012 "y.tab.c" /* yacc.c:1646  */
    break;

  case 95:
#line 7975 "prolang.y" /* yacc.c:1646  */
    {
          string_t *p;

          /* Extract the string from the ident structure */
          p = ref_mstring((yyvsp[0].ident)->name);
          (yyval.sh_string) = p;
      }
#line 11024 "y.tab.c" /* yacc.c:1646  */
    break;

  case 96:
#line 7984 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.sh_string) = ref_mstring((yyvsp[0].ident)->name);
      }
#line 11032 "y.tab.c" /* yacc.c:1646  */
    break;

  case 97:
#line 7996 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = 0; }
#line 11038 "y.tab.c" /* yacc.c:1646  */
    break;

  case 98:
#line 7997 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = 0; }
#line 11044 "y.tab.c" /* yacc.c:1646  */
    break;

  case 100:
#line 8002 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = 1; }
#line 11050 "y.tab.c" /* yacc.c:1646  */
    break;

  case 101:
#line 8003 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) + 1; }
#line 11056 "y.tab.c" /* yacc.c:1646  */
    break;

  case 102:
#line 8008 "prolang.y" /* yacc.c:1646  */
    {
          funflag_t illegal_flags = (yyvsp[-1].fulltype).t_flags & (TYPE_MOD_STATIC|TYPE_MOD_NO_MASK|TYPE_MOD_PRIVATE|TYPE_MOD_PUBLIC|TYPE_MOD_VIRTUAL|TYPE_MOD_PROTECTED|TYPE_MOD_NOSAVE|TYPE_MOD_VISIBLE);
          if (illegal_flags)
#line 8011 "prolang.y"
          {
              yyerrorf("Illegal modifier for function argument: %s"
                     , get_f_visibility(illegal_flags));
              (yyvsp[-1].fulltype).t_flags &= ~illegal_flags;
          }

          if (!(yyvsp[-1].fulltype).t_type)
#line 8018 "prolang.y"
          {
              if (exact_types)
                  yyerror("Missing type for argument");

              /* Supress more errors */
              (yyvsp[-1].fulltype).t_type = lpctype_mixed;
          }

          add_local_name((yyvsp[0].ident), (yyvsp[-1].fulltype), block_depth);
      }
#line 11083 "y.tab.c" /* yacc.c:1646  */
    break;

  case 103:
#line 8030 "prolang.y" /* yacc.c:1646  */
    {
          funflag_t illegal_flags = (yyvsp[-1].fulltype).t_flags & (TYPE_MOD_STATIC|TYPE_MOD_NO_MASK|TYPE_MOD_PRIVATE|TYPE_MOD_PUBLIC|TYPE_MOD_VIRTUAL|TYPE_MOD_PROTECTED|TYPE_MOD_NOSAVE|TYPE_MOD_VISIBLE);
          if (illegal_flags)
#line 8033 "prolang.y"
          {
              yyerrorf("Illegal modifier for function argument: %s"
                     , get_f_visibility(illegal_flags));
              (yyvsp[-1].fulltype).t_flags &= ~illegal_flags;
          }

          /* A local name is redeclared. */
          if (current_inline == NULL)
#line 8041 "prolang.y"
          {
              /* Since this is the argument list of a function, it can't be
               * legal.
               */
              yyerror("Illegal to redeclare local name");
              free_fulltype((yyvsp[-1].fulltype));
          }
          else
#line 8049 "prolang.y"
          {
              /* However, it is legal for the argument list of an inline
               * closure.
               */

              if (!(yyvsp[-1].fulltype).t_type)
#line 8055 "prolang.y"
              {
                  if (exact_types)
                      yyerror("Missing type for argument");

                  /* Supress more errors */
                  (yyvsp[-1].fulltype).t_type = lpctype_mixed;
              }

              redeclare_local((yyvsp[0].ident), (yyvsp[-1].fulltype), block_depth);
          }
      }
#line 11128 "y.tab.c" /* yacc.c:1646  */
    break;

  case 104:
#line 8073 "prolang.y" /* yacc.c:1646  */
    {
#line 8075 "prolang.y"
          if ((yyvsp[-1].fulltype).t_type == NULL)
#line 8076 "prolang.y"
          {
              yyerror("Missing type");
              (yyvsp[-1].fulltype).t_type = lpctype_mixed;
          }

          define_global_variable((yyvsp[0].ident), (yyvsp[-1].fulltype), MY_FALSE);
          (yyval.fulltype) = (yyvsp[-1].fulltype);
      }
#line 11145 "y.tab.c" /* yacc.c:1646  */
    break;

  case 105:
#line 8088 "prolang.y" /* yacc.c:1646  */
    {
          if ((yyvsp[-1].fulltype).t_type == NULL)
#line 8090 "prolang.y"
          {
              yyerror("Missing type");
              (yyvsp[-1].fulltype).t_type = lpctype_mixed;
          }

          (yyval.number) = define_global_variable((yyvsp[0].ident), (yyvsp[-1].fulltype), MY_TRUE);
      }
#line 11160 "y.tab.c" /* yacc.c:1646  */
    break;

  case 106:
#line 8099 "prolang.y" /* yacc.c:1646  */
    {
          init_global_variable((yyvsp[-2].number), (yyvsp[-3].ident), (yyvsp[-4].fulltype), (yyvsp[-1].number), (yyvsp[0].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);
          (yyval.fulltype) = (yyvsp[-4].fulltype);
      }
#line 11170 "y.tab.c" /* yacc.c:1646  */
    break;

  case 107:
#line 8106 "prolang.y" /* yacc.c:1646  */
    {
          fulltype_t type;
          type.t_type = get_array_type_with_depth((yyvsp[-3].fulltype).t_type, (yyvsp[-1].number));
          type.t_flags = (yyvsp[-3].fulltype).t_flags;

          define_global_variable((yyvsp[0].ident), type, MY_FALSE);
          free_fulltype(type);
          (yyval.fulltype) = (yyvsp[-3].fulltype);
      }
#line 11184 "y.tab.c" /* yacc.c:1646  */
    break;

  case 108:
#line 8119 "prolang.y" /* yacc.c:1646  */
    {
          fulltype_t type;
          type.t_type = get_array_type_with_depth((yyvsp[-3].fulltype).t_type, (yyvsp[-1].number));
          type.t_flags = (yyvsp[-3].fulltype).t_flags;

          (yyval.number) = define_global_variable((yyvsp[0].ident), type, MY_TRUE); 
          free_fulltype(type);
      }
#line 11197 "y.tab.c" /* yacc.c:1646  */
    break;

  case 109:
#line 8129 "prolang.y" /* yacc.c:1646  */
    {
          fulltype_t type;
          type.t_type = get_array_type_with_depth((yyvsp[-6].fulltype).t_type, (yyvsp[-4].number));
          type.t_flags = (yyvsp[-6].fulltype).t_flags;

          init_global_variable((yyvsp[-2].number), (yyvsp[-3].ident), type, (yyvsp[-1].number), (yyvsp[0].rvalue).type);

          free_fulltype(type);
          free_fulltype((yyvsp[0].rvalue).type);
          (yyval.fulltype) = (yyvsp[-6].fulltype);
      }
#line 11213 "y.tab.c" /* yacc.c:1646  */
    break;

  case 111:
#line 8153 "prolang.y" /* yacc.c:1646  */
    { enter_block_scope(); }
#line 11219 "y.tab.c" /* yacc.c:1646  */
    break;

  case 112:
#line 8157 "prolang.y" /* yacc.c:1646  */
    {
          /* If this is a local block, the declarations inserted
           * a code fragment to zero out the locals (previous blocks
           * may have left values in them). Complete the fragment
           * with the number of locals to clear, now that we
           * know it.
           */
#line 8164 "prolang.y"
          {
              block_scope_t *scope = block_scope + block_depth - 1;

              if (scope->num_locals > scope->num_cleared)
#line 8168 "prolang.y"
              {
                  mem_block[A_PROGRAM].block[scope->addr+2]
                    = (char)(scope->num_locals - scope->num_cleared);
              }
          }
     
          leave_block_scope(MY_FALSE);
      }
#line 11245 "y.tab.c" /* yacc.c:1646  */
    break;

  case 114:
#line 8183 "prolang.y" /* yacc.c:1646  */
    { free_lpctype((yyvsp[-1].lpctype)); }
#line 11251 "y.tab.c" /* yacc.c:1646  */
    break;

  case 116:
#line 8188 "prolang.y" /* yacc.c:1646  */
    {
          define_local_variable((yyvsp[0].ident), (yyvsp[-1].lpctype), NULL, MY_FALSE, MY_FALSE);

          (yyval.lpctype) = (yyvsp[-1].lpctype);
      }
#line 11261 "y.tab.c" /* yacc.c:1646  */
    break;

  case 117:
#line 8194 "prolang.y" /* yacc.c:1646  */
    {
          define_local_variable((yyvsp[0].ident), (yyvsp[-1].lpctype), NULL, MY_TRUE, MY_FALSE);

          (yyval.lpctype) = (yyvsp[-1].lpctype);
      }
#line 11271 "y.tab.c" /* yacc.c:1646  */
    break;

  case 118:
#line 8200 "prolang.y" /* yacc.c:1646  */
    {
          define_local_variable((yyvsp[0].ident), (yyvsp[-1].lpctype), &(yyval.lvalue), MY_FALSE, MY_TRUE);
      }
#line 11279 "y.tab.c" /* yacc.c:1646  */
    break;

  case 119:
#line 8204 "prolang.y" /* yacc.c:1646  */
    {
          init_local_variable((yyvsp[-3].ident), &(yyvsp[-2].lvalue), (yyvsp[-1].number), (yyvsp[0].rvalue).type);

          free_fulltype((yyvsp[0].rvalue).type);
          (yyval.lpctype) = (yyvsp[-4].lpctype);
      }
#line 11290 "y.tab.c" /* yacc.c:1646  */
    break;

  case 120:
#line 8211 "prolang.y" /* yacc.c:1646  */
    {
          define_local_variable((yyvsp[0].ident), (yyvsp[-1].lpctype), &(yyval.lvalue), MY_TRUE, MY_TRUE);
      }
#line 11298 "y.tab.c" /* yacc.c:1646  */
    break;

  case 121:
#line 8215 "prolang.y" /* yacc.c:1646  */
    {
          init_local_variable((yyvsp[-3].ident), &(yyvsp[-2].lvalue), (yyvsp[-1].number), (yyvsp[0].rvalue).type);

          free_fulltype((yyvsp[0].rvalue).type);
          (yyval.lpctype) = (yyvsp[-4].lpctype);
      }
#line 11309 "y.tab.c" /* yacc.c:1646  */
    break;

  case 122:
#line 8222 "prolang.y" /* yacc.c:1646  */
    {
          lpctype_t* type = get_array_type_with_depth((yyvsp[-3].lpctype), (yyvsp[-1].number));
          define_local_variable((yyvsp[0].ident), type, NULL, MY_FALSE, MY_FALSE);
          free_lpctype(type);

          (yyval.lpctype) = (yyvsp[-3].lpctype);
      }
#line 11321 "y.tab.c" /* yacc.c:1646  */
    break;

  case 123:
#line 8230 "prolang.y" /* yacc.c:1646  */
    {
          lpctype_t* type = get_array_type_with_depth((yyvsp[-3].lpctype), (yyvsp[-1].number));
          define_local_variable((yyvsp[0].ident), type, NULL, MY_TRUE, MY_FALSE);
          free_lpctype(type);

          (yyval.lpctype) = (yyvsp[-3].lpctype);
      }
#line 11333 "y.tab.c" /* yacc.c:1646  */
    break;

  case 124:
#line 8238 "prolang.y" /* yacc.c:1646  */
    {
          lpctype_t* type = get_array_type_with_depth((yyvsp[-3].lpctype), (yyvsp[-1].number));
          define_local_variable((yyvsp[0].ident), type, &(yyval.lvalue), MY_FALSE, MY_TRUE);
          free_lpctype(type);
      }
#line 11343 "y.tab.c" /* yacc.c:1646  */
    break;

  case 125:
#line 8244 "prolang.y" /* yacc.c:1646  */
    {
          init_local_variable((yyvsp[-3].ident), &(yyvsp[-2].lvalue), (yyvsp[-1].number), (yyvsp[0].rvalue).type);

          free_fulltype((yyvsp[0].rvalue).type);
          (yyval.lpctype) = (yyvsp[-6].lpctype);
      }
#line 11354 "y.tab.c" /* yacc.c:1646  */
    break;

  case 126:
#line 8251 "prolang.y" /* yacc.c:1646  */
    {
          lpctype_t* type = get_array_type_with_depth((yyvsp[-3].lpctype), (yyvsp[-1].number));
          define_local_variable((yyvsp[0].ident), type, &(yyval.lvalue), MY_TRUE, MY_TRUE);
          free_lpctype(type);
      }
#line 11364 "y.tab.c" /* yacc.c:1646  */
    break;

  case 127:
#line 8257 "prolang.y" /* yacc.c:1646  */
    {
          init_local_variable((yyvsp[-3].ident), &(yyvsp[-2].lvalue), (yyvsp[-1].number), (yyvsp[0].rvalue).type);

          free_fulltype((yyvsp[0].rvalue).type);
          (yyval.lpctype) = (yyvsp[-6].lpctype);
      }
#line 11375 "y.tab.c" /* yacc.c:1646  */
    break;

  case 128:
#line 8268 "prolang.y" /* yacc.c:1646  */
    {
          insert_pop_value();
#ifdef F_BREAK_POINT
          if (d_flag)
              ins_f_code(F_BREAK_POINT);
#endif /* F_BREAK_POINT */

          free_fulltype((yyvsp[-1].rvalue).type);
      }
#line 11389 "y.tab.c" /* yacc.c:1646  */
    break;

  case 139:
#line 8285 "prolang.y" /* yacc.c:1646  */
    {
          /* Compile the break statement */

          if (current_break_address == 0)
              yyerror("break statement outside loop");

          if (current_break_address & BREAK_ON_STACK)
#line 8292 "prolang.y"
          {
              /* We break from a switch() */

              ins_f_code(F_BREAK);
          }
          else
#line 8298 "prolang.y"
          {
              /* A normal loop break: add the FBRANCH to the list */

              ins_f_code(F_FBRANCH);
              ins_jump_offset(current_break_address & BREAK_ADDRESS_MASK);
              current_break_address = CURRENT_PROGRAM_SIZE - sizeof(int32);
              if (current_break_address > BREAK_ADDRESS_MASK)
                  yyerrorf("Compiler limit: (L_BREAK) value too large: %"PRIdBcOffset
                          , current_break_address);
          }
      }
#line 11420 "y.tab.c" /* yacc.c:1646  */
    break;

  case 140:
#line 8311 "prolang.y" /* yacc.c:1646  */
    {
          p_int depth;
#line 8314 "prolang.y"
          if (current_continue_address == 0)
              yyerror("continue statement outside loop");

          if ( 0 != (depth = (current_continue_address & SWITCH_DEPTH_MASK)) )
#line 8318 "prolang.y"
          {
              /* A continue inside a switch */

              /* For more than 255 nested switches, generate a series
               * of BREAKN_CONTINUE instructions.
               */
              while (depth > SWITCH_DEPTH_UNIT*256)
#line 8325 "prolang.y"
              {
                  ins_f_code(F_BREAKN_CONTINUE);
                  ins_byte(255);
                  ins_jump_offset(4);
                  depth -= SWITCH_DEPTH_UNIT*256;
              }

              /* BREAK_CONTINUE the last switches */
              if (depth > SWITCH_DEPTH_UNIT)
#line 8334 "prolang.y"
              {
                  depth /= SWITCH_DEPTH_UNIT;
                  ins_f_code(F_BREAKN_CONTINUE);
                  ins_byte(depth-1);
              }
              else
#line 8340 "prolang.y"
              {
                  ins_f_code(F_BREAK_CONTINUE);
              }
          }
          else
#line 8345 "prolang.y"
          {
              /* Normal continue */
              ins_f_code(F_FBRANCH);
          }

          /* In either case, handle the list of continues alike */
          ins_jump_offset(current_continue_address & CONTINUE_ADDRESS_MASK);
          current_continue_address =
                        ( current_continue_address & SWITCH_DEPTH_MASK ) |
                        ( CURRENT_PROGRAM_SIZE - sizeof(int32) );
      }
#line 11475 "y.tab.c" /* yacc.c:1646  */
    break;

  case 141:
#line 8361 "prolang.y" /* yacc.c:1646  */
    {
          if (exact_types && exact_types != lpctype_void && exact_types != lpctype_mixed)
              lpctype_error("Must return a value for a function declared",
                         exact_types);
          ins_f_code(F_RETURN0);
      }
#line 11486 "y.tab.c" /* yacc.c:1646  */
    break;

  case 142:
#line 8369 "prolang.y" /* yacc.c:1646  */
    {
#line 8371 "prolang.y"
          fulltype_t type2 = (yyvsp[0].rvalue).type;

          if (exact_types)
#line 8374 "prolang.y"
          {
              /* More checks, ie. mixed vs non-mixed, would be nice,
               * but the general type tracking is too lacking for it.
               */
              if (!has_common_type(type2.t_type, exact_types))
#line 8379 "prolang.y"
              {
                  char tmp[512];
                  get_fulltype_name_buf(type2, tmp, sizeof(tmp));

                  yyerrorf("Return type not matching: got %s, expected %s"
                         , tmp, get_lpctype_name(exact_types));
              }
          }

          if (last_expression == CURRENT_PROGRAM_SIZE - 1
           && mem_block[A_PROGRAM].block[last_expression] ==
                  F_CONST0 )
#line 8391 "prolang.y"
          {
              /* Optimize "CONST0 RETURN" to "RETURN0" */
              mem_block[A_PROGRAM].block[last_expression] =
                    F_RETURN0;
              last_expression = -1;
          }
          else
              ins_f_code(F_RETURN);

          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 11527 "y.tab.c" /* yacc.c:1646  */
    break;

  case 143:
#line 8416 "prolang.y" /* yacc.c:1646  */
    {
          /* Save the previous environment */

          (yyval.numbers)[0] = current_continue_address;
          (yyval.numbers)[1] = current_break_address;

          push_address(); /* Remember the starting address */
      }
#line 11540 "y.tab.c" /* yacc.c:1646  */
    break;

  case 144:
#line 8427 "prolang.y" /* yacc.c:1646  */
    {
#line 8429 "prolang.y"
          p_int addr = pop_address();
          p_int length = CURRENT_PROGRAM_SIZE - addr;
          bytecode_p expression;

          /* Take the <cond> code, add the BBRANCH instruction and
           * store all of it outside the program. After the <body>
           * has been compiled, the code will be put back in.
           */
          expression = yalloc(length+2);
          memcpy(expression, mem_block[A_PROGRAM].block+addr, length);
          if (last_expression == CURRENT_PROGRAM_SIZE - 1
           && expression[length-1] == F_NOT
             )
#line 8442 "prolang.y"
          {
              /* Optimizize
               *   NOT
               *   BBRANCH_WHEN_NON_ZERO
               * into
               *   BBRANCH_WHEN_ZERO
               */
              length--;
              expression[length] = F_BBRANCH_WHEN_ZERO;
          }
          else
#line 8453 "prolang.y"
          {
              expression[length] = F_BBRANCH_WHEN_NON_ZERO;
          }

          /* Save the code as 'expression' */
          (yyval.expression).p = expression;
          (yyval.expression).length = length;
          (yyval.expression).line = current_loc.line;

          /* Restart codegeneration for the body where we began */
          CURRENT_PROGRAM_SIZE = addr;
          last_expression = -1;

          /* The initial branch to the condition code */
          ins_f_code(F_BRANCH);
          push_address();
          ins_byte(0);

          current_continue_address = CONTINUE_DELIMITER;
          current_break_address = BREAK_DELIMITER;
      }
#line 11594 "y.tab.c" /* yacc.c:1646  */
    break;

  case 145:
#line 8477 "prolang.y" /* yacc.c:1646  */
    {
#line 8479 "prolang.y"
          /* The body compiled ok. Now patch up the breaks and continues
           * and insert the condition checking.
           */

          p_int offset;
          bc_offset_t next_addr;
          p_int addr = pop_address();

          /* Update the offsets of all continue BRANCHes
           * (resp BREAK_CONTINUEs) to branch to the current address.
           */
          for ( ; current_continue_address > 0
                ; current_continue_address = next_addr)
#line 8492 "prolang.y"
          {
              next_addr = read_jump_offset(current_continue_address);
              upd_jump_offset(current_continue_address,
                  CURRENT_PROGRAM_SIZE - current_continue_address);
          }

          /* If necessary, update the leading BRANCH to an LBRANCH */
          offset = fix_branch( F_LBRANCH, CURRENT_PROGRAM_SIZE, addr);

          /* Add the condition code to the program */
          if ((yyvsp[-1].expression).line != current_loc.line)
              store_line_number_info();
          add_to_mem_block(A_PROGRAM, (yyvsp[-1].expression).p, (yyvsp[-1].expression).length+2);
          yfree((yyvsp[-1].expression).p);

          /* Complete the branch at the end of the condition code */
          offset += addr + 1 - ( CURRENT_PROGRAM_SIZE - 1 );
          if (offset < -0xff)
#line 8510 "prolang.y"
          {
              /* We need a LBRANCH instead of the BBRANCH */

              bytecode_p codep;

              if (offset < -0x8000)
                  yyerror("offset overflow");
              codep = PROGRAM_BLOCK + --CURRENT_PROGRAM_SIZE - 1;
              *codep = *codep == F_BBRANCH_WHEN_NON_ZERO
                       ? F_LBRANCH_WHEN_NON_ZERO
                       : F_LBRANCH_WHEN_ZERO
              ;
              ins_short(offset);
          }
          else
#line 8525 "prolang.y"
          {
              /* Just add the short offset */
              mem_block[A_PROGRAM].block[CURRENT_PROGRAM_SIZE-1] = -offset;
          }

          if ((yyvsp[-1].expression).line != current_loc.line)
              store_line_number_relocation((yyvsp[-1].expression).line);

          /* Now that we have the end of the while(), we can finish
           * up the breaks.
           */
          for( ; current_break_address > 0
               ; current_break_address = next_addr)
#line 8538 "prolang.y"
          {
              next_addr = read_jump_offset(current_break_address);
              upd_jump_offset(current_break_address,
                  CURRENT_PROGRAM_SIZE - current_break_address);
          }

          /* Restore the previous environment */
          current_continue_address = (yyvsp[-6].numbers)[0];
          current_break_address    = (yyvsp[-6].numbers)[1];

          free_fulltype((yyvsp[-3].rvalue).type);
      }
#line 11676 "y.tab.c" /* yacc.c:1646  */
    break;

  case 146:
#line 8563 "prolang.y" /* yacc.c:1646  */
    {
          /* Save the previous environment */
          (yyval.numbers)[0] = current_continue_address;
          (yyval.numbers)[1] = current_break_address;

          current_break_address = BREAK_DELIMITER;
          current_continue_address = CONTINUE_DELIMITER;

          push_address(); /* Address to branch back to */
      }
#line 11691 "y.tab.c" /* yacc.c:1646  */
    break;

  case 147:
#line 8576 "prolang.y" /* yacc.c:1646  */
    {
          /* The body is complete - we can already patch up
           * the continue statements.
           */

          bc_offset_t next_addr;
          p_int current;
#line 8584 "prolang.y"
          current = CURRENT_PROGRAM_SIZE;
          for(; current_continue_address > 0
              ; current_continue_address = next_addr)
#line 8587 "prolang.y"
          {
              next_addr = read_jump_offset(current_continue_address);
              upd_jump_offset(current_continue_address,
                  current - current_continue_address);
          }
      }
#line 11714 "y.tab.c" /* yacc.c:1646  */
    break;

  case 148:
#line 8596 "prolang.y" /* yacc.c:1646  */
    {
#line 8598 "prolang.y"
          /* The loop is complete - we just need the final branch
           * instruction and to patch up the breaks.
           */

          p_int offset;
          bc_offset_t next_addr;
          p_int addr = pop_address();
          mp_uint current;
          bytecode_p dest;

          current = CURRENT_PROGRAM_SIZE;
          if (!realloc_a_program(3))
#line 8610 "prolang.y"
          {
              yyerrorf("Out of memory: program size %"PRIuMPINT"\n", current+3);
              YYACCEPT;
          }

          /* Add the branch statement */
          dest = PROGRAM_BLOCK + current;
          if (current == last_expression + 1 && dest[-1] == F_NOT)
#line 8618 "prolang.y"
          {
              /* Optimize 'NOT BBRANCH_WHEN_NON_ZERO' to 'BBRANCH_WHEN_ZERO'
               */
              offset = addr - current;
              if (offset < -0xff)
#line 8623 "prolang.y"
              {
                  if (offset < -0x8000)
                      yyerror("offset overflow");
                  PUT_CODE(dest-1, F_LBRANCH_WHEN_ZERO);
                  PUT_SHORT(dest, offset);
                  current += 2;
              }
              else
#line 8631 "prolang.y"
              {
                  PUT_CODE(dest-1, F_BBRANCH_WHEN_ZERO);
                  PUT_UINT8(dest, -offset);
                  current++;
              }
          }
          else
#line 8638 "prolang.y"
          {
              offset = addr - ( current + 1 );
              if (offset < -0xff) {
                  if (offset < -0x8000)
                      yyerror("offset overflow");
                  STORE_CODE(dest, F_LBRANCH_WHEN_NON_ZERO);
                  STORE_SHORT(dest, offset);
                  current += 3;
              } else {
                  STORE_CODE(dest, F_BBRANCH_WHEN_NON_ZERO);
                  STORE_UINT8(dest, -offset);
                  current += 2;
              }
          }

          CURRENT_PROGRAM_SIZE = current;

          /* Now that we have the end of the do-while(), we can finish
           * up the breaks.
           */
          for (; current_break_address > 0
               ; current_break_address = next_addr)
#line 8660 "prolang.y"
          {
              next_addr = read_jump_offset(current_break_address);
              upd_jump_offset(current_break_address,
                  current - current_break_address);
          }

          /* Restore the previous environment */
          current_continue_address = (yyvsp[-8].numbers)[0];
          current_break_address    = (yyvsp[-8].numbers)[1];

          free_fulltype((yyvsp[-2].rvalue).type);
      }
#line 11801 "y.tab.c" /* yacc.c:1646  */
    break;

  case 149:
#line 8693 "prolang.y" /* yacc.c:1646  */
    {
#line 8695 "prolang.y"
          /* Save the previous environment */
          (yyval.numbers)[0] = current_continue_address;
          (yyval.numbers)[1] = current_break_address;

          /* Open a new scope to all variables local to the
           * for-statement as a whole.
           */
          enter_block_scope();
      }
#line 11817 "y.tab.c" /* yacc.c:1646  */
    break;

  case 150:
#line 8707 "prolang.y" /* yacc.c:1646  */
    {
#line 8709 "prolang.y"
          /* Get rid of whatever init_expr computed */
          insert_pop_value();

          /* From here, the <body> will be placed eventually */

          current_continue_address = CONTINUE_DELIMITER;
          (yyval.number) = CURRENT_PROGRAM_SIZE;
      }
#line 11832 "y.tab.c" /* yacc.c:1646  */
    break;

  case 151:
#line 8720 "prolang.y" /* yacc.c:1646  */
    {
#line 8722 "prolang.y"
          /* Add the BBRANCH to the condition and save it all
           * in an 'expression' on the compiler stack for later
           * re-insertion.
           */

          p_int start, length;
          bytecode_p expression;

          start = (yyvsp[-2].number);
          length = CURRENT_PROGRAM_SIZE - start;
          expression = yalloc(length+2);
          memcpy(expression, mem_block[A_PROGRAM].block + start, length );

          /* Add the branch instruction */
          if (last_expression == CURRENT_PROGRAM_SIZE - 1
           && expression[length-1] == F_NOT
             )
#line 8739 "prolang.y"
          {
              /* Optimize 'NOT BBRANCH_WHEN_NON_ZERO'
               * to 'BBRANCH_WHEN_ZERO'
               */
              length--;
              expression[length] = F_BBRANCH_WHEN_ZERO;
          }
          else
#line 8747 "prolang.y"
          {
              expression[length] = F_BBRANCH_WHEN_NON_ZERO;
          }

          /* Save the codeblock on the stack */
          (yyval.expression).p = expression;
          (yyval.expression).length = length;
          (yyval.expression).line = current_loc.line;

          /* Restart codegeneration from here */
          CURRENT_PROGRAM_SIZE = start;
          last_expression = -1;
      }
#line 11879 "y.tab.c" /* yacc.c:1646  */
    break;

  case 152:
#line 8763 "prolang.y" /* yacc.c:1646  */
    {
#line 8765 "prolang.y"
          /* Save the <incr> code block on the compiler stack
           * for later re-insertion and start the compilation
           * of the loop body.
           */

          p_int length;

          /* Save the code block */
          insert_pop_value();
          length = CURRENT_PROGRAM_SIZE - (yyvsp[-5].number);
          (yyval.expression).p = yalloc(length);
          if (length)
              memcpy( (yyval.expression).p
                    , mem_block[A_PROGRAM].block + (yyvsp[-5].number)
                    , length );
          (yyval.expression).length = length;
          (yyval.expression).line = current_loc.line;

          /* Restart the codegeneration for the body */
          CURRENT_PROGRAM_SIZE = (yyvsp[-5].number);
          last_expression = -1;
          current_break_address = BREAK_DELIMITER;

          ins_f_code(F_BRANCH); /* over the body to the condition */
          ins_byte(0);

          /* Fix the number of locals to clear, now that we know it
           */
#line 8793 "prolang.y"
          {
              block_scope_t *scope = block_scope + block_depth - 1;

              if (scope->num_locals > scope->num_cleared)
#line 8797 "prolang.y"
              {
                  mem_block[A_PROGRAM].block[scope->addr+2]
                    = (char)(scope->num_locals - scope->num_cleared);
              }
          }
      }
#line 11926 "y.tab.c" /* yacc.c:1646  */
    break;

  case 153:
#line 8806 "prolang.y" /* yacc.c:1646  */
    {
#line 8808 "prolang.y"
          /* The loop is complete, now add the <incr> and <cond>
           * code saved on the compiler stack and patch up
           * the break and continues.
           */

          p_int offset;
          bc_offset_t next_addr;

          /* Patch up the continues */
          for (; current_continue_address > 0
               ; current_continue_address = next_addr)
#line 8819 "prolang.y"
          {
              next_addr = read_jump_offset(current_continue_address);
              upd_jump_offset(current_continue_address,
                  CURRENT_PROGRAM_SIZE - current_continue_address);
          }

          if ( (yyvsp[-4].expression).line != current_loc.line
           || (    (yyvsp[-1].expression).line != current_loc.line
                && (yyvsp[-1].expression).length)
             )
              store_line_number_info();

          /* Add the <incr> code block if needed */
          if ((yyvsp[-1].expression).length)
#line 8833 "prolang.y"
          {
              add_to_mem_block(A_PROGRAM, (yyvsp[-1].expression).p
                                        , (yyvsp[-1].expression).length);
              if ((yyvsp[-1].expression).line != (yyvsp[-4].expression).line)
                  store_line_number_relocation((yyvsp[-1].expression).line);
          }
          yfree((yyvsp[-1].expression).p);

          /* Fix the branch over the body */
          offset =
            fix_branch( F_LBRANCH, CURRENT_PROGRAM_SIZE, (yyvsp[-7].number) + 1);

          /* Add the <cond> code block */
          add_to_mem_block(A_PROGRAM, (yyvsp[-4].expression).p, (yyvsp[-4].expression).length+2);
          yfree((yyvsp[-4].expression).p);

          /* Create the branch back after the condition */
          offset += (yyvsp[-7].number) + 2 - ( CURRENT_PROGRAM_SIZE - 1 );
          if (offset < -0xff)
#line 8852 "prolang.y"
          {
              bytecode_p codep;

              if (offset < -0x8000)
                  yyerror("offset overflow");

              codep = PROGRAM_BLOCK + --CURRENT_PROGRAM_SIZE - 1;
              *codep = *codep == F_BBRANCH_WHEN_NON_ZERO
                       ? F_LBRANCH_WHEN_NON_ZERO
                       : F_LBRANCH_WHEN_ZERO
              ;
              ins_short(offset);
          }
          else
#line 8866 "prolang.y"
          {
              mem_block[A_PROGRAM].block[CURRENT_PROGRAM_SIZE-1] = -offset;
          }

          if ((yyvsp[-4].expression).line != current_loc.line)
              store_line_number_relocation((yyvsp[-4].expression).line);

          /* Now complete the break instructions.
           */
          for (; current_break_address > 0
               ; current_break_address = next_addr)
#line 8877 "prolang.y"
          {
              next_addr = read_jump_offset(current_break_address);
              upd_jump_offset(current_break_address,
                  CURRENT_PROGRAM_SIZE - current_break_address);
          }

          /* Restore the previous environment */
          current_continue_address = (yyvsp[-10].numbers)[0];
          current_break_address    = (yyvsp[-10].numbers)[1];

          /* and leave the for scope */
          leave_block_scope(MY_FALSE);
      }
#line 12020 "y.tab.c" /* yacc.c:1646  */
    break;

  case 154:
#line 8897 "prolang.y" /* yacc.c:1646  */
    {
          last_expression = mem_block[A_PROGRAM].current_size;
          ins_number(1);
            /* insert_pop_value() will optimize this away */
      }
#line 12030 "y.tab.c" /* yacc.c:1646  */
    break;

  case 157:
#line 8910 "prolang.y" /* yacc.c:1646  */
    {
          insert_pop_value();
      }
#line 12038 "y.tab.c" /* yacc.c:1646  */
    break;

  case 159:
#line 8919 "prolang.y" /* yacc.c:1646  */
    {
          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 12046 "y.tab.c" /* yacc.c:1646  */
    break;

  case 160:
#line 8923 "prolang.y" /* yacc.c:1646  */
    {
          /* We got a "int <name> = <expr>" type expression. */

#line 8927 "prolang.y"
          fulltype_t type2;
          Bool res;

          /* Check the assignment for validity */
          type2 = (yyvsp[0].rvalue).type;
          if (exact_types && !has_common_type(type2.t_type, (yyvsp[-2].lvalue).type))
#line 8933 "prolang.y"
          {
              yyerrorf("Bad assignment %s", get_two_lpctypes((yyvsp[-2].lvalue).type, type2.t_type));
          }

          if ((yyvsp[-1].number) != F_ASSIGN)
#line 8938 "prolang.y"
          {
              yyerror("Only plain assignments allowed here");
          }

          /* Add the bytecode to create the lvalue and do the
           * assignment.
           */
          res = add_lvalue_code(&(yyvsp[-2].lvalue), (yyvsp[-1].number));

          free_lpctype((yyvsp[-2].lvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);

          if (!res)
              YYACCEPT;
      }
#line 12083 "y.tab.c" /* yacc.c:1646  */
    break;

  case 161:
#line 8955 "prolang.y" /* yacc.c:1646  */
    {
          /* We got a "int <name>" type expression.
           * Compile it as if it was a "int <name> = 0" expression.
           */
#line 8960 "prolang.y"
          Bool res;

          /* Insert the implied push of number 0 */
          ins_number(0);

          /* Add the bytecode to create the lvalue and do the
           * assignment.
           */
          res = add_lvalue_code(&(yyvsp[0].lvalue), F_ASSIGN);

          free_lpctype((yyvsp[0].lvalue).type);

          if (!res)
              YYACCEPT;
      }
#line 12108 "y.tab.c" /* yacc.c:1646  */
    break;

  case 162:
#line 8980 "prolang.y" /* yacc.c:1646  */
    {
          last_expression = mem_block[A_PROGRAM].current_size;
          ins_number(1);
      }
#line 12117 "y.tab.c" /* yacc.c:1646  */
    break;

  case 163:
#line 8985 "prolang.y" /* yacc.c:1646  */
    {
          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 12125 "y.tab.c" /* yacc.c:1646  */
    break;

  case 164:
#line 9012 "prolang.y" /* yacc.c:1646  */
    {
          /* Save the previous environment */
          (yyval.numbers)[0] = current_continue_address;
          (yyval.numbers)[1] = current_break_address;

          current_break_address = BREAK_DELIMITER;
          current_continue_address = CONTINUE_DELIMITER;

          /* Open a new scope to all variables local to the
           * foreach-statement as a whole.
           */
          enter_block_scope();
      }
#line 12143 "y.tab.c" /* yacc.c:1646  */
    break;

  case 165:
#line 9028 "prolang.y" /* yacc.c:1646  */
    {
#line 9030 "prolang.y"
          /* Remember the starting address of the expression */
          (yyval.address) = CURRENT_PROGRAM_SIZE;
      }
#line 12153 "y.tab.c" /* yacc.c:1646  */
    break;

  case 166:
#line 9036 "prolang.y" /* yacc.c:1646  */
    {
#line 9038 "prolang.y"
          /* Fix the number of locals to clear, now that we know it
           */
#line 9040 "prolang.y"
          {
              block_scope_t *scope = block_scope + block_depth - 1;

              if (scope->num_locals > scope->num_cleared)
#line 9044 "prolang.y"
              {
                  mem_block[A_PROGRAM].block[scope->addr+2]
                    = (char)(scope->num_locals - scope->num_cleared);
              }
          }

          /* Create the FOREACH instruction, leaving the branch field
           * blank.
           */
          switch ((yyvsp[-1].number))
#line 9054 "prolang.y"
          {
          case FOREACH_LOOP:
              ins_f_code(F_FOREACH); break;
          case FOREACH_REF:
              ins_f_code(F_FOREACH_REF); break;
          case FOREACH_RANGE:
              ins_f_code(F_FOREACH_RANGE); break;
          default:
              yyerrorf("Unknown foreach_expr type %ld.\n", (long)(yyvsp[-1].number));
              fatal("Unknown foreach_expr type %ld.\n", (long)(yyvsp[-1].number));
              /* NOTREACHED */
          }
          ins_byte((yyvsp[-4].number)+1);
          ins_short(0);

          push_address(); /* Address to branch back to */
      }
#line 12196 "y.tab.c" /* yacc.c:1646  */
    break;

  case 167:
#line 9074 "prolang.y" /* yacc.c:1646  */
    {
          /* The body is complete - patch up the continue and
           * break statements and generate the remaining statements.
           */

          bc_offset_t next_addr;
          p_int addr;
          mp_uint current;

#line 9084 "prolang.y"
          current = CURRENT_PROGRAM_SIZE;
          addr = pop_address(); /* Where the body began */

          /* One obvious optimisation: when there is no code in
           * the body, we can save space and even more time by
           * just compiling the expression.
           * Too bad that we can't find out whether the expression
           * has side effects or not, otherwise we could try to
           * remove it, too.
           */
          if (addr == (p_int)current)
#line 9095 "prolang.y"
          {
              p_int expr_addr;  /* Address of the expr0 */
              p_int start_addr; /* Address of the first PUSH_LOCAL_LVALUE */
              bytecode_p src, dest;

              expr_addr = (yyvsp[-4].address);
              start_addr = expr_addr - (yyvsp[-6].number)*2;
              current = start_addr + (addr - 4 - expr_addr);
              for ( src = PROGRAM_BLOCK + expr_addr,
                    dest = PROGRAM_BLOCK + start_addr
                  ; expr_addr < addr-4
                  ; src++, dest++, expr_addr++)
                  *dest = *src;
              CURRENT_PROGRAM_SIZE = current;
              ins_f_code(F_POP_VALUE);
              current++;
              if ((yyvsp[-3].number) == FOREACH_RANGE)
#line 9112 "prolang.y"
              {
                  ins_f_code(F_POP_VALUE);
                  current++;
              }
          }
          else /* Create the full statement */
#line 9118 "prolang.y"
          {
              /* First patch up the continue statements */

              for(; current_continue_address > 0
                  ; current_continue_address = next_addr)
#line 9123 "prolang.y"
              {
                  next_addr = read_jump_offset(current_continue_address);
                  upd_jump_offset(current_continue_address,
                      current - current_continue_address);
              }

              /* Create the FOREACH_NEXT instruction and update
               * the branch of the earlier F_FOREACH.
               */

              upd_short(addr - 2, current - addr);

              ins_f_code(F_FOREACH_NEXT);
              ins_short(current + 3 - addr);

              current += 3;

              /* Finish up the breaks.
               */
              for (; current_break_address > 0
                   ; current_break_address = next_addr)
#line 9144 "prolang.y"
              {
                  next_addr = read_jump_offset(current_break_address);
                  upd_jump_offset(current_break_address,
                      current - current_break_address);
              }

              /* Finish with the FOREACH_END.
               */
              ins_f_code(F_FOREACH_END);
          }

          /* Restore the previous environment */
          current_continue_address = (yyvsp[-7].numbers)[0];
          current_break_address    = (yyvsp[-7].numbers)[1];

          /* and leave the scope */
          leave_block_scope(MY_FALSE);
      }
#line 12294 "y.tab.c" /* yacc.c:1646  */
    break;

  case 168:
#line 9171 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = 1; }
#line 12300 "y.tab.c" /* yacc.c:1646  */
    break;

  case 169:
#line 9172 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) + 1; }
#line 12306 "y.tab.c" /* yacc.c:1646  */
    break;

  case 170:
#line 9178 "prolang.y" /* yacc.c:1646  */
    {
          /* Add the bytecode to create the lvalue, and good is.
           */

#line 9183 "prolang.y"
          Bool res = add_lvalue_code(&(yyvsp[0].lvalue), 0);

          free_lpctype((yyvsp[0].lvalue).type);

          if (!res)
              YYACCEPT;
      }
#line 12323 "y.tab.c" /* yacc.c:1646  */
    break;

  case 173:
#line 9211 "prolang.y" /* yacc.c:1646  */
    {
          if (!mstreq((yyvsp[0].sh_string), STR_IN))
              yyerror("Expected keyword 'in' in foreach()");
          free_mstring((yyvsp[0].sh_string));
      }
#line 12333 "y.tab.c" /* yacc.c:1646  */
    break;

  case 175:
#line 9222 "prolang.y" /* yacc.c:1646  */
    {
          lpctype_t *dtype;
          Bool       gen_refs;

#line 9227 "prolang.y"
          gen_refs = ((yyvsp[0].rvalue).type.t_flags & TYPE_MOD_REFERENCE) != 0;
          dtype = (yyvsp[0].rvalue).type.t_type;

          /* Allowed are arrays of all kinds, strings, mappings,
           * ints (but not &int), mixed and unknown (when !exact_types).
           */
          if (!has_common_type(lpctype_any_array, dtype)
           && !has_common_type(lpctype_any_struct, dtype)
           && !lpctype_contains(lpctype_string, dtype)
           && !lpctype_contains(lpctype_mapping, dtype)
           && (gen_refs || !lpctype_contains(lpctype_int, dtype))
           && (exact_types || dtype != lpctype_unknown)
             )
#line 9240 "prolang.y"
          {
              fulltype_error("Expression for foreach() of wrong type", (yyvsp[0].rvalue).type);
          }

          (yyval.number) = gen_refs ? FOREACH_REF : FOREACH_LOOP;

          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 12365 "y.tab.c" /* yacc.c:1646  */
    break;

  case 176:
#line 9250 "prolang.y" /* yacc.c:1646  */
    {
          lpctype_t *dtype;

#line 9254 "prolang.y"
          if (((yyvsp[-2].rvalue).type.t_flags & TYPE_MOD_REFERENCE) != 0)
#line 9255 "prolang.y"
          {
              fulltype_error("Expression for foreach() of wrong type", (yyvsp[-2].rvalue).type);
          }

          dtype = (yyvsp[-2].rvalue).type.t_type;

          if (!lpctype_contains(lpctype_int, dtype)
           && (exact_types || dtype != lpctype_unknown)
             )
#line 9264 "prolang.y"
          {
              fulltype_error("Expression for foreach() of wrong type", (yyvsp[-2].rvalue).type);
          }

          if (((yyvsp[0].rvalue).type.t_flags & TYPE_MOD_REFERENCE) != 0)
#line 9269 "prolang.y"
          {
              fulltype_error("Expression for foreach() of wrong type", (yyvsp[0].rvalue).type);
          }

          dtype = (yyvsp[0].rvalue).type.t_type;

          if (!lpctype_contains(lpctype_int, dtype)
           && (exact_types || dtype != lpctype_unknown)
             )
#line 9278 "prolang.y"
          {
              fulltype_error("Expression for foreach() of wrong type", (yyvsp[0].rvalue).type);
          }

          (yyval.number) = FOREACH_RANGE;

          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 12411 "y.tab.c" /* yacc.c:1646  */
    break;

  case 177:
#line 9315 "prolang.y" /* yacc.c:1646  */
    {
        /* We start a new switch(), which might be nested into
         * an outer switch().
         */

        case_state_t *statep;
#line 9322 "prolang.y"
        current_break_stack_need++;
        if ( current_break_stack_need > max_break_stack_need )
            max_break_stack_need = current_break_stack_need;

        /* Save the previous switch state */
        if ( !(statep = yalloc(sizeof(case_state_t))) )
#line 9328 "prolang.y"
        {
            yyerrorf("Out of memory: case state (%zu bytes)"
                    , sizeof(case_state_t));
            YYACCEPT;
        }
        *statep = case_state;
        case_state.previous = statep;
        push_explicit(current_break_address);
        push_explicit(switch_pc);

        /* Create the SWITCH instruction plus two empty bytes */
        ins_f_code(F_SWITCH);
        switch_pc = mem_block[A_PROGRAM].current_size;
        ins_short(0);

        /* Set up the new switch generation */
        case_state.list0 = case_state.list1 = NULL;
        case_state.zero = NULL;
        case_state.no_string_labels = MY_TRUE;
        case_state.some_numeric_labels = MY_FALSE;
        case_state.default_addr = 0;

        current_break_address =
                BREAK_ON_STACK | BREAK_FROM_SWITCH | CASE_LABELS_ENABLED ;
        if (current_continue_address)
            current_continue_address += SWITCH_DEPTH_UNIT;
      }
#line 12457 "y.tab.c" /* yacc.c:1646  */
    break;

  case 178:
#line 9362 "prolang.y" /* yacc.c:1646  */
    {
#line 9364 "prolang.y"
        /* The statement (which hopefully contained cases) is complete.
         * Now create the lookup tables and restore the previous state.
         */

        case_state_t *statep;

        current_break_address &=
            ~(BREAK_ON_STACK|BREAK_FROM_SWITCH|CASE_LABELS_ENABLED);

        if (!case_state.default_addr)
#line 9374 "prolang.y"
        {
            /* no default given -> create one */
            case_state.default_addr = CURRENT_PROGRAM_SIZE-switch_pc;
        }

        /* it isn't unusual that the last case/default has no break */
        ins_f_code(F_BREAK);

        /* Create the lookup tables */
        store_case_labels(
          CURRENT_PROGRAM_SIZE-switch_pc,
          case_state.default_addr,
          case_state.no_string_labels || case_state.some_numeric_labels,
          case_state.zero,
          yyget_space, yymove_switch_instructions, yyerror, yycerrorl
        );

        /* Restore the previous state */
        switch_pc = pop_address();
        current_break_address = pop_address();
        statep = case_state.previous;
        case_state = *statep;
        yfree(statep);
        if (current_continue_address)
            current_continue_address -= SWITCH_DEPTH_UNIT;
        current_break_stack_need--;

        free_fulltype((yyvsp[-5].rvalue).type);
      }
#line 12504 "y.tab.c" /* yacc.c:1646  */
    break;

  case 184:
#line 9419 "prolang.y" /* yacc.c:1646  */
    {
#line 9421 "prolang.y"
        /* Mark the current program address as another
         * case target for the current switch.
         */
        case_list_entry_t *temp;

        /* Should be within a switch statement. */
        assert(current_break_address & CASE_LABELS_ENABLED);

        /* Get and fill in a new case entry structure */
        if ( !(temp = new_case_entry()) )
#line 9431 "prolang.y"
        {
            yyerror("Out of memory: new case entry");
            break;
        }

        if ( !(temp->key = (yyvsp[-1].case_label).key) )
#line 9437 "prolang.y"
        {
            case_state.zero = temp;
        }
        temp->addr = mem_block[A_PROGRAM].current_size - switch_pc;
        temp->line = current_loc.line;
    }
#line 12535 "y.tab.c" /* yacc.c:1646  */
    break;

  case 185:
#line 9445 "prolang.y" /* yacc.c:1646  */
    {
#line 9447 "prolang.y"
        /* Mark the current program address as another
         * range-case target for the current switch.
         */

        case_list_entry_t *temp;

        if ( !(yyvsp[-3].case_label).numeric || !(yyvsp[-1].case_label).numeric )
            yyerror("String case labels not allowed as range bounds");

        /* Should be within a switch statement. */
        assert(current_break_address & CASE_LABELS_ENABLED);

        /* A range like "case 4..2" is illegal,
         * a range like "case 4..4" counts as simple "case 4".
         */
        if ((yyvsp[-3].case_label).key >= (yyvsp[-1].case_label).key)
#line 9463 "prolang.y"
        {
            if ((yyvsp[-3].case_label).key > (yyvsp[-1].case_label).key)
#line 9465 "prolang.y"
            {
                yyerrorf("Illegal case range: lower limit %ld > upper limit %ld"
                        , (long)(yyvsp[-3].case_label).key, (long)(yyvsp[-1].case_label).key);
                break;
            }
            if ( !(temp = new_case_entry()) )
#line 9471 "prolang.y"
            {
                yyerror("Out of memory: new case entry");
                break;
            }
            temp->key = (yyvsp[-3].case_label).key;
            temp->addr = CURRENT_PROGRAM_SIZE - switch_pc;
            temp->line = current_loc.line;
        }

        /* Get and fill in the two case entries */

        if ( !(temp = new_case_entry()) )
#line 9483 "prolang.y"
        {
            yyerror("Out of memory: new case entry");
            break;
        }
        temp->key = (yyvsp[-3].case_label).key;
        temp->addr = 1; /* marks the lower bound of the range */
        temp->line = current_loc.line;

        if ( !(temp = new_case_entry()) ) {
            yyerror("Out of memory: new case entry");
            break;
        }
        temp->key = (yyvsp[-1].case_label).key;
        temp->addr = CURRENT_PROGRAM_SIZE - switch_pc;
        temp->line = 0; /* marks the upper bound of the range */
    }
#line 12598 "y.tab.c" /* yacc.c:1646  */
    break;

  case 186:
#line 9504 "prolang.y" /* yacc.c:1646  */
    {
#line 9506 "prolang.y"
          if ( 0 != ((yyval.case_label).key = (yyvsp[0].number)) ) {
              if ( !(case_state.no_string_labels) )
                  yyerror("Mixed case label list not allowed");
              case_state.some_numeric_labels = 1;
          }
          (yyval.case_label).numeric = MY_TRUE;
      }
#line 12612 "y.tab.c" /* yacc.c:1646  */
    break;

  case 187:
#line 9515 "prolang.y" /* yacc.c:1646  */
    {
#line 9517 "prolang.y"
          if ( case_state.some_numeric_labels )
              yyerror("Mixed case label list not allowed");

          case_state.no_string_labels = MY_FALSE;
          store_prog_string(last_string_constant);
          (yyval.case_label).key = (p_int)last_string_constant;
          (yyval.case_label).numeric = MY_FALSE;
          last_string_constant = NULL;
      }
#line 12628 "y.tab.c" /* yacc.c:1646  */
    break;

  case 188:
#line 9531 "prolang.y" /* yacc.c:1646  */
    {
#line 9533 "prolang.y"
          /* Mark the current program address as the default target
           * for the current switch.
           */

          /* Should be within a switch statement. */
          assert(current_break_address & CASE_LABELS_ENABLED);

          if (case_state.default_addr)
              yyerror("Duplicate default");

          case_state.default_addr = CURRENT_PROGRAM_SIZE - switch_pc;
    }
#line 12647 "y.tab.c" /* yacc.c:1646  */
    break;

  case 189:
#line 9563 "prolang.y" /* yacc.c:1646  */
    {
          /* When we enter a condition, we must not allow case labels
           * anymore.
           */

          mp_uint current;
          bytecode_p current_code;

          /* Turn off the case labels */

          (yyval.numbers)[0] = current_break_address;
          current_break_address &= ~CASE_LABELS_ENABLED;

          current = CURRENT_PROGRAM_SIZE;
          if (!realloc_a_program(2))
#line 9578 "prolang.y"
          {
              yyerrorf("Out of memory: program size %"PRIuMPINT"\n", current+3);
              YYACCEPT;
          }
          current_code = PROGRAM_BLOCK + current;

          /* Add the branch instruction, with the usual optimization */
          if (last_expression == current - 1
           && current_code[-1] == F_NOT)
#line 9587 "prolang.y"
          {
              current_code[-1] = F_BRANCH_WHEN_NON_ZERO;
          }
          else
#line 9591 "prolang.y"
          {
              *current_code = F_BRANCH_WHEN_ZERO;
              current++;
          }

          (yyval.numbers)[1] = current;
          CURRENT_PROGRAM_SIZE = current + 1;

          free_fulltype((yyvsp[-1].rvalue).type);
      }
#line 12693 "y.tab.c" /* yacc.c:1646  */
    break;

  case 190:
#line 9608 "prolang.y" /* yacc.c:1646  */
    {
          p_int destination, location, offset;

          /* Complete the branch over the if-part */
          destination = (p_int)(yyvsp[0].address);
          location = (yyvsp[-2].numbers)[1];
          if ( (offset = destination - location) > 0x100)
#line 9615 "prolang.y"
          {
              fix_branch(
                mem_block[A_PROGRAM].block[location-1] ==
                 F_BRANCH_WHEN_ZERO ?
                  F_LBRANCH_WHEN_ZERO :
                  F_LBRANCH_WHEN_NON_ZERO
                ,
                destination, location
              );
          }
          else
#line 9626 "prolang.y"
          {
              mem_block[A_PROGRAM].block[location] = offset - 1;
          }

          /* Restore the previous case-labels status without
           * changing the actual break-address.
           */
          current_break_address |= (yyvsp[-2].numbers)[0] & CASE_LABELS_ENABLED;
      }
#line 12727 "y.tab.c" /* yacc.c:1646  */
    break;

  case 191:
#line 9640 "prolang.y" /* yacc.c:1646  */
    {
          /* The if-part ends here */
          (yyval.address) = CURRENT_PROGRAM_SIZE;
      }
#line 12736 "y.tab.c" /* yacc.c:1646  */
    break;

  case 192:
#line 9646 "prolang.y" /* yacc.c:1646  */
    {
          /* Add the branch over the else part */
          ins_f_code(F_BRANCH);
          (yyval.address) = CURRENT_PROGRAM_SIZE;
          ins_byte(0);
      }
#line 12747 "y.tab.c" /* yacc.c:1646  */
    break;

  case 193:
#line 9653 "prolang.y" /* yacc.c:1646  */
    {
          /* Fix up the branch over the else part and return
           * the start address of the else part.
           */
          (yyval.address) = fix_branch( F_LBRANCH, CURRENT_PROGRAM_SIZE, (yyvsp[-1].address));
          (yyval.address) += (yyvsp[-1].address) + 1;
      }
#line 12759 "y.tab.c" /* yacc.c:1646  */
    break;

  case 194:
#line 9669 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) |  (yyvsp[0].number); }
#line 12765 "y.tab.c" /* yacc.c:1646  */
    break;

  case 195:
#line 9670 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) ^  (yyvsp[0].number); }
#line 12771 "y.tab.c" /* yacc.c:1646  */
    break;

  case 196:
#line 9671 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) &  (yyvsp[0].number); }
#line 12777 "y.tab.c" /* yacc.c:1646  */
    break;

  case 197:
#line 9672 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) == (yyvsp[0].number); }
#line 12783 "y.tab.c" /* yacc.c:1646  */
    break;

  case 198:
#line 9673 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) != (yyvsp[0].number); }
#line 12789 "y.tab.c" /* yacc.c:1646  */
    break;

  case 199:
#line 9674 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) >  (yyvsp[0].number); }
#line 12795 "y.tab.c" /* yacc.c:1646  */
    break;

  case 200:
#line 9675 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) >= (yyvsp[0].number); }
#line 12801 "y.tab.c" /* yacc.c:1646  */
    break;

  case 201:
#line 9676 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) <  (yyvsp[0].number); }
#line 12807 "y.tab.c" /* yacc.c:1646  */
    break;

  case 202:
#line 9677 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) <= (yyvsp[0].number); }
#line 12813 "y.tab.c" /* yacc.c:1646  */
    break;

  case 203:
#line 9678 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (p_uint)(yyvsp[0].number) > MAX_SHIFT ? 0 : (yyvsp[-2].number) << (yyvsp[0].number); }
#line 12819 "y.tab.c" /* yacc.c:1646  */
    break;

  case 204:
#line 9679 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (p_uint)(yyvsp[0].number) > MAX_SHIFT ? ((yyvsp[-2].number) >= 0 ? 0 : -1) : ((yyvsp[-2].number) >> (yyvsp[0].number)); }
#line 12825 "y.tab.c" /* yacc.c:1646  */
    break;

  case 205:
#line 9680 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (p_uint)(yyvsp[0].number) > MAX_SHIFT ? 0 : ((p_uint)(yyvsp[-2].number) >> (yyvsp[0].number)); }
#line 12831 "y.tab.c" /* yacc.c:1646  */
    break;

  case 206:
#line 9681 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) +  (yyvsp[0].number); }
#line 12837 "y.tab.c" /* yacc.c:1646  */
    break;

  case 207:
#line 9682 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) -  (yyvsp[0].number); }
#line 12843 "y.tab.c" /* yacc.c:1646  */
    break;

  case 208:
#line 9683 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) *  (yyvsp[0].number); }
#line 12849 "y.tab.c" /* yacc.c:1646  */
    break;

  case 209:
#line 9685 "prolang.y" /* yacc.c:1646  */
    {
          if ((yyvsp[0].number))
#line 9687 "prolang.y"
          {
              (yyval.number) = (yyvsp[-2].number) % (yyvsp[0].number);
          }
          else
#line 9691 "prolang.y"
          {
              yyerror("modulus by zero");
              (yyval.number) = 0;
          }
      }
#line 12867 "y.tab.c" /* yacc.c:1646  */
    break;

  case 210:
#line 9697 "prolang.y" /* yacc.c:1646  */
    {
          if ((yyvsp[0].number)) {
              (yyval.number) = (yyvsp[-2].number) / (yyvsp[0].number);
          } else {
              yyerror("division by zero");
              (yyval.number) = 0;
          }
      }
#line 12880 "y.tab.c" /* yacc.c:1646  */
    break;

  case 211:
#line 9705 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-1].number); }
#line 12886 "y.tab.c" /* yacc.c:1646  */
    break;

  case 212:
#line 9706 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = -(yyvsp[0].number); }
#line 12892 "y.tab.c" /* yacc.c:1646  */
    break;

  case 213:
#line 9707 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = !(yyvsp[0].number); }
#line 12898 "y.tab.c" /* yacc.c:1646  */
    break;

  case 214:
#line 9708 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = ~(yyvsp[0].number); }
#line 12904 "y.tab.c" /* yacc.c:1646  */
    break;

  case 216:
#line 9715 "prolang.y" /* yacc.c:1646  */
    {
          last_string_constant = last_lex_string;
          last_lex_string = NULL;
      }
#line 12913 "y.tab.c" /* yacc.c:1646  */
    break;

  case 217:
#line 9720 "prolang.y" /* yacc.c:1646  */
    {
          add_string_constant();
      }
#line 12921 "y.tab.c" /* yacc.c:1646  */
    break;

  case 218:
#line 9724 "prolang.y" /* yacc.c:1646  */
    { fatal("L_STRING LSTRING: presence of rule should prevent its reduction\n"); }
#line 12927 "y.tab.c" /* yacc.c:1646  */
    break;

  case 219:
#line 9726 "prolang.y" /* yacc.c:1646  */
    { fatal("L_STRING LSTRING: presence of rule should prevent its reduction\n"); }
#line 12933 "y.tab.c" /* yacc.c:1646  */
    break;

  case 222:
#line 9759 "prolang.y" /* yacc.c:1646  */
    {
          insert_pop_value();
      }
#line 12941 "y.tab.c" /* yacc.c:1646  */
    break;

  case 223:
#line 9765 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.rvalue).start = (yyvsp[-3].rvalue).start;
          (yyval.rvalue).type = (yyvsp[0].rvalue).type;

          free_fulltype((yyvsp[-3].rvalue).type);
      }
#line 12952 "y.tab.c" /* yacc.c:1646  */
    break;

  case 224:
#line 9791 "prolang.y" /* yacc.c:1646  */
    {
          if ((yyvsp[0].number) == F_LAND_EQ || (yyvsp[0].number) == F_LOR_EQ)
#line 9793 "prolang.y"
          {
              if (!add_lvalue_code(&(yyvsp[-1].lvalue), F_MAKE_PROTECTED))
#line 9795 "prolang.y"
              {
                  free_lpctype((yyvsp[-1].lvalue).type);
                  YYACCEPT;
              }

              /* Add the operator specific code */

              if ((yyvsp[0].number) == F_LAND_EQ)
#line 9803 "prolang.y"
              {
                  /* Insert the LDUP, LAND and remember the position */

                  ins_f_code(F_LDUP);
                  ins_f_code(F_LAND);
                  (yyval.address) = CURRENT_PROGRAM_SIZE;
                  ins_byte(0);
              }
              else if ((yyvsp[0].number) == F_LOR_EQ)
#line 9812 "prolang.y"
              {
                  /* Insert the LDUP, LOR and remember the position */

                  ins_f_code(F_LDUP);
                  ins_f_code(F_LOR);
                  (yyval.address) = CURRENT_PROGRAM_SIZE;
                  ins_byte(0);
              }
          }
      }
#line 12992 "y.tab.c" /* yacc.c:1646  */
    break;

  case 225:
#line 9825 "prolang.y" /* yacc.c:1646  */
    {
          fulltype_t type1, type2, restype;
#line 9828 "prolang.y"
          (yyval.rvalue) = (yyvsp[0].rvalue);

          type1.t_type = (yyvsp[-3].lvalue).type;
          type1.t_flags = 0;
          type2 = (yyvsp[0].rvalue).type;

          /* Check the validity of the assignment */
          /* There only need to be a common subset:
           *
           * int|string val = 10;
           * int result;
           *
           * if(intp(val))
           *    result = val;
           *
           * But if it's a zero (0), pass lpctype_mixed as the result type, because of:
           *
           *  string a;
           *  int b;
           *
           *  a = b = 0;
           */
          if(type2.t_type == lpctype_mixed && !type2.t_flags)
              restype.t_type = lpctype_mixed;
          else
              restype.t_type = get_common_type(type1.t_type, type2.t_type);
          restype.t_flags = type2.t_flags;
          if (exact_types && !restype.t_type)
#line 9856 "prolang.y"
          {
              switch((yyvsp[-2].number))
#line 9858 "prolang.y"
              {
              case F_LAND_EQ:
              case F_LOR_EQ:
                  break;

              case F_ADD_EQ:
                  free_lpctype(check_binary_op_types(type1.t_type, type2.t_type, "+=", types_add_assignment, NULL));
                  break;

              case F_SUB_EQ:
                  free_lpctype(check_binary_op_types(type1.t_type, type2.t_type, "-=", types_sub_assignment, NULL));
                  break;

              case F_MULT_EQ:
                  free_lpctype(check_binary_op_types(type1.t_type, type2.t_type, "*=", types_mul_assignment, NULL));
                  break;

              case F_DIV_EQ:
                  free_lpctype(check_binary_op_types(type1.t_type, type2.t_type, "/=", types_div_assignment, NULL));
                  break;

              case F_AND_EQ:
                  free_lpctype(check_binary_op_types(type1.t_type, type2.t_type, "&=", types_binary_and_assignment, NULL));
                  break;

              default:
                  yyerrorf("Bad assignment %s", get_two_fulltypes(type1, type2));

              } /* switch(assign op) */

              /* Operator assignment: result type is determined by assigned-to
               * type.
               */
              restype = ref_fulltype(type1);
          }

          /* Special checks for struct assignments */
          if (is_type_struct(type1.t_type) || is_type_struct(type2.t_type)
             )
#line 9897 "prolang.y"
          {
              free_fulltype(restype);
              restype = ref_fulltype(type1);
              if ((yyvsp[-2].number) != F_ASSIGN)
                  yyerror("Only plain assignment allowed for structs");
          }

          if ((yyvsp[-2].number) == F_LAND_EQ || (yyvsp[-2].number) == F_LOR_EQ)
#line 9905 "prolang.y"
          {
              /* Update the offset the earlier LAND/LOR instruction */

              if ((yyvsp[-2].number) == F_LAND_EQ)
#line 9909 "prolang.y"
              {
                  update_lop_branch((yyvsp[-1].address), F_LBRANCH_WHEN_ZERO);
              }
              else if ((yyvsp[-2].number) == F_LOR_EQ)
#line 9913 "prolang.y"
              {
                  update_lop_branch((yyvsp[-1].address), F_LBRANCH_WHEN_NON_ZERO);
              }

              /* Insert the SWAP and the ASSIGN */

              ins_f_code(F_SWAP_VALUES);
              ins_f_code(F_ASSIGN);
          }
          else
#line 9923 "prolang.y"
          {
              if (!add_lvalue_code(&(yyvsp[-3].lvalue), (yyvsp[-2].number)))
#line 9925 "prolang.y"
              {
                  free_lpctype((yyvsp[-3].lvalue).type);
                  free_fulltype((yyvsp[0].rvalue).type);
                  free_fulltype(restype);
                  YYACCEPT;
              }
          }
          (yyval.rvalue).type = restype;

          free_lpctype((yyvsp[-3].lvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 13117 "y.tab.c" /* yacc.c:1646  */
    break;

  case 226:
#line 9940 "prolang.y" /* yacc.c:1646  */
    {
          yyerror("Bad assignment: illegal lhs (target)");

          (yyval.rvalue) = (yyvsp[0].rvalue);
          (yyval.rvalue).type.t_type = lpctype_mixed;
          (yyval.rvalue).type.t_flags = 0;
          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 13130 "y.tab.c" /* yacc.c:1646  */
    break;

  case 227:
#line 9951 "prolang.y" /* yacc.c:1646  */
    {
          /* Insert the branch to the :-part and remember this address */
          ins_f_code(F_BRANCH_WHEN_ZERO);
          (yyval.address) = CURRENT_PROGRAM_SIZE;
          ins_byte(0);
      }
#line 13141 "y.tab.c" /* yacc.c:1646  */
    break;

  case 228:
#line 9960 "prolang.y" /* yacc.c:1646  */
    {
          /* Insert the branch over the :-part, and update
           * the earlier branch to the :-part.
           */

          p_int address, offset;

          address = (p_int)(yyvsp[-1].address);

          /* The branch to the end */
          ins_f_code(F_BRANCH);
          (yyval.address) = CURRENT_PROGRAM_SIZE;
          ins_byte(0);

          /* Update the earlier branch to point here */
          offset = CURRENT_PROGRAM_SIZE - ( address + 1);
          if (offset > 0xff - 1)
#line 9977 "prolang.y"
          {
              /* We have to make it a long branch and move the code
               * generated so far.
               */

              int i;
              bytecode_p p;

              (yyval.address) = CURRENT_PROGRAM_SIZE;
              ins_byte(0);
              p = PROGRAM_BLOCK + mem_block[A_PROGRAM].current_size-1;
              for (i = offset; --i >= 0; --p )
                  *p = p[-1];
              p[-2] = F_LBRANCH_WHEN_ZERO;
              upd_short(address, offset+2);
              if (offset > 0x7ffd)
                  yyerror("offset overflow");
          }
          else
#line 9996 "prolang.y"
          {
              mem_block[A_PROGRAM].block[address] = offset;
          }
      }
#line 13188 "y.tab.c" /* yacc.c:1646  */
    break;

  case 229:
#line 10003 "prolang.y" /* yacc.c:1646  */
    {
          /* Update the earlier branch skipping the :-part
           * and check the types of the two parts.
           */
          p_int address, old_address;
          int offset;
          fulltype_t type1, type2;

          last_expression = -1;

          old_address = (yyvsp[-4].address);
          address = (yyvsp[-2].address);
          offset = mem_block[A_PROGRAM].current_size - ( address + 1);
          if (offset > 0xff)
#line 10017 "prolang.y"
          {
              /* We have to make the branch a long branch.
               * This could also mean that the first branch now
               * have to become a long branch, too.
               */
              int i;
              bytecode_p p;

              ins_byte(0);
              p = PROGRAM_BLOCK + mem_block[A_PROGRAM].current_size-1;
              for( i = offset; --i >= 0; --p )
                  *p = p[-1];
              p[-2] = F_LBRANCH;
              upd_short(address, offset+2);
              if (offset > 0x7ffd)
                  yyerror("offset overflow");
              if ( mem_block[A_PROGRAM].block[old_address-1] ==
                  F_BRANCH_WHEN_ZERO )
                  mem_block[A_PROGRAM].block[old_address]++;
              else
                  upd_short(old_address,read_short(old_address)+1);
          }
          else
#line 10040 "prolang.y"
          {
              mem_block[A_PROGRAM].block[address] = offset;
          }

          /* Check the types and determine the result type */
          type1 = (yyvsp[-3].rvalue).type;
          type2 = (yyvsp[0].rvalue).type;

          (yyval.rvalue) = (yyvsp[-6].rvalue);
          (yyval.rvalue).type = get_fulltype(get_union_type(type1.t_type, type2.t_type));

          free_fulltype((yyvsp[-6].rvalue).type);
          free_fulltype((yyvsp[-3].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 13247 "y.tab.c" /* yacc.c:1646  */
    break;

  case 230:
#line 10058 "prolang.y" /* yacc.c:1646  */
    {
          /* Insert the LOR and remember the position */

          ins_f_code(F_LOR);
          (yyval.address) = CURRENT_PROGRAM_SIZE;
          ins_byte(0);
      }
#line 13259 "y.tab.c" /* yacc.c:1646  */
    break;

  case 231:
#line 10068 "prolang.y" /* yacc.c:1646  */
    {
          /* Update the offset the earlier LOR instruction */

          update_lop_branch((yyvsp[-1].address), F_LBRANCH_WHEN_NON_ZERO);

          (yyval.rvalue) = (yyvsp[-3].rvalue);

          /* Determine the result type */
          (yyval.rvalue).type.t_type = get_union_type((yyvsp[-3].rvalue).type.t_type, (yyvsp[0].rvalue).type.t_type);

          free_fulltype((yyvsp[-3].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 13277 "y.tab.c" /* yacc.c:1646  */
    break;

  case 232:
#line 10084 "prolang.y" /* yacc.c:1646  */
    {
          /* Insert the LAND and remember the position */

          ins_f_code(F_LAND);
          (yyval.address) = CURRENT_PROGRAM_SIZE;
          ins_byte(0);
      }
#line 13289 "y.tab.c" /* yacc.c:1646  */
    break;

  case 233:
#line 10094 "prolang.y" /* yacc.c:1646  */
    {
          /* Update the offset the earlier LAND instruction */

          update_lop_branch((yyvsp[-1].address), F_LBRANCH_WHEN_ZERO);

          (yyval.rvalue) = (yyvsp[-3].rvalue);

          /* Determine the result type */
          (yyval.rvalue).type = (yyvsp[0].rvalue).type; /* It's the second value or zero. */

          free_fulltype((yyvsp[-3].rvalue).type);
       }
#line 13306 "y.tab.c" /* yacc.c:1646  */
    break;

  case 234:
#line 10109 "prolang.y" /* yacc.c:1646  */
    {
          lpctype_t *result = check_binary_op_types((yyvsp[-2].rvalue).type.t_type, (yyvsp[0].rvalue).type.t_type, "|", types_binary_or, lpctype_mixed);

          (yyval.rvalue) = (yyvsp[-2].rvalue);
          (yyval.rvalue).type = get_fulltype(result);

          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);

          ins_f_code(F_OR);
      }
#line 13322 "y.tab.c" /* yacc.c:1646  */
    break;

  case 235:
#line 10123 "prolang.y" /* yacc.c:1646  */
    {
          lpctype_t *result = check_binary_op_types((yyvsp[-2].rvalue).type.t_type, (yyvsp[0].rvalue).type.t_type, "^", types_binary_or, lpctype_mixed);

          (yyval.rvalue) = (yyvsp[-2].rvalue);
          (yyval.rvalue).type = get_fulltype(result);

          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);

          ins_f_code(F_XOR);
      }
#line 13338 "y.tab.c" /* yacc.c:1646  */
    break;

  case 236:
#line 10137 "prolang.y" /* yacc.c:1646  */
    {
          lpctype_t *result = check_binary_op_types((yyvsp[-2].rvalue).type.t_type, (yyvsp[0].rvalue).type.t_type, "&", types_binary_and, lpctype_mixed);

          (yyval.rvalue) = (yyvsp[-2].rvalue);
          (yyval.rvalue).type = get_fulltype(result);

          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);

          ins_f_code(F_AND);
      }
#line 13354 "y.tab.c" /* yacc.c:1646  */
    break;

  case 237:
#line 10151 "prolang.y" /* yacc.c:1646  */
    {
          lpctype_t *result = check_binary_op_types((yyvsp[-2].rvalue).type.t_type, (yyvsp[0].rvalue).type.t_type, NULL, types_equality, NULL);

          if (result == NULL)
#line 10155 "prolang.y"
          {
              yyerrorf("== always false because of different types %s"
                      , get_two_fulltypes((yyvsp[-2].rvalue).type, (yyvsp[0].rvalue).type));
          }

          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);
          free_lpctype(result);

          ins_f_code(F_EQ);

          (yyval.rvalue) = (yyvsp[-2].rvalue);
          (yyval.rvalue).type = get_fulltype(lpctype_int);
      }
#line 13378 "y.tab.c" /* yacc.c:1646  */
    break;

  case 238:
#line 10172 "prolang.y" /* yacc.c:1646  */
    {
          lpctype_t *result = check_binary_op_types((yyvsp[-2].rvalue).type.t_type, (yyvsp[0].rvalue).type.t_type, NULL, types_equality, NULL);

          if (result == NULL)
#line 10176 "prolang.y"
          {
              yyerrorf("!= always true because of different types %s"
                      , get_two_fulltypes((yyvsp[-2].rvalue).type, (yyvsp[0].rvalue).type));
          }

          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);
          free_lpctype(result);

          ins_f_code(F_NE);

          (yyval.rvalue) = (yyvsp[-2].rvalue);
          (yyval.rvalue).type = get_fulltype(lpctype_int);
      }
#line 13402 "y.tab.c" /* yacc.c:1646  */
    break;

  case 239:
#line 10193 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.rvalue) = (yyvsp[-2].rvalue);
          (yyval.rvalue).type = get_fulltype(lpctype_int);

          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);

          ins_f_code(F_GT);
      }
#line 13416 "y.tab.c" /* yacc.c:1646  */
    break;

  case 240:
#line 10203 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.rvalue) = (yyvsp[-2].rvalue);
          (yyval.rvalue).type = get_fulltype(lpctype_int);

          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);

          ins_f_code(F_GE);
      }
#line 13430 "y.tab.c" /* yacc.c:1646  */
    break;

  case 241:
#line 10213 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.rvalue) = (yyvsp[-2].rvalue);
          (yyval.rvalue).type = get_fulltype(lpctype_int);

          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);

          ins_f_code(F_LT);
      }
#line 13444 "y.tab.c" /* yacc.c:1646  */
    break;

  case 242:
#line 10223 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.rvalue) = (yyvsp[-2].rvalue);
          (yyval.rvalue).type = get_fulltype(lpctype_int);

          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);

          ins_f_code(F_LE);
      }
#line 13458 "y.tab.c" /* yacc.c:1646  */
    break;

  case 243:
#line 10235 "prolang.y" /* yacc.c:1646  */
    {
          /* Just check the types. */
          free_lpctype(check_binary_op_types((yyvsp[-2].rvalue).type.t_type, (yyvsp[0].rvalue).type.t_type, "<<", types_shift, NULL));

          (yyval.rvalue) = (yyvsp[-2].rvalue);
          (yyval.rvalue).type = get_fulltype(lpctype_int);

          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);

          ins_f_code(F_LSH);
      }
#line 13475 "y.tab.c" /* yacc.c:1646  */
    break;

  case 244:
#line 10250 "prolang.y" /* yacc.c:1646  */
    {
          free_lpctype(check_binary_op_types((yyvsp[-2].rvalue).type.t_type, (yyvsp[0].rvalue).type.t_type, ">>", types_shift, NULL));

          (yyval.rvalue) = (yyvsp[-2].rvalue);
          (yyval.rvalue).type = get_fulltype(lpctype_int);

          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);

          ins_f_code(F_RSH);
      }
#line 13491 "y.tab.c" /* yacc.c:1646  */
    break;

  case 245:
#line 10264 "prolang.y" /* yacc.c:1646  */
    {
          free_lpctype(check_binary_op_types((yyvsp[-2].rvalue).type.t_type, (yyvsp[0].rvalue).type.t_type, ">>>", types_shift, NULL));

          (yyval.rvalue) = (yyvsp[-2].rvalue);
          (yyval.rvalue).type = get_fulltype(lpctype_int);

          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);

          ins_byte(F_RSHL);
      }
#line 13507 "y.tab.c" /* yacc.c:1646  */
    break;

  case 246:
#line 10278 "prolang.y" /* yacc.c:1646  */
    {
#line 10280 "prolang.y"
          (yyval.numbers)[0] = last_expression;
          (yyval.numbers)[1] = last_string_is_new;
      }
#line 13517 "y.tab.c" /* yacc.c:1646  */
    break;

  case 247:
#line 10284 "prolang.y" /* yacc.c:1646  */
    {
          mp_uint current_size;
          bytecode_p p;
#line 10288 "prolang.y"
          (yyval.rvalue) = (yyvsp[-3].rvalue);

          current_size = CURRENT_PROGRAM_SIZE;
          p = &(PROGRAM_BLOCK[current_size]);

          /* Check if we can combine strings: the last four bytes must be two 
           * CSTRINGx instructions.
           * TODO: handle F_STRING as well.
           */
          if (last_expression + 2 == current_size
           && (yyvsp[-1].numbers)[0] + 4 == (mp_int)current_size
           && ((p[-2]-(F_CSTRING0)) & ~3) == 0
           && ((p[-4]-(F_CSTRING0)) & ~3) == 0
             )
#line 10302 "prolang.y"
          {
              /* Yup, we can combine the two strings.
               */
              string_t *str1, *str2, *sum;
              int i;

              /* Retrieve both strings from the A_STRINGS area
               * and catenate them.
               */
              str1 = PROG_STRING( p[-3] | (p[-4]-(F_CSTRING0))<<8 );
              str2 = PROG_STRING( p[-1] | (p[-2]-(F_CSTRING0))<<8 );
              sum = mstr_add(str1, str2);
              if (!sum)
#line 10315 "prolang.y"
              {
                  yyerrorf("Out of memory for string literal (%zu bytes)"
                          , (mstrsize(str1)+mstrsize(str2))
                          );
                  YYACCEPT;
              }

              /* If possible, try to delete the constituent strings
               * from the string area.
               */
              if (last_string_is_new)
                  delete_prog_string();
              if ((yyvsp[-1].numbers)[1])
                  delete_prog_string();

              /* Store the new string and update the CSTRING
               * instructions.
               */
              sum = make_tabled(sum);
              if (!sum)
#line 10335 "prolang.y"
              {
                  yyerror("Out of memory for string literal");
                  YYACCEPT;
              }
              i = store_prog_string(sum);

              last_expression = current_size - 4;
              if (i < 0x400)
#line 10343 "prolang.y"
              {
                  p[-4] = F_CSTRING0 + (i>>8);
                  p[-3] = i;
                  CURRENT_PROGRAM_SIZE = current_size - 2;
              }
              else
#line 10349 "prolang.y"
              {
                  p[-4] = F_STRING;
                  upd_short(current_size - 3, i);
                  CURRENT_PROGRAM_SIZE = current_size - 1;
              }
              (yyval.rvalue).type = get_fulltype(lpctype_string);
          }
          else
#line 10357 "prolang.y"
          {
              /* Just add */
              lpctype_t *result = check_binary_op_types((yyvsp[-3].rvalue).type.t_type, (yyvsp[0].rvalue).type.t_type, "+", types_addition, lpctype_mixed);
              (yyval.rvalue).type = get_fulltype(result);

              ins_f_code(F_ADD);
          }

          free_fulltype((yyvsp[-3].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 13612 "y.tab.c" /* yacc.c:1646  */
    break;

  case 248:
#line 10371 "prolang.y" /* yacc.c:1646  */
    {
#line 10373 "prolang.y"
          (yyval.rvalue) = (yyvsp[-2].rvalue);
          lpctype_t *result = check_binary_op_types((yyvsp[-2].rvalue).type.t_type, (yyvsp[0].rvalue).type.t_type, "-", types_subtraction, lpctype_mixed);
          (yyval.rvalue).type = get_fulltype(result);

          ins_f_code(F_SUBTRACT);
          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 13627 "y.tab.c" /* yacc.c:1646  */
    break;

  case 249:
#line 10384 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.rvalue) = (yyvsp[-2].rvalue);
          lpctype_t *result = check_binary_op_types((yyvsp[-2].rvalue).type.t_type, (yyvsp[0].rvalue).type.t_type, "*", types_multiplication, lpctype_mixed);
          (yyval.rvalue).type = get_fulltype(result);

          ins_f_code(F_MULTIPLY);
          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 13641 "y.tab.c" /* yacc.c:1646  */
    break;

  case 250:
#line 10396 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.rvalue) = (yyvsp[-2].rvalue);
          lpctype_t *result = check_binary_op_types((yyvsp[-2].rvalue).type.t_type, (yyvsp[0].rvalue).type.t_type, "%", types_modulus, lpctype_int);
          (yyval.rvalue).type = get_fulltype(result);

          ins_f_code(F_MOD);
          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 13655 "y.tab.c" /* yacc.c:1646  */
    break;

  case 251:
#line 10407 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.rvalue) = (yyvsp[-2].rvalue);
          lpctype_t *result = check_binary_op_types((yyvsp[-2].rvalue).type.t_type, (yyvsp[0].rvalue).type.t_type, "/", types_division, lpctype_int);
          (yyval.rvalue).type = get_fulltype(result);

          ins_f_code(F_DIVIDE);
          free_fulltype((yyvsp[-2].rvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 13669 "y.tab.c" /* yacc.c:1646  */
    break;

  case 252:
#line 10419 "prolang.y" /* yacc.c:1646  */
    {
          /* Declarative casts are legal from general to more specialized types.
           * So we're getting the common type between the cast expression
           * and the value, and if it's different, that means, it's more specialized.
           *
           * We always allow casts from lpctype_unknown and lpctype_mixed
           * (because lpctype_unknown must be cast to avoid type errors,
           * and changing the strict_types pragma to strong_types - and therefore
           * the result type of call_other from unknown to mixed - should not
           * introduce additional errors).
           */

          (yyval.rvalue) = (yyvsp[0].rvalue);

          if ((yyvsp[0].rvalue).type.t_type == lpctype_unknown || (yyvsp[0].rvalue).type.t_type == lpctype_mixed)
#line 10434 "prolang.y"
          {
              (yyval.rvalue).type = get_fulltype((yyvsp[-1].lpctype));
          }
          else
#line 10438 "prolang.y"
          {
              lpctype_t *result = get_common_type((yyvsp[-1].lpctype), (yyvsp[0].rvalue).type.t_type);

              if(result == NULL || result == (yyvsp[0].rvalue).type.t_type)
#line 10442 "prolang.y"
              {
                  /* No common type or not specialized. */
                  yyerrorf("Declarative casts are only legal from general to specialized types: %s", get_two_lpctypes((yyvsp[0].rvalue).type.t_type, (yyvsp[-1].lpctype)));
              }

              if(result == NULL)
                  (yyval.rvalue).type = get_fulltype((yyvsp[-1].lpctype));
              else
#line 10450 "prolang.y"
              {
                  (yyval.rvalue).type = get_fulltype(result);
                  free_lpctype((yyvsp[-1].lpctype));
              }
          }

          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 13717 "y.tab.c" /* yacc.c:1646  */
    break;

  case 253:
#line 10461 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.rvalue) = (yyvsp[0].rvalue);
          (yyval.rvalue).type = get_fulltype((yyvsp[-1].lpctype));

          /* We are trying to convert the value to the new type
           * and give an error, when we can't find a suitable conversion.
           * If the source or destination type is mixed or unknown,
           * we just do a declarative cast.
           */

          if((yyvsp[-1].lpctype) == lpctype_mixed || (yyvsp[0].rvalue).type.t_type == lpctype_mixed || (yyvsp[0].rvalue).type.t_type == lpctype_unknown)
#line 10472 "prolang.y"
          {
              /* Do nothing... */
          }
          else if((yyvsp[-1].lpctype) == (yyvsp[0].rvalue).type.t_type)
#line 10476 "prolang.y"
          {
              if(pragma_warn_empty_casts)
                  yywarnf("casting a value to its own type: %s", get_lpctype_name((yyvsp[-1].lpctype)));
          }
          else if((yyvsp[-1].lpctype) == lpctype_int)
#line 10481 "prolang.y"
          {
              ins_f_code(F_TO_INT);
          }
          else if((yyvsp[-1].lpctype) == lpctype_float)
#line 10485 "prolang.y"
          {
              ins_f_code(F_TO_FLOAT);
          }
          else if((yyvsp[-1].lpctype) == lpctype_string)
#line 10489 "prolang.y"
          {
              ins_f_code(F_TO_STRING);
          }
          else if((yyvsp[-1].lpctype) == lpctype_object)
#line 10493 "prolang.y"
          {
              ins_f_code(F_TO_OBJECT);
          }
          else if((yyvsp[-1].lpctype) == lpctype_int_array)
#line 10497 "prolang.y"
          {
              ins_f_code(F_TO_ARRAY);
          }
          else if(is_type_struct((yyvsp[-1].lpctype)))
#line 10501 "prolang.y"
          {
              /* Do nothing, just adapt the type information */
          }
          else
#line 10505 "prolang.y"
          {
              lpctype_error("Illegal cast", (yyvsp[-1].lpctype));
          }

          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 13781 "y.tab.c" /* yacc.c:1646  */
    break;

  case 254:
#line 10514 "prolang.y" /* yacc.c:1646  */
    {
          lpctype_t *result;
          (yyval.rvalue).start = (yyvsp[-1].incdec).start;

          if (!add_lvalue_code(&(yyvsp[0].lvalue), (yyvsp[-1].incdec).code))
#line 10519 "prolang.y"
          {
              free_lpctype((yyvsp[0].lvalue).type);
              YYACCEPT;
          }

          result = get_common_type((yyvsp[0].lvalue).type, lpctype_int_float);
          if(result == NULL)
#line 10526 "prolang.y"
          {
              argument_type_error((yyvsp[-1].incdec).code, (yyvsp[0].lvalue).type);
              result = lpctype_int_float;
          }

          (yyval.rvalue).type = get_fulltype(result);

          free_lpctype((yyvsp[0].lvalue).type);
      }
#line 13809 "y.tab.c" /* yacc.c:1646  */
    break;

  case 255:
#line 10537 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.rvalue) = (yyvsp[0].rvalue);
          last_expression = CURRENT_PROGRAM_SIZE;
          ins_f_code(F_NOT);        /* Any type is valid here. */
          (yyval.rvalue).type = get_fulltype(lpctype_int);

          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 13822 "y.tab.c" /* yacc.c:1646  */
    break;

  case 256:
#line 10548 "prolang.y" /* yacc.c:1646  */
    {
#line 10550 "prolang.y"
          (yyval.rvalue) = (yyvsp[0].rvalue);
          if (exact_types && !lpctype_contains(lpctype_int, (yyvsp[0].rvalue).type.t_type))
              fulltype_error("Bad argument to ~", (yyvsp[0].rvalue).type);

          ins_f_code(F_COMPL);
          (yyval.rvalue).type = get_fulltype(lpctype_int);

          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 13838 "y.tab.c" /* yacc.c:1646  */
    break;

  case 257:
#line 10562 "prolang.y" /* yacc.c:1646  */
    {
#line 10564 "prolang.y"
          if (CURRENT_PROGRAM_SIZE - last_expression == 2
           && mem_block[A_PROGRAM].block[last_expression] ==
                F_CLIT )
#line 10567 "prolang.y"
          {
              mem_block[A_PROGRAM].block[last_expression] =
                F_NCLIT;
          }
          else if (CURRENT_PROGRAM_SIZE - last_expression == 1
           && mem_block[A_PROGRAM].block[last_expression] ==
                F_CONST1 )
#line 10574 "prolang.y"
          {
              mem_block[A_PROGRAM].block[last_expression] =
                F_NCONST1;
          }
          else if (CURRENT_PROGRAM_SIZE - last_expression == 1 + sizeof(p_int)
           && mem_block[A_PROGRAM].block[last_expression] ==
                F_NUMBER )
#line 10581 "prolang.y"
          {
              p_int number;
              number = read_p_int(last_expression + 1);
              number = -number;
              upd_p_int(last_expression + 1, number);
          }
          else
#line 10588 "prolang.y"
          {
              ins_f_code(F_NEGATE);
          }

          (yyval.rvalue) = (yyvsp[0].rvalue);
          (yyval.rvalue).type = get_fulltype(check_unary_op_type((yyvsp[0].rvalue).type.t_type, "unary '-'", types_unary_math, lpctype_mixed));

          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 13882 "y.tab.c" /* yacc.c:1646  */
    break;

  case 258:
#line 10600 "prolang.y" /* yacc.c:1646  */
    {
#line 10602 "prolang.y"
          /* Create the code to push the lvalue plus POST_INC */
          (yyval.rvalue).start = CURRENT_PROGRAM_SIZE;
          if (!add_lvalue_code(&(yyvsp[-1].lvalue), F_POST_INC))
#line 10605 "prolang.y"
          {
              free_lpctype((yyvsp[-1].lvalue).type);
              YYACCEPT;
          }

          /* Check the types */
          (yyval.rvalue).type = get_fulltype(check_unary_op_type((yyvsp[-1].lvalue).type, "++", types_unary_math, lpctype_mixed));

          free_lpctype((yyvsp[-1].lvalue).type);
      }
#line 13903 "y.tab.c" /* yacc.c:1646  */
    break;

  case 259:
#line 10618 "prolang.y" /* yacc.c:1646  */
    {
#line 10620 "prolang.y"
          (yyval.rvalue).start = CURRENT_PROGRAM_SIZE;

          /* Create the code to push the lvalue plus POST_DEC */
          if (!add_lvalue_code(&(yyvsp[-1].lvalue), F_POST_DEC))
#line 10624 "prolang.y"
          {
              free_lpctype((yyvsp[-1].lvalue).type);
              YYACCEPT;
          }

          /* Check the types */
          (yyval.rvalue).type = get_fulltype(check_unary_op_type((yyvsp[-1].lvalue).type, "--", types_unary_math, NULL));

          free_lpctype((yyvsp[-1].lvalue).type);
      }
#line 13925 "y.tab.c" /* yacc.c:1646  */
    break;

  case 260:
#line 10638 "prolang.y" /* yacc.c:1646  */
    {
          uint32 current = CURRENT_PROGRAM_SIZE;

          if (!(yyvsp[0].lvalue).lvalue.size)
#line 10642 "prolang.y"
          {
              current = (yyvsp[0].lvalue).lvalue.start;
          }
          else if(!add_lvalue_code(&(yyvsp[0].lvalue), F_MAKE_PROTECTED))
#line 10646 "prolang.y"
          {
              free_lpctype((yyvsp[0].lvalue).type);
              YYACCEPT;
          }

          (yyval.rvalue).start = current;
          (yyval.rvalue).type = get_fulltype((yyvsp[0].lvalue).type); /* Adapt the reference. */
          (yyval.rvalue).type.t_flags |= TYPE_MOD_REFERENCE;
      }
#line 13949 "y.tab.c" /* yacc.c:1646  */
    break;

  case 261:
#line 10658 "prolang.y" /* yacc.c:1646  */
    {
          fulltype_t type1, type2, restype;
#line 10661 "prolang.y"
          (yyval.rvalue) = (yyvsp[0].rvalue);

          if ((yyvsp[-1].number) != F_ASSIGN)
              yyerror("Only plain assignments allowed with references");

          /* Change the instruction into a reseating instruction. */
          if ((yyvsp[-2].lvalue).vlvalue_inst)
              LVALUE_BLOCK[(yyvsp[-2].lvalue).lvalue.start + (yyvsp[-2].lvalue).lvalue.size - (yyvsp[-2].lvalue).num_arg - 1] = (yyvsp[-2].lvalue).vlvalue_inst;
          else
              yyerror("Illegal rhs for assignment");

          type1.t_type = (yyvsp[-2].lvalue).type;
          type1.t_flags = 0;
          type2 = (yyvsp[0].rvalue).type;

          if(type2.t_type == lpctype_mixed && !type2.t_flags)
              restype.t_type = lpctype_mixed;
          else
              restype.t_type = get_common_type(type1.t_type, type2.t_type);
          restype.t_flags = type2.t_flags;

          if (exact_types && !restype.t_type)
#line 10683 "prolang.y"
          {
              yyerrorf("Bad assignment %s", get_two_fulltypes(type1, type2));
              restype = ref_fulltype(type1);
          }

          /* Special checks for struct assignments */
          if (is_type_struct(type1.t_type) || is_type_struct(type2.t_type))
#line 10690 "prolang.y"
          {
              free_fulltype(restype);
              restype = ref_fulltype(type1);
          }

          if (!add_lvalue_code(&(yyvsp[-2].lvalue), (yyvsp[-1].number)))
#line 10696 "prolang.y"
          {
              free_lpctype((yyvsp[-2].lvalue).type);
              free_fulltype((yyvsp[0].rvalue).type);
              free_fulltype(restype);
              YYACCEPT;
          }

          (yyval.rvalue).type = restype;

          free_lpctype((yyvsp[-2].lvalue).type);
          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 14007 "y.tab.c" /* yacc.c:1646  */
    break;

  case 262:
#line 10711 "prolang.y" /* yacc.c:1646  */
    {
#line 10713 "prolang.y"
          (yyval.rvalue).start = (yyvsp[0].lrvalue).start;
          (yyval.rvalue).type = (yyvsp[0].lrvalue).type;

          free_lvalue_block((yyvsp[0].lrvalue).lvalue);
      }
#line 14019 "y.tab.c" /* yacc.c:1646  */
    break;

  case 263:
#line 10725 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.lvalue) = (yyvsp[0].lvalue);
      }
#line 14027 "y.tab.c" /* yacc.c:1646  */
    break;

  case 264:
#line 10731 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.lvalue) = (yyvsp[-1].lvalue);
      }
#line 14035 "y.tab.c" /* yacc.c:1646  */
    break;

  case 265:
#line 10736 "prolang.y" /* yacc.c:1646  */
    {
          /* We abuse the .lvalue.start to save the start
           * of the expression in the program code.
           */
          (yyval.lvalue).lvalue = (lvalue_block_t) {(yyvsp[-1].function_call_result).start, 0};
          (yyval.lvalue).type = (yyvsp[-1].function_call_result).type.t_type;
          (yyval.lvalue).vlvalue_inst = 0;
          (yyval.lvalue).num_arg = 0;
      }
#line 14049 "y.tab.c" /* yacc.c:1646  */
    break;

  case 266:
#line 10750 "prolang.y" /* yacc.c:1646  */
    { (yyval.incdec).code = F_PRE_INC; (yyval.incdec).start = CURRENT_PROGRAM_SIZE; }
#line 14055 "y.tab.c" /* yacc.c:1646  */
    break;

  case 267:
#line 10751 "prolang.y" /* yacc.c:1646  */
    { (yyval.incdec).code = F_PRE_DEC; (yyval.incdec).start = CURRENT_PROGRAM_SIZE; }
#line 14061 "y.tab.c" /* yacc.c:1646  */
    break;

  case 268:
#line 10757 "prolang.y" /* yacc.c:1646  */
    {
          /* And add an opcode to make it into an rvalue,
           * just to be on the safe side.
           */
          if ((yyvsp[0].function_call_result).might_lvalue)
              ins_f_code(F_MAKE_RVALUE);

          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
          (yyval.lrvalue).start =  (yyvsp[0].function_call_result).start;
          (yyval.lrvalue).type =   (yyvsp[0].function_call_result).type;
      }
#line 14077 "y.tab.c" /* yacc.c:1646  */
    break;

  case 269:
#line 10769 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
          (yyval.lrvalue).start =  (yyvsp[0].rvalue).start;
          (yyval.lrvalue).type =   (yyvsp[0].rvalue).type;
      }
#line 14087 "y.tab.c" /* yacc.c:1646  */
    break;

  case 270:
#line 10775 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
          (yyval.lrvalue).start =  (yyvsp[0].rvalue).start;
          (yyval.lrvalue).type =   (yyvsp[0].rvalue).type;
      }
#line 14097 "y.tab.c" /* yacc.c:1646  */
    break;

  case 271:
#line 10781 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
          (yyval.lrvalue).start =  (yyvsp[0].rvalue).start;
          (yyval.lrvalue).type =   (yyvsp[0].rvalue).type;
      }
#line 14107 "y.tab.c" /* yacc.c:1646  */
    break;

  case 272:
#line 10788 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
          (yyval.lrvalue).start =  (yyvsp[0].rvalue).start;
          (yyval.lrvalue).type =   (yyvsp[0].rvalue).type;
      }
#line 14117 "y.tab.c" /* yacc.c:1646  */
    break;

  case 273:
#line 10797 "prolang.y" /* yacc.c:1646  */
    {
          /* Push a constant string */

          string_t *p;
#line 10802 "prolang.y"
          p = last_lex_string;
          last_lex_string = NULL;
          (yyval.lrvalue).start = last_expression = CURRENT_PROGRAM_SIZE;
          (yyval.lrvalue).type = get_fulltype(lpctype_string);
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};

          ins_prog_string(p);
      }
#line 14135 "y.tab.c" /* yacc.c:1646  */
    break;

  case 274:
#line 10813 "prolang.y" /* yacc.c:1646  */
    {
          /* Store a number */

          p_int current;
          p_int number;
          PREPARE_INSERT(1 + sizeof (p_int))
#line 10820 "prolang.y"
          (yyval.lrvalue).start = last_expression = current = CURRENT_PROGRAM_SIZE;
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
          number = (yyvsp[0].number);
          if ( number == 0 )
#line 10824 "prolang.y"
          {
              current++;
              add_f_code(F_CONST0);
              (yyval.lrvalue).type = get_fulltype(lpctype_mixed);
              /* TODO: Introduce a TYPE_NULL instead */
          }
          else if ( number == 1 )
#line 10831 "prolang.y"
          {
              add_f_code(F_CONST1);
              current++;
              (yyval.lrvalue).type = get_fulltype(lpctype_int);
          }
          else if ( number >= 0 && number <= 0xff )
#line 10837 "prolang.y"
          {
              add_f_code(F_CLIT);
              add_byte(number);
              current += 2;
              (yyval.lrvalue).type = get_fulltype(lpctype_int);
          }
          else if ( number < 0 && number >= -0x0ff )
#line 10844 "prolang.y"
          {
              add_f_code(F_NCLIT);
              add_byte(-number);
              current += 2;
              (yyval.lrvalue).type = get_fulltype(lpctype_int);
          }
          else
#line 10851 "prolang.y"
          {
              add_f_code(F_NUMBER);
              upd_p_int((char*)__PREPARE_INSERT__p - mem_block[A_PROGRAM].block, (yyvsp[0].number));
              current += 1 + sizeof (p_int);
              (yyval.lrvalue).type = get_fulltype(lpctype_int);
          }
          CURRENT_PROGRAM_SIZE = current;
      }
#line 14191 "y.tab.c" /* yacc.c:1646  */
    break;

  case 275:
#line 10862 "prolang.y" /* yacc.c:1646  */
    {
          int ix, inhIndex;

          (yyval.lrvalue).start = CURRENT_PROGRAM_SIZE;
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
          if (!pragma_warn_deprecated)
              ins_byte(F_NO_WARN_DEPRECATED);
          ix = (yyvsp[0].closure).number;
          inhIndex = (yyvsp[0].closure).inhIndex;

          // check for deprecated functions



          if (ix < CLOSURE_EFUN_OFFS)

#line 10878 "prolang.y"
          {
              // check only closures not directly to inherited functions (#'::fun),
              // they were checked by the lexxer.
              if (!inhIndex && ix < FUNCTION_COUNT)
#line 10882 "prolang.y"
              {
                  // ok, closure to lfun.
                  function_t *fun = FUNCTION(ix);
                  if (fun->flags & TYPE_MOD_DEPRECATED)
#line 10886 "prolang.y"
                  {
                      yywarnf("Creating lfun closure to deprecated function %s",
                              get_txt(fun->name));
                  }
              }
              else if (ix >= CLOSURE_IDENTIFIER_OFFS)
#line 10892 "prolang.y"
              {
                  // closure to global variable
                  // the lexxer only creates closure to non-virtual variables - our luck ;)
                  variable_t *varp = NV_VARIABLE(ix - CLOSURE_IDENTIFIER_OFFS - num_virtual_variables);
                  if (varp->type.t_flags & TYPE_MOD_DEPRECATED)
                      yywarnf("Creating closure to deprecated global variable %s.\n",
                              get_txt(varp->name));
              }
          }
          ins_f_code(F_CLOSURE);
          ins_short(ix);
          ins_short(inhIndex);
          (yyval.lrvalue).type = get_fulltype(lpctype_closure);
      }
#line 14244 "y.tab.c" /* yacc.c:1646  */
    break;

  case 276:
#line 10909 "prolang.y" /* yacc.c:1646  */
    {
          /* Generate a symbol */

          int string_number;
          int quotes;

          (yyval.lrvalue).start = CURRENT_PROGRAM_SIZE;
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
          quotes = (yyvsp[0].symbol).quotes;
          string_number = store_prog_string((yyvsp[0].symbol).name);
          if (quotes == 1 && string_number < 0x100)
#line 10920 "prolang.y"
          {
                /* One byte shorter than the other way */
                ins_f_code(F_CSTRING0);
                ins_byte(string_number);
                ins_f_code(F_QUOTE);
          }
          else
#line 10927 "prolang.y"
          {
                ins_f_code(F_SYMBOL);
                ins_short(string_number);
                ins_byte(quotes);
          }
          (yyval.lrvalue).type = get_fulltype(lpctype_symbol);
      }
#line 14276 "y.tab.c" /* yacc.c:1646  */
    break;

  case 277:
#line 10937 "prolang.y" /* yacc.c:1646  */
    {
          /* Generate a float literal */

          (yyval.lrvalue).start = CURRENT_PROGRAM_SIZE;
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
          ins_f_code(F_FLOAT);
#ifdef FLOAT_FORMAT_2
          ins_double((yyvsp[0].float_number));
#else
          int exponent;
          ins_uint32 ( SPLIT_DOUBLE( (yyvsp[0].float_number), &exponent) );
          ins_uint16 ( exponent );
#endif  /* FLOAT_FORMAT_2 */
          (yyval.lrvalue).type = get_fulltype(lpctype_float);
      }
#line 14296 "y.tab.c" /* yacc.c:1646  */
    break;

  case 278:
#line 10955 "prolang.y" /* yacc.c:1646  */
    {
          /* A nested expression */

          (yyval.lrvalue).type = (yyvsp[-1].rvalue).type;
          (yyval.lrvalue).start = (yyvsp[-2].address);
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
      }
#line 14308 "y.tab.c" /* yacc.c:1646  */
    break;

  case 279:
#line 10965 "prolang.y" /* yacc.c:1646  */
    {
          /* Generate an array */

          ins_f_code(F_AGGREGATE);
          ins_short((yyvsp[-2].number));
          if (max_array_size && (yyvsp[-2].number) > (p_int)max_array_size)
              yyerror("Illegal array size");
          (yyval.lrvalue).type = get_fulltype(get_aggregate_type((yyvsp[-2].number)));
          (yyval.lrvalue).start = (yyvsp[-3].address);
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
      }
#line 14324 "y.tab.c" /* yacc.c:1646  */
    break;

  case 280:
#line 10979 "prolang.y" /* yacc.c:1646  */
    {
          /* Generate a quoted array by generating a normal
           * array first and then applying QUOTE as often
           * as possible.
           */

          int quotes;

          pop_arg_stack((yyvsp[-2].number));

          ins_f_code(F_AGGREGATE);
          ins_short((yyvsp[-2].number));
          if (max_array_size && (yyvsp[-2].number) > (p_int)max_array_size)
              yyerror("Illegal array size");
          (yyval.lrvalue).type = get_fulltype(lpctype_quoted_array);
          (yyval.lrvalue).start = (yyvsp[-3].address);
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
          quotes = (yyvsp[-4].number);
          do {
                ins_f_code(F_QUOTE);
          } while (--quotes);
      }
#line 14351 "y.tab.c" /* yacc.c:1646  */
    break;

  case 281:
#line 11007 "prolang.y" /* yacc.c:1646  */
    {
          ins_number(0);
      }
#line 14359 "y.tab.c" /* yacc.c:1646  */
    break;

  case 282:
#line 11013 "prolang.y" /* yacc.c:1646  */
    {
          ins_f_code(F_M_ALLOCATE);

          (yyval.lrvalue).type = get_fulltype(lpctype_mapping);
          (yyval.lrvalue).start = (yyvsp[-4].address);
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};

          free_fulltype((yyvsp[-2].rvalue).type);
      }
#line 14373 "y.tab.c" /* yacc.c:1646  */
    break;

  case 283:
#line 11025 "prolang.y" /* yacc.c:1646  */
    {
          /* Generate a mapping */

          mp_int num_keys;

          pop_arg_stack((yyvsp[-2].numbers)[0]);
          num_keys = (yyvsp[-2].numbers)[0] / ((yyvsp[-2].numbers)[1]+1);

          if ((num_keys|(yyvsp[-2].numbers)[1]) & ~0xffff)
              yyerror("cannot handle more than 65535 keys/values "
                      "in mapping aggregate");

          if ( (num_keys | (yyvsp[-2].numbers)[1]) &~0xff)
#line 11038 "prolang.y"
          {
              ins_f_code(F_M_AGGREGATE);
              ins_short(num_keys);
              ins_short((yyvsp[-2].numbers)[1]);
          }
          else
#line 11044 "prolang.y"
          {
              ins_f_code(F_M_CAGGREGATE);
              ins_byte(num_keys);
              ins_byte((yyvsp[-2].numbers)[1]);
          }

          (yyval.lrvalue).type = get_fulltype(lpctype_mapping);
          (yyval.lrvalue).start = (yyvsp[-3].address);
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
      }
#line 14409 "y.tab.c" /* yacc.c:1646  */
    break;

  case 284:
#line 11057 "prolang.y" /* yacc.c:1646  */
    {
          yyerror("Missing identifier for empty struct literal");
          (yyval.lrvalue).type = get_fulltype(lpctype_unknown);
          (yyval.lrvalue).start = (yyvsp[-2].address);
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
      }
#line 14420 "y.tab.c" /* yacc.c:1646  */
    break;

  case 285:
#line 11064 "prolang.y" /* yacc.c:1646  */
    {
          /* Rule allows the parser to resynchronize after errors */
          (yyval.lrvalue).type = get_fulltype(lpctype_unknown);
          (yyval.lrvalue).start = (yyvsp[-2].address);
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
      }
#line 14431 "y.tab.c" /* yacc.c:1646  */
    break;

  case 286:
#line 11071 "prolang.y" /* yacc.c:1646  */
    {
          int num;

          num = find_struct((yyvsp[-1].sh_string));
          if (num < 0)
#line 11076 "prolang.y"
          {
              yyerrorf("Unknown struct '%s'", get_txt((yyvsp[-1].sh_string)));
              YYACCEPT;
          }
          (yyval.number) = num;
          free_mstring((yyvsp[-1].sh_string));
      }
#line 14449 "y.tab.c" /* yacc.c:1646  */
    break;

  case 287:
#line 11086 "prolang.y" /* yacc.c:1646  */
    {
          /* Generate a literal struct */

          int num = (yyvsp[-3].number);
          struct_def_t *pdef = &(STRUCT_DEF(num));

          if ((yyvsp[-1].struct_init_list).length > STRUCT_MAX_MEMBERS
           || (yyvsp[-1].struct_init_list).length > struct_t_size(pdef->type))
#line 11094 "prolang.y"
          {
              /* Too many elements - create an empty struct */
              yyerrorf("Too many elements for literal struct '%s'"
                      , get_txt(struct_t_name(pdef->type)));
              CURRENT_PROGRAM_SIZE = (yyvsp[-2].address);
              create_struct_literal(pdef, 0, NULL);
          }
          else if (!create_struct_literal(pdef, (yyvsp[-1].struct_init_list).length, (yyvsp[-1].struct_init_list).list))
#line 11102 "prolang.y"
          {
              /* Creation failed - create an empty struct */
              CURRENT_PROGRAM_SIZE = (yyvsp[-2].address);
              create_struct_literal(pdef, 0, NULL);
          }

          /* Free the list of member descriptors */
          while ((yyvsp[-1].struct_init_list).list != NULL)
#line 11110 "prolang.y"
          {
              struct_init_t * p = (yyvsp[-1].struct_init_list).list;
              (yyvsp[-1].struct_init_list).list = p->next;
              if (p->name != NULL)
                  free_mstring(p->name);
              free_fulltype(p->type);
              xfree(p);
          }

          (yyval.lrvalue).type = get_fulltype(get_struct_type(pdef->type));
          (yyval.lrvalue).start = (yyvsp[-2].address);
          (yyval.lrvalue).lvalue = (lvalue_block_t) {0, 0};
      }
#line 14494 "y.tab.c" /* yacc.c:1646  */
    break;

  case 288:
#line 11127 "prolang.y" /* yacc.c:1646  */
    {
          /* Access a global variable */
          int i;
          mp_uint current;
          bytecode_p p;
          variable_t *varp;
#line 11134 "prolang.y"
          i = verify_declared((yyvsp[0].ident));
          if (i == -1)
              /* variable not declared */
              YYACCEPT;

          (yyval.lrvalue).start = current = CURRENT_PROGRAM_SIZE;

          if (!realloc_a_program(3))
#line 11142 "prolang.y"
          {
              yyerrorf("Out of memory: program size %"PRIuMPINT"\n", current+3);
              YYACCEPT;
          }
          p = PROGRAM_BLOCK + current;

          if (i & VIRTUAL_VAR_TAG)
#line 11149 "prolang.y"
          {
              /* Access a virtual variable */

              bytecode_p q;

              (yyval.lrvalue).lvalue = alloc_lvalue_block(2);
              q = LVALUE_BLOCK + (yyval.lrvalue).lvalue.start;

              q[0] = F_PUSH_VIRTUAL_VARIABLE_LVALUE;
              q[1] = i;

              *p++ = F_VIRTUAL_VARIABLE;
              *p = i;

              varp = V_VARIABLE(i);
              (yyval.lrvalue).type = ref_fulltype(varp->type);
          }
          else
#line 11167 "prolang.y"
          {
              /* Access a non-virtual variable */

              if (i & ~0xff)
#line 11171 "prolang.y"
              {
                  bytecode_p q;

                  (yyval.lrvalue).lvalue = alloc_lvalue_block(3);
                  q = LVALUE_BLOCK + (yyval.lrvalue).lvalue.start;

                  q[0] = F_PUSH_IDENTIFIER16_LVALUE;
                  PUT_SHORT(q+1, i);

                  *p = F_IDENTIFIER16;
                  upd_short(++current, i);
              }
              else
#line 11184 "prolang.y"
              {
                  bytecode_p q;

                  (yyval.lrvalue).lvalue = alloc_lvalue_block(2);
                  q = LVALUE_BLOCK + (yyval.lrvalue).lvalue.start;

                  q[0] = F_PUSH_IDENTIFIER_LVALUE;
                  q[1] = i;

                  *p++ = F_IDENTIFIER;
                  *p = i;
              }
              varp = NV_VARIABLE(i);
              (yyval.lrvalue).type = ref_fulltype(varp->type);
          }
          if (varp->type.t_flags & TYPE_MOD_DEPRECATED)
#line 11200 "prolang.y"
          {
              yywarnf("Using deprecated global variable %s.\n",
                      get_txt(varp->name));
          }
          (yyval.lrvalue).type.t_flags = 0;

          CURRENT_PROGRAM_SIZE = current + 2;
      }
#line 14586 "y.tab.c" /* yacc.c:1646  */
    break;

  case 289:
#line 11211 "prolang.y" /* yacc.c:1646  */
    {
          /* Access a local variable */

          mp_uint current;
          bytecode_p p, q;
          lpctype_t *type;
#line 11218 "prolang.y"
          (yyvsp[0].ident) = check_for_context_local((yyvsp[0].ident), &type);

          (yyval.lrvalue).type = get_fulltype(ref_lpctype(type));
          (yyval.lrvalue).start = current = CURRENT_PROGRAM_SIZE;
          if (!realloc_a_program(2))
#line 11223 "prolang.y"
          {
              yyerrorf("Out of memory: program size %"PRIuMPINT"\n", current+2);
              YYACCEPT;
          }
          p = PROGRAM_BLOCK + current;

          (yyval.lrvalue).lvalue = alloc_lvalue_block(2);
          q = LVALUE_BLOCK + (yyval.lrvalue).lvalue.start;

          if ((yyvsp[0].ident)->u.local.context >= 0)
#line 11233 "prolang.y"
          {
              q[0] = F_PUSH_CONTEXT_LVALUE;
              q[1] = (yyvsp[0].ident)->u.local.context;

              *p++ = F_CONTEXT_IDENTIFIER;
              *p = (yyvsp[0].ident)->u.local.context;
          }
          else
#line 11241 "prolang.y"
          {
              q[0] = F_PUSH_LOCAL_VARIABLE_LVALUE;
              q[1] = (yyvsp[0].ident)->u.local.num;

              *p++ = F_LOCAL;
              *p = (yyvsp[0].ident)->u.local.num;
          }
          CURRENT_PROGRAM_SIZE = current + 2;
      }
#line 14633 "y.tab.c" /* yacc.c:1646  */
    break;

  case 290:
#line 11253 "prolang.y" /* yacc.c:1646  */
    {
          /* Lookup a struct member */
          short s_index = -1;
          int m_index = -1;

#line 11259 "prolang.y"
          lpctype_t* result = get_struct_member_result_type((yyvsp[-2].lrvalue).type.t_type, (yyvsp[0].sh_string), &s_index, &m_index);
          if (!result)
              result = lpctype_mixed;

          /* We don't need the lvalue for <expr4>. */
          free_lvalue_block((yyvsp[-2].lrvalue).lvalue);

          (yyval.lrvalue).start = (yyvsp[-2].lrvalue).start;
          (yyval.lrvalue).type = get_fulltype(result);

          if ((yyvsp[0].sh_string) != NULL) /* Compile time lookup. */
#line 11270 "prolang.y"
          {
              if (s_index == FSM_AMBIGUOUS) /* Not anymore. */
                  ins_prog_string((yyvsp[0].sh_string));
              else
#line 11274 "prolang.y"
              {
                  ins_number(m_index);
                  free_mstring((yyvsp[0].sh_string));
              }
          }

          ins_number(s_index);

          /* Put that expression also into an lvalue buffer. */
          (yyval.lrvalue).lvalue = compose_lvalue_block((lvalue_block_t) {0, 0}, 0, (yyvsp[-2].lrvalue).start, F_S_INDEX_LVALUE);

          ins_f_code(F_S_INDEX);

          free_fulltype((yyvsp[-2].lrvalue).type);
      }
#line 14676 "y.tab.c" /* yacc.c:1646  */
    break;

  case 291:
#line 11292 "prolang.y" /* yacc.c:1646  */
    {
#line 11294 "prolang.y"
          /* Generate (R)INDEX/PUSH_(R)INDEXED_LVALUE */

          (yyval.lrvalue).start = (yyvsp[-1].lrvalue).start;

          /* Is <expr4> already an lvalue? */
          if ((yyvsp[-1].lrvalue).lvalue.size > 0)
#line 11300 "prolang.y"
          {
              /* Make <expr4> a protected lvalue, because we have to
               * evaluate the index expression first, before using it.
               */
              (yyval.lrvalue).lvalue = compose_lvalue_block((yyvsp[-1].lrvalue).lvalue, F_MAKE_PROTECTED, (yyvsp[0].index).start, (yyvsp[0].index).lvalue_inst);
          }
          else
#line 11307 "prolang.y"
          {
              /* We can just copy the instruction block
               * and add a the index operation.
               */
              (yyval.lrvalue).lvalue = compose_lvalue_block((lvalue_block_t) {0, 0}, 0, (yyvsp[-1].lrvalue).start, (yyvsp[0].index).lvalue_inst);
          }

          ins_f_code((yyvsp[0].index).rvalue_inst);

          /* Check and compute the types */
          (yyval.lrvalue).type = get_fulltype(get_index_result_type((yyvsp[-1].lrvalue).type.t_type, (yyvsp[0].index).type1, (yyvsp[0].index).rvalue_inst, lpctype_mixed));

          free_fulltype((yyvsp[-1].lrvalue).type);
          free_fulltype((yyvsp[0].index).type1);
          free_fulltype((yyvsp[0].index).type2);
      }
#line 14714 "y.tab.c" /* yacc.c:1646  */
    break;

  case 292:
#line 11326 "prolang.y" /* yacc.c:1646  */
    {
#line 11328 "prolang.y"
          /* Generate a range expression */
          (yyval.lrvalue).start = (yyvsp[-1].lrvalue).start;

          /* Is <expr4> already an lvalue? */
          if ((yyvsp[-1].lrvalue).lvalue.size > 0)
#line 11333 "prolang.y"
          {
              /* Make <expr4> a protected lvalue, because we have to
               * evaluate the index expression first, before using it.
               */
              (yyval.lrvalue).lvalue = compose_lvalue_block((yyvsp[-1].lrvalue).lvalue, F_MAKE_PROTECTED, (yyvsp[0].index).start, (yyvsp[0].index).lvalue_inst);
          }
          else
#line 11340 "prolang.y"
          {
              /* We can just copy the instruction block
               * and add a the index operation.
               */
              (yyval.lrvalue).lvalue = compose_lvalue_block((lvalue_block_t) {0, 0}, 0, (yyvsp[-1].lrvalue).start, (yyvsp[0].index).lvalue_inst);
          }

          ins_f_code((yyvsp[0].index).rvalue_inst);

          /* Check the types */
          (yyval.lrvalue).type = get_fulltype(check_unary_op_type((yyvsp[-1].lrvalue).type.t_type, "range index", types_range_index, lpctype_mixed));

          if (!lpctype_contains(lpctype_int, (yyvsp[0].index).type1.t_type))
              fulltype_error("Bad type of index", (yyvsp[0].index).type1);
          if (!lpctype_contains(lpctype_int, (yyvsp[0].index).type2.t_type))
              fulltype_error("Bad type of index", (yyvsp[0].index).type2);

          free_fulltype((yyvsp[-1].lrvalue).type);
          free_fulltype((yyvsp[0].index).type1);
          free_fulltype((yyvsp[0].index).type2);
      }
#line 14756 "y.tab.c" /* yacc.c:1646  */
    break;

  case 293:
#line 11364 "prolang.y" /* yacc.c:1646  */
    {
#line 11366 "prolang.y"
          /* Generate MAP_INDEX/PUSH_INDEXED_MAP_LVALUE */

          (yyval.lrvalue).start = (yyvsp[-5].lrvalue).start;

          /* Well, just generate the code: expr4 must be
           * a mapping, or a runtime error will occur.
           * Therefore we don't need its lvalue.
           */
          free_lvalue_block((yyvsp[-5].lrvalue).lvalue);
          (yyval.lrvalue).lvalue = compose_lvalue_block((lvalue_block_t) {0, 0}, 0, (yyvsp[-5].lrvalue).start, F_MAP_INDEX_LVALUE);
          (yyval.lrvalue).type = get_fulltype(lpctype_mixed);
          ins_f_code(F_MAP_INDEX);

          /* Check and compute types */
          if ((yyvsp[-3].rvalue).type.t_flags & TYPE_MOD_REFERENCE
           || (yyvsp[-1].rvalue).type.t_flags & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          if (exact_types && !lpctype_contains(lpctype_mapping, (yyvsp[-5].lrvalue).type.t_type))
              fulltype_error("Bad type to indexed lvalue", (yyvsp[-5].lrvalue).type);

          if (exact_types && !lpctype_contains(lpctype_int, (yyvsp[-1].rvalue).type.t_type))
              fulltype_error("Bad type of index", (yyvsp[-1].rvalue).type);

          free_fulltype((yyvsp[-5].lrvalue).type);
          free_fulltype((yyvsp[-3].rvalue).type);
          free_fulltype((yyvsp[-1].rvalue).type);
      }
#line 14791 "y.tab.c" /* yacc.c:1646  */
    break;

  case 294:
#line 11400 "prolang.y" /* yacc.c:1646  */
    {
          /* Generate the lvalue for a global variable */

          int i;
          variable_t *varp;
#line 11406 "prolang.y"
          i = verify_declared((yyvsp[0].ident));
          if (i == -1)
              /* variable not declared */
              YYACCEPT;

          if (i & VIRTUAL_VAR_TAG)
#line 11412 "prolang.y"
          {
              bytecode_p q;

              (yyval.lvalue).lvalue = alloc_lvalue_block(2);
              q = LVALUE_BLOCK + (yyval.lvalue).lvalue.start;

              q[0] = F_PUSH_VIRTUAL_VARIABLE_LVALUE;
              q[1] = i;
              (yyval.lvalue).vlvalue_inst = F_PUSH_VIRTUAL_VARIABLE_VLVALUE;
              (yyval.lvalue).num_arg = 1;

              varp = V_VARIABLE(i);
              (yyval.lvalue).type = ref_lpctype(varp->type.t_type);
          }
          else
#line 11427 "prolang.y"
          {
              if (i & ~0xff)
#line 11429 "prolang.y"
              {
                  bytecode_p q;

                  (yyval.lvalue).lvalue = alloc_lvalue_block(3);
                  q = LVALUE_BLOCK + (yyval.lvalue).lvalue.start;

                  q[0] = F_PUSH_IDENTIFIER16_LVALUE;
                  PUT_SHORT(q+1, i);

                  (yyval.lvalue).vlvalue_inst = F_PUSH_IDENTIFIER16_VLVALUE;
                  (yyval.lvalue).num_arg = 2;
              }
              else
#line 11442 "prolang.y"
              {
                  bytecode_p q;

                  (yyval.lvalue).lvalue = alloc_lvalue_block(2);
                  q = LVALUE_BLOCK + (yyval.lvalue).lvalue.start;

                  q[0] = F_PUSH_IDENTIFIER_LVALUE;
                  q[1] = i;

                  (yyval.lvalue).vlvalue_inst = F_PUSH_IDENTIFIER_VLVALUE;
                  (yyval.lvalue).num_arg = 1;
              }
              varp = NV_VARIABLE(i);
              (yyval.lvalue).type = ref_lpctype(varp->type.t_type);
          }
          if (varp->type.t_flags & TYPE_MOD_DEPRECATED)
              yywarnf("Using deprecated global variable %s.\n",
                      get_txt(varp->name));
      }
#line 14861 "y.tab.c" /* yacc.c:1646  */
    break;

  case 295:
#line 11464 "prolang.y" /* yacc.c:1646  */
    {
          lpctype_t *type;
          bytecode_p q;
#line 11468 "prolang.y"
          /* Generate the lvalue for a local */

          (yyvsp[0].ident) = check_for_context_local((yyvsp[0].ident), &type);

          (yyval.lvalue).type = ref_lpctype(type);
          (yyval.lvalue).lvalue = alloc_lvalue_block(2);
          q = LVALUE_BLOCK + (yyval.lvalue).lvalue.start;

          if ((yyvsp[0].ident)->u.local.context >= 0)
#line 11477 "prolang.y"
          {
              q[0] = F_PUSH_CONTEXT_LVALUE;
              q[1] = (yyvsp[0].ident)->u.local.context;

              (yyval.lvalue).vlvalue_inst = F_PUSH_CONTEXT_VLVALUE;
              (yyval.lvalue).num_arg = 1;
          }
          else
#line 11485 "prolang.y"
          {
              q[0] = F_PUSH_LOCAL_VARIABLE_LVALUE;
              q[1] = (yyvsp[0].ident)->u.local.num;

              (yyval.lvalue).vlvalue_inst = F_PUSH_LOCAL_VARIABLE_VLVALUE;
              (yyval.lvalue).num_arg = 1;
          }
      }
#line 14897 "y.tab.c" /* yacc.c:1646  */
    break;

  case 297:
#line 11501 "prolang.y" /* yacc.c:1646  */
    {
          /* Generate/add an (R)INDEX_LVALUE */

#line 11505 "prolang.y"
          /* First change the rvalue 'expr4' into an lvalue.
           */
          if ((yyvsp[-1].lrvalue).lvalue.size > 0)
#line 11508 "prolang.y"
          {
              /* Make <expr4> a protected lvalue, because we have to
               * evaluate the index expression first, before using it.
               */
              (yyval.lvalue).lvalue = compose_lvalue_block((yyvsp[-1].lrvalue).lvalue, F_MAKE_PROTECTED, (yyvsp[0].index).start, (yyvsp[0].index).lvalue_inst);
          }
          else
#line 11515 "prolang.y"
          {
              /* We can just copy the instruction block
               * and add a the index operation.
               */
              (yyval.lvalue).lvalue = compose_lvalue_block((lvalue_block_t) {0, 0}, 0, (yyvsp[-1].lrvalue).start, (yyvsp[0].index).lvalue_inst);
          }

          (yyval.lvalue).vlvalue_inst = (yyvsp[0].index).vlvalue_inst;
          (yyval.lvalue).num_arg = 0;

          /* Remove the code from the program block. */
          CURRENT_PROGRAM_SIZE = (yyvsp[-1].lrvalue).start;
          last_expression = -1;

          /* Check and compute the types */
          (yyval.lvalue).type = get_index_result_type((yyvsp[-1].lrvalue).type.t_type, (yyvsp[0].index).type1, (yyvsp[0].index).rvalue_inst, lpctype_mixed);

          free_fulltype((yyvsp[-1].lrvalue).type);
          free_fulltype((yyvsp[0].index).type1);
          free_fulltype((yyvsp[0].index).type2);
      }
#line 14939 "y.tab.c" /* yacc.c:1646  */
    break;

  case 298:
#line 11539 "prolang.y" /* yacc.c:1646  */
    {
          /* Generate/add an PUSH_INDEXED_MAP_LVALUE */

#line 11543 "prolang.y"

          /* Well, just generate the code: expr4 must be
           * a mapping, or a runtime error will occur.
           * Therefore we don't need its lvalue.
           */
          free_lvalue_block((yyvsp[-5].lrvalue).lvalue);
          (yyval.lvalue).lvalue = compose_lvalue_block((lvalue_block_t) {0, 0}, 0, (yyvsp[-5].lrvalue).start, F_MAP_INDEX_LVALUE);
          (yyval.lvalue).type = lpctype_mixed;

          (yyval.lvalue).vlvalue_inst = F_MAP_INDEX_VLVALUE;
          (yyval.lvalue).num_arg = 0;

          /* Remove the code from the program block. */
          CURRENT_PROGRAM_SIZE = (yyvsp[-5].lrvalue).start;
          last_expression = -1;

          /* Check and compute types */
          if ((yyvsp[-3].rvalue).type.t_flags & TYPE_MOD_REFERENCE
           || (yyvsp[-1].rvalue).type.t_flags & TYPE_MOD_REFERENCE)
              yyerror("Reference used as index");

          if (exact_types && !lpctype_contains(lpctype_mapping, (yyvsp[-5].lrvalue).type.t_type))
              fulltype_error("Bad type to indexed lvalue", (yyvsp[-5].lrvalue).type);

          if (exact_types && !lpctype_contains(lpctype_int, (yyvsp[-1].rvalue).type.t_type))
              fulltype_error("Bad type of index", (yyvsp[-1].rvalue).type);

          free_fulltype((yyvsp[-5].lrvalue).type);
          free_fulltype((yyvsp[-3].rvalue).type);
          free_fulltype((yyvsp[-1].rvalue).type);
      }
#line 14979 "y.tab.c" /* yacc.c:1646  */
    break;

  case 299:
#line 11577 "prolang.y" /* yacc.c:1646  */
    {
          /* RANGE_LVALUE generation */

#line 11581 "prolang.y"
          /* Change the expr4 into an lvalue
           */
          if ((yyvsp[-1].lrvalue).lvalue.size <= 0)
#line 11584 "prolang.y"
          {
                yyerror("Need lvalue for range lvalue.");

                /* We can't continue without an lvalue on our hand. */
                free_fulltype((yyvsp[-1].lrvalue).type);
                free_fulltype((yyvsp[0].index).type1);
                free_fulltype((yyvsp[0].index).type2);
                YYACCEPT;
          }
          else
#line 11594 "prolang.y"
          {
              /* Make <expr4> a protected lvalue, because we have to
               * evaluate the index expression first, before using it.
               */
              (yyval.lvalue).lvalue = compose_lvalue_block((yyvsp[-1].lrvalue).lvalue, F_MAKE_PROTECTED, (yyvsp[0].index).start, (yyvsp[0].index).lvalue_inst);
          }

          (yyval.lvalue).vlvalue_inst = 0;
          (yyval.lvalue).num_arg = 0;

          /* Remove the code from the program block. */
          CURRENT_PROGRAM_SIZE = (yyvsp[-1].lrvalue).start;
          last_expression = -1;

          /* Compute and check the types */
          (yyval.lvalue).type = check_unary_op_type((yyvsp[-1].lrvalue).type.t_type, "range index", types_range_index, lpctype_mixed);

          if (exact_types && !lpctype_contains(lpctype_int, (yyvsp[0].index).type1.t_type))
              fulltype_error("Bad type of index", (yyvsp[0].index).type1);
          if (exact_types && !lpctype_contains(lpctype_int, (yyvsp[0].index).type2.t_type))
              fulltype_error("Bad type of index", (yyvsp[0].index).type2);

          free_fulltype((yyvsp[-1].lrvalue).type);
          free_fulltype((yyvsp[0].index).type1);
          free_fulltype((yyvsp[0].index).type2);
      }
#line 15029 "y.tab.c" /* yacc.c:1646  */
    break;

  case 300:
#line 11623 "prolang.y" /* yacc.c:1646  */
    {
          /* Create a struct member lvalue */
          short s_index = -1;
          int m_index = -1;

#line 11629 "prolang.y"
          lpctype_t* result = get_struct_member_result_type((yyvsp[-2].lrvalue).type.t_type, (yyvsp[0].sh_string), &s_index, &m_index);
          if (!result)
              result = lpctype_mixed;

          /* We don't need the lvalue for <expr4>. */
          free_lvalue_block((yyvsp[-2].lrvalue).lvalue);

          /* We have to generate some code, so if the struct lookup is
           * invalid, we just play along and generate code to look up
           * member #-1 in whatever we got.
           */

#line 11641 "prolang.y"
          {
              if ((yyvsp[0].sh_string) != NULL) /* Compile time lookup. */
#line 11643 "prolang.y"
              {
                  if (s_index == FSM_AMBIGUOUS) /* Not anymore. */
                      ins_prog_string((yyvsp[0].sh_string));
                  else
#line 11647 "prolang.y"
                  {
                      ins_number(m_index);
                      free_mstring((yyvsp[0].sh_string));
                  }
              }

              /* Insert the struct type index and the index opcode */
              ins_number(s_index);
              ins_f_code(F_S_INDEX_LVALUE);

              /* Now move that into an lvalue buffer. */
              (yyval.lvalue).lvalue = compose_lvalue_block((lvalue_block_t) {0, 0}, 0, (yyvsp[-2].lrvalue).start, 0);

              (yyval.lvalue).vlvalue_inst = F_S_INDEX_VLVALUE;
              (yyval.lvalue).num_arg = 0;

              /* And remove it from the program block. */
              CURRENT_PROGRAM_SIZE = (yyvsp[-2].lrvalue).start;
              last_expression = -1;

              (yyval.lvalue).type = result;
          }

          free_fulltype((yyvsp[-2].lrvalue).type);
      }
#line 15086 "y.tab.c" /* yacc.c:1646  */
    break;

  case 301:
#line 11675 "prolang.y" /* yacc.c:1646  */
    {
          /* This rule is there to distinguish the 'lvalue' rules
           * from the expr4 rules and thus to prevent conflicts
           * because of the LALR simplication process.
           * (Alternatively the canonical-lr parser could be used.)
           */
           fatal("There should be no reduction with this rule.");

           (yyval.lvalue).type = lpctype_mixed;
           (yyval.lvalue).lvalue = (lvalue_block_t) {0, 0};
           (yyval.lvalue).vlvalue_inst = 0;
           (yyval.lvalue).num_arg = 0;
      }
#line 15104 "y.tab.c" /* yacc.c:1646  */
    break;

  case 302:
#line 11695 "prolang.y" /* yacc.c:1646  */
    {
          bytecode_p q = LVALUE_BLOCK + (yyvsp[0].lvalue).lvalue.start;

          (yyval.lvalue) = (yyvsp[0].lvalue);

          q[(yyvsp[0].lvalue).lvalue.size - (yyvsp[0].lvalue).num_arg - 1] = (yyvsp[0].lvalue).vlvalue_inst;
      }
#line 15116 "y.tab.c" /* yacc.c:1646  */
    break;

  case 303:
#line 11707 "prolang.y" /* yacc.c:1646  */
    {
          define_local_variable((yyvsp[0].ident), (yyvsp[-1].lpctype), &(yyval.lvalue), MY_FALSE, MY_TRUE);
          ref_lpctype((yyval.lvalue).type);
          free_lpctype((yyvsp[-1].lpctype));
      }
#line 15126 "y.tab.c" /* yacc.c:1646  */
    break;

  case 304:
#line 11713 "prolang.y" /* yacc.c:1646  */
    {
          define_local_variable((yyvsp[0].ident), (yyvsp[-1].lpctype), &(yyval.lvalue), MY_TRUE, MY_TRUE);
          ref_lpctype((yyval.lvalue).type);
          free_lpctype((yyvsp[-1].lpctype));
      }
#line 15136 "y.tab.c" /* yacc.c:1646  */
    break;

  case 305:
#line 11728 "prolang.y" /* yacc.c:1646  */
    {
            (yyval.index).rvalue_inst  = F_INDEX;
            (yyval.index).lvalue_inst  = F_INDEX_LVALUE;
            (yyval.index).vlvalue_inst = F_INDEX_VLVALUE;
            (yyval.index).start        = (yyvsp[-1].rvalue).start;
            (yyval.index).end          = CURRENT_PROGRAM_SIZE;
            (yyval.index).type1        = (yyvsp[-1].rvalue).type;
            (yyval.index).type2.t_type = NULL;
            if (!pragma_warn_deprecated)
#line 11737 "prolang.y"
            {
                ins_byte(F_NO_WARN_DEPRECATED);
                (yyval.index).end++;
            }

        }
#line 15157 "y.tab.c" /* yacc.c:1646  */
    break;

  case 306:
#line 11745 "prolang.y" /* yacc.c:1646  */
    {
            (yyval.index).rvalue_inst  = F_RINDEX;
            (yyval.index).lvalue_inst  = F_RINDEX_LVALUE;
            (yyval.index).vlvalue_inst = F_RINDEX_VLVALUE;
            (yyval.index).start        = (yyvsp[-1].rvalue).start;
            (yyval.index).end          = CURRENT_PROGRAM_SIZE;
            (yyval.index).type1        = (yyvsp[-1].rvalue).type;
            (yyval.index).type2.t_type = NULL;
            if (!pragma_warn_deprecated)
#line 11754 "prolang.y"
            {
                ins_byte(F_NO_WARN_DEPRECATED);
                (yyval.index).end++;
            }
        }
#line 15177 "y.tab.c" /* yacc.c:1646  */
    break;

  case 307:
#line 11761 "prolang.y" /* yacc.c:1646  */
    {
            (yyval.index).rvalue_inst  = F_AINDEX;
            (yyval.index).lvalue_inst  = F_AINDEX_LVALUE;
            (yyval.index).vlvalue_inst = F_AINDEX_VLVALUE;
            (yyval.index).start        = (yyvsp[-1].rvalue).start;
            (yyval.index).end          = CURRENT_PROGRAM_SIZE;
            (yyval.index).type1        = (yyvsp[-1].rvalue).type;
            (yyval.index).type2.t_type = NULL;
            if (!pragma_warn_deprecated)
#line 11770 "prolang.y"
            {
                ins_byte(F_NO_WARN_DEPRECATED);
                (yyval.index).end++;
            }
        }
#line 15197 "y.tab.c" /* yacc.c:1646  */
    break;

  case 308:
#line 11782 "prolang.y" /* yacc.c:1646  */
    {
          /* Simulate an expression yielding 0 for the lower bound.
           * We pretend that it's part of the upper bound expr.
           */

          p_int current;
          p_int length;
          bytecode_p mark, p;

          current = CURRENT_PROGRAM_SIZE;

          if (!realloc_a_program(1))
#line 11794 "prolang.y"
          {
              yyerrorf("Out of memory: program size %"PRIdPINT"\n", current+1);
              free_fulltype((yyvsp[-1].rvalue).type);
              YYACCEPT;
          }

          mark = PROGRAM_BLOCK + (yyvsp[-1].rvalue).start;
          p = PROGRAM_BLOCK + current;
          length = current - (yyvsp[-1].rvalue).start;
          for( ; --length >= 0; p--) PUT_CODE(p, GET_CODE(p-1));
          STORE_CODE(mark, F_CONST0);
          CURRENT_PROGRAM_SIZE++;

          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          /* Return the data */

          (yyval.index).rvalue_inst  = F_RANGE;
          (yyval.index).lvalue_inst  = F_RANGE_LVALUE;
          (yyval.index).start        = (yyvsp[-1].rvalue).start;
          (yyval.index).end          = CURRENT_PROGRAM_SIZE;
          (yyval.index).type1        = get_fulltype(lpctype_int);
          (yyval.index).type2        = (yyvsp[-1].rvalue).type;
      }
#line 15240 "y.tab.c" /* yacc.c:1646  */
    break;

  case 309:
#line 11823 "prolang.y" /* yacc.c:1646  */
    {
          /* Simulate an expression yielding 0 for the lower bound.
           * We pretend that it's part of the upper bound expr.
           */

          p_int current;
          p_int length;
          bytecode_p mark, p;

          current = CURRENT_PROGRAM_SIZE;

          if (!realloc_a_program(1))
#line 11835 "prolang.y"
          {
              yyerrorf("Out of memory: program size %"PRIdPINT"\n", current+1);
              free_fulltype((yyvsp[-1].rvalue).type);
              YYACCEPT;
          }

          mark = PROGRAM_BLOCK + (yyvsp[-1].rvalue).start;
          p = PROGRAM_BLOCK + current;
          length = current - (yyvsp[-1].rvalue).start;
          for( ; --length >= 0; p--) PUT_CODE(p, GET_CODE(p-1));
          STORE_CODE(mark, F_CONST0);
          CURRENT_PROGRAM_SIZE++;

          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          /* Return the data */

          (yyval.index).rvalue_inst = F_NR_RANGE;
          (yyval.index).lvalue_inst = F_NR_RANGE_LVALUE;
          (yyval.index).start       = (yyvsp[-1].rvalue).start;
          (yyval.index).end         = CURRENT_PROGRAM_SIZE;
          (yyval.index).type1       = get_fulltype(lpctype_int);
          (yyval.index).type2       = (yyvsp[-1].rvalue).type;
      }
#line 15283 "y.tab.c" /* yacc.c:1646  */
    break;

  case 310:
#line 11864 "prolang.y" /* yacc.c:1646  */
    {
          /* Simulate an expression yielding 0 for the lower bound.
           * We pretend that it's part of the upper bound expr.
           */

          p_int current;
          p_int length;
          bytecode_p mark, p;

          current = CURRENT_PROGRAM_SIZE;

          if (!realloc_a_program(1))
#line 11876 "prolang.y"
          {
              yyerrorf("Out of memory: program size %"PRIdPINT"\n", current+1);
              free_fulltype((yyvsp[-1].rvalue).type);
              YYACCEPT;
          }

          mark = PROGRAM_BLOCK + (yyvsp[-1].rvalue).start;
          p = PROGRAM_BLOCK + current;
          length = current - (yyvsp[-1].rvalue).start;
          for( ; --length >= 0; p--) PUT_CODE(p, GET_CODE(p-1));
          STORE_CODE(mark, F_CONST0);
          CURRENT_PROGRAM_SIZE++;

          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          /* Return the data */

          (yyval.index).rvalue_inst = F_NA_RANGE;
          (yyval.index).lvalue_inst = F_NA_RANGE_LVALUE;
          (yyval.index).start       = (yyvsp[-1].rvalue).start;
          (yyval.index).end         = CURRENT_PROGRAM_SIZE;
          (yyval.index).type1       = get_fulltype(lpctype_int);
          (yyval.index).type2       = (yyvsp[-1].rvalue).type;
      }
#line 15326 "y.tab.c" /* yacc.c:1646  */
    break;

  case 311:
#line 11904 "prolang.y" /* yacc.c:1646  */
    {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          (yyval.index).rvalue_inst = F_RANGE;
          (yyval.index).lvalue_inst = F_RANGE_LVALUE;
          (yyval.index).start       = (yyvsp[-3].rvalue).start;
          (yyval.index).end         = CURRENT_PROGRAM_SIZE;
          (yyval.index).type1       = (yyvsp[-3].rvalue).type;
          (yyval.index).type2       = (yyvsp[-1].rvalue).type;
      }
#line 15342 "y.tab.c" /* yacc.c:1646  */
    break;

  case 312:
#line 11918 "prolang.y" /* yacc.c:1646  */
    {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          (yyval.index).rvalue_inst = F_NR_RANGE;
          (yyval.index).lvalue_inst = F_NR_RANGE_LVALUE;
          (yyval.index).start       = (yyvsp[-4].rvalue).start;
          (yyval.index).end         = CURRENT_PROGRAM_SIZE;
          (yyval.index).type1       = (yyvsp[-4].rvalue).type;
          (yyval.index).type2       = (yyvsp[-1].rvalue).type;
      }
#line 15358 "y.tab.c" /* yacc.c:1646  */
    break;

  case 313:
#line 11932 "prolang.y" /* yacc.c:1646  */
    {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          (yyval.index).rvalue_inst = F_RN_RANGE;
          (yyval.index).lvalue_inst = F_RN_RANGE_LVALUE;
          (yyval.index).start       = (yyvsp[-3].rvalue).start;
          (yyval.index).end         = CURRENT_PROGRAM_SIZE;
          (yyval.index).type1       = (yyvsp[-3].rvalue).type;
          (yyval.index).type2       = (yyvsp[-1].rvalue).type;
      }
#line 15374 "y.tab.c" /* yacc.c:1646  */
    break;

  case 314:
#line 11946 "prolang.y" /* yacc.c:1646  */
    {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          (yyval.index).rvalue_inst = F_RR_RANGE;
          (yyval.index).lvalue_inst = F_RR_RANGE_LVALUE;
          (yyval.index).start       = (yyvsp[-4].rvalue).start;
          (yyval.index).end         = CURRENT_PROGRAM_SIZE;
          (yyval.index).type1       = (yyvsp[-4].rvalue).type;
          (yyval.index).type2       = (yyvsp[-1].rvalue).type;
      }
#line 15390 "y.tab.c" /* yacc.c:1646  */
    break;

  case 315:
#line 11960 "prolang.y" /* yacc.c:1646  */
    {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          (yyval.index).rvalue_inst = F_NA_RANGE;
          (yyval.index).lvalue_inst = F_NA_RANGE_LVALUE;
          (yyval.index).start       = (yyvsp[-4].rvalue).start;
          (yyval.index).end         = CURRENT_PROGRAM_SIZE;
          (yyval.index).type1       = (yyvsp[-4].rvalue).type;
          (yyval.index).type2       = (yyvsp[-1].rvalue).type;
      }
#line 15406 "y.tab.c" /* yacc.c:1646  */
    break;

  case 316:
#line 11974 "prolang.y" /* yacc.c:1646  */
    {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          (yyval.index).rvalue_inst = F_AN_RANGE;
          (yyval.index).lvalue_inst = F_AN_RANGE_LVALUE;
          (yyval.index).start       = (yyvsp[-3].rvalue).start;
          (yyval.index).end         = CURRENT_PROGRAM_SIZE;
          (yyval.index).type1       = (yyvsp[-3].rvalue).type;
          (yyval.index).type2       = (yyvsp[-1].rvalue).type;
      }
#line 15422 "y.tab.c" /* yacc.c:1646  */
    break;

  case 317:
#line 11988 "prolang.y" /* yacc.c:1646  */
    {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          (yyval.index).rvalue_inst = F_RA_RANGE;
          (yyval.index).lvalue_inst = F_RA_RANGE_LVALUE;
          (yyval.index).start       = (yyvsp[-4].rvalue).start;
          (yyval.index).end         = CURRENT_PROGRAM_SIZE;
          (yyval.index).type1       = (yyvsp[-4].rvalue).type;
          (yyval.index).type2       = (yyvsp[-1].rvalue).type;
      }
#line 15438 "y.tab.c" /* yacc.c:1646  */
    break;

  case 318:
#line 12002 "prolang.y" /* yacc.c:1646  */
    {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          (yyval.index).rvalue_inst = F_AR_RANGE;
          (yyval.index).lvalue_inst = F_AR_RANGE_LVALUE;
          (yyval.index).start       = (yyvsp[-4].rvalue).start;
          (yyval.index).end         = CURRENT_PROGRAM_SIZE;
          (yyval.index).type1       = (yyvsp[-4].rvalue).type;
          (yyval.index).type2       = (yyvsp[-1].rvalue).type;
      }
#line 15454 "y.tab.c" /* yacc.c:1646  */
    break;

  case 319:
#line 12016 "prolang.y" /* yacc.c:1646  */
    {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          (yyval.index).rvalue_inst = F_AA_RANGE;
          (yyval.index).lvalue_inst = F_AA_RANGE_LVALUE;
          (yyval.index).start       = (yyvsp[-4].rvalue).start;
          (yyval.index).end         = CURRENT_PROGRAM_SIZE;
          (yyval.index).type1       = (yyvsp[-4].rvalue).type;
          (yyval.index).type2       = (yyvsp[-1].rvalue).type;
      }
#line 15470 "y.tab.c" /* yacc.c:1646  */
    break;

  case 320:
#line 12030 "prolang.y" /* yacc.c:1646  */
    {
          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          (yyval.index).rvalue_inst = F_NX_RANGE;
          (yyval.index).lvalue_inst = F_NX_RANGE_LVALUE;
          (yyval.index).start       = (yyvsp[-2].rvalue).start;
          (yyval.index).end         = CURRENT_PROGRAM_SIZE;
          (yyval.index).type1       = (yyvsp[-2].rvalue).type;
          (yyval.index).type2       = get_fulltype(lpctype_int);
      }
#line 15486 "y.tab.c" /* yacc.c:1646  */
    break;

  case 321:
#line 12044 "prolang.y" /* yacc.c:1646  */
    {
          /* Simulate an expression yielding <1 for the upper bound.
           * We pretend that it's part of the lower bound expr.
           */

          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          (yyval.index).rvalue_inst = F_RX_RANGE;
          (yyval.index).lvalue_inst = F_RX_RANGE_LVALUE;
          (yyval.index).start       = (yyvsp[-2].rvalue).start;
          (yyval.index).end         = CURRENT_PROGRAM_SIZE;
          (yyval.index).type1       = (yyvsp[-2].rvalue).type;
          (yyval.index).type2       = get_fulltype(lpctype_int);
      }
#line 15506 "y.tab.c" /* yacc.c:1646  */
    break;

  case 322:
#line 12062 "prolang.y" /* yacc.c:1646  */
    {
          /* Simulate an expression yielding <1 for the upper bound.
           * We pretend that it's part of the lower bound expr.
           */

          if (pragma_range_check)
              ins_byte(F_ARRAY_RANGE_CHECK);

          (yyval.index).rvalue_inst = F_AX_RANGE;
          (yyval.index).lvalue_inst = F_AX_RANGE_LVALUE;
          (yyval.index).start       = (yyvsp[-2].rvalue).start;
          (yyval.index).end         = CURRENT_PROGRAM_SIZE;
          (yyval.index).type1       = (yyvsp[-2].rvalue).type;
          (yyval.index).type2       = get_fulltype(lpctype_int);
      }
#line 15526 "y.tab.c" /* yacc.c:1646  */
    break;

  case 323:
#line 12087 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = 0; }
#line 15532 "y.tab.c" /* yacc.c:1646  */
    break;

  case 324:
#line 12088 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[0].number); }
#line 15538 "y.tab.c" /* yacc.c:1646  */
    break;

  case 325:
#line 12089 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-1].number); }
#line 15544 "y.tab.c" /* yacc.c:1646  */
    break;

  case 326:
#line 12093 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = 1;      add_arg_type((yyvsp[0].rvalue).type); }
#line 15550 "y.tab.c" /* yacc.c:1646  */
    break;

  case 327:
#line 12094 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) + 1; add_arg_type((yyvsp[0].rvalue).type); }
#line 15556 "y.tab.c" /* yacc.c:1646  */
    break;

  case 328:
#line 12103 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = 0; }
#line 15562 "y.tab.c" /* yacc.c:1646  */
    break;

  case 329:
#line 12104 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[0].number); }
#line 15568 "y.tab.c" /* yacc.c:1646  */
    break;

  case 330:
#line 12109 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.number) = 1;
          if (!got_ellipsis[argument_level])
              add_arg_type((yyvsp[0].rvalue).type);
          else
#line 12114 "prolang.y"
          {
              /* Can't determine the exact type of argument <X>,
               * because we don't know the number of previous
               * arguments due to the ellipsis.
               */
              add_arg_type(get_fulltype(lpctype_mixed));
              free_fulltype((yyvsp[0].rvalue).type);
          }
      }
#line 15588 "y.tab.c" /* yacc.c:1646  */
    break;

  case 331:
#line 12125 "prolang.y" /* yacc.c:1646  */
    {
          PREPARE_INSERT(2);

          (yyval.number) = 0;
          got_ellipsis[argument_level] = MY_TRUE;
          add_f_code(F_FLATTEN_XARG);
          free_fulltype((yyvsp[-1].rvalue).type);
          CURRENT_PROGRAM_SIZE++;
      }
#line 15602 "y.tab.c" /* yacc.c:1646  */
    break;

  case 332:
#line 12136 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.number) = (yyvsp[-2].number) + 1;
          if (!got_ellipsis[argument_level])
              add_arg_type((yyvsp[0].rvalue).type);
          else
#line 12141 "prolang.y"
          {
              add_arg_type(get_fulltype(lpctype_mixed));
              free_fulltype((yyvsp[0].rvalue).type);
          }
      }
#line 15618 "y.tab.c" /* yacc.c:1646  */
    break;

  case 333:
#line 12148 "prolang.y" /* yacc.c:1646  */
    {
          PREPARE_INSERT(2);

          (yyval.number) = (yyvsp[-3].number);
          got_ellipsis[argument_level] = MY_TRUE;
          add_f_code(F_FLATTEN_XARG);
          free_fulltype((yyvsp[-1].rvalue).type);
          CURRENT_PROGRAM_SIZE++;
      }
#line 15632 "y.tab.c" /* yacc.c:1646  */
    break;

  case 334:
#line 12161 "prolang.y" /* yacc.c:1646  */
    { (yyval.numbers)[0] = 0; (yyval.numbers)[1]= 1; }
#line 15638 "y.tab.c" /* yacc.c:1646  */
    break;

  case 337:
#line 12164 "prolang.y" /* yacc.c:1646  */
    { (yyval.numbers)[0] = (yyvsp[0].number); (yyval.numbers)[1] = 0; }
#line 15644 "y.tab.c" /* yacc.c:1646  */
    break;

  case 338:
#line 12165 "prolang.y" /* yacc.c:1646  */
    { (yyval.numbers)[0] = (yyvsp[-1].number); (yyval.numbers)[1] = 0; }
#line 15650 "y.tab.c" /* yacc.c:1646  */
    break;

  case 339:
#line 12170 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.numbers)[0] = 1 + (yyvsp[0].number);
          (yyval.numbers)[1] = (yyvsp[0].number);
          add_arg_type((yyvsp[-1].rvalue).type); /* order doesn't matter */
      }
#line 15660 "y.tab.c" /* yacc.c:1646  */
    break;

  case 340:
#line 12177 "prolang.y" /* yacc.c:1646  */
    {
          if ((yyvsp[-3].numbers)[1] != (yyvsp[0].number)) {
              yyerror("Inconsistent number of values in mapping literal");
          }
          (yyval.numbers)[0] = (yyvsp[-3].numbers)[0] + 1 + (yyvsp[0].number);
          (yyval.numbers)[1] = (yyvsp[-3].numbers)[1];
          add_arg_type((yyvsp[-1].rvalue).type);
      }
#line 15673 "y.tab.c" /* yacc.c:1646  */
    break;

  case 341:
#line 12188 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = 1;      add_arg_type((yyvsp[0].rvalue).type); }
#line 15679 "y.tab.c" /* yacc.c:1646  */
    break;

  case 342:
#line 12189 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = (yyvsp[-2].number) + 1; add_arg_type((yyvsp[0].rvalue).type); }
#line 15685 "y.tab.c" /* yacc.c:1646  */
    break;

  case 343:
#line 12198 "prolang.y" /* yacc.c:1646  */
    { (yyval.sh_string) = (yyvsp[0].sh_string); }
#line 15691 "y.tab.c" /* yacc.c:1646  */
    break;

  case 344:
#line 12201 "prolang.y" /* yacc.c:1646  */
    { fatal("presence of rule should prevent its reduction"); }
#line 15697 "y.tab.c" /* yacc.c:1646  */
    break;

  case 345:
#line 12204 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.sh_string) = last_lex_string; /* Adopt the reference */
          last_lex_string = NULL;
      }
#line 15706 "y.tab.c" /* yacc.c:1646  */
    break;

  case 346:
#line 12210 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.sh_string) = NULL;
          if (!lpctype_contains(lpctype_string, (yyvsp[-1].rvalue).type.t_type)
           && !lpctype_contains(lpctype_int, (yyvsp[-1].rvalue).type.t_type))
              fulltype_error("Illegal type for struct member name", (yyvsp[-1].rvalue).type);

          free_fulltype((yyvsp[-1].rvalue).type);
      }
#line 15719 "y.tab.c" /* yacc.c:1646  */
    break;

  case 347:
#line 12225 "prolang.y" /* yacc.c:1646  */
    { (yyval.struct_init_list).length = 0; (yyval.struct_init_list).list = (yyval.struct_init_list).last = NULL; }
#line 15725 "y.tab.c" /* yacc.c:1646  */
    break;

  case 348:
#line 12226 "prolang.y" /* yacc.c:1646  */
    { (yyval.struct_init_list) = (yyvsp[-1].struct_init_list); }
#line 15731 "y.tab.c" /* yacc.c:1646  */
    break;

  case 349:
#line 12228 "prolang.y" /* yacc.c:1646  */
    {
          /* Allow the parser to resynchronize */
          (yyval.struct_init_list).length = 0; (yyval.struct_init_list).list = (yyval.struct_init_list).last = NULL;
      }
#line 15740 "y.tab.c" /* yacc.c:1646  */
    break;

  case 352:
#line 12242 "prolang.y" /* yacc.c:1646  */
    {
          struct_init_t * p;

          p = xalloc(sizeof(*p));
          p->next = NULL;
          p->name = (yyvsp[0].struct_init_member).name;
          p->type = (yyvsp[0].struct_init_member).type;
          (yyval.struct_init_list).length = 1;
          (yyval.struct_init_list).list = p;
          (yyval.struct_init_list).last = p;
      }
#line 15756 "y.tab.c" /* yacc.c:1646  */
    break;

  case 353:
#line 12255 "prolang.y" /* yacc.c:1646  */
    {
          struct_init_t * p;

          p = xalloc(sizeof(*p));
          p->next = NULL;
          p->name = (yyvsp[0].struct_init_member).name;
          p->type = (yyvsp[0].struct_init_member).type;
          (yyval.struct_init_list).length = (yyvsp[-2].struct_init_list).length + 1;
          (yyval.struct_init_list).list = (yyvsp[-2].struct_init_list).list;
          (yyvsp[-2].struct_init_list).last->next = p;
          (yyval.struct_init_list).last = p;
      }
#line 15773 "y.tab.c" /* yacc.c:1646  */
    break;

  case 354:
#line 12271 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.struct_init_member).name = (yyvsp[-2].sh_string);
          (yyval.struct_init_member).type = (yyvsp[0].rvalue).type;
      }
#line 15782 "y.tab.c" /* yacc.c:1646  */
    break;

  case 355:
#line 12276 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.struct_init_member).name = NULL;
          (yyval.struct_init_member).type = (yyvsp[0].rvalue).type;
      }
#line 15791 "y.tab.c" /* yacc.c:1646  */
    break;

  case 356:
#line 12290 "prolang.y" /* yacc.c:1646  */
    {
#line 12292 "prolang.y"
          /* The generic function call by name.
           *
           * It may be an ordinary intra-object function call.
           * But, if the function is not defined, then it might be a call
           * to a simul_efun. If it is, then we make it a simul_efun or
           * even call_other(), of which the latter requires the function
           * name as argument.
           * It might even be a real efun.
           */

          ident_t *real_name;

          /* Save the (simple) state */
          (yyval.function_call_head).start = CURRENT_PROGRAM_SIZE;
          (yyval.function_call_head).simul_efun = -1;

          if((yyvsp[0].function_name).super)
#line 12309 "prolang.y"
          {
              if (strcmp((yyvsp[0].function_name).super, get_txt(STR_EFUN)) == 0)
                  (yyval.function_call_head).efun_override = OVERRIDE_EFUN;
              else if (strcmp((yyvsp[0].function_name).super, get_txt(STR_SEFUN)) == 0)
                  (yyval.function_call_head).efun_override = OVERRIDE_SEFUN;
              else
                  (yyval.function_call_head).efun_override = OVERRIDE_NONE;
          }
          else
              (yyval.function_call_head).efun_override = OVERRIDE_NONE;

          /* Insert the save_arg_frame instruction.
           * If it's not really needed, we'll remove it later.
           */
#line 12323 "prolang.y"
          {
              PREPARE_INSERT(2)
              add_f_code(F_SAVE_ARG_FRAME);
              CURRENT_PROGRAM_SIZE++;
          }

          if (argument_level+1 == sizeof(got_ellipsis)/sizeof(got_ellipsis[0]))
#line 12330 "prolang.y"
          {
              yyerror("Functions nested too deeply.");
              YYACCEPT;
          }
          argument_level++;
          got_ellipsis[argument_level] = MY_FALSE;

          real_name = (yyvsp[0].function_name).real;
            /* we rely on the fact that $1.real->type is either
             * I_TYPE_UNKNOWN or I_TYPE_GLOBAL here. All others are filtered
             * by the lexical analysis.
             */

          if (real_name->type == I_TYPE_UNKNOWN)
#line 12344 "prolang.y"
          {
              /* prevent freeing by exotic name clashes */
              /* also makes life easier below */
              init_global_identifier(real_name, /* bVariable: */ MY_TRUE);
              real_name->next_all = all_globals;
              all_globals = real_name;
          }
          else if ( ((yyvsp[0].function_name).super ? ( (yyval.function_call_head).efun_override == OVERRIDE_SEFUN )
                              : ( real_name->u.global.function < 0 ))
                && real_name->u.global.sim_efun >= 0
                && !disable_sefuns)
#line 12355 "prolang.y"
          {
              /* It's a real simul-efun */

              (yyval.function_call_head).simul_efun = real_name->u.global.sim_efun;
              /* real_name->u.global.sim_efun is >=0 (see above), so it can
               * be casted to unsigned long before comparison (SEFUN_TABLE_SIZE
               * is unsigned long) */
              if ((unsigned long)real_name->u.global.sim_efun >= SEFUN_TABLE_SIZE)
#line 12363 "prolang.y"
              {
                  /* The simul-efun has to be called by name:
                   * prepare the extra args for the call_other
                   */
                  PREPARE_INSERT(8)
                  string_t *p;

                  p = ref_mstring(real_name->name);
                  add_f_code(F_STRING);
                  add_short(store_prog_string(
                    ref_mstring(query_simul_efun_file_name())));
                  add_f_code(F_STRING);
                  add_short(store_prog_string(p));
                  CURRENT_PROGRAM_SIZE += 6;
              }
          }
      }
#line 15892 "y.tab.c" /* yacc.c:1646  */
    break;

  case 357:
#line 12383 "prolang.y" /* yacc.c:1646  */
    {
          /* We got the arguments. Now we have to generate the
           * proper instructions to call the function.
           */
#line 12388 "prolang.y"
          int         f = 0;             /* Function index */
          int         simul_efun;
          lpctype_t **arg_types = NULL; /* Argtypes from the program */
          int         first_arg;         /* Startindex in arg_types[] */
          Bool        ap_needed;         /* TRUE if arg frame is needed */
          Bool        has_ellipsis;      /* TRUE if '...' was used */

          has_ellipsis = got_ellipsis[argument_level];
          ap_needed = MY_FALSE;

          (yyval.function_call_result).start = (yyvsp[-3].function_call_head).start;
          (yyval.function_call_result).might_lvalue = true;

          if ( (yyvsp[-1].number) >= 0xff )
              /* since num_arg is encoded in just one byte, and 0xff
               * is taken for SIMUL_EFUN_VARARG */
              yyerrorf("Too many arguments to function");

          do {
              /* The function processing is in a big do...while(0)
               * block so we can exit out of it prematurely and
               * still get the required arg-frame handling
               * afterwards
               */

              if ( !disable_sefuns
               && (simul_efun = (yyvsp[-3].function_call_head).simul_efun) >= 0)
#line 12415 "prolang.y"
              {
                  /* SIMUL EFUN */

                  PREPARE_INSERT(6)

                  function_t *funp;

                  funp = &simul_efunp[simul_efun];

                  if (funp->num_arg != SIMUL_EFUN_VARARGS
                   && !(funp->flags & TYPE_MOD_XVARARGS))
#line 12426 "prolang.y"
                  {
                      if ((yyvsp[-1].number) > funp->num_arg)
                          yyerrorf("Too many arguments to simul_efun %s"
                                  , get_txt(funp->name));

                      if ((yyvsp[-1].number) < funp->num_arg && !has_ellipsis)
#line 12432 "prolang.y"
                      {
                          if (pragma_pedantic)
                              yyerrorf("Missing arguments to simul_efun %s"
                                      , get_txt(funp->name));
                          else
#line 12437 "prolang.y"
                          {
                              yywarnf("Missing arguments to simul_efun %s"
                                     , get_txt(funp->name));
                              ap_needed = MY_TRUE;
                          }
                      }

                  }
                  
                  if (funp->flags & TYPE_MOD_DEPRECATED)
                      yywarnf("Calling deprecated simul_efun \'%s\'",
                              get_txt(funp->name));
                      
                  if (funp->num_arg == SIMUL_EFUN_VARARGS
                   || (funp->flags & TYPE_MOD_XVARARGS)
                   || has_ellipsis)
                      ap_needed = MY_TRUE;

                  /* simul_efun is >= 0, see above) */
                  if ((unsigned long)simul_efun >= SEFUN_TABLE_SIZE)
#line 12457 "prolang.y"
                  {
                      /* call-other: the number of arguments will be
                       * corrected at runtime.
                       */
                      add_f_code(F_CALL_DIRECT);
                      CURRENT_PROGRAM_SIZE++;
                      ap_needed = MY_TRUE;
                  }
                  else
#line 12466 "prolang.y"
                  {
                      /* Direct call */

                      if (ap_needed)
#line 12470 "prolang.y"
                      {
                          add_f_code(F_USE_ARG_FRAME);
                          CURRENT_PROGRAM_SIZE++;
                      }
                      add_f_code(F_SIMUL_EFUN);
                      add_short(simul_efun);
                      CURRENT_PROGRAM_SIZE += 3;
                  }
                  (yyval.function_call_result).type = get_fulltype(ref_lpctype(funp->type));
              } /* if (simul-efun) */

              else if ((yyvsp[-4].function_name).super ? ((yyvsp[-3].function_call_head).efun_override == OVERRIDE_NONE)
                                : (f = defined_function((yyvsp[-4].function_name).real)) >= 0
                      )
#line 12484 "prolang.y"
              {
                  /* LFUN or INHERITED LFUN */

                  PREPARE_INSERT(6)

                  function_t *funp;
                  function_t  inherited_function;

                  ap_needed = MY_TRUE;

                  if ((yyvsp[-4].function_name).super)
#line 12495 "prolang.y"
                  {
                      /* Inherited lfun: check its existance and call it */

                      program_t *super_prog;
                      int ix;

                      ix = insert_inherited( (yyvsp[-4].function_name).super, (yyvsp[-4].function_name).real->name
                                           , &super_prog, &inherited_function
                                           , (yyvsp[-1].number), (bytecode_p)__PREPARE_INSERT__p
                                           );

                      if (ix < 0)
#line 12507 "prolang.y"
                      {
                          switch(ix) {
                          case INHERITED_NOT_FOUND:
                              yyerror("function not defined by inheritance as specified");
                              break;
                          case INHERITED_WILDCARDED_ARGS:
                              yyerror("wildcarded call to inherited function can't pass arguments");
                              break;
                          case INHERITED_WILDCARDED_NOT_FOUND:
                              ap_needed = MY_FALSE;
                              /* Not an error, but we can't do argument
                               * checks either.
                               */
                              break;
                          default:
                              fatal("Unknown return code %d from insert_inherited()\n", ix);
                              break;
                          }

                          (yyval.function_call_result).type = get_fulltype(lpctype_mixed);
                          break; /* Out of do..while(0) */
                      }

                      /* Find the argument types */
                      if (super_prog
                       && NULL != (arg_types = super_prog->argument_types))
#line 12533 "prolang.y"
                      {
                          first_arg = super_prog->type_start[ix];
                      }
                      else
#line 12537 "prolang.y"
                      {
                          first_arg = INDEX_START_NONE;
                      }

                      funp = &inherited_function;
                  }
                  else
#line 12544 "prolang.y"
                  {
                      /* Normal lfun in this program */

                      ap_needed = MY_TRUE;
                      add_f_code(F_CALL_FUNCTION);
                      add_short(f);
                      funp = FUNCTION(f);
                      arg_types = GET_BLOCK(A_ARGUMENT_TYPES);
                      first_arg = ARGUMENT_INDEX(f);
                      CURRENT_PROGRAM_SIZE += 3;
                  }

                  /* Verify that the function has been defined already.
                   * For inherited functions this is a no-brainer.
                   */
                  if (funp->flags & (NAME_UNDEFINED|NAME_HIDDEN))
#line 12560 "prolang.y"
                  {
                      if ( !(funp->flags & (NAME_PROTOTYPE|NAME_INHERITED))
                       && exact_types)
#line 12563 "prolang.y"
                      {
                          yyerrorf("Function %.50s undefined", get_txt(funp->name));
                      }
                      else if ((funp->flags
                                & (NAME_PROTOTYPE|NAME_HIDDEN))
                               == NAME_HIDDEN)
#line 12569 "prolang.y"
                      {
                          yyerrorf("Function %.50s is private", get_txt(funp->name));
                      }
                  }
                  // warn about obsoleted functions
                  if (funp->flags & TYPE_MOD_DEPRECATED)
                      yywarnf("Calling deprecated function \'%s\'",
                              get_txt(funp->name));


                  (yyval.function_call_result).type = get_fulltype(ref_lpctype(funp->type)); /* Result type */

                  /* Check number of arguments.
                   */
                  if (funp->num_arg != (yyvsp[-1].number)
                   && !(funp->flags & TYPE_MOD_VARARGS)
                   && (first_arg != INDEX_START_NONE)
                   && exact_types
                   && !has_ellipsis)
#line 12588 "prolang.y"
                  {
                      if (funp->num_arg-1 > (yyvsp[-1].number) || !(funp->flags & TYPE_MOD_XVARARGS))
                        yyerrorf("Wrong number of arguments to %.60s: "
                                 "expected %ld, got %ld"
                                , get_txt((yyvsp[-4].function_name).real->name)
                                , (long)funp->num_arg, (long)(yyvsp[-1].number));
                  }

                  /* Check the argument types.
                   */
                  if (exact_types && first_arg != INDEX_START_NONE)
#line 12599 "prolang.y"
                  {
                      int i;
                      fulltype_t *argp;
                      int num_arg, anum_arg;

                      if ( 0 != (num_arg = funp->num_arg) )
#line 12605 "prolang.y"
                      {
                          /* There are arguments to check */

                          int argno; /* Argument number for error message */

                          if (funp->flags & TYPE_MOD_XVARARGS)
                              num_arg--; /* last argument is checked separately */

                          if (num_arg > (anum_arg = (yyvsp[-1].number)) )
                              num_arg = anum_arg;

                          arg_types += first_arg;
                          argp = get_argument_types_start(anum_arg);

                          for (argno = 1, i = num_arg; --i >= 0; argno++)
#line 12620 "prolang.y"
                          {
                              if (!has_common_type(argp->t_type, *arg_types))
#line 12622 "prolang.y"
                              {
                                  yyerrorf("Bad type for argument %d of %s %s",
                                    argno,
                                    get_txt(funp->name),
                                    get_two_lpctypes(*arg_types, argp->t_type));
                              }

                              argp++;
                              arg_types++;
                          } /* for (all args) */

                          if (funp->flags & TYPE_MOD_XVARARGS)
#line 12634 "prolang.y"
                          {
                              lpctype_t *flat_type = get_flattened_type(*arg_types);

                              for (i = anum_arg - num_arg; --i >=0; )
#line 12638 "prolang.y"
                              {
                                  if (!has_common_type(argp->t_type, flat_type))
#line 12640 "prolang.y"
                                  {
                                      yyerrorf("Bad type for argument %d of %s %s",
                                          anum_arg - i,
                                          get_txt(funp->name),
                                          get_two_lpctypes(flat_type, argp->t_type));
                                  }
                                  argp++;
                              }
                          } /* if (xvarargs) */

                      } /* if (has args) */
                  } /* if (check types) */
              } /* if (inherited lfun) */





















              else if ( (f = lookup_predef((yyvsp[-4].function_name).real)) != -1 )
#line 12675 "prolang.y"
              {
                  /* EFUN */

                  PREPARE_INSERT(8)

                  fulltype_t *argp;
                  int min, max, def, num_arg;
                  int f2;

                  /* Get the information from the efun table */
                  min = instrs[f].min_arg;
                  max = instrs[f].max_arg;
                  def = instrs[f].Default;
                  (yyval.function_call_result).type = get_fulltype(ref_lpctype(instrs[f].ret_type));
                  argp = &efun_arg_types[instrs[f].arg_index];

                  /* Warn if the efun is deprecated */
                  if (pragma_warn_deprecated && instrs[f].deprecated != NULL)
                      yywarnf("%s() is deprecated: %s"
                             , instrs[f].name, instrs[f].deprecated);

                  num_arg = (yyvsp[-1].number);

                  /* Check and/or complete number of arguments */
                  if (def && num_arg == min-1 && !has_ellipsis)
#line 12700 "prolang.y"
                  {
                      /* Default argument */
                      add_f_code(def);
                      CURRENT_PROGRAM_SIZE++;
                      max--;
                      min--;
                  }
                  else if (num_arg < min
                        && !has_ellipsis
                        && (   (f2 = proxy_efun(f, num_arg)) < 0
                            || (f = f2, MY_FALSE) )
                           )
#line 12712 "prolang.y"
                  {
                      /* Not enough args, and no proxy_efun to replace this */
                      yyerrorf("Too few arguments to %s", instrs[f].name);
                  }
                  else if (num_arg > max && max != -1)
#line 12717 "prolang.y"
                  {
                      yyerrorf("Too many arguments to %s", instrs[f].name);
                      pop_arg_stack (num_arg - max);
                      (yyvsp[-1].number) -= num_arg - max; /* Don't forget this for the final pop */
                      num_arg = max;
                  }

                  /* Check the types of the arguments
                   */
                  if (max != -1 && exact_types && num_arg)
#line 12727 "prolang.y"
                  {
                      int         argn;
                      fulltype_t *aargp;

                      aargp = get_argument_types_start(num_arg);

                      /* Loop over all arguments and compare each given
                       * type against all allowed types in efun_arg_types()
                       */
                      for (argn = 0; argn < num_arg; argn++)
#line 12737 "prolang.y"
                      {
                          fulltype_t *beginArgp = argp;

                          for (;;argp++)
#line 12741 "prolang.y"
                          {
                              if ( argp->t_type == NULL )
#line 12743 "prolang.y"
                              {
                                  /* Possible types for this arg exhausted */
                                  efun_argument_error(argn+1, f, beginArgp
                                                     , *aargp);
                                  break;
                              }

                              /* Break if types are compatible.
                               */
                              if (has_common_type(argp->t_type, aargp->t_type))
                                  break;
                          } /* end for (efun_arg_types) */

                          /* Advance argp to point to the allowed argtypes
                           * of the next arg.
                           */
                          while((argp++)->t_type) NOOP;

                          /* The pointer on the argument type stack. */
                          aargp++;
                      } /* for (all args) */
                  } /* if (check arguments) */

                  /* If the function takes a variable number of arguments
                   * the ap is needed and evaluated automatically.
                   * If the function takes a fixed number of arguments, but
                   * the ellipsis has been used, the ap is needed but not
                   * evaluated automatically.
                   */
                  if (max != min)
#line 12773 "prolang.y"
                  {
                      ap_needed = MY_TRUE;
                  }
                  else if (has_ellipsis)
#line 12777 "prolang.y"
                  {
                      ap_needed = MY_TRUE;
                      add_byte(F_USE_ARG_FRAME);
                      CURRENT_PROGRAM_SIZE++;
                  }

                  /* Alias for an efun? */
                  if (f > LAST_INSTRUCTION_CODE)
                      f = efun_aliases[f-LAST_INSTRUCTION_CODE-1];

                  if (instrs[f].prefix)
#line 12788 "prolang.y"
                  {
                      /* This efun needs a prefix byte */
                      add_byte(instrs[f].prefix);
                      CURRENT_PROGRAM_SIZE++;
                  }
                  add_byte(instrs[f].opcode);
                  CURRENT_PROGRAM_SIZE++;

                  /* If the efun doesn't return a value, fake a 0.
                   * This is especially important is ap_needed, as the
                   * restore_arg_frame expects a result on the stack.
                   */
                  if ( instrs[f].ret_type == lpctype_void )
#line 12801 "prolang.y"
                  {
                      last_expression = mem_block[A_PROGRAM].current_size;
                      add_f_code(F_CONST0);
                      CURRENT_PROGRAM_SIZE++;
                  }
                  (yyval.function_call_result).might_lvalue = instrs[f].might_return_lvalue;
              } /* efun */

              else if ((yyvsp[-3].function_call_head).efun_override)
#line 12810 "prolang.y"
              {
                  yyerrorf(((yyvsp[-3].function_call_head).efun_override == OVERRIDE_EFUN)
                      ? "Unknown efun: %s" : "Unknown simul-efun: %s", get_txt((yyvsp[-4].function_name).real->name));
                  (yyval.function_call_result).type = get_fulltype(lpctype_mixed);
              }
              else
#line 12816 "prolang.y"
              {
                  /* There is no such function, but maybe it's defined later,
                   * maybe it's resolved through (cross-)inheritance.
                   * epilog() will take care of it.
                   */
                  PREPARE_INSERT(4)

                  f = define_new_function(MY_FALSE,
                      (yyvsp[-4].function_name).real, 0, 0, 0, NAME_UNDEFINED, get_fulltype(lpctype_unknown)
                  );
                  ap_needed = MY_TRUE;
                  add_f_code(F_CALL_FUNCTION);
                  add_short(f);
                  CURRENT_PROGRAM_SIZE += 3;
                  if (exact_types)
#line 12831 "prolang.y"
                  {
                      yyerrorf("Undefined function '%.50s'", get_txt((yyvsp[-4].function_name).real->name));
                  }
                  else if (pragma_pedantic)
#line 12835 "prolang.y"
                  {
                      yywarnf("Undefined function '%.50s'", get_txt((yyvsp[-4].function_name).real->name));
                  }
                  (yyval.function_call_result).type = get_fulltype(lpctype_mixed);  /* Just a guess */
              }
          } while (0); /* Function handling */

          /* Do the post processing of the arg frame handling */
          if (ap_needed)
#line 12844 "prolang.y"
          {
              /* Restore the previous arg frame pointer */

              PREPARE_INSERT(2)

              add_f_code(F_RESTORE_ARG_FRAME);
              CURRENT_PROGRAM_SIZE++;
          }
          else if (!ap_needed)
#line 12853 "prolang.y"
          {
              /* Since the arg frame is not needed, remove the
               * earlier save_arg_frame instruction.
               */

              bytecode_p src, dest;
              size_t left;

              dest = PROGRAM_BLOCK + (yyvsp[-3].function_call_head).start;
              src = dest+1;
              left = CURRENT_PROGRAM_SIZE - (yyvsp[-3].function_call_head).start - 1;

              while (left-- > 0)
#line 12866 "prolang.y"
              {
                  *dest++ = *src++;
              }

              CURRENT_PROGRAM_SIZE--;

              /* If last_expression lies within the program area
               * that was moved one bytecode adjust it accordingly.
               */
              if(last_expression > (yyvsp[-3].function_call_head).start)
                  last_expression--;
          }

          argument_level--;

          if ((yyvsp[-4].function_name).super)
              yfree((yyvsp[-4].function_name).super);
          pop_arg_stack((yyvsp[-1].number));   /* Argument types no longer needed */
      }
#line 16442 "y.tab.c" /* yacc.c:1646  */
    break;

  case 358:
#line 12888 "prolang.y" /* yacc.c:1646  */
    {
#line 12890 "prolang.y"
          string_t *name;

          /* Don't need <expr4> as an lvalue. */
          free_lvalue_block((yyvsp[-2].lrvalue).lvalue);

          /* Save the (simple) state */
          (yyval.function_call_head).start = CURRENT_PROGRAM_SIZE;

          /* Insert the save_arg_frame instruction.
           * If it's not really needed, we'll remove it later.
           * Putting this code block before the <expr4> in the rule
           * however yields a faulty grammar.
           */
#line 12903 "prolang.y"
          {
              char *p, *q;
              p_int left;

              if (!realloc_a_program(1))
#line 12908 "prolang.y"
              {
                  yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                          , mem_block[A_PROGRAM].current_size + 2);
                  free_fulltype((yyvsp[-2].lrvalue).type);
                  YYACCEPT;
              }

              /* Move the generated code forward by 1 */
              p = mem_block[A_PROGRAM].block + CURRENT_PROGRAM_SIZE - 1;
              q = p + 1;
              for (left = CURRENT_PROGRAM_SIZE - (yyvsp[-2].lrvalue).start
                  ; left > 0
                  ; left--, p--, q--)
                  *q = *p;

              /* p now points to program[$1.start]-1.
               * Store the instruction there.
               */
              p[1] = F_SAVE_ARG_FRAME;
              CURRENT_PROGRAM_SIZE += 1;

              /* No expression to optimize here anymore. */
              last_expression = -1;
          }

          if (argument_level+1 == sizeof(got_ellipsis)/sizeof(got_ellipsis[0]))
#line 12934 "prolang.y"
          {
              yyerror("Functions nested too deeply.");
              free_fulltype((yyvsp[-2].lrvalue).type);
              YYACCEPT;
          }
          argument_level++;
          got_ellipsis[argument_level] = MY_FALSE;

          /* If call_other() has been replaced by a sefun, and
           * if we need to use F_CALL_DIRECT to call it, we have
           * to insert additional code before the <expr4> already parsed.
           * Putting this code block before the <expr4> in the rule
           * however yields a faulty grammar.
           */

          if (!disable_sefuns
           && call_other_sefun >= 0
           && (unsigned long)call_other_sefun >= SEFUN_TABLE_SIZE)
#line 12952 "prolang.y"
          {
              /* The simul-efun has to be called by name:
               * insert the extra args for the call_other
               */
              char *p, *q;
              p_int left;

              if (!realloc_a_program(6))
#line 12960 "prolang.y"
              {
                  yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                          , mem_block[A_PROGRAM].current_size + 2);
                  free_fulltype((yyvsp[-2].lrvalue).type);
                  YYACCEPT;
              }

              /* Move the generated code forward by 6 */
              p = mem_block[A_PROGRAM].block + CURRENT_PROGRAM_SIZE - 1;
              q = p + 6;
              for (left = CURRENT_PROGRAM_SIZE - (yyvsp[-2].lrvalue).start
                  ; left > 0
                  ; left--, p--, q--)
                  *q = *p;

              /* p now points to program[$1.start]-1.
               * Store the first two call-other args there.
               */
              p[1] = F_STRING;
              upd_short((yyvsp[-2].lrvalue).start+1, store_prog_string(
                        ref_mstring(query_simul_efun_file_name())));
              p[4] = F_STRING;
              upd_short((yyvsp[-2].lrvalue).start+4, store_prog_string(ref_mstring(STR_CALL_OTHER)));

              CURRENT_PROGRAM_SIZE += 6;
          }

#line 12988 "prolang.y"
          /* If we received a string as call_other_name, it's a constant call.
           */
          name = (yyvsp[0].sh_string);

          if (name)
#line 12993 "prolang.y"
          {
              /* Push the function name (the expr4 is already on the stack)
               */
              ins_prog_string(name);
          } /* if (name) */
          /* otherwise the name was given by an expression for which
           * the code and value have been already generated.
           */

      }
#line 16568 "y.tab.c" /* yacc.c:1646  */
    break;

  case 359:
#line 13006 "prolang.y" /* yacc.c:1646  */
    {
          /* Now generate the CALL_OTHER resp. the SIMUL_EFUN instruction. */

          PREPARE_INSERT(10)
          Bool has_ellipsis;
          Bool ap_needed;

          has_ellipsis = got_ellipsis[argument_level];
          ap_needed = MY_TRUE;

          (yyval.function_call_result).might_lvalue = true;

          if (!disable_sefuns && call_other_sefun >= 0)
#line 13019 "prolang.y"
          {
              /* SIMUL EFUN */

              function_t *funp;
              int num_arg;

              num_arg = (yyvsp[-1].number) + 2; /* Don't forget the obj and the fun! */

              funp = &simul_efunp[call_other_sefun];
              if (num_arg > funp->num_arg
               && !(funp->flags & TYPE_MOD_XVARARGS)
               && !has_ellipsis)
                  yyerrorf("Too many arguments to simul_efun %s"
                          , get_txt(funp->name));

              /* call_other_sefun is >= 0 (see above) */
              if ((unsigned long)call_other_sefun >= SEFUN_TABLE_SIZE)
#line 13036 "prolang.y"
              {
                  /* call-other: the number of arguments will be
                   * detected and corrected at runtime.
                   */
                  add_f_code(F_CALL_DIRECT);
                  CURRENT_PROGRAM_SIZE++;
              }
              else
#line 13044 "prolang.y"
              {
                  /* Direct call */
                  if (funp->num_arg != SIMUL_EFUN_VARARGS
                   && !(funp->flags & TYPE_MOD_XVARARGS)
                   && !has_ellipsis)
#line 13049 "prolang.y"
                  {
                      int i;

                      i = funp->num_arg - num_arg;
                      if (funp->flags & TYPE_MOD_XVARARGS)
                          i--; /* Last argument may be omitted */

                      if (i > 4)
#line 13057 "prolang.y"
                      {
                          if (!realloc_a_program(i+2))
#line 13059 "prolang.y"
                          {
                              yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                                      , mem_block[A_PROGRAM].current_size + i+2);
                              YYACCEPT;
                          }

                          __PREPARE_INSERT__p = PROGRAM_BLOCK
                                                + CURRENT_PROGRAM_SIZE;
                      }
                      CURRENT_PROGRAM_SIZE += i;
                      while ( --i >= 0 )
#line 13070 "prolang.y"
                      {
                          add_f_code(F_CONST0);
                      }
                  }

                  if (funp->num_arg != SIMUL_EFUN_VARARGS
                   && !(funp->flags & TYPE_MOD_XVARARGS)
                   && !has_ellipsis)
                      ap_needed = MY_FALSE;

                  if (ap_needed)
#line 13081 "prolang.y"
                  {
                      add_f_code(F_USE_ARG_FRAME);
                      CURRENT_PROGRAM_SIZE++;
                  }
                  add_f_code(F_SIMUL_EFUN);
                  add_short(call_other_sefun);
                  CURRENT_PROGRAM_SIZE += 3;
              }
              (yyval.function_call_result).type = get_fulltype(funp->type);
          }
          else /* true call_other */
#line 13092 "prolang.y"
          {
              add_f_code(F_CALL_OTHER);
              CURRENT_PROGRAM_SIZE++;
              (yyval.function_call_result).type = get_fulltype(instrs[F_CALL_OTHER].ret_type);
          }
          (yyval.function_call_result).start = (yyvsp[-6].lrvalue).start;
          pop_arg_stack((yyvsp[-1].number));
            /* No good need of these arguments because we don't
             * know what we are going to call.
             */

          /* Do the post processing of the arg frame handling */
          if (ap_needed)
#line 13105 "prolang.y"
          {
              /* Restore the previous arg frame pointer */

              add_f_code(F_RESTORE_ARG_FRAME);
              CURRENT_PROGRAM_SIZE++;
          }
          else
#line 13112 "prolang.y"
          {
              /* Since the arg frame is not needed, remove the
               * earlier save_arg_frame instruction.
               */

              bytecode_p src, dest;
              size_t left;

              dest = PROGRAM_BLOCK + (yyvsp[-3].function_call_head).start;
              src = dest+1;
              left = CURRENT_PROGRAM_SIZE - (yyvsp[-3].function_call_head).start - 1;

              while (left-- > 0)
#line 13125 "prolang.y"
              {
                  *dest++ = *src++;
              }

              CURRENT_PROGRAM_SIZE--;

              /* If last_expression lies within the program area
               * that was moved one bytecode adjust it accordingly.
               */
              if(last_expression > (yyvsp[-3].function_call_head).start)
                  last_expression--;
          }

          argument_level--;

          free_fulltype((yyvsp[-6].lrvalue).type);
      }
#line 16721 "y.tab.c" /* yacc.c:1646  */
    break;

  case 360:
#line 13160 "prolang.y" /* yacc.c:1646  */
    { (yyval.sh_string) = (yyvsp[0].sh_string); }
#line 16727 "y.tab.c" /* yacc.c:1646  */
    break;

  case 361:
#line 13163 "prolang.y" /* yacc.c:1646  */
    { fatal("presence of rule should prevent its reduction"); }
#line 16733 "y.tab.c" /* yacc.c:1646  */
    break;

  case 362:
#line 13154 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.sh_string) = last_lex_string; /* Adopt the reference */
          last_lex_string = NULL;
      }
#line 16742 "y.tab.c" /* yacc.c:1646  */
    break;

  case 363:
#line 13160 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.sh_string) = NULL;
          if (!lpctype_contains(lpctype_string, (yyvsp[-1].rvalue).type.t_type))
              fulltype_error("Illegal type for lfun name", (yyvsp[-1].rvalue).type);
          free_fulltype((yyvsp[-1].rvalue).type);
      }
#line 16753 "y.tab.c" /* yacc.c:1646  */
    break;

  case 364:
#line 13172 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.function_name).super = NULL;
          (yyval.function_name).real  = (yyvsp[0].ident);
      }
#line 16762 "y.tab.c" /* yacc.c:1646  */
    break;

  case 365:
#line 13178 "prolang.y" /* yacc.c:1646  */
    {
          ident_t *lvar = (yyvsp[0].ident);
          ident_t *fun = find_shared_identifier_mstr(lvar->name, I_TYPE_UNKNOWN, 0);

          /* Search the inferior list for this identifier for a global
           * (function) definition.
           */

          while (fun && fun->type > I_TYPE_GLOBAL)
              fun = fun->inferior;

          if (!fun || fun->type != I_TYPE_GLOBAL)
#line 13190 "prolang.y"
          {
              yyerrorf("Undefined function '%.50s'\n", get_txt(lvar->name));
              YYACCEPT;
          }

          (yyval.function_name).super = NULL;
          (yyval.function_name).real  = fun;
      }
#line 16788 "y.tab.c" /* yacc.c:1646  */
    break;

  case 366:
#line 13200 "prolang.y" /* yacc.c:1646  */
    {
          *((yyval.function_name).super = yalloc(1)) = '\0';
          (yyval.function_name).real  = (yyvsp[0].ident);
      }
#line 16797 "y.tab.c" /* yacc.c:1646  */
    break;

  case 367:
#line 13206 "prolang.y" /* yacc.c:1646  */
    {
          ident_t *lvar = (yyvsp[0].ident);

          *((yyval.function_name).super = yalloc(1)) = '\0';
          (yyval.function_name).real  = lvar;
      }
#line 16808 "y.tab.c" /* yacc.c:1646  */
    break;

  case 368:
#line 13214 "prolang.y" /* yacc.c:1646  */
    {
#line 13216 "prolang.y"
          /* Attempt to call an efun directly even though there
           * is a nomask simul-efun for it?
           */
          if ( !strcmp((yyvsp[-2].string), "efun")
           && (yyvsp[0].ident)->type == I_TYPE_GLOBAL
           && (yyvsp[0].ident)->u.global.sim_efun >= 0
           && simul_efunp[(yyvsp[0].ident)->u.global.sim_efun].flags & TYPE_MOD_NO_MASK
           && master_ob
           && (!EVALUATION_TOO_LONG())
             )
#line 13226 "prolang.y"
          {
              /* Yup, check it with a privilege violation.
               * If it's denied, ignore the "efun::" qualifier.
               */

              svalue_t *res;

              push_ref_string(inter_sp, STR_NOMASK_SIMUL_EFUN);
              push_c_string(inter_sp, current_loc.file->name);
              push_ref_string(inter_sp, (yyvsp[0].ident)->name);
              res = apply_master(STR_PRIVILEGE, 3);
              if (!res || res->type != T_NUMBER || res->u.number < 0)
#line 13238 "prolang.y"
              {
                  yyerrorf("Privilege violation: nomask simul_efun %s"
                          , get_txt((yyvsp[0].ident)->name));
                  yfree((yyvsp[-2].string));
                  (yyval.function_name).super = NULL;
              }
              else if (!res->u.number)
#line 13245 "prolang.y"
              {
                  yfree((yyvsp[-2].string));
                  (yyval.function_name).super = NULL;
              }
              else
#line 13250 "prolang.y"
              {
                  (yyval.function_name).super = (yyvsp[-2].string);
              }
          }
          else if (EVALUATION_TOO_LONG())
#line 13255 "prolang.y"
          {
              yyerrorf("Can't call master::%s for "
                       "'nomask simul_efun %s': eval cost too big"
                      , get_txt(STR_PRIVILEGE), get_txt((yyvsp[0].ident)->name));
              yfree((yyvsp[-2].string));
              (yyval.function_name).super = NULL;
          }
          else /* the qualifier is ok */
              (yyval.function_name).super = (yyvsp[-2].string);

          (yyval.function_name).real = (yyvsp[0].ident); /* and don't forget the function ident */
      }
#line 16871 "y.tab.c" /* yacc.c:1646  */
    break;

  case 369:
#line 13269 "prolang.y" /* yacc.c:1646  */
    {
#line 13271 "prolang.y"
          ident_t *lvar = (yyvsp[0].ident);

          /* Attempt to call an efun directly even though there
           * is a nomask simul-efun for it?
           */
          if ( !strcmp((yyvsp[-2].string), "efun")
           && lvar->type == I_TYPE_GLOBAL
           && lvar->u.global.sim_efun >= 0
           && simul_efunp[lvar->u.global.sim_efun].flags & TYPE_MOD_NO_MASK
           && master_ob
           && (!EVALUATION_TOO_LONG())
             )
#line 13283 "prolang.y"
          {
              /* Yup, check it with a privilege violation.
               * If it's denied, ignore the "efun::" qualifier.
               */

              svalue_t *res;

              push_ref_string(inter_sp, STR_NOMASK_SIMUL_EFUN);
              push_c_string(inter_sp, current_loc.file->name);
              push_ref_string(inter_sp, lvar->name);
              res = apply_master(STR_PRIVILEGE, 3);
              if (!res || res->type != T_NUMBER || res->u.number < 0)
#line 13295 "prolang.y"
              {
                  yyerrorf("Privilege violation: nomask simul_efun %s"
                          , get_txt(lvar->name));
                  yfree((yyvsp[-2].string));
                  (yyval.function_name).super = NULL;
              }
              else if (!res->u.number)
#line 13302 "prolang.y"
              {
                  yfree((yyvsp[-2].string));
                  (yyval.function_name).super = NULL;
              }
              else
#line 13307 "prolang.y"
              {
                  (yyval.function_name).super = (yyvsp[-2].string);
              }
          }
          else if (EVALUATION_TOO_LONG())
#line 13312 "prolang.y"
          {
              yyerrorf("Can't call master::%s for "
                       "'nomask simul_efun %s': eval cost too big"
                      , get_txt(STR_PRIVILEGE), get_txt(lvar->name));
              yfree((yyvsp[-2].string));
              (yyval.function_name).super = NULL;
          }
          else /* the qualifier is ok */
              (yyval.function_name).super = (yyvsp[-2].string);

          (yyval.function_name).real = lvar; /* and don't forget the function ident */
      }
#line 16936 "y.tab.c" /* yacc.c:1646  */
    break;

  case 370:
#line 13330 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.string) = ystring_copy(get_txt((yyvsp[0].ident)->name));
      }
#line 16944 "y.tab.c" /* yacc.c:1646  */
    break;

  case 371:
#line 13335 "prolang.y" /* yacc.c:1646  */
    { fatal("presence of rule should prevent its reduction"); }
#line 16950 "y.tab.c" /* yacc.c:1646  */
    break;

  case 372:
#line 13338 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.string) = ystring_copy(get_txt(last_lex_string));
          free_mstring(last_lex_string);
          last_lex_string = NULL;
      }
#line 16960 "y.tab.c" /* yacc.c:1646  */
    break;

  case 373:
#line 13353 "prolang.y" /* yacc.c:1646  */
    {
          ins_f_code(F_SAVE_ARG_FRAME);
          (yyval.address) = CURRENT_PROGRAM_SIZE;
          ins_f_code(F_CATCH);
          ins_byte(0); /* Placeholder for flags */
          ins_byte(0); /* Placeholder for the jump offset */
      }
#line 16972 "y.tab.c" /* yacc.c:1646  */
    break;

  case 374:
#line 13363 "prolang.y" /* yacc.c:1646  */
    {
#line 13365 "prolang.y"
          p_int origstart, start, modstart, offset;
          p_int flags = (yyvsp[-1].number);

          /* Get the address of the CATCH instruction
           * and of the modifications
           */
          origstart = start = (yyvsp[-5].address);
          modstart = (yyvsp[-2].address);

          free_fulltype((yyvsp[-3].rvalue).type);

          /* If there were code creating modifiers, move their code
           * before the F_CATCH (currently only 'reserve' does that).
           * We need to do this before we add the END_CATCH.
           */
          if (flags & CATCH_FLAG_RESERVE)
#line 13381 "prolang.y"
          {
              shuffle_code(start, modstart, CURRENT_PROGRAM_SIZE);
              start += CURRENT_PROGRAM_SIZE - modstart;
          }

          ins_f_code(F_END_CATCH);

          /* Modify the instruction if necessary */
          if (flags)
#line 13390 "prolang.y"
          {
              bytecode_p p;
              p = PROGRAM_BLOCK + start + 1;
              *p = flags & 0xff;
          }

          /* Update the offset field of the CATCH instruction */
          offset = CURRENT_PROGRAM_SIZE - (start + 3);
          if (offset >= 0x100)
#line 13399 "prolang.y"
          {
              /* Too big offset, change
               *
               *      CATCH l
               *      <expr>
               *   l: END_CATCH
               *
               * to
               *
               *      CATCH l0
               *      BRANCH l1
               *  l0: LBRANCH l2
               *  l1: <expr>
               *  l2: END_CATCH
               */

              int i;
              bytecode_p p;

              if (!realloc_a_program(5))
#line 13419 "prolang.y"
              {
                  yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                          , CURRENT_PROGRAM_SIZE + 5);
                  YYACCEPT;
              }
              CURRENT_PROGRAM_SIZE += 5;
              p = PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE - 1;
              for( i = offset; --i >= 0; --p ) *p = p[-5];
              p[-5] = 2;
              p[-4] = F_BRANCH ;
              p[-3] = 3;
              p[-2] = F_LBRANCH;
              upd_short(start + 6, offset+2);
              if (offset > 0x7ffd)
                  yyerror("offset overflow");
          }
          else
#line 13436 "prolang.y"
          {
              mem_block[A_PROGRAM].block[start+2] = offset;
          }

          /* Restore the argument frame */
          ins_f_code(F_RESTORE_ARG_FRAME);

          (yyval.rvalue).start = origstart;
          (yyval.rvalue).type  = get_fulltype(lpctype_string);
      }
#line 17065 "y.tab.c" /* yacc.c:1646  */
    break;

  case 375:
#line 13452 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.number) = (yyvsp[0].number);
      }
#line 17073 "y.tab.c" /* yacc.c:1646  */
    break;

  case 376:
#line 13458 "prolang.y" /* yacc.c:1646  */
    {
         (yyval.number)  = 0;
      }
#line 17081 "y.tab.c" /* yacc.c:1646  */
    break;

  case 377:
#line 13466 "prolang.y" /* yacc.c:1646  */
    {
          if ((yyvsp[-2].number) & (yyvsp[0].number) & CATCH_FLAG_RESERVE)
#line 13468 "prolang.y"
          {
              /* On multiple 'reserve's, use only the first one */
              yywarnf("Multiple 'reserve' modifiers in catch()");
              insert_pop_value();
          }
          (yyval.number) = (yyvsp[-2].number) | (yyvsp[0].number);
      }
#line 17096 "y.tab.c" /* yacc.c:1646  */
    break;

  case 378:
#line 13477 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.number) = (yyvsp[0].number);
      }
#line 17104 "y.tab.c" /* yacc.c:1646  */
    break;

  case 379:
#line 13485 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.number) = 0;

          if (mstreq((yyvsp[0].sh_string), STR_NOLOG))
              (yyval.number) = CATCH_FLAG_NOLOG;
          else if (mstreq((yyvsp[0].sh_string), STR_PUBLISH))
              (yyval.number) = CATCH_FLAG_PUBLISH;
          else if (mstreq((yyvsp[0].sh_string), STR_RESERVE))
              yyerrorf("Bad 'reserve' modifier in catch(): missing expression");
          else
              yyerrorf("Illegal modifier '%s' in catch() - "
                      "expected 'nolog', 'publish' or 'reserve <expr>'"
                      , get_txt((yyvsp[0].sh_string))
                      );
          free_mstring((yyvsp[0].sh_string));
      }
#line 17125 "y.tab.c" /* yacc.c:1646  */
    break;

  case 380:
#line 13502 "prolang.y" /* yacc.c:1646  */
    {
          (yyval.number) = 0;

          if (mstreq((yyvsp[-1].sh_string), STR_NOLOG)
           || mstreq((yyvsp[-1].sh_string), STR_PUBLISH)
             )
#line 13508 "prolang.y"
          {
              yyerrorf("Bad modifier '%s' in catch(): no expression allowed"
                      , get_txt((yyvsp[-1].sh_string))
                      );
          }
          else if (mstreq((yyvsp[-1].sh_string), STR_RESERVE))
#line 13514 "prolang.y"
          {
              if (!lpctype_contains(lpctype_int, (yyvsp[0].rvalue).type.t_type))
                  yyerrorf("Bad 'reserve' expression type to catch(): %s, "
                           "expected int"
                          , get_fulltype_name((yyvsp[0].rvalue).type)
                          );
              (yyval.number) = CATCH_FLAG_RESERVE;
          }
          else
              yyerrorf("Illegal modifier '%s' in catch() - "
                      "expected 'nolog', 'publish' or 'reserve <expr>'"
                      , get_txt((yyvsp[-1].sh_string))
                      );
          free_mstring((yyvsp[-1].sh_string));
          free_fulltype((yyvsp[0].rvalue).type);
      }
#line 17160 "y.tab.c" /* yacc.c:1646  */
    break;

  case 381:
#line 13545 "prolang.y" /* yacc.c:1646  */
    {
          ins_f_code(F_SSCANF);
          ins_byte((yyvsp[-1].number) + 2);
          (yyval.rvalue).start = (yyvsp[-6].address);
          (yyval.rvalue).type = get_fulltype(lpctype_int);

          free_fulltype((yyvsp[-4].rvalue).type);
          free_fulltype((yyvsp[-2].rvalue).type);
      }
#line 17174 "y.tab.c" /* yacc.c:1646  */
    break;

  case 382:
#line 13560 "prolang.y" /* yacc.c:1646  */
    {
          ins_f_code(F_PARSE_COMMAND);
          ins_byte((yyvsp[-1].number) + 3);
          (yyval.rvalue).start = (yyvsp[-8].address);
          (yyval.rvalue).type = get_fulltype(lpctype_int);

          free_fulltype((yyvsp[-6].rvalue).type);
          free_fulltype((yyvsp[-4].rvalue).type);
          free_fulltype((yyvsp[-2].rvalue).type);
      }
#line 17189 "y.tab.c" /* yacc.c:1646  */
    break;

  case 383:
#line 13575 "prolang.y" /* yacc.c:1646  */
    { (yyval.number) = 0; }
#line 17195 "y.tab.c" /* yacc.c:1646  */
    break;

  case 384:
#line 13579 "prolang.y" /* yacc.c:1646  */
    {
#line 13581 "prolang.y"
          Bool res = add_lvalue_code(&(yyvsp[0].lvalue), F_MAKE_PROTECTED);

          (yyval.number) = (yyvsp[-2].number) + 1;
          free_lpctype((yyvsp[0].lvalue).type);

          if (!res)
              YYACCEPT;
      }
#line 17210 "y.tab.c" /* yacc.c:1646  */
    break;


#line 17214 "y.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 13594 "prolang.y" /* yacc.c:1906  */

#line 13596 "prolang.y"

#ifdef __MWERKS__
#    pragma warn_possunwant reset
#    pragma warn_implicitconv reset
#endif

/*=========================================================================*/

/*-------------------------------------------------------------------------*/
static void
define_local_variable (ident_t* name, lpctype_t* actual_type, struct lvalue_s *lv, Bool redeclare, Bool with_init)

/* This is called directly from a parser rule: <type> <name>
 * if with_init is true, then an initialization of this variable will follow.
 * if redeclare is true, then a local name is redeclared.
 * It creates the local variable and returns the corresponding lvalue
 * in lv.
 * The references of <actual_type> are NOT adopted. Although this function
 * puts a type in <lv.type>, it doesn't increment the reference count of it.
 * The calling function must add the reference, when using <lv> further.
 *
 * If <lv> is not NULL, then code for the variable as an lvalue will be
 * stored in <lv.lvalue>. If it won't be used, it should be freed using
 * free_lvalue_block().
 */

#line 13622 "prolang.y"
{
    /* redeclare:
     *    MY_FALSE: A new local variable
     *    MY_TRUE:  A local name is redeclared. If this happens
     *              on a deeper level, it is even legal.
     */

    block_scope_t *scope = block_scope + block_depth - 1;
    ident_t *q;
    bytecode_p lvalue_code;

    if (current_inline && current_inline->parse_context)
#line 13634 "prolang.y"
    {
#ifdef DEBUG_INLINES
printf("DEBUG:   context name '%s'\n", get_txt(name->name));
#endif /* DEBUG_INLINES */

        if (redeclare && current_inline->block_depth+1 <= name->u.local.depth)
#line 13640 "prolang.y"
        {
            yyerrorf("Illegal to redeclare local name '%s'"
                , get_txt(name->name));
            q = name;
        }
        else
            q = add_context_name(current_inline, name, actual_type, -1);

        scope = block_scope + current_inline->block_depth;
    }
    else
#line 13651 "prolang.y"
    {
        if(redeclare)
            q = redeclare_local(name, get_fulltype(ref_lpctype(actual_type)), block_depth);
        else
            q = add_local_name(name, get_fulltype(ref_lpctype(actual_type)), block_depth);
    }

    if (scope->clobbered)
#line 13659 "prolang.y"
    {
        /* finish the previous CLEAR_LOCALS, if any */
        if (scope->num_locals - 1 > scope->num_cleared)
            mem_block[A_PROGRAM].block[scope->addr+2]
              = (char)(scope->num_locals - 1 - scope->num_cleared);
        scope->clobbered = MY_FALSE;
        scope->num_cleared = scope->num_locals - 1;
    }

    if (scope->num_locals == scope->num_cleared + 1)
#line 13669 "prolang.y"
    {
        /* First definition of a local, so insert the
         * clear_locals bytecode and remember its position
         */
        scope->addr = mem_block[A_PROGRAM].current_size;
        ins_f_code(F_CLEAR_LOCALS);
        ins_byte(scope->first_local + scope->num_cleared);
        ins_byte(0);
    }

    if (lv)
#line 13680 "prolang.y"
    {
        lv->lvalue = alloc_lvalue_block(2);
        lvalue_code = LVALUE_BLOCK + lv->lvalue.start;

        lvalue_code[0] = F_PUSH_LOCAL_VARIABLE_LVALUE;
        lvalue_code[1] = q->u.local.num;

        lv->vlvalue_inst = F_PUSH_LOCAL_VARIABLE_VLVALUE;
        lv->num_arg = 1;

        lv->type = actual_type;
    }

    if (!with_init)
#line 13694 "prolang.y"
    {
        /* If this is a float variable, we need to insert an appropriate
         * initializer, as the default svalue-0 is not a valid float value.
         */

#line 13700 "prolang.y"
        if (actual_type == lpctype_float)
#line 13701 "prolang.y"
        {
            ins_f_code(F_FCONST0);
            ins_f_code(F_PUSH_LOCAL_VARIABLE_LVALUE);
            ins_byte(q->u.local.num);
            ins_f_code(F_VOID_ASSIGN);
            return;
        } /* if (float variable) */
    }
} /* define_local_variable() */

/*-------------------------------------------------------------------------*/
static void
init_local_variable ( ident_t* name, struct lvalue_s *lv, int assign_op
                    , fulltype_t exprtype)

/* This is called directly from a parser rule: <type> <name> = <expr>
 * It will be called after the call to define_local_variable().
 * It assigns the result of <expr> to the variable.
 *
 * The lvalue code block in <lv> will be freed.
 */

#line 13723 "prolang.y"
{
    /* We got a "<name> = <expr>" type declaration. */

#line 13727 "prolang.y"

#ifdef DEBUG_INLINES
if (current_inline && current_inline->parse_context) 
  printf("DEBUG: inline context decl: name = expr, program_size %"PRIuMPINT"\n", 
         CURRENT_PROGRAM_SIZE);
#endif /* DEBUG_INLINES */

    /* Check the assignment for validity */
    if (exact_types && !has_common_type(exprtype.t_type, lv->type))
#line 13736 "prolang.y"
    {
        yyerrorf("Bad assignment %s", get_two_lpctypes(lv->type, exprtype.t_type));
    }

    if (assign_op != F_ASSIGN)
#line 13741 "prolang.y"
    {
        yyerror("Only plain assignments allowed here");
    }

    if (!add_lvalue_code(lv, F_VOID_ASSIGN))
        return;
} /* init_local_variable() */

/*-------------------------------------------------------------------------*/
static Bool
add_lvalue_code ( struct lvalue_s * lv, int instruction)

/* Add the lvalue code held in * <lv> to the end of the program.
 * If <instruction> is not zero, it is the code for an instruction
 * to be added after the lvalue code. The lvalue code block in <lv>
 * will be freed.
 * Return TRUE on success, and FALSE on failure.
 */

#line 13760 "prolang.y"
{
    /* Create the code to push the lvalue */
    if (!add_to_mem_block(A_PROGRAM, LVALUE_BLOCK + lv->lvalue.start, lv->lvalue.size))
        return MY_FALSE;

    free_lvalue_block(lv->lvalue);
    last_expression = CURRENT_PROGRAM_SIZE;

    if (instruction != 0)
       ins_f_code(instruction);

    return MY_TRUE;
} /* add_lvalue_code() */

/*-------------------------------------------------------------------------*/
static void
insert_pop_value (void)

/* Remove the last value computed from the stack. If possible, use
 * last_expression to prohibit that value from being generated
 * in the first place.
 * TODO: check the sizeof() during bytecode cleanup...
 */

#line 13784 "prolang.y"
{
    /* We don't have to fear sideeffects and try to prevent
     * the value from being generated if the last expression is not too long
     * ago.
     */
    if (last_expression > CURRENT_PROGRAM_SIZE)
#line 13790 "prolang.y"
    {
        /* No last expression but a value to pop, interesting... */
        ins_f_code(F_POP_VALUE);
    }
    else if (last_expression == CURRENT_PROGRAM_SIZE - sizeof(bytecode_t))
#line 13795 "prolang.y"
    {
         /* The following ops have no data in the bytecode. */
        switch ( mem_block[A_PROGRAM].block[last_expression])
#line 13798 "prolang.y"
        {
                /* The following ops have no data in the bytecode. */
            case F_ASSIGN:
                mem_block[A_PROGRAM].block[last_expression] =
                F_VOID_ASSIGN;
                break;
            case F_ADD_EQ:
                mem_block[A_PROGRAM].block[last_expression] =
                F_VOID_ADD_EQ;
                break;
            case F_PRE_INC:
            case F_POST_INC:
                mem_block[A_PROGRAM].block[last_expression] =
                F_INC;
                break;
            case F_PRE_DEC:
            case F_POST_DEC:
                mem_block[A_PROGRAM].block[last_expression] =
                F_DEC;
                break;
            case F_CONST0:
            case F_CONST1:
            case F_NCONST1:
                mem_block[A_PROGRAM].current_size = last_expression;
                break;
            default:
                ins_f_code(F_POP_VALUE);
                break;
        }
    }
    else if (last_expression == CURRENT_PROGRAM_SIZE - sizeof(bytecode_t)
                                                     -  sizeof(char))
#line 13830 "prolang.y"
    {
        /* The following ops are followed by 1 chars of data in the bytecode. */
        switch ( mem_block[A_PROGRAM].block[last_expression])
#line 13833 "prolang.y"
        {
            case F_CLIT:
            case F_NCLIT:
            case F_CSTRING0:
            case F_CSTRING1:
            case F_CSTRING2:
            case F_CSTRING3:
                mem_block[A_PROGRAM].current_size = last_expression;
                break;
            default:
                ins_f_code(F_POP_VALUE);
                break;
        }
    }
    else if (last_expression == CURRENT_PROGRAM_SIZE - sizeof(bytecode_t) 
                                                     - sizeof(short))
#line 13849 "prolang.y"
    {
        /* The following ops are followed by 2 chars of data in the bytecode. */
        if ( mem_block[A_PROGRAM].block[last_expression] == F_STRING)
            mem_block[A_PROGRAM].current_size = last_expression;
        else
            ins_f_code(F_POP_VALUE);            
    }
    else if (last_expression == CURRENT_PROGRAM_SIZE - sizeof(bytecode_t) 
                                                     - sizeof(p_int))
#line 13858 "prolang.y"
    {
        /* The following ops are followed by sizeof(p_int) chars of data in 
         * the bytecode. */
        if ( mem_block[A_PROGRAM].block[last_expression] == F_NUMBER)
            mem_block[A_PROGRAM].current_size = last_expression;
        else
            ins_f_code(F_POP_VALUE);            
    }
    else
#line 13867 "prolang.y"
    {
        /* last expression unknown or too long ago - just pop whatever there
         * is on the stack */
        ins_f_code(F_POP_VALUE);
    }
    
    last_expression = -1;
} /* insert_pop_value() */

/*-------------------------------------------------------------------------*/
int
proxy_efun (int function, int num_arg UNUSED)

/* If the number of arguments doesn't fit the <function>, maybe there
 * is an alternative.
 * Return the code of the alternative efun, or -1 if there is none.
 */

#line 13885 "prolang.y"
{
#if defined(__MWERKS__)
#    pragma unused(num_arg)
#endif

    if (function == F_PREVIOUS_OBJECT)
#line 13891 "prolang.y"
    {
        /* num_arg == 0 */
        return F_PREVIOUS_OBJECT0;
    }

    return -1;
} /* proxy_efun() */

/*-------------------------------------------------------------------------*/
static void
transfer_init_control (void)

/* The compiler is about to generate another INIT fragment at the current
 * address: update the JUMP of the last INIT fragment to point to this
 * address.
 * If this is the first call, the function header for __INIT is generated
 * as well.
 */

#line 13910 "prolang.y"
{
    if (last_initializer_end < 0)
#line 13912 "prolang.y"
    {
        /* First call: we have to generate the __INIT function
         * header.
         */

        CURRENT_PROGRAM_SIZE = align(CURRENT_PROGRAM_SIZE);
          /* Add space for the function header index. */
        realloc_a_program(FUNCTION_HDR_SIZE);

        first_initializer_start =   CURRENT_PROGRAM_SIZE
                                  + FUNCTION_PRE_HDR_SIZE;
        CURRENT_PROGRAM_SIZE += FUNCTION_HDR_SIZE;
    }
    else if ((p_int)(CURRENT_PROGRAM_SIZE - sizeof(bc_offset_t)) == last_initializer_end)
#line 13926 "prolang.y"
    {
        /* The new INIT fragment directly follows the old one, so
         * just overwrite the JUMP instruction of the last.
         */
        mem_block[A_PROGRAM].current_size -= sizeof(bc_offset_t)+sizeof(bytecode_t);
    }
    else
#line 13933 "prolang.y"
    {
        /* Change the address of the last jump after the last
         * initializer to this point.
         */
        upd_jump_offset(last_initializer_end, mem_block[A_PROGRAM].current_size);
    }
    block_depth = 1;
    init_scope(block_depth);
    max_number_of_locals = max_number_of_init_locals;
} /* transfer_init_control() */

/*-------------------------------------------------------------------------*/
static void
add_new_init_jump (void)

/* The compiler just finished an INIT fragment: add a JUMP instruction
 * and let last_initializer_end point to its offset.
 */

#line 13952 "prolang.y"
{
    ins_f_code(F_JUMP);
    last_initializer_end = (bc_offset_t)CURRENT_PROGRAM_SIZE;

    max_number_of_init_locals = max_number_of_locals;
    block_depth = 0;
    
    if (realloc_a_program(sizeof(bc_offset_t)))
#line 13960 "prolang.y"
    {
        // just 'reserve' space for the offset, will be updated later.
        put_bc_offset(PROGRAM_BLOCK + last_initializer_end, 0);
        CURRENT_PROGRAM_SIZE += sizeof(bc_offset_t);
    }
    else
#line 13966 "prolang.y"
    {
        yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                , CURRENT_PROGRAM_SIZE + sizeof(bc_offset_t));
    }
} /* add_new_init_jump() */

/*-------------------------------------------------------------------------*/
static bool
is_function_defined (program_t *progp, int fx)

/* Checks whether there is code for that function.
 */
#line 13978 "prolang.y"
{
    const program_t *inhprogp;
    int inh_fx;

    get_function_header_extended(progp, fx, &inhprogp, &inh_fx);
    return !is_undef_function(inhprogp->program + (inhprogp->functions[inh_fx] & FUNSTART_MASK));
} /* is_function_defined */

/*-------------------------------------------------------------------------*/
static bool
adjust_virtually_inherited ( short *pFX, inherit_t **pIP)

/* The caller is about to create a call to the inherited function <pFX>
 * in <pIP>s program. We check whether this function is a virtually
 * inherited function (<pIP> inherited that function from another
 * program), then we replace <pIP> with the inherit pointing to that
 * virtually inherited program and adjust <pFX> accordingly.
 *
 * Returns false to indicate that this function does no longer exist
 * (when an update to the program doesn't contain it anymore). <pIP>
 * is set to NULL in that case, too. On success the function returns
 * true.
 */
#line 14001 "prolang.y"
{
    short fx = *pFX;
    inherit_t *ip = *pIP;
    funflag_t flags = ip->prog->functions[fx];

    if (flags & NAME_INHERITED)
#line 14007 "prolang.y"
    {
        /* The parent inherits the function itself: we have to
         * check if it's a virtual inheritance.
         */

        inherit_t *ip2;
        program_t *prog1, *prog2;

        prog1 = ip->prog;
        ip2 = &prog1->inherit[flags & INHERIT_MASK];
        prog2 = ip2->prog;

        if (ip2->inherit_type != INHERIT_TYPE_NORMAL)
#line 14020 "prolang.y"
        {
            /* The source was virtually inherited - we have to find
             * the first inheritance of the program.
             * And adjust the function index, of course.
             */
            do --ip; while (ip->prog != prog2);
            fx -= ip2->function_index_offset;

            /* Is there an update for this program? */
            while (ip->inherit_type & INHERIT_TYPE_MAPPED)
#line 14030 "prolang.y"
            {
                fx = GET_BLOCK(A_UPDATE_INDEX_MAP)[fx + ip->function_map_offset];
                if (fx == USHRT_MAX)
#line 14033 "prolang.y"
                {
                    /* Not defined anymore. */
                    *pIP = NULL;
                    return false;
                }

                ip = GET_BLOCK(A_INHERITS) + ip->updated_inherit;
            }

            *pFX = fx;
            *pIP = ip;
        }
    }

    return true;
}

/*-------------------------------------------------------------------------*/

static short
lookup_inherited (const char *super_name, string_t *real_name
                 , inherit_t **pIP, funflag_t *pFlags)

/* Lookup an inherited function <super_name>::<real_name> and return
 * it's function index, setting *pIP to the inherit_t pointer and
 * *pFlags to the function flags.
 * Return -1 if not found, *pIP set to NULL, and *pFlags set to 0.
 *
 * <super_name> can be an empty string or the (partial) name of one
 * of the inherits. <real_name> must be shared string.
 */
#line 14064 "prolang.y"
{
    inherit_t *ip, *foundp;
    int num_inherits, super_length;
    short found_ix;

    found_ix = -1;
    *pIP = NULL;
    *pFlags = 0;

    if (!real_name)
        return -1;

    /* Strip leading '/' */
    while (*super_name == '/')
        super_name++;
    super_length = strlen(super_name);
    num_inherits = INHERIT_COUNT;

    /* TODO: Is this really necessary?  real_name should be tabled
     * already.
     */
#line 14085 "prolang.y"
    {
        string_t *tmp;

        tmp = find_tabled(real_name);

#ifdef DEBUG
        if (!tmp)
            fprintf(stderr, "DEBUG: insert_inherited(): Can't find function "
                            "'%s'.\n", get_txt(real_name));
        else if (tmp != real_name)
            fprintf(stderr, "DEBUG: insert_inherited(): Function "
                            "'%s' is not a tabled string.\n"
                          , get_txt(real_name));
#endif
        if (tmp && tmp != real_name)
#line 14100 "prolang.y"
        {
            free_mstring(real_name);
            real_name = ref_mstring(tmp);
        }
    }

    /* Search the function in all inherits.
     * For normal inherits its sufficient to search the inherits
     * from the back in order to get the topmost definition; however,
     * with virtual inherits the order gets messed up.
     */
    ip = GET_BLOCK(A_INHERITS);
    for ( foundp = NULL ; num_inherits > 0 ; ip++, num_inherits--)
#line 14113 "prolang.y"
    {
        short i;

        if (ip->inherit_type & INHERIT_TYPE_MAPPED)
            /* this is an old inherit */
            continue;

        /* Test if super_name matches the end of the name of the inherit. */
        if (super_length)
#line 14122 "prolang.y"
        {
            /* ip->prog->name includes .c */
            int l = mstrsize(ip->prog->name)-2;

            if (l < super_length)
                continue;
            if (l > super_length
             && get_txt(ip->prog->name)[l-super_length-1] != '/')
                continue;
            if (strncmp(super_name, get_txt(ip->prog->name) + l - super_length,
                        super_length) != 0)
                continue;
        }

        /* Look for the function */
        if ( (i = find_function(real_name, ip->prog)) < 0)
            continue;

        if (!is_function_defined(ip->prog, i))
            continue;

        /* Found one */
        if (foundp == NULL
         || ip->inherit_depth < foundp->inherit_depth
           )
#line 14147 "prolang.y"
        {
            foundp = ip;
            found_ix = i;

            if (foundp->inherit_depth < 2) /* toplevel inherit */
                break;
        }
    } /* for (all inherits) */

    if (foundp != NULL)
#line 14157 "prolang.y"
    {
        if (!adjust_virtually_inherited(&found_ix, &foundp))
#line 14159 "prolang.y"
        {
            *pIP = NULL;
            *pFlags = 0;
            return -1;
        }

        /* Found it! */
        *pIP = foundp;
        *pFlags = foundp->prog->functions[found_ix];

    } /* if (foundp) */

    return found_ix;
} /* lookup_inherited() */

/*-------------------------------------------------------------------------*/
short
find_inherited_function ( const char * super_name
                        , const char * real_name
                        , unsigned short * pInherit
                        , funflag_t * flags
                        )

/* Lookup an inherited function <super_name>::<real_name> and return
 * it's function index as result, and the inheritance index in *<pInherit>.
 * Return -1 if not found.
 *
 * The returned function index is not adjusted for the compiled program's
 * function table.
 *
 * This function is called by the lexer to resolve #'<inherited_fun> closures,
 * and by restore_value()/restore_object() to restore closure values.
 *
 * <super_name> can be an empty string or the (partial) name of one
 * of the inherits.
 */

#line 14196 "prolang.y"
{
    inherit_t *ip;
    string_t *rname;
    short     ix;

    rname = find_tabled_str(real_name);

    ix =  rname ? lookup_inherited(super_name, rname, &ip, flags) : -1;
    if (ix >= 0) /* Also return the inherit index. */
        *pInherit = ip - GET_BLOCK(A_INHERITS);
    else
        *pInherit = 0;
    return ix;
} /* find_inherited_function() */

/*-------------------------------------------------------------------------*/
static int
insert_inherited (char *super_name, string_t *real_name
                 , program_t **super_p, function_t *fun_p
                 , int num_arg, bytecode_p __prepare_insert__p
                 )

/* The compiler encountered a <super_name>::<real_name>() call with
 * <num_arg> arguments; the codepointer is <__prepare_insert__p>.
 *
 * Look up the function information and set *<super_p> and *<fun_p>
 * the program pointer and the function_t information. Also compile
 * the function call(s).
 *
 * Result is the function index, or one of the negative error codes:
 * INHERITED_NOT_FOUND (-1): the function wasn't found.
 * INHERITED_WILDCARDED_ARGS (-2): it was a wildcarded supercall with
 *   arguments
 * INHERITED_WILDCARDED_NOT_FOUND (-3): it was a wildcarded supercall,
 *   but not a single function was found.

 * Result is -1 if the function wasn't found, -2 if it was a wildcarded
 * supercall to a function with arguments, otherwise the function index.
 *
 * <super_name> can be an empty string, the (partial) name of one
 * of the inherits, or a wildcarded name (and no args). In the latter
 * case, the function is called in all inherits matching the pattern.
 * The results from such a wildcarded call are returned in an array,
 * <super_p>, <fun_p> and the returned function index are those of
 * the first function found.
 */

#line 14243 "prolang.y"
{
    inherit_t *ip;
    funflag_t flags;
    short found_ix;

    found_ix = lookup_inherited(super_name, real_name, &ip, &flags);

    if (ip != NULL)
#line 14251 "prolang.y"
    {
        /* Found it! */
        bytecode_p __PREPARE_INSERT__p = __prepare_insert__p;

        /* Generate the function call */
        add_f_code(F_CALL_INHERITED);
        add_short(ip - GET_BLOCK(A_INHERITS));
        add_short(found_ix);
        CURRENT_PROGRAM_SIZE += 5;

        /* Return the program pointer */
        *super_p = ip->prog;

        /* Return a copy of the function structure */
        fun_p->flags = flags & ~INHERIT_MASK;
        get_function_information(fun_p, ip->prog, found_ix);
        fun_p->name = real_name;
        return found_ix;
    } /* if (ip) */


    /* Inherit not found, maybe it's a wildcarded call */
    if (strpbrk(super_name, "*?"))
#line 14274 "prolang.y"
    {
        Bool *was_called;  /* Flags which inh. fun has been called already */
        inherit_t *ip0;
        int num_inherits;
        int calls = 0;
        int ip_index;
        int first_index;
        short i;

        /* Prepare wildcard calls with arguments. */
        if (num_arg)
#line 14285 "prolang.y"
        {
            /* We have to put an array on the stack, behind the argument
             * frame. The size of the array we'll have to insert later,
             * as we don't know the number of calls yet.
             *
             * For each call we'll duplicate the argument frame, and enter
             * the result into that array. So the opcodes are as follows:
             *
             *   Initialize array:
             *     F_ARRAY <calls>
             *     F_MOVE_VALUE <num_arg>
             *
             *   Copy argument frame, call, enter the result:
             *     F_SAVE_ARG_FRAME
             *     F_DUP_N 1 <num_arg>
             *
             *     F_CALL_INHERITED <inh> <fx>
             *
             *     F_RESTORE_ARG_FRAME
             *     F_PUT_ARRAY_ELEMENT <num_arg+1> <num_call>
             *
             *   Repeat the step for each call.
             *
             *   Remove the arguments from the stack and move the resulting
             *   array before the argument frame.
             *     F_POP_N <num_arg>
             *     F_MOVE_VALUE 0
             *
             * As an optimization the last call doesn't need to duplicate the
             * arguments. Also the F_RESTORE_ARG_FRAME and F_SAVE_ARG_FRAME
             * between consecutive calls can be omitted.
             */

            /* 6 bytes are already reserved in the program. */
            bytecode_p __PREPARE_INSERT__p = __prepare_insert__p;

            /* Create array for the return values. */
            add_f_code(F_ARRAY0);
            add_short(0);
            /* Move array behind arguments and argument frame. */
            add_f_code(F_MOVE_VALUE);
            add_byte(num_arg);

            CURRENT_PROGRAM_SIZE += 5;
        }
        else
#line 14331 "prolang.y"
        {
            /* Without arguments we call with F_CALL_INHERITED_NOARGS.
             *
             * All the results are put consecutively on the stack.
             * There is special handling for F_CALL_INHERITED_NOARGS, so
             * that the results are not taken as arguments even though
             * they lie within the argument frame (above ap).
             *
             * Afterwards we call F_AGGREGATE to make the resulting
             * array of them.
             */
        }

        *super_p = NULL;
        num_inherits = INHERIT_COUNT;

        was_called = alloca(sizeof(*was_called)*num_inherits);
        for (i = 0; i < num_inherits; i++)
            was_called[i] = MY_FALSE;

        /* Test every inherit if the name matches and if
         * it does, generate the function call.
         */
        ip0 = GET_BLOCK(A_INHERITS);
        first_index = num_inherits > 0 ? INHERITED_WILDCARDED_NOT_FOUND
                                       : INHERITED_NOT_FOUND;
        for (; num_inherits > 0; ip0++, num_inherits--)
#line 14358 "prolang.y"
        {
            PREPARE_INSERT(13)

            /* ip->prog->name includes .c */
            int l = mstrsize(ip0->prog->name) - 2;

            ip = ip0; /* ip will be changed in the body */

            if (ip->inherit_type & INHERIT_TYPE_DUPLICATE)
                /* duplicate inherit */
                continue;

            if (ip->inherit_depth > 1)
                /* Only consider direct inherits, otherwise we would even
                 * call functions in sub-inherits which have been redefined.
                 */
                continue;

            if ( !match_string(super_name, get_txt(ip->prog->name), l) )
                continue;

            if ( (i = find_function(real_name, ip->prog)) < 0)
                continue;

            if (!is_function_defined(ip->prog, i))
                continue;

            if (!adjust_virtually_inherited(&i, &ip))
                continue;

            /* Found a match */
            flags = ip->prog->functions[i];
            ip_index = ip - GET_BLOCK(A_INHERITS);

            /* The (new) ip might be duplicate inherit, or point to
             * a virtually inherited function we called already.
             */
            if ((ip->inherit_type & INHERIT_TYPE_DUPLICATE)
             || was_called[ip_index])
                /* duplicate inherit */
                continue;

            /* Generate the function call.
             */
            if (num_arg)
#line 14403 "prolang.y"
            {
                if (!calls)
                    add_f_code(F_SAVE_ARG_FRAME);
                add_f_code(F_DUP_N);
                add_byte(1);
                add_byte(num_arg);
                add_f_code(F_CALL_INHERITED);
                add_short(ip_index);
                add_short(i);
                add_f_code(F_PUT_ARRAY_ELEMENT);
                add_short(num_arg+2);
                add_byte(calls);

                CURRENT_PROGRAM_SIZE += (calls ? 12 : 13);
            }
            else
#line 14419 "prolang.y"
            {
                add_f_code(F_CALL_INHERITED_NOARGS);
                add_short(ip_index);
                add_short(i);
                CURRENT_PROGRAM_SIZE += 5;
            }

            /* Mark this function as called */
            was_called[ip_index] = MY_TRUE;

            if (!calls) /* First function found */
#line 14430 "prolang.y"
            {
                first_index = i;

                /* Return the program pointer to the caller */
                *super_p = ip->prog;

                /* Return a copy of the function structure to the caller */
                fun_p->flags = flags & ~INHERIT_MASK;
                get_function_information(fun_p, ip->prog, i);
                fun_p->name = real_name;
            }

            calls++;
        } /* for() */

        /* The calls above left their results on the stack.
         * Combine them into a single array (which might be empty).
         */
        if (!num_arg)
#line 14449 "prolang.y"
        {
            PREPARE_INSERT(3)
            add_f_code(F_AGGREGATE);
            add_short(calls);
            CURRENT_PROGRAM_SIZE += 3;
        }
        else
#line 14456 "prolang.y"
        {
            /* Backpatch number of calls. */
            put_short(__prepare_insert__p+1, calls);

            /* And now a lot of backpatching, depending on how
             * many calls we made.
             */
            if (!calls)
#line 14464 "prolang.y"
            {
                /* Remove our code altogether and add some to
                 * remove the arguments and put an empty array
                 * as a result on the stack.
                 */
                bytecode_p __PREPARE_INSERT__p = __prepare_insert__p;

                add_f_code(F_POP_N);
                add_byte(num_arg);
                add_f_code(F_ARRAY0);
                add_short(0);
                /* 5 bytes like the original code. */
            }
            else if (calls == 1)
#line 14478 "prolang.y"
            {
                /* There was only one call, remove the F_DUP_N
                 * call above and patch the F_PUT_ARRAY_ELEMENT
                 * accordingly.
                 */
                memmove(__prepare_insert__p + 5,  __prepare_insert__p + 9, 9);
                put_short(__prepare_insert__p + 11,  1);
                put_uint8(__prepare_insert__p + 14, instrs[F_MOVE_VALUE].opcode);
                put_uint8(__prepare_insert__p + 15, 0);

                CURRENT_PROGRAM_SIZE -= 2;
            }
            else
#line 14491 "prolang.y"
            {
                /* In the last but one call an F_RESTORE_ARG_FRAME
                 * has to be inserted. In the last call the F_DUP_N
                 * must be removed.
                 */
                bytecode_p addr = PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE - 16;
                put_uint8(addr++, instrs[F_RESTORE_ARG_FRAME].opcode);
                put_uint8(addr++, instrs[F_PUT_ARRAY_ELEMENT].opcode);
                put_short(addr, num_arg+1); addr+=2;
                put_uint8(addr++, calls-2);

                memmove(addr, addr + 2, 9);
                put_short(addr + 6, 1);
                addr += 9;
                put_uint8(addr++, instrs[F_MOVE_VALUE].opcode);
                put_uint8(addr++, 0);
            }
        }

        return first_index;
    }

    /* No such function */
    return INHERITED_NOT_FOUND;
} /* insert_inherited() */

/*-------------------------------------------------------------------------*/
static void
cross_define (function_t *from, function_t *to, int32 offset)

/* The function <to> is a cross-definition from real function <from>,
 * separated by <offset>.
 * Set the flags and offset of <to> accordingly to point to <from>, and
 * synchronize the NO_MASK flag of both.
 */

#line 14527 "prolang.y"
{
    short nomask;
    to->flags = (to->flags & ~NAME_UNDEFINED)
              | (from->flags & (NAME_UNDEFINED|NAME_PROTOTYPE))
              | NAME_CROSS_DEFINED | NAME_HIDDEN | NAME_INHERITED;
    to->offset.func = MAKE_CROSSDEF_OFFSET(offset);
    nomask = (from->flags|to->flags) & TYPE_MOD_NO_MASK;
    from->flags |= nomask;
    to  ->flags |= nomask;
} /* cross_define() */

/*-------------------------------------------------------------------------*/
static void
update_function_identifier (function_t *from, short fromidx, function_t *to, short toidx)

/* The function <to> (function index <toidx> was cross-defined to the real
 * function <from> (with function index <fromidx>).
 * If there is any identifier with their name update it accordingly.
 */

#line 14547 "prolang.y"
{
    ident_t *ident = find_shared_identifier_mstr(from->name, I_TYPE_GLOBAL, 0);
    if (ident && ident->u.global.function == toidx)
#line 14550 "prolang.y"
    {
        /* We first have to check, whether <from> is also a cross-defintion. */
        while (from->flags & NAME_CROSS_DEFINED)
#line 14553 "prolang.y"
        {
            int32 offset = GET_CROSSDEF_OFFSET(from->offset.func);
            from += offset;
            fromidx += offset;
        }

        ident->u.global.function = fromidx;
    }
} /* update_function_identifier() */

/*-------------------------------------------------------------------------*/
static funflag_t *
get_virtual_function_id (program_t *progp, int fx)

/* Return a pointer to the flags of the first entry of function <fx> in <progp>
 * that is inherited virtual (i.e. the first entry we encounter that doesn't have
 * TYPE_MOD_VIRTUAL).
 *
 * This function takes care of resolving cross-definitions and inherits
 * to the real function flag.
 */

#line 14575 "prolang.y"
{
    funflag_t flags;

    assert(fx < progp->num_functions);

    flags = progp->functions[fx];

    /* Handle a cross-define */
    if (flags & NAME_CROSS_DEFINED)
#line 14584 "prolang.y"
    {
        fx += CROSSDEF_NAME_OFFSET(flags);
        flags = progp->functions[fx];
    }

    /* Walk the inherit chain */
    while((flags & (NAME_INHERITED|TYPE_MOD_VIRTUAL)) == (NAME_INHERITED|TYPE_MOD_VIRTUAL))
#line 14591 "prolang.y"
    {
        inherit_t *inheritp;

        inheritp = &progp->inherit[flags & INHERIT_MASK];
        progp = inheritp->prog;
        fx -= inheritp->function_index_offset;
        flags = progp->functions[fx];
    }

    /* This is the one */
    return &progp->functions[fx];
} /* get_virtual_function_id() */

/*-------------------------------------------------------------------------*/

static void
copy_structs (program_t *from, funflag_t flags)

/* Copy the struct definitions from program <from> which is inherited
 * with visibility <flags>.
 */

#line 14613 "prolang.y"
{
    int struct_id;

    for (struct_id = 0; struct_id < from->num_structs; struct_id++)
#line 14617 "prolang.y"
    {
        int id;
        ident_t *p;
        struct_def_t *pdef = from->struct_defs + struct_id;
        funflag_t f;

        /* Combine the visibility flags. */
        f = flags;
        if (pdef->flags & TYPE_MOD_PUBLIC)
            f &= ~(TYPE_MOD_PRIVATE | TYPE_MOD_PROTECTED);

        f |= pdef->flags;

        if (f & (TYPE_MOD_PRIVATE | NAME_HIDDEN))
            f = (f | NAME_HIDDEN) & ~(TYPE_MOD_PROTECTED | TYPE_MOD_PUBLIC);
        else if (f & TYPE_MOD_PROTECTED)
            f &= ~(TYPE_MOD_PUBLIC);

        /* Duplicate definition? */
        id = find_struct(struct_t_name(pdef->type));
        if (!(f & NAME_HIDDEN) && id >= 0)
#line 14638 "prolang.y"
        {
            /* We have a struct with this name. Check if we just
             * inherited it again, or if it's a name clash.
             */
            if (STRUCT_DEF(id).type != pdef->type)
#line 14643 "prolang.y"
            {
                if (STRUCT_DEF(id).inh >= 0)
#line 14645 "prolang.y"
                {
                    inherit_t * pInh = &INHERIT(STRUCT_DEF(id).inh);
                    yyerrorf("Different structs '%s' inherited from '%s' "
                             "and from '%s'"
                            , get_txt(struct_t_name(STRUCT_DEF(id).type))
                            , get_txt(from->name)
                            , get_txt(pInh->prog->name)
                            );
                }
                else
                    yyerrorf("struct '%s' inherited from '%s' differs "
                             "from existing struct"
                            , get_txt(struct_t_name(STRUCT_DEF(id).type))
                            , get_txt(from->name)
                            );
                continue;
            }

            f |= NAME_HIDDEN;
        }

        /* New struct */
        p = make_global_identifier(get_txt(struct_t_name(pdef->type)), I_TYPE_GLOBAL);
        if (p == NULL)
            continue;

        /* Create a new struct entry, then replace the struct prototype
         * type with the one we inherited.
         */
        current_struct = define_new_struct( MY_FALSE, p, get_txt(struct_t_pname(pdef->type)), f);
        free_struct_type(STRUCT_DEF(current_struct).type);
        STRUCT_DEF(current_struct).type = ref_struct_type(pdef->type);
        STRUCT_DEF(current_struct).inh = INHERIT_COUNT;
        update_struct_type(STRUCT_DEF(current_struct).type->name->lpctype, pdef->type);
    }
} /* copy_structs() */

/*-------------------------------------------------------------------------*/
static bool
inherit_functions (program_t *from, uint32 inheritidx)

/* Copies the function table from <from> into our program.
 *
 * Put's NAME_INHERITED entries into our program for each
 * function entry in <from>'s program. The inherit index
 * is set to <inheritidx> and the visibility is set to
 * NAME_HIDDEN.
 *
 * Returns true on success, false otherwise (out of memory).
 */

#line 14696 "prolang.y"
{
    function_t *fun_p;

    /* Make space for the inherited function structures */
    if(!RESERVE_FUNCTIONS(from->num_functions))
        return false;

    /* The new functions will be stored from here */
    fun_p = FUNCTION(FUNCTION_COUNT);

    /* Copy the function definitions one by one and adjust the flags. */
    for (int i = 0; i < from->num_functions; i++, fun_p++)
#line 14708 "prolang.y"
    {
        funflag_t  flags;
        int i2; /* The index of the real function */

        flags = from->functions[i];
        fun_p->offset.inherit = inheritidx;
        i2 = i;

        if (flags & NAME_INHERITED)
#line 14717 "prolang.y"
        {
            /* The inherit-index has to be recomputed */
            fun_p->flags =
                (flags & ~INHERIT_MASK) | NAME_INHERITED | NAME_HIDDEN;

            /* If cross-defined, get the real function index */
            if (flags & NAME_CROSS_DEFINED)
#line 14724 "prolang.y"
            {
                fun_p->offset.func = flags & INHERIT_MASK;
                i2 += CROSSDEF_NAME_OFFSET(flags);
            }
        }
        else
#line 14730 "prolang.y"
        {
            /* Also, the function-code offset needs adjustment */
            fun_p->flags =
                (flags & ~FUNSTART_MASK) | NAME_INHERITED | NAME_HIDDEN;
        }

        /* Copy the function information */
        get_function_information(fun_p, from, i2);


        /* Copy information about the types of the arguments, if it is
         * available.
         */
        A_ARGUMENT_INDEX_t argindex = INDEX_START_NONE; /* Presume not available. */
        if (from->type_start != 0)
#line 14745 "prolang.y"
        {
            if (from->type_start[i] != INDEX_START_NONE)
#line 14747 "prolang.y"
            {
                /* They are available for function number 'i'. Copy types of
                 * all arguments, and remember where they started.
                 */
                argindex = ARGTYPE_COUNT;
                if (fun_p->num_arg)
#line 14753 "prolang.y"
                {
                    int ix;

                    ix = ARGTYPE_COUNT;

                    add_to_mem_block(
                      A_ARGUMENT_TYPES,
                      &from->argument_types[from->type_start[i]],
                      (sizeof (A_ARGUMENT_TYPES_t)) * fun_p->num_arg
                    );

                    for ( ; (size_t)ix < ARGTYPE_COUNT; ix++)
                        ref_lpctype(ARGUMENT_TYPE(ix));
                }

            }
        }
        else
#line 14771 "prolang.y"
        {
            fun_p->flags |= NAME_TYPES_LOST;
        }

        /* Save the index where they started. Every function will have an
         * index where the type info of arguments starts.
         */
        ADD_ARGUMENT_INDEX(argindex);

    } /* for (inherited functions) pass 1 */

    return true;
}

/*-------------------------------------------------------------------------*/
static bool
inherit_variable (variable_t *variable, funflag_t varmodifier, int redeclare)

/* Copy a single variable into our program. If <redeclare> >= 0, then
 * it's a virtual variable and already exists with that number. The modifiers
 * of it will then be adjusted accordingly.
 *
 * Returns true on success, false otherwise (out of memory).
 */

#line 14796 "prolang.y"
{
    ident_t *p;
    funflag_t new_type = varmodifier;

    p = make_global_identifier(get_txt(variable->name), I_TYPE_GLOBAL);
    if (!p)
        return false;

    /* 'public' variables should not become private when inherited
     * 'private'.
     */
    if (variable->type.t_flags & TYPE_MOD_PUBLIC)
        new_type &= ~(TYPE_MOD_PRIVATE | TYPE_MOD_PROTECTED);

    fulltype_t vartype = variable->type;

    vartype.t_flags |= new_type 
                    | (variable->type.t_flags & TYPE_MOD_PRIVATE
                       ? (NAME_HIDDEN|NAME_INHERITED)
                       :  NAME_INHERITED
                      );
    /* The most restrictive visibility wins. */
    if (vartype.t_flags & (TYPE_MOD_PRIVATE | NAME_HIDDEN))
        vartype.t_flags &= ~(TYPE_MOD_PROTECTED | TYPE_MOD_PUBLIC);
    else if (vartype.t_flags & TYPE_MOD_PROTECTED)
        vartype.t_flags &= ~(TYPE_MOD_PUBLIC);

    if (redeclare >= 0)
        redeclare_variable(p, vartype, VIRTUAL_VAR_TAG | redeclare);
    else
        define_variable(p, vartype);
    return true;

} /* inherit_variable() */

/*-------------------------------------------------------------------------*/
static bool
is_old_inherited_function (program_t* from, int first_function_index, inherit_t *oldinheritp, int oldix, bool update_existing)

/* Check whether the function <oldix> in <oldinheritp> is actually inherited
 * from there in <from> (where it is at <first_function_index>+<oldix>).
 * <update_existing> is true, if <oldinheritp> was already fully handled.
 */

#line 14840 "prolang.y"
{
    function_t* oldfunp = FUNCTION(oldinheritp->function_index_offset);
    function_t* realoldfunp = oldfunp + oldix;

    while (realoldfunp->flags & NAME_CROSS_DEFINED)
        realoldfunp = realoldfunp + GET_CROSSDEF_OFFSET(realoldfunp->offset.func);

    /* Check whether it's still the original function from <oldinheritp>.
     * First condition: It must be in <oldinheritp>'s function block.
     */
    if (realoldfunp < oldfunp || oldfunp + oldinheritp->prog->num_functions <= realoldfunp)
        return false;

    if (update_existing)
#line 14854 "prolang.y"
    {
        /* Function entry must point to <oldinheritp>. */
        return ((realoldfunp->flags & NAME_INHERITED) && realoldfunp->offset.inherit == (oldinheritp - &INHERIT(0)));
    }
    else
#line 14859 "prolang.y"
    {
        /* Function entry must point to <oldinheritp>, but because the inherit handling
         * hasn't processed the function block fully, we must look at <from>'s function table.
         */
        funflag_t oldfunflag = from->functions[first_function_index + realoldfunp - oldfunp];
        return (oldfunflag & (NAME_INHERITED|NAME_CROSS_DEFINED)) == NAME_INHERITED
            && from->inherit[oldfunflag & INHERIT_MASK].prog == oldinheritp->prog;
    }
} /* is_old_inherited_function() */

/*-------------------------------------------------------------------------*/
static void
update_duplicate_function (program_t* from, int first_function_index, inherit_t *oldinheritp, inherit_t *newinheritp, int oldix, int newix, bool update_existing)

/* Do the cross-definition for a single function <oldix> in <oldinheritp> to <newix> in <newinheritp>.
 * The function block of <oldinheritp> is at <first_function_index> in <from>.
 * <update_existing> is true, if <oldinheritp> was already fully handled.
 */

#line 14878 "prolang.y"
{
    function_t* oldfunp = FUNCTION(oldinheritp->function_index_offset);
    function_t* newfunp = FUNCTION(newinheritp->function_index_offset);

    /* A visible entry can't be a cross-definition in an inherited entry. */
    assert(!(newfunp[newix].flags & NAME_CROSS_DEFINED));

    if (is_old_inherited_function(from, first_function_index, oldinheritp, oldix, update_existing))
#line 14886 "prolang.y"
    {
        /* The function was not cross-defined or overridden in <from>.
         * So, let's cross-define them, from old to new.
         */
        newfunp[newix].flags = (newfunp[newix].flags & ~(NAME_HIDDEN))
                             | (oldfunp[oldix].flags & (TYPE_MOD_STATIC|TYPE_MOD_PRIVATE|TYPE_MOD_PROTECTED|NAME_HIDDEN))
                             | TYPE_MOD_VIRTUAL;

        cross_define(newfunp + newix, oldfunp + oldix,
            newinheritp->function_index_offset + newix - oldinheritp->function_index_offset - oldix);

        update_function_identifier(newfunp + newix, newinheritp->function_index_offset + newix,
                                   oldfunp + oldix, oldinheritp->function_index_offset + oldix);
    }
    else
#line 14901 "prolang.y"
    {
        /* <from> already did overload or cross-define it.
         * Applying that to the new function.
         */
        cross_define(oldfunp + oldix, newfunp + newix,
            oldinheritp->function_index_offset + oldix - newinheritp->function_index_offset - newix);

        update_function_identifier(oldfunp + oldix, oldinheritp->function_index_offset + oldix,
                                   newfunp + newix, newinheritp->function_index_offset + newix);
    }

} /* update_duplicatefunction() */

/*-------------------------------------------------------------------------*/
static void
update_duplicate_functions (program_t* from, int first_function_index, inherit_t *oldinheritp, inherit_t *dupinheritp, inherit_t *newinheritp, bool update_existing)

/* Do the cross-definitions to <newinheritp> that update_virtual_program()
 * did for another inherit entry <oldinheritp> to the duplicate <dupinheritp>
 * of it (with functions corresponding to the functions starting with
 * <first_function_index> in <from>'s program).
 * <update_existing> is true, if <dupinheritp> was already fully handled.
 */

#line 14925 "prolang.y"
{
    program_t*  oldprogp = dupinheritp->prog;
    function_t* newfunp = FUNCTION(newinheritp->function_index_offset);
    function_t* oldfunp = FUNCTION(dupinheritp->function_index_offset);
    A_UPDATE_INDEX_MAP_t* fun_map = GET_BLOCK(A_UPDATE_INDEX_MAP) + oldinheritp->function_map_offset;

    /* And now cross-define as given in the function index map. */
    for (int oldix = 0; oldix < oldprogp->num_functions; oldix++)
#line 14933 "prolang.y"
    {
        if (fun_map[oldix] == USHRT_MAX)
#line 14935 "prolang.y"
        {
            /* No destination but visible, make it undefined. */
            if (!(oldfunp[oldix].flags & NAME_HIDDEN)
             && is_old_inherited_function(from, first_function_index, dupinheritp, oldix, update_existing))
                oldfunp[oldix].flags = NAME_UNDEFINED|NAME_HIDDEN|TYPE_MOD_PRIVATE;

            continue;
        }

        update_duplicate_function(from, first_function_index, dupinheritp, newinheritp, oldix, fun_map[oldix], update_existing);
    }

    /* And hide any surfaced functions. */
#line 14948 "prolang.y"
    {
        program_t* newprogp = newinheritp->prog;
        unsigned short *newix = newprogp->function_names;
        int newleft = newprogp->num_function_names;

        for (; --newleft >= 0; newix++)
#line 14954 "prolang.y"
        {
            if ( (newfunp[*newix].flags & (NAME_HIDDEN|NAME_CROSS_DEFINED)) == NAME_HIDDEN )
                newfunp[*newix].flags |= TYPE_MOD_PRIVATE;
        }
    }

} /* update_duplicate_functions() */

/*-------------------------------------------------------------------------*/
static bool
update_virtual_program (program_t *from, inherit_t *oldinheritp, inherit_t *newinheritp, int num_old_variables, int num_new_variables, int first_function_index, int first_variable_index, bool update_existing, funflag_t varmodifier)

/* Patch the inherited old program in <oldinheritp> to the new program in
 * <newinheritp>. We may either have already inherited the new program
 * and trying to inherit an old one from <from> (<update_existing> == false),
 * or have inherited an old one und trying to add the new program
 * (<update_existing> == true).
 *
 * <num_old_variables> contains the number of variables in the old
 * program itself (not counting inherited virtual variables).
 * <first_variable_index> denotes the index of the first variable
 * of <oldinheritp> (when <update_existing> == false) resp.
 * <newinheritp> (<update_existing> == true) in <from>.
 *
 * These steps need to be done in these cases:
 * 1. Create a table that maps each variable from the old
 *    to the new program.
 * 2. If <update_existing> == false, inherit the variables
 *    that aren't in the new program anymore.
 *    If <update_existing> == true, remove any old variables
 *    that are in the new program, too.
 * 3. Create the function index mapping and cross-define the functions.
 * 4. If <update_existing> == true, fix all duplicate entries
 *    of the old program that are virtual, too.
 */

#line 14990 "prolang.y"
{
    A_UPDATE_INDEX_MAP_t *var_map, *fun_map;
    int newvx;
    inherit_t updateinherit;
    program_t *oldprogp = oldinheritp->prog, *newprogp = newinheritp->prog;
    int num_old_virtual_variables = oldprogp->num_variables - num_old_variables;
    int num_new_virtual_variables = newprogp->num_variables - num_new_variables;
    variable_t *last_additional_var;

    assert(num_old_virtual_variables == oldprogp->num_virtual_variables);
    assert(num_new_virtual_variables == newprogp->num_virtual_variables);

    /* We'll add an additional inherit entry for the
     * updated program.
     */
    updateinherit = *newinheritp;
    updateinherit.inherit_depth = oldinheritp->inherit_depth;

    if (update_existing)
        newinheritp->inherit_type |= INHERIT_TYPE_DUPLICATE;
    else
        updateinherit.inherit_type |= INHERIT_TYPE_DUPLICATE;

    oldinheritp->updated_inherit = INHERIT_COUNT;
    oldinheritp->inherit_type |= INHERIT_TYPE_MAPPED;
    oldinheritp->num_additional_variables = 0;
    oldinheritp->variable_map_offset = UPDATE_INDEX_MAP_COUNT;
    oldinheritp->function_map_offset = UPDATE_INDEX_MAP_COUNT + num_old_variables;

    /* And now map *every* old variable to the new one. */

    /* We could have saved <num_old_virtual_variables> entries, because
     * these are never mapped, as they are virtual in <oldprogp> itself.
     * But that would require either recording this number in <oldprog> or
     * <oldinheritp>, or subtracting the number from the offsets, which
     * might make them negative (but they are unsigned shorts). So we'll
     * include them for now...
     */
    if (!RESERVE_UPDATE_INDEX_MAP(num_old_variables + oldprogp->num_functions))
        return false;

    /* We'll walk through the older program's variable and have
     * to find the corresponding variable in the newer program.
     * We do this by walking in parallel through the newer
     * program's variables and looking for a variable with the same
     * name. We can't use a lookup table, as there may be several
     * variables with the same name. So we use the same lookup
     * type as restore_object and hope for the correct order of variables.
     */
    var_map = GET_BLOCK(A_UPDATE_INDEX_MAP) + oldinheritp->variable_map_offset;
    newvx = num_new_virtual_variables;
    last_additional_var = V_VARIABLE(oldinheritp->variable_index_offset + VIRTUAL_VAR_TAG);

    for (int oldvx = num_old_virtual_variables; oldvx < oldprogp->num_variables; oldvx++)
#line 15044 "prolang.y"
    {
        variable_t *oldvar = oldprogp->variables + oldvx;
        int lastnewvx = newvx;
        bool varfound = false;

        do
#line 15050 "prolang.y"
        {
            variable_t *newvar = newprogp->variables + newvx;
            if (mstreq(oldvar->name, newvar->name))
#line 15053 "prolang.y"
            {
                varfound = true;
                break;
            }

            newvx++;
            if (newvx == newprogp->num_variables)
                newvx = num_new_virtual_variables;
        } while (newvx != lastnewvx);

        if (varfound)
#line 15064 "prolang.y"
        {
            *var_map = newvx - num_new_virtual_variables;

            if (update_existing)
#line 15068 "prolang.y"
            {
                /* We have to remove the variables from the A_VIRTUAL_VAR
                 * block and cancel the global identifier (by setting
                 * the variable index back to I_GLOBAL_VARIABLE_OTHER).
                 */
                int cur_oldindex = oldinheritp->variable_index_offset + oldvx - num_old_virtual_variables;
                variable_t *cur_oldvar = V_VARIABLE(cur_oldindex + VIRTUAL_VAR_TAG);
                ident_t *oldvar_ident = find_shared_identifier_mstr(cur_oldvar->name, I_TYPE_GLOBAL, 0);

                /* There should have been an identifier, as that is a defined variable. */
                assert(oldvar_ident != NULL);
                /* And it should be a global identifier. */
                assert(oldvar_ident->type == I_TYPE_GLOBAL);

                /* Not redefined? */
                if (oldvar_ident->u.global.variable == (VIRTUAL_VAR_TAG | cur_oldindex))
                    oldvar_ident->u.global.variable = I_GLOBAL_VARIABLE_OTHER;

                free_mstring(cur_oldvar->name);
                free_fulltype(cur_oldvar->type);
            }
            else
#line 15090 "prolang.y"
            {
                /* We have to update the modifier. */
                int cur_newindex = newinheritp->variable_index_offset + newvx - num_new_virtual_variables;
                if (!inherit_variable(from->variables + first_variable_index + oldvx - num_old_virtual_variables, varmodifier, cur_newindex))
                    return false;
            }

            newvx++;
            if (newvx == newprogp->num_variables)
                newvx = num_new_virtual_variables;
        }
        else
#line 15102 "prolang.y"
        {
            /* The old variable doesn't exist anymore,
             * but it may be referenced by the inherited program,
             * so we have to preserve that variable.
             */
            *var_map = num_new_variables + oldinheritp->num_additional_variables;

            if (update_existing)
#line 15110 "prolang.y"
            {
                int cur_oldindex = oldinheritp->variable_index_offset + oldvx - num_old_virtual_variables;
                variable_t *cur_oldvar = V_VARIABLE(cur_oldindex + VIRTUAL_VAR_TAG);
                if (last_additional_var != cur_oldvar)
#line 15114 "prolang.y"
                {
                    /* We have to move the variable. */
                    ident_t *oldvar_ident = find_shared_identifier_mstr(cur_oldvar->name, I_TYPE_GLOBAL, 0);

                    /* There should have been an identifier, as that is a defined variable. */
                    assert(oldvar_ident != NULL);
                    /* And it should be a global identifier. */
                    assert(oldvar_ident->type == I_TYPE_GLOBAL);

                    if (oldvar_ident->u.global.variable == (VIRTUAL_VAR_TAG | cur_oldindex))
                        oldvar_ident->u.global.variable = VIRTUAL_VAR_TAG | (last_additional_var - V_VARIABLE(VIRTUAL_VAR_TAG));

                    *last_additional_var = *cur_oldvar;
                }

                last_additional_var++;
            }
            else
#line 15132 "prolang.y"
            {
                /* We'll take the variable description from <from>
                 * instead of <oldprogp>, because the visibility
                 * might have changed.
                 */
                if (!inherit_variable(from->variables + first_variable_index + oldvx - num_old_virtual_variables, varmodifier, -1))
                    return false;
            }

            oldinheritp->num_additional_variables++;
        }
        var_map++;
    }

    mem_block[A_UPDATE_INDEX_MAP].current_size += oldprogp->num_variables * sizeof(A_UPDATE_INDEX_MAP_t);

    if (update_existing && last_additional_var != V_VARIABLE(oldinheritp->variable_index_offset + num_old_variables + VIRTUAL_VAR_TAG))
#line 15149 "prolang.y"
    {
        /* We have to move all the variables that came after oldinheritp. */
        int last_variable = V_VARIABLE_COUNT;
        int from_idx = oldinheritp->variable_index_offset + num_old_variables, to_idx = last_additional_var - V_VARIABLE(VIRTUAL_VAR_TAG);
        int diff = from_idx - to_idx;
        inherit_t *last_inherit = &INHERIT(INHERIT_COUNT);

        for (inherit_t* inh = oldinheritp + 1; inh < last_inherit; inh++)
#line 15157 "prolang.y"
        {
            if(inh->variable_index_offset & NON_VIRTUAL_OFFSET_TAG)
                continue;

            if(inh->inherit_type & (INHERIT_TYPE_DUPLICATE))
                continue;

            inh->variable_index_offset -= diff;
        }

        for (; from_idx < last_variable; from_idx++, to_idx++)
#line 15168 "prolang.y"
        {
            variable_t *var = V_VARIABLE(from_idx + VIRTUAL_VAR_TAG);
            ident_t *var_ident = find_shared_identifier_mstr(var->name, I_TYPE_GLOBAL, 0);

            /* There should have been an identifier, as that is a defined variable. */
            assert(var_ident != NULL);
            /* And it should be a global identifier. */
            assert(var_ident->type == I_TYPE_GLOBAL);

            if (var_ident->u.global.variable == (VIRTUAL_VAR_TAG | from_idx))
                var_ident->u.global.variable = VIRTUAL_VAR_TAG | to_idx;

            *V_VARIABLE(to_idx + VIRTUAL_VAR_TAG) = *var;
        }

        newinheritp->variable_index_offset -= diff;
        updateinherit.variable_index_offset -= diff;
        mem_block[A_VIRTUAL_VAR].current_size -= diff * sizeof(A_VIRTUAL_VAR_t);
    }

    /* So long for the variables, now copy the new functions
     * and cross-define them to the old functions.
     *
     * We need only the cross-define visible functions,
     * because the invisible ones wouldn't be called from
     * outside of <progp> (and <progp> isn't gonna be used
     * anymore).
     * That's why we can walk the function_names list
     * on <inhprog> and <progp> and thus find common
     * functions (both lists are sorted).
     */
    fun_map = GET_BLOCK(A_UPDATE_INDEX_MAP) + oldinheritp->function_map_offset;

#line 15201 "prolang.y"
    {
        function_t *newfunp;
        function_t *oldfunp;
        unsigned short *oldix = oldprogp->function_names;
        unsigned short *newix = newprogp->function_names;
        int oldleft = oldprogp->num_function_names;
        int newleft = newprogp->num_function_names;

        /* Init each function map entry as undefined. */
        for (int j = 0; j < oldprogp->num_functions; j++)
            fun_map[j] = USHRT_MAX;

        updateinherit.function_index_offset = FUNCTION_COUNT;
        if (!inherit_functions(newprogp, INHERIT_COUNT))
            return false;

        mem_block[A_FUNCTIONS].current_size += sizeof(A_FUNCTIONS_t) * newprogp->num_functions;

        newfunp = FUNCTION(updateinherit.function_index_offset);
        oldfunp = FUNCTION(oldinheritp->function_index_offset);

        for (;oldleft && newleft; oldix++, oldleft--)
#line 15223 "prolang.y"
        {
            string_t *oldname = oldfunp[*oldix].name;
            int cmp = -1;

            /* Similar to function_cmp() in closure.c,
             * Go through the new functions as long as oldname is smaller.
             */
            while(newleft && (cmp = memcmp(&oldname, &newfunp[*newix].name, sizeof(string_t*))) > 0)
#line 15231 "prolang.y"
            {
                newix++;
                newleft--;
            }

            if (!cmp) /* They are equal (and therefore newleft > 0) */
#line 15237 "prolang.y"
            {
                function_t *curoldfunp = oldfunp + *oldix;
                function_t *curnewfunp = newfunp + *newix;

                update_duplicate_function(from, first_function_index, oldinheritp, &updateinherit, *oldix, *newix, update_existing);
                fun_map[*oldix] = *newix;

                /* Check whether their signatures are compatible
                 * and throw a warning otherwise.
                 */
                if (curoldfunp->type != NULL
                 && curoldfunp->type != lpctype_unknown
                 && curnewfunp->type != NULL
                 && curnewfunp->type != lpctype_unknown)
#line 15251 "prolang.y"
                {
                    /* These are the same checks as with function overloads. */
                    bool differs = false;

                    if ((curoldfunp->num_arg > curnewfunp->num_arg
                      && !(curoldfunp->flags & TYPE_MOD_VARARGS))
                     || (curoldfunp->num_arg == curnewfunp->num_arg
                      && ((curoldfunp->flags ^ curnewfunp->flags) & TYPE_MOD_XVARARGS)
                      && !(curoldfunp->flags & TYPE_MOD_VARARGS)))
#line 15260 "prolang.y"
                    {
                        differs = true;
                        yywarnf("Number of arguments changed when updating '%s'.", get_txt(oldname));
                    }

                    if (!differs && !has_common_type(curoldfunp->type, curnewfunp->type))
#line 15266 "prolang.y"
                    {
                        differs = true;
                        yywarnf("Return type changed when updating '%s'; %s", get_txt(oldname), get_two_lpctypes(curoldfunp->type, curnewfunp->type));
                    }

                    if (!differs
                     && !(curoldfunp->flags & NAME_TYPES_LOST)
                     && !(curnewfunp->flags & NAME_TYPES_LOST))
#line 15274 "prolang.y"
                    {
                        lpctype_t **oldargp = &ARGUMENT_TYPE(ARGUMENT_INDEX(oldinheritp->function_index_offset + *oldix));
                        lpctype_t **newargp = &ARGUMENT_TYPE(ARGUMENT_INDEX(updateinherit.function_index_offset + *newix));

                        int num_arg = curoldfunp->num_arg;
                        if (num_arg > curnewfunp->num_arg)
                            num_arg = curnewfunp->num_arg;
                        if (oldfunp->flags & TYPE_MOD_XVARARGS)
                            num_arg--;

                        for (int i = 0; i < num_arg; i++)
#line 15285 "prolang.y"
                        {
                            if (!has_common_type(oldargp[i], newargp[i]))
                                yywarnf("Argument type changed when updating '%s': arg %d %s", get_txt(oldname), i+1, get_two_lpctypes(oldargp[i], newargp[i]));
                        }
                    }
                }
            }
        }

        /* And now make all functions that have not been
         * cross-defined as undefined.
         */
        oldix = oldprogp->function_names;
        for (oldleft = oldprogp->num_function_names; --oldleft >= 0; oldix++)
#line 15299 "prolang.y"
        {
            if (fun_map[*oldix] != USHRT_MAX)
                continue;

            if (is_old_inherited_function(from, first_function_index, oldinheritp, *oldix, update_existing))
                oldfunp[*oldix].flags = NAME_UNDEFINED|NAME_HIDDEN|TYPE_MOD_PRIVATE;
        }

        /* Now we have to take care of the remaining functions with names
         * (the ones still have NAME_HIDDEN, but not being NAME_CROSS_DEFINED).
         * They must either be a registered identifier, being cross-defined,
         * or hidden as TYPE_MOD_PRIVATE. The first and second options
         * one would require to implement the function inherit logic for
         * a function that was inherited and the old inherit's place
         * with the old inherit's modifier. Therefore we use the simple
         * approach and keep all surfacing functions hidden (because they
         * were not part of the inherited program).
         */

        /* Reset newix and newleft. */
        newix = newprogp->function_names;
        newleft = newprogp->num_function_names;

        for (; --newleft >= 0; newix++)
#line 15323 "prolang.y"
        {
            if ( (newfunp[*newix].flags & (NAME_HIDDEN|NAME_CROSS_DEFINED)) == NAME_HIDDEN )
                newfunp[*newix].flags |= TYPE_MOD_PRIVATE;
        }
    }

    mem_block[A_UPDATE_INDEX_MAP].current_size += oldprogp->num_functions * sizeof(A_UPDATE_INDEX_MAP_t);

    ADD_INHERIT(&updateinherit);

    if (update_existing)
#line 15334 "prolang.y"
    {
        /* We'll have to do these cross-definitions for each
         * duplicate inherit entry of the old program.
         * But now we can use the update map for that.
         */
        inherit_t* last_inherit = &INHERIT(INHERIT_COUNT);

        for (inherit_t* dupinheritp = oldinheritp + 1; dupinheritp < last_inherit; dupinheritp++)
#line 15342 "prolang.y"
        {
            if (dupinheritp->prog == oldprogp && dupinheritp->inherit_type != INHERIT_TYPE_NORMAL)
#line 15344 "prolang.y"
            {
                inherit_t dupupdate;

                /* If this is a virtual inherit, it must be a duplicate. */
                assert((dupinheritp->inherit_type & (INHERIT_TYPE_DUPLICATE|INHERIT_TYPE_MAPPED)) == INHERIT_TYPE_DUPLICATE);

                dupupdate = updateinherit;
                dupupdate.inherit_type |= INHERIT_TYPE_DUPLICATE;
                dupupdate.inherit_depth = dupinheritp->inherit_depth;
                dupupdate.function_index_offset = FUNCTION_COUNT;

                dupinheritp->inherit_type |= INHERIT_TYPE_MAPPED;
                dupinheritp->updated_inherit = INHERIT_COUNT;
                dupinheritp->variable_index_offset = oldinheritp->variable_index_offset;
                dupinheritp->num_additional_variables = oldinheritp->num_additional_variables;
                dupinheritp->variable_map_offset = oldinheritp->variable_map_offset;
                dupinheritp->function_map_offset = oldinheritp->function_map_offset;

                if (!inherit_functions(dupupdate.prog, INHERIT_COUNT))
                    return false;

                mem_block[A_FUNCTIONS].current_size += sizeof(A_FUNCTIONS_t) * dupupdate.prog->num_functions;

                /* And now cross-define as given in the function index map. */
                update_duplicate_functions(from, first_function_index, oldinheritp, dupinheritp, &dupupdate, true);

                ADD_INHERIT(&dupupdate);
            }
        }
    }

    return true;
} /* update_virtual_program() */

/*-------------------------------------------------------------------------*/
static bool
copy_updated_inherit (inherit_t *oldinheritp, inherit_t *newinheritp, program_t *from, int first_function_index, int first_variable_index, int last_variable_index, funflag_t varmodifier)

/* We want to copy the variables for <newinheritp>, but we already
 * got them in <oldinheritp>, and that one is an obsolete program
 * (INHERIT_TYPE_MAPPED). Copy the information from there and update
 * variable modifiers from <from> (variables from <first_variable_index>
 * to <last_variable_index>) and <varmodifier>.
 *
 * Returns true on success, false otherwise (out of memory).
 */

#line 15391 "prolang.y"
{
    /* We need to duplate the update inherit entry. */
    inherit_t inheritupdate = INHERIT(oldinheritp->updated_inherit);

    newinheritp->inherit_type |= INHERIT_TYPE_DUPLICATE|INHERIT_TYPE_MAPPED;
    newinheritp->variable_index_offset = oldinheritp->variable_index_offset;
    newinheritp->num_additional_variables = oldinheritp->num_additional_variables;
    newinheritp->variable_map_offset = oldinheritp->variable_map_offset;
    newinheritp->function_map_offset = oldinheritp->function_map_offset;
    newinheritp->updated_inherit = INHERIT_COUNT;

    /* Adjust modifier and identifier. */
    for (int j = first_variable_index; j < last_variable_index; j++)
#line 15404 "prolang.y"
    {
        inherit_t* inheritvar = oldinheritp;
        int varidx = j - first_variable_index;

        do
#line 15409 "prolang.y"
        {
            inherit_t* inheritnext = &INHERIT(inheritvar->updated_inherit);
            varidx = GET_BLOCK(A_UPDATE_INDEX_MAP)[inheritvar->variable_map_offset + varidx];

            if (varidx >= inheritnext->prog->num_variables - inheritnext->prog->num_virtual_variables)
#line 15414 "prolang.y"
            {
                /* Additional variable. */
                varidx -= inheritnext->prog->num_variables - inheritnext->prog->num_virtual_variables;
                break;
            }

            inheritvar = inheritnext;
        } while (inheritvar->inherit_type & INHERIT_TYPE_MAPPED);

        if (!inherit_variable(from->variables + j, varmodifier, inheritvar->variable_index_offset + varidx))
            return false;
    }

    inheritupdate.inherit_type |= INHERIT_TYPE_DUPLICATE;
    inheritupdate.inherit_depth = newinheritp->inherit_depth;

    /* Add the update function table. */
    inheritupdate.function_index_offset = FUNCTION_COUNT;
    if (!inherit_functions(inheritupdate.prog, INHERIT_COUNT))
        return false;

    mem_block[A_FUNCTIONS].current_size += sizeof(A_FUNCTIONS_t) * inheritupdate.prog->num_functions;

    update_duplicate_functions(from, first_function_index, oldinheritp, newinheritp, &inheritupdate, false);

    ADD_INHERIT(&inheritupdate);

    /* Resolve further inherit mappings. */
    while (inheritupdate.inherit_type & INHERIT_TYPE_MAPPED)
#line 15443 "prolang.y"
    {
        INHERIT(INHERIT_COUNT-1).updated_inherit = INHERIT_COUNT;

        inheritupdate = INHERIT(inheritupdate.updated_inherit);
        inheritupdate.inherit_type |= INHERIT_TYPE_DUPLICATE;
        inheritupdate.inherit_depth = newinheritp->inherit_depth;
        ADD_INHERIT(&inheritupdate);
    }

    return true;
} /* copy_updated_inherit() */

/*-------------------------------------------------------------------------*/
static bool
inherit_virtual_variables (inherit_t *newinheritp, program_t *from, int first_function_index, int first_variable_index, int last_variable_index, funflag_t varmodifier)

/* Copy the virtual variables from <from> into our program.
 * <newinheritp> is the inherit structure that is going to be inserted into
 * our programm and should already be initialized. <first_function_index>
 * is the index into <from>'s function of the first function of this inherit
 * (the original function_index_offset in <from>).
 * <first_variable_index> and <last_variable_index> are indices into <from>'s
 * variable corresponding to this virtual inherit (last_variable_index really
 * points one variable behind the last inherit's variable).
 *
 * Returns true on success, false otherwise (out of memory).
 */

#line 15471 "prolang.y"
{
    program_t* progp = newinheritp->prog;

    /* Do we already know this inherit? */
    inherit_t* first_inherit = GET_BLOCK(A_INHERITS);
    inherit_t* inheritdup = first_inherit;
    inherit_t* inheritorig = NULL;
    bool found = false;

    /* First look for the same program already inherited virtually. */
    for (int i = INHERIT_COUNT; --i >= 0; inheritdup++)
#line 15482 "prolang.y"
    {
        /* Non-Virtual? */
        if(inheritdup->variable_index_offset & NON_VIRTUAL_OFFSET_TAG)
            continue;

        /* Ignore inherits we already looked at. */
        if(inheritdup->inherit_type & INHERIT_TYPE_DUPLICATE)
            continue;

        if (inheritdup->prog != progp)
            continue;

        if(inheritdup->inherit_type & INHERIT_TYPE_MAPPED)
#line 15495 "prolang.y"
        {
            /* Found it, but is obsolete.
             * We use its function and variable map and additional variables.
             * And we copy its update inherit entry.
             */
            if (!copy_updated_inherit(inheritdup, newinheritp, from, first_function_index, first_variable_index, last_variable_index, varmodifier))
                return false;
        }
        else
#line 15504 "prolang.y"
        {
            /* Found it, use their variables. */
            newinheritp->variable_index_offset = inheritdup->variable_index_offset;
            newinheritp->inherit_type |= INHERIT_TYPE_DUPLICATE;

            /* Adjust modifier and identifier. */
            for (int j = first_variable_index; j < last_variable_index; j++)
                if (!inherit_variable(from->variables + j, varmodifier, inheritdup->variable_index_offset + j - first_variable_index))
                    return false;
        }

        found = true;
        break;
    }

    /* Then we search for a program with the same name (also virtually inherited). */
    inheritdup = first_inherit;
    for (int i = INHERIT_COUNT; !found && --i >= 0; inheritdup++)
#line 15522 "prolang.y"
    {
        /* Non-Virtual? */
        if(inheritdup->variable_index_offset & NON_VIRTUAL_OFFSET_TAG)
            continue;

        /* We are only interested in the newest one. */
        if(inheritdup->inherit_type & INHERIT_TYPE_MAPPED)
            continue;

        if (!mstreq(inheritdup->prog->name, progp->name))
            continue;

        /* First determine the number of variables. */
        program_t *inhprog = inheritdup->prog;
        int first_inh_variable = inheritdup->variable_index_offset;
        int last_inh_variable = inheritdup->variable_index_offset + inhprog->num_variables - inhprog->num_virtual_variables;

        assert(last_variable_index == first_variable_index + progp->num_variables - progp->num_virtual_variables);

        if ( (inhprog->load_time == progp->load_time)
           ? (inhprog->id_number > progp->id_number)
           : (inhprog->load_time > progp->load_time) )
#line 15544 "prolang.y"
        {
            /* Phew, the already inherited program is newer.
             * We'll use their variables but also create
             * a mapping from old to new variable indices.
             */
            if (inheritdup->inherit_type & INHERIT_TYPE_DUPLICATE)
                continue; /* The original will come later. */

            update_virtual_program(from
                                  , newinheritp
                                  , inheritdup
                                  , last_variable_index - first_variable_index
                                  , last_inh_variable - first_inh_variable
                                  , first_function_index
                                  , first_variable_index
                                  , false
                                  , varmodifier
                                  );

            found = true;
        }
        else if (!(inheritdup->inherit_type & INHERIT_TYPE_DUPLICATE))
#line 15566 "prolang.y"
        {
            /* Damn, we've inherited an old program.
             * We'll have to fix that one now.
             */
            update_virtual_program(from
                                  , inheritdup
                                  , newinheritp
                                  , last_inh_variable - first_inh_variable
                                  , last_variable_index - first_variable_index
                                  , first_function_index
                                  , first_variable_index
                                  , true
                                  , varmodifier
                                  );

            /* Remember this, in case we meet some duplicates. */
            inheritorig = inheritdup;

            /* Variables still need to be inherited. */
            found = false;
        }
        else
#line 15588 "prolang.y"
        {
            /* Okay, this is the duplicate of an old program.
             * So we must have seen the non-duplicate version.
             * Use that mapping.
             */
            assert(inheritorig != NULL);
            assert(inheritorig->inherit_type & INHERIT_TYPE_MAPPED);

            inheritdup->inherit_type |= INHERIT_TYPE_MAPPED;
            inheritdup->variable_index_offset = inheritorig->variable_index_offset;
            inheritdup->num_additional_variables = inheritorig->num_additional_variables;
            inheritdup->variable_map_offset = inheritorig->variable_map_offset;
            inheritdup->function_map_offset = inheritorig->function_map_offset;
            inheritdup->updated_inherit = INHERIT_COUNT;

            /* We need to duplicate the update inherit as well. */
            inherit_t inheritupdate = INHERIT(inheritorig->updated_inherit);
            inheritupdate.inherit_type |= INHERIT_TYPE_DUPLICATE;
            inheritupdate.inherit_depth = inheritdup->inherit_depth;

            /* And now we need to cross-define its functions. */
            inheritupdate.function_index_offset = FUNCTION_COUNT;
            if (!inherit_functions(inheritupdate.prog, INHERIT_COUNT))
                return false;

            mem_block[A_FUNCTIONS].current_size += sizeof(A_FUNCTIONS_t) * inheritupdate.prog->num_functions;

            update_duplicate_functions(from, first_function_index, inheritorig, inheritdup, &inheritupdate, true);

            ADD_INHERIT(&inheritupdate);

            /* The update inherit cannot already be obsoleted, too. */
            assert(!(inheritupdate.inherit_type & INHERIT_TYPE_MAPPED));

            /* Continue to other duplicates. */
            found = false;
        }
    }

    if (!found)
#line 15628 "prolang.y"
    {
        assert(last_variable_index <= first_variable_index + progp->num_variables);

        /* First occurence of these virtual variables, we're
         * going to copy them into our variables.
         */
        for (int i = first_variable_index; i < last_variable_index; i++)
            if (!inherit_variable(from->variables + i, varmodifier, -1))
                return false;
    }

    return true;

} /* inherit_virtual_variables() */


/*-------------------------------------------------------------------------*/
static int
inherit_obsoleted_variables  (inherit_t *newinheritp, program_t *from, int first_variable_index, funflag_t varmodifier)

/* Copy the virtual variables from <from> into our program.
 * This is like inherit_virtual_variables() but just for inherited programs
 * that are obsoleted by other programs. We don't calculate mappings
 * between old and new program, but adapt the mappings from <from>.
 *
 * This is needed, because obsoleted inherited programs may still have
 * variables (ones the old program had, but not the new program)
 * and may still be called, so we need to find it and its mapping.
 *
 * Returns true on success, false otherwise (out of memory).
 */
#line 15659 "prolang.y"
{
    /* Copy the additional variables. */
    for (int i = first_variable_index, j = 0; j < newinheritp->num_additional_variables; i++, j++)
        if (!inherit_variable(from->variables + i, varmodifier, -1))
            return false;


    return true;
} /* inherit_obsoleted_variables() */


/*-------------------------------------------------------------------------*/
static int
inherit_program (program_t *from, funflag_t funmodifier, funflag_t varmodifier)

/* Copies struct definitions, functions and variables from the program <from>
 * into our program. Functions are copied with visibility <funmodifier>,
 * variables with visibility <varmodifier>.
 *
 * We do this by iterating through <from>'s inherit list and copying the
 * function definitions and variables for each inherit into our program.
 * It is important that the order of function and variables will be preserved.
 *
 * Special care must be taken for virtual inherits in <from>.
 * 1. If this is the first occurrence of an inherit with this name,
 *    its variables get another variable index offset,
 *    because virtual variables are stored at the beginning of
 *    the variable block. This is done by introducing a new
 *    inherit entry (INHERIT_TYPE_EXTRA) that is referenced
 *    by the functions of this inherit.
 * 2. If this inherit is already known don't copy the variables
 *    but adopt the variable index offset of the earlier occurrence.
 *    (Still add a INHERIT_TYPE_EXTRA entry. It'll get the
 *    INHERIT_TYPE_DUPLICATE flag.)
 * 3. If there was already a different inherited program with
 *    the same, check which of them is newer.
 *    a) If the earlier inherit is newer, treat it like case (2),
 *       but add a variable index mapping to the INHERIT_TYPE_EXTRA
 *       structure. Set the INHERIT_TYPE_MAPPED flag. All vanished
 *       variables will be reserved and added to the and of the
 *       variable block.
 *       Also copy the functions again, this time from the newer
 *       program, and cross-define the function entries from the
 *       older program to the newer (unless they we're already
 *       overridden or cross-defined). Make vanished functions
 *       undefined.
 *    b) If the new inherit is newer, do (a) the other way around:
 *       Add a variable index mapping to its inherit entry.
 *       Replace the already copied variables from the old
 *       program with ones from the newer program. (We might move
 *       later inherited virtual variables a little bit.)
 *       And copy and cross-define functions from the newer program.
 *
 * The result is the function index of the inherited __INIT function,
 * or -1 if the inherited program doesn't have an initializer.
 */

#line 15716 "prolang.y"
{
    int initializer = -1;                      /* Function index of __INIT */
    function_t *fun_p;                         /* Function pointer as we move
                                                * through the list. */
    uint32 first_func_index = FUNCTION_COUNT;  /* Index of the first inherited
                                                * function. */
    unsigned short* new_inherit_indices;       /* For each inherit entry in
                                                * <from> remember the index
                                                * in the current program.
                                                */

   /*                                *
    *   Preparations for functions   *
    *                                */

    /* We reserve space for update inherit entries,
     * so there wouldn't happen any reallocation and thus
     * no moving of our inherit entries later on.
     */
    if (!RESERVE_INHERITS(INHERIT_COUNT + 2*from->num_inherited + 1))
        return -1;

    /* For now, we mask out the INHERIT field in the flags and
     * use NEW_INHERITED_INDEX for the value.
     *
     * We'll do cross-definitions and visibility later.
     * For now we just collect the function information.
     * We'll need to do it here at the beginning, so we can easily
     * detect visible definitions (by looping over the function names).
     */
    if (!inherit_functions(from, NEW_INHERITED_INDEX))
        return -1;

    /* Point back to the begin of the copied function data */
    fun_p = FUNCTION(first_func_index);

    /* Unhide all function for which names exist */
#line 15753 "prolang.y"
    {
        unsigned short *ixp = from->function_names;
        for (int i = from->num_function_names; --i >= 0; )
#line 15756 "prolang.y"
        {
            fun_p[*ixp++].flags &= ~NAME_HIDDEN;
        }
    }

    mem_block[A_FUNCTIONS].current_size += sizeof *fun_p * from->num_functions;


    /* Shouldn't have VAR_INITILIALIZED set, but you never know... */
    varmodifier &= ~VAR_INITIALIZED;


   /*                                  *
    *   Processing <from>'s inherits   *
    *                                  */

   /* Remember that <from>'s layout looks something like this:
    *
    * Inherit list:                V1    V2    I1  V3  V1 I2
    * Function table:          (H1 V1 H2 V2 H3 I1) V3 (V1 I2) <from>
    * Variable list:  V1 V2 V3 (H1    H2    H3 I1)        I2  <from>
    *
    * Where V1, V2, V3 are virtual inherits either directly or
    * indirectly from the non-virtual inherit I1,and H1 - H3
    * are indirectly by I1 inherited non-virtual programs.
    * The paranthesis denote the area that is indicated by I1's
    * inherit entry.
    *
    * There are two things to pay attention to:
    * 1) For functions before the functions denoted by an
    *    INHERIT_TYPE_EXTRA inherit (function_index_offset),
    *    there may already be (non-virtual) functions from
    *    other not-mentioned inherits.
    * 2) Virtual variables are moved to the beginning of the
    *    variable block (see above). Because of that
    *    variable_index_offset points to the begin of
    *    its block of non-virtual variables.
    *    (So variable_index_offset + progp->num_variables
    *    - progp->num_virtual_variables points to the
    *    end of the variable block.)
    */

    /* Virtual variables need special treatment,
     * as we have to copy the corresponding inherit_t
     * entry from <from> into our own program and
     * sort duplicates out. Therefore first we're
     * going through the the inherit list and
     * handle virtual variables only. At the end all
     * remaining variables are non-virtual ones and
     * will be copied into our own as well.
     *
     * Functions will be handled similarly. First we'll
     * look at all (already) virtual functions and then
     * at the rest (the remaining functions will still have
     * NEW_INHERITED_INDEX as the inherit index). In this
     * phase we just set the inherit index for each function.
     * Cross-definitions will be done later in one big loop.
     */

    /* Remember the last handled variable.
     * This is the index into <from>'s variables.
     */
    int last_bound_variable = 0;

    int inheritnum = from->num_inherited; /* Number of inherits.          */
    new_inherit_indices = xalloc(sizeof(*new_inherit_indices) * inheritnum);

    for (int inheritidx = 0; inheritidx < inheritnum; inheritidx++)
#line 15824 "prolang.y"
    {
        inherit_t* inheritp = from->inherit + inheritidx;
        program_t* progp    = inheritp->prog;

        /* We'll handle non-virtual variables and functions at the end. */
        if (inheritp->inherit_type == INHERIT_TYPE_NORMAL)
            continue;

        /* Create a new inherit entry. */
        inherit_t newinherit;
        newinherit = *inheritp;
        newinherit.inherit_type = INHERIT_TYPE_EXTRA | (inheritp->inherit_type & INHERIT_TYPE_DUPLICATE);
        newinherit.inherit_depth++;
        newinherit.function_index_offset += first_func_index;
        newinherit.variable_index_offset += V_VARIABLE_COUNT - last_bound_variable;

        assert((inheritp->inherit_type & INHERIT_TYPE_DUPLICATE) || (last_bound_variable == inheritp->variable_index_offset));

        if (inheritp->inherit_type & INHERIT_TYPE_MAPPED)
#line 15843 "prolang.y"
        {
            if (!(inheritp->inherit_type & INHERIT_TYPE_DUPLICATE))
                inherit_obsoleted_variables(&newinherit, from, last_bound_variable, varmodifier);
            newinherit.inherit_type |= INHERIT_TYPE_MAPPED;

            if (!(inheritp->inherit_type & INHERIT_TYPE_DUPLICATE))
                last_bound_variable = inheritp->variable_index_offset + inheritp->num_additional_variables;
        }
        else
#line 15852 "prolang.y"
        {
            int next_bound_variable = inheritp->variable_index_offset + progp->num_variables - progp->num_virtual_variables;

            inherit_virtual_variables(&newinherit, from,
                newinherit.function_index_offset - first_func_index,
                inheritp->variable_index_offset, next_bound_variable, varmodifier);

            if (!(inheritp->inherit_type & INHERIT_TYPE_DUPLICATE))
                last_bound_variable = next_bound_variable;

            /* Now adjust the function inherit index.
             * If the function isn't cross-defined or overridden in <from>
             * (i.e. it really is a call into the virtual inherit),
             * then call it through <newinherit> now.
             *
             * inherit_virtual_variables may have done some cross-definitions.
             * And not all function within inheritp's range may have its
             * inherit index, because an intermediate inherit might have
             * overridden some of them, then they'll get that inherit's index.
             */
            fun_p = FUNCTION(newinherit.function_index_offset);
            funflag_t* flag_p = from->functions + inheritp->function_index_offset;
            int newinheritidx = INHERIT_COUNT;
            for (int i = progp->num_functions; --i >= 0; fun_p++, flag_p++)
#line 15876 "prolang.y"
            {
                if ((*flag_p & (NAME_INHERITED|NAME_CROSS_DEFINED)) == NAME_INHERITED
                  && (fun_p->flags & (NAME_INHERITED|NAME_CROSS_DEFINED)) == NAME_INHERITED
                  && (*flag_p & INHERIT_MASK) == inheritidx
                   )
#line 15881 "prolang.y"
                {
                    fun_p->offset.inherit = newinheritidx;
                }
            }
        }

        new_inherit_indices[inheritidx] = INHERIT_COUNT;
        ADD_INHERIT(&newinherit);
    }

    /* So, now walk over them again to copy variable and function mappings
     * for obsoleted programs (that were already obsoleted in <from>).
     */
    for (int inheritidx = 0; inheritidx < inheritnum; inheritidx++)
#line 15895 "prolang.y"
    {
        inherit_t *from_old_inheritp = from->inherit + inheritidx;
        inherit_t *cur_old_inheritp;
        int num_vars, num_funs;

        if (!(from_old_inheritp->inherit_type & INHERIT_TYPE_MAPPED))
            continue;

        cur_old_inheritp = &INHERIT(new_inherit_indices[inheritidx]);

        /* Our obsoleted inherit entry shouldn't have an updated_inherit entry, yet. */
        cur_old_inheritp->updated_inherit = new_inherit_indices[from_old_inheritp->updated_inherit];

        /* We can copy the mapping as it is, because the indices
         * are always relative to the inherited program.
         */
        num_vars = cur_old_inheritp->prog->num_variables - cur_old_inheritp->prog->num_virtual_variables;
        num_funs = cur_old_inheritp->prog->num_functions;

        cur_old_inheritp->variable_map_offset = UPDATE_INDEX_MAP_COUNT;
        cur_old_inheritp->function_map_offset = UPDATE_INDEX_MAP_COUNT + num_vars;

        if (!RESERVE_UPDATE_INDEX_MAP(num_vars + num_funs))
            break;

        memcpy(GET_BLOCK(A_UPDATE_INDEX_MAP) + cur_old_inheritp->variable_map_offset,
               from->update_index_map + from_old_inheritp->variable_map_offset,
               (num_vars + num_funs) * sizeof(A_UPDATE_INDEX_MAP_t));

        mem_block[A_UPDATE_INDEX_MAP].current_size += (num_vars + num_funs) * sizeof(A_UPDATE_INDEX_MAP_t);
    }

    xfree(new_inherit_indices);

    /* And now to something completely different, <from> itself. */

    inherit_t frominherit;
    frominherit.prog = from;
    frominherit.function_index_offset = first_func_index;
    frominherit.inherit_depth = 1;

    /* We're done with the virtual extra inherits,
     * copy the remaining variables (variable indices from
     * last_bound_variables to from->num_variables).
     */
    assert(last_bound_variable == from->num_virtual_variables);
    if (varmodifier & TYPE_MOD_VIRTUAL)
#line 15942 "prolang.y"
    {
        /* And they're gonna be virtual, too...
         */
        frominherit.inherit_type = INHERIT_TYPE_VIRTUAL;
        frominherit.variable_index_offset = V_VARIABLE_COUNT;

        inherit_virtual_variables(&frominherit, from, 0, last_bound_variable,
            from->num_variables, varmodifier);
    }
    else
#line 15952 "prolang.y"
    {
        frominherit.inherit_type = INHERIT_TYPE_NORMAL;
        frominherit.variable_index_offset = NV_VARIABLE_COUNT | NON_VIRTUAL_OFFSET_TAG;

        for (int i = last_bound_variable; i < from->num_variables; i++)
            if (!inherit_variable(from->variables + i, varmodifier, -1))
                break;
    }

    /* Hey, we're done with the variables, now to the functions.
     * Set the inherit index for all functions that have none, yet.
     */
    fun_p = FUNCTION(frominherit.function_index_offset);
    int frominheritidx = INHERIT_COUNT;
    for (int i = from->num_functions; --i >= 0; fun_p++)
#line 15967 "prolang.y"
    {
        if (fun_p->offset.inherit == NEW_INHERITED_INDEX)
            fun_p->offset.inherit = frominheritidx;
    }

    ADD_INHERIT(&frominherit);

    /* And finally, let's do cross-definitions and apply the modifiers.
     *
     * Loop again over the inherited functions, checking visibility
     * and re/crossdefinition, and updating their function indices.
     * Do not call define_new_function() from here, as duplicates would
     * be removed.
     */
    fun_p = FUNCTION(first_func_index);
    for (int newix = 0; newix < from->num_functions; newix++)
#line 15983 "prolang.y"
    {
        int i = newix;                                 /* Index relative to the inherit
                                                        * Same as index into fun_p.
                                                        */
        int current_func_index = first_func_index + i; /* Index into the current prog. */

        program_t *funprogp = from;
        int funprogidx = i;                            /* Index into <funprog>. */

        function_t fun = fun_p[i];
        if (fun.flags & NAME_CROSS_DEFINED)
#line 15994 "prolang.y"
        {
            /* We'll check whether this is a cross-definition to an
             * updated virtual program. Then we need to handle the
             * name there.
             */
            int32 offset = GET_CROSSDEF_OFFSET(fun.offset.func);
            if (i + offset >= from->num_functions)
#line 16001 "prolang.y"
            {
                i += offset;
                current_func_index += offset;
                fun = fun_p[i];

                /* This should be a virtual inherited function. */
                assert(fun.flags & TYPE_MOD_VIRTUAL);
                funprogp = INHERIT(fun.offset.inherit).prog;
                funprogidx = i + first_func_index - INHERIT(fun.offset.inherit).function_index_offset;
            }
        }

        /* Apply nomask now, visibility later when we know
         * that this the dominant definition.
         */
        fun.flags |= funmodifier & TYPE_MOD_NO_MASK;

        /* Perform a lot of tests and actions for the visibility
         * and definitiability. The do-while(false) allows us to abort
         * easily without using gotos.
         */
        do
#line 16023 "prolang.y"
        {
            /* Ignore cross defines.
             * They are the only complete invisible entries.
             */
            if (fun.flags & NAME_CROSS_DEFINED)
                break;

            /* Visible: create a new identifier for it */
            ident_t* p = make_global_identifier(get_txt(fun.name), I_TYPE_GLOBAL);
            if (!p)
                break;

            if (p->type != I_TYPE_UNKNOWN)
#line 16036 "prolang.y"
            {
                /* We got this ident already somewhere */

                int32 n; /* existing function index */

                n = p->u.global.function;

                /* If the identifier is (also) an lfun, handle it, even if
                 * it's overloaded by something else as well. If we didn't
                 * subsequent inheritors would receive illegal function
                 * start offsets.
                 */
                if ( n >= 0 && n != first_func_index + i)
#line 16049 "prolang.y"
                {
                    /* Already inherited from somewhere else.
                     * Don't try to resolve cross-references inside the
                     * currently inherited program; not only is this superflous,
                     * but it can also lead to circular cross-inheritance
                     * when there was a misplaced prototype or an explicit
                     * directive to inherit a multiply inherited function
                     * from a particular base class (the latter is not
                     * implemented). In these cases, the information that lead
                     * to the non-standard preference would be very hard to
                     * reconstruct.
                     */
                    if ((uint32)n < first_func_index || (uint32)n >= first_func_index + from->num_functions)
#line 16062 "prolang.y"
                    {
                        /* We already have a function definition/prototype
                         * for this name.
                         */

                        function_t *OldFunction = FUNCTION(n);

                        if ( !(OldFunction->flags & NAME_INHERITED) )
#line 16070 "prolang.y"
                        {
                            /* Since inherits are not possible after
                             * functions have been compiled, the only
                             * way to get here is when we had a prototype
                             * for the function.
                             * It's not fatal, but annoying.
                             */
                            yywarnf(
                                "Misplaced prototype for %s in %s ignored.\n"
                                , get_txt(fun.name), current_loc.file->name
                            );
                            cross_define( &fun, OldFunction
                                        , current_func_index - n );
                            p->u.global.function = current_func_index;
                        }
                        else if ( (fun.flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN))
                                    == (TYPE_MOD_PRIVATE|NAME_HIDDEN) )
#line 16087 "prolang.y"
                        {
                            /* There is already one function with this
                            * name. Ignore the private one, as we
                            * only need it for useful error messages.
                            */

                            break;
                        }
                        else if ( (OldFunction->flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN))
                                     == (TYPE_MOD_PRIVATE|NAME_HIDDEN) )
#line 16097 "prolang.y"
                        {
                            /* The old one was invisible, ignore it
                             * and take this one.
                             */

                            p->u.global.function = current_func_index;
                        }
                        else if ((fun.flags | funmodifier) & TYPE_MOD_VIRTUAL
                              && OldFunction->flags & TYPE_MOD_VIRTUAL
                          &&    get_virtual_function_id(funprogp, funprogidx)
  == get_virtual_function_id(INHERIT(OldFunction->offset.inherit).prog
                , n - INHERIT(OldFunction->offset.inherit).function_index_offset
                     )
                                 )
#line 16111 "prolang.y"
                        {
                            /* Entries denote the same function and both
                             * entries are visible. We have to use
                             * cross_define nonetheless, to get consistant
                             * redefinition (and to avoid the nomask
                             * checking that comes next), and we prefer
                             * the first one.
                             *
                             * It is important, that both entries are
                             * indeed visible, because otherwise invisible
                             * (i.e. private) functions would be made
                             * visible again by another visible occurrence
                             * of the same function. The originally invisible
                             * occurrence would then be subject to
                             * redefinition and nomask checking.
                             */
                            OldFunction->flags |= fun.flags &
                                (TYPE_MOD_PUBLIC|TYPE_MOD_NO_MASK);
                            OldFunction->flags &= fun.flags |
                                ~(TYPE_MOD_STATIC|TYPE_MOD_PRIVATE|TYPE_MOD_PROTECTED|NAME_HIDDEN);
                            cross_define( OldFunction, &fun
                                        , n - current_func_index );
                        }
                        else if ( (fun.flags & OldFunction->flags & TYPE_MOD_NO_MASK)
                             &&  !( (fun.flags|OldFunction->flags) & NAME_UNDEFINED ) )
#line 16136 "prolang.y"
                        {
                            yyerrorf(
                              "Illegal to inherit 'nomask' function '%s' twice",
                              get_txt(fun.name));
                        }
                        else if ((   fun.flags & TYPE_MOD_NO_MASK
                                  || OldFunction->flags & NAME_UNDEFINED )
                              && !(fun.flags & NAME_UNDEFINED)
                                )
#line 16145 "prolang.y"
                        {
                            /* This function is visible and existing, but the
                             * inherited one is not, or this one is also nomask:
                             * prefer this one one.
                             */
                            cross_define( &fun, OldFunction
                                        , current_func_index - n );
                            p->u.global.function = current_func_index;
                        }
                        else
#line 16155 "prolang.y"
                        {
                            /* At least one of the functions is visible
                             * or redefinable: prefer the first one.
                             */

                            cross_define( OldFunction, &fun
                                        , n - current_func_index );
                        }
                    } /* if (n < first_func_index) */
                    else if ( (fun.flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN))
                                != (TYPE_MOD_PRIVATE|NAME_HIDDEN) )
#line 16166 "prolang.y"
                    {
                        /* This is the dominant definition in the superclass,
                         * inherit this one.
                         */
#ifdef DEBUG
                        /* The definition we picked before can't be
                         * cross-defined, because cross-defines won't
                         * be registered as global identifiers.
                         * So the previous definition should be
                         * nominally invisible so we can redefine it.
                         */
                        if ( (FUNCTION(n)->flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN))
                                != (TYPE_MOD_PRIVATE|NAME_HIDDEN) )
#line 16179 "prolang.y"
                        {
                            fatal(
                              "Inconsistent definition of %s() within "
                              "superclass '%s'.\n"
                            , get_txt(fun.name), get_txt(from->name)
                            );
                        }
#endif
                        p->u.global.function = current_func_index;
                    }
                }

                /* Handle the non-lfun aspects of the identifier */
#line 16192 "prolang.y"
                {
                    if (n != I_GLOBAL_FUNCTION_OTHER
                     || (p->u.global.efun < 0 && p->u.global.sim_efun < 0
#ifdef USE_PYTHON
                      && !is_python_efun(p)
#endif
                        )
                     || (fun.flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN)) == 0
                       )
#line 16201 "prolang.y"
                     {
                        /* This is not an inherited private function shadowing
                         * a (simul-)efun.
                         */

                        if (p->u.global.efun >= 0 || p->u.global.sim_efun >= 0
#ifdef USE_PYTHON
                         || is_python_efun(p)
#endif
                           )
#line 16211 "prolang.y"
                        {
                            /* This inherited function shadows an efun */

                            efun_shadow_t *q;

                            q = xalloc(sizeof(efun_shadow_t));
                            if (!q) {
                                yyerrorf("Out of memory: efun shadow (%zu bytes)"
                                        , sizeof(efun_shadow_t));
                                break;
                            }
                            q->shadow = p;
                            q->next = all_efun_shadows;
                            all_efun_shadows = q;
                        }

                        /* Update the symbol table entry to point
                         * to the newly read function, unless of course
                         * the code above already took care of that change.
                         */
                        if (p->u.global.function < 0)
                            p->u.global.function = current_func_index;
                    }
                    /* else: inherited private defined function must not hide
                     * the (simul-)efun and is thusly not added to
                     * the symbol-table.
                     */
                }
            } /* if (p != I_TYPE_UNKNOWN) */

            if (p->type == I_TYPE_UNKNOWN)
#line 16242 "prolang.y"
            {
                /* First time this function-ident was ever encountered.
                 * Just make a new global.
                 */

                init_global_identifier(p, /* bVariable: */ MY_TRUE);
                p->u.global.function  = current_func_index;
                p->next_all = all_globals;
                all_globals = p;
            }

            /* Done with re/crossdefinition, now handle visibility.
             * Especially: public functions should not become private
             * when inherited 'private'.
             */
            funflag_t new_type = funmodifier;
            if (fun.flags & TYPE_MOD_PUBLIC)
                new_type &= ~(TYPE_MOD_PRIVATE | TYPE_MOD_PROTECTED | TYPE_MOD_STATIC);

            fun.flags |= new_type;

            /* The most restrictive visibility wins. */
            if (fun.flags & (TYPE_MOD_PRIVATE | NAME_HIDDEN))
                fun.flags &= ~(TYPE_MOD_PROTECTED | TYPE_MOD_STATIC | TYPE_MOD_PUBLIC);
            else if (fun.flags & TYPE_MOD_PROTECTED)
                fun.flags &= ~(TYPE_MOD_STATIC | TYPE_MOD_PUBLIC);
            else if (fun.flags & TYPE_MOD_STATIC)
                fun.flags &= ~(TYPE_MOD_PUBLIC);

            /* Recognize an inherited heart_beat(), making it possible
             * to mask it.
             */
            if ((heart_beat == -1)
             && mstreq(fun.name, STR_HEART_BEAT))
#line 16276 "prolang.y"
            {
                heart_beat = current_func_index;
            }

            /* Recognize the initializer function */
            if (mstreq(fun.name, STR_VARINIT)
             && (fun.flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN)) != (TYPE_MOD_PRIVATE|NAME_HIDDEN))
#line 16283 "prolang.y"
            {
                initializer = i;
                fun.flags |= NAME_UNDEFINED;
            }
        } while(false); /* do loop for visibility/redefinability */


        /* Finally update the entry in the A_FUNCTIONS area */
        fun_p[i] = fun;
    } /* for (inherited functions), pass 2 */

    copy_structs(from, funmodifier & ~(TYPE_MOD_STATIC|TYPE_MOD_VIRTUAL));

    return initializer;

} /* inherit_program() */

/*-------------------------------------------------------------------------*/
static void
fix_variable_index_offsets (program_t *new_prog)

/* Add num_virtual_variables to the index_offset of all variables
 * in <new_prog> marked with NON_VIRTUAL_OFFSET_TAG. The tag is removed.
 *
 * Reason is that the non-virtual variables have to be put after
 * the virtual variables, so the offsets of these variables are
 * first counted from 0 up and then corrected in this function after
 * the last virtual inherit.
 */

#line 16313 "prolang.y"
{
    int i;
    inherit_t *inheritp;

    i = new_prog->num_inherited;
    for (inheritp = new_prog->inherit; --i >= 0; inheritp++)
#line 16319 "prolang.y"
    {
        if (inheritp->variable_index_offset & NON_VIRTUAL_OFFSET_TAG)
            inheritp->variable_index_offset &= ~NON_VIRTUAL_OFFSET_TAG;
    }
} /* fix_variable_index_offsets() */

/*-------------------------------------------------------------------------*/
void
store_line_number_info (void)

#line 16329 "prolang.y"
{
    unsigned char c;
    short offset;

    /* Was code generated since the last call?
     * If not, return.
     */
    offset = mem_block[A_PROGRAM].current_size - stored_bytes;
    if (offset <= 0)
        return;
    stored_bytes = mem_block[A_PROGRAM].current_size;

    /* Less than 8 bytes code in 2..9 lines */
    if (offset <= 8
     && current_loc.line - stored_lines >= 2 && current_loc.line - stored_lines <= 9)
#line 16344 "prolang.y"
    {
        c = offset + 8*(current_loc.line - stored_lines) + 47;
          /* == (lineincr+6) << 3 | (codesize-1) */
        byte_to_mem_block(A_LINENUMBERS, c);
        stored_lines = current_loc.line;
        return;
    }

    /* Use up the excessive amounts of lines */
    stored_lines++;

    while (stored_lines > current_loc.line)
#line 16356 "prolang.y"
    {
        int lines;

        lines = stored_lines - current_loc.line;
        if (lines > 256)
            lines = 256;
        stored_lines -= lines;
        byte_to_mem_block(A_LINENUMBERS, LI_BACK);
        byte_to_mem_block(A_LINENUMBERS, lines-1);
    }
    
    while (stored_lines < current_loc.line)
#line 16368 "prolang.y"
    {
        int lines;

        lines = current_loc.line - stored_lines;
        if (lines > LI_MAXEMPTY)
            lines = LI_MAXEMPTY;
        stored_lines += lines;
        c = 256 - lines;
        byte_to_mem_block(A_LINENUMBERS, c);
    }

    while (offset >= LI_MAXOFFSET)
#line 16380 "prolang.y"
    {
        byte_to_mem_block(A_LINENUMBERS, LI_MAXOFFSET);
        offset -= LI_MAXOFFSET;
    }
    byte_to_mem_block(A_LINENUMBERS, offset);
} /* store_line_number_info() */

/*-------------------------------------------------------------------------*/
static void
store_line_number_relocation (int relocated_from)

/* Since the last store_line_number_info(), the compiler added a code
 * block which was compiled out of order at the earlier line <relocated_from>.
 * Add the relocation marker with the offset to <relocated_from>, call
 * store_line_number_info() for the modified linenumbers and the added
 * codeblock, then restore the current line number.
 */

#line 16398 "prolang.y"
{
    int save_current, offset;

    save_current = current_loc.line;
    stored_lines -= 2;
    current_loc.line = stored_lines+1;
    offset = current_loc.line - relocated_from;
    if (offset >= LI_SMALL_REL)
#line 16406 "prolang.y"
    {
        byte_to_mem_block(A_LINENUMBERS, LI_L_RELOCATED);
        byte_to_mem_block(A_LINENUMBERS, offset >> 8);
        byte_to_mem_block(A_LINENUMBERS, offset);
        /* trailing LI_L_RELOCATED allows bidirectional traversal */
        byte_to_mem_block(A_LINENUMBERS, LI_L_RELOCATED);
    }
    else
#line 16414 "prolang.y"
    {
        byte_to_mem_block(A_LINENUMBERS, LI_RELOCATED + offset);
    }
    store_line_number_info();
    current_loc.line = save_current;
} /* store_line_number_relocation() */

/*-------------------------------------------------------------------------*/
void
store_line_number_backward (int offset)

/* The current line counter is set back by <offset> lines.
 * Adapted the stored_lines counter and add the LI_BACK linenumber entry.
 */

#line 16429 "prolang.y"
{
    if (offset > 0)
#line 16431 "prolang.y"
    {
        store_line_number_info();
        stored_lines -= offset;
        while (offset > 256)
#line 16435 "prolang.y"
        {
            byte_to_mem_block(A_LINENUMBERS, LI_BACK);
            byte_to_mem_block(A_LINENUMBERS, 255);
            offset -= 256;
        }
        byte_to_mem_block(A_LINENUMBERS, LI_BACK);
        byte_to_mem_block(A_LINENUMBERS, offset-1);
    }
} /* store_line_number_backward() */

/*-------------------------------------------------------------------------*/
mp_uint
store_include_info (char *name, char * filename, char delim, int depth)

/* The lexer is going to include <name>, which can be the filename given
 * in an #include directive, or a descriptive name for a different source.
 * The full (file)name of the source as seen by the lexer is <filename>.
 * This will be include depth <depth>.
 * <delim> is either '"' or '>' if this include is from a file, or ')'
 * if it's a different source.
 *
 * Result is the offset of the include information in the mem_block.
 * It is to be considered a handle and has to be passed to
 * store_include_end().
 */

#line 16461 "prolang.y"
{
    mp_uint rc;

    /* Generate and store the plain include information */
#line 16465 "prolang.y"
    {
        include_t inc;
        char * tmp;
        size_t len;

        /* Make sure that the filename starts with a leading slash,
         * then make it a tabled string and store it.
         */
        if (*filename != '/')
#line 16474 "prolang.y"
        {
            tmp = alloca(strlen(filename)+2);
            if (tmp == NULL)
#line 16477 "prolang.y"
            {
                yyerror("Out of stack memory: copy of filename");
            }
            else
#line 16481 "prolang.y"
            {
                *tmp = '/';
                strcpy(tmp+1, filename);
                filename = tmp;
            }
        }

        inc.filename = new_tabled(filename);
        if (inc.filename == NULL)
#line 16490 "prolang.y"
        {
            inc.filename = ref_mstring(STR_DEFAULT);
            yyerror("Out of memory: sharing include filename");
        }

        /* Surround the <name> with the delimiters, then
         * make it a tabled string and store it.
         */
        len = strlen(name);
        tmp = alloca(len+3);
        if (tmp == NULL)
#line 16501 "prolang.y"
        {
            yyerror("Out of stack memory: copy of name");
        }
        else
#line 16505 "prolang.y"
        {
            *tmp = delim == '"' ? delim
                                : (delim == '>' ? '<' : '(');
            strcpy(tmp+1, name);
            tmp[len+1] = delim;
            tmp[len+2] = '\0';

            inc.name = new_tabled(tmp);
            if (inc.name == NULL)
#line 16514 "prolang.y"
            {
                inc.name = ref_mstring(STR_DEFAULT);
                yyerror("Out of memory: sharing include name");
            }
        }

        /* Complete the structure and store it */
        inc.depth = depth;
        rc = INCLUDE_COUNT;
        ADD_INCLUDE(&inc);
    }

    /* Store the information for the linenumber tracing */

#line 16528 "prolang.y"
    {
        if (last_include_start == mem_block[A_LINENUMBERS].current_size)
#line 16530 "prolang.y"
        {
            simple_includes++;
        }
        else
#line 16534 "prolang.y"
        {
            simple_includes = 0;
        }

        stored_lines++;  /* don't count the #include line */

        /* Use up the amounts of lines collected */
        while (stored_lines < current_loc.line)
#line 16542 "prolang.y"
        {
            int lines;

            lines = current_loc.line - stored_lines;
            if (lines > LI_MAXEMPTY) lines = LI_MAXEMPTY;
            stored_lines += lines;
            byte_to_mem_block(A_LINENUMBERS, 256 - lines);
        }

        /* Store the bytecode and mark the position */
        byte_to_mem_block(A_LINENUMBERS, LI_INCLUDE);
        last_include_start = mem_block[A_LINENUMBERS].current_size;

        /* Restart linecount */
        stored_lines = 0;
    }

    return rc;
} /* store_include_info() */

/*-------------------------------------------------------------------------*/
void
store_include_end (mp_uint inc_offset, int include_line)

/* The current include ended. <inc_offset> has to be the offset returned by
 * store_include_info() for this include file, <include_line> is the
 * line number of the #include statement in the including file.
 */

#line 16571 "prolang.y"
{
    unsigned char c;

    stored_lines = include_line;
    if (last_include_start == mem_block[A_LINENUMBERS].current_size)
#line 16576 "prolang.y"
    {
        include_t * inc = &(INCLUDE(inc_offset));
        /* No code was generated in this include - remove the
         * line number information stored by store_include_info()
         * and tag the include information in A_INCLUDES.
         */

        last_include_start = mem_block[A_LINENUMBERS].current_size - 1;
        stored_lines--;
        while (last_include_start
            && (c = mem_block[A_LINENUMBERS].block[last_include_start - 1])
               >= 0x100 - LI_MAXEMPTY)
#line 16588 "prolang.y"
        {
            stored_lines += c - 0x100;
            last_include_start--;
        }

        mem_block[A_LINENUMBERS].current_size = last_include_start;
        if (--simple_includes < 0)
#line 16595 "prolang.y"
        {
            last_include_start--;
        }

        inc->depth = -inc->depth;
    }
    else
#line 16602 "prolang.y"
    {
        /* Store the include end and correct the linenumber */

        byte_to_mem_block(A_LINENUMBERS, LI_INCLUDE_END);
    }
} /* store_include_end() */

/*-------------------------------------------------------------------------*/
static void
prolog (const char * fname, Bool isMasterObj)

/* Initialize the compiler environment prior to a compile.
 * <fname> is the name of the top LPC file to be compiled.
 * <isMasterObj> is TRUE if this compile is part of the compilation of
 * the master object (in which case sefuns are disabled).
 */

#line 16619 "prolang.y"
{
    int i;
    ident_t *id;

    /* Initialize the memory for the argument types */
    if (type_of_arguments.block == NULL)
#line 16625 "prolang.y"
    {
        type_of_arguments.max_size = 100;
        type_of_arguments.block = xalloc(type_of_arguments.max_size);
    }
    type_of_arguments.current_size = 0;

    if (!_lpctypes_initialized)
#line 16632 "prolang.y"
    {
        make_static_type(get_array_type(lpctype_unknown),            &_lpctype_unknown_array);
        make_static_type(get_array_type(lpctype_mixed),              &_lpctype_any_array);
        make_static_type(get_union_type(lpctype_int, lpctype_float), &_lpctype_int_float);
        make_static_type(get_array_type(lpctype_int),                &_lpctype_int_array);
        make_static_type(get_array_type(lpctype_string),             &_lpctype_string_array);
        make_static_type(get_array_type(lpctype_object),             &_lpctype_object_array);

        _lpctypes_initialized = true;
    }


    /* Initialize all the globals */
    variables_defined = MY_FALSE;
    disable_sefuns   = isMasterObj;
    last_expression  = -1;
    compiled_prog    = NULL;  /* NULL means fail to load. */
    heart_beat       = -1;
    comp_stackp      = 0;     /* Local temp stack used by compiler */
    current_continue_address = 0;
    current_break_address    = 0;
    num_parse_error  = 0;
    block_depth      = 0;
    default_varmod = 0;
    default_funmod = 0;
    current_inline = NULL;
    inline_closure_id = 0;

    free_all_local_names();   /* In case of earlier error */

    /* Initialize memory blocks where the result of the compilation
     * will be stored.
     */
    for (i = 0; i < NUMAREAS; i++)
#line 16666 "prolang.y"
    {
        mem_block[i].block = xalloc(START_BLOCK_SIZE);
        mem_block[i].current_size = 0;
        mem_block[i].max_size = START_BLOCK_SIZE;
    }

    extend_mem_block(A_LOCAL_TYPES, MAX_LOCAL * sizeof(A_LOCAL_TYPES_t));
    memset(&LOCAL_TYPE(0), 0, LOCAL_TYPE_COUNT * sizeof(A_LOCAL_TYPES_t));

    type_of_locals = &(LOCAL_TYPE(0));
    type_of_context = type_of_locals;
#ifdef DEBUG_INLINES
printf("DEBUG: prolog: type ptrs: %p, %p\n", type_of_locals, type_of_context );
#endif /* DEBUG_INLINES */

    compiled_file = fname;
    stored_lines = 0;
    stored_bytes = 0;
    last_include_start = -1;
    memset(prog_string_indizes, -1, sizeof prog_string_indizes);
    num_virtual_variables = 0;
    case_state.free_block = NULL;
    case_state.next_free = NULL;
    last_initializer_end = -4; /* To pass the test in transfer_init_control() */
    variables_initialized = 0;
    argument_level = 0;
    got_ellipsis[0] = MY_FALSE;

    max_number_of_init_locals = 0;

    /* Check if call_other() has been replaced by a sefun.
     */
    call_other_sefun = -1;

    if (!disable_sefuns)
#line 16701 "prolang.y"
    {
        id = make_shared_identifier_mstr(STR_CALL_OTHER, I_TYPE_UNKNOWN, 0);

        if (!id)
            fatal("Out of memory: identifier '%s'.\n", get_txt(STR_CALL_OTHER));

        if (id->type != I_TYPE_UNKNOWN)
#line 16708 "prolang.y"
        {
            /* This shouldn't be necessary, but just in case... */
            while (id && id->type > I_TYPE_GLOBAL)
                id = id->inferior;

            if ( id
              && id->u.global.function < 0
              && id->u.global.sim_efun >= 0)
#line 16716 "prolang.y"
            {
                /* There is a sefun for call_other() */
                call_other_sefun = id->u.global.sim_efun;
            }
        }
    } /* if (!disable_sefuns) */

} /* prolog() */

/*-------------------------------------------------------------------------*/
static void
epilog (void)

/* The parser finished - now collect the information and generate
 * the program structure, if the parse was successful.
 */

#line 16733 "prolang.y"
{
    int          i, fx;
    p_int        size;
    mp_int       num_functions;
    mp_int       num_strings;
    mp_int       num_variables;
    mp_int       num_function_headers;
    bytecode_p   p;
    ident_t     *g, *q;
    function_t  *f;
    function_t  *funname_start1;  /* The name chains (to sort) */
    function_t  *funname_start2;
    mp_int       num_function_names;
    program_t   *prog;

    /* First, clean up */
#ifdef DEBUG
    if (num_parse_error == 0 && type_of_arguments.current_size != 0)
        fatal("Failed to deallocate argument type stack\n");
#endif

    if (last_string_constant)

#line 16756 "prolang.y"
    {
        free_mstring(last_string_constant);
        last_string_constant = NULL;
    }

    free_case_blocks();

    for (i = 0; (size_t)i < STRUCT_MEMBER_COUNT; i++)
#line 16764 "prolang.y"
    {
        free_struct_member_data(&STRUCT_MEMBER(i));
    }
    mem_block[A_STRUCT_MEMBERS].current_size = 0;

    /* If the parse was successful, Make sure that all structs are defined and
     * reactivate old structs where possible.
     * If an error occurs, num_parse_error is incremented and epilog() will
     * bail out below.
     */
    if (!num_parse_error && !inherit_file)
#line 16775 "prolang.y"
    {
        struct_epilog();
    }

    /* Append the non-virtual variable block to the virtual ones,
     * and take care of the initializers.
     */
    if (V_VARIABLE_COUNT > 0x100)
#line 16783 "prolang.y"
    {
        yyerror("Too many virtual variables");
    }

    add_to_mem_block(
        A_VIRTUAL_VAR,
        mem_block[A_VARIABLES].block,
        mem_block[A_VARIABLES].current_size
    );
    mem_block[A_VARIABLES].current_size = 0;

    /* Define the __INIT function, but only if there was any code
     * to initialize.
     */
    if (last_initializer_end > 0)
#line 16798 "prolang.y"
    {
        ident_t *ip;

        ip = make_global_identifier(get_txt(STR_VARINIT), I_TYPE_UNKNOWN);
        if (ip)
#line 16803 "prolang.y"
        {
            /* Update the function header for __INIT. Look at the __INIT
             * block_scope (#0), whether we need some space for local variables.
             */
            define_new_function(MY_FALSE, ip, 0, max_number_of_init_locals
                                , first_initializer_start
                                , TYPE_MOD_PROTECTED, get_fulltype(lpctype_unknown));
        }
        /* ref count for ip->name was incremented by transfer_init_control() */

        /* Change the last jump after the last initializer into a
         * return(1) statement.
         */
        mem_block[A_PROGRAM].block[last_initializer_end-1] =
            F_CONST1;
        mem_block[A_PROGRAM].block[last_initializer_end-0] =
            F_RETURN;
    } /* if (has initializer) */

    /* Check the string block. We don't have to count the include file names
     * as those won't be accessed from the program code.
     */
    if (STRING_COUNT > 0x10000)
        yyerror("Too many strings");

    /* Get and check the numbers of functions, strings, and variables */
    num_functions = FUNCTION_COUNT;
    if (num_functions > 0x10000)
#line 16831 "prolang.y"
    {
        yyerror("Too many functions");
    }
    num_strings = STRING_COUNT;
    num_variables = V_VARIABLE_COUNT;
    if (num_variables >= VIRTUAL_VAR_TAG)
#line 16837 "prolang.y"
    {
        yyerror("Too many variables");
    }

#if 0
    printf("DEBUG: ----- Inherit list for %s: -----\n", current_loc.file->name);
    for(i = 0; i < INHERIT_COUNT; i++)
#line 16844 "prolang.y"
    {
        inherit_t* inh = &INHERIT(i);

        printf("DEBUG: [%03d: %02x] var: %3d%s, fun: %3d - %-32s ",
            i, inh->inherit_type,
            inh->variable_index_offset & ~NON_VIRTUAL_OFFSET_TAG,
            (inh->variable_index_offset & NON_VIRTUAL_OFFSET_TAG) ? "r" : "v",
            inh->function_index_offset,
            get_txt(inh->prog->name));

        if (inh->inherit_type & INHERIT_TYPE_MAPPED)
            printf("(mapped to %d)\n", inh->updated_inherit);
        else if (inh->inherit_type & INHERIT_TYPE_DUPLICATE)
            printf("(duplicate)\n");
        else if (inh->inherit_type & INHERIT_TYPE_EXTRA)
            printf("(extra)\n");
        else if (inh->inherit_type & INHERIT_TYPE_VIRTUAL)
            printf("(virtual)\n");
        else
            printf("(regular)\n");
    }
    printf("DEBUG: ------\n");

    printf("DEBUG: ----- Function table for %s: -----\n", current_loc.file->name);
    for(i = 0; i < num_functions; i++)
#line 16869 "prolang.y"
    {
        f = FUNCTION(i);

        printf("DEBUG: [%03d: %08x] %-32s ", i, f->flags, get_txt(f->name));
        if (f->flags & NAME_INHERITED)
#line 16874 "prolang.y"
        {
            if (f->flags & NAME_CROSS_DEFINED)
                printf("(cross-defined to %d)\n", (int)(i + GET_CROSSDEF_OFFSET(f->offset.func)));
            //else if (f->flags & NAME_HIDDEN)
            //    printf("(hidden)\n");
            else
                printf("(inherited from %s [%d])\n", get_txt(INHERIT(f->offset.inherit).prog->name), f->offset.inherit);
        }
        else if (f->flags & NAME_UNDEFINED)
            printf("(undefined)\n");
        else if (f->flags & NAME_PROTOTYPE)
            printf("(declared)\n");
        else
            printf("(defined)\n");
    }
    printf("DEBUG: ------\n");
#endif

    num_function_names = 0;
    num_function_headers = 0;
    if (!num_parse_error && !inherit_file)
#line 16895 "prolang.y"
    {
        /* If the parse was successful, fill in undefined functions,
         * resolve cross-defines, and sort the program names with mergesort.
         */

        function_t **link1, **link2;  /* Linkpointer for the sort */

        f = FUNCTION(0);
        link1 = &funname_start2;
        link2 = &funname_start1;

        for (i = num_functions; --i >= 0; f++)
#line 16907 "prolang.y"
        {
            funflag_t flags;

            /* If the function was cross-defined, the targeted function might
             * be a cross-definition itself. Unravel such a cross-definition
             * chain and let f->offset.func point to the actual definition.
             */
            if ( f->flags & NAME_CROSS_DEFINED )
#line 16915 "prolang.y"
            {
                int32 offset;

                offset = GET_CROSSDEF_OFFSET(f->offset.func);
                while (f[offset].flags & NAME_CROSS_DEFINED)
#line 16920 "prolang.y"
                {
                    f->offset.func = offset + f[offset].offset.func;
                    offset = GET_CROSSDEF_OFFSET(f->offset.func);
                }
            }

            /* If the function is undefined, generate a dummy function
             * with UNDEF as body.
             * Except __INIT, which is created as CONST1 RETURN.
             */
            if ((f->flags & (NAME_UNDEFINED|NAME_INHERITED)) == NAME_UNDEFINED)
#line 16931 "prolang.y"
            {
                CURRENT_PROGRAM_SIZE = align(CURRENT_PROGRAM_SIZE);
                if (!realloc_a_program(FUNCTION_HDR_SIZE + 2))
#line 16934 "prolang.y"
                {
                    yyerrorf("Out of memory: program size %"PRIuMPINT"\n"
                            , CURRENT_PROGRAM_SIZE + FUNCTION_HDR_SIZE + 2);
                }
                else
#line 16939 "prolang.y"
                {
                    f->offset.pc = CURRENT_PROGRAM_SIZE + FUNCTION_PRE_HDR_SIZE;
                    p = PROGRAM_BLOCK + CURRENT_PROGRAM_SIZE
                        + FUNCTION_HDR_SIZE;
                    /* If __INIT() is undefined (i.e. there was a prototype, but
                     * no explicit function nor the automagic initialization code,
                     * then a dummy function is generated. This prevents crashes
                     * when this program is inherited later.
                     */
                    if (mstreq(f->name, STR_VARINIT) && !f->num_arg)
#line 16949 "prolang.y"
                    {
                        f->flags &= ~NAME_UNDEFINED;
                        *p++ = F_CONST1;
                        *p   = F_RETURN;
                    } else {
                        *p = F_UNDEF;
                    }
                    CURRENT_PROGRAM_SIZE += FUNCTION_HDR_SIZE + 2;
                }

                /* We'll include prototype in the function_names list,
                 * but make it protected. We need them in the function_names
                 * list, so we can easily handle program updates of virtually
                 * inherited programs, as we have to patch prototypes there.
                 * But when prototypes are included in function_names, then
                 * they'll get callable by call_other() and similar functions,
                 * which will result in F_UNDEF throwing an error. So we make
                 * them protected. This'll make sure, no external program
                 * can call them. And inheriting programs can override it
                 * with their own visibility.
                 */
                if (!(f->flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN)))
                    f->flags = ( f->flags & ~TYPE_MOD_PUBLIC ) | TYPE_MOD_PROTECTED;
                else if (f->flags & NAME_PROTOTYPE) /* Private prototypes don't make sense. */
                    yywarnf("Private prototype of '%s' has no definition", get_txt(f->name));
            }

            /* Set the function address resp. inherit index in
             * the function's flags.
             */
            flags = f->flags;
            f->flags = flags & NAME_INHERITED ?
              (flags & ~INHERIT_MASK)  | (f->offset.inherit & INHERIT_MASK) :
              (flags & ~FUNSTART_MASK) | (f->offset.pc & FUNSTART_MASK);
            /* If the function is visible, add it to the list of names
             * to be sorted.
             */
            if ( !(flags & (NAME_HIDDEN|TYPE_MOD_PRIVATE) ) )
#line 16987 "prolang.y"
            {
                *link1 = f;
                link1 = link2;
                link2 = &f->offset.next;
                num_function_names++;
            }

            if ( !(flags & (NAME_INHERITED)))
                num_function_headers++;
        }

        /* End the two chains */
        *link1 = NULL;
        *link2 = NULL;

        /* Store line number info for undefined functions */
        store_line_number_info();

        /* Sort the function names */
        if (num_function_names <= 1)
#line 17007 "prolang.y"
        {
            /* Nothing to sort */
            funname_start1 = funname_start2;
        }
        else
#line 17012 "prolang.y"
        {
            /* Mergesort again.
             * TODO: Make this a standard function.
             */
            int runlength;

            runlength = 1;
            do {
                function_t *out_start1, *out_start2, **out1, **out2;
                int count1, count2;

                count1 = num_function_names & (runlength-1);
                count2 = num_function_names & runlength;
                if (!count1)
#line 17026 "prolang.y"
                {
                    out2 = &out_start1;
                    *out2 = funname_start2;
                    while (--count2 >= 0)
#line 17030 "prolang.y"
                    {
                        out2 = &(*out2)->offset.next;
                    }
                    funname_start2 = *out2;
                    count1 = count2 = runlength;
                    out1 = &out_start2;
                }
                else if (!count2)
#line 17038 "prolang.y"
                {
                    out2 = &out_start1;
                    *out2 = funname_start1;
                    do
#line 17042 "prolang.y"
                    {
                        out2 = &(*out2)->offset.next;
                    } while (--count1);
                    funname_start1 = *out2;
                    count1 = count2 = runlength;
                    out1 = &out_start2;
                }
                else
#line 17050 "prolang.y"
                {
                    out1 = &out_start1;
                    out2 = &out_start2;
                }

                while (funname_start1)
#line 17056 "prolang.y"
                {
                    while (1) {
                        /* Compare the two pointers.
                         * The comparison operation has to match the
                         * one in closure.c:function_cmp().
                         */
                        if (memcmp(
                              &funname_start2->name,
                              &funname_start1->name,
                              sizeof(char *)
                            ) < 0)
#line 17067 "prolang.y"
                        {
                            *out1 = funname_start2;
                            out1 = &funname_start2->offset.next;
                            funname_start2 = *out1;
                            if (!--count2)
#line 17072 "prolang.y"
                            {
                                *out1 = funname_start1;
                                do {
                                    out1 = &(*out1)->offset.next;
                                } while (--count1);
                                funname_start1 = *out1;
                                break;
                            }
                        }
                        else
#line 17082 "prolang.y"
                        {
                            *out1 = funname_start1;
                            out1 = &funname_start1->offset.next;
                            funname_start1 = *out1;
                            if (!--count1)
#line 17087 "prolang.y"
                            {
                                *out1 = funname_start2;
                                do {
                                    out1 = &(*out1)->offset.next;
                                } while (--count2);
                                funname_start2 = *out1;
                                break;
                            }
                        }
                    }
#line 17097 "prolang.y"
                    {
                        function_t **temp;

                        temp = out1;
                        out1 = out2;
                        out2 = temp;
                    }
                    count1 = count2 = runlength;
                }
                *out1 = NULL;
                *out2 = NULL;
                funname_start1 = out_start1;
                funname_start2 = out_start2;

                runlength <<= 1;
            } while (runlength < num_function_names);
        } /* end of sort */

        /* either funname_start1 or funname_start2 now has the
         * sorted list of function names.
         */

        /* Raise error if program got too large. */
        if (CURRENT_PROGRAM_SIZE > FUNSTART_MASK)
#line 17121 "prolang.y"
        {
            yyerror("Program too large");
        }

        /* Done: functions are sorted, resolved, etc etc */
    } /* if (parse successful) */

    /* Free unneeded memory */
    free_all_local_names();

    for (q = all_globals; NULL != (g = q); )
#line 17132 "prolang.y"
    {
         q = g->next_all;
         free_shared_identifier(g);
    }

    while(last_yalloced)
#line 17138 "prolang.y"
    {
        yfree(last_yalloced);
        debug_message("%s freeing lost block\n", time_stamp());
    }

    if (all_efun_shadows)
#line 17144 "prolang.y"
    {
        efun_shadow_t *s, *t;

        for (t = all_efun_shadows; NULL != (s = t); )
#line 17148 "prolang.y"
        {
            s->shadow->u.global.function = I_GLOBAL_FUNCTION_OTHER;
            s->shadow->u.global.variable = I_GLOBAL_VARIABLE_FUN;
            t = s->next;
            xfree(s);
        }
        all_efun_shadows = NULL;
    }

    all_globals = NULL;
    
    remove_unknown_identifier();

    /* Remove the concrete struct definition from the lpctype object. */
    for (i = 0; (size_t)i < STRUCT_COUNT; i++)
        clean_struct_type(STRUCT_DEF(i).type->name->lpctype);

    /* Now create the program structure */
    switch (0) { default:

#if 0
#line 17169 "prolang.y"
{
    int i, j;

    printf("DEBUG: --- structs in %s ---\n", current_loc.file->name);
    for (i = 0; i < STRUCT_COUNT; i++)
#line 17174 "prolang.y"
    {
        struct_type_t * ptype;
        ptype = STRUCT_DEF(i).type;
        printf("DEBUG: [%d] struct %s: (%s #%"PRId32") ref %"PRIdPINT
               ", %hd members, base %s, flags %"PRIx32"\n"
              , i, get_txt(ptype->name)
              , ptype->prog_name ? get_txt(ptype->prog_name) : "<none>"
              , ptype->prog_id
              , ptype->ref
              , ptype->num_members
              , ptype->base ? get_txt(ptype->base->name) : "<none>"
              , STRUCT_DEF(i).flags
              );
        fflush(stdout);
#if 1
        for (j = 0; j < ptype->num_members; j++)
#line 17190 "prolang.y"
        {
            fulltype_t ftype;

            assign_var_to_fulltype(&ftype, ptype->member[j].type);
            printf("DEBUG:       [%d] member %s: %s\n"
                  , j, get_txt(ptype->member[j].name)
                  , get_type_name(ftype)
                  );
            fflush(stdout);
        }
#endif
    }
    printf("DEBUG: ------\n");

}
#endif /* 0 */

        /* On error, don't create anything */
        if (num_parse_error > 0 || inherit_file)
            break;

        /* Compute the size of the program.
         * Right now, we allocate everything in one block.
         */

        size = align(sizeof (program_t));

        if (!pragma_save_types)
#line 17218 "prolang.y"
        {
            for (i = 0; (size_t)i < ARGTYPE_COUNT; i++)
                free_lpctype(ARGUMENT_TYPE(i));
            mem_block[A_ARGUMENT_TYPES].current_size = 0;
            mem_block[A_ARGUMENT_INDEX].current_size = 0;
        }
        for (i = 0; i< NUMPAREAS; i++)
#line 17225 "prolang.y"
        {
            if (i != A_LINENUMBERS)
                size += align(mem_block[i].current_size);
        }

        size += align(num_function_names * sizeof *prog->function_names);
        size += align(num_functions * sizeof *prog->functions);
        size += align(num_function_headers * sizeof *prog->function_headers);

        /* Get the program structure */
        if ( !(p = xalloc(size)) )
#line 17236 "prolang.y"
        {
            yyerrorf("Out of memory: program structure (%"PRIdPINT" bytes)", 
                     size);
            break;
        }

        prog = (program_t *)p;
        *prog = NULL_program;

        /* Set up the program structure */
        if ( !(prog->name = new_mstring(current_loc.file->name)) )
#line 17247 "prolang.y"
        {
            xfree(prog);
            yyerrorf("Out of memory: filename '%s'", current_loc.file->name);
            break;
        }
        prog->blueprint = NULL;
        prog->total_size = size;
        prog->ref = 0;
        prog->heart_beat = heart_beat;
        prog->id_number =
          ++current_id_number ? current_id_number : renumber_programs();
        prog->flags = (pragma_no_clone ? P_NO_CLONE : 0)
                    | (pragma_no_inherit ? P_NO_INHERIT : 0)
                    | (pragma_no_shadow ? P_NO_SHADOW : 0)
                    | (pragma_share_variables ? P_SHARE_VARIABLES : 0)
                    | (pragma_rtt_checks ? P_RTT_CHECKS : 0)
                    | (pragma_warn_rtt_checks ? P_WARN_RTT_CHECKS : 0)
                    ;

        prog->load_time = current_time;

        total_prog_block_size += prog->total_size + mstrsize(prog->name);
        total_num_prog_blocks += 1;
        p += align(sizeof (program_t));

        /* Add the program code
         */
        prog->program = p;
        if (mem_block[A_PROGRAM].current_size)
            memcpy(p, mem_block[A_PROGRAM].block,
                   mem_block[A_PROGRAM].current_size);
        p += align(mem_block[A_PROGRAM].current_size);

        /* Add the function headers right after the program code
         */
        prog->num_function_headers = num_function_headers;
        prog->function_headers = (A_FUNCTIONS_t*)p;

        f = GET_BLOCK(A_FUNCTIONS);
        fx = 0;
        for (i = 0; i< num_functions; i++, f++)
#line 17288 "prolang.y"
        {
            if ( !(f->flags & (NAME_INHERITED)))
#line 17290 "prolang.y"
            {
                function_t * head = ((A_FUNCTIONS_t*)p) + fx;

                *head = *f;
                head->offset.fx = i;
                /* We'll adopt the reference of the name. */

                *FUNCTION_HEADER_INDEXP(prog->program + (f->flags & FUNSTART_MASK)) = fx;

                fx++;
            }
        }

        p += align(num_function_headers * sizeof *prog->function_headers);

        /* Add the function names.
         */
        prog->num_function_names = num_function_names;
        prog->function_names = (unsigned short *)p;
#line 17309 "prolang.y"
        {
            unsigned short *namep;

            namep = (unsigned short *)p;
            if ( NULL != (f = funname_start1) || NULL != (f = funname_start2) )
#line 17314 "prolang.y"
            {
                do {
                    *namep++ = f - FUNCTION(0);
                } while ( NULL != (f = f->offset.next) );
            }
        }
        p += align(num_function_names * sizeof *prog->function_names);

        /* Add the function flags
         */
        prog->num_functions = num_functions;
        prog->functions = (funflag_t *)p;
#line 17326 "prolang.y"
        {
            funflag_t *flagp;

            f = FUNCTION(0);
            flagp = (funflag_t *)p;
            for (i = num_functions; --i >= 0; f++)
#line 17332 "prolang.y"
            {
                *flagp++ = f->flags;
            }
        }
        p += align(num_functions * sizeof *prog->functions);

        /* Add the program strings
         */
        prog->strings = (A_STRINGS_t *)p;
        prog->num_strings = num_strings;
        if (mem_block[A_STRINGS].current_size)
            memcpy(p, mem_block[A_STRINGS].block,
                   mem_block[A_STRINGS].current_size);

        p += align(mem_block[A_STRINGS].current_size);

        /* Add the variable descriptions
         */
        prog->variables = (A_VIRTUAL_VAR_t *)p;
        prog->num_variables = num_variables;
        prog->num_virtual_variables = num_virtual_variables;
        if (mem_block[A_VIRTUAL_VAR].current_size)
            memcpy(p, mem_block[A_VIRTUAL_VAR].block,
                   mem_block[A_VIRTUAL_VAR].current_size);

        p += align(mem_block[A_VIRTUAL_VAR].current_size);

        /* Add the inheritance information, and don't forget
         * to delete our internal flags.
         */
        prog->num_inherited = INHERIT_COUNT;
        if (prog->num_inherited)
#line 17364 "prolang.y"
        {
            memcpy(p, mem_block[A_INHERITS].block,
                   mem_block[A_INHERITS].current_size);
            prog->inherit = (A_INHERITS_t *)p;
        } else {
            prog->inherit = NULL;
        }
        p += align(mem_block[A_INHERITS].current_size);

        /* Index table for obsolete inherited programs. */
        prog->update_index_map = (A_UPDATE_INDEX_MAP_t *)p;
        if (mem_block[A_UPDATE_INDEX_MAP].current_size)
            memcpy(p, mem_block[A_UPDATE_INDEX_MAP].block,
                   mem_block[A_UPDATE_INDEX_MAP].current_size);

        p += align(mem_block[A_UPDATE_INDEX_MAP].current_size);

        /* Add the struct information.
         */
        prog->num_structs = STRUCT_COUNT;
        if (prog->num_structs)
#line 17385 "prolang.y"
        {
            memcpy(p, mem_block[A_STRUCT_DEFS].block,
                   mem_block[A_STRUCT_DEFS].current_size);
            prog->struct_defs = (A_STRUCT_DEFS_t *)p;
        } else {
            prog->struct_defs = NULL;
        }
        p += align(mem_block[A_STRUCT_DEFS].current_size);

        /* Add the include file information */
        prog->num_includes = INCLUDE_COUNT;
        if (prog->num_includes)
#line 17397 "prolang.y"
        {
            memcpy(p, mem_block[A_INCLUDES].block
                    , mem_block[A_INCLUDES].current_size);
            prog->includes = (A_INCLUDES_t *)p;
        }
        else
            prog->includes = NULL;
        p += align(mem_block[A_INCLUDES].current_size);

        /* Add the argument type information
         */
        if (pragma_save_types)
#line 17409 "prolang.y"
        {
            if (mem_block[A_ARGUMENT_TYPES].current_size)
                memcpy(p, mem_block[A_ARGUMENT_TYPES].block,
                       mem_block[A_ARGUMENT_TYPES].current_size);
            prog->argument_types = (A_ARGUMENT_TYPES_t *)p;
            prog->num_argument_types = ARGTYPE_COUNT;
            p += align(mem_block[A_ARGUMENT_TYPES].current_size);

            if (mem_block[A_ARGUMENT_INDEX].current_size)
                memcpy(p, mem_block[A_ARGUMENT_INDEX].block,
                       mem_block[A_ARGUMENT_INDEX].current_size);
            prog->type_start = (A_ARGUMENT_INDEX_t *)p;
            p += align(mem_block[A_ARGUMENT_INDEX].current_size);
        }
        else
#line 17424 "prolang.y"
        {
            prog->argument_types = NULL;
            prog->type_start = NULL;
            prog->num_argument_types = 0;

            for (i = 0; (size_t)i < ARGTYPE_COUNT; i++)
                free_lpctype(ARGUMENT_TYPE(i));
        }

        /* Add the linenumber information.
         */
#line 17435 "prolang.y"
        {
            size_t linenumber_size;

            linenumber_size = mem_block[A_LINENUMBERS].current_size
                              + sizeof(linenumbers_t);

            if ( !(prog->line_numbers = xalloc(linenumber_size)) )
#line 17442 "prolang.y"
            {
                total_prog_block_size -= prog->total_size + mstrsize(prog->name)+1;
                total_num_prog_blocks -= 1;
                xfree(prog);
                yyerrorf("Out of memory: linenumber structure (%zu bytes)"
                        , linenumber_size);
                break;
            }
            total_prog_block_size += linenumber_size;
            prog->line_numbers->size = linenumber_size;
            if (mem_block[A_LINENUMBERS].current_size)
                memcpy( prog->line_numbers->line_numbers
                      , mem_block[A_LINENUMBERS].block
                      , mem_block[A_LINENUMBERS].current_size);
        }

        /* Correct the variable index offsets */
        fix_variable_index_offsets(prog);

        prog->swap_num = -1;

        /* Free the memareas */

        f = FUNCTION(0);
        for (i = 0; i< num_functions; i++, f++)
            /* All other references were adopted by the
             * function headers in the program.. */
            if ( (f->flags & (NAME_INHERITED)))
                free_lpctype(f->type);

        for (i = 0; i < NUMAREAS; i++)
#line 17473 "prolang.y"
        {
            xfree(mem_block[i].block);
        }

        type_of_locals = NULL;
        type_of_context = NULL;

        /* Reference the program and all inherits, but avoid multiple
         * referencing when an object inherits more than one object
         * and one of the inherited is already loaded and not the
         * last inherited.
         */
        reference_prog(prog, "epilog");
        for (i = 0; i < prog->num_inherited; i++)
#line 17487 "prolang.y"
        {
            reference_prog(prog->inherit[i].prog, "inheritance");
        }

        /* Return the value */
        compiled_prog = prog;
        return;
    }

    /* If we come here, the program couldn't be created - just
     * free all memory.
     */
#line 17499 "prolang.y"
    {
        function_t *functions;

        /* Free all function names and type data. */
        functions = FUNCTION(0);
        for (i = num_functions; --i >= 0; functions++)
#line 17505 "prolang.y"
        {
            if ( !(functions->flags & NAME_INHERITED) && functions->name )
#line 17507 "prolang.y"
            {
                /* The other references have been adopted. */
                free_mstring(functions->name);
            }
            free_lpctype(functions->type);
        }

        do_free_sub_strings( num_strings
                           , GET_BLOCK(A_STRINGS)
                           , num_variables
                           , GET_BLOCK(A_VIRTUAL_VAR)
                           , INCLUDE_COUNT
                           , GET_BLOCK(A_INCLUDES)
                           , STRUCT_COUNT
                           , GET_BLOCK(A_STRUCT_DEFS)
                           );

        /* Free the type information */
        for (i = 0; (size_t)i < ARGTYPE_COUNT; i++)
            free_lpctype(ARGUMENT_TYPE(i));

        compiled_prog = NULL;

        for (i = 0; i < NUMAREAS; i++)
#line 17531 "prolang.y"
        {
            xfree(mem_block[i].block);
        }

        type_of_locals = NULL;
        type_of_context = NULL;
        return;
    }

    /* NOTREACHED */
} /* epilog() */

/*-------------------------------------------------------------------------*/
void
compile_file (int fd, const char * fname,  Bool isMasterObj)

/* Compile an LPC file. See the head comment for instructions.
 */

#line 17550 "prolang.y"
{
    prolog(fname, isMasterObj);
    start_new_file(fd, fname);
    yyparse();
    /* If the parse failed, either num_parse_error != 0
     * or inherit_file != NULL here.
     */
    epilog();
    end_new_file();
} /* compile_file() */

/*-------------------------------------------------------------------------*/
Bool
is_undef_function (bytecode_p fun)

/* Return TRUE if <fun> points to a referenced but undefined function.
 */

#line 17568 "prolang.y"
{
    return GET_CODE(fun) == F_UNDEF;
} /* is_undef_function() */

/*-------------------------------------------------------------------------*/
#if defined( DEBUG ) && defined ( TRACE_CODE )

static int code_window_offset = -1;

void
set_code_window (void)

/* #pragma set_code_window: Remember the current program position.
 */

#line 17583 "prolang.y"
{
    code_window_offset = CURRENT_PROGRAM_SIZE;
}

void
show_code_window (void)

/* #pragma show_code_window: Print 32 bytes following the last
 * position remembered with set_code_window to stdout.
 */

#line 17594 "prolang.y"
{
    int i;
    bytecode_p p;

    if (code_window_offset < 0)
        return;
    p = (bytecode_p)mem_block[A_PROGRAM].block + code_window_offset;
    for (i = 0; i < 32; i++) {
        printf("%3d ", p[i]);
    }
    printf("\n");
    fflush(stdout);
} /* show_code_window() */

#endif

/*-------------------------------------------------------------------------*/
#ifdef GC_SUPPORT
void
clear_compiler_refs (void)

/* GC support: clear the references of memory held by the compiler environment.
 */

#line 17618 "prolang.y"
{
    /* clear_lpctype_ref handles NULL pointers, so we don't need to check. */
    clear_lpctype_ref(lpctype_unknown_array);
    clear_lpctype_ref(lpctype_any_array);
    clear_lpctype_ref(lpctype_int_float);
    clear_lpctype_ref(lpctype_int_array);
    clear_lpctype_ref(lpctype_string_array);
    clear_lpctype_ref(lpctype_object_array);
}

void
count_compiler_refs (void)

/* GC support: mark the memory held by the compiler environment.
 */

#line 17634 "prolang.y"
{
    if (type_of_arguments.block)
#line 17636 "prolang.y"
    {
        note_malloced_block_ref(type_of_arguments.block);
    }

    count_lpctype_ref(lpctype_unknown_array);
    count_lpctype_ref(lpctype_any_array);
    count_lpctype_ref(lpctype_int_float);
    count_lpctype_ref(lpctype_int_array);
    count_lpctype_ref(lpctype_string_array);
    count_lpctype_ref(lpctype_object_array);
}

#endif

#if defined(__MWERKS__) && !defined(WARN_ALL)
#    pragma warn_possunwant off
#    pragma warn_implicitconv off
#endif

/*-------------------------------------------------------------------------*/

/***************************************************************************/
/* vim: filetype=c
 */
