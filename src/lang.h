/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

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
#line 6687 "prolang.y" /* yacc.c:1909  */

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


#line 424 "y.tab.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
