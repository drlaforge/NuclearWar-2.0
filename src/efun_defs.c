#ifndef EFUN_DEFS_C__
#define EFUN_DEFS_C__ 1

/* DO NOT EDIT!
 *
 * This file is created automatically by make_func from
 * the specifications in func_spec.
 * It is meant to be included in lex.c
 */

#include "exec.h"    /* struct instr_s == instr_t */
#include "types.h"   /* lpctype_* definitions     */
#include "prolang.h" /* Some aggregate types      */

/*----------------------------------------------------------------------*/

/* The table of all instructions
 */

instr_t instrs[] = {

  /* --- codes --- */

  /*   0 */ { 0, F_ILLEGAL-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "illegal", NULL },
  /*   1 */ { 0, F_UNDEF-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "undef", NULL },
  /*   2 */ { 0, F_EFUN0-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "efun0", NULL },
  /*   3 */ { 0, F_EFUN1-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "efun1", NULL },
  /*   4 */ { 0, F_EFUN2-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "efun2", NULL },
  /*   5 */ { 0, F_EFUN3-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "efun3", NULL },
  /*   6 */ { 0, F_EFUN4-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "efun4", NULL },
  /*   7 */ { 0, F_EFUNV-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "efunv", NULL },
  /*   8 */ { 0, F_IDENTIFIER-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "identifier", NULL },
  /*   9 */ { 0, F_STRING-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "string", NULL },
  /*  10 */ { 0, F_CSTRING0-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "cstring0", NULL },
  /*  11 */ { 0, F_CSTRING1-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "cstring1", NULL },
  /*  12 */ { 0, F_CSTRING2-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "cstring2", NULL },
  /*  13 */ { 0, F_CSTRING3-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "cstring3", NULL },
  /*  14 */ { 0, F_NUMBER-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "number", NULL },
  /*  15 */ { 0, F_CONST0-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "const0", NULL },
  /*  16 */ { 0, F_CONST1-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "const1", NULL },
  /*  17 */ { 0, F_NCONST1-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "nconst1", NULL },
  /*  18 */ { 0, F_CLIT-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "clit", NULL },
  /*  19 */ { 0, F_NCLIT-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "nclit", NULL },
  /*  20 */ { 0, F_FCONST0-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "fconst0", NULL },
  /*  21 */ { 0, F_FLOAT-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "float", NULL },
  /*  22 */ { 0, F_CLOSURE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "closure", NULL },
  /*  23 */ { 0, F_SYMBOL-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "symbol", NULL },
  /*  24 */ { 0, F_RETURN-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "return", NULL },
  /*  25 */ { 0, F_RETURN0-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "return0", NULL },
  /*  26 */ { 0, F_DEFAULT_RETURN-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "default_return", NULL },
  /*  27 */ { 0, F_BREAK-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "break", NULL },
  /*  28 */ { 0, F_SWITCH-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "switch", NULL },
  /*  29 */ { 0, F_SSCANF-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "sscanf", NULL },
  /*  30 */ { 0, F_PARSE_COMMAND-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "parse_command", NULL },
  /*  31 */ { 0, F_LOCAL-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "local", NULL },
  /*  32 */ { 0, F_CATCH-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "catch", NULL },
  /*  33 */ { 0, F_INC-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "++", NULL },
  /*  34 */ { 0, F_DEC-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "--", NULL },
  /*  35 */ { 0, F_POST_INC-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "x++", NULL },
  /*  36 */ { 0, F_POST_DEC-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "x--", NULL },
  /*  37 */ { 0, F_PRE_INC-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "++x", NULL },
  /*  38 */ { 0, F_PRE_DEC-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "--x", NULL },
  /*  39 */ { 0, F_LAND-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "&&", NULL },
  /*  40 */ { 0, F_LOR-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "||", NULL },
  /*  41 */ { 0, F_ASSIGN-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "=", NULL },
  /*  42 */ { 0, F_VOID_ASSIGN-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "(void)=", NULL },
  /*  43 */ { 0, F_ADD-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "+", NULL },
  /*  44 */ { 0, F_SUBTRACT-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "-", NULL },
  /*  45 */ { 0, F_MULTIPLY-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "*", NULL },
  /*  46 */ { 0, F_DIVIDE-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "/", NULL },
  /*  47 */ { 0, F_MOD-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "%", NULL },
  /*  48 */ { 0, F_GT-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, ">", NULL },
  /*  49 */ { 0, F_GE-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, ">=", NULL },
  /*  50 */ { 0, F_LT-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "<", NULL },
  /*  51 */ { 0, F_LE-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "<=", NULL },
  /*  52 */ { 0, F_EQ-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "==", NULL },
  /*  53 */ { 0, F_NE-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "!=", NULL },
  /*  54 */ { 0, F_COMPL-CODE_OFFSET, 1, 1, 0, &_lpctype_mixed, -1, -1, false, "~", NULL },
  /*  55 */ { 0, F_AND-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "&", NULL },
  /*  56 */ { 0, F_OR-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "|", NULL },
  /*  57 */ { 0, F_XOR-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "^", NULL },
  /*  58 */ { 0, F_LSH-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "<<", NULL },
  /*  59 */ { 0, F_RSH-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, ">>", NULL },
  /*  60 */ { 0, F_RSHL-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, ">>>", NULL },
  /*  61 */ { 0, F_NOT-CODE_OFFSET, 1, 1, 0, &_lpctype_mixed, -1, -1, false, "!", NULL },
  /*  62 */ { 0, F_INDEX-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "index", NULL },
  /*  63 */ { 0, F_RINDEX-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "rindex", NULL },
  /*  64 */ { 0, F_AINDEX-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "aindex", NULL },
  /*  65 */ { 0, F_MAP_INDEX-CODE_OFFSET, 3, 3, 0, &_lpctype_mixed, -1, -1, false, "map_index", NULL },
  /*  66 */ { 0, F_S_INDEX-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "s_index", NULL },
  /*  67 */ { 0, F_RANGE-CODE_OFFSET, 3, 3, 0, &_lpctype_mixed, -1, -1, false, "..", NULL },
  /*  68 */ { 0, F_NR_RANGE-CODE_OFFSET, 3, 3, 0, &_lpctype_mixed, -1, -1, false, "..<", NULL },
  /*  69 */ { 0, F_RN_RANGE-CODE_OFFSET, 3, 3, 0, &_lpctype_mixed, -1, -1, false, "<..", NULL },
  /*  70 */ { 0, F_RR_RANGE-CODE_OFFSET, 3, 3, 0, &_lpctype_mixed, -1, -1, false, "<..<", NULL },
  /*  71 */ { 0, F_NA_RANGE-CODE_OFFSET, 3, 3, 0, &_lpctype_mixed, -1, -1, false, "..>", NULL },
  /*  72 */ { 0, F_RA_RANGE-CODE_OFFSET, 3, 3, 0, &_lpctype_mixed, -1, -1, false, "<..>", NULL },
  /*  73 */ { 0, F_AN_RANGE-CODE_OFFSET, 3, 3, 0, &_lpctype_mixed, -1, -1, false, ">..", NULL },
  /*  74 */ { 0, F_AR_RANGE-CODE_OFFSET, 3, 3, 0, &_lpctype_mixed, -1, -1, false, ">..<", NULL },
  /*  75 */ { 0, F_AA_RANGE-CODE_OFFSET, 3, 3, 0, &_lpctype_mixed, -1, -1, false, ">..>", NULL },
  /*  76 */ { 0, F_NX_RANGE-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "nx_range", NULL },
  /*  77 */ { 0, F_RX_RANGE-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "rx_range", NULL },
  /*  78 */ { 0, F_AX_RANGE-CODE_OFFSET, 2, 2, 0, &_lpctype_mixed, -1, -1, false, "ax_range", NULL },
  /*  79 */ { 0, F_VOID_ADD_EQ-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "(void)+=", NULL },
  /*  80 */ { 0, F_ADD_EQ-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "+=", NULL },
  /*  81 */ { 0, F_SUB_EQ-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "-=", NULL },
  /*  82 */ { 0, F_DIV_EQ-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "/=", NULL },
  /*  83 */ { 0, F_MULT_EQ-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "*=", NULL },
  /*  84 */ { 0, F_MOD_EQ-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "%=", NULL },
  /*  85 */ { 0, F_AND_EQ-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "&=", NULL },
  /*  86 */ { 0, F_OR_EQ-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "|=", NULL },
  /*  87 */ { 0, F_XOR_EQ-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "^=", NULL },
  /*  88 */ { 0, F_LSH_EQ-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "<<=", NULL },
  /*  89 */ { 0, F_RSH_EQ-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, ">>=", NULL },
  /*  90 */ { 0, F_RSHL_EQ-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, ">>>=", NULL },
  /*  91 */ { 0, F_LAND_EQ-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "&&=", NULL },
  /*  92 */ { 0, F_LOR_EQ-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "||=", NULL },
  /*  93 */ { 0, F_POP_VALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "pop_value", NULL },
  /*  94 */ { 0, F_POP_SECOND-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "pop_second", NULL },
  /*  95 */ { 0, F_DUP-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "dup", NULL },
  /*  96 */ { 0, F_LDUP-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "ldup", NULL },
  /*  97 */ { 0, F_SWAP_VALUES-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "swap_values", NULL },
  /*  98 */ { 0, F_CLEAR_LOCALS-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "clear_locals", NULL },
  /*  99 */ { 0, F_SAVE_ARG_FRAME-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "save_arg_frame", NULL },
  /* 100 */ { 0, F_RESTORE_ARG_FRAME-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "restore_arg_frame", NULL },
  /* 101 */ { 0, F_USE_ARG_FRAME-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "use_arg_frame", NULL },
  /* 102 */ { 0, F_FLATTEN_XARG-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "flatten_xarg", NULL },
  /* 103 */ { 0, F_FBRANCH-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "fbranch", NULL },
  /* 104 */ { 0, F_LBRANCH-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "lbranch", NULL },
  /* 105 */ { 0, F_LBRANCH_WHEN_ZERO-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "lbranch_when_zero", NULL },
  /* 106 */ { 0, F_LBRANCH_WHEN_NON_ZERO-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "lbranch_when_non_zero", NULL },
  /* 107 */ { 0, F_BRANCH-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "branch", NULL },
  /* 108 */ { 0, F_BRANCH_WHEN_ZERO-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "branch_when_zero", NULL },
  /* 109 */ { 0, F_BRANCH_WHEN_NON_ZERO-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "branch_when_non_zero", NULL },
  /* 110 */ { 0, F_BBRANCH_WHEN_ZERO-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "bbranch_when_zero", NULL },
  /* 111 */ { 0, F_BBRANCH_WHEN_NON_ZERO-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "bbranch_when_non_zero", NULL },
  /* 112 */ { 0, F_CALL_FUNCTION-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "call_function", NULL },
  /* 113 */ { 0, F_CALL_INHERITED-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "call_inherited", NULL },
  /* 114 */ { 0, F_CALL_INHERITED_NOARGS-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "call_inherited_noargs", NULL },
  /* 115 */ { 0, F_CALL_CLOSURE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "call_closure", NULL },
  /* 116 */ { 0, F_CONTEXT_CLOSURE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "context_closure", NULL },
  /* 117 */ { 0, F_CONTEXT_IDENTIFIER-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "context_identifier", NULL },
  /* 118 */ { 0, F_PUSH_CONTEXT_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "push_context_lvalue", NULL },
  /* 119 */ { 0, F_CONTEXT_IDENTIFIER16-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "context_identifier16", NULL },
  /* 120 */ { 0, F_PUSH_CONTEXT16_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "push_context16_lvalue", NULL },
  /* 121 */ { 0, F_PUSH_IDENTIFIER_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "push_identifier_lvalue", NULL },
  /* 122 */ { 0, F_VIRTUAL_VARIABLE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "virtual_variable", NULL },
  /* 123 */ { 0, F_PUSH_VIRTUAL_VARIABLE_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "push_virtual_variable_lvalue", NULL },
  /* 124 */ { 0, F_IDENTIFIER16-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "identifier16", NULL },
  /* 125 */ { 0, F_PUSH_IDENTIFIER16_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "push_identifier16_lvalue", NULL },
  /* 126 */ { 0, F_PUSH_LOCAL_VARIABLE_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "push_local_variable_lvalue", NULL },
  /* 127 */ { 0, F_INDEX_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "index_lvalue", NULL },
  /* 128 */ { 0, F_RINDEX_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "rindex_lvalue", NULL },
  /* 129 */ { 0, F_AINDEX_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "aindex_lvalue", NULL },
  /* 130 */ { 0, F_S_INDEX_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "s_index_lvalue", NULL },
  /* 131 */ { 0, F_RANGE_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "range_lvalue", NULL },
  /* 132 */ { 0, F_NR_RANGE_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "nr_range_lvalue", NULL },
  /* 133 */ { 0, F_RN_RANGE_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "rn_range_lvalue", NULL },
  /* 134 */ { 0, F_RR_RANGE_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "rr_range_lvalue", NULL },
  /* 135 */ { 0, F_NA_RANGE_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "na_range_lvalue", NULL },
  /* 136 */ { 0, F_RA_RANGE_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "ra_range_lvalue", NULL },
  /* 137 */ { 0, F_AN_RANGE_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "an_range_lvalue", NULL },
  /* 138 */ { 0, F_AR_RANGE_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "ar_range_lvalue", NULL },
  /* 139 */ { 0, F_AA_RANGE_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "aa_range_lvalue", NULL },
  /* 140 */ { 0, F_NX_RANGE_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "nx_range_lvalue", NULL },
  /* 141 */ { 0, F_RX_RANGE_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "rx_range_lvalue", NULL },
  /* 142 */ { 0, F_AX_RANGE_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "ax_range_lvalue", NULL },
  /* 143 */ { 0, F_MAP_INDEX_LVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "map_index_lvalue", NULL },
  /* 144 */ { 0, F_MAKE_PROTECTED-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "make_protected", NULL },
  /* 145 */ { 0, F_MAKE_RVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "make_rvalue", NULL },
  /* 146 */ { 0, F_PUSH_VIRTUAL_VARIABLE_VLVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "push_virtual_variable_vlvalue", NULL },
  /* 147 */ { 0, F_PUSH_IDENTIFIER_VLVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "push_identifier_vlvalue", NULL },
  /* 148 */ { 0, F_PUSH_IDENTIFIER16_VLVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "push_identifier16_vlvalue", NULL },
  /* 149 */ { 0, F_PUSH_CONTEXT_VLVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "push_context_vlvalue", NULL },
  /* 150 */ { 0, F_PUSH_LOCAL_VARIABLE_VLVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "push_local_variable_vlvalue", NULL },
  /* 151 */ { 0, F_INDEX_VLVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "index_vlvalue", NULL },
  /* 152 */ { 0, F_RINDEX_VLVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "rindex_vlvalue", NULL },
  /* 153 */ { 0, F_AINDEX_VLVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "aindex_vlvalue", NULL },
  /* 154 */ { 0, F_S_INDEX_VLVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "s_index_vlvalue", NULL },
  /* 155 */ { 0, F_MAP_INDEX_VLVALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "map_index_vlvalue", NULL },
  /* 156 */ { 0, F_SIMUL_EFUN-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "simul_efun", NULL },
  /* 157 */ { 0, F_AGGREGATE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "aggregate", NULL },
  /* 158 */ { 0, F_M_AGGREGATE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "m_aggregate", NULL },
  /* 159 */ { 0, F_M_CAGGREGATE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "m_caggregate", NULL },
  /* 160 */ { 0, F_S_AGGREGATE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "s_aggregate", NULL },
  /* 161 */ { 0, F_S_M_AGGREGATE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "s_m_aggregate", NULL },
  /* 162 */ { 0, F_PREVIOUS_OBJECT0-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "previous_object0", NULL },
  /* 163 */ { 0, F_LAMBDA_CCONSTANT-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "lambda_cconstant", NULL },
  /* 164 */ { 0, F_LAMBDA_CONSTANT-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "lambda_constant", NULL },
  /* 165 */ { 0, F_FOREACH-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "foreach", NULL },
  /* 166 */ { 0, F_FOREACH_REF-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "foreach_ref", NULL },
  /* 167 */ { 0, F_FOREACH_RANGE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "foreach_range", NULL },
  /* 168 */ { 0, F_FOREACH_NEXT-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "foreach_next", NULL },
  /* 169 */ { 0, F_FOREACH_END-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "foreach_end", NULL },
  /* 170 */ { 0, F_END_CATCH-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "end_catch", NULL },
  /* 171 */ { 0, F_BREAK_CONTINUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "break_continue", NULL },
  /* 172 */ { 0, F_BREAKN_CONTINUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "breakn_continue", NULL },
  /* 173 */ { 0, F_JUMP-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "jump", NULL },
  /* 174 */ { 0, F_NO_WARN_DEPRECATED-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "no_warn_deprecated", NULL },
  /* 175 */ { 0, F_ARRAY_RANGE_CHECK-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "array_range_check", NULL },
  /* 176 */ { 0, F_ARRAY0-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "array0", NULL },
  /* 177 */ { 0, F_MOVE_VALUE-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "move_value", NULL },
  /* 178 */ { 0, F_DUP_N-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "dup_n", NULL },
  /* 179 */ { 0, F_POP_N-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "pop_n", NULL },
  /* 180 */ { 0, F_PUT_ARRAY_ELEMENT-CODE_OFFSET, 0, 0, -1, NULL , -1, -1, false, "put_array_element", NULL },

  /* --- efuns --- */

  /* 181 */ { 0, F_CALL_DIRECT-CODE_OFFSET, -1, 2, 0, &_lpctype_unknown, 18, 6, true, "call_direct", NULL },
  /* 182 */ { 0, F_CALL_OTHER-CODE_OFFSET, -1, 2, 0, &_lpctype_unknown, 18, 6, true, "call_other", NULL },
  /* 183 */ { 0, F_CLONEP-CODE_OFFSET, 1, 1, F_THIS_OBJECT, &_lpctype_int, 0, 0, false, "clonep", NULL },
  /* 184 */ { 0, F_CLOSUREP-CODE_OFFSET, 1, 1, 0, &_lpctype_int, 0, 0, false, "closurep", NULL },
  /* 185 */ { 0, F_EXTERN_CALL-CODE_OFFSET, 0, 0, 0, &_lpctype_int, 0, 0, false, "extern_call", NULL },
  /* 186 */ { 0, F_FLOATP-CODE_OFFSET, 1, 1, 0, &_lpctype_int, 0, 0, false, "floatp", NULL },
  /* 187 */ { 0, F_INTP-CODE_OFFSET, 1, 1, 0, &_lpctype_int, 0, 0, false, "intp", NULL },
  /* 188 */ { 0, F_MAPPINGP-CODE_OFFSET, 1, 1, 0, &_lpctype_int, 0, 0, false, "mappingp", NULL },
  /* 189 */ { 0, F_MASTER-CODE_OFFSET, 1, 1, F_CONST0, &_lpctype_object, 26, 8, false, "master", NULL },
  /* 190 */ { 0, F_NEGATE-CODE_OFFSET, 1, 1, 0, &_lpctype_mixed, 7, 3, false, "negate", NULL },
  /* 191 */ { 0, F_OBJECTP-CODE_OFFSET, 1, 1, 0, &_lpctype_int, 0, 0, false, "objectp", NULL },
  /* 192 */ { 0, F_POINTERP-CODE_OFFSET, 1, 1, 0, &_lpctype_int, 0, 0, false, "pointerp", NULL },
  /* 193 */ { 0, F_RAISE_ERROR-CODE_OFFSET, 1, 1, 0, &_lpctype_void, 10, 4, false, "raise_error", NULL },
  /* 194 */ { 0, F_REFERENCEP-CODE_OFFSET, 1, 1, 0, &_lpctype_int, 2, 1, false, "referencep", NULL },
  /* 195 */ { 0, F_SIZEOF-CODE_OFFSET, 1, 1, 0, &_lpctype_int, 12, 5, false, "sizeof", NULL },
  /* 196 */ { 0, F_STRINGP-CODE_OFFSET, 1, 1, 0, &_lpctype_int, 0, 0, false, "stringp", NULL },
  /* 197 */ { 0, F_STRUCTP-CODE_OFFSET, 1, 1, 0, &_lpctype_int, 0, 0, false, "structp", NULL },
  /* 198 */ { 0, F_SYMBOLP-CODE_OFFSET, 1, 1, 0, &_lpctype_int, 0, 0, false, "symbolp", NULL },
  /* 199 */ { 0, F_THIS_INTERACTIVE-CODE_OFFSET, 0, 0, 0, &_lpctype_object, 0, 0, false, "this_interactive", NULL },
  /* 200 */ { 0, F_THIS_OBJECT-CODE_OFFSET, 0, 0, 0, &_lpctype_object, 0, 0, false, "this_object", NULL },
  /* 201 */ { 0, F_THIS_PLAYER-CODE_OFFSET, 0, 0, 0, &_lpctype_object, 0, 0, false, "this_player", NULL },
  /* 202 */ { 0, F_THROW-CODE_OFFSET, 1, 1, 0, &_lpctype_void, 0, 0, false, "throw", NULL },
  /* 203 */ { 0, F_TYPEOF-CODE_OFFSET, 1, 1, 0, &_lpctype_int, 4, 2, false, "typeof", NULL },

  /* --- 0-arg efuns --- */

  /* 204 */ { F_EFUN0, F_CALL_OUT_INFO-EFUN0_OFFSET, 0, 0, 0, &_lpctype_any_array, 0, 0, false, "call_out_info", NULL },
  /* 205 */ { F_EFUN0, F_CALLER_STACK_DEPTH-EFUN0_OFFSET, 0, 0, 0, &_lpctype_int, 0, 0, false, "caller_stack_depth", NULL },
  /* 206 */ { F_EFUN0, F_COMMAND_STACK-EFUN0_OFFSET, 0, 0, 0, &_lpctype_any_array, 0, 0, false, "command_stack", NULL },
  /* 207 */ { F_EFUN0, F_COMMAND_STACK_DEPTH-EFUN0_OFFSET, 0, 0, 0, &_lpctype_int, 0, 0, false, "command_stack_depth", NULL },
  /* 208 */ { F_EFUN0, F_GET_EVAL_COST-EFUN0_OFFSET, 0, 0, 0, &_lpctype_int, 0, 0, false, "get_eval_cost", NULL },
  /* 209 */ { F_EFUN0, F_HEART_BEAT_INFO-EFUN0_OFFSET, 0, 0, 0, &_lpctype_any_array, 0, 0, false, "heart_beat_info", NULL },
  /* 210 */ { F_EFUN0, F_QUERY_COMMAND-EFUN0_OFFSET, 0, 0, 0, &_lpctype_string, 0, 0, false, "query_command", NULL },
  /* 211 */ { F_EFUN0, F_REGEXP_PACKAGE-EFUN0_OFFSET, 0, 0, 0, &_lpctype_int, 0, 0, false, "regexp_package", NULL },
  /* 212 */ { F_EFUN0, F_RUSAGE-EFUN0_OFFSET, 0, 0, 0, &_lpctype_any_array, 0, 0, false, "rusage", NULL },
  /* 213 */ { F_EFUN0, F_TIME-EFUN0_OFFSET, 0, 0, 0, &_lpctype_int, 0, 0, false, "time", NULL },
  /* 214 */ { F_EFUN0, F_UNSHADOW-EFUN0_OFFSET, 0, 0, 0, &_lpctype_void, 0, 0, false, "unshadow", NULL },
  /* 215 */ { F_EFUN0, F_USERS-EFUN0_OFFSET, 0, 0, 0, &_lpctype_object_array, 0, 0, false, "users", NULL },
  /* 216 */ { F_EFUN0, F_UTIME-EFUN0_OFFSET, 0, 0, 0, &_lpctype_int_array, 0, 0, false, "utime", NULL },
  /* 217 */ { F_EFUN0, F_WIZLIST_INFO-EFUN0_OFFSET, 0, 0, 0, &_lpctype_any_array, 0, 0, false, "wizlist_info", NULL },

  /* --- 1-arg efuns --- */

  /* 218 */ { F_EFUN1, F_ABS-EFUN1_OFFSET, 1, 1, 0, &_lpctype_mixed, 7, 3, false, "abs", NULL },
  /* 219 */ { F_EFUN1, F_ACOS-EFUN1_OFFSET, 1, 1, 0, &_lpctype_float, 8, 9, false, "acos", NULL },
  /* 220 */ { F_EFUN1, F_ALL_INVENTORY-EFUN1_OFFSET, 1, 1, F_THIS_OBJECT, &_lpctype_object_array, 289, 106, false, "all_inventory", NULL },
  /* 221 */ { F_EFUN1, F_ASIN-EFUN1_OFFSET, 1, 1, 0, &_lpctype_float, 8, 9, false, "asin", NULL },
  /* 222 */ { F_EFUN1, F_ATAN-EFUN1_OFFSET, 1, 1, 0, &_lpctype_float, 7, 3, false, "atan", NULL },
  /* 223 */ { F_EFUN1, F_BLUEPRINT-EFUN1_OFFSET, 1, 1, F_THIS_OBJECT, &_lpctype_object, 298, 118, false, "blueprint", NULL },
  /* 224 */ { F_EFUN1, F_CALLER_STACK-EFUN1_OFFSET, 1, 1, F_CONST0, &_lpctype_object_array, 26, 8, false, "caller_stack", NULL },
  /* 225 */ { F_EFUN1, F_CAPITALIZE-EFUN1_OFFSET, 1, 1, 0, &_lpctype_string, 10, 4, false, "capitalize", NULL },
  /* 226 */ { F_EFUN1, F_CEIL-EFUN1_OFFSET, 1, 1, 0, &_lpctype_float, 7, 3, false, "ceil", NULL },
  /* 227 */ { F_EFUN1, F_CLONE_OBJECT-EFUN1_OFFSET, 1, 1, 0, &_lpctype_object, 298, 118, false, "clone_object", NULL },
  /* 228 */ { F_EFUN1, F_COPY-EFUN1_OFFSET, 1, 1, 0, &_lpctype_mixed, 0, 0, false, "copy", NULL },
  /* 229 */ { F_EFUN1, F_COS-EFUN1_OFFSET, 1, 1, 0, &_lpctype_float, 7, 3, false, "cos", NULL },
  /* 230 */ { F_EFUN1, F_COUNT_BITS-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 10, 4, false, "count_bits", NULL },
  /* 231 */ { F_EFUN1, F_CTIME-EFUN1_OFFSET, 1, 1, F_TIME, &_lpctype_string, 89, 30, false, "ctime", NULL },
  /* 232 */ { F_EFUN1, F_DEEP_COPY-EFUN1_OFFSET, 1, 1, 0, &_lpctype_mixed, 0, 0, false, "deep_copy", NULL },
  /* 233 */ { F_EFUN1, F_DESTRUCT-EFUN1_OFFSET, 1, 1, 0, &_lpctype_void, 289, 121, false, "destruct", NULL },
  /* 234 */ { F_EFUN1, F_DRIVER_INFO-EFUN1_OFFSET, 1, 1, 0, &_lpctype_mixed, 26, 8, false, "driver_info", NULL },
  /* 235 */ { F_EFUN1, F_EXP-EFUN1_OFFSET, 1, 1, 0, &_lpctype_float, 7, 3, false, "exp", NULL },
  /* 236 */ { F_EFUN1, F_FILE_SIZE-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 10, 4, false, "file_size", NULL },
  /* 237 */ { F_EFUN1, F_FIND_CALL_OUT-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 66, 60, false, "find_call_out", NULL },
  /* 238 */ { F_EFUN1, F_FIND_OBJECT-EFUN1_OFFSET, 1, 1, 0, &_lpctype_object, 10, 4, false, "find_object", NULL },
  /* 239 */ { F_EFUN1, F_FIRST_INVENTORY-EFUN1_OFFSET, 1, 1, F_THIS_OBJECT, &_lpctype_object, 101, 118, false, "first_inventory", NULL },
  /* 240 */ { F_EFUN1, F_FLOOR-EFUN1_OFFSET, 1, 1, 0, &_lpctype_float, 7, 3, false, "floor", NULL },
  /* 241 */ { F_EFUN1, F_GET_EXTRA_WIZINFO-EFUN1_OFFSET, 1, 1, 0, &_lpctype_mixed, 101, 35, false, "get_extra_wizinfo", NULL },
  /* 242 */ { F_EFUN1, F_GETEUID-EFUN1_OFFSET, 1, 1, F_THIS_OBJECT, &_lpctype_string, 289, 106, false, "geteuid", NULL },
  /* 243 */ { F_EFUN1, F_GETUID-EFUN1_OFFSET, 1, 1, F_THIS_OBJECT, &_lpctype_string, 289, 106, false, "getuid", NULL },
  /* 244 */ { F_EFUN1, F_GMTIME-EFUN1_OFFSET, 1, 1, F_TIME, &_lpctype_any_array, 89, 30, false, "gmtime", NULL },
  /* 245 */ { F_EFUN1, F_IDNA_TO_ASCII-EFUN1_OFFSET, 1, 1, 0, &_lpctype_string, 10, 4, false, "idna_to_ascii", NULL },
  /* 246 */ { F_EFUN1, F_IDNA_TO_UNICODE-EFUN1_OFFSET, 1, 1, 0, &_lpctype_string, 10, 4, false, "idna_to_unicode", NULL },
  /* 247 */ { F_EFUN1, F_INPUT_TO_INFO-EFUN1_OFFSET, 1, 1, 0, &_lpctype_any_array, 289, 106, false, "input_to_info", NULL },
  /* 248 */ { F_EFUN1, F_INTERACTIVE-EFUN1_OFFSET, 1, 1, F_THIS_OBJECT, &_lpctype_int, 289, 106, false, "interactive", NULL },
  /* 249 */ { F_EFUN1, F_INVERT_BITS-EFUN1_OFFSET, 1, 1, 0, &_lpctype_string, 10, 4, false, "invert_bits", NULL },
  /* 250 */ { F_EFUN1, F_LAST_BIT-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 10, 4, false, "last_bit", NULL },
  /* 251 */ { F_EFUN1, F_LIVING-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 289, 121, false, "living", NULL },
  /* 252 */ { F_EFUN1, F_LOAD_NAME-EFUN1_OFFSET, 1, 1, F_THIS_OBJECT, &_lpctype_string, 298, 35, false, "load_name", NULL },
  /* 253 */ { F_EFUN1, F_LOAD_OBJECT-EFUN1_OFFSET, 1, 1, 0, &_lpctype_object, 10, 4, false, "load_object", NULL },
  /* 254 */ { F_EFUN1, F_LOCALTIME-EFUN1_OFFSET, 1, 1, F_TIME, &_lpctype_any_array, 89, 30, false, "localtime", NULL },
  /* 255 */ { F_EFUN1, F_LOG-EFUN1_OFFSET, 1, 1, 0, &_lpctype_float, 7, 3, false, "log", NULL },
  /* 256 */ { F_EFUN1, F_LOWER_CASE-EFUN1_OFFSET, 1, 1, 0, &_lpctype_string, 10, 4, false, "lower_case", NULL },
  /* 257 */ { F_EFUN1, F_M_INDICES-EFUN1_OFFSET, 1, 1, 0, &_lpctype_any_array, 229, 86, false, "m_indices", NULL },
  /* 258 */ { F_EFUN1, F_MAKE_SHARED_STRING-EFUN1_OFFSET, 1, 1, 0, &_lpctype_string, 10, 4, false, "make_shared_string", "no longer useful" },
  /* 259 */ { F_EFUN1, F_MKDIR-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 10, 4, false, "mkdir", NULL },
  /* 260 */ { F_EFUN1, F_MKTIME-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 99, 34, false, "mktime", NULL },
  /* 261 */ { F_EFUN1, F_NEXT_INVENTORY-EFUN1_OFFSET, 1, 1, F_THIS_OBJECT, &_lpctype_object, 289, 106, false, "next_inventory", NULL },
  /* 262 */ { F_EFUN1, F_NOTIFY_FAIL-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 66, 60, false, "notify_fail", NULL },
  /* 263 */ { F_EFUN1, F_OBJECT_NAME-EFUN1_OFFSET, 1, 1, F_THIS_OBJECT, &_lpctype_string, 289, 121, false, "object_name", NULL },
  /* 264 */ { F_EFUN1, F_OBJECT_TIME-EFUN1_OFFSET, 1, 1, F_THIS_OBJECT, &_lpctype_int, 289, 106, false, "object_time", NULL },
  /* 265 */ { F_EFUN1, F_PREVIOUS_OBJECT-EFUN1_OFFSET, 1, 1, 0, &_lpctype_object, 26, 8, false, "previous_object", NULL },
  /* 266 */ { F_EFUN1, F_PROCESS_STRING-EFUN1_OFFSET, 1, 1, 0, &_lpctype_string, 10, 4, false, "process_string", NULL },
  /* 267 */ { F_EFUN1, F_PROGRAM_NAME-EFUN1_OFFSET, 1, 1, F_THIS_OBJECT, &_lpctype_string, 289, 121, false, "program_name", NULL },
  /* 268 */ { F_EFUN1, F_PROGRAM_TIME-EFUN1_OFFSET, 1, 1, F_THIS_OBJECT, &_lpctype_int, 289, 106, false, "program_time", NULL },
  /* 269 */ { F_EFUN1, F_QUERY_NOTIFY_FAIL-EFUN1_OFFSET, 1, 1, F_CONST0, &_lpctype_mixed, 26, 8, false, "query_notify_fail", NULL },
  /* 270 */ { F_EFUN1, F_QUERY_VERB-EFUN1_OFFSET, 1, 1, F_CONST0, &_lpctype_string, 26, 8, false, "query_verb", NULL },
  /* 271 */ { F_EFUN1, F_QUOTE-EFUN1_OFFSET, 1, 1, 0, &_lpctype_mixed, 81, 28, false, "quote", NULL },
  /* 272 */ { F_EFUN1, F_RANDOM-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 26, 8, false, "random", NULL },
  /* 273 */ { F_EFUN1, F_REMOVE_CALL_OUT-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 66, 60, false, "remove_call_out", NULL },
  /* 274 */ { F_EFUN1, F_REMOVE_INTERACTIVE-EFUN1_OFFSET, 1, 1, 0, &_lpctype_void, 289, 106, false, "remove_interactive", NULL },
  /* 275 */ { F_EFUN1, F_RESTORE_OBJECT-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 10, 4, false, "restore_object", NULL },
  /* 276 */ { F_EFUN1, F_RESTORE_VALUE-EFUN1_OFFSET, 1, 1, 0, &_lpctype_mixed, 10, 4, false, "restore_value", NULL },
  /* 277 */ { F_EFUN1, F_REVERSE-EFUN1_OFFSET, 1, 1, 0, &_lpctype_mixed, 34, 12, false, "reverse", NULL },
  /* 278 */ { F_EFUN1, F_RM-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 10, 4, false, "rm", NULL },
  /* 279 */ { F_EFUN1, F_RMDIR-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 10, 4, false, "rmdir", NULL },
  /* 280 */ { F_EFUN1, F_SET_NEXT_RESET-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 26, 8, false, "set_next_reset", NULL },
  /* 281 */ { F_EFUN1, F_SET_THIS_OBJECT-EFUN1_OFFSET, 1, 1, 0, &_lpctype_void, 289, 106, false, "set_this_object", NULL },
  /* 282 */ { F_EFUN1, F_SET_THIS_PLAYER-EFUN1_OFFSET, 1, 1, 0, &_lpctype_void, 289, 121, false, "set_this_player", NULL },
  /* 283 */ { F_EFUN1, F_SGN-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 7, 3, false, "sgn", NULL },
  /* 284 */ { F_EFUN1, F_SHADOW-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 289, 106, false, "shadow", NULL },
  /* 285 */ { F_EFUN1, F_SHUTDOWN-EFUN1_OFFSET, 1, 1, F_CONST0, &_lpctype_void, 26, 8, false, "shutdown", NULL },
  /* 286 */ { F_EFUN1, F_SIN-EFUN1_OFFSET, 1, 1, 0, &_lpctype_float, 7, 3, false, "sin", NULL },
  /* 287 */ { F_EFUN1, F_SQRT-EFUN1_OFFSET, 1, 1, 0, &_lpctype_float, 7, 3, false, "sqrt", NULL },
  /* 288 */ { F_EFUN1, F_SYMBOL_VARIABLE-EFUN1_OFFSET, 1, 1, 0, &_lpctype_closure, 301, 111, false, "symbol_variable", NULL },
  /* 289 */ { F_EFUN1, F_TAN-EFUN1_OFFSET, 1, 1, 0, &_lpctype_float, 7, 3, false, "tan", NULL },
  /* 290 */ { F_EFUN1, F_TO_ARRAY-EFUN1_OFFSET, 1, 1, 0, &_lpctype_any_array, 53, 16, false, "to_array", NULL },
  /* 291 */ { F_EFUN1, F_TO_FLOAT-EFUN1_OFFSET, 1, 1, 0, &_lpctype_float, 49, 15, false, "to_float", NULL },
  /* 292 */ { F_EFUN1, F_TO_INT-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 44, 14, false, "to_int", NULL },
  /* 293 */ { F_EFUN1, F_TO_OBJECT-EFUN1_OFFSET, 1, 1, 0, &_lpctype_object, 65, 19, false, "to_object", NULL },
  /* 294 */ { F_EFUN1, F_TO_STRING-EFUN1_OFFSET, 1, 1, 0, &_lpctype_string, 0, 0, false, "to_string", NULL },
  /* 295 */ { F_EFUN1, F_TRACE-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 26, 8, false, "trace", NULL },
  /* 296 */ { F_EFUN1, F_TRACEPREFIX-EFUN1_OFFSET, 1, 1, 0, &_lpctype_string, 50, 31, false, "traceprefix", NULL },
  /* 297 */ { F_EFUN1, F_TRANSPOSE_ARRAY-EFUN1_OFFSET, 1, 1, 0, &_lpctype_any_array, 41, 34, false, "transpose_array", NULL },
  /* 298 */ { F_EFUN1, F_UNMKMAPPING-EFUN1_OFFSET, 1, 1, 0, &_lpctype_any_array, 229, 86, false, "unmkmapping", NULL },
  /* 299 */ { F_EFUN1, F_UNQUOTE-EFUN1_OFFSET, 1, 1, 0, &_lpctype_mixed, 86, 29, false, "unquote", NULL },
  /* 300 */ { F_EFUN1, F_UPPER_CASE-EFUN1_OFFSET, 1, 1, 0, &_lpctype_string, 10, 4, false, "upper_case", NULL },
  /* 301 */ { F_EFUN1, F_WIDTHOF-EFUN1_OFFSET, 1, 1, 0, &_lpctype_int, 229, 103, false, "widthof", NULL },
  /* 302 */ { F_EFUN1, F_WRITE-EFUN1_OFFSET, 1, 1, 0, &_lpctype_void, 0, 0, false, "write", NULL },

  /* --- 2-arg efuns --- */

  /* 303 */ { F_EFUN2, F_AND_BITS-EFUN2_OFFSET, 2, 2, 0, &_lpctype_string, 73, 37, false, "and_bits", NULL },
  /* 304 */ { F_EFUN2, F_ATAN2-EFUN2_OFFSET, 2, 2, 0, &_lpctype_float, 28, 10, false, "atan2", NULL },
  /* 305 */ { F_EFUN2, F_ATTACH_ERQ_DEMON-EFUN2_OFFSET, 2, 2, F_CONST0, &_lpctype_int, 337, 126, false, "attach_erq_demon", NULL },
  /* 306 */ { F_EFUN2, F_BASEOF-EFUN2_OFFSET, 2, 2, 0, &_lpctype_int, 61, 22, false, "baseof", NULL },
  /* 307 */ { F_EFUN2, F_BINARY_MESSAGE-EFUN2_OFFSET, 2, 2, F_CONST0, &_lpctype_int, 126, 46, false, "binary_message", NULL },
  /* 308 */ { F_EFUN2, F_CLEAR_BIT-EFUN2_OFFSET, 2, 2, 0, &_lpctype_string, 120, 7, false, "clear_bit", NULL },
  /* 309 */ { F_EFUN2, F_CONFIGURE_DRIVER-EFUN2_OFFSET, 2, 2, 0, &_lpctype_void, 308, 113, false, "configure_driver", NULL },
  /* 310 */ { F_EFUN2, F_COPY_FILE-EFUN2_OFFSET, 2, 2, 0, &_lpctype_int, 73, 37, false, "copy_file", NULL },
  /* 311 */ { F_EFUN2, F_CRYPT-EFUN2_OFFSET, 2, 2, F_CONST0, &_lpctype_string, 115, 39, false, "crypt", NULL },
  /* 312 */ { F_EFUN2, F_DEBUG_MESSAGE-EFUN2_OFFSET, 2, 2, F_CONST0, &_lpctype_void, 120, 7, false, "debug_message", NULL },
  /* 313 */ { F_EFUN2, F_DUMP_DRIVER_INFO-EFUN2_OFFSET, 2, 2, F_CONST0, &_lpctype_int, 71, 147, false, "dump_driver_info", NULL },
  /* 314 */ { F_EFUN2, F_EXEC-EFUN2_OFFSET, 2, 2, 0, &_lpctype_int, 333, 124, false, "exec", NULL },
  /* 315 */ { F_EFUN2, F_EXPAND_DEFINE-EFUN2_OFFSET, 2, 2, F_CONST0, &_lpctype_string, 73, 24, false, "expand_define", NULL },
  /* 316 */ { F_EFUN2, F_EXPLODE-EFUN2_OFFSET, 2, 2, 0, &_lpctype_string_array, 73, 37, false, "explode", NULL },
  /* 317 */ { F_EFUN2, F_FUNCTIONLIST-EFUN2_OFFSET, 2, 2, F_CONST1, &_lpctype_any_array, 337, 126, false, "functionlist", NULL },
  /* 318 */ { F_EFUN2, F_GET_DIR-EFUN2_OFFSET, 2, 2, F_CONST1, &_lpctype_any_array, 120, 7, false, "get_dir", NULL },
  /* 319 */ { F_EFUN2, F_GET_ERROR_FILE-EFUN2_OFFSET, 2, 2, F_CONST1, &_lpctype_any_array, 120, 7, false, "get_error_file", NULL },
  /* 320 */ { F_EFUN2, F_IMPLODE-EFUN2_OFFSET, 2, 2, 0, &_lpctype_string, 21, 44, false, "implode", NULL },
  /* 321 */ { F_EFUN2, F_INTERACTIVE_INFO-EFUN2_OFFSET, 2, 2, 0, &_lpctype_mixed, 323, 121, false, "interactive_info", NULL },
  /* 322 */ { F_EFUN2, F_LAMBDA-EFUN2_OFFSET, 2, 2, 0, &_lpctype_closure, 291, 107, false, "lambda", NULL },
  /* 323 */ { F_EFUN2, F_LAST_INSTRUCTIONS-EFUN2_OFFSET, 2, 2, F_CONST1, &_lpctype_string_array, 93, 32, false, "last_instructions", NULL },
  /* 324 */ { F_EFUN2, F_M_ALLOCATE-EFUN2_OFFSET, 2, 2, F_CONST1, &_lpctype_mapping, 93, 32, false, "m_allocate", NULL },
  /* 325 */ { F_EFUN2, F_M_DELETE-EFUN2_OFFSET, 2, 2, 0, &_lpctype_mapping, 255, 92, false, "m_delete", NULL },
  /* 326 */ { F_EFUN2, F_M_ENTRY-EFUN2_OFFSET, 2, 2, 0, &_lpctype_any_array, 255, 92, false, "m_entry", NULL },
  /* 327 */ { F_EFUN2, F_M_REALLOCATE-EFUN2_OFFSET, 2, 2, 0, &_lpctype_mapping, 260, 94, false, "m_reallocate", NULL },
  /* 328 */ { F_EFUN2, F_M_VALUES-EFUN2_OFFSET, 2, 2, F_CONST0, &_lpctype_any_array, 260, 94, false, "m_values", NULL },
  /* 329 */ { F_EFUN2, F_MATCH_COMMAND-EFUN2_OFFSET, 2, 2, 0, &_lpctype_any_array, 369, 138, false, "match_command", NULL },
  /* 330 */ { F_EFUN2, F_MD5_CRYPT-EFUN2_OFFSET, 2, 2, F_CONST0, &_lpctype_string, 115, 39, false, "md5_crypt", NULL },
  /* 331 */ { F_EFUN2, F_MOVE_OBJECT-EFUN2_OFFSET, 2, 2, 0, &_lpctype_void, 101, 151, false, "move_object", NULL },
  /* 332 */ { F_EFUN2, F_NET_CONNECT-EFUN2_OFFSET, 2, 2, 0, &_lpctype_int, 120, 7, false, "net_connect", NULL },
  /* 333 */ { F_EFUN2, F_OBJECT_INFO-EFUN2_OFFSET, 2, 2, 0, &_lpctype_mixed, 323, 121, false, "object_info", NULL },
  /* 334 */ { F_EFUN2, F_OR_BITS-EFUN2_OFFSET, 2, 2, 0, &_lpctype_string, 73, 37, false, "or_bits", NULL },
  /* 335 */ { F_EFUN2, F_POW-EFUN2_OFFSET, 2, 2, 0, &_lpctype_float, 28, 10, false, "pow", NULL },
  /* 336 */ { F_EFUN2, F_QUERY_ACTIONS-EFUN2_OFFSET, 2, 2, F_CONST1, &_lpctype_any_array, 447, 118, false, "query_actions", NULL },
  /* 337 */ { F_EFUN2, F_REMOVE_ACTION-EFUN2_OFFSET, 2, 2, F_THIS_PLAYER, &_lpctype_int, 454, 168, false, "remove_action", NULL },
  /* 338 */ { F_EFUN2, F_RENAME-EFUN2_OFFSET, 2, 2, 0, &_lpctype_int, 73, 37, false, "rename", NULL },
  /* 339 */ { F_EFUN2, F_RENAME_OBJECT-EFUN2_OFFSET, 2, 2, 0, &_lpctype_void, 371, 139, false, "rename_object", NULL },
  /* 340 */ { F_EFUN2, F_SET_BIT-EFUN2_OFFSET, 2, 2, 0, &_lpctype_string, 120, 7, false, "set_bit", NULL },
  /* 341 */ { F_EFUN2, F_SET_DRIVER_HOOK-EFUN2_OFFSET, 2, 2, 0, &_lpctype_void, 398, 149, false, "set_driver_hook", NULL },
  /* 342 */ { F_EFUN2, F_SET_ENVIRONMENT-EFUN2_OFFSET, 2, 2, 0, &_lpctype_void, 333, 157, false, "set_environment", NULL },
  /* 343 */ { F_EFUN2, F_SET_EXTRA_WIZINFO-EFUN2_OFFSET, 2, 2, 0, &_lpctype_void, 104, 35, false, "set_extra_wizinfo", NULL },
  /* 344 */ { F_EFUN2, F_STRUCT_INFO-EFUN2_OFFSET, 2, 2, 0, &_lpctype_any_array, 69, 20, false, "struct_info", NULL },
  /* 345 */ { F_EFUN2, F_SYMBOL_FUNCTION-EFUN2_OFFSET, 2, 2, F_CONST0, &_lpctype_closure, 295, 109, false, "symbol_function", NULL },
  /* 346 */ { F_EFUN2, F_TELL_OBJECT-EFUN2_OFFSET, 2, 2, 0, &_lpctype_void, 375, 141, false, "tell_object", NULL },
  /* 347 */ { F_EFUN2, F_TEST_BIT-EFUN2_OFFSET, 2, 2, 0, &_lpctype_int, 120, 7, false, "test_bit", NULL },
  /* 348 */ { F_EFUN2, F_TRANSFER-EFUN2_OFFSET, 2, 2, 0, &_lpctype_int, 335, 125, false, "transfer", "replace by a simul-efun" },
  /* 349 */ { F_EFUN2, F_UNBOUND_LAMBDA-EFUN2_OFFSET, 2, 2, 0, &_lpctype_closure, 291, 107, false, "unbound_lambda", NULL },
  /* 350 */ { F_EFUN2, F_VARIABLE_LIST-EFUN2_OFFSET, 2, 2, F_CONST1, &_lpctype_any_array, 337, 126, false, "variable_list", NULL },
  /* 351 */ { F_EFUN2, F_XOR_BITS-EFUN2_OFFSET, 2, 2, 0, &_lpctype_string, 73, 37, false, "xor_bits", NULL },

  /* --- 3-arg efuns --- */

  /* 352 */ { F_EFUN3, F_CONFIGURE_INTERACTIVE-EFUN3_OFFSET, 3, 3, 0, &_lpctype_void, 327, 121, false, "configure_interactive", NULL },
  /* 353 */ { F_EFUN3, F_CONFIGURE_OBJECT-EFUN3_OFFSET, 3, 3, 0, &_lpctype_void, 327, 121, false, "configure_object", NULL },
  /* 354 */ { F_EFUN3, F_CONVERT_CHARSET-EFUN3_OFFSET, 3, 3, 0, &_lpctype_string, 109, 37, false, "convert_charset", NULL },
  /* 355 */ { F_EFUN3, F_EXECUTE_COMMAND-EFUN3_OFFSET, 3, 3, 0, &_lpctype_int, 441, 165, false, "execute_command", NULL },
  /* 356 */ { F_EFUN3, F_HMAC-EFUN3_OFFSET, 3, 3, 0, &_lpctype_string, 138, 49, false, "hmac", NULL },
  /* 357 */ { F_EFUN3, F_IDNA_STRINGPREP-EFUN3_OFFSET, 3, 3, F_CONST0, &_lpctype_string, 120, 41, false, "idna_stringprep", NULL },
  /* 358 */ { F_EFUN3, F_NEXT_BIT-EFUN3_OFFSET, 3, 3, F_CONST0, &_lpctype_int, 120, 41, false, "next_bit", NULL },
  /* 359 */ { F_EFUN3, F_REGEXP-EFUN3_OFFSET, 3, 3, F_CONST0, &_lpctype_string_array, 145, 52, false, "regexp", NULL },
  /* 360 */ { F_EFUN3, F_REGEXPLODE-EFUN3_OFFSET, 3, 3, F_CONST0, &_lpctype_string_array, 151, 55, false, "regexplode", NULL },
  /* 361 */ { F_EFUN3, F_SEND_ERQ-EFUN3_OFFSET, 3, 3, F_CONST0, &_lpctype_int, 391, 144, false, "send_erq", NULL },
  /* 362 */ { F_EFUN3, F_SEND_UDP-EFUN3_OFFSET, 3, 3, 0, &_lpctype_int, 384, 143, false, "send_udp", NULL },
  /* 363 */ { F_EFUN3, F_STRRSTR-EFUN3_OFFSET, 3, 3, F_NCONST1, &_lpctype_int, 151, 55, false, "strrstr", NULL },
  /* 364 */ { F_EFUN3, F_STRSTR-EFUN3_OFFSET, 3, 3, F_CONST0, &_lpctype_int, 151, 55, false, "strstr", NULL },
  /* 365 */ { F_EFUN3, F_WRITE_BYTES-EFUN3_OFFSET, 3, 3, 0, &_lpctype_int, 147, 53, false, "write_bytes", NULL },
  /* 366 */ { F_EFUN3, F_WRITE_FILE-EFUN3_OFFSET, 3, 3, F_CONST0, &_lpctype_int, 151, 55, false, "write_file", NULL },

  /* --- 4-arg efuns --- */

  /* 367 */ { F_EFUN4, F_REGREPLACE-EFUN4_OFFSET, 4, 4, 0, &_lpctype_string, 157, 58, false, "regreplace", NULL },

  /* --- vararg efuns --- */

  /* 368 */ { F_EFUNV, F_ADD_ACTION-EFUNV_OFFSET, 3, 2, 0, &_lpctype_void, 434, 162, false, "add_action", NULL },
  /* 369 */ { F_EFUNV, F_ALL_ENVIRONMENT-EFUNV_OFFSET, 1, 0, 0, &_lpctype_object_array, 289, 121, false, "all_environment", NULL },
  /* 370 */ { F_EFUNV, F_ALLOCATE-EFUNV_OFFSET, 2, 1, 0, &_lpctype_any_array, 200, 78, false, "allocate", NULL },
  /* 371 */ { F_EFUNV, F_APPLY-EFUNV_OFFSET, -1, 1, 0, &_lpctype_mixed, 283, 104, true, "apply", NULL },
  /* 372 */ { F_EFUNV, F_BIND_LAMBDA-EFUNV_OFFSET, 2, 1, 0, &_lpctype_closure, 287, 105, false, "bind_lambda", NULL },
  /* 373 */ { F_EFUNV, F_CALL_DIRECT_RESOLVED-EFUNV_OFFSET, -1, 3, 0, &_lpctype_int, 313, 115, false, "call_direct_resolved", NULL },
  /* 374 */ { F_EFUNV, F_CALL_OUT-EFUNV_OFFSET, -1, 2, 0, &_lpctype_void, 305, 112, false, "call_out", NULL },
  /* 375 */ { F_EFUNV, F_CALL_RESOLVED-EFUNV_OFFSET, -1, 3, 0, &_lpctype_int, 313, 115, false, "call_resolved", NULL },
  /* 376 */ { F_EFUNV, F_CLONES-EFUNV_OFFSET, 2, 0, 0, &_lpctype_object_array, 321, 119, false, "clones", NULL },
  /* 377 */ { F_EFUNV, F_COMMAND-EFUNV_OFFSET, 2, 1, 0, &_lpctype_int, 369, 138, false, "command", NULL },
  /* 378 */ { F_EFUNV, F_COPY_BITS-EFUNV_OFFSET, 5, 2, 0, &_lpctype_string, 190, 73, false, "copy_bits", NULL },
  /* 379 */ { F_EFUNV, F_DEEP_INVENTORY-EFUNV_OFFSET, 2, 0, 0, &_lpctype_object_array, 323, 131, false, "deep_inventory", NULL },
  /* 380 */ { F_EFUNV, F_ED-EFUNV_OFFSET, 2, 0, 0, &_lpctype_int, 73, 24, false, "ed", NULL },
  /* 381 */ { F_EFUNV, F_ENVIRONMENT-EFUNV_OFFSET, 1, 0, 0, &_lpctype_object, 101, 118, false, "environment", NULL },
  /* 382 */ { F_EFUNV, F_FILTER-EFUNV_OFFSET, -1, 2, 0, &_lpctype_mixed, 223, 84, false, "filter", NULL },
  /* 383 */ { F_EFUNV, F_FILTER_INDICES-EFUNV_OFFSET, -1, 2, 0, &_lpctype_mapping, 232, 86, false, "filter_indices", NULL },
  /* 384 */ { F_EFUNV, F_FILTER_OBJECTS-EFUNV_OFFSET, -1, 2, 0, &_lpctype_any_array, 205, 44, false, "filter_objects", NULL },
  /* 385 */ { F_EFUNV, F_FIND_INPUT_TO-EFUNV_OFFSET, 3, 2, 0, &_lpctype_int, 363, 136, false, "find_input_to", NULL },
  /* 386 */ { F_EFUNV, F_FUNCALL-EFUNV_OFFSET, -1, 1, 0, &_lpctype_mixed, 283, 104, true, "funcall", NULL },
  /* 387 */ { F_EFUNV, F_FUNCTION_EXISTS-EFUNV_OFFSET, 3, 1, 0, &_lpctype_mixed, 342, 128, false, "function_exists", NULL },
  /* 388 */ { F_EFUNV, F_GARBAGE_COLLECTION-EFUNV_OFFSET, 2, 0, 0, &_lpctype_void, 120, 7, false, "garbage_collection", NULL },
  /* 389 */ { F_EFUNV, F_GET_TYPE_INFO-EFUNV_OFFSET, 2, 1, 0, &_lpctype_mixed, 77, 26, false, "get_type_info", NULL },
  /* 390 */ { F_EFUNV, F_HASH-EFUNV_OFFSET, 3, 2, 0, &_lpctype_string, 131, 47, false, "hash", NULL },
  /* 391 */ { F_EFUNV, F_INCLUDE_LIST-EFUNV_OFFSET, 2, 0, 0, &_lpctype_string_array, 323, 131, false, "include_list", NULL },
  /* 392 */ { F_EFUNV, F_INHERIT_LIST-EFUNV_OFFSET, 2, 0, 0, &_lpctype_string_array, 323, 131, false, "inherit_list", NULL },
  /* 393 */ { F_EFUNV, F_INPUT_TO-EFUNV_OFFSET, -1, 1, 0, &_lpctype_int, 349, 60, false, "input_to", NULL },
  /* 394 */ { F_EFUNV, F_LIMITED-EFUNV_OFFSET, -1, 1, 0, &_lpctype_mixed, 214, 105, true, "limited", NULL },
  /* 395 */ { F_EFUNV, F_M_ADD-EFUNV_OFFSET, -1, 2, 0, &_lpctype_mapping, 255, 92, false, "m_add", NULL },
  /* 396 */ { F_EFUNV, F_M_CONTAINS-EFUNV_OFFSET, -1, 2, 0, &_lpctype_int, 248, 90, false, "m_contains", NULL },
  /* 397 */ { F_EFUNV, F_MAP-EFUNV_OFFSET, -1, 2, 0, &_lpctype_mixed, 238, 88, false, "map", NULL },
  /* 398 */ { F_EFUNV, F_MAP_INDICES-EFUNV_OFFSET, -1, 2, 0, &_lpctype_mapping, 232, 86, false, "map_indices", NULL },
  /* 399 */ { F_EFUNV, F_MAP_OBJECTS-EFUNV_OFFSET, -1, 2, 0, &_lpctype_any_array, 205, 44, false, "map_objects", NULL },
  /* 400 */ { F_EFUNV, F_MAX-EFUNV_OFFSET, -1, 1, 0, &_lpctype_mixed, 40, 13, false, "max", NULL },
  /* 401 */ { F_EFUNV, F_MD5-EFUNV_OFFSET, 2, 1, 0, &_lpctype_string, 126, 46, false, "md5", "obsoleted by hash" },
  /* 402 */ { F_EFUNV, F_MEMBER-EFUNV_OFFSET, 3, 2, 0, &_lpctype_int, 268, 97, false, "member", NULL },
  /* 403 */ { F_EFUNV, F_MIN-EFUNV_OFFSET, -1, 1, 0, &_lpctype_mixed, 40, 13, false, "min", NULL },
  /* 404 */ { F_EFUNV, F_MKMAPPING-EFUNV_OFFSET, -1, 1, 0, &_lpctype_mapping, 264, 96, false, "mkmapping", NULL },
  /* 405 */ { F_EFUNV, F_OBJECTS-EFUNV_OFFSET, -1, 0, 0, &_lpctype_object_array, 1, 0, false, "objects", NULL },
  /* 406 */ { F_EFUNV, F_PRESENT-EFUNV_OFFSET, 3, 1, 0, &_lpctype_object, 406, 152, false, "present", NULL },
  /* 407 */ { F_EFUNV, F_PRESENT_CLONE-EFUNV_OFFSET, 3, 1, 0, &_lpctype_object, 355, 133, false, "present_clone", NULL },
  /* 408 */ { F_EFUNV, F_PRINTF-EFUNV_OFFSET, -1, 1, 0, &_lpctype_void, 23, 4, false, "printf", NULL },
  /* 409 */ { F_EFUNV, F_READ_BYTES-EFUNV_OFFSET, 3, 1, 0, &_lpctype_string, 120, 41, false, "read_bytes", NULL },
  /* 410 */ { F_EFUNV, F_READ_FILE-EFUNV_OFFSET, 3, 1, 0, &_lpctype_string, 120, 41, false, "read_file", NULL },
  /* 411 */ { F_EFUNV, F_REGMATCH-EFUNV_OFFSET, 4, 2, 0, &_lpctype_mixed, 166, 62, false, "regmatch", NULL },
  /* 412 */ { F_EFUNV, F_REMOVE_INPUT_TO-EFUNV_OFFSET, 3, 1, 0, &_lpctype_int, 363, 136, false, "remove_input_to", NULL },
  /* 413 */ { F_EFUNV, F_REPLACE_PROGRAM-EFUNV_OFFSET, 1, 0, 0, &_lpctype_void, 10, 4, false, "replace_program", NULL },
  /* 414 */ { F_EFUNV, F_RMEMBER-EFUNV_OFFSET, 3, 2, 0, &_lpctype_int, 276, 100, false, "rmember", NULL },
  /* 415 */ { F_EFUNV, F_SAVE_OBJECT-EFUNV_OFFSET, 2, 0, 0, &_lpctype_mixed, 92, 31, false, "save_object", NULL },
  /* 416 */ { F_EFUNV, F_SAVE_VALUE-EFUNV_OFFSET, 2, 1, 0, &_lpctype_string, 77, 26, false, "save_value", NULL },
  /* 417 */ { F_EFUNV, F_SAY-EFUNV_OFFSET, 2, 1, 0, &_lpctype_void, 414, 155, false, "say", NULL },
  /* 418 */ { F_EFUNV, F_SHA1-EFUNV_OFFSET, 2, 1, 0, &_lpctype_string, 126, 46, false, "sha1", "obsoleted by hash" },
  /* 419 */ { F_EFUNV, F_SNOOP-EFUNV_OFFSET, 2, 1, 0, &_lpctype_int, 333, 124, false, "snoop", NULL },
  /* 420 */ { F_EFUNV, F_SORT_ARRAY-EFUNV_OFFSET, -1, 2, 0, &_lpctype_any_array, 210, 80, false, "sort_array", NULL },
  /* 421 */ { F_EFUNV, F_SPRINTF-EFUNV_OFFSET, -1, 1, 0, &_lpctype_string, 23, 4, false, "sprintf", NULL },
  /* 422 */ { F_EFUNV, F_STRFTIME-EFUNV_OFFSET, 3, 0, 0, &_lpctype_string, 92, 31, false, "strftime", NULL },
  /* 423 */ { F_EFUNV, F_TELL_ROOM-EFUNV_OFFSET, 3, 2, 0, &_lpctype_void, 423, 159, false, "tell_room", NULL },
  /* 424 */ { F_EFUNV, F_TERMINAL_COLOUR-EFUNV_OFFSET, 4, 2, 0, &_lpctype_string, 174, 66, false, "terminal_colour", NULL },
  /* 425 */ { F_EFUNV, F_TO_STRUCT-EFUNV_OFFSET, 2, 1, 0, &_lpctype_mixed, 59, 17, false, "to_struct", NULL },
  /* 426 */ { F_EFUNV, F_TRIM-EFUNV_OFFSET, 3, 1, 0, &_lpctype_string, 183, 70, false, "trim", NULL },
  /* 427 */ { F_EFUNV, F_UNIQUE_ARRAY-EFUNV_OFFSET, -1, 2, 0, &_lpctype_any_array, 217, 82, false, "unique_array", NULL },
  /* 428 */ { F_EFUNV, F_VARIABLE_EXISTS-EFUNV_OFFSET, 3, 1, 0, &_lpctype_string, 342, 128, false, "variable_exists", NULL },
  /* 429 */ { F_EFUNV, F_WALK_MAPPING-EFUNV_OFFSET, -1, 2, 0, &_lpctype_void, 232, 86, false, "walk_mapping", NULL },

  /* --- aliased efuns --- */

  /* 430 */ { 0, 0, 1, 1, 0, &_lpctype_string, 289, 106, false, "creator", NULL },
};


/* Aliased efuns.
 * Index it with <code>-F_LAST_INSTRUCTION-1 to retrieve
 * the real instruction code.
 */

short efun_aliases[] = {
    F_GETUID,
};


/* Table of function argument signatures (compiler).
 * The internal structure is that of arg_types[] in make_func.
 */

fulltype_t efun_arg_types[] = {
    /*   0 */ { &_lpctype_mixed, 0 }, { NULL, 0 },
    /*   2 */ { &_lpctype_mixed, TYPE_MOD_REFERENCE }, { NULL, 0 },
    /*   4 */ { &_lpctype_mixed, 0 }, { &_lpctype_mixed, TYPE_MOD_REFERENCE }, { NULL, 0 },
    /*   7 */ { &_lpctype_int, 0 }, { &_lpctype_float, 0 }, { NULL, 0 },
    /*  10 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /*  12 */ { &_lpctype_string, 0 }, { &_lpctype_mixed, 0 }, { &_lpctype_any_array, 0 }, { &_lpctype_mapping, 0 }, { &_lpctype_any_struct, 0 }, { NULL, 0 },
    /*  18 */ { &_lpctype_object, 0 }, { &_lpctype_string, 0 }, { &_lpctype_object_array, 0 }, { &_lpctype_string_array, 0 }, { NULL, 0 },
    /*  23 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /*  25 */ { NULL, 0 },
    /*  26 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /*  28 */ { &_lpctype_int, 0 }, { &_lpctype_float, 0 }, { NULL, 0 },
    /*  31 */ { &_lpctype_int, 0 }, { &_lpctype_float, 0 }, { NULL, 0 },
    /*  34 */ { &_lpctype_int, 0 }, { &_lpctype_string, 0 }, { &_lpctype_string, TYPE_MOD_REFERENCE }, { &_lpctype_any_array, 0 }, { &_lpctype_any_array, TYPE_MOD_REFERENCE }, { NULL, 0 },
    /*  40 */ { &_lpctype_mixed, 0 }, { &_lpctype_any_array, 0 }, { NULL, 0 },
    /*  43 */ { NULL, 0 },
    /*  44 */ { &_lpctype_int, 0 }, { &_lpctype_string, 0 }, { &_lpctype_float, 0 }, { &_lpctype_closure, 0 }, { NULL, 0 },
    /*  49 */ { &_lpctype_float, 0 }, { &_lpctype_string, 0 }, { &_lpctype_int, 0 }, { NULL, 0 },
    /*  53 */ { &_lpctype_string, 0 }, { &_lpctype_any_array, 0 }, { &_lpctype_symbol, 0 }, { &_lpctype_quoted_array, 0 }, { &_lpctype_any_struct, 0 }, { NULL, 0 },
    /*  59 */ { &_lpctype_mapping, 0 }, { &_lpctype_any_array, 0 }, { &_lpctype_any_struct, 0 }, { NULL, 0 },
    /*  63 */ { &_lpctype_any_struct, 0 }, { NULL, 0 },
    /*  65 */ { &_lpctype_object, 0 }, { &_lpctype_string, 0 }, { &_lpctype_closure, 0 }, { NULL, 0 },
    /*  69 */ { &_lpctype_any_struct, 0 }, { NULL, 0 },
    /*  71 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /*  73 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /*  75 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /*  77 */ { &_lpctype_mixed, 0 }, { NULL, 0 },
    /*  79 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /*  81 */ { &_lpctype_any_array, 0 }, { &_lpctype_quoted_array, 0 }, { &_lpctype_symbol, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /*  86 */ { &_lpctype_quoted_array, 0 }, { &_lpctype_symbol, 0 }, { NULL, 0 },
    /*  89 */ { &_lpctype_int_array, 0 }, { &_lpctype_int, 0 }, { NULL, 0 },
    /*  92 */ { &_lpctype_string, 0 }, { &_lpctype_int, 0 }, { NULL, 0 },
    /*  95 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /*  97 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /*  99 */ { &_lpctype_int_array, 0 }, { NULL, 0 },
    /* 101 */ { &_lpctype_object, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 104 */ { &_lpctype_object, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 107 */ { &_lpctype_mixed, 0 }, { NULL, 0 },
    /* 109 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 111 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 113 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 115 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 117 */ { &_lpctype_string, 0 }, { &_lpctype_int, 0 }, { NULL, 0 },
    /* 120 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 122 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 124 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 126 */ { &_lpctype_int_array, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 129 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 131 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 133 */ { &_lpctype_string, 0 }, { &_lpctype_int_array, 0 }, { NULL, 0 },
    /* 136 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 138 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 140 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 142 */ { &_lpctype_string, 0 }, { &_lpctype_int_array, 0 }, { NULL, 0 },
    /* 145 */ { &_lpctype_string_array, 0 }, { NULL, 0 },
    /* 147 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 149 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 151 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 153 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 155 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 157 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 159 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 161 */ { &_lpctype_closure, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 164 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 166 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 168 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 170 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 172 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 174 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 176 */ { &_lpctype_mapping, 0 }, { &_lpctype_closure, 0 }, { NULL, 0 },
    /* 179 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 181 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 183 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 185 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 187 */ { &_lpctype_int, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 190 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 192 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 194 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 196 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 198 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 200 */ { &_lpctype_int, 0 }, { &_lpctype_int_array, 0 }, { NULL, 0 },
    /* 203 */ { &_lpctype_mixed, 0 }, { NULL, 0 },
    /* 205 */ { &_lpctype_any_array, 0 }, { NULL, 0 },
    /* 207 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 209 */ { NULL, 0 },
    /* 210 */ { &_lpctype_any_array, 0 }, { &_lpctype_any_array, TYPE_MOD_REFERENCE }, { NULL, 0 },
    /* 213 */ { &_lpctype_string, 0 }, { &_lpctype_closure, 0 }, { NULL, 0 },
    /* 216 */ { NULL, 0 },
    /* 217 */ { &_lpctype_any_array, 0 }, { NULL, 0 },
    /* 219 */ { &_lpctype_string, 0 }, { &_lpctype_closure, 0 }, { NULL, 0 },
    /* 222 */ { NULL, 0 },
    /* 223 */ { &_lpctype_mapping, 0 }, { &_lpctype_any_array, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 227 */ { &_lpctype_string, 0 }, { &_lpctype_closure, 0 }, { &_lpctype_mapping, 0 }, { NULL, 0 },
    /* 231 */ { NULL, 0 },
    /* 232 */ { &_lpctype_mapping, 0 }, { NULL, 0 },
    /* 234 */ { &_lpctype_string, 0 }, { &_lpctype_closure, 0 }, { NULL, 0 },
    /* 237 */ { NULL, 0 },
    /* 238 */ { &_lpctype_any_array, 0 }, { &_lpctype_mapping, 0 }, { &_lpctype_any_struct, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 243 */ { &_lpctype_mapping, 0 }, { &_lpctype_string, 0 }, { &_lpctype_closure, 0 }, { NULL, 0 },
    /* 247 */ { NULL, 0 },
    /* 248 */ { &_lpctype_mixed, TYPE_MOD_REFERENCE }, { &_lpctype_mapping, 0 }, { NULL, 0 },
    /* 251 */ { &_lpctype_mixed, TYPE_MOD_REFERENCE }, { &_lpctype_mixed, 0 }, { NULL, 0 },
    /* 254 */ { NULL, 0 },
    /* 255 */ { &_lpctype_mapping, 0 }, { NULL, 0 },
    /* 257 */ { &_lpctype_mixed, 0 }, { NULL, 0 },
    /* 259 */ { NULL, 0 },
    /* 260 */ { &_lpctype_mapping, 0 }, { NULL, 0 },
    /* 262 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 264 */ { &_lpctype_any_array, 0 }, { &_lpctype_any_struct, 0 }, { NULL, 0 },
    /* 267 */ { NULL, 0 },
    /* 268 */ { &_lpctype_any_array, 0 }, { &_lpctype_string, 0 }, { &_lpctype_mapping, 0 }, { NULL, 0 },
    /* 272 */ { &_lpctype_mixed, 0 }, { NULL, 0 },
    /* 274 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 276 */ { &_lpctype_any_array, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 279 */ { &_lpctype_mixed, 0 }, { NULL, 0 },
    /* 281 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 283 */ { &_lpctype_mixed, 0 }, { &_lpctype_closure, 0 }, { NULL, 0 },
    /* 286 */ { NULL, 0 },
    /* 287 */ { &_lpctype_closure, 0 }, { NULL, 0 },
    /* 289 */ { &_lpctype_object, 0 }, { NULL, 0 },
    /* 291 */ { &_lpctype_any_array, 0 }, { NULL, 0 },
    /* 293 */ { &_lpctype_mixed, 0 }, { NULL, 0 },
    /* 295 */ { &_lpctype_symbol, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 298 */ { &_lpctype_string, 0 }, { &_lpctype_object, 0 }, { NULL, 0 },
    /* 301 */ { &_lpctype_symbol, 0 }, { &_lpctype_string, 0 }, { &_lpctype_int, 0 }, { NULL, 0 },
    /* 305 */ { &_lpctype_string, 0 }, { &_lpctype_closure, 0 }, { NULL, 0 },
    /* 308 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 310 */ { &_lpctype_mixed, 0 }, { NULL, 0 },
    /* 312 */ { NULL, 0 },
    /* 313 */ { &_lpctype_mixed, TYPE_MOD_REFERENCE }, { NULL, 0 },
    /* 315 */ { &_lpctype_object, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 318 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 320 */ { NULL, 0 },
    /* 321 */ { &_lpctype_int, 0 }, { &_lpctype_string, 0 }, { &_lpctype_object, 0 }, { NULL, 0 },
    /* 325 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 327 */ { &_lpctype_object, 0 }, { NULL, 0 },
    /* 329 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 331 */ { &_lpctype_mixed, 0 }, { NULL, 0 },
    /* 333 */ { &_lpctype_object, 0 }, { NULL, 0 },
    /* 335 */ { &_lpctype_object, 0 }, { NULL, 0 },
    /* 337 */ { &_lpctype_object, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 340 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 342 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 344 */ { &_lpctype_int, 0 }, { &_lpctype_object, 0 }, { NULL, 0 },
    /* 347 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 349 */ { &_lpctype_string, 0 }, { &_lpctype_closure, 0 }, { NULL, 0 },
    /* 352 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 354 */ { NULL, 0 },
    /* 355 */ { &_lpctype_object, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 358 */ { &_lpctype_object, 0 }, { &_lpctype_int, 0 }, { NULL, 0 },
    /* 361 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 363 */ { &_lpctype_object, 0 }, { NULL, 0 },
    /* 365 */ { &_lpctype_object, 0 }, { &_lpctype_string, 0 }, { &_lpctype_closure, 0 }, { NULL, 0 },
    /* 369 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 371 */ { &_lpctype_object, 0 }, { NULL, 0 },
    /* 373 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 375 */ { &_lpctype_object, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 378 */ { &_lpctype_string, 0 }, { &_lpctype_any_array, 0 }, { &_lpctype_mapping, 0 }, { &_lpctype_any_struct, 0 }, { &_lpctype_object, 0 }, { NULL, 0 },
    /* 384 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 386 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 388 */ { &_lpctype_string, 0 }, { &_lpctype_int_array, 0 }, { NULL, 0 },
    /* 391 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 393 */ { &_lpctype_int_array, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 396 */ { &_lpctype_closure, 0 }, { NULL, 0 },
    /* 398 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 400 */ { &_lpctype_closure, 0 }, { &_lpctype_int, 0 }, { &_lpctype_string, 0 }, { &_lpctype_mapping, 0 }, { &_lpctype_string_array, 0 }, { NULL, 0 },
    /* 406 */ { &_lpctype_object, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 409 */ { &_lpctype_int, 0 }, { &_lpctype_object, 0 }, { NULL, 0 },
    /* 412 */ { &_lpctype_object, 0 }, { NULL, 0 },
    /* 414 */ { &_lpctype_string, 0 }, { &_lpctype_any_array, 0 }, { &_lpctype_object, 0 }, { &_lpctype_mapping, 0 }, { &_lpctype_any_struct, 0 }, { NULL, 0 },
    /* 420 */ { &_lpctype_object, 0 }, { &_lpctype_object_array, 0 }, { NULL, 0 },
    /* 423 */ { &_lpctype_object, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 426 */ { &_lpctype_string, 0 }, { &_lpctype_any_array, 0 }, { &_lpctype_object, 0 }, { &_lpctype_mapping, 0 }, { &_lpctype_any_struct, 0 }, { NULL, 0 },
    /* 432 */ { &_lpctype_object_array, 0 }, { NULL, 0 },
    /* 434 */ { &_lpctype_string, 0 }, { &_lpctype_closure, 0 }, { NULL, 0 },
    /* 437 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 439 */ { &_lpctype_int, 0 }, { NULL, 0 },
    /* 441 */ { &_lpctype_string, 0 }, { NULL, 0 },
    /* 443 */ { &_lpctype_object, 0 }, { NULL, 0 },
    /* 445 */ { &_lpctype_object, 0 }, { NULL, 0 },
    /* 447 */ { &_lpctype_object, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 450 */ { &_lpctype_int, 0 }, { &_lpctype_object, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 454 */ { &_lpctype_int, 0 }, { &_lpctype_string, 0 }, { NULL, 0 },
    /* 457 */ { &_lpctype_object, 0 }, { NULL, 0 },
};


/* Table of function argument signatures (runtime).
 * The internal structure is that of lpc_types[] in make_func.
 */

long efun_lpc_types[] = {
     /*   0 */ TF_ANYTYPE,
    /*   1 */ TF_LVALUE,
    /*   2 */ TF_ANYTYPE,
    /*   3 */ TF_NUMBER|TF_FLOAT,
    /*   4 */ TF_STRING,
    /*   5 */ TF_ANYTYPE,
    /*   6 */ TF_POINTER|TF_STRING|TF_OBJECT,
    /*   7 */ TF_STRING,
    /*   8 */ TF_NUMBER,
    /*   9 */ TF_FLOAT,
    /*  10 */ TF_NUMBER|TF_FLOAT,
    /*  11 */ TF_NUMBER|TF_FLOAT,
    /*  12 */ TF_LVALUE|TF_POINTER|TF_NUMBER|TF_STRING,
    /*  13 */ TF_ANYTYPE,
    /*  14 */ TF_NUMBER|TF_STRING|TF_FLOAT|TF_CLOSURE,
    /*  15 */ TF_NUMBER|TF_STRING|TF_FLOAT,
    /*  16 */ TF_POINTER|TF_STRING|TF_SYMBOL|TF_QUOTED_ARRAY|TF_STRUCT,
    /*  17 */ TF_POINTER|TF_MAPPING|TF_STRUCT,
    /*  18 */ TF_STRUCT,
    /*  19 */ TF_STRING|TF_OBJECT|TF_CLOSURE|TF_NULL,
    /*  20 */ TF_STRUCT,
    /*  21 */ TF_NUMBER,
    /*  22 */ TF_STRUCT,
    /*  23 */ TF_STRUCT,
    /*  24 */ TF_STRING,
    /*  25 */ TF_STRING|TF_NULL,
    /*  26 */ TF_ANYTYPE,
    /*  27 */ TF_NUMBER,
    /*  28 */ TF_POINTER|TF_STRING|TF_SYMBOL|TF_QUOTED_ARRAY,
    /*  29 */ TF_SYMBOL|TF_QUOTED_ARRAY,
    /*  30 */ TF_POINTER|TF_NUMBER,
    /*  31 */ TF_NUMBER|TF_STRING,
    /*  32 */ TF_NUMBER,
    /*  33 */ TF_NUMBER,
    /*  34 */ TF_POINTER,
    /*  35 */ TF_STRING|TF_OBJECT|TF_NULL,
    /*  36 */ TF_ANYTYPE,
    /*  37 */ TF_STRING,
    /*  38 */ TF_STRING,
    /*  39 */ TF_STRING,
    /*  40 */ TF_NUMBER|TF_STRING,
    /*  41 */ TF_STRING,
    /*  42 */ TF_NUMBER,
    /*  43 */ TF_NUMBER,
    /*  44 */ TF_POINTER,
    /*  45 */ TF_STRING,
    /*  46 */ TF_POINTER|TF_STRING,
    /*  47 */ TF_NUMBER,
    /*  48 */ TF_POINTER|TF_STRING,
    /*  49 */ TF_NUMBER,
    /*  50 */ TF_STRING,
    /*  51 */ TF_POINTER|TF_STRING,
    /*  52 */ TF_POINTER,
    /*  53 */ TF_STRING,
    /*  54 */ TF_NUMBER,
    /*  55 */ TF_STRING,
    /*  56 */ TF_STRING,
    /*  57 */ TF_NUMBER,
    /*  58 */ TF_STRING,
    /*  59 */ TF_STRING,
    /*  60 */ TF_STRING|TF_CLOSURE,
    /*  61 */ TF_NUMBER,
    /*  62 */ TF_STRING,
    /*  63 */ TF_STRING,
    /*  64 */ TF_NUMBER,
    /*  65 */ TF_NUMBER,
    /*  66 */ TF_STRING,
    /*  67 */ TF_MAPPING|TF_CLOSURE|TF_NULL,
    /*  68 */ TF_NUMBER,
    /*  69 */ TF_NUMBER,
    /*  70 */ TF_STRING,
    /*  71 */ TF_NUMBER,
    /*  72 */ TF_NUMBER|TF_STRING,
    /*  73 */ TF_STRING,
    /*  74 */ TF_STRING,
    /*  75 */ TF_NUMBER,
    /*  76 */ TF_NUMBER,
    /*  77 */ TF_NUMBER,
    /*  78 */ TF_POINTER|TF_NUMBER,
    /*  79 */ TF_ANYTYPE,
    /*  80 */ TF_LVALUE|TF_POINTER,
    /*  81 */ TF_STRING|TF_CLOSURE,
    /*  82 */ TF_POINTER,
    /*  83 */ TF_STRING|TF_CLOSURE,
    /*  84 */ TF_POINTER|TF_STRING|TF_MAPPING,
    /*  85 */ TF_STRING|TF_MAPPING|TF_CLOSURE,
    /*  86 */ TF_MAPPING,
    /*  87 */ TF_STRING|TF_CLOSURE,
    /*  88 */ TF_POINTER|TF_STRING|TF_MAPPING|TF_STRUCT,
    /*  89 */ TF_STRING|TF_MAPPING|TF_CLOSURE,
    /*  90 */ TF_LVALUE|TF_MAPPING,
    /*  91 */ TF_ANYTYPE,
    /*  92 */ TF_MAPPING,
    /*  93 */ TF_ANYTYPE,
    /*  94 */ TF_MAPPING,
    /*  95 */ TF_NUMBER,
    /*  96 */ TF_POINTER|TF_STRUCT,
    /*  97 */ TF_POINTER|TF_STRING|TF_MAPPING,
    /*  98 */ TF_ANYTYPE,
    /*  99 */ TF_NUMBER,
    /* 100 */ TF_POINTER|TF_STRING,
    /* 101 */ TF_ANYTYPE,
    /* 102 */ TF_NUMBER,
    /* 103 */ TF_MAPPING|TF_NULL,
    /* 104 */ TF_ANYTYPE,
    /* 105 */ TF_CLOSURE,
    /* 106 */ TF_OBJECT,
    /* 107 */ TF_POINTER|TF_NULL,
    /* 108 */ TF_ANYTYPE,
    /* 109 */ TF_STRING|TF_SYMBOL,
    /* 110 */ TF_STRING|TF_OBJECT|TF_NULL,
    /* 111 */ TF_NUMBER|TF_STRING|TF_SYMBOL,
    /* 112 */ TF_STRING|TF_CLOSURE,
    /* 113 */ TF_NUMBER,
    /* 114 */ TF_ANYTYPE,
    /* 115 */ TF_LVALUE,
    /* 116 */ TF_STRING|TF_OBJECT|TF_NULL,
    /* 117 */ TF_STRING,
    /* 118 */ TF_STRING|TF_OBJECT,
    /* 119 */ TF_NUMBER|TF_STRING|TF_OBJECT,
    /* 120 */ TF_NUMBER,
    /* 121 */ TF_OBJECT|TF_NULL,
    /* 122 */ TF_NUMBER,
    /* 123 */ TF_ANYTYPE,
    /* 124 */ TF_OBJECT,
    /* 125 */ TF_OBJECT,
    /* 126 */ TF_STRING|TF_OBJECT,
    /* 127 */ TF_NUMBER,
    /* 128 */ TF_STRING,
    /* 129 */ TF_NUMBER|TF_OBJECT,
    /* 130 */ TF_NUMBER,
    /* 131 */ TF_OBJECT,
    /* 132 */ TF_NUMBER,
    /* 133 */ TF_STRING|TF_OBJECT,
    /* 134 */ TF_NUMBER|TF_OBJECT,
    /* 135 */ TF_NUMBER,
    /* 136 */ TF_OBJECT,
    /* 137 */ TF_STRING|TF_OBJECT|TF_CLOSURE,
    /* 138 */ TF_STRING,
    /* 139 */ TF_OBJECT,
    /* 140 */ TF_STRING,
    /* 141 */ TF_STRING|TF_OBJECT,
    /* 142 */ TF_POINTER|TF_STRING|TF_OBJECT|TF_MAPPING|TF_STRUCT,
    /* 143 */ TF_STRING,
    /* 144 */ TF_NUMBER,
    /* 145 */ TF_POINTER|TF_STRING,
    /* 146 */ TF_CLOSURE|TF_NULL,
    /* 147 */ TF_NUMBER,
    /* 148 */ TF_STRING|TF_NULL,
    /* 149 */ TF_NUMBER,
    /* 150 */ TF_POINTER|TF_NUMBER|TF_STRING|TF_MAPPING|TF_CLOSURE,
    /* 151 */ TF_STRING|TF_OBJECT,
    /* 152 */ TF_STRING|TF_OBJECT,
    /* 153 */ TF_NUMBER|TF_OBJECT,
    /* 154 */ TF_OBJECT,
    /* 155 */ TF_POINTER|TF_STRING|TF_OBJECT|TF_MAPPING|TF_STRUCT,
    /* 156 */ TF_POINTER|TF_OBJECT,
    /* 157 */ TF_OBJECT,
    /* 158 */ TF_OBJECT|TF_NULL,
    /* 159 */ TF_STRING|TF_OBJECT,
    /* 160 */ TF_POINTER|TF_STRING|TF_OBJECT|TF_MAPPING|TF_STRUCT,
    /* 161 */ TF_POINTER,
    /* 162 */ TF_STRING|TF_CLOSURE,
    /* 163 */ TF_STRING,
    /* 164 */ TF_NUMBER,
    /* 165 */ TF_STRING,
    /* 166 */ TF_OBJECT,
    /* 167 */ TF_OBJECT,
    /* 168 */ TF_NUMBER|TF_STRING,
    /* 169 */ TF_OBJECT,
};


/* Prototypes of the tabled efuns
 */

extern svalue_t *f_call_out_info(svalue_t *);
extern svalue_t *f_caller_stack_depth(svalue_t *);
extern svalue_t *f_command_stack(svalue_t *);
extern svalue_t *f_command_stack_depth(svalue_t *);
extern svalue_t *f_get_eval_cost(svalue_t *);
extern svalue_t *f_heart_beat_info(svalue_t *);
extern svalue_t *f_query_command(svalue_t *);
extern svalue_t *f_regexp_package(svalue_t *);
extern svalue_t *f_rusage(svalue_t *);
extern svalue_t *f_time(svalue_t *);
extern svalue_t *f_unshadow(svalue_t *);
extern svalue_t *f_users(svalue_t *);
extern svalue_t *f_utime(svalue_t *);
extern svalue_t *f_wizlist_info(svalue_t *);


extern svalue_t *f_abs(svalue_t *);
extern svalue_t *f_acos(svalue_t *);
extern svalue_t *f_all_inventory(svalue_t *);
extern svalue_t *f_asin(svalue_t *);
extern svalue_t *f_atan(svalue_t *);
extern svalue_t *f_blueprint(svalue_t *);
extern svalue_t *f_caller_stack(svalue_t *);
extern svalue_t *f_capitalize(svalue_t *);
extern svalue_t *f_ceil(svalue_t *);
extern svalue_t *f_clone_object(svalue_t *);
extern svalue_t *f_copy(svalue_t *);
extern svalue_t *f_cos(svalue_t *);
extern svalue_t *f_count_bits(svalue_t *);
extern svalue_t *f_ctime(svalue_t *);
extern svalue_t *f_deep_copy(svalue_t *);
extern svalue_t *f_destruct(svalue_t *);
extern svalue_t *f_driver_info(svalue_t *);
extern svalue_t *f_exp(svalue_t *);
extern svalue_t *f_file_size(svalue_t *);
extern svalue_t *f_find_call_out(svalue_t *);
extern svalue_t *f_find_object(svalue_t *);
extern svalue_t *f_first_inventory(svalue_t *);
extern svalue_t *f_floor(svalue_t *);
extern svalue_t *f_get_extra_wizinfo(svalue_t *);
extern svalue_t *f_geteuid(svalue_t *);
extern svalue_t *f_getuid(svalue_t *);
extern svalue_t *f_gmtime(svalue_t *);
extern svalue_t *f_idna_to_ascii(svalue_t *);
extern svalue_t *f_idna_to_unicode(svalue_t *);
extern svalue_t *f_input_to_info(svalue_t *);
extern svalue_t *f_interactive(svalue_t *);
extern svalue_t *f_invert_bits(svalue_t *);
extern svalue_t *f_last_bit(svalue_t *);
extern svalue_t *f_living(svalue_t *);
extern svalue_t *f_load_name(svalue_t *);
extern svalue_t *f_load_object(svalue_t *);
extern svalue_t *f_localtime(svalue_t *);
extern svalue_t *f_log(svalue_t *);
extern svalue_t *f_lower_case(svalue_t *);
extern svalue_t *f_m_indices(svalue_t *);
extern svalue_t *f_make_shared_string(svalue_t *);
extern svalue_t *f_mkdir(svalue_t *);
extern svalue_t *f_mktime(svalue_t *);
extern svalue_t *f_next_inventory(svalue_t *);
extern svalue_t *f_notify_fail(svalue_t *);
extern svalue_t *f_object_name(svalue_t *);
extern svalue_t *f_object_time(svalue_t *);
extern svalue_t *f_previous_object(svalue_t *);
extern svalue_t *f_process_string(svalue_t *);
extern svalue_t *f_program_name(svalue_t *);
extern svalue_t *f_program_time(svalue_t *);
extern svalue_t *f_query_notify_fail(svalue_t *);
extern svalue_t *f_query_verb(svalue_t *);
extern svalue_t *f_quote(svalue_t *);
extern svalue_t *f_random(svalue_t *);
extern svalue_t *f_remove_call_out(svalue_t *);
extern svalue_t *f_remove_interactive(svalue_t *);
extern svalue_t *f_restore_object(svalue_t *);
extern svalue_t *f_restore_value(svalue_t *);
extern svalue_t *f_reverse(svalue_t *);
extern svalue_t *f_rm(svalue_t *);
extern svalue_t *f_rmdir(svalue_t *);
extern svalue_t *f_set_next_reset(svalue_t *);
extern svalue_t *f_set_this_object(svalue_t *);
extern svalue_t *f_set_this_player(svalue_t *);
extern svalue_t *f_sgn(svalue_t *);
extern svalue_t *f_shadow(svalue_t *);
extern svalue_t *f_shutdown(svalue_t *);
extern svalue_t *f_sin(svalue_t *);
extern svalue_t *f_sqrt(svalue_t *);
extern svalue_t *f_symbol_variable(svalue_t *);
extern svalue_t *f_tan(svalue_t *);
extern svalue_t *f_to_array(svalue_t *);
extern svalue_t *f_to_float(svalue_t *);
extern svalue_t *f_to_int(svalue_t *);
extern svalue_t *f_to_object(svalue_t *);
extern svalue_t *f_to_string(svalue_t *);
extern svalue_t *f_trace(svalue_t *);
extern svalue_t *f_traceprefix(svalue_t *);
extern svalue_t *f_transpose_array(svalue_t *);
extern svalue_t *f_unmkmapping(svalue_t *);
extern svalue_t *f_unquote(svalue_t *);
extern svalue_t *f_upper_case(svalue_t *);
extern svalue_t *f_widthof(svalue_t *);
extern svalue_t *f_write(svalue_t *);


extern svalue_t *f_and_bits(svalue_t *);
extern svalue_t *f_atan2(svalue_t *);
extern svalue_t *f_attach_erq_demon(svalue_t *);
extern svalue_t *f_baseof(svalue_t *);
extern svalue_t *f_binary_message(svalue_t *);
extern svalue_t *f_clear_bit(svalue_t *);
extern svalue_t *f_configure_driver(svalue_t *);
extern svalue_t *f_copy_file(svalue_t *);
extern svalue_t *f_crypt(svalue_t *);
extern svalue_t *f_debug_message(svalue_t *);
extern svalue_t *f_dump_driver_info(svalue_t *);
extern svalue_t *f_exec(svalue_t *);
extern svalue_t *f_expand_define(svalue_t *);
extern svalue_t *f_explode(svalue_t *);
extern svalue_t *f_functionlist(svalue_t *);
extern svalue_t *f_get_dir(svalue_t *);
extern svalue_t *f_get_error_file(svalue_t *);
extern svalue_t *f_implode(svalue_t *);
extern svalue_t *f_interactive_info(svalue_t *);
extern svalue_t *f_lambda(svalue_t *);
extern svalue_t *f_last_instructions(svalue_t *);
extern svalue_t *f_m_allocate(svalue_t *);
extern svalue_t *f_m_delete(svalue_t *);
extern svalue_t *f_m_entry(svalue_t *);
extern svalue_t *f_m_reallocate(svalue_t *);
extern svalue_t *f_m_values(svalue_t *);
extern svalue_t *f_match_command(svalue_t *);
extern svalue_t *f_md5_crypt(svalue_t *);
extern svalue_t *f_move_object(svalue_t *);
extern svalue_t *f_net_connect(svalue_t *);
extern svalue_t *f_object_info(svalue_t *);
extern svalue_t *f_or_bits(svalue_t *);
extern svalue_t *f_pow(svalue_t *);
extern svalue_t *f_query_actions(svalue_t *);
extern svalue_t *f_remove_action(svalue_t *);
extern svalue_t *f_rename(svalue_t *);
extern svalue_t *f_rename_object(svalue_t *);
extern svalue_t *f_set_bit(svalue_t *);
extern svalue_t *f_set_driver_hook(svalue_t *);
extern svalue_t *f_set_environment(svalue_t *);
extern svalue_t *f_set_extra_wizinfo(svalue_t *);
extern svalue_t *f_struct_info(svalue_t *);
extern svalue_t *f_symbol_function(svalue_t *);
extern svalue_t *f_tell_object(svalue_t *);
extern svalue_t *f_test_bit(svalue_t *);
extern svalue_t *f_transfer(svalue_t *);
extern svalue_t *f_unbound_lambda(svalue_t *);
extern svalue_t *f_variable_list(svalue_t *);
extern svalue_t *f_xor_bits(svalue_t *);


extern svalue_t *f_configure_interactive(svalue_t *);
extern svalue_t *f_configure_object(svalue_t *);
extern svalue_t *f_convert_charset(svalue_t *);
extern svalue_t *f_execute_command(svalue_t *);
extern svalue_t *f_hmac(svalue_t *);
extern svalue_t *f_idna_stringprep(svalue_t *);
extern svalue_t *f_next_bit(svalue_t *);
extern svalue_t *f_regexp(svalue_t *);
extern svalue_t *f_regexplode(svalue_t *);
extern svalue_t *f_send_erq(svalue_t *);
extern svalue_t *f_send_udp(svalue_t *);
extern svalue_t *f_strrstr(svalue_t *);
extern svalue_t *f_strstr(svalue_t *);
extern svalue_t *f_write_bytes(svalue_t *);
extern svalue_t *f_write_file(svalue_t *);


extern svalue_t *f_regreplace(svalue_t *);


/* Prototypes of the tabled vararg efuns
 */

extern svalue_t *v_add_action(svalue_t *, int);
extern svalue_t *v_all_environment(svalue_t *, int);
extern svalue_t *v_allocate(svalue_t *, int);
extern svalue_t *v_apply(svalue_t *, int);
extern svalue_t *v_bind_lambda(svalue_t *, int);
extern svalue_t *v_call_direct_resolved(svalue_t *, int);
extern svalue_t *v_call_out(svalue_t *, int);
extern svalue_t *v_call_resolved(svalue_t *, int);
extern svalue_t *v_clones(svalue_t *, int);
extern svalue_t *v_command(svalue_t *, int);
extern svalue_t *v_copy_bits(svalue_t *, int);
extern svalue_t *v_deep_inventory(svalue_t *, int);
extern svalue_t *v_ed(svalue_t *, int);
extern svalue_t *v_environment(svalue_t *, int);
extern svalue_t *v_filter(svalue_t *, int);
extern svalue_t *v_filter_indices(svalue_t *, int);
extern svalue_t *v_filter_objects(svalue_t *, int);
extern svalue_t *v_find_input_to(svalue_t *, int);
extern svalue_t *v_funcall(svalue_t *, int);
extern svalue_t *v_function_exists(svalue_t *, int);
extern svalue_t *v_garbage_collection(svalue_t *, int);
extern svalue_t *v_get_type_info(svalue_t *, int);
extern svalue_t *v_hash(svalue_t *, int);
extern svalue_t *v_include_list(svalue_t *, int);
extern svalue_t *v_inherit_list(svalue_t *, int);
extern svalue_t *v_input_to(svalue_t *, int);
extern svalue_t *v_limited(svalue_t *, int);
extern svalue_t *v_m_add(svalue_t *, int);
extern svalue_t *v_m_contains(svalue_t *, int);
extern svalue_t *v_map(svalue_t *, int);
extern svalue_t *v_map_indices(svalue_t *, int);
extern svalue_t *v_map_objects(svalue_t *, int);
extern svalue_t *v_max(svalue_t *, int);
extern svalue_t *v_md5(svalue_t *, int);
extern svalue_t *v_member(svalue_t *, int);
extern svalue_t *v_min(svalue_t *, int);
extern svalue_t *v_mkmapping(svalue_t *, int);
extern svalue_t *v_objects(svalue_t *, int);
extern svalue_t *v_present(svalue_t *, int);
extern svalue_t *v_present_clone(svalue_t *, int);
extern svalue_t *v_printf(svalue_t *, int);
extern svalue_t *v_read_bytes(svalue_t *, int);
extern svalue_t *v_read_file(svalue_t *, int);
extern svalue_t *v_regmatch(svalue_t *, int);
extern svalue_t *v_remove_input_to(svalue_t *, int);
extern svalue_t *v_replace_program(svalue_t *, int);
extern svalue_t *v_rmember(svalue_t *, int);
extern svalue_t *v_save_object(svalue_t *, int);
extern svalue_t *v_save_value(svalue_t *, int);
extern svalue_t *v_say(svalue_t *, int);
extern svalue_t *v_sha1(svalue_t *, int);
extern svalue_t *v_snoop(svalue_t *, int);
extern svalue_t *v_sort_array(svalue_t *, int);
extern svalue_t *v_sprintf(svalue_t *, int);
extern svalue_t *v_strftime(svalue_t *, int);
extern svalue_t *v_tell_room(svalue_t *, int);
extern svalue_t *v_terminal_colour(svalue_t *, int);
extern svalue_t *v_to_struct(svalue_t *, int);
extern svalue_t *v_trim(svalue_t *, int);
extern svalue_t *v_unique_array(svalue_t *, int);
extern svalue_t *v_variable_exists(svalue_t *, int);
extern svalue_t *v_walk_mapping(svalue_t *, int);


/* The table of tabled efuns
 */

svalue_t *(*efun_table[]) (svalue_t *) = {
    /* 204 */ f_call_out_info,
    /* 205 */ f_caller_stack_depth,
    /* 206 */ f_command_stack,
    /* 207 */ f_command_stack_depth,
    /* 208 */ f_get_eval_cost,
    /* 209 */ f_heart_beat_info,
    /* 210 */ f_query_command,
    /* 211 */ f_regexp_package,
    /* 212 */ f_rusage,
    /* 213 */ f_time,
    /* 214 */ f_unshadow,
    /* 215 */ f_users,
    /* 216 */ f_utime,
    /* 217 */ f_wizlist_info,
    /* 218 */ f_abs,
    /* 219 */ f_acos,
    /* 220 */ f_all_inventory,
    /* 221 */ f_asin,
    /* 222 */ f_atan,
    /* 223 */ f_blueprint,
    /* 224 */ f_caller_stack,
    /* 225 */ f_capitalize,
    /* 226 */ f_ceil,
    /* 227 */ f_clone_object,
    /* 228 */ f_copy,
    /* 229 */ f_cos,
    /* 230 */ f_count_bits,
    /* 231 */ f_ctime,
    /* 232 */ f_deep_copy,
    /* 233 */ f_destruct,
    /* 234 */ f_driver_info,
    /* 235 */ f_exp,
    /* 236 */ f_file_size,
    /* 237 */ f_find_call_out,
    /* 238 */ f_find_object,
    /* 239 */ f_first_inventory,
    /* 240 */ f_floor,
    /* 241 */ f_get_extra_wizinfo,
    /* 242 */ f_geteuid,
    /* 243 */ f_getuid,
    /* 244 */ f_gmtime,
    /* 245 */ f_idna_to_ascii,
    /* 246 */ f_idna_to_unicode,
    /* 247 */ f_input_to_info,
    /* 248 */ f_interactive,
    /* 249 */ f_invert_bits,
    /* 250 */ f_last_bit,
    /* 251 */ f_living,
    /* 252 */ f_load_name,
    /* 253 */ f_load_object,
    /* 254 */ f_localtime,
    /* 255 */ f_log,
    /* 256 */ f_lower_case,
    /* 257 */ f_m_indices,
    /* 258 */ f_make_shared_string,
    /* 259 */ f_mkdir,
    /* 260 */ f_mktime,
    /* 261 */ f_next_inventory,
    /* 262 */ f_notify_fail,
    /* 263 */ f_object_name,
    /* 264 */ f_object_time,
    /* 265 */ f_previous_object,
    /* 266 */ f_process_string,
    /* 267 */ f_program_name,
    /* 268 */ f_program_time,
    /* 269 */ f_query_notify_fail,
    /* 270 */ f_query_verb,
    /* 271 */ f_quote,
    /* 272 */ f_random,
    /* 273 */ f_remove_call_out,
    /* 274 */ f_remove_interactive,
    /* 275 */ f_restore_object,
    /* 276 */ f_restore_value,
    /* 277 */ f_reverse,
    /* 278 */ f_rm,
    /* 279 */ f_rmdir,
    /* 280 */ f_set_next_reset,
    /* 281 */ f_set_this_object,
    /* 282 */ f_set_this_player,
    /* 283 */ f_sgn,
    /* 284 */ f_shadow,
    /* 285 */ f_shutdown,
    /* 286 */ f_sin,
    /* 287 */ f_sqrt,
    /* 288 */ f_symbol_variable,
    /* 289 */ f_tan,
    /* 290 */ f_to_array,
    /* 291 */ f_to_float,
    /* 292 */ f_to_int,
    /* 293 */ f_to_object,
    /* 294 */ f_to_string,
    /* 295 */ f_trace,
    /* 296 */ f_traceprefix,
    /* 297 */ f_transpose_array,
    /* 298 */ f_unmkmapping,
    /* 299 */ f_unquote,
    /* 300 */ f_upper_case,
    /* 301 */ f_widthof,
    /* 302 */ f_write,
    /* 303 */ f_and_bits,
    /* 304 */ f_atan2,
    /* 305 */ f_attach_erq_demon,
    /* 306 */ f_baseof,
    /* 307 */ f_binary_message,
    /* 308 */ f_clear_bit,
    /* 309 */ f_configure_driver,
    /* 310 */ f_copy_file,
    /* 311 */ f_crypt,
    /* 312 */ f_debug_message,
    /* 313 */ f_dump_driver_info,
    /* 314 */ f_exec,
    /* 315 */ f_expand_define,
    /* 316 */ f_explode,
    /* 317 */ f_functionlist,
    /* 318 */ f_get_dir,
    /* 319 */ f_get_error_file,
    /* 320 */ f_implode,
    /* 321 */ f_interactive_info,
    /* 322 */ f_lambda,
    /* 323 */ f_last_instructions,
    /* 324 */ f_m_allocate,
    /* 325 */ f_m_delete,
    /* 326 */ f_m_entry,
    /* 327 */ f_m_reallocate,
    /* 328 */ f_m_values,
    /* 329 */ f_match_command,
    /* 330 */ f_md5_crypt,
    /* 331 */ f_move_object,
    /* 332 */ f_net_connect,
    /* 333 */ f_object_info,
    /* 334 */ f_or_bits,
    /* 335 */ f_pow,
    /* 336 */ f_query_actions,
    /* 337 */ f_remove_action,
    /* 338 */ f_rename,
    /* 339 */ f_rename_object,
    /* 340 */ f_set_bit,
    /* 341 */ f_set_driver_hook,
    /* 342 */ f_set_environment,
    /* 343 */ f_set_extra_wizinfo,
    /* 344 */ f_struct_info,
    /* 345 */ f_symbol_function,
    /* 346 */ f_tell_object,
    /* 347 */ f_test_bit,
    /* 348 */ f_transfer,
    /* 349 */ f_unbound_lambda,
    /* 350 */ f_variable_list,
    /* 351 */ f_xor_bits,
    /* 352 */ f_configure_interactive,
    /* 353 */ f_configure_object,
    /* 354 */ f_convert_charset,
    /* 355 */ f_execute_command,
    /* 356 */ f_hmac,
    /* 357 */ f_idna_stringprep,
    /* 358 */ f_next_bit,
    /* 359 */ f_regexp,
    /* 360 */ f_regexplode,
    /* 361 */ f_send_erq,
    /* 362 */ f_send_udp,
    /* 363 */ f_strrstr,
    /* 364 */ f_strstr,
    /* 365 */ f_write_bytes,
    /* 366 */ f_write_file,
    /* 367 */ f_regreplace,
};


/* The table of tabled vararg efuns
 */

svalue_t *(*vefun_table[]) (svalue_t *, int) = {
    /* 368 */ v_add_action,
    /* 369 */ v_all_environment,
    /* 370 */ v_allocate,
    /* 371 */ v_apply,
    /* 372 */ v_bind_lambda,
    /* 373 */ v_call_direct_resolved,
    /* 374 */ v_call_out,
    /* 375 */ v_call_resolved,
    /* 376 */ v_clones,
    /* 377 */ v_command,
    /* 378 */ v_copy_bits,
    /* 379 */ v_deep_inventory,
    /* 380 */ v_ed,
    /* 381 */ v_environment,
    /* 382 */ v_filter,
    /* 383 */ v_filter_indices,
    /* 384 */ v_filter_objects,
    /* 385 */ v_find_input_to,
    /* 386 */ v_funcall,
    /* 387 */ v_function_exists,
    /* 388 */ v_garbage_collection,
    /* 389 */ v_get_type_info,
    /* 390 */ v_hash,
    /* 391 */ v_include_list,
    /* 392 */ v_inherit_list,
    /* 393 */ v_input_to,
    /* 394 */ v_limited,
    /* 395 */ v_m_add,
    /* 396 */ v_m_contains,
    /* 397 */ v_map,
    /* 398 */ v_map_indices,
    /* 399 */ v_map_objects,
    /* 400 */ v_max,
    /* 401 */ v_md5,
    /* 402 */ v_member,
    /* 403 */ v_min,
    /* 404 */ v_mkmapping,
    /* 405 */ v_objects,
    /* 406 */ v_present,
    /* 407 */ v_present_clone,
    /* 408 */ v_printf,
    /* 409 */ v_read_bytes,
    /* 410 */ v_read_file,
    /* 411 */ v_regmatch,
    /* 412 */ v_remove_input_to,
    /* 413 */ v_replace_program,
    /* 414 */ v_rmember,
    /* 415 */ v_save_object,
    /* 416 */ v_save_value,
    /* 417 */ v_say,
    /* 418 */ v_sha1,
    /* 419 */ v_snoop,
    /* 420 */ v_sort_array,
    /* 421 */ v_sprintf,
    /* 422 */ v_strftime,
    /* 423 */ v_tell_room,
    /* 424 */ v_terminal_colour,
    /* 425 */ v_to_struct,
    /* 426 */ v_trim,
    /* 427 */ v_unique_array,
    /* 428 */ v_variable_exists,
    /* 429 */ v_walk_mapping,
};


/*----------------------------------------------------------------------*/

/* Our own ctype implementation. This way we can be sure that
 *   (a) we won't choke on non-ASCII characters
 *   (b) we are fast
 *   (c) we get the non-standard classifications we need anyway
 */

#define lexwhite(c) (_my_ctype[(unsigned char)(c)]&16)
#define leXdigit(c) (_my_ctype[(unsigned char)(c)]&64)

unsigned char _my_ctype[] = {
    0,0,0,0,0,0,0,1,1,17,5,17,17,17,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    16,0,1,0,0,0,0,0,0,0,0,0,4,0,0,0,
    194,194,194,194,194,194,194,194,194,194,4,4,0,0,0,0,
    0,192,192,192,192,192,192,128,128,128,128,128,128,128,128,128,
    128,128,128,128,128,128,128,128,128,128,128,0,1,0,0,128,
    0,192,192,192,192,192,192,128,128,128,128,128,128,128,128,128,
    128,128,128,128,128,128,128,128,128,128,128,0,4,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,
    128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,
    128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,
    128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,
};

/************************************************************************/

#endif /* EFUN_DEFS_C__ */
