/* DO NOT EDIT!
 *
 * This file is created automatically by make_func from
 * the specifications in string_spec.
 *
 * It's purpose is to define and initialize the standard shared string
 * table shstring[].
 */

#include "driver.h"
#include "typedefs.h"

#include "stdstrings.h"
#include "mstrings.h"

/*-------------------------------------------------------------------------*/

string_t *shstring[SHSTR_NOSTRINGS];
  /* Table of common used and therefore shared strings.
   */

/*-------------------------------------------------------------------------*/
void
init_standard_strings (void)

/* Initialize the common string table.
 */

{
#   define INIT(x,s) shstring[x] = mstring_new_tabled(s MTRACE_ARG);

    INIT(SHX_DEFAULT, "This string is used as a substitute if allocating another one failed.");
    INIT(SHX_EMPTY, "");
    INIT(SHX_ANONYMOUS, "anonymous");
    INIT(SHX_ATAT, "@@");
    INIT(SHX_CALL_OTHER, "call_other");
    INIT(SHX_CATCH, "CATCH");
    INIT(SHX_DANGLING_LAMBDA, "Dangling function call in lambda closure");
    INIT(SHX_CRPATTERN, "\015$");
    INIT(SHX_DANGLING_V_CL, "dangling var closure");
    INIT(SHX_DEFAULT_PROMPT, "> ");
    INIT(SHX_DESTRUCTED, "destructed");
    INIT(SHX_DISCARDED_MSG, "\n*** Text lost in transmission ***\n");
    INIT(SHX_EFUN, "efun");
    INIT(SHX_SEFUN, "sefun");
    INIT(SHX_EFUN_CLOSURE, "<efun closure>");
    INIT(SHX_FATAL_ERROR, "Fatal Error");
    INIT(SHX_FROM, " from ");
    INIT(SHX_FUNCTIONS, "functions");
    INIT(SHX_HEART_BEAT, "heart_beat");
    INIT(SHX_IN, "in");
    INIT(SHX_LINE, " line ");
    INIT(SHX_MEMORY, "memory");
    INIT(SHX_MEMDUMP, "memdump");
    INIT(SHX_MYSQL, "mysql");
    INIT(SHX_NEWLINE, "\n");
    INIT(SHX_NONAME, "NONAME");
    INIT(SHX_NO_PROG_TRACE, "No program to trace.");
    INIT(SHX_NO_TRACE, "No trace.");
    INIT(SHX_NOLOG, "nolog");
    INIT(SHX_PUBLISH, "publish");
    INIT(SHX_OBJDUMP_FNAME, "/OBJ_DUMP");
    INIT(SHX_DESTOBJDUMP_FNAME, "/DEST_OBJ_DUMP");
    INIT(SHX_OPCDUMP_FNAME, "/OPC_DUMP");
    INIT(SHX_MEMDUMP_FNAME, "/MEMORY_DUMP");
    INIT(SHX_OBJECTS, "objects");
    INIT(SHX_OPCODES, "opcodes");
    INIT(SHX_OUT_OF_MEMORY_CATCH, "catch() error: Out of memory.\n");
    INIT(SHX_OUT_OF_MEMORY, "(s)sprintf() error: Out of memory.\n");
    INIT(SHX_PERCENT, "%");
    INIT(SHX_PERCENT_S, "%s");
    INIT(SHX_PERIOD, ".");
    INIT(SHX_PGSQL, "pgsql");
    INIT(SHX_PROG_DEALLOCATED, "program deallocated");
    INIT(SHX_RESERVE, "reserve");
    INIT(SHX_SEFUN_CLOSURE, "<simul_efun closure>");
    INIT(SHX_SLABALLOC, "slaballoc");
    INIT(SHX_SMALLOC, "smalloc");
    INIT(SHX_SLASH, "/");
    INIT(SHX_SPACE, " ");
    INIT(SHX_SYSTEM_MALLOC, "system malloc");
    INIT(SHX_TRACE, "trace");
    INIT(SHX_TRACEPREFIX, "traceprefix");
    INIT(SHX_UNDEFINED, "UNDEFINED");
    INIT(SHX_UNKNOWN_LAMBDA, "<lambda ?>");
    INIT(SHX_UNKNOWN_OBJECT, "<object ?>");
    INIT(SHX_VARIABLES, "variables");
    INIT(SHX_ATTACH_ERQ_DEMON, "attach_erq_demon");
    INIT(SHX_BIND_LAMBDA, "bind_lambda");
    INIT(SHX_CALL_OUT_INFO, "call_out_info");
    INIT(SHX_CONFIGURE_INTERACTIVE, "configure_interactive");
    INIT(SHX_CONFIGURE_DRIVER, "configure_driver");
    INIT(SHX_CONFIGURE_OBJECT, "configure_object");
    INIT(SHX_SEND_ERQ, "erq");
    INIT(SHX_INPUT_TO, "input_to");
    INIT(SHX_SEND_UDP, "send_udp");
    INIT(SHX_ENABLE_TELNET, "enable_telnet");
    INIT(SHX_EXECUTE_COMMAND, "execute_command");
    INIT(SHX_GET_EXTRA_WIZINFO, "get_extra_wizinfo");
    INIT(SHX_LIMITED, "limited");
    INIT(SHX_NET_CONNECT, "net_connect");
    INIT(SHX_NOMASK_SIMUL_EFUN, "nomask simul_efun");
    INIT(SHX_RENAME_OBJECT, "rename_object");
    INIT(SHX_SET_AUTO_INCLUDE, "set_auto_include_string");
    INIT(SHX_SET_DRIVER_HOOK, "set_driver_hook");
    INIT(SHX_SET_EXTRA_WIZINFO, "set_extra_wizinfo");
    INIT(SHX_SET_EXTRA_WIZINFO_SIZE, "set_extra_wizinfo_size");
    INIT(SHX_SET_LIMITS, "set_limits");
    INIT(SHX_SET_THIS_OBJECT, "set_this_object");
    INIT(SHX_SHADOW_ADD_ACTION, "shadow_add_action");
    INIT(SHX_SYMBOL_VARIABLE, "symbol_variable");
    INIT(SHX_VARIABLE_LIST, "variable_list");
    INIT(SHX_WIZLIST_INFO, "wizlist_info");
    INIT(SHX_COPY_FILE, "copy_file");
    INIT(SHX_ED_START, "ed_start");
    INIT(SHX_FILE_SIZE, "file_size");
    INIT(SHX_GARBAGE_COLLECTION, "garbage_collection");
    INIT(SHX_GET_DIR, "get_dir");
    INIT(SHX_MKDIR, "mkdir");
    INIT(SHX_OBJDUMP, "objdump");
    INIT(SHX_OPCDUMP, "opcdump");
    INIT(SHX_PRINT_FILE, "print_file");
    INIT(SHX_READ_BYTES, "read_bytes");
    INIT(SHX_READ_FILE, "read_file");
    INIT(SHX_REMOVE_FILE, "remove_file");
    INIT(SHX_RENAME_FROM, "rename_from");
    INIT(SHX_RENAME_TO, "rename_to");
    INIT(SHX_RESTORE_OBJECT, "restore_object");
    INIT(SHX_RMDIR, "rmdir");
    INIT(SHX_SAVE_OBJECT, "save_object");
    INIT(SHX_TAIL, "tail");
    INIT(SHX_WRITE_BYTES, "write_bytes");
    INIT(SHX_WRITE_FILE, "write_file");
    INIT(SHX_ED_PROMPT, ":");
    INIT(SHX_ED_APPEND_PROMPT, "*\b");
    INIT(SHX_CATCH_TELL, "catch_tell");
    INIT(SHX_CATCH_MSG, "catch_msg");
    INIT(SHX_ID, "id");
    INIT(SHX_VARINIT, "__INIT");
    INIT(SHX_ABS_PATH, "make_path_absolute");
    INIT(SHX_COMPILE_OBJECT, "compile_object");
    INIT(SHX_CONNECT, "connect");
    INIT(SHX_DANGLING_LFUN, "dangling_lfun_closure");
    INIT(SHX_DISCONNECT, "disconnect");
    INIT(SHX_EPILOG, "epilog");
    INIT(SHX_EXT_RELOAD, "external_master_reload");
    INIT(SHX_FLAG, "flag");
    INIT(SHX_GET_BB_UID, "get_bb_uid");
    INIT(SHX_GET_ED_FNAME, "get_ed_buffer_save_file_name");
    INIT(SHX_GET_M_UID, "get_master_uid");
    INIT(SHX_GET_SEFUN, "get_simul_efun");
    INIT(SHX_GET_WNAME, "get_wiz_name");
    INIT(SHX_HANDLE_EXTERNAL_SIGNAL, "handle_external_signal");
    INIT(SHX_HEART_ERROR, "heart_beat_error");
    INIT(SHX_INAUGURATE, "inaugurate_master");
    INIT(SHX_INCLUDE_FILE, "include_file");
    INIT(SHX_INHERIT_FILE, "inherit_file");
    INIT(SHX_LOG_ERROR, "log_error");
    INIT(SHX_LOGON, "logon");
    INIT(SHX_LOW_MEMORY, "low_memory");
    INIT(SHX_PRELOAD, "preload");
    INIT(SHX_PREP_DEST, "prepare_destruct");
    INIT(SHX_PRINTF_OBJ_NAME, "printf_obj_name");
    INIT(SHX_PRIVILEGE, "privilege_violation");
    INIT(SHX_QUERY_SHADOW, "query_allow_shadow");
    INIT(SHX_QUOTA_DEMON, "quota_demon");
    INIT(SHX_RETR_ED, "retrieve_ed_setup");
    INIT(SHX_REACTIVATE, "reactivate_destructed_master");
    INIT(SHX_RECEIVE_UDP, "receive_udp");
    INIT(SHX_REMOVE_PL, "remove_player");
    INIT(SHX_RUNTIME, "runtime_error");
    INIT(SHX_SAVE_ED, "save_ed_setup");
    INIT(SHX_SET_MAX_CMDS, "set_max_commands");
    INIT(SHX_NOTIFY_SHUTDOWN, "notify_shutdown");
    INIT(SHX_SHUTDOWN, "shutdown");
    INIT(SHX_SLOW_SHUT, "slow_shut_down");
    INIT(SHX_STALE_ERQ, "stale_erq");
    INIT(SHX_VALID_EXEC, "valid_exec");
    INIT(SHX_VALID_QSNOOP, "valid_query_snoop");
    INIT(SHX_VALID_READ, "valid_read");
    INIT(SHX_VALID_SNOOP, "valid_snoop");
    INIT(SHX_VALID_TRACE, "valid_trace");
    INIT(SHX_VALID_WRITE, "valid_write");
    INIT(SHX_WARNING, "runtime_warning");
    INIT(SHX_ADD_WEIGHT, "add_weight");
    INIT(SHX_CANPUTGET, "can_put_and_get");
    INIT(SHX_DROP, "drop");
    INIT(SHX_GET, "get");
    INIT(SHX_QUERY_WEIGHT, "query_weight");
    INIT(SHX_PREVENT_INSERT, "prevent_insert");
    INIT(SHX_PC_FIND_LIVING, "find_living");
    INIT(SHX_PC_FIND_PLAYER, "find_player");
    INIT(SHX_PC_ID_LIST, "parse_command_id_list");
    INIT(SHX_PC_P_ID_LIST, "parse_command_plural_id_list");
    INIT(SHX_PC_ADJ_LIST, "parse_command_adjectiv_id_list");
    INIT(SHX_PC_PREPOS, "parse_command_prepos_list");
    INIT(SHX_PC_ALLWORD, "parse_command_all_word");
    INIT(SHX_PC_PLURAL_ID, "plural_id");
    INIT(SHX_PC_ADJ_ID, "adjectiv_id");
    INIT(SHX_PC_SHORT, "short");
    INIT(SHX_PC_OF, "of");
    INIT(SHX_PC_CORPSE, "corpse");
    INIT(SHX_PC_CORPSES, "corpses");
    INIT(SHX_PC_TOOTH, "tooth");
    INIT(SHX_PC_TEETH, "teeth");
    INIT(SHX_PC_FOOT, "foot");
    INIT(SHX_PC_FEET, "feet");
    INIT(SHX_PC_MAN, "man");
    INIT(SHX_PC_MEN, "men");
    INIT(SHX_PC_WOMAN, "woman");
    INIT(SHX_PC_WOMEN, "women");
    INIT(SHX_PC_CHILD, "child");
    INIT(SHX_PC_CHILDREN, "children");
    INIT(SHX_PC_SHEEP, "sheep");

#   undef INIT
} /* init_standard_strings() */

/************************************************************************/
