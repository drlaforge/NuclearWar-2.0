#ifndef STDSTRINGS_H__
#define STDSTRINGS_H__ 1

/* DO NOT EDIT!
 *
 * This file is created automatically by make_func from
 * the specifications in string_spec.
 */

#include "driver.h"
#include "typedefs.h"

/* --- Common used shared strings. --- */
/* The most common strings, including all the predefined applies,
 * are kept in shstrings[] for faster usage.
 * The indices are SHX_xxx, the defines STR_xxx expand to shstring[SHX_xxx]
 */

enum StandardStrings {
    SHX_DEFAULT      = 0 /* "This string is used as a substitute if allocating another one failed." */
  , SHX_EMPTY            /* "" */
  , SHX_ANONYMOUS        /* "anonymous" */
  , SHX_ATAT             /* "@@" */
  , SHX_CALL_OTHER       /* "call_other" */
  , SHX_CATCH            /* "CATCH" */
  , SHX_DANGLING_LAMBDA  /* "Dangling function call in lambda closure" */
  , SHX_CRPATTERN        /* "\015$" */
  , SHX_DANGLING_V_CL    /* "dangling var closure" */
  , SHX_DEFAULT_PROMPT   /* "> " */
  , SHX_DESTRUCTED       /* "destructed" */
  , SHX_DISCARDED_MSG    /* "\n*** Text lost in transmission ***\n" */
  , SHX_EFUN             /* "efun" */
  , SHX_SEFUN            /* "sefun" */
  , SHX_EFUN_CLOSURE     /* "<efun closure>" */
  , SHX_FATAL_ERROR      /* "Fatal Error" */
  , SHX_FROM             /* " from " */
  , SHX_FUNCTIONS        /* "functions" */
  , SHX_HEART_BEAT       /* "heart_beat" */
  , SHX_IN               /* "in" */
  , SHX_LINE             /* " line " */
  , SHX_MEMORY           /* "memory" */
  , SHX_MEMDUMP          /* "memdump" */
  , SHX_MYSQL            /* "mysql" */
  , SHX_NEWLINE          /* "\n" */
  , SHX_NONAME           /* "NONAME" */
  , SHX_NO_PROG_TRACE    /* "No program to trace." */
  , SHX_NO_TRACE         /* "No trace." */
  , SHX_NOLOG            /* "nolog" */
  , SHX_PUBLISH          /* "publish" */
  , SHX_OBJDUMP_FNAME    /* "/OBJ_DUMP" */
  , SHX_DESTOBJDUMP_FNAME /* "/DEST_OBJ_DUMP" */
  , SHX_OPCDUMP_FNAME    /* "/OPC_DUMP" */
  , SHX_MEMDUMP_FNAME    /* "/MEMORY_DUMP" */
  , SHX_OBJECTS          /* "objects" */
  , SHX_OPCODES          /* "opcodes" */
  , SHX_OUT_OF_MEMORY_CATCH /* "catch() error: Out of memory.\n" */
  , SHX_OUT_OF_MEMORY    /* "(s)sprintf() error: Out of memory.\n" */
  , SHX_PERCENT          /* "%" */
  , SHX_PERCENT_S        /* "%s" */
  , SHX_PERIOD           /* "." */
  , SHX_PGSQL            /* "pgsql" */
  , SHX_PROG_DEALLOCATED /* "program deallocated" */
  , SHX_RESERVE          /* "reserve" */
  , SHX_SEFUN_CLOSURE    /* "<simul_efun closure>" */
  , SHX_SLABALLOC        /* "slaballoc" */
  , SHX_SMALLOC          /* "smalloc" */
  , SHX_SLASH            /* "/" */
  , SHX_SPACE            /* " " */
  , SHX_SYSTEM_MALLOC    /* "system malloc" */
  , SHX_TRACE            /* "trace" */
  , SHX_TRACEPREFIX      /* "traceprefix" */
  , SHX_UNDEFINED        /* "UNDEFINED" */
  , SHX_UNKNOWN_LAMBDA   /* "<lambda ?>" */
  , SHX_UNKNOWN_OBJECT   /* "<object ?>" */
  , SHX_VARIABLES        /* "variables" */
  , SHX_ATTACH_ERQ_DEMON /* "attach_erq_demon" */
  , SHX_BIND_LAMBDA      /* "bind_lambda" */
  , SHX_CALL_OUT_INFO    /* "call_out_info" */
  , SHX_CONFIGURE_INTERACTIVE /* "configure_interactive" */
  , SHX_CONFIGURE_DRIVER /* "configure_driver" */
  , SHX_CONFIGURE_OBJECT /* "configure_object" */
  , SHX_SEND_ERQ         /* "erq" */
  , SHX_INPUT_TO         /* "input_to" */
  , SHX_SEND_UDP         /* "send_udp" */
  , SHX_ENABLE_TELNET    /* "enable_telnet" */
  , SHX_EXECUTE_COMMAND  /* "execute_command" */
  , SHX_GET_EXTRA_WIZINFO /* "get_extra_wizinfo" */
  , SHX_LIMITED          /* "limited" */
  , SHX_NET_CONNECT      /* "net_connect" */
  , SHX_NOMASK_SIMUL_EFUN /* "nomask simul_efun" */
  , SHX_RENAME_OBJECT    /* "rename_object" */
  , SHX_SET_AUTO_INCLUDE /* "set_auto_include_string" */
  , SHX_SET_DRIVER_HOOK  /* "set_driver_hook" */
  , SHX_SET_EXTRA_WIZINFO /* "set_extra_wizinfo" */
  , SHX_SET_EXTRA_WIZINFO_SIZE /* "set_extra_wizinfo_size" */
  , SHX_SET_LIMITS       /* "set_limits" */
  , SHX_SET_THIS_OBJECT  /* "set_this_object" */
  , SHX_SHADOW_ADD_ACTION /* "shadow_add_action" */
  , SHX_SYMBOL_VARIABLE  /* "symbol_variable" */
  , SHX_VARIABLE_LIST    /* "variable_list" */
  , SHX_WIZLIST_INFO     /* "wizlist_info" */
  , SHX_COPY_FILE        /* "copy_file" */
  , SHX_ED_START         /* "ed_start" */
  , SHX_FILE_SIZE        /* "file_size" */
  , SHX_GARBAGE_COLLECTION /* "garbage_collection" */
  , SHX_GET_DIR          /* "get_dir" */
  , SHX_MKDIR            /* "mkdir" */
  , SHX_OBJDUMP          /* "objdump" */
  , SHX_OPCDUMP          /* "opcdump" */
  , SHX_PRINT_FILE       /* "print_file" */
  , SHX_READ_BYTES       /* "read_bytes" */
  , SHX_READ_FILE        /* "read_file" */
  , SHX_REMOVE_FILE      /* "remove_file" */
  , SHX_RENAME_FROM      /* "rename_from" */
  , SHX_RENAME_TO        /* "rename_to" */
  , SHX_RESTORE_OBJECT   /* "restore_object" */
  , SHX_RMDIR            /* "rmdir" */
  , SHX_SAVE_OBJECT      /* "save_object" */
  , SHX_TAIL             /* "tail" */
  , SHX_WRITE_BYTES      /* "write_bytes" */
  , SHX_WRITE_FILE       /* "write_file" */
  , SHX_ED_PROMPT        /* ":" */
  , SHX_ED_APPEND_PROMPT /* "*\b" */
  , SHX_CATCH_TELL       /* "catch_tell" */
  , SHX_CATCH_MSG        /* "catch_msg" */
  , SHX_ID               /* "id" */
  , SHX_VARINIT          /* "__INIT" */
  , SHX_ABS_PATH         /* "make_path_absolute" */
  , SHX_COMPILE_OBJECT   /* "compile_object" */
  , SHX_CONNECT          /* "connect" */
  , SHX_DANGLING_LFUN    /* "dangling_lfun_closure" */
  , SHX_DISCONNECT       /* "disconnect" */
  , SHX_EPILOG           /* "epilog" */
  , SHX_EXT_RELOAD       /* "external_master_reload" */
  , SHX_FLAG             /* "flag" */
  , SHX_GET_BB_UID       /* "get_bb_uid" */
  , SHX_GET_ED_FNAME     /* "get_ed_buffer_save_file_name" */
  , SHX_GET_M_UID        /* "get_master_uid" */
  , SHX_GET_SEFUN        /* "get_simul_efun" */
  , SHX_GET_WNAME        /* "get_wiz_name" */
  , SHX_HANDLE_EXTERNAL_SIGNAL /* "handle_external_signal" */
  , SHX_HEART_ERROR      /* "heart_beat_error" */
  , SHX_INAUGURATE       /* "inaugurate_master" */
  , SHX_INCLUDE_FILE     /* "include_file" */
  , SHX_INHERIT_FILE     /* "inherit_file" */
  , SHX_LOG_ERROR        /* "log_error" */
  , SHX_LOGON            /* "logon" */
  , SHX_LOW_MEMORY       /* "low_memory" */
  , SHX_PRELOAD          /* "preload" */
  , SHX_PREP_DEST        /* "prepare_destruct" */
  , SHX_PRINTF_OBJ_NAME  /* "printf_obj_name" */
  , SHX_PRIVILEGE        /* "privilege_violation" */
  , SHX_QUERY_SHADOW     /* "query_allow_shadow" */
  , SHX_QUOTA_DEMON      /* "quota_demon" */
  , SHX_RETR_ED          /* "retrieve_ed_setup" */
  , SHX_REACTIVATE       /* "reactivate_destructed_master" */
  , SHX_RECEIVE_UDP      /* "receive_udp" */
  , SHX_REMOVE_PL        /* "remove_player" */
  , SHX_RUNTIME          /* "runtime_error" */
  , SHX_SAVE_ED          /* "save_ed_setup" */
  , SHX_SET_MAX_CMDS     /* "set_max_commands" */
  , SHX_NOTIFY_SHUTDOWN  /* "notify_shutdown" */
  , SHX_SHUTDOWN         /* "shutdown" */
  , SHX_SLOW_SHUT        /* "slow_shut_down" */
  , SHX_STALE_ERQ        /* "stale_erq" */
  , SHX_VALID_EXEC       /* "valid_exec" */
  , SHX_VALID_QSNOOP     /* "valid_query_snoop" */
  , SHX_VALID_READ       /* "valid_read" */
  , SHX_VALID_SNOOP      /* "valid_snoop" */
  , SHX_VALID_TRACE      /* "valid_trace" */
  , SHX_VALID_WRITE      /* "valid_write" */
  , SHX_WARNING          /* "runtime_warning" */
  , SHX_ADD_WEIGHT       /* "add_weight" */
  , SHX_CANPUTGET        /* "can_put_and_get" */
  , SHX_DROP             /* "drop" */
  , SHX_GET              /* "get" */
  , SHX_QUERY_WEIGHT     /* "query_weight" */
  , SHX_PREVENT_INSERT   /* "prevent_insert" */
  , SHX_PC_FIND_LIVING   /* "find_living" */
  , SHX_PC_FIND_PLAYER   /* "find_player" */
  , SHX_PC_ID_LIST       /* "parse_command_id_list" */
  , SHX_PC_P_ID_LIST     /* "parse_command_plural_id_list" */
  , SHX_PC_ADJ_LIST      /* "parse_command_adjectiv_id_list" */
  , SHX_PC_PREPOS        /* "parse_command_prepos_list" */
  , SHX_PC_ALLWORD       /* "parse_command_all_word" */
  , SHX_PC_PLURAL_ID     /* "plural_id" */
  , SHX_PC_ADJ_ID        /* "adjectiv_id" */
  , SHX_PC_SHORT         /* "short" */
  , SHX_PC_OF            /* "of" */
  , SHX_PC_CORPSE        /* "corpse" */
  , SHX_PC_CORPSES       /* "corpses" */
  , SHX_PC_TOOTH         /* "tooth" */
  , SHX_PC_TEETH         /* "teeth" */
  , SHX_PC_FOOT          /* "foot" */
  , SHX_PC_FEET          /* "feet" */
  , SHX_PC_MAN           /* "man" */
  , SHX_PC_MEN           /* "men" */
  , SHX_PC_WOMAN         /* "woman" */
  , SHX_PC_WOMEN         /* "women" */
  , SHX_PC_CHILD         /* "child" */
  , SHX_PC_CHILDREN      /* "children" */
  , SHX_PC_SHEEP         /* "sheep" */

  , SHSTR_NOSTRINGS /* The number of strings */
};

extern string_t *shstring[SHSTR_NOSTRINGS];

#define STR_DEFAULT           shstring[SHX_DEFAULT]
#define STR_EMPTY             shstring[SHX_EMPTY]
#define STR_ANONYMOUS         shstring[SHX_ANONYMOUS]
#define STR_ATAT              shstring[SHX_ATAT]
#define STR_CALL_OTHER        shstring[SHX_CALL_OTHER]
#define STR_CATCH             shstring[SHX_CATCH]
#define STR_DANGLING_LAMBDA   shstring[SHX_DANGLING_LAMBDA]
#define STR_CRPATTERN         shstring[SHX_CRPATTERN]
#define STR_DANGLING_V_CL     shstring[SHX_DANGLING_V_CL]
#define STR_DEFAULT_PROMPT    shstring[SHX_DEFAULT_PROMPT]
#define STR_DESTRUCTED        shstring[SHX_DESTRUCTED]
#define STR_DISCARDED_MSG     shstring[SHX_DISCARDED_MSG]
#define STR_EFUN              shstring[SHX_EFUN]
#define STR_SEFUN             shstring[SHX_SEFUN]
#define STR_EFUN_CLOSURE      shstring[SHX_EFUN_CLOSURE]
#define STR_FATAL_ERROR       shstring[SHX_FATAL_ERROR]
#define STR_FROM              shstring[SHX_FROM]
#define STR_FUNCTIONS         shstring[SHX_FUNCTIONS]
#define STR_HEART_BEAT        shstring[SHX_HEART_BEAT]
#define STR_IN                shstring[SHX_IN]
#define STR_LINE              shstring[SHX_LINE]
#define STR_MEMORY            shstring[SHX_MEMORY]
#define STR_MEMDUMP           shstring[SHX_MEMDUMP]
#define STR_MYSQL             shstring[SHX_MYSQL]
#define STR_NEWLINE           shstring[SHX_NEWLINE]
#define STR_NONAME            shstring[SHX_NONAME]
#define STR_NO_PROG_TRACE     shstring[SHX_NO_PROG_TRACE]
#define STR_NO_TRACE          shstring[SHX_NO_TRACE]
#define STR_NOLOG             shstring[SHX_NOLOG]
#define STR_PUBLISH           shstring[SHX_PUBLISH]
#define STR_OBJDUMP_FNAME     shstring[SHX_OBJDUMP_FNAME]
#define STR_DESTOBJDUMP_FNAME  shstring[SHX_DESTOBJDUMP_FNAME]
#define STR_OPCDUMP_FNAME     shstring[SHX_OPCDUMP_FNAME]
#define STR_MEMDUMP_FNAME     shstring[SHX_MEMDUMP_FNAME]
#define STR_OBJECTS           shstring[SHX_OBJECTS]
#define STR_OPCODES           shstring[SHX_OPCODES]
#define STR_OUT_OF_MEMORY_CATCH  shstring[SHX_OUT_OF_MEMORY_CATCH]
#define STR_OUT_OF_MEMORY     shstring[SHX_OUT_OF_MEMORY]
#define STR_PERCENT           shstring[SHX_PERCENT]
#define STR_PERCENT_S         shstring[SHX_PERCENT_S]
#define STR_PERIOD            shstring[SHX_PERIOD]
#define STR_PGSQL             shstring[SHX_PGSQL]
#define STR_PROG_DEALLOCATED  shstring[SHX_PROG_DEALLOCATED]
#define STR_RESERVE           shstring[SHX_RESERVE]
#define STR_SEFUN_CLOSURE     shstring[SHX_SEFUN_CLOSURE]
#define STR_SLABALLOC         shstring[SHX_SLABALLOC]
#define STR_SMALLOC           shstring[SHX_SMALLOC]
#define STR_SLASH             shstring[SHX_SLASH]
#define STR_SPACE             shstring[SHX_SPACE]
#define STR_SYSTEM_MALLOC     shstring[SHX_SYSTEM_MALLOC]
#define STR_TRACE             shstring[SHX_TRACE]
#define STR_TRACEPREFIX       shstring[SHX_TRACEPREFIX]
#define STR_UNDEFINED         shstring[SHX_UNDEFINED]
#define STR_UNKNOWN_LAMBDA    shstring[SHX_UNKNOWN_LAMBDA]
#define STR_UNKNOWN_OBJECT    shstring[SHX_UNKNOWN_OBJECT]
#define STR_VARIABLES         shstring[SHX_VARIABLES]
#define STR_ATTACH_ERQ_DEMON  shstring[SHX_ATTACH_ERQ_DEMON]
#define STR_BIND_LAMBDA       shstring[SHX_BIND_LAMBDA]
#define STR_CALL_OUT_INFO     shstring[SHX_CALL_OUT_INFO]
#define STR_CONFIGURE_INTERACTIVE  shstring[SHX_CONFIGURE_INTERACTIVE]
#define STR_CONFIGURE_DRIVER  shstring[SHX_CONFIGURE_DRIVER]
#define STR_CONFIGURE_OBJECT  shstring[SHX_CONFIGURE_OBJECT]
#define STR_SEND_ERQ          shstring[SHX_SEND_ERQ]
#define STR_INPUT_TO          shstring[SHX_INPUT_TO]
#define STR_SEND_UDP          shstring[SHX_SEND_UDP]
#define STR_ENABLE_TELNET     shstring[SHX_ENABLE_TELNET]
#define STR_EXECUTE_COMMAND   shstring[SHX_EXECUTE_COMMAND]
#define STR_GET_EXTRA_WIZINFO  shstring[SHX_GET_EXTRA_WIZINFO]
#define STR_LIMITED           shstring[SHX_LIMITED]
#define STR_NET_CONNECT       shstring[SHX_NET_CONNECT]
#define STR_NOMASK_SIMUL_EFUN  shstring[SHX_NOMASK_SIMUL_EFUN]
#define STR_RENAME_OBJECT     shstring[SHX_RENAME_OBJECT]
#define STR_SET_AUTO_INCLUDE  shstring[SHX_SET_AUTO_INCLUDE]
#define STR_SET_DRIVER_HOOK   shstring[SHX_SET_DRIVER_HOOK]
#define STR_SET_EXTRA_WIZINFO  shstring[SHX_SET_EXTRA_WIZINFO]
#define STR_SET_EXTRA_WIZINFO_SIZE  shstring[SHX_SET_EXTRA_WIZINFO_SIZE]
#define STR_SET_LIMITS        shstring[SHX_SET_LIMITS]
#define STR_SET_THIS_OBJECT   shstring[SHX_SET_THIS_OBJECT]
#define STR_SHADOW_ADD_ACTION  shstring[SHX_SHADOW_ADD_ACTION]
#define STR_SYMBOL_VARIABLE   shstring[SHX_SYMBOL_VARIABLE]
#define STR_VARIABLE_LIST     shstring[SHX_VARIABLE_LIST]
#define STR_WIZLIST_INFO      shstring[SHX_WIZLIST_INFO]
#define STR_COPY_FILE         shstring[SHX_COPY_FILE]
#define STR_ED_START          shstring[SHX_ED_START]
#define STR_FILE_SIZE         shstring[SHX_FILE_SIZE]
#define STR_GARBAGE_COLLECTION  shstring[SHX_GARBAGE_COLLECTION]
#define STR_GET_DIR           shstring[SHX_GET_DIR]
#define STR_MKDIR             shstring[SHX_MKDIR]
#define STR_OBJDUMP           shstring[SHX_OBJDUMP]
#define STR_OPCDUMP           shstring[SHX_OPCDUMP]
#define STR_PRINT_FILE        shstring[SHX_PRINT_FILE]
#define STR_READ_BYTES        shstring[SHX_READ_BYTES]
#define STR_READ_FILE         shstring[SHX_READ_FILE]
#define STR_REMOVE_FILE       shstring[SHX_REMOVE_FILE]
#define STR_RENAME_FROM       shstring[SHX_RENAME_FROM]
#define STR_RENAME_TO         shstring[SHX_RENAME_TO]
#define STR_RESTORE_OBJECT    shstring[SHX_RESTORE_OBJECT]
#define STR_RMDIR             shstring[SHX_RMDIR]
#define STR_SAVE_OBJECT       shstring[SHX_SAVE_OBJECT]
#define STR_TAIL              shstring[SHX_TAIL]
#define STR_WRITE_BYTES       shstring[SHX_WRITE_BYTES]
#define STR_WRITE_FILE        shstring[SHX_WRITE_FILE]
#define STR_ED_PROMPT         shstring[SHX_ED_PROMPT]
#define STR_ED_APPEND_PROMPT  shstring[SHX_ED_APPEND_PROMPT]
#define STR_CATCH_TELL        shstring[SHX_CATCH_TELL]
#define STR_CATCH_MSG         shstring[SHX_CATCH_MSG]
#define STR_ID                shstring[SHX_ID]
#define STR_VARINIT           shstring[SHX_VARINIT]
#define STR_ABS_PATH          shstring[SHX_ABS_PATH]
#define STR_COMPILE_OBJECT    shstring[SHX_COMPILE_OBJECT]
#define STR_CONNECT           shstring[SHX_CONNECT]
#define STR_DANGLING_LFUN     shstring[SHX_DANGLING_LFUN]
#define STR_DISCONNECT        shstring[SHX_DISCONNECT]
#define STR_EPILOG            shstring[SHX_EPILOG]
#define STR_EXT_RELOAD        shstring[SHX_EXT_RELOAD]
#define STR_FLAG              shstring[SHX_FLAG]
#define STR_GET_BB_UID        shstring[SHX_GET_BB_UID]
#define STR_GET_ED_FNAME      shstring[SHX_GET_ED_FNAME]
#define STR_GET_M_UID         shstring[SHX_GET_M_UID]
#define STR_GET_SEFUN         shstring[SHX_GET_SEFUN]
#define STR_GET_WNAME         shstring[SHX_GET_WNAME]
#define STR_HANDLE_EXTERNAL_SIGNAL  shstring[SHX_HANDLE_EXTERNAL_SIGNAL]
#define STR_HEART_ERROR       shstring[SHX_HEART_ERROR]
#define STR_INAUGURATE        shstring[SHX_INAUGURATE]
#define STR_INCLUDE_FILE      shstring[SHX_INCLUDE_FILE]
#define STR_INHERIT_FILE      shstring[SHX_INHERIT_FILE]
#define STR_LOG_ERROR         shstring[SHX_LOG_ERROR]
#define STR_LOGON             shstring[SHX_LOGON]
#define STR_LOW_MEMORY        shstring[SHX_LOW_MEMORY]
#define STR_PRELOAD           shstring[SHX_PRELOAD]
#define STR_PREP_DEST         shstring[SHX_PREP_DEST]
#define STR_PRINTF_OBJ_NAME   shstring[SHX_PRINTF_OBJ_NAME]
#define STR_PRIVILEGE         shstring[SHX_PRIVILEGE]
#define STR_QUERY_SHADOW      shstring[SHX_QUERY_SHADOW]
#define STR_QUOTA_DEMON       shstring[SHX_QUOTA_DEMON]
#define STR_RETR_ED           shstring[SHX_RETR_ED]
#define STR_REACTIVATE        shstring[SHX_REACTIVATE]
#define STR_RECEIVE_UDP       shstring[SHX_RECEIVE_UDP]
#define STR_REMOVE_PL         shstring[SHX_REMOVE_PL]
#define STR_RUNTIME           shstring[SHX_RUNTIME]
#define STR_SAVE_ED           shstring[SHX_SAVE_ED]
#define STR_SET_MAX_CMDS      shstring[SHX_SET_MAX_CMDS]
#define STR_NOTIFY_SHUTDOWN   shstring[SHX_NOTIFY_SHUTDOWN]
#define STR_SHUTDOWN          shstring[SHX_SHUTDOWN]
#define STR_SLOW_SHUT         shstring[SHX_SLOW_SHUT]
#define STR_STALE_ERQ         shstring[SHX_STALE_ERQ]
#define STR_VALID_EXEC        shstring[SHX_VALID_EXEC]
#define STR_VALID_QSNOOP      shstring[SHX_VALID_QSNOOP]
#define STR_VALID_READ        shstring[SHX_VALID_READ]
#define STR_VALID_SNOOP       shstring[SHX_VALID_SNOOP]
#define STR_VALID_TRACE       shstring[SHX_VALID_TRACE]
#define STR_VALID_WRITE       shstring[SHX_VALID_WRITE]
#define STR_WARNING           shstring[SHX_WARNING]
#define STR_ADD_WEIGHT        shstring[SHX_ADD_WEIGHT]
#define STR_CANPUTGET         shstring[SHX_CANPUTGET]
#define STR_DROP              shstring[SHX_DROP]
#define STR_GET               shstring[SHX_GET]
#define STR_QUERY_WEIGHT      shstring[SHX_QUERY_WEIGHT]
#define STR_PREVENT_INSERT    shstring[SHX_PREVENT_INSERT]
#define STR_PC_FIND_LIVING    shstring[SHX_PC_FIND_LIVING]
#define STR_PC_FIND_PLAYER    shstring[SHX_PC_FIND_PLAYER]
#define STR_PC_ID_LIST        shstring[SHX_PC_ID_LIST]
#define STR_PC_P_ID_LIST      shstring[SHX_PC_P_ID_LIST]
#define STR_PC_ADJ_LIST       shstring[SHX_PC_ADJ_LIST]
#define STR_PC_PREPOS         shstring[SHX_PC_PREPOS]
#define STR_PC_ALLWORD        shstring[SHX_PC_ALLWORD]
#define STR_PC_PLURAL_ID      shstring[SHX_PC_PLURAL_ID]
#define STR_PC_ADJ_ID         shstring[SHX_PC_ADJ_ID]
#define STR_PC_SHORT          shstring[SHX_PC_SHORT]
#define STR_PC_OF             shstring[SHX_PC_OF]
#define STR_PC_CORPSE         shstring[SHX_PC_CORPSE]
#define STR_PC_CORPSES        shstring[SHX_PC_CORPSES]
#define STR_PC_TOOTH          shstring[SHX_PC_TOOTH]
#define STR_PC_TEETH          shstring[SHX_PC_TEETH]
#define STR_PC_FOOT           shstring[SHX_PC_FOOT]
#define STR_PC_FEET           shstring[SHX_PC_FEET]
#define STR_PC_MAN            shstring[SHX_PC_MAN]
#define STR_PC_MEN            shstring[SHX_PC_MEN]
#define STR_PC_WOMAN          shstring[SHX_PC_WOMAN]
#define STR_PC_WOMEN          shstring[SHX_PC_WOMEN]
#define STR_PC_CHILD          shstring[SHX_PC_CHILD]
#define STR_PC_CHILDREN       shstring[SHX_PC_CHILDREN]
#define STR_PC_SHEEP          shstring[SHX_PC_SHEEP]

/* --- Prototypes --- */
extern void init_standard_strings(void);

#endif /* STDSTRINGS_H__ */
