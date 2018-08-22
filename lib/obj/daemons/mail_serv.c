/*
 *  A kinder, gentler (simpler) mail server.
 *  By Cynic, 12/91 -> 1/92.
 *
 *  It's not really a mail server, but I didn't know what else to 
 *  call it.  :)
 *
 */

#include <config.h>
#include <pmb.h>

int new_mail, old_mail;

int query_new_mail(string who);
int query_old_mail(string who);

int query_new_mail(string who) {
    seteuid((string)MASTER_OB->get_bb_uid());
    new_mail = 0;
    who = capitalize(who);
    if (file_size(SAVE_DIR + who + ".o") < 0) return 0;
    if (!restore_object(SAVE_DIR + who)) return 0;
    return new_mail;
}

int query_old_mail(string who) {
    seteuid((string)MASTER_OB->get_bb_uid());
    old_mail = 0;
    who = capitalize(who);
    if (file_size(SAVE_DIR + who + ".o") < 0) return 0;
    if (!restore_object(SAVE_DIR + who)) return 0;
    return old_mail;
}

/* EOF */
