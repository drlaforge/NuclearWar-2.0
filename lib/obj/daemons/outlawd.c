/* Checks which players have chosen to be outlaw already,
   and what they chose. */

#define SAVE "obj/o/outlaws"

mapping outlaws;

/* Players in an outlaw/freeman group are automatically that.
 * Players with set outlaw or freeman are automatically that (but
 * group comes first).
 * With both outlaw and freeman set, you are outlaw.
 * Players who leave their outlaw group can still choose what they
 * want to be for the rest of the reboot, players who chose by
 * setting and unset it can't.
 */
query_done(name,groupname,variables) {
    int i;
    if (outlaws[name]) return outlaws[name];
    if (groupname) {
        catch(i = ("room/group_hqs/"+groupname)->query_outlaw());
        if(i) return i;
    }
    if (variables) {
        if (member_array("outlaw",m_indices(variables)) != -1) {
            outlaws[name] = 2;
            return 2;
        }
        if (member_array("freeman",m_indices(variables)) != -1) {
            outlaws[name] = 1;
            return 1;
        }
    }
    return 0;
}

set_done(name,i) {
    outlaws[name] = i+1;
    save_object(SAVE);
}

create() {
    outlaws = ([]);
    (this_player()) ?
        (restore_object(SAVE)) :
        (save_object(SAVE));
}

