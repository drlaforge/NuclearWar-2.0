/* This object checks whether the player can login,
 * he can't login if his alternate character is online
 * or if his alternate character has logged in earlier
 * during this reboot and this player is not a wizard.
 */

#include <tune.h>
#define SAVE "obj/o/alternate_save"

string alternate;

prevent_login(string alter, string name, int exec) {
    if (!alter) return 0;
    if (find_player(alter)) return 1;
    if (exec) {
        if (!(member_array(name,alternate)+1)) alternate+=({name});
        save_object(SAVE);
        return 0;
    }
    if (member_array(alter,alternate)+1) return 1;
    else alternate+=({name});
    save_object(SAVE);
    return 0;
}
        
create() {
    alternate = ({});
    (this_player()) ?
         (restore_object(SAVE)) :
         (save_object(SAVE));
}

delete_name(name) {
    if (this_player(1)->query_exec() < SEC_EXEC) return
        write("NO WAY BUSTER.\n");
    alternate -= ({name});
    save_object(SAVE);
    return write("Ok.\n");
}
