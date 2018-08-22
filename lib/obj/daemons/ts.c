/*
 * Intermud tell server
 * Original author: Huthar@Portals
 * Rewritten: Blackthorn@Genocide (10/29/92)
 * Include files tweaked: Buddha@tmi-2 (11/6/92)
 */
 
#include <daemons.h>
#include <config.h>

read_callback(int id, string msg)
{
  string originator, mud, target, mesg;
  object ob;
 
  if (!msg) return;
  if (sscanf(msg, "%s@%s tells %s: %s", originator, mud, target, mesg) != 4 ||
      !(ob = find_player(lower_case(target))))
  {
    INETD->write_socket(id, "TellServ@" + THIS_MUD + " tells " + originator + 
                     ": Cannot find " + capitalize(target) + " here.\n");
  } else {  
    INETD->write_socket(id, "TellServ@" + THIS_MUD + " tells " + originator + 
			": Remote tell was successful.\n");
    tell_object(ob, capitalize(originator) + "@" + capitalize(mud) + 
    " tells you: " + mesg + "");
  }
  INETD->close_socket(id);
}
