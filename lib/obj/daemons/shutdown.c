/*
   mudlib:  Basis
   file:    /bin/daemon/shutdownd.c
   author:  Truilkan
   created: 1992/09/26
*/

//  $Locker:  $
//
//  $Source: /usr/local/mud/libs/basis/bin/daemon/RCS/shutdownd.c,v $
//  $Revision: 1.1 $
//  $Author: garnett $
//  $Date: 92/09/26 22:21:17 $
//  $State: Exp $

/*
  $Log:   shutdownd.c,v $
 * Revision 1.1  92/09/26  22:21:17  garnett
 * Initial revision
 * 
 */

#include <config.h>
#include <uid.h>
#include <daemons.h>

void
save_daemons()
{
   EMOTE_D->save_data();
   CMWHO_D->halt();
   users()->save_data();
}

void
do_shutdown(int how)
{
   if (getuid(previous_object()) != ROOT_UID) {
      return;
   }
   save_daemons();
   shutdown(how);
}
