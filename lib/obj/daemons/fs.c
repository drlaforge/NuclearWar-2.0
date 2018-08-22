/*
 * Remote finger server.
 */

#include <daemons.h>

service_request(id, args)
{
  int i;
  string ret, arg;

  if (pointerp(args) && sizeof(args) && stringp(args[0]))
      arg = args[0];
  ret = explode((string)FINGER_D->finger(arg), "\n"); 
  for (i=0; i < sizeof(ret); i++) 
      INETD->write_socket(id, ret[i]+"\n");
  INETD->close_socket(id);
}
