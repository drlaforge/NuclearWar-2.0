#include <tune.h>

#define ACCESS   "obj/o/access"
#define SAVE     save_object(ACCESS)
#define RESTORE  restore_object(ACCESS)

mapping read_access, write_access;

void reset(int arg)
{
    if (arg)
        return;

    if(cloned())
    {
        destruct(this_object());
	return;
    }
    if (!RESTORE)
    {
        read_access = write_access = ([]);
	SAVE;
    }
}

string check_access(string file, object tp, int readflag)
{
    int i;
    mixed access;
    string dummy, name;
    
    if(!objectp(tp) || !stringp(file))
        return 0;

    if(!query_ip_number(tp))
        return 0;
    name = (string) tp->query_real_name();
    if(sscanf(file, "/players/"+name+"/%s", dummy))
    {
        sscanf(file, "/%s", file);
	return file;
    }
    access = readflag ? read_access : write_access;
    access = ((pointerp(access[name]) ? access[name] : ({})) +
             (pointerp(access["public"]) ? access["public"] : ({})));

        

    for	(i=0; i < sizeof(access); i++)
	if (sscanf (file + "@", access[i] + "%s", dummy) == 1)
	{
	    sscanf(file, "/%s", file);
	    return file;
	}
    return 0;
}

int add_access(string where, string who, object tp, int readflag)
{
    mixed access;
    string name, owner, dummy;

    if (!objectp(tp) || !stringp(where) || !stringp(who))
        return 0;
    if(!query_ip_number(tp))
        return 0;
    if(!query_ip_number(previous_object()))
        return 0;
    if(tp != previous_object())
        return 0;

    name = (string) tp->query_real_name();
    if((int) tp->query_exec() < SEC_EXEC)
    {
        if(sscanf(where + "/dummy","/players/%s/%s", owner, dummy) != 2)
	    return 0;
	if (owner != (string) previous_object()->query_real_name())
	    return 0;
    }
    access = readflag ? read_access : write_access;
    if (!pointerp(access[who]))
    {
        if (who != "public" && !restore_object("players/" + who))
	    return 0;
	access[who] = ({});
    }
    access[who] += ({ where });
    readflag ? (read_access = access) : (write_access = access);
    SAVE;
    return 1;
}

int rem_access(string where, string who, object tp, int readflag)
{
  int index;
  mixed access;
  string name, owner, dummy;
  
  if(!objectp(tp) || !stringp(who) || !stringp(where))
      return 0;
  if(!query_ip_number(tp))
      return 0;
  if (tp != previous_object())
      return 0;
  name = (string) tp->query_real_name();
  if((int) tp->query_exec() < MIN_EXEC)  /* Own dir only if not Arch */
  {
      if(sscanf(where+"/dummy", "/players/%s/%s", owner, dummy) != 2)
	  return 0;
      if (owner != (string) previous_object()->query_real_name())
	  return 0;
  }
  access = readflag ? read_access[who] : write_access[who];
  if (!pointerp(access))
      return 0;
  index = member_array(where, access);
  if (index < 0)
      return 0;
  access = access[0..(index-1)] + access[(index+1)..(sizeof(access)-1)];
  readflag ? (read_access[who] = access) : (write_access[who] = access);
  SAVE;
  return 1;
}

void list_acc(string who)
{
    int i;
    string *racc, *wacc;

    if (!stringp(who))
        who = (string) this_player()->query_real_name();

    racc = (pointerp(read_access[who]) ? read_access[who] : ({}));
    wacc = (pointerp(write_access[who]) ? write_access[who] : ({}));
    if (!sizeof(racc) && !sizeof(wacc))
    {
        write(capitalize(who) + " has no special access.\n");
        return;
    }
    write("Full access:                           Read access:\n");
    write("-=-=-=-=-=-                            -=-=-=-=-=-\n");
    for (i=0; i < sizeof(racc) || i < sizeof(wacc); i++)
    {
        string outp;
      
        outp = ((i < sizeof(wacc) ? wacc[i] : "") + 
	       "                                          ")[0..38] +
	       (i < sizeof(racc) ? racc[i] : "") + "\n";
	write(outp);
    }
}

int query_prevent_shadow()
{
	return	1;
}
