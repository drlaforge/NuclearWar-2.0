/* Information object. Tell you when other people log in/off. Might be
 * extended in the future.
 */

/* Objects allowed to send info.. */
#define ACCESS	({ "obj/player", "obj/login" , "obj/master"})
#include <tune.h>
object *info = ({});

void display_info (string str, int wiz)
{
    int i;
    string tmp,number,host;
    mixed svar;
    object ob;
    tmp = file_name(previous_object());
    sscanf(tmp, "%s#%*s", tmp);
    if(member_array(tmp, ACCESS) < 0)
	!notify_fail("Illegal caller!\n");
    for (i=0; i < sizeof (info); i++)
	if (this_player() != info[i])
	    if (objectp (info[i]))
		if(info[i]->query_level()>=APP_WIZ_LEVEL) {
		    svar=explode(str, " ");
		    if(ob=find_player(lower_case(svar[0])))
			number=query_ip_name(ob);
		    if (tmp == "obj/master") 
			tell_object (info[i], "<VOID INFO> ["+
				     ctime(time())[11..15]+"]: " + 
				     str + " \n");		
		    if ((tmp != "obj/master"))
			if(number)
			    tell_object (info[i],
					 "WIZINFO ["+ctime(time())[11..15]+
					 "]: " + str + "("+number+") \n");
			else 
			    tell_object (info[i],"WIZINFO ["+
					 ctime(time())[11..15]+
					 "]: " + str + " \n");
		} else
		    if (!wiz)
			tell_object (info[i], "INFO ["+
				     ctime(time())[11..15]+"]: " + str + "\n");
		    else
			info = exclude_array (info, i--, 0);
}

void display_invis_info(string str,int level)
{
    int i;
    string tmp,number;
    mixed svar;
    object ob;
    tmp = file_name(previous_object());
    sscanf(tmp, "%s#%*s", tmp);
    if(member_array(tmp, ACCESS) < 0)
      !notify_fail("Illegal caller!\n");
    for (i=0; i < sizeof (info); i++)
      if (this_player() != info[i])
	if (objectp (info[i])) {
	    svar=explode(str, " ");
	    if(ob=find_player(lower_case(svar[0])))
		number=query_ip_name(ob);    
	    if(info[i]->query_level() >= level || info[i]->query_level() >= ARCH_LEVEL)
		if(number)
		    tell_object (info[i], "WIZINFO ["+ctime(time())[11..15]+"]: (Invisible) " + str + "("+number+") \n");
	    else
		    tell_object (info[i], "WIZINFO ["+ctime(time())[11..15]+"]: (Invisible) " + str + "\n");
	else
	    info = exclude_array (info, i--, 0);
	}
}

string toggle_info (object player)
{
    int index;

    if ((index = member_array (player, info)) < 0)
    {
        info = add_array (info, player);
	return "Info mode on.";
    }
    info = exclude_array (info, index, 0);
    return "Info mode off.";
}

mixed
query_info (object player)
{
    if (objectp (player))
        return member_array (player, info);
    return info;
}







