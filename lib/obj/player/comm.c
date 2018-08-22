#include <daemons.h>
#include "/secure/include/defines.h"
static	mapping	cmds;

cmd_hook(str)
{
    int i;
    string *action;

    if (SOUL_D->do_feeling(str))
	return 1;
    if(this_player() != this_object())
	return 0;
    if(!mappingp(cmds))
	rehash();
    if(action = cmds[query_verb()])
	return call_other(action, "main", str);
}

rehash()
{
    string *path, *dir;
    int i;

    path = query_cmdpaths();
    cmds = ([]);
    for(i=0; i < sizeof(path); i++) {
        path[i] = resolv_path(path[i],"/",this_object());
	if (pointerp(dir = get_dir(path[i]+"/"))) {
	    dir =regexp(dir, "\\.(c|o)$");
	    cmds += mkmapping(
			      map_array(dir,"map_cmds",this_object()),
			      map_array(dir,"map_cmds",this_object(),path[i])
			     );
	}
    }
}

map_cmds(cmd, path)
{
    return (stringp(path) ? path+"/" : "") + cmd[0..-3];
}

query_cmds() { return cmds; }

static add_cmdpath(path)
{
    if(!stringp(path))
	return;
    if (!mappingp(env)) {
	env = mkmapping(({}),({}));
    }
    if(!env["PATH"])
	env["PATH"] = path;
    if(member_array(path, query_cmdpaths()) < 0)
	env["PATH"] += ":"+path;
}

query_cmdpaths()
{
    return env["PATH"]?explode(env["PATH"],":"):({});
}

