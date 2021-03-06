#define MAX_ALIAS	25
#define HISTORY_SIZE    20
#define	MAX_ENV		25

string prompt = ">";
mapping aliases = ([ ]);
status alias_echo = 0;
mapping	env = ([ ]);
static string *history = allocate(HISTORY_SIZE);
static int commands;
static int ptr = 0;

#ifdef MUDOS
 /* I prefer a variable here to avoid the overhead of macro usage */
static private mapping exits = ([
	"n":  "north",
	"s":  "south",
    "e":  "east",
    "w":  "west",
    "nw": "northwest",
    "ne": "northeast",
	"sw": "southwest",
	"se": "southeast",
	"u":  "up",
	"d":  "down",
    ]);
#endif

/* Env routines */
substitute_env(string input) {
string ret,what,foo,foo2,inp;
    ret = "";
    inp = input+" @";
    while(sscanf("@ "+inp,"%s $%s %s",foo,what,foo2) == 3) {
        if(member_array(what,m_indices(env)) >= 0)
            ret += (foo == "@")?sprintf("%s%s ",foo[2..-1],process_string(env[what])):
                sprintf("%s %s ",foo[2..-1],process_string(env[what]));
        else
            ret += (foo == "@")?sprintf("%s$%s ",foo[2..-1],what):
				sprintf("%s $%s ",foo[2..-1],what);
        inp = foo2;
    }
    ret += inp[0..-3];
    if(ret != input)
        printf("%s\n",ret);
    return ret;
}

show_env() {
    int i;
    string *ind;
    write("Environment variables:\n");
    ind = m_indices(env);
    for(i=0;i<sizeof(ind);i++)
        printf("%10-s%O\n","\""+ind[i]+"\":",env[ind[i]]);
    return 1;
}

query_env(string what) {
    mixed ret;
    ret = env[what];
    if (stringp(ret))
        return process_string(ret);
    return ret;
}

set_env(string str) {
    string what,set;
    if (!str)
        return show_env();
    if (sscanf(str,"%s %s",what,set) != 2) {
        if (env[str]) {
            printf("Clear variable %s\n",str);
            env = m_delete(env,str);
            return 1;
        }
        notify_fail("Usage: "+query_verb()+" <what> <to>\n"+
                    "Or     "+query_verb()+" <what>\n");
        return 0;
    }
    if(m_sizeof(env) > MAX_ENV) {
        notify_fail(query_verb()+": Environment data table full.\n");
        return 0;
    }
    env[what] = set;
    return 1;
}

/* Process input stuff */
/*  BAH!!! Somehow this is really wierd; all this great code and coded out!
nomask process_input(input)
{
    if(stringp(input)) {
#ifdef MUDOS
        if (exits[input])
            input = exits[input];
#endif
	input = (sscanf(input,"!%s",input) ?
		 substitute_history(input) : substitute(input));
	if(strlen(input))
	    history_enqueue(input);
    }
    return input;
}

nomask static substitute_history(input)
{
    int hist;
    string tmp, ret;

    if(input == "!")
	ret = history[(ptr + 1) % HISTORY_SIZE];
    else if(sscanf(input,"%d",hist) && hist) {
	if(hist < 0)
	    hist = commands + hist + 1;
	if(hist <= commands && hist > commands - HISTORY_SIZE)
	    ret = history[(ptr + (commands - hist + 1)) % HISTORY_SIZE];
    } else if(strlen(input)) {
	tmp = regexp(history[ptr+1..HISTORY_SIZE]+history[0..ptr], "^"+input);
	if(sizeof(tmp))
	    ret = tmp[0];
    }

    if(ret) {
	tell_object(this_object(), ret + "\n");
	return ret;
    }

    tell_object(this_object(),input + ": Event not found.\n");
    return "";
}

nomask static history_enqueue(command)
{
    history[ptr--] = command;
    if(ptr < 0)
	ptr = HISTORY_SIZE -1;
    commands++;
}

history()
{
    int i;
    string *tmp;
    
    if(this_player() != this_object())
	return;
    tmp = history[ptr+1..HISTORY_SIZE] + history[0..ptr];
    for(i=sizeof(tmp)-1; i >= 0; i--)
	if(stringp(tmp[i]))
	    tell_object(this_object(),(commands-i)+"\t"+tmp[i]+"\n");
    return 1;
}

substitute(string input)
{
    input = substitute_alias(input);
    if (query_exec() >= POL_EXEC)
        input = substitute_env(input);
    return input;
}
nomask static substitute_alias(string input)
{
    string arg,tmp,*args,foo1,foo2;
    status used;
    int no;

    if(!mappingp(aliases))
	aliases = ([]);
    sscanf(input,"%s %s",input,arg);
    if(tmp = aliases[input])
      {
	arg = arg?arg:"";
	args = explode(arg," ");
	while(sscanf("@"+tmp+"@","%s$*%s",foo1,foo2) == 2)
	  {
	    tmp = foo1[1..-1]+arg+foo2[0..-2];
	    used = 1;
	  }
	while(sscanf("@"+tmp+"@","%s$%d%s",foo1,no,foo2) == 3)
	  {
	    tmp = foo1[1..-1]+args[(no-1)%sizeof(args)]+foo2[0..-2];
	    used = 1;
	  }
	if(tmp[-1..-1] == " ")
	  tmp = tmp[0..-2];
	tmp = used?tmp:tmp+(arg==""?"":" "+arg);
	if(tmp != input && alias_echo)
	    write(tmp+"\n");
	return tmp;
      }
    else
	return input+(arg?" "+arg:"");
}

add_alias(str)
{
    string verb;

    if(this_player(1) != this_object())
	return 0;
    if(!stringp(str))
	return show_aliases(0);
    if(sscanf(str, "%s %s", verb, str) != 2)
	return show_aliases(str);
    notify_fail("Maximum number of aliases already used.\n");
    if(m_sizeof(aliases) > MAX_ALIAS)
        return 0;
    notify_fail("Illegal alias.\n");
    if(verb == "alias" || verb == "unalias" || verb == "\\")
	return 0;
    aliases[verb] = str;
    return 1;
}

static show_aliases(verb)
{
    int i;
    string *verbs;
	
    if(!stringp(verb))
	verbs = m_indices(aliases);
    else if(verb[-1..-1] == "*")
	verbs = regexp(m_indices(aliases), "^"+verb[0..-2]);
    else
	verbs = ({ verb });
    if(sizeof(verbs))
	for(i=0; i < sizeof(verbs); i++)
	    if(aliases[verbs[i]])
	       tell_object(this_object(),verbs[i]+"\t"+aliases[verbs[i]]+"\n");
    return 1;
}


query_alias(){ return aliases; }
  

remove_alias(verb)
{
    int i;
    string *verbs;

    if(this_player(1) != this_object())
	return 0;
    if(!stringp(verb))
	return !notify_fail("Usage: Unalias 'verb'.\n");
    if(verb[-1..-1] == "*")
	verbs = regexp(m_indices(aliases), "^"+verb[0..-2]);
    else
	verbs = ({ verb });

    for(i=0; i < sizeof(verbs); i++)
	aliases = m_delete(aliases, verbs[i]);
    return 1;
}
*/
/*
 * Routines for the prompt 
 */

set_prompt(p) {
    if (this_player(1) != this_object())
        return;
    prompt = p;
    tell_object(this_object(),"New prompt: "+query_prompt()+"\n");
    return 1;
}

query_prompt_string()
{
    if (query_variable_exists("prompt"))
        return query_variable("prompt");
    if (stringp(prompt) && strlen(prompt))
        return prompt;
}

query_prompt() {
    string real, pre, rest, tmp;
    object obj;

    real = "";
    if (query_variable_exists("prompt"))
        rest = query_variable("prompt");
    else if(stringp(prompt) && strlen(prompt))
        rest = prompt;
    else real = ">";
    if (obj = "/players/pkunk/room/cpu"->whatami())
        if (obj->query_isplaying(this_object()) &&
           query_variable_exists("doom.prompt"))
            rest = query_variable("doom.prompt");
    if (rest) {
#ifdef MUDOS
        while(sscanf(rest,"%s%%%s",pre,rest) == 2) {
#else
        while(sscanf(rest,"%s%%s",pre,rest) == 2) {
#endif
            switch(rest[0]) {
                case '$':   pre += query_money(); break;
                case '%':   pre += "%"; break;
                case 'b':   pre += query_bank_account(); break;
                case 'c':   pre += (100*query_weight())/query_max_weight(); break;
                case 'e':   pre += query_exp(); break;
                case 'h':   pre += query_hp_percentage(); break;
                case 'H':   pre += sprintf("%3d(%3d)",query_hp(),query_max_hp()); break;
                case 'm':   pre += "Power Struggle"; break;
                case 'n':   pre += query_real_name(); break;
                case 'N':   pre += query_name(); break;
                case 's':   pre += query_sp_percentage(); break;
                case 'S':   pre += sprintf("%3d(%3d)",query_sp(),query_max_sp()); break;
                case 't':   pre += ctime(time())[11..15]; break;
                case '~':   pre += query_exec() < POL_EXEC?"~":fix_path(); break;
                case 'd':   if (obj && obj->query_is_playing(this_object()))
                                tmp = obj->query_hp(this_object());
                            pre += (tmp ? tmp : "--");
                            break;
                case 'D':   if (obj && obj->query_is_playing(this_object()))
                                tmp = obj->query_armour(this_object());
                            pre += (tmp ? tmp : "--");
                            break;
                default:    pre += "%" + rest[0..0];
            }
            real += pre;
            rest = rest[1..strlen(rest)];
        }
        real += rest;
    }
    return real + " ";
}

static fix_path() {
    string tmp;
    if(!stringp(query_path()))
        return "";
    if(sscanf(query_path(),"players/%s",tmp)) {
        if(tmp[0..strlen(query_real_name())-1] == query_real_name())
            return "~"+tmp[strlen(query_real_name())..-1];
        else
            return "~"+tmp;
    }
    else
        return "/"+query_path();
}

write_prompt() {
   tell_object(this_object(),query_prompt());
}
