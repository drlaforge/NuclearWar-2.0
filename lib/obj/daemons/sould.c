/*  The Soul Daemon

 *  Power Struggle MUD (from a ripoff from AbacusMud)

 *  Basic contributors:
 *  - Menta (AbacusMUD)
 *  - Bigfoot (AbacusMUD)
 *    This bloke did most of the nifty work I guess...
 *  - Draconian (AbacusMUD)
 *  - Scarblac (Power Struggle)
 *    Added query_feeling() and separated loading of adverbs and feelings
 *  - Cobra (Power Struggle)
 *    Basic cleanup, changed adverb structure (for QC reasons)
 */

/*  Filenames of the definition files   */
#define ADVERBS     "/etc/adverbs.new"
#define FEELINGS    "/etc/feelings.new"

/*  Filename of the soul's savefile     */
#define SOULDATA    "obj/o/souldata"

/*  Filenames of the help files         */
#define TARGET_F    "/doc/mortal/feelings"
#define TARGET_A    "/doc/mortal/adverbs"

#define replace(XX,YY,ZZ) implode(explode(XX,YY),ZZ)

mapping feelings = ([]);
mapping adverbs = ([]);

query_feelings() { return feelings; }
query_adverbs() { return adverbs; }

void create() {
    restore_object(SOULDATA);
}

reload(what) {
string file, *lines;
    switch(what) {
        case "feelings":
            feelings=([]);
            file=read_file(FEELINGS);
            break;
        case "adverbs":
            adverbs=([]);
            file=read_file(ADVERBS);
            break;
        default:
            return 0;
    }
    if (stringp(file) && pointerp((lines=explode(file, "\n"))))
        map_array(lines, "map_data", this_object(), what);
    update(what);
    save_object(SOULDATA);
}

map_data(arg, what) {
string *arr;
    if (!stringp(arg)) return 0;
    if (pointerp(arr=explode(arg, "@")))
        switch(what) {
            case "feelings":
                feelings[arr[0]]=arr[1..sizeof(arr)-1];
                if (sizeof(feelings[arr[0]])<4)
                    feelings[arr[0]]+=({""});
                break;
            case "adverbs":
                adverbs[arr[0]]=arr[1];
                break;
            default:
                return;
        }
}

update(what) {
    if (what=="feelings") {
        if (file_size(TARGET_F)>0)
            rm(TARGET_F);
        write_file(TARGET_F, sprintf("\nHere are the feelings you can use "+
            "for some atmospherical fun:\n%-#*s\n\nSee also 'help adverbs' "+
            "for the adverbs to use with the feelings.\n", 78,
            implode(sort_array(m_indices(feelings), "sort", this_object()), "\n")));
    }
    else {
        if (file_size(TARGET_A)>0)
            rm(TARGET_A);
        write_file(TARGET_A, sprintf("\nHere are the adverbs you can use "+
            "with the feelings:\n%-#*s\n\nSee also 'help feelings' for a "+
            "list of feelings to use these adverbs with.\n", 78,
            implode(sort_array(map_array(m_indices(adverbs), "feeling_help",
                this_object(), adverbs), "sort", this_object()), "\n")));
    }
}

sort(arg1, arg2) {
    if (!stringp(arg1) || !stringp(arg2)) return;
    return (arg1>arg2);
}

feeling_help(arg, advs) {
    return sprintf("%s = %-=72s", arg, advs[arg][1..-1]);
}

do_feeling(str, cmd) {
string adverb, whom;
string *feeling;
object obj;
    if (cmd) {
        if (sscanf(str, "%s %s", cmd, str)!=2) {
            cmd=str;
            str=0;
        }
    }
    else cmd=query_verb();
    if (!mappingp(feelings))
        restore_object(SOULDATA);
    if (!(feeling=feelings[cmd]))
        return 0;
    if (this_player()->query_ghost())
        return !notify_fail("You are a ghost. Ghosts don't have any emotions. Deal with it.\n");
    if ((obj=environment(this_player())) && source_file_name(obj)=="room/post")
        return !notify_fail("Someone says: Would you please refrain from using feelings here?\n");
    if (!strlen(feeling[3]))
        feeling[3]=sprintf("%s-%ss", cmd, cmd);
    if (str) {
        if (sscanf(str, "%s %s", adverb, whom)!=2) {
            if (obj=present(str, environment(this_player()))) return
                perform(feeling[1], feeling[3], feeling[2], obj);
            else if (adverb=find_adverb(str)) return
                perform(feeling[0], feeling[3], adverb, 0);
            else return
                !notify_fail("No such player or adverb: "+str+".\n");
        }
        else {
            if (!(obj=present(whom, environment(this_player())))) return
                !notify_fail("There is no "+capitalize(whom)+" present.\n");
            str=adverb;
            if (adverb=find_adverb(adverb)) return
                perform(feeling[1], feeling[3], adverb, obj);
            else return
                !notify_fail("No such adverb: "+str+".\n");
        }
    }
    else return
        perform(feeling[0], feeling[3], feeling[2], 0);
}

perform(format, verbs, adverb, victim, player) {
    if (!stringp(adverb) || !strlen(adverb)) {
        format=replace(format, " #AD1", "");
        format=replace(format, " #AD2", "");
        format=replace(format, "#AD1", "");
        format=replace(format, "#AD2", "");
    }
    else if (adverb[0]=='$') {
        format=replace(format, " #AD1", "");
        format=replace(format, "#AD1", "");
        format=replace(format, "#AD2", adverb[1..-1]);
    }
    else {
        format=replace(format,"#AD1",adverb[1..-1]);
        format=replace(format," #AD2","");
        format=replace(format,"#AD2","");
    }
    if (player) return
        own_action(format, verbs, victim, player);
    else return
        !action(format, verbs, victim);
}

find_adverb(arg) {
string tmp;
    if (arg=="%") return "";
    if (!(tmp=adverbs[arg]))
        if (member_array(arg, m_values(adverbs))!=-1)
            tmp=arg;
    return tmp;
}

query_feeling(cmd, player, victim) {
string *feeling, adv, str; int i; object *ob;
i=0; str=0;
    if (!cmd || !player) return
        !notify_fail("Strange soul-daemon error occurred.\n");
    cmd=explode(cmd, " ");
    if (!pointerp(cmd) || !sizeof(cmd)) return
        !notify_fail("Very strange soul-daemon error occurred.\n");
    if (!(feeling=feelings[cmd[0]])) {
    /* Check for feelings in other objects - Scarblac */
        if (this_player()) ob = all_inventory(this_player()); else ob = ({});
        while (!str && i<sizeof(ob)) {
            if (ob[i]->id("sould_obj"))
               str = ob[i]->query_feeling(cmd,player,victim);
            i++;
       }
        if (!str) return !notify_fail("No such feeling: "+cmd[0]+".\n");
        else return own_action(str[0],str[1],victim,player);
    }
    if (!strlen(feeling[3]))
        feeling[3]=sprintf("%s-%ss", cmd[0], cmd[0]);
    if (sizeof(cmd)>1) {
        if (sizeof(cmd)==2) {
            if (victim) return
                perform(feeling[1], feeling[3], feeling[2], victim, player);
            else if (adv=find_adverb(cmd[1])) return
                perform(feeling[0], feeling[3], adv, 0, player);
            else return 0;
        }
        else {
            if (!victim) return 0;
            if (adv=find_adverb(cmd[1])) return
                perform(feeling[1], feeling[3], adv, victim, player);
            return 0;
        }
    }
    else return
        perform(feeling[0], feeling[3], feeling[2], 0, player);
}

own_action(format, verb, victim, player) {
string pre, icon, result;
    result="";
    if (!pointerp((verb=make_verbs(verb)))) return 0;
    while (sscanf(format+" ", "%s#%s %s", pre, icon, format)==3) {
        result+=pre;
        format=format[0..-2];
        if (icon[0]=='#') {
            result+=verb[0]+icon[1..-1];
            verb=verb[1..sizeof(verb)-1];
        }
        else if (strlen(icon)>1 && victim && icon[0..1]=="V#") {
            result+=verb[0]+icon[2..-1];
            verb=verb[1..sizeof(verb)-1];
        }
        else {
            switch(icon[0..1]) {
                case "NA": result+=player->query_name()+icon[2..-1]; break;
                case "PR": result+=player->query_pronoun()+icon[2..-1]; break;
                case "PO": result+=player->query_possessive()+icon[2..-1]; break;
                case "OB": result+=player->query_objective()+icon[2..-1]; break;
                case "OS": result+=player->query_objective()+"self"+icon[2..-1]; break;
                case "NS": result+=player->query_name()+"'s"+icon[2..-1]; break;
                    default: break;
            }
            if (strlen(icon)>2) switch(icon[0..2]) {
                case "VNA": result+=victim->query_name()+icon[3..-1]; break;
                case "VPR": result+=victim->query_pronoun()+icon[3..-1]; break;
                case "VPO": result+=victim->query_possessive()+icon[3..-1]; break;
                case "VOB": result+=victim->query_objective()+icon[3..-1]; break;
                case "VOS": result+=victim->query_objective()+"self"+icon[3..-1]; break;
                case "VNS": result+=victim->query_name()+"'s"+icon[3..-1]; break;
                    default: break;
            }
        }
        if (strlen(format)) format=" "+format;
    }
    return (result+format);
}

static make_verbs(arg) {
string verb, *verbs;
    verbs=({});
    if (!stringp(arg) || arg=="") return 0;
    if (arg[strlen(arg)-1]!=',')
        arg+=",";
    while (sscanf(arg, "%s,%s", verb, arg)==2)
        verbs+=({ explode(verb, "-")[1] });
    return verbs;
}
