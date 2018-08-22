#include <tune.h>
#include <lines.h>
#define HISTORY_SIZE 20

mapping history=([]);   /* line: ({ last_hour, counter, ({ history }) }) */
string line,verb;       /* eg. arch and ! */

/* Line histories are initialized when first called */
init_history(linename) {
    if (member_array(linename, m_indices(history))==-1)
        history[linename]=({ "0", 0, allocate(HISTORY_SIZE) });

}

line_send(arg) {
string tmp;
    if (!determine_line(query_verb())) return 0;
    if (arg&&!this_player()->query_line(LINES[line][0])) return
        !notify_fail("You're not connected to the "+line+"-line.\n");
    switch(verb) {
        case "": break;
        case "@": if (!arg) break; return line_emote(arg);
        case "'": if (!arg) break; return line_pmote(arg);
        case "#": if (!arg) break; return line_echo(arg);
        case "!": if (!arg) break; return line_feeling(arg);
        case ".": return line_history();
        case "?": return line_who();
        case "+": return line_on();
        case "-": return line_off();
        default:  return !notify_fail("That is not a valid command.\n");
    }
    if (!arg) {
        write("Commands for the "+line+"-line:\n"+
               line+"  <str>    Sends <str> over the "+line+"-line.\n"+
               line+"@ <str>    Sends <str> as emote over the "+line+"-line.\n"+
               line+"' <str>    Sends <str> as pmote over the "+line+"-line.\n"+
               line+"# <str>    Sends <str> as echo over the "+line+"-line.\n"+
               line+"! <str>    Sends feeling <str> over the "+line+"-line.\n");
        if (!LINES[line][5])
            write(line+".          Displays the "+line+"-line's history.\n");
        write( line+"-          Disconnect from the "+line+"-line.\n"+
               line+"+          Connect to the "+line+"-line.\n"+
               line+"?          Displays who is online and who is offline.\n");
        return 1;
    }
    return send(modify(arg,2),modify(arg,2,1));
}

static line_emote(arg) { return send(modify(arg,3),modify(arg,3,1)); }
static line_pmote(arg) { return send(modify(arg,3,2),modify(arg,3,3)); }
static line_echo(arg) {
    if (this_player()->query_exec()>=RET_EXEC)
        return send(modify(arg,4),0);
    else return write("Only Cabinet Members can echo on communication lines.\n");
}

static line_feeling(arg) {
string str1,str2,msg;
object victim;
    str1=explode(arg," ");
    str2=str1[sizeof(str1)-1];
    if (str2) {
        victim=efun::find_player(str2);
        if (!victim&&(str2=="king"||str2=="willie"||str2=="king willie"))
            victim="room/shop"->query_willie();
    }
    msg="obj/daemons/sould"->query_feeling(arg,this_player(),victim);
    if (!msg || (victim && victim->query_invis() && this_player()->query_exec() < victim->query_exec() && this_player()->query_exec() < SEC_EXEC)) return
        write("That is not a valid feeling or adverb.\n");
    return send(modify(msg,4),0);
}

determine_line(arg) {
string *str;
int i;
    line="";
    str=m_indices(LINES);
    for (i=0;i<sizeof(str);i++)
        if (arg[0..2]==str[i][0..2]) { line=str[i]; break; }
    if (!stringp(line)) return
        !notify_fail("That is not a valid line.\n");
    else if (stringp(LINES[line][1])&&!call_other(this_object(),LINES[line][1],this_player())) return
        !notify_fail("You are not authorised to access this line.\n");
    i=strlen(line);
    verb=arg[i..i];
    return 1;
}

/* RN 1: cap real name, RN 2: normal 's, RN 3: cap real name 's */
modify(arg,which,RN) { 
string tmp;
    if ((RN==1||RN==3)&&!this_player()->query_invis()) return 0;
    if (which!=4) {
        if (stringp(LINES[line][5]))
            tmp=sprintf(LINES[line][which],capitalize(call_other(this_player(),LINES[line][5])),
                    name(this_player(),RN),arg);
        else
            tmp=sprintf(LINES[line][which],name(this_player(),RN),arg);
    }
    else {
        if (stringp(LINES[line][5]))
            tmp=sprintf(LINES[line][4],capitalize(call_other(this_player(),LINES[line][5])),arg);
        else
            tmp=sprintf(LINES[line][4],arg);
    }
    return tmp;
}

name(ob,RN) {
string str;
    switch(RN) {
        case 1:
            return "("+capitalize(ob->query_real_name())+")";
        case 2:
            str=ob->query_name();
            return str+"'"+(((str=str[strlen(str)-1]) == 's' || str=='x')?"":"s");
        case 3:
            str=capitalize(ob->query_real_name());
            return "("+str+"'"+(((str=str[strlen(str)-1]) == 's' || str=='x')?"":"s")+")";
        default:
            return ob->query_name();
    }
}

send(arg,RNarg) {
object *all;
string msg1,msg2;
int i,ex;
    if (stringp(LINES[line][1]))
        all=filter_array(efun::users(),LINES[line][1],this_object());
    else all=efun::users();
    all=filter_array(all,"filter_online",this_object());
    msg1=break_string(arg,78,strlen(explode(arg," ")[0])+1);
    if (RNarg) msg2=break_string(RNarg,78,strlen(explode(RNarg," ")[0])+1);
    i=sizeof(all);
    ex=this_player()->query_exec();
    while (i--)
        if (all[i]->short()!="the title less") {
            if (!RNarg||(all[i]->query_exec()<ex&&all[i]->query_exec()<SEC_EXEC))
                tell_object(all[i],msg1);
            else
                tell_object(all[i],msg2);
        }
    if (!LINES[line][5]) save_history(arg);
    return 1;
}

filter_online(ob) {
    if (!objectp(ob)) return 0;
    if (ob->query_line(LINES[line][0])) return 1;
    else return 0;
}

filter_users(ob) {
    if (this_player()->query_exec()>=SEC_EXEC) return 1;
    if (this_player()->query_exec()<ob->query_exec()
        && ob->query_invis()) return 0;
    return 1;
}

save_history(arg){
string str;
    if (!pointerp(history[line])) init_history(line);
    if (history[line][0]!=get_time()) {
        history[line][0]=get_time();
        str=history[line][0]+" | "+arg;
        history[line][2][history[line][1]]=break_string(str,78,"      | ");
    }
    else {
        str="| "+arg;
        history[line][2][history[line][1]]="      "+break_string(str,72,"      | ");
    }
    history[line][1]++;
    if (history[line][1]>=(HISTORY_SIZE-1)) history[line][1]=0;
}

get_time() { return (ctime(time())[11..12]+"."+ctime(time())[14..15]); }

static line_history() {
int i;
    if (LINES[line][5]) return
        !notify_fail("The "+line+"-line does not have a line history.\n");
    if (!pointerp(history[line])) init_history(line);
    printf("---< %-'-'73s\n",line+"-line history >");
    i=history[line][1];
    do {
        i++;
        if (i >= (HISTORY_SIZE-1)) i = 0;
        if (history[line][2][i] && history[line][1]!=i)
            write(history[line][2][i]);
    } while (i!=history[line][1]);
    if (!history[line][2][0])
        write("\t\tNothing has been said over this line... yet. \n");
    write("------------------------------------------------------------------------------\n");
    return 1;
}

static line_on() {
    if (this_player()->query_line(LINES[line][0])) return
        !notify_fail("You're already connected to the "+line+"-line.\n");
    write("You connect to the "+line+"-line.\n");
    line_emote("connects to the "+line+"-line.");
    this_player()->set_line(LINES[line][0]);
    return 1;
}

static line_off() {
    if (!this_player()->query_line(LINES[line][0])) return
        !notify_fail("You aren't connected to the "+line+"-line.\n");
    write("You disconnect from the "+line+"-line.\n");
    this_player()->set_line(LINES[line][0]);
    line_emote("disconnects from the "+line+"-line.");
    return 1;
}

static line_who() {
object *online,*offline,*all;
string *names;
int i;
    online=({}); offline=({});
    all=filter_array(efun::users(),"filter_users",this_object());
    if (stringp(LINES[line][1]))
        all=filter_array(all,LINES[line][1],this_object());
    for (i=0;i<sizeof(all);i++) {
        if (all[i]->query_line(LINES[line][0])) online+=({ all[i] });
        else offline+=({ all[i] });
    }
    if (sizeof(online)) {
        write("Connected to the "+line+"-line:\n");
        online=sort_array(online,"ascending",this_object());
        names=map_array(online,"show_name",this_object());
        printf("%-#*s\n",80,implode(names,"\n"));
        write("\n");
    }
    else write("Nobody is connected to the "+line+"-line.\n\n");
    if (sizeof(offline)) {
        write("Disconnected from the "+line+"-line:\n");
        offline=sort_array(offline,"ascending",this_object());
        names=map_array(offline,"show_name",this_object());
        printf("%-#*s\n",80,implode(names,"\n"));
        write("\n");
    }
    else write("Nobody is disconnected from the "+line+"-line.\n");
    return 1;
}

show_name(ob) {
    if (ob->query_invis()) return
        sprintf("(%s)",capitalize(ob->query_real_name()));
    else return
        capitalize(ob->query_real_name());
}

ascending(ob1,ob2) { return ob1->query_real_name()>ob2->query_real_name(); }
asc_str(str1,str2) { return str1>str2; }

show_lines() {
string *str;
int i;
    str=m_indices(LINES);
    str=sort_array(str,"asc_str",this_object());
    write("\t Mud-wide communication lines on Power Struggle:\n");
    for (i=0;i<sizeof(str);i++) {
                /* addl limit, no open access, no access */
        if ( LINES[str[i]][5] && stringp(LINES[str[i]][1]) &&
                !call_other(this_object(),LINES[str[i]][1],this_player()) )
                        continue;
       printf("%-8s %-55s %-10s\n",
            str[i],LINES[str[i]][6],
            ((stringp(LINES[str[i]][1])&&
            !call_other(this_object(),LINES[str[i]][1],this_player()))?"Restricted":
            (this_player()->query_line(LINES[str[i]][0])?"Online":"Offline")));
    }
    return 1;
}
