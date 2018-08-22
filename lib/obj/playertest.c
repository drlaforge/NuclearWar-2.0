#define NEWLINES
/*
 * player.c   - The player object.
 * 
 * Last update: 950507 / Frossworth
 *
 * Shadowstats added 940104 /Zenra
 */

 
#define MAX_IDLE        (60*60*(((exec!=0)*2)+1)) /* 15 minutes */
#define TICK_STATUE        5*60        /* 5 minutes per statue "tick" */
#define MAX_STATUE         6           /* number of ticks, 1.5 hours */
#define CHA_RANDOM         random(19)  /*charisma*/


#include "secure/include/playervars.h"
#include <tune.h>
#include <log.h>
#include "secure/include/daemons.h"
#include "living.h"
#include "/obj/player/file.c"
#include "/obj/player/access.c"
#include "/obj/player/interpret.c"
#include "/obj/player/login.c"
#include "/obj/player/politician.c"
#include "/obj/player/lines.c"
#include "/obj/player/comm.c"
#include "/obj/player/news.c"

nomask reset(arg)
{
    if (arg || myself)
	return;
/*
 *   With arg = 0 this function should only be entered once!
 */
    if(file_name()[0..3] != "obj/") {
        illegal_patch("Cloned player.c");
        destruct(this_object());
        return;
    }
#ifdef NEWLINES
    line = 0; /* On reset no line should be set! */
#else
    wiz_line=0;
#endif
    statue_type = "a sleeping";
    variables = ([]);
    out_law = 0;
    is_linkdead = 0;
    name = "logon";
    msgin = "#N arrives"; msgout = "#N leaves #D";
    mmsgin = mvis = "#N arrives in a puff of smoke";
    mmsgout = minvis = "#N disappears in a puff of smoke";
    msgclone = "#N takes something from a pocket";
    mdest = "#O is disintegrated by #N";
    mhome = "#N leaves home";
    variables["title"] = "the newcomer";
    myself = this_object();
    host = 0;
    surname = 0;
    player_kill = 0;
    deaths = 0;
    player_deaths = 0;
    set_dex(1);
    set_int(1);
    set_str(1);
    set_con(1);
    lifestyle = "Regular";
    hit_point = 50;
    spell_points = 50;
    charisma = 1 + CHA_RANDOM;
    skills = ([]);
    variables = ([]);
    NEWS_ignore = ({});
    NEWS_group = "";
}

/* Identify ourself. */
nomask id(str, lvl)
{
  /*
   *  Some scientists make invisibility items useable by
   *  players , and this will prevent cheating.
   */
    if (!str)
	return 0;
    str = lower_case(str);
      if(!exec)
	  if(str == name)
	      return 1;
  /* 
   *  I think this looks stupid. When I am invisible it is
   *  because I want to work in PEACE.
   */
    if (is_invis && lvl <= query_level())
	return 0;
    if (ghost) {
	    return str == "ghost of "+name || str == name;
    }
    if (str == name)
	return 1;
    if (!query_ip_number(myself)) {
	if (str == "body")
	    return 1;
	if (str == "body of " + name)
	    return 1;
    }
    return 0;
}

nomask short(room_parser_arg)
{
    string desc;
    
    if (is_invis)
        return 0;
    if (ghost) return "Ghost of " + capitalize(name);
    if (room_parser_arg)
        return query_tot_name();
    if (!query_ip_number(this_object()))
        return statue_type + " body of " + query_name();
    desc = query_tot_name();
    if (surname)
        return (capitalize(desc));
    return (capitalize(desc)+" "+query_title());
}

nomask long()
{
    if (!query_ip_number(this_object())) {
	write(capitalize(statue_type) + " body of " + query_name() + ".\n");
	return 1;
    }
    write(short() + ".\n");
    if (query_test_char())
	write(capitalize(query_pronoun()) + " is " + 
	      capitalize(query_test_char()) + "'s test character.\n");
    if (ghost) /* ||  turtle ) */
	return;
    write(capitalize(query_pronoun()) + " is ");
    write(get_mud_age(query_age()) + "\n");
    show_scar();
    write(capitalize(get_shape()));
    if (my_desc)
	write(query_name()+" "+my_desc+".\n");
    if (query_idle(this_object()) >5*60) 
	write("IDLE: "+query_idle(this_object()) / 60+".\n");
}

df_get_mud_age(j)
{
    int tmp,sec,min,hour,day,week,month,year,min2,week2,hour2,day2,month2;
    string Age;

    Age = "";
    j=j*2;
    year=j/3600/24/7/30/12; /*years*/
    month=j/3600/24/7/30-(year*12);   /*months*/
    month2=j/3600/24/7/30;   /*months*/
    week=j/3600/24/7-(month2*30);    /*weeks*/
    week2=j/3600/24/7;    /*weeks*/
    day=j/3600/24-(week2*7);       /*days*/
    day2=j/3600/24;       /*days*/
    hour=j/3600-(day2*24);         /*hours*/
    hour2=j/3600;         /*hours*/
    min=j/60-(hour2*60);            /*minutes*/
    min2=j/60;            /*minutes*/
    sec=j-(min2*60);               /*seconds*/
    if (year) {   
      if (sizeof(Age))
	Age += ", ";
      Age += (year + (year > 1 ? " years" : " year"));
    }
    if (month) {
      if (sizeof(Age))
	Age += ", ";
      Age += (month + (month > 1 ? " months" : " month"));
    }
    if (week) {
      if (sizeof(Age))
	Age += ", ";
      Age += (week + (week > 1 ? " weeks" : " week"));
    }
    if (day) {
      if(sizeof(Age))
        Age += ", ";
      Age += (day + (day > 1 ? " days" : " day"));
    }
    if (hour) {
      if (sizeof(Age))
	Age += ", ";
      Age += (hour + (hour > 1 ? " hours" : " hour"));
    }
    if (min) {
      if (sizeof(Age))
	Age += ", ";
      Age += (min + (min > 1 ? " minutes" : " minute"));
    }
    if (sec) {
      if (sizeof(Age))
	Age += " and ";
      Age += (sec+ (sec > 1 ? " seconds" : " second"));
    }
      return(Age);
  }

  

get_mud_age(i)
{
    int tmp;
    string Age;

    Age = "";
    i = (i/120)+5840;
    tmp = i/365;
    if (tmp) {
      Age += (tmp + (tmp > 1 ? " years" : " year"));
      i -= (tmp*365);
    }
    tmp = (i*2)/61;
    if (tmp) {
      if (sizeof(Age))
	Age += ", ";
      Age += (tmp + (tmp > 1 ? " months" : " month"));
      i = ((i*2)-(tmp*61)+1)/2;
    }
    if (i) {
      if (sizeof(Age))
	Age += " and ";
      Age += (i + (i > 1 ? " days" : " day"));
    }

    return(Age);
}      
    
query_title() {
    if (stringp(variables["title"])&&sizeof(variables["title"]))
        return variables["title"];
    else return "";
}

query_deaths() { return deaths; }

set_title(dummy,only) {}

nomask add_standard_commands() {
    add_action("cmd_hook",          "", 1);
    add_action("communicate",       "'", 1);
    add_action("set_email",         "email");
    add_action("quit",              "quit");
    add_action("change_password",   "password");
    add_action("toggle_whimpy",	    "wimpy");
    add_action("toggle_info",	    "info");
    add_action("toggle_report",	    "report");
    add_action("stop_hunting_mode", "stop");
    add_action("set_my_desc",       "describe");
    add_action("show_tells",        "tell.");
    add_action("news",              "news");
#ifdef NEWLINES
    init_lines();
#endif
    add_cmdpath("/cmds/mortal/");
}

nomask static vet_commands() {
    if (this_object() != this_player())
        return;
    add_action("setmin",            "setmin");
    add_action("setmout",           "setmout");
    add_cmdpath("/cmds/vet/");
}

nomask static app_commands() {
    string ipname;
    ipname = lower_case(query_ip_name(this_player()));
    if (this_object() != this_player())
        return;
    add_action("setmmin",   "setmmin");
    add_action("setmmout",	"setmmout");
    add_action("setmclone",	"setmclone");
    add_action("setmdest",	"setmdest");
    add_action("setmin",    "setmin");
    add_action("setmout",   "setmout");
#ifndef NEWLINES
    add_action("wiz","sci",1);
    set_wiz_line(WIZ_LINE);
#endif
    add_cmdpath("/cmds/apprentice/");
}

nomask static pol_commands() {
    if (this_object() != this_player())
        return;
    add_action("setminvis",	"setminvis");
    add_action("setmvis",	"setmvis");
    add_action("setmhome",	"setmhome");
    add_action("snoop_on",	"snoop");
    add_action("invis",		"invis");
    add_action("vis",		"vis");
    add_action("grant",		"grant");
    add_action("ungrant",	"ungrant");
    add_action("list_acc",	"access");
    add_action("edit",  	"ed");
    add_action("demote_pl",	"demote");
    add_action("set_env",	"setenv");
#ifndef NEWLINES
    add_action("wiz","arch",1);
    set_wiz_line(ARCH_LINE);
#endif
    add_cmdpath("/cmds/sci/");
}

nomask static min_commands() {
    add_cmdpath("/cmds/admin");
}

listen_shout(lev) { return listen_to_shouts_from_level=lev; }

query_listen_shout() { return listen_to_shouts_from_level; }

/* This is called for every shouted string to this player. */
catch_shout(str) {
    if ((this_player()->query_exec()>exec)||
        (this_player()->query_eval()>=listen_to_shouts_from_level)) {
        tell_object(this_object(),str);
        return 1;
    }
}

nomask quit() {
    string errstr, *enters;
    int tot_exp;
    if (this_player() != this_object())
        return 0;
    if (attacker_ob && !attacker_ob->query_npc()) {
        write ("You chicken! You are in battle with "+
            attacker_ob->query_name()+".\n");
        variables["title"] = "the chicken";
    }
    last_login_time = extract(ctime(time()), 4, 15);
    if (environment() && !is_linkdead)
        save_me(0);
    if (exec > APP_EXEC && file_size("/players/"+name+"/.logoutrc") > 0) {
        string input, comm;
        int i;
        input = read_file("/players/"+name+"/.logoutrc", 1, 500);
        comm = explode(input, "\n");
        if (sizeof(comm) > 0)
            for (i = 0; i < sizeof(comm); ++i)
                command(comm[i], this_player());
    }
    write("Saving " + capitalize(name) + ".\n");
    if (!is_invis && !is_linkdead) {
        say(query_name() + " left the game.\n");
        catch(INFO_D->display_info(capitalize(name)+" left the game."));
    }
    else if(is_invis && !is_linkdead)
        catch(INFO_D->display_invis_info(capitalize(name)+" left the game.",exec));
    if(exec>APP_EXEC) catch(move_object(this_object(),"players/"+name+"/workroom"));
    if(errstr = catch(call_other("obj/bin/mortal/drop","drop_all",1))) {
        destruct_all(1);
    }
    /*if (!exec) {
	tot_exp = experience;
	for (i=1;i<query_real_str();i++) tot_exp += query_cost(i);
	for (i=1;i<query_real_int();i++) tot_exp += query_cost(i);
	for (i=1;i<query_real_dex();i++) tot_exp += query_cost(i);
	for (i=1;i<query_real_con();i++) tot_exp += query_cost(i);
	log_file("ENTER2", capitalize(name)+":-:"+time()+":"+query_ip_name()+":"+tot_exp+"\n");
    }
    enters = regexp(explode(read_file("/log/ENTER2"),"\n"), capitalize(name));
    if (sizeof(enters)) {
	string exp_str;
	int exp;
//	printf("%O\n",enters);
	//	printf("%O\n",sscanf(enters[sizeof(enters)-2], "%s:+:%s:%*s", name, exp));
	if (sscanf(enters[sizeof(enters)-2], "%s:+:%s:%*s", name, exp_str) == 2) {
	    exp = atoi(exp_str,10);
	    if (experience && exp)
		printf("Experience gained: %i\n", experience-exp);
	}
    }*/
    if (errstr = catch(destruct(this_object())))
	syslog("Error in quit().\ndestruct() for player " + capitalize(name) +
	       ": " + errstr);
    return 1;
}

nomask toggle_info() {
    write("The function 'info' has been made obsolete.\n"+
          "You now have to use 'set info' instead. This is a regular\n"+
          "setting, which means that it will be saved when you log out.\n"+
          "Use 'unset info' to switch it off again.\n");
    INFO_D->set_info(this_object());
    return 1;
}

  /* Damn hack! */
communicate(str) { return "cmds/mortal/say"->main(str); }
  
static object hb_pause_obj;
pause_heart_beat(obj) {
	hb_pause_obj = obj;
}

passed_my_idle() {
    int i;
    return 0; /* PUT IT UNTIL DEBUGGED */
    if (!query_variable_exists("idle") || query_idle(this_object()) < 30) return 0;
    sscanf(query_variable("idle"),"%d",i);
    return i <= query_idle(this_object());
}

nomask static heart_beat() {
    int   n, variable_wimpy;
    object *inv;
    if(query_ip_number(this_object())) {
        age++;
        if(query_idle(this_object())>=300) idle_age++;
    }
    if (!query_ip_number(this_object())) {
        set_heart_beat(0);
        if (!is_linkdead) {
            if (!ghost && !is_invis)
            say("Suddenly, "+capitalize(name)+
                " gets tired and falls asleep!\n");
            last_login_time = extract(ctime(time()), 4, 15);
            if (!is_invis)
                INFO_D->display_info(capitalize(name) + " went linkdead.");
            else
                INFO_D->display_invis_info(capitalize(name)+" went linkdead.",exec);
            save_me(1);
            is_linkdead = 1;
            time_to_save = age + 500;
            statue_type = "a napping";
            call_out("remove_statue", TICK_STATUE, 0);
	    for(n=0, inv=deep_inventory(this_object()); n < sizeof(inv); n++)
		catch(inv[n]->linkdead(this_object()));
        }
        return;
    }
    else {
        if (is_linkdead) {
            if (!ghost && !is_invis)
                say(capitalize(name) + " wakes up with a yawn!\n");
            is_linkdead = 0;
            remove_call_out("remove_statue");
            for(n=0, inv=deep_inventory(this_object()); n < sizeof(inv); n++)
                catch(inv[n]->reconnect(this_object()));
        }
    }
   /* Arches are supposed to be idle :)
      And temporary wizards...*/
    if (exec<RET_EXEC &&
        (query_idle(this_object()) >= MAX_IDLE || passed_my_idle())) {
        object ob,oblist;
        int i;
        save_me(1);
        say (query_name()+"'s idle time has reached its limit.\n");
        write ("Your idle time has reached its limit.\n");
        oblist = deep_inventory(this_object());
        for (i = 0; i < sizeof(oblist); i++) {
            ob = oblist[i];
            destruct(ob);
        }
        destruct (this_object());
        return;
    }
    if ((((query_idle(this_object())) % (5 * 60)) == 0) && !exec)
        write("You have been idle for "+(query_idle(this_object()) / 60)+" minutes!! Do something!!\n");
    if (ghost) return;
    if (age >= time_to_save) {
        if (!query_variable_exists("brief") && exec<POL_EXEC)
            write("Autosave.\n");
        if(environment(this_object()))
            save_me(1);
        time_to_save = age + 500;
    }
    if (intoxicated && !random(30)) {
        n = random(7);
        switch(n) {
            case 0:
                say(query_name() + " hiccups.\n");
                write("You hiccup.\n");
                break;
            case 1:
                say(query_name() + " seems to fall, but takes a "+
                    "step and recovers.\n");
                write("You stumble.\n");
                break;
            case 3:
                say(query_name() + " looks like a drunk fool.\n");
                write("You feel drunk.\n");
                break;
            case 5:
                say(query_name() + " burps.\n");
                write("You burp.\n");
                break;
            case 6:
                say(query_name() + " seems to be very drunk.\n");
                write("You are very drunk.\n");
                break;
            default:
                break;
        }
    }
    /* No obvious effects of being stuffed or soaked */
    if (hit_point < query_max_hp() || spell_points < query_max_sp() || intoxicated ||
        headache) {
        if (--time_to_heal < 0) {
            if (headache) {
                --headache;
                if (!headache)
                tell_object(myself, "You no longer have a headache.\n");
            }
            if (hit_point < query_max_hp()) {
                ++hit_point;
                if (intoxicated)
                    hit_point += 3;
                if (hit_point > query_max_hp())
                    hit_point = query_max_hp();
            }
            if (spell_points < query_max_sp()) {
                ++spell_points;
                if (intoxicated)
                    spell_points += 3;
                if (spell_points > query_max_sp())
                    spell_points = query_max_sp();
            }
            if (intoxicated) {
                --intoxicated;
                if (!intoxicated) {
                    headache = max_headache;
                    max_headache = 0;
                    tell_object(myself,"You suddenly without "+
                                "reason get a bad headache.\n");
                    hit_point -= 3;
                    if (hit_point < 0)
                        hit_point = 0;
                }
            }
            time_to_heal = 5;
        }
    }
    if (stuffed)
        stuffed--;
    if (soaked)
        soaked--;
    if (attacker_ob)
        attack();
    if (hold_attack<0) hold_attack=0;
    if (hold_attack) hold_attack--;
    if (attacker_ob && (whimpy || query_variable_exists("wimpy"))){
	if(variable_wimpy = atoi(query_variable("wimpy"))){
	    if(variable_wimpy < 5) variable_wimpy = 5;
	    if(variable_wimpy > 50) variable_wimpy = 50;
	    if(hit_point < (variable_wimpy * query_max_hp() / 100)) run_away();
	} else {
	    if(hit_point < (query_max_hp() / 5)) run_away();
	}
    }
}

query_bank_account() { return money_in_bank; }

nomask add_bank_account (amount, ignore_money_check) {
    if (money_in_bank + amount < 0)
        return 0;
    if (!ignore_money_check && money < amount)
        return 0;
    if (this_player() && this_player() != this_object() && exec < MIN_EXEC)
        log_file ("EXPERIENCE",ctime(time()) + " " +
                               this_player()->query_real_name() + " added " +
                               amount + " to " + name + "'s bank account.\n");
    money_in_bank += amount;
    if (!ignore_money_check)
        add_money(-amount);
    save_me(0);
    return 1;
}

test_dark() {
    if (set_light(0) > 0 || present("unique_light",this_object()))
        return 0;
    write("It is too dark.\n");
    return 1;
}

query_max_weight() { return query_str()+10; }
nomask add_weight(w) {
    int max;
    max = query_max_weight();
    if ((w + local_weight > max) && (w > 0))
        return 0;
    if(w + local_weight < 0)
        return 0;
    local_weight += w;
    return 1;
}

/*
 * This one is called when the player wants to change his password.
 */
nomask static change_password(str) {
    if (password != 0 && !str) {
        write("Give old password as an argument.\n");
        return 1;
    }
    if (password != 0 && password != crypt(str, password)) {
        write("Wrong old password.\n");
        return 1;
    }
    password2 = 0;
    input_to("change_password2", 1);
    write("New password: ");
    return 1;
}

nomask static change_password2(str) {
    if (!str) {
        write("Password not changed.\n");
        return;
    }
    if (password2 == 0)	{
        password2 = str;
        input_to("change_password2", 1);
        write("Again: ");
        return;
    }
    if (password2 != str) {
        write("Wrong! Password not changed.\n");
        return;
    }
    password = crypt(password2, 0);
    password2 = 0;
    write("Password changed.\n");
    save_me(1);
}

static toggle_whimpy() {
    if (!whimpy) return
        write("This command is out now. Try 'help set' and 'set wimpy'.\n");
    whimpy=0;
    write("Brave mode. You can't toggle back using this command. Try 'help set'.\n");
    return 1;
}

query_brief() { return query_variable_exists("brief"); }

nomask add_exp(e) {
    if (e >= ROOM_EXP_LIMIT) {
        if (this_player() && this_player() != this_object()
	    && !query_test_char() && query_ip_number(this_player()) 
        && !exec)
        log_file("EXPERIENCE", ctime(time())+" "+name+"(eval "+query_eval()+"/exec "+exec+") "+e+
             " exp by "+this_player()->query_real_name()+"(eval "+this_player()->query_eval()+"/exec "+
             this_player()->query_exec() + ")" +"\n");
        else if (this_player() != this_object()) {
            string filename;
            sscanf(file_name(this_object()), "%s#", filename);
            if (!filename) {
                log_file("MASTER", "Bug in player: add_exp for variable filname.\n");
                return 0;
            }
            log_file("EXPERIENCE", ctime(time())+" "+name+"(eval "+query_eval()+"/exec "+exec+") "+e+
                            " exp by "+this_player()->query_real_name()+"(eval "+this_player()->query_eval()+"/exec "+
                            this_player()->query_exec() + ")" +"\n");
        }
    }
    experience += e;
    if (team && (e > 0)) teamxp += e;
    if (exec<POL_EXEC) /* ??? */
        add_worth(e);
}

set_intoxination(i) { intoxicated = i; } /* added by Thorwald */

add_intoxination(i) {
    if (i < 0) {
        if (-i > intoxicated / 10)
            i = -intoxicated / 10;
    }
    intoxicated += i;
    if (intoxicated < 0)
        intoxicated = 0;
}

query_intoxination() { return intoxicated; }
query_stuffed() { return stuffed; }
query_soaked() { return soaked; }

add_stuffed(i) {
    if (i < 0) {
        if (-i > stuffed / 10)
            i = -stuffed / 10;
    }
    stuffed += i;
    if (stuffed < 0)
        stuffed = 0;
}

add_soaked(i) {
    if (i < 0) {
        if (-i > soaked / 10)
            i = -soaked / 10;
    }
    soaked += i;
    if (soaked < 0)
        soaked = 0;
}

nomask second_life() {
    string tmp,tmp2,tmp3,s1,s2;
    if (exec || previous_object() != this_object())
        return illegal_patch("second_life");
    make_scar();
    ghost = 1;
    if (attacker_ob) {
        tmp = creator(attacker_ob)+" eval "+attacker_ob->query_eval();
        tmp2 = attacker_ob->query_name();
        if (!creator(attacker_ob)) {
            if (attacker_ob->query_npc())
                tmp = "npc, eval "+attacker_ob->query_eval();
            else {
                tmp2 = capitalize(attacker_ob->query_real_name());
                tmp = "plr eval "+attacker_ob->query_eval()+"/exec "+attacker_ob->query_exec();
                tmp += " outlaw:"+attacker_ob->query_out_law();
                if (illegal_kill(attacker_ob))   /* If it is illegal!! */
                    tmp += " ILLEGAL PK";
                else {
                    "obj/daemons/ratings"->log_pk(attacker_ob->query_real_name(),
                         query_real_name(),attacker_ob->query_eval()-query_eval());
                    attacker_ob->add_player_kill(1);
                    player_deaths++;
                }
            }
        }
    }
    else 
        tmp = "none";
        if(attacker_ob && attacker_ob->query_npc() && query_real_name() != "guest")
            log_file("KILLED",query_real_name()+" (eval "+query_eval()+") "+" outlaw:"+out_law+
                     " exp:"+(experience*3/2)+", by "+tmp2+" ("+tmp+") "+
                     extract(ctime(time()),4,15)+".\n");
        else if(attacker_ob && !attacker_ob->query_npc() && test_char != attacker_ob->query_real_name()) {
            log_file("PLAYER_KILLED",query_real_name()+" (eval "+query_eval()+") "+" outlaw:"+out_law+
                     " exp:"+(experience*3/2)+", by "+tmp2+" ("+tmp+") "+
                     extract(ctime(time()),4,15)+".\n");
        if (illegal_kill(attacker_ob)) attacker_ob->set_out_law(1);
        else set_out_law(0);
    }
    if (!illegal_kill(attacker_ob)) deaths++;
    if (query_real_str() > 1)
        set_str(query_real_str()-1);
    if (query_real_con() > 1)
        set_con(query_real_con()-1);
    if (query_real_dex() > 1)
        set_dex(query_real_dex()-1);
    if (query_real_int() > 1)
        set_int(query_real_int()-1);
    if (query_real_cha() > 1)
        set_cha(query_real_cha()-1);
    msgin = "#N drifts around";
    msgout = "#N blows #D";
    headache = 0;
    intoxicated = 0;
    stuffed = 0;
    soaked = 0;
    hunter = 0;
    hunted = 0;
    attacker_ob = 0;
    alt_attacker_ob = 0;
    teamxp = 0;
    variables["title"] = "- the one who just recently died";
    tell_object(myself,"\n\nSurprise! You're dead!\n\n");
    return 1;
}

illegal_kill(ob) {
    if (!ob) return 0;
    if (ob->query_npc()) return 0;
    if (test_char && test_char == ob->query_real_name()) return 0;
    if (!query_out_law() || !ob->query_out_law()) return 1;
    if (((get_average_stat() - ob->get_average_stat()) > 5) || ((get_average_stat() - ob->get_average_stat()) < -5)) return 1;
}

remove_ghost(silent) {
    if (!ghost)
        return 0;
    if (!silent) {
        write("You feel a very strong force.\n");
        write("You are sucked away...\n");
        write("You reappear in a more solid form.\n");
        say("Some mist disappears.\n");
        say(capitalize(query_real_name()) + " appears in a more solid form.\n");
    }
    ghost = 0;
    dead = 0;
    msgin = "#N arrives";
    msgout = "#N leaves #D";
    save_me(1);    
    return 1;
}

stop_hunting_mode() {
    if (!hunted)
        return !notify_fail("You are not hunting anyone.\n");
    call_other(hunted, "stop_hunter");
    hunted = 0;
    write("Ok.\n");
    return 1;
}

drink_alcohol(strength) {
    if (intoxicated > query_con()) {
        write("You are too drunk already, you'd only throw it up.\n");
        return 0;
    }
    intoxicated += strength;
    if (intoxicated < 0)
        intoxicated = 0;
    if (intoxicated == 0)
        write("You are completely sober.\n");
    if (intoxicated > 0 && headache) {
        headache = 0;
        tell_object(myself, "Your headache disappears.\n");
    }
    if (intoxicated > max_headache)
        max_headache = intoxicated;
    if (max_headache > 8)
        max_headache = 8;
    return 1;
}

drink_soft(strength) {
    if (soaked > query_con()) {
        write("You can't possibly drink that much right now!\n" +
              "You feel crosslegged enough as it is.\n");
        return 0;
    }
    soaked += strength;
    if (soaked < 0)
        soaked = 0;
    if (!soaked)
        write("You feel a bit dry in the mouth.\n");
    return 1;
}

eat_food(strength) {
    if (stuffed > query_con()) {
        write("Try something lighter.\n");
        return 0;
    }
    stuffed += strength;
    if (stuffed < 0)
        stuffed = 0;
    if (!stuffed)
        write("Your stomach makes a rumbling sound.\n");
    return 1;
}

/* Politicians must call this function in their test char
   with their own name as the argument */
set_test_char(str) {
    if (!str || test_char || this_player() == this_object())
        return;
    str = lower_case (str);
    if (this_player()->query_real_name() != str)
        return;
    if (query_ip_number(this_player()) != query_ip_number(this_object()))
        return;
    return test_char = str;
}
query_test_char() {  return test_char; }

/*
 * Recursively compute the values of the inventory.
 * Beware that object may selfdestruct when asked for query_value().
 */
nomask compute_values(ob) {
    int v;
    while(ob) {
        int tmp;
        object next_ob;
        next_ob = next_inventory(ob);
        tmp = ob->query_value();
        if (tmp > 1000)
            tmp = 1000;
        v += tmp;
        if (ob && first_inventory(ob))
            v += compute_values(first_inventory(ob));
            ob = next_ob;
        }
        return v;
}

nomask save_me(value_items) {
    object lifestyle_ob;
    if (name == "logon" || name == "guest")
        return;
    tot_value=value_items ? compute_values(first_inventory(this_object())) : 0;
    lifestyle_ob = present("lifestyle",this_object());
    if (!lifestyle_ob || !lifestyle_ob->query_lifestyle_name())
        lifestyle = "regular";
    else
        lifestyle = capitalize(lifestyle_ob->query_lifestyle_name());
    compute_auto_str();
    save_object("players/" + name);
}

nomask illegal_patch(what) {
    if (this_player()->query_exec()<MIN_EXEC) {
        write("You are struck by a mental bolt from the interior of the game.\n");
        log_file("ILLEGAL",ctime(time())+":\n"+this_player()->query_real_name()+
                 " "+what+"\n");
        return 0;
    }
    else
        write("You did something ILLEGAL but we don't care because you are a minister!\n");
    return 1;
}

nomask load_auto_obj(str) {
    string file, argument, rest, err;
    object ob;
    if (!str)
        return 1;
    if (pointerp(str)) {
        int i;
        for(i=sizeof(str)-1;i>=0;i--)
            if(stringp(str[i]) && sscanf (str[i],"%s:%s",file,argument) == 2)
                if(err = catch(ob = clone_object(file)))
                    log_file("AUTO_LOAD", ctime(time())+
                             " ("+name+") "+file+": "+err+"\n");
                else {
                    if(argument)
                        catch(ob->init_arg(argument));
                    transfer(ob, this_object());
                }
                return;
    }
    if (!pointerp(str) && stringp(str)) { /* The old stile, must convert */
    }
}

nomask compute_auto_str() {
    object ob;
    string str;
    auto_load = ({});
    ob = first_inventory(this_object());
    while(ob) {
        str = ob->query_auto_load();
        ob = next_inventory(ob);
        if (!stringp(str))
            continue;
        auto_load += ({ str });
    }
}

/* Enable other objects to query our hit point. */
query_hit_point() { return hit_point; }

query_quests(str) {
    string tmp, rest, rest_tmp;
    int i;
    if (str == 0)
        return quests;
    rest = quests;
    while(rest)	{
        if (str == rest)
            return 1;
        i = sscanf(rest, "%s#%s", tmp, rest_tmp);
        if (i == 0)
            return 0;
        if (tmp == str)
            return 1;
        if (i == 1)
            return 0;
        rest = rest_tmp;
    }
    return 0;
}

nomask set_quest(q) {
    if (!q)
        return;
    if (query_quests(q))
        return 0;
    if (previous_object()) {
        log_file("QUESTS", name+": "+q+" from "+file_name(previous_object())+
                                " "+ctime(time())+"\n");
        if (this_player() && this_player() != this_object() &&
            query_ip_number(this_player()))
            log_file("QUESTS", "Done by " + this_player()->query_real_name() + "\n");
    }
    if (quests == 0)
        quests = q;
    else
        quests = quests + "#" + q;
    return 1;
}

query_real_name() { return name; }

nomask time_out() {
    if(this_player() && this_player() != this_object())
        return;
    write("Time out.\n");
    destruct(this_object());
}

#define SPACE "                                "
nomask people_string(lev) {
    string ret, tmp;
    int dage;
    if (!sscanf (file_name(), "%s#%s", tmp, tmp))
        return;
    ret = allocate(2);
    ret[0] = capitalize(name);
    if (is_invis) {
        if (lev < exec && lev < SEC_EXEC)
            return 0;
        ret[0] = "(" + ret[0] + ")";
    }
    ret[0] = (ret[0] + SPACE)[0..12];
    ret[0] += (exec < 10 ? " " : "");
    ret[0] = (ret[0] + exec + SPACE)[0..15];
    ret[0] += (query_in_ed(this_object()) ? "E" : " ");
    ret[0] += (query_idle(this_object()) >= 600 ? "I" : " ");
    ret[0] += (test_char ? "T" : " ");
    ret[0] = (ret[0] +" "+ query_ip_name(this_object()) + SPACE)[0..45] + " ";
    dage = dtime(age * 2, 0);
    dage[3] = dage[3] + dage[4]*7;
    if (dage[5])      {  dage = dage[5]; tmp = "Y";  }
    else if (dage[3]) {  dage = dage[3]; tmp = "D";  }
    else if (dage[2]) {  dage = dage[2]; tmp = "h";  }
    else if (dage[1]) {  dage = dage[1]; tmp = "m";  }
    else  {  dage = dage[0]; tmp = "s";  }
    ret[0] += (dage < 100 ? " " : "");
    ret[0] += (dage < 10 ? " " : "");
    ret[0] = ret[0] + dage + " " + tmp + " ";
    if (!environment())   return ret[0] + "\n";
        tmp = file_name(environment());
    ret[0] += (sscanf(tmp,"players/%s",tmp) ? "~"+tmp : "/"+tmp);
    ret[0] += "\n";
    if (query_idle(this_object()) > 150)
        ret[1] = 1;
    return ret;
}

reset_host() { host = 0; }

query_host() {
    if (!host) {
        host = query_ip_name(this_object());
        if (!host)   /* Couldn't find this sucker's domain name */
            host = query_ip_number(this_object());
    }
    return host;
}

#define MAX_SCAR	13
int scar;

static make_scar() {
    if (query_eval() < 10)
        return;
    scar |= 1 << random(MAX_SCAR);
}

show_scar(scars) {
    int i, j, first, old_value, tmp;
    string scar_desc, foo;
    scar_desc = ({"left leg", "right leg", "nose", "left arm", "right arm",
                  "left hand", "right hand", "right ear", "left ear",
                  "right foot", "left foot","right cheek","left cheek" });
    j = 1;
    first = 1;
    tmp = (!scars || !intp(scars)) ? scar : scars;
    old_value = tmp;
    foo = "";
    while(i < MAX_SCAR)	{
        if (tmp & j) {
            old_value &= ~j;
            if (first) {
                foo += query_name() + " has a scar on " + query_possessive() + " " + scar_desc[i];
                first = 0;
            }
            else if (old_value)
                foo += (", " + scar_desc[i]);
            else
                foo += (" and " + scar_desc[i]);
        }
        j *= 2;
        i += 1;
    }
    if (!first)
        foo += (".\n");
    if (scars)
        return foo;
    write(foo);
}

nomask query_tot_name() {
    string totname;
    if (is_invis)
        return;
    totname = capitalize (name);
    if (surname && (surname != "0")) 
        totname += " "+capitalize(surname);
    return totname;
}

set_my_desc(str) {
    my_desc = str;
    write("Description: "+query_name()+" "+str+".\n");
    return 1;
}

/* Team stuff... */
set_team_flag(obb) {
    if (!team) teamxp = 0;
    team = obb;
    if ((!!obb) != (query_line(LINES["tchat"][0]) == LINES["tchat"][0])) set_line(LINES["tchat"][0]);
    return team;
}
set_accept_member (obb) { return (accept_team_member=obb); }
query_accept_member() { return accept_team_member; }
query_team () { return team; }
query_teamname() {
string tname;
    if (team) {
        if (!(tname=team->query_variable("teamname")))
            tname=team->query_real_name();
        return tname;
    }
    return 0;
}
query_teamxp() { return teamxp; }

static set_email(str) {
    if (!str) {
		write("Your official email address is: " + mailaddr + "\n");
		return 1;
	}
	mailaddr = str;
	write("Changed your email address.\n");
	return 1;
}

static ofindplayer(str) {
    object list;
    int i;
    string name;
    list = users();
    while (i < sizeof(list)) {
        name = list[i]->query_real_name();
        if (!name)
            name = list[i]->query_name();
        if (name && name == str)
            return list[i];
        i += 1;
    }
    return 0;
}

remove_statue(arg) {
    object *oblist;
    int i;
    arg++;
    switch (arg) {
        case 1:  statue_type = "a snoring";
	       break;
        case 2:  statue_type = "the slumbering";
	       break;
        case 3:  statue_type = "a comatose";
	       break;
    }
    if (arg >= MAX_STATUE) {
        object ob;
        say("A cleaning lady appears and removes the braindead player.\n");
        is_linkdead = 0;
        statue_type = "a sleeping";
        save_me(1);
        oblist = deep_inventory(this_object());
        for (i = 0; i < sizeof(oblist); i++) {
            ob = oblist[i];
            destruct(ob);
        }
        ob = clone_object("/obj/dust");
        move_object(ob, environment());
        destruct(myself);
    }
    else
        call_out("remove_statue", TICK_STATUE, arg);
}

query_last_login() { return last_login_time; }

nomask query_prevent_shadow(ob) {
    return file_name(ob)[0..16] != "domains/Cybernet/";
}

init_mudlogin() {
    if (exec > APP_EXEC)
        if (file_size("/players/" + name + "/.loginrc") > 0) {
            string input, comm;
            int x;
            input = read_file("/players/" + name + "/.loginrc", 1, 500);
            comm = explode(input, "\n");
            if (comm)
                for(x = 0; x < sizeof(comm); ++x)
                    command(comm[x], this_object());
        }
    if (query_variable_exists("info")) INFO_D->set_info(this_object());
    if (query_variable("logincom")) call_out("logincom",2);
}

logincom() {
    string s;
    s = query_variable("logincom");
    if (s) {
        write("LOGIN COMMAND: "+s+"\n");
        command(s,this_object());
    }
}

nomask query_out_law() { return out_law; }

set_out_law(i) { out_law = i; }

add_player_kill(i) {
    player_kill += i;
    "players/techno/pklist"->update_pk(query_real_name(),player_kill);
}

query_pk() { return player_kill; }

add_deaths(i) { deaths +=i; }

query_player_kills() { return player_kill; }
query_player_deaths() { return player_deaths; }

query_charisma() { return charisma; }

set_surname(s) {
    if (this_player()->query_exec() < MIN_EXEC &&
          this_player()!=this_object()) return;
    surname = s;
}
query_surname() { if (surname) return capitalize(surname); }
query_real_surname() { return surname; }

save_tell(str) {
    int i;
    if (pointerp(tells)) {
        if (sizeof(tells) < 5) {
            tells += ({ str });
        }
        else {
            for (i = 0; i < (sizeof(tells)-1); i++)
                tells[i] = tells[i+1];
            tells[sizeof(tells)-1] = str;
        }
    }
    else {
        tells = ({ str });
    }
}

show_tells() {
    if (!tells) {
        write("No tells saved.\n");
        return 1;
    }
    if (pointerp(tells)) {
        write("Tell history:\n");
        for (i = 0; i < sizeof(tells);i++)
            write(tells[i]);
    }
    else
        write("Tell history:\n"+tells);
    return 1;
}
query_tell() { return tells; }


/* Two functions to record legal seconds */

set_alternate_character(s) {
    if ((file_name(previous_object())[0..8] != "obj/clerk") &&
          (this_player(1)->query_exec() < MIN_EXEC)) return;
    alternate=s;
    return alternate;
}
query_alternate_character() { return alternate; }

set_group(s) {
    groupname = s;
    if ((!!s) != (query_line(LINES["gchat"][0]) == LINES["gchat"][0])) set_line(LINES["gchat"][0]);
}
query_group() { return groupname; }

/* Three functions for skill handling */

set_skill(s,i) {
    if (!stringp(s)) return;
    if (this_player() != this_object() && !exec
            && !query_test_char()) {
         log_file("SKILLS",capitalize(this_player(1)->query_real_name())+" patched "+
                capitalize(name)+"'s "+s+" skill from "+skills[s]+" to "+i+".\n");
         return;
    }
    skills[s] = i;
}
add_skill(s,i) {
    if (!stringp(s)) return;
    if (this_player() != this_object() && !exec
            && !query_test_char()) {
         log_file("SKILLS",capitalize(this_player(1)->query_real_name())+" patched "+
                capitalize(name)+"'s "+s+" skill from "+skills[s]+" to "+skills[s]+i+".\n");
         return;
    }
    skills[s] += i;
}
query_skill(s) {
    if (!stringp(s)) return skills;
    return skills[s];
}

/* Functions for variable setting with 'set' */
set_variable(s,i) {
    if (this_player() && this_player(1) != this_object() && this_player(1)->query_exec() < SEC_EXEC) return write("Nope.\n");
    variables[s]=i;
}

query_variable(s) {
    if (!s) return variables;
    return variables[s];
}

query_variable_exists(s) {
    return (s && member_array(s,m_indices(variables)) != -1);
}

delete_variable(s) {
    if (this_player() && (this_player(1) != this_object() && this_player(1)->query_exec() < SEC_EXEC)) return write ("Nope.\n");
    variables = m_delete(variables,s);
}

nomask query_eval() {
    if ((exec||test_char) && query_variable("eval")) {
        int i;
        if (sscanf(query_variable("eval"),"%d",i)) return i;
    }
    return get_average_stat();
}
nomask query_exec() { return exec; }

nomask set_exec(i) {
    if (!intp(i))
        return illegal_patch("set_exec "+i+" at "+name);
    if (i<0)
        return illegal_patch("set_exec "+i+" at "+name);
    if (this_player(1)!=this_object()&&this_player(1)->query_exec()<MIN_EXEC)
        return illegal_patch("set_exec "+i+" at "+name);
    if (i>=MIN_EXEC||i<exec&&exec>POL_EXEC)
        return illegal_patch("set_exec "+i+" at "+name);
    if (!this_player()||!interactive(this_player())||this_player()->query_exec()
        <MIN_EXEC&&i>POL_EXEC)
        return illegal_patch("set_exec "+i+" at "+name);
    if (this_player()) {
        log_file("SET_EXEC",previous_object()->short()+" "+
                             this_player()->query_name()+" "+i+"\n");
        exec=i;
    }
    if (exec>=APP_EXEC) {
        app_commands();
        if (exec>=POL_EXEC)
            pol_commands();
        if (exec>=SEC_EXEC)
            min_commands();
        tell_object(myself,"Adding politician's commands...\n");
    }
}
