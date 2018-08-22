#define write(XX)	tell_object(previous_object(),XX)
#define query_cost(XX) ("room/std_guild"->query_stat_cost(XX))
static string input, comm;
static i;

nomask init_player(nam, other_copy) {
    int tot_exp;
    if(!nam || file_name(previous_object())[0..9] != "obj/login#")
        return;
    enable_commands();
    set_living_name(nam);
    set_heart_beat(1);
    if(!restore_object("players/"+nam))
        return 0;
    add_commands();
#ifdef MUDOS
    if (exec) /* Temp until we get started Ergus 940215 */
        enable_wizard();
#endif
    if(!objectp(other_copy))
        load_auto_obj(auto_load);
    last_login_time = ctime(time())[4..15];
    if (tot_value) {
        write("You find $" + tot_value + " of your lost money!\n");
        money += tot_value;
        tot_value = 0;
    }
    if (exec)  {
        current_path = "players/"+name;
        check_bugreports();
    }
    if (dead && !ghost)
        dead = 0;
    save_me(0);
/*
    if (!query_exec()) {
	tot_exp = experience;
	for (i=1;i<query_real_str();i++) tot_exp += query_cost(i);
	for (i=1;i<query_real_int();i++) tot_exp += query_cost(i);
	for (i=1;i<query_real_dex();i++) tot_exp += query_cost(i);
	for (i=1;i<query_real_con();i++) tot_exp += query_cost(i);
	log_file("ENTER2", capitalize(name)+":+:"+time()+":"+query_ip_name()+":"+tot_exp+"\n");
    }
*/
    return 1;
}

check_bugreports() {
int tmp;
    if (file_size("/players/"+name+"/log/"+name+".rep") > 0) {
        tmp=count_lines("/players/"+name+"/log/"+name+".rep")/3;
        printf("%s have %d bugreports in ~/log/%s.rep\n",
            (tmp>bugreports?"You have NEW bugreports.\nYou now":"You"),
            tmp,name);
        bugreports = tmp;
    }
}

#undef write 

nomask static add_commands() {
    if (exec <= POL_EXEC)
	env = m_delete(env, "PATH"); /* Clear paths.. for demotions etc.. */
    add_standard_commands();
    if (query_level() >= VET_LEVEL)
        vet_commands();
    if (exec) {
        string tmp;
        app_commands();
        if (exec > APP_EXEC) /* Temp until get started. Ergus 940215 */
            pol_commands();
         if (exec >= SEC_EXEC)
             min_commands();
    }
}
    
nomask static demote_pl(str) {
    string old_name,his_name,his_reason; 
    int his_time;
    object temp_pl,env;
    
    if(this_player(1)!=this_object() || this_player()->query_exec()<SEC_EXEC)
        return;
    if (LOWARCHES[capitalize(this_player()->query_real_name())] != "Justice"
        && this_player()->query_exec() < MIN_EXEC)
        return;
    if(!str||sscanf(lower_case(str),"%s %d %s",his_name,his_time,his_reason)!=3)
	return write("Syntax: Demote <playername> <time in minutes> <reason>\n");
    if(file_size("/players/"+his_name+".o")<=0)
	return write("There is no player with that name.\n");
    temp_pl = find_player(his_name);
    if (temp_pl) {
	if (temp_pl == this_player())
	    return write ("You can't demote yourself.\n");
	env = environment(temp_pl);
	tell_object(temp_pl,"You are demoted and are forced to leave the game\n"+
		    "The reason is :"+his_reason+"\n"+
		    "Please come back in "+dstring(dtime(60*his_time,0))+"\n");
	save_me(1);
	destruct(temp_pl);
	tell_room(env,capitalize(his_name)+" left the game unhappily.\n");
    }
    write_file("/players/"+his_name+".o",
	       "demote "+(time()+(his_time*60))+"\n"+
	       "reason \""+his_reason+"\"\n"
	       );

    write(capitalize(his_name)+" is demoted for "+dstring(dtime(60*his_time,0))+"\n");
    log_file("DEMOTIONS",ctime(time())+":'"+his_name+"' demoted by '"+this_player()->query_real_name()+"' reason is\n"+
	     "'"+his_reason+"' he was demoted for "+dstring(dtime(60*his_time,0))+"\n");
    return 1;
}
