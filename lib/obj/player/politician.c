/* 1=arch 2=wiz 4=wizsv */

nomask query_wiz_line(nr) {
    return (wiz_line & nr);
}

nomask set_wiz_line(num)
{
    string ipname;
    ipname = query_ip_name(this_player());
    if (wiz_line & num)
	wiz_line &= ~num;
    else {
	if (num == WIZSV_LINE && (ipname[(strlen(ipname) - 2)..-1] == "se" ||
				  ipname[0..2] == "130" || ipname == "129"))
	    wiz_line |= num;
	else
	    wiz_line |= num;	    
    }
}

nomask wiz(str) {
    return "obj/wiz_line"->wiz(str);
}

nomask static destruct_all(force)
{
    object          ob, next;
    int             temp_level;
    string          err;


    ob = first_inventory(this_object());
    while (ob) {
	next = next_inventory(ob);
	if (err = catch(destruct(ob)))
	    syslog("destruct_all(): destruct() failed: " + err);
	ob = next;
    }
}

#define CHECKWIZ if(!exec) return 0;
#define WIZSET(XX,YY,ZZ) \
XX(m) { \
  CHECKWIZ \
  if (!m) { \
      write("current: " + YY + ".\n"); \
      return 1; \
    } \
    YY  = m; \
    write("Ok.\n"); \
    return 1; \
} \
ZZ(ob) { return convert_message(YY, ob); }

WIZSET(setmclone,msgclone,query_msgclone)
WIZSET(setmdest,mdest,query_mdest)
WIZSET(setmhome,mhome,query_mhome)
WIZSET(setminvis,minvis,query_minvis)
WIZSET(setmvis,mvis,query_mvis)

nomask static 
snoop_on(str) {
object          ob, snooper;
int             ob_exec;

    if (!str) {
        snoop();
        return 1;
    }
    ob = find_player(str);
    if (!ob) {
        write("No such player.\n");
        return 1;
    }
    ob_exec = call_other(ob, "query_exec");
    if (exec <= ob_exec && exec < SEC_EXEC) {
        write("You fail.\n");
        return 1;
    }
    if (exec < SEN_EXEC)
        if (!ob->query_test_char()){
            write("You fail.\n");
            return 1;
        }
    if (exec < SEC_EXEC)
        if (environment(ob) && environment(ob)->query_snoop_protected(ob)) {
            log_file("SNOOP", ctime(time()) + " " + capitalize(name) +
                     " tried to snoop " + capitalize(ob->query_real_name()) +
                     " (" + file_name(environment(ob)) + ")\n");
            tell_object(ob, capitalize(name) + " tried to snoop you.\n");
            write("The room is snoop protected.\n");
            return 1;
	    }
    if (ob_exec >= MIN_EXEC)
        tell_object(ob, capitalize(name) + " started to snoop you at " +
                        ctime(time())[11..18] + "\n");
    snoop(ob);
    snooper = query_snoop(ob);
    if (objectp(snooper) && snooper != this_object()) {
        if (exec >= SEC_EXEC)
            write(capitalize(ob->query_real_name()) + " is snooped by "
                          + capitalize(snooper->query_real_name()) + ".\n");
    }
    else if (!ob->query_test_char() && exec<SEC_EXEC)
        log_file("SNOOP", ctime(time()) + " " + capitalize(name) + " started to " +
                 "snoop " + capitalize(ob->query_real_name()) + "(" +
                 file_name(environment(ob)) + ").\n");
    return 1;
}

nomask 
invis()
{
    if (is_invis) {
	tell_object(this_object(), "You are already invisible.\n");
	return 1;
    }
    tell_object(this_object(), "You are now invisible.\n");
    say(convert_message(minvis) + "\n", this_object());
    is_invis = 1;
    return 1;
}

nomask 
vis()
{
    if (!is_invis) {
	tell_object(this_object(), "You are not invisible.\n");
	return 1;
    }
    is_invis = 0;
    tell_object(this_object(), "You are now visible.\n");
    say(convert_message(mvis) + "\n", this_object());
    return 1;
}






nomask
set_invis(i) {
  if (!intp(i)) return 0;
  if (this_player(1)!=this_player()) return 0;
  is_invis=i;
}
