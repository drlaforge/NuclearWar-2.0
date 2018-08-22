#include <mail.h>

static mapping aliases;
static int lastupdated;
mapping mails;

mail(string from, string to, string cc, string subject, string message) {
    string *To;
    int cnt;
    To = check_aliases((cc ? explode(to, ",") + explode(cc, ",") : 
			explode(to, ",")));
    cnt = sizeof(To) - 1;
    write("Sending mail to: " + to +(cc ?  "," + cc : "") + "\n");
    while (cnt >= 0) {
       if (!restore_object(MAILDIR + "/" + To[cnt]))
	   mails = ([ ]);
       mails += ([sprintf("%-5s Date: %s", subject, DATE):
		  ({NEW, from, implode(To - ({To[cnt]}), ", ") , message})]);
       save_object(MAILDIR + "/" + To[cnt]);
       report_new(To[cnt]);
       cnt--;
   }
}

check_aliases(string *recipents) {
    string *ret;
    int i;
    if (!lastupdated || lastupdated < file_time(MAIL_ALIASES))
	update_aliases();
    i = sizeof(recipents) - 1;
    ret = ({});
    while(i >= 0) {
	if (aliases[recipents[i]]) 
	    ret += filter_array(aliases[recipents[i]], "filter_recipents",
				this_object(), recipents);
	else
	    ret += ({recipents[i]});
	i--;
    }
    return ret;
}

filter_recipents(string who, string *list) {
    return (member_array(list, who) != -1 ? 0 : who);
}

report_new(string who) {
    object whom;
    whom = find_player(who);
    if (whom) 
	tell_object(whom, "You got NEW mail in the post office.\n");
}

update_aliases() {
    string out_file, out, crap;
    string *out_arr, *tmp;
    int i, size;
    i = 1;
    out_file = "";
    while (out = read_file(MAIL_ALIASES, i, 500)) {
        out_file += out;
        i += 500;
    }
    out_arr = explode(out_file, "\n");
    out_arr = map_array(out_arr, "sort_arr", this_object());
    out_arr = filter_array(out_arr, "filter_arr", this_object());
    i = 0;
    size = sizeof(out_arr);
    aliases = ([ ]);
    while (i < size) {
        tmp = explode(out_arr[i], ": ");
        aliases = aliases + ([tmp[0]: tmp[1]]);
        i++;
    }
    lastupdated = file_time(MAIL_ALIASES);
}

sort_arr(string str) {
    string back, crap;
    if (sscanf(str, "%s#%s", back, crap) == 2)
        return back;
    return str;
}

filter_arr(string str) {
    return (stringp(str) && strlen(str) ? 1 : 0 );
}
