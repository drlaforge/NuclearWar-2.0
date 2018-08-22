/*
   More a file.
   Ergus 940206
   */

#define CHUNK       (variables["lines"]?atoi(variables["lines"]):16)
                                            /* Number of rows per return */
static int more_line;
static string file, tmp;

nomask help() {
    write("\
?           - This help\n\
u           - Up "+CHUNK+" lines\n\
d           - Down "+CHUNK+" lines (same as <ENTER>)\n\
+[number]   - Go [number] lines down\n\
-[number]   - Go [number] lines up\n\
![command]  - Do a command in more\n\
/[word]     - Search for word in file\n\
q           - Quit\n");
    return 1;
}

nomask int search_file(string what) {
    string searchstring, *expl, match1, match2;
    int tmp_line;

    tmp_line = more_line;
    while ((searchstring = read_file(file, tmp_line, CHUNK))) {
        int i;
        expl = explode(searchstring, "\n");
        i = sizeof(expl);
        while(i) {
            if (sscanf(expl[i - 1], "%s" + what + "%s", match1, match2) == 2)
                return (tmp_line + i - 1);
            i--;
        }
        tmp_line += CHUNK;
    }
}

nomask more(string str) {
    file = mk_path(str);
    if (file_size(file) < 0) {
        write("That file doesn't exists.\n");
        return 1;
    }
    if(!valid_read(file)) {
        write("Access denied.\n");
        return 1;
    }
    more_line = 1;
    if (cat(file, more_line, CHUNK)<CHUNK) return 1;
    input_to("even_more");
    write("More: [!,/[word],+[nr],-[nr],d,u,q,?] (line " + (CHUNK) + ") ");
    return 1;
}

nomask even_more(string what) {
    int catflag;
    catflag = 1;
    if (what == "")
        what = "d";
    switch (what[0]) {
        case 'u':
            more_line -= CHUNK;
            if (more_line < 1)
                more_line = 1;
            break;
        case '\n':
        case 'd':
            more_line += CHUNK;
            break;
        case '?':
            help();
            catflag =0;
            break;
        case '/':
            more_line = search_file(what[1..sizeof(what)]);
            break;
        case '+':
            if (!atoi(what[1..sizeof(what)])) {
                help();
                catflag = 0;
            }
            else
                more_line += atoi(what[1..sizeof(what)]);
            break;
        case '-':
            if (!atoi(what[1..sizeof(what)])) {
                help();
                catflag = 0;
            }
            else
                more_line = ((more_line - atoi(what[1..sizeof(what)])) < 1 ?
                 (more_line - atoi(what[1..sizeof(what)])) : 0);
            break;
        case 'q':
            return write("Ok.\n");
            break;
        default:
            write("More: not a valid command; type '?' for help.\n");
            catflag = 0;
            break;
    }
    if (catflag == 1) {
        int i;
        i=cat(file,more_line,CHUNK);
         if (i==0) return write("EOF\n");
         else if (i<CHUNK) return 1;
    }
    write("More: [!,/[word],+[nr],-[nr],d,u,q,?] (line "+(more_line + CHUNK - 1) + ") ");
    input_to("even_more");
    return 1;
}
	
static private object after_ed_obj;
static private string after_ed_fun;
static int in_ed;

nomask edit_mode(file, fun, obj) {
    if (!file) return;
    if (this_player() != this_object()) return;
    if (!exec) return;
    if (stringp(fun) && objectp(obj)) {
        after_ed_fun = fun;
        after_ed_obj = obj;
    }
    edit(file);
}

nomask static edit(file) {
	string tmp_file;
    if (!file) {
	    in_ed = 1;
	    ed();
	    in_ed = 0;
	    return 1;
	}
	file = mk_path(file);
	file = valid_read(file);
    if (!file) {
		write("Access denied.\n");
		return 1;
	}
	in_ed = 1;
	tmp = file;
#ifdef MUDOS
	ed("/"+file, "leave_ed");
#else
	ed(file, "leave_ed");
#endif
	in_ed = 0;
	return 1;
}

nomask query_ed_file() { return tmp; }

nomask query_in_ed() { return in_ed; }

nomask leave_ed() {
    if (!after_ed_fun || !after_ed_obj)
        return;
    call_other(after_ed_obj, after_ed_fun, this_object());
    after_ed_fun = after_ed_obj = 0;
}

set_path(path) {
    if(this_player() == this_object())
        current_path = path;
    return current_path;
}

nomask query_path() { return current_path; }

set_plan(str) {
    if(str)
        plan_desc = str;
    return plan_desc; 
}

nomask mk_path(path) { return resolv_path(path, query_path(), this_object()); }

set_paths(arg) {
    if(object_name(previous_object()) == "cmds/sci/paths") {
        paths = arg;
        if(!mappingp(paths) || !m_sizeof(paths))
            paths = 0;
    }
}

query_paths() { return paths; }
