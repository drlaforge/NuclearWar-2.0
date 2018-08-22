nomask valid_write(str) {
string who, file, owner, tmp;
    if (!str)
        return 0;
    owner = name;
    if (previous_object()) {
        tmp = file_name(previous_object());
        if (sscanf(tmp, "players/%s/", who) == 1) {
            if (who == "")
                return 0;
            owner = who;
        }
        if (file_name(previous_object()) == "obj/daemons/newsd") {
            if (str[0] == '/')
                return str[1..strlen(str)];
            else
                return str;
        }
    }
    if (str[0] != '/') {
	/* Prepend the name of the scientist that created the object (if any) */
        if (previous_object()) {
            str = "players/" + owner + "/" + str;
            return str;
        }
        if (current_path != "")
            str = "/" + current_path + "/" + str;
        else
            str = "/" + str;
    }
    if ((!previous_object() || previous_object() == this_object()) && 
        exec >= MIN_EXEC) {
	/*
     * Full access for ministers.  We have to check for
	 * previous_object() or people would be able to cheat.
	 */
        return str[1..strlen(str) - 1];
    }
    if (sscanf(str, "/log/%s", who) == 1) {
        if (exec < SEC_EXEC && who[0] >= 'A' && who[0] <= 'Z')
            return 0;
        return "log/" + who;
    }
    if (sscanf(str, "/players/%s/%s", tmp, file) == 2)
        if (exec >= SEC_EXEC)
            return "players/" + tmp + "/" + file;
    if (sscanf(str, "/obj/%s", file) == 1) {
        if (exec < SEC_EXEC)
            return 0;
        return "obj/" + file;
    }
    if (sscanf(str, "/room/%s", file) == 1) {
        if (exec < SEC_EXEC)
            return 0;
        return "room/" + file;
    }
    if (sscanf(str, "/doc/%s", file) == 1) {
        if (exec < SEC_EXEC)
            return 0;
        return "doc/" + file;
    }
    if (sscanf(str, "/news/%s", file) == 1) {
        if (exec < SEC_EXEC)
            return 0;
        return "news/" + file;
    }
    if (sscanf(str, "/open/%s", file) == 1)
        return "open/" + file;
    if (sscanf(str, "/ftp/%s", file) == 1)
        return "ftp/" + file;
    if (sscanf(str, "/tmp/%s", file) == 1)
        return "tmp/" + file;
    return ACCESS_D->check_access(str, this_player(), 0);
}

nomask 
valid_read(str, lvl) {
string          who, file;
    if (!str)
        return 0;
    if (str[0] != '/')
        str = mk_path(str);
    file = valid_write(str);
    if (file)
        return file;
    if (sscanf(str, "/./%s", file))
        return 0;
    if (file = ACCESS_D->check_access(str, this_player(), 1))
        return file;
    if (sscanf(str, "/players/%s", file) == 1 && exec < MIN_EXEC)
        return 0;
    if (sscanf(str, "/news/%s",file) == 1 && exec < MIN_EXEC) {
        if (previous_object() ? ((source_file_name(previous_object()) != "obj/news_reader") && (file_name(previous_object()) != "obj/simul_efun/count_lines")) : 0)
            return 0;
    }
    if (sscanf(str, "/room/post_dir/%s", file) == 1 && exec < MIN_EXEC)
        return 0;
    /*
     * if (sscanf(str, "/local/%s", file) == 1 && exec < MIN_EXEC) return
     * 0; if (sscanf(str, "/secure/%s", file) == 1 && exec < MIN_EXEC)
     * return 0;
     */
    if (sscanf(str, "/%s", file) == 1)
        return file;
    return 0;
}

/* Grant() must be static. Otherwise you can command() an arch to grant you. */

nomask static 
grant(str)
{
int             flag;
string          tmp, who, path;
    if (!str) {
        show_how_to_grant();
        return 1;
    }
    if (sscanf(str, "-%s %s", tmp, str) == 2) {
        switch (tmp) {
            case "w":
                flag = 1;
                break;
            case "r":
                flag = 0;
                break;
            default:
                show_how_to_grant();
                return 1;
        }
    }
    if (sscanf(str, "%s %s", who, path) != 2) {
        path = "/" + query_path() + "/";
        who = str;
    }
    else if (path == "~")
        path += "/";
    path = valid_read(path);
    if (path != "players/" + query_real_name()) {
        if (file_size("/" + path) == -1) {
            write("Error: \"/" + path + "\" wasn't found.\n");
            return 1;
        }
        if (path != "" && file_size("/" + path) == -2 && path[strlen(path) - 1] != '/')
            path = path + "/";
    }
    else
        path = path + "/";
    if (path == "/")
        path = "";
    if (!(ACCESS_D->add_access("/" + path, who, this_object(), !flag)))
        write("Grant failed. " + who + " does not exist or you have not access\n" +
              "to grant to \"/" + path + "\".\n");
    else
        write(who + " is granted to \"/" + path + "\".\n");
    return 1;
}

nomask static 
ungrant(str)
{
    string          tmp, who, path;
    int             flag;

    if (!str) {
	show_how_to_grant();
	return 1;
    }
    if (sscanf(str, "-%s %s", tmp, str) == 2) {
	switch (tmp) {
	case "w":
	    flag = 1;
	    break;
	case "r":
	    flag = 0;
	    break;
	default:
	    show_how_to_grant();
	    return 1;
	}
    }
    if (sscanf(str, "%s %s", who, path) != 2) {
	path = "/" + query_path() + "/";
	who = str;
    } else if (path == "~")
	path += "/";
    path = valid_read(path);
    if (path != "players/" + query_real_name()) {
	if (path != "" && file_size("/" + path) == -2 && path[strlen(path) - 1] != '/')
	    path = path + "/";
    } else
	path = path + "/";
    if (path == "/")
	path = "";
    if (!(ACCESS_D->rem_access("/" + path, who, this_object(), !flag)))
	write("Ungrant failed. " + who + " does not exist or you have not " +
	      "granted\n" + who + " to \"/" + path + "\".\n");
    else
	write(capitalize(who) + "'s grant to \"/" + path + "\" is removed.\n");
    return 1;
}

nomask          list_acc(who) {
    ACCESS_D->list_acc(who);
    return 1;
}

nomask 
show_how_to_grant()
{
    write("Usage: " + query_verb() + " [-rw] name [where].  (Check with 'help access')\n");
}
