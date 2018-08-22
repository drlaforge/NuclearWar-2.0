/*  newsd.c -- last update 26/10/95  version II                             */
/*  Original by Heman, re-written by Cobra                                  */

#include <exec.h>
#include <news.h>

mapping main_news_map=([]);

query_map() {
    if ("/"+source_file_name(previous_object())==NEWSREADER)
        return (main_news_map? main_news_map: ([]));
}

create() {
    restore_object(NEWS_FILE);
    if (!mappingp(main_news_map))
        main_news_map=([]);
}

quit() {
    hook_destruct();
}

hook_destruct() {
    save_object(NEWS_FILE);
}

nomask query_help() {
    printf("\n   COMMANDS IN NEWS:\n\n\
h[ead]                      Give an overview of all newsgroups.\n\
i[gnore] <group name>       Allows you to ignore group <group name>.\n\
a[ttend] <group name>       Revoke a decision to ignore group <group name>.\n\
g[oto] <group name>         Switch your group to <group name>.\n\
l[ist] [<group name>]       List all notes [in group <group name>].\n\
[r[ead]] <note number>      Read note <note number>. Yes, it's that easy!\n\
w[rite]/n[ote] <subj>       Write a note with title <subj>.\n\
re[ply] <note number>       Reply to note <note number>.\n\
d[elete] <note number>      Delete note <note number> - if it's yours.\n\
c[atchup] [<group name>]    Catch up with the news [in group <group name>].\n\
s[ecure] <note number>      Secure note <note number> against auto-deletion.\n\
u[nsecure] <note number>    Lift the security of note <note number>.\n\
%s\
q[uit]/[e]x[it]             Exit the news system. 'Nuff said.\n\
If you press <ENTER> without a command, you will be shown the next new note.\n\n",
        (this_player()->query_exec()>=MIN_EXEC?
"create <group name>         Create a new group called <group name>.\n": ""));
}


nomask update_groups() {
string line, groupname, desc, *notes;
int i, j, group;
mapping old;
    old=main_news_map;
    main_news_map=([]);
    for (group=1; line=read_file(GROUP_FILE, group, 1); group++) {
        if (sscanf(line, "%s@%s\n", groupname, desc)==2) {
            add_group(groupname, desc);
            if (!(notes=get_dir(get_dirname(groupname)))) {
                string *sub, current;
                sub=explode(get_dirname(groupname), "/");
                current="";
                for (i=0; i<sizeof(sub); i++) {
                    current+=("/"+sub[i]);
                    if (!get_dir(current) && !mkdir(current)) {
                        printf("Couldn't make dir %s. Aborting...\n", current);
                        return "ERROR";
                    }
                }
            }
            notes=get_dir(get_dirname(groupname));
            if (pointerp(notes) && sizeof(notes)) {
                for (i=0; i<sizeof(notes); i++) {
                    int o;
                    o=add_note(groupname);  // Old had a move here - ask Heman
                }
            }
        }
    }
    if (old? m_sizeof(old): 0) {
        for (i=0; i<m_sizeof(old); i++)
            for (j=1; j<(m_sizeof(old[m_indices(old)[i]][1])+1); j++) {
                if (member_array(m_indices(old[m_indices(old)[i]][1])[j-1],
                    m_indices(main_news_map[m_indices(old)[i]][1]))!=-1)
                    main_news_map[m_indices(old)[i]][1][j]=
                        old[m_indices(old)[i]][1][j];  // Odd, ask Heman
            }
    }
    save_object(NEWS_FILE);
    return "Newsgroups updated";
}

nomask add_group(groupname, desc) {
    if (mappingp(main_news_map) && stringp(groupname) && stringp(desc))
        main_news_map+=([groupname: ({desc, ([]) }) ]);
}

nomask list_groups() {
string *ignored, *tmp_arr, tmp;
int i;
    if ("/"+source_file_name(previous_object())!=NEWSREADER) return 0;
    tmp_arr=sort_array(m_indices(main_news_map), "groupsort", this_object());
    ignored=this_player()->query_NEWS_ignore();
    write("\nNewsgroups available:\n");
    for (i=0; i<sizeof(tmp_arr); i++) {
        sscanf(tmp_arr[i], "%s.%*s", tmp);
        if (check_access(tmp))
            printf(" (%2d): %3s %-'.'20s%s\n",
                sizeof(main_news_map[tmp_arr[i]][1]),
                (member_array(tmp_arr[i], ignored)==-1? "": "[I]"),
                tmp_arr[i],
                main_news_map[tmp_arr[i]][0]);
    }
    if (!sizeof(tmp_arr)) write("None.\n");
    else write("\n");
}

nomask groupsort(arg1, arg2) {
    return (arg1>arg2);
}

nomask create_group(arg) {
string groupname, desc;
    if (this_player()->query_exec()<MIN_EXEC) 
        write("New newsgroups can only be created by ministers.\n");
    else if (sscanf(arg, "%s %s", groupname, desc)!=2) 
        write("Syntax: create <group name> <description>\n");
    else if (member_array(groupname, m_indices(main_news_map))!=-1) 
        printf("The newsgroup [%s] already exists.\n", groupname);
    else {
        news_write_file(GROUP_FILE, groupname+"@"+desc);
        update_groups();
        printf("New newsgroup [%s] added.\n", groupname);
    }
}

nomask select_group(new, current) {
string tmp;
    if ("/"+source_file_name(previous_object())!=NEWSREADER) return 0;
    if (!new) {
        write("Syntax: g[oto] <group name>\n");
        return current;
    }
    if (member_array(new, m_indices(main_news_map))==-1) {
        printf("The newsgroup [%s] does not exist.\n", new);
        return current;
    }
    sscanf(new, "%s.%*s", tmp);
    if (!check_access(tmp)) {
        printf("You have no access to [%s].\n", new);
        return current;
    }
    printf("Switching to [%s].\n", new);
    return new;
}

nomask catchup_group(groupname, current) {
string name;
int i;
    if ("/"+source_file_name(previous_object())!=NEWSREADER) return 0;
    if (!groupname) groupname=current;
    else if (member_array(groupname, m_indices(main_news_map))==-1) return
        !printf("The newsgroup [%s] does not exist.\n", groupname);
    for (i=1; i<=m_sizeof(main_news_map[groupname][1]); i++)
        if (member_array((name=this_player()->query_real_name()),
            main_news_map[groupname][1][i])==-1)
            main_news_map[groupname][1][i]+=({name});
    printf("You catch up with the news in newsgroup [%s].\n", groupname);
}

nomask list_notes(groupname, current) {
string subject, dir, tmp;
mixed *notes;
int i;
    if ("/"+source_file_name(previous_object())!=NEWSREADER) return 0;
    if (!groupname) groupname=current;
    if (!stringp(groupname) || groupname=="")
        return;
    if (member_array(groupname, m_indices(main_news_map))==-1) return
        !printf("The newsgroup [%s] does not exist.\n", groupname);
    sscanf(groupname, "%s.%*s", tmp);
    if (!check_access(tmp)) return
        !printf("You have no access to [%s].\n", groupname);
    dir=get_dirname(groupname);
    if (!(notes=get_files(dir)) || !sizeof(notes)) return
        !printf("There are no notes in newsgroup [%s].\n", groupname);
    printf("\nNotes in newsgroup [%s]:\n", groupname);
    for (i=0; i<sizeof(notes); i++) {
        sscanf(read_file(dir+notes[i], 1, 1), "%s\n", subject);
        printf("%s %2d. %s\n",
            (member_array(this_player()->query_real_name(),
                main_news_map[groupname][1][atoi(notes[i])])==-1? "N":
            (member_array(SECURE, main_news_map[groupname][1][atoi(notes[i])])
                ==-1? " ": "S")), i+1, subject);
    }
    write("\n");
}

nomask get_files(arg) {
string files;
    if (!(files=get_dir(arg))) return 0;
    return sort_array(files, "notesort", this_object());
}

nomask notesort(arg1, arg2) {
    return (atoi(arg1)>atoi(arg2));
}

nomask add_note(groupname) {
int i;
    if (mappingp(main_news_map) && stringp(groupname))
        main_news_map[groupname][1]+=([
            (i=(m_sizeof(main_news_map[groupname][1])+1)):
                ({this_player()->query_real_name()})
        ]);
    return i;
}

nomask secure_note(groupname, note) {
    if ("/"+source_file_name(previous_object())!=NEWSREADER) return 0;
    if (member_array(atoi(note), m_indices(main_news_map[groupname][1]))==-1)
        return printf("There is no note %s.\n", note);
    main_news_map[groupname][1][atoi(note)]+=({SECURE});
    printf("Note %s of newsgroup [%s] is now secured.\n", note, groupname);
}

nomask unsecure_note(groupname, note) {
    if ("/"+source_file_name(previous_object())!=NEWSREADER) return 0;
    if (member_array(atoi(note), m_indices(main_news_map[groupname][1]))==-1)
        return !printf("There is no note %s.\n", note);
    if (member_array(SECURE, main_news_map[groupname][1][atoi(note)])==-1)
        return !printf("Note %s was not secured.\n", note);
    main_news_map[groupname][1][atoi(note)]-=({SECURE});
    printf("Note %s of newsgroup [%s] is no longer secured.\n", note, groupname);
}

nomask check_note_owner(file) {
string owner;
    owner=((sscanf(read_file(file, 1, 1), "%*s[%s %*s]", owner)==3)?
        lower_case(owner): "");
    return owner;
}

nomask get_dirname(groupname) {
    return NEWS_DIR+implode(explode(groupname, "."), "/")+"/";
}

nomask check_access(arg) {
object lifestyle;
    switch(arg) {
        case "min":
            return (this_player()->query_exec()>=RET_EXEC);
        case "pol":
            return (this_player()->query_exec()>=POL_EXEC);
        case "trasher":
        case "yuppie":
        case "goth":
            return (((lifestyle=present("lifestyle", this_player()))?
                lower_case(lifestyle->query_lifestyle_name())==arg: 0) ||
                this_player()->query_exec()>=POL_EXEC);
        default:
            return 1;
    }
}

nomask news_write_file(file, what) {
    if ("/"+source_file_name(previous_object())!=NEWSREADER &&
        "/"+source_file_name(previous_object())!=NEWSDAEMON) return -1;
    file=(file[0]=='/'? file: "/"+file);
    if (!write_file(file, what)) {
        log_file(NEWS_LOG, sprintf("%'-'-79s\nCould not write to %s\n",
            ctime(time())[4..-9], file));
        return -1;
    }
    return 1;
}

nomask read_note(groupname, nr, prev) {
string name;
    if ("/"+source_file_name(previous_object())!=NEWSREADER &&
        "/"+source_file_name(previous_object())!=NEWSDAEMON) return 0;
    if (member_array(nr, m_indices(main_news_map[groupname][1]))==-1) {
        printf("There is no note %d in newsgroup [%s].\n", nr, groupname);
        (prev? prev: previous_object())->commands(1);
        return;
    }
    name=this_player()->query_real_name();
    if (member_array(name, main_news_map[groupname][1][nr])==-1)
        main_news_map[groupname][1][nr]+=({name});
    write("\n");
    previous_object()->more_note(get_dirname(groupname)+nr, nr);
    return 1;
}

nomask read_first_new_note(groupname, *ignored) {
int i, j, flag;
string tmp;
    for (i=0, flag=0; i<m_sizeof(main_news_map); i++) {
        if (member_array(m_indices(main_news_map)[i], ignored)==-1) {
        sscanf(m_indices(main_news_map)[i], "%s.%*s", tmp);
        if (check_access(tmp)) {
            for (j=1;j<(m_sizeof(
                main_news_map[m_indices(main_news_map)[i]][1])+1); j++) {
                if (member_array(this_player()->query_real_name(),
                    main_news_map[m_indices(main_news_map)[i]][1][j])==-1) {
                    flag=1;
                    if (m_indices(main_news_map)[i]!=groupname) {
                        groupname=m_indices(main_news_map)[i];
                        printf("Switching to [%s].\n", groupname);
                        previous_object()->set_newsgroup(groupname);
                    }
                    read_note(groupname, j, previous_object());
                    return groupname;
                }
            }
        }
    } /* ADDED FOR CHECKING IGNORANCE */
    }
    if (flag) return groupname;
    else return "";
}

nomask reply_to_note(note, groupname) {
string subject;
int nr;
    if ("/"+source_file_name(previous_object())!=NEWSREADER) return 0;
    if (!note || !(nr=atoi(note))) {
        write("Syntax: R/reply <note number>\n");
        previous_object()->commands(1);
        return;
    }
    if (member_array(nr, m_indices(main_news_map[groupname][1]))==-1) {
        printf("There is no note %d in newsgroup [%s].\n", nr, groupname);
        previous_object()->commands(1);
        return;
    }
    sscanf(read_file(get_dirname(groupname)+note, 1, 1), "%s [%*s]", subject);
    subject="Re: "+subject;
    previous_object()->write_note(subject);
    return;
}

nomask init_note(subject) {
    if ("/"+source_file_name(previous_object())!=NEWSREADER || !stringp(subject))
        return;
    return sprintf("%-:42s [%-16s%s]\n%'-'79s", subject, sprintf("%s%s",
        capitalize(this_player()->query_real_name()), (this_player()->
            query_exec()? "": sprintf(" (%d)", this_player()->query_eval()))),
            ctime(time())[4..15], "");
}

nomask write_note(newsgroup, *note) {
string dir, file;
int i;
    if ("/"+source_file_name(previous_object())!=NEWSREADER || !pointerp(note)) return;
    dir=get_dirname(newsgroup);
    file=sprintf("%s%i", dir, ((i=sizeof(get_files(dir)))? i: 0)+1);
    i=news_write_file(file, implode(note, "\n"));
    if (i==1) add_note(newsgroup);
    save_object(NEWS_FILE);
    return i;
}

nomask delete_note(note, groupname) {
string dir, file;
int nr;
    if ("/"+source_file_name(previous_object())!=NEWSREADER) return;
    if (!note) return
        !write("Syntax: d[elete] <note number>\n");
    nr=atoi(note);
    if (!nr) return
        !write("Syntax: d[elete] <note number>\n");
    file=sprintf("%s%d", dir=get_dirname(groupname), nr);
    if (!stat(file)) return
        !printf("There is no note %d in newsgroup [%s].\n", nr, groupname);
    if (this_player()->query_exec()<SEC_EXEC && check_note_owner(file)!=
        this_player()->query_real_name()) return
        !printf("Note %d in newsgroup [%s] isn't yours.\n", nr, groupname);
    rm(file);
    if (member_array(nr, m_indices(main_news_map[groupname][1]))==-1)
        log_file(NEWS_LOG, sprintf("%'-'-79s\nDeleted non-existing note %s\n",
            ctime(time())[4..-9], file));
    else
        main_news_map[groupname][1]=m_delete(main_news_map[groupname][1], nr);
    nr++;
    while (stat(file=sprintf("%s%d", dir, nr))) {
        write_file(sprintf("%s%d", dir, (nr-1)), read_file(file));
        rm(file);
        if (member_array(nr, m_indices(main_news_map[groupname][1]))!=-1) {
            main_news_map[groupname][1]+=([(nr-1):
                main_news_map[groupname][1][nr]]);
            main_news_map[groupname][1]=m_delete(main_news_map[groupname][1],
                nr);
        }
        nr++;
    }
    printf("Deleted note %s from newsgroup [%s].\n", note, groupname);
    save_object(NEWS_FILE);
}

/*
remove_old_notes() {
string dir, file, tmp, *info;
int i, j, k;
    for (i=0; i<m_sizeof(main_news_map); i++) {
        tmp=m_indices(main_news_map)[i];
        dir=get_dirname(tmp);
        for (j=1; j<m_sizeof(main_news_map[tmp][1]); j++) {
            if (info=stat(sprintf("%s%i", dir, j))) {
                if (time()-info[1] > AUTODELETE_TIME) {
                    if (pointerp(main_news_map[tmp][1][j])? member_array(SECURE,
                        main_news_map[tmp][1][j])==-1: 1) {
                        rm(sprintf("%s%i", dir, j));
                        if (member_array(j,
                            m_indices(main_news_map[tmp][1]))!=-1)
                            main_news_map[tmp][1]=
                                m_delete(main_news_map[tmp][1][i], j);
                        k=j+1;
                        while (stat(file=sprintf("%s%d", dir, k))) {
                            write_file(sprintf("%s%d", dir, (k-1)),
                                read_file(file));
                            rm(file);
                            if (member_array(k,
                                m_indices(main_news_map[tmp][i]))!=-1) {
                                main_news_map[tmp][1]+=([ (k-1):
                                    main_news_map[tmp][1][k] ]);
                                main_news_map[tmp][1]=
                                    m_delete(main_news_map[tmp][i][1], k);
                            }
                            k+=1;
                        }
                    }
                }
            }
        }
    }
    update_groups();
}
*/
