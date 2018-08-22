/*
 *  The news daemon.
 */

#include <tune.h>
#include <news.h>

mapping news_main_map;

reset(arg) {
    if (arg) return;
    restore_object(NEWS_FILE);
    if (!news_main_map) news_main_map = ([]);
}

hook_destruct() {
    save_object(NEWS_FILE);
}

quit() {
    hook_destruct();
}

remove_old_notes() {
    int i,j,date,k;
    string path;
    string *file,fil;

    for (i = 0; i < sizeof(m_indices(news_main_map)); i++) {
	path = get_news_dir(m_indices(news_main_map)[i]);
	for (j = 1; j < sizeof(m_indices(news_main_map[m_indices(news_main_map)[i]][1])); j++) {
	    printf("%O ",path+sprintf("%i",j));
	    if (file = stat(path+sprintf("%i",j))) {
		    printf("%O ",file);
		if ((time()-file[1] > (60*60*24*30))) {
		    if (pointerp(news_main_map[m_indices(news_main_map)[i]][1][j])?(member_array("SECURED",news_main_map[m_indices(news_main_map)[i]][1][j])==-1):1) {
			rm(path+sprintf("%i",j));
			if (member_array(j, m_indices(news_main_map[m_indices(news_main_map)[i]][1]))!= -1)
			    news_main_map[m_indices(news_main_map)[i]][1]=m_delete(news_main_map[m_indices(news_main_map)[i]][1], j);
			k = j+1;
			printf("%O\n", path+sprintf("%i",j));
			while(stat(fil=sprintf("%s%d", path, k))) {
			    write_file(sprintf("%s%d", path, k-1), read_file(fil));
			    rm(fil);
			    printf("%O\n",fil);
//			    mv(fil, sprintf("%s%d", path, k-1));
			    if (member_array(k,m_indices(news_main_map[m_indices(news_main_map)[i]][1]))!=-1) {
				news_main_map[m_indices(news_main_map)[i]][1] += ([ (k-1): news_main_map[m_indices(news_main_map)[i]][1][k] ]);
				news_main_map[m_indices(news_main_map)[i]][1] = m_delete(news_main_map[m_indices(news_main_map)[i]][1], j);
			    }
			    k += 1;
			}
		    }
		}
	    }
	}
    }
    update_groups();
}

void add_group(string group_name, string desc);
string get_news_dir(string name);
int add_note(string group);
void help();

string update_groups() {
    string line, group, desc;
    int number,i,j;
    string *dirs;
    mapping old_map;

    old_map = news_main_map;
    news_main_map = ([]);
    for (number = 1;line = read_file(NEWS_GROUPS, number, 1);number++) {
	if (sscanf(line, "%s\t%s\n", group, desc) == 2) {
	    add_group(group, desc);
	    if (!(dirs = get_dir(get_news_dir(group)))) {
		string *sub, current;
		sub = explode(get_news_dir(group), "/");
		current = NEWS_DIR;
		for (i = 0; i < sizeof(sub); i++) {
		    current += sub[i];
		    if (!get_dir(current))
			if (!mkdir(current)) {
			    printf("Couldn't make dir "+get_news_dir(group)[0..-2]+"\nAborting.\n");
			    return "Error.\n";
			}
		}
	    }
	    dirs = get_dir(get_news_dir(group));
	    if (pointerp(dirs)) {
		for (i = 0; i < sizeof(dirs); i++) {
		    int o;
		    o = add_note(group);
//		    printf("%s %s\n",get_news_dir(group)+dirs[i],get_news_dir(group)+sprintf("%i",o));
//		    mv(get_news_dir(group)+dirs[i],get_news_dir(group)+sprintf("%i",o));
		}
	    }
	}
    }
    if (old_map ? sizeof(m_indices(old_map)) :0) {
	for (i = 0; i < sizeof(m_indices(old_map)); i++)
	    for (j = 1; j < (sizeof(m_indices(old_map[m_indices(old_map)[i]][1]))+1); j++) {
		if (member_array(m_indices(old_map[m_indices(old_map)[i]][1])[j-1], m_indices(news_main_map[m_indices(old_map)[i]][1]))!=-1)
		    news_main_map[m_indices(old_map)[i]][1][j] = old_map[m_indices(old_map)[i]][1][j];
	    }
    }
    save_object(NEWS_FILE);
    return "News groups updated.";
}

void help() {
    printf("\n\
Commands:\n\n\
h/head                List all news groups.\n\
g/goto <group name>   Goto news group.\n\
ignore <group name>   Ignore a group.\n\
attend <group name>   Attend the group again.\n\
l/list                List notes in current group.\n\
read <number>         Read a note.\n\
write/note <subject>  Write a note.\n\
reply <number>        Reply to a note.\n\
delete <number>       Delete a note.\n\
catchup <group name>  Catchup a group.\n\
secure <number>       Secure a note.\n\
unsecure <number>     Unsecure a note.\n\
x/exit/q/quit         Quit.\n\n\
If you press enter on empty line you will read first new note.\n\n");
}

nomask check_access(str) {
    object gang_ob;
    return (str=="min"?(this_player()->query_exec()>=RET_EXEC):
        (str=="pol"?(this_player()->query_exec()>=POL_EXEC):
	     ((str=="trashers"||str=="yuppies"||str=="goth")?
	      ((gang_ob=present("lifestyle",this_player()))
	       ?(((lower_case(gang_ob->query_lifestyle_name())==str) ||
          (this_player()->query_exec()>=POL_EXEC))):
           (this_player()->query_exec()>=POL_EXEC)):
	      1)));
}

nomask int news_write_file(string file, string what)
{
    if (source_file_name(previous_object()) != "obj/news_reader")
	return -1;

    file = ((file[0] == '/') ? file : "/" + file);

    if (!write_file(file, what))
    {
	log_file(NEWS_LOG,
		 sprintf("%'-'79s\n%s : Couldn't open %s for writing\n",
			 "",ctime(time())[4..-9], file));
	return -1;
    }
    return 1;
}

nomask mapping query_map() { return (news_main_map ? news_main_map : ([])); }

nomask string get_news_dir(string name) {
    return NEWS_DIR+implode(explode(name, "."), "/")+"/";
}

nomask void catchup(string group,string orig) {
    string name;
    int note;
    if (source_file_name(previous_object()) != "obj/news_reader")
	return 0;
    if (!group) group = orig;
    else
	if (member_array(group,m_indices(news_main_map))==-1) {
	    printf("No such group.\n");
	    return;
	}
    for (note = 1;note<=sizeof(m_indices(news_main_map[group][1]));note++) {
	if (member_array((name = (string)this_player()->query_real_name()),
			 news_main_map[group][1][note])==-1)
	    news_main_map[group][1][note] += ({ name });
    }
    printf("You've caught up with all the news in %s\n",group);
}

nomask void secure_note(string group,string note) {
    int note_nr;

    if (source_file_name(previous_object()) != "obj/news_reader")
	return 0;
    note_nr = atoi(note);
    if (member_array(note_nr,m_indices(news_main_map[group][1]))==-1) {
	printf("Note doesn't exist.\n");
	return;
    }
    news_main_map[group][1][note_nr] += ({ "SECURED" });
    printf("Note secured.\n");
}
nomask void unsecure_note(string group,string note) {
    int note_nr;

    if (source_file_name(previous_object()) != "obj/news_reader")
	return 0;
    note_nr = atoi(note);
    if (member_array(note_nr,m_indices(news_main_map[group][1]))==-1) {
	printf("Note doesn't exist.\n");
	return;
    }
    if (member_array("SECURED",news_main_map[group][1][note_nr])==-1) {
	printf("Note isn't secured.\n");
	return;
    }
    news_main_map[group][1][note_nr] -= ({ "SECURED" });
    printf("Note unsecured.\n");
}

nomask int read_note(string group, int note) {
    string news_dir;
    string name;
    int i;

    if (source_file_name(previous_object()) != "obj/news_reader")
	return 0;

    if (member_array(note, m_indices(news_main_map[group][1])) == -1) {
	printf("Note doesn't exist.\n");
	return 0;
    }
    if (member_array((name = (string)this_player()->query_real_name()),
		     news_main_map[group][1][note])==-1)
	news_main_map[group][1][note] += ({ name });
    printf("\n");
    news_dir = get_news_dir(group);
    previous_object()->more_note(news_dir+note,note);
//    save_object(NEWS_FILE);
    return 1;
}


nomask string read_first_new_note(string NEWS_group) {
  string tmp;
  int i, j,flagga;
  
  flagga = 0;
  for (i=0;i<sizeof(m_indices(news_main_map));i++) {
      sscanf(m_indices(news_main_map)[i], "%s.%*s",tmp);
      if (check_access(tmp)) {
	  for (j=1;j<(sizeof(m_indices(news_main_map[m_indices(news_main_map)[i]][1]))+1);j++) {
	      if (member_array(this_player()->query_real_name(), news_main_map[m_indices(news_main_map)[i]][1][j]) == -1) {
		  flagga = 1;
                if (m_indices(news_main_map)[i] != NEWS_group) {
		      write("\nChanging to group ["+m_indices(news_main_map)[i]+"]\n");
                     previous_object()->set_news_group(m_indices(news_main_map)[i]);
          }
		  read_note(NEWS_group=m_indices(news_main_map)[i], j);
		  return NEWS_group;
	      }
	  }
      }
  }	
  if (flagga)
      return NEWS_group;
  else return "";
}

nomask reply_to_note(note, group) {
    string subject;
    int note_number;

    if (source_file_name(previous_object()) != "obj/news_reader")
	return;

    if (!note) {
	write("reply <number>\n");
	return;
    }
    note_number = atoi(note);
    if (!note_number) {
	write("Not an existing note.\n");
    }
    if (member_array(note_number, m_indices(news_main_map[group][1])) == -1) {
	write("Not an existing note.\n");
	return;
    }
    sscanf(read_file(get_news_dir(group)+note, 1, 1), "%s [%*s]",subject);
    subject = "Re: "+subject;
    previous_object()->write_note(subject);
    return;
}




nomask mixed *get_files(string str) {
    string *files;
    if (!(files = get_dir(str))) return (mixed *)0;
    return sort_array(files, "sorter", this_object());
}

nomask sorter(string str, string old) { 
    return (atoi(str) > atoi(old));
}

nomask gsort(string str, string str2) {
    return (str > str2);
}


nomask void list_groups() {
    string line, group_name, group_desc, tmp;
    string *player_ignore, *tmp_array;
    int i,size;
    
    if (source_file_name(previous_object()) != "obj/news_reader")
	return;

    tmp_array = sort_array(m_indices(news_main_map), "gsort", this_object());

    player_ignore = (string *)this_player()->query_NEWS_ignore();
    printf("\nNews groups available:\n");
    for (i = 0;i < sizeof(m_indices(news_main_map));i++) {
	sscanf(tmp_array[i], "%s.%*s", tmp);
	if (check_access(tmp))
            printf("%-6s%-4s%-20s%s\n",
		   (size=sizeof(news_main_map[tmp_array[i]][1]))?
		   (" ("+size+") "):"",
		   (member_array(tmp_array[i],
				 player_ignore)==-1?
		    "   " : " * "),
		   tmp_array[i], 
		   news_main_map[tmp_array[i]][0]);
    }
    printf("\n");
}

nomask void list_notes(string NEWS_group) {
    string subject, news_dir;
    mixed *dir;
    int i;

    if (!NEWS_group || (NEWS_group == ""))
	return;
    printf("\n");
    news_dir = get_news_dir(NEWS_group);
    if (!(dir = get_files(news_dir))?1:(!sizeof(dir))) {
	write("No notes.\n");
	return;
    }
    for (i=0;i<sizeof(dir);i++) {
	sscanf(read_file(news_dir+dir[i], 1, 1),
	       "%s\n", subject);
	printf("%s %2i. %s\n",(member_array(this_player()->query_real_name(), news_main_map[NEWS_group][1][atoi(dir[i])])==-1)?"N":((member_array("SECURED", news_main_map[NEWS_group][1][atoi(dir[i])])==-1)?" ":"S"), i+1, subject);
    }
    printf("\n");
}


nomask int add_note(string group) {
    int number;

    news_main_map[group][1] += ([ 
			    (number=(sizeof(m_indices(news_main_map[group][1]))+1)): 
			    ({ this_player()->query_real_name() })
			    ]);
//    save_object(NEWS_FILE);
    return number;
}

nomask void add_group(string group_name, string desc) {
    if (news_main_map)
	news_main_map += ([ group_name : ({ desc , ([]) }) ]);
    else
	news_main_map = ([ group_name : ({ desc, ([]) }) ]);
//    save_object(NEWS_FILE);
}

//void create_group(string str) {
//    string group_name, desc;
//
//    if (sscanf(str, "%s %s", group_name, desc) != 2) {
//	write("c <group name> <description>\n");
//	return;
//    }
//    if (member_array(group_name, m_indices(news_main_map)) != -1) {
//	write("Group already exist.\n");
//	return;
//    }
//    add_group(group_name, desc);
//}

nomask string select_group(string group_name,string NEWS_group) {
    int group_number;
    string grp;

    if (!group_name) {
	write("g <group name>\n");
	return NEWS_group;
    }
    if (member_array(group_name, m_indices(news_main_map)) == -1) {
	write("Group doesn't exist.\n");
	return NEWS_group;
    }
    sscanf(group_name, "%s.%*s", grp);
    if (!check_access(grp)) {
	write("Group doesn't exist.\n");
	return NEWS_group;
    }
    write(group_name+" group selected.\n");
    return group_name;
}    

/*
nomask string *get_files(string str) {
    string *files;
    if (!(files = get_dir(str))) return ({});
    return sort_array(files, "sorter", this_object());
}

nomask int sorter(string str, string old) { 
    return (atoi(str) > atoi(old));
}
*/
nomask string init_note(string group, string subject) {
    string file_name, news_dir;
    string *files;
    int exec;

    if (source_file_name(previous_object()) != "obj/news_reader")
	return "";
    news_dir = get_news_dir(group);
    add_note(group);
    file_name = sprintf("%s%i", news_dir, 
		   sizeof((files = get_files(news_dir))?files:({}))+1);
    write_file(file_name,
	       sprintf("%-42s [%-16s%s]\n",
		       (strlen(subject) > 41 ? subject[0..41] : subject),
               sprintf("%s%s",capitalize((string)this_player()->query_real_name()),((exec=(int)this_player()->query_exec())>=POL_EXEC)?"   ":sprintf(" (%i) ",this_player()->query_eval())),
		       extract(ctime(time()), 4, 15)));
    write_file(file_name,sprintf("%'-'79s\n",""));
	       
    return file_name;
}

nomask void write_line(string file_name, string line) {
    if (source_file_name(previous_object()) != "obj/news_reader")
	return;
    write_file(file_name, line);
}

nomask void write_note(string file_name, string *note) {
    int i;
    if (source_file_name(previous_object()) != "obj/news_reader")
	return;
    if (note) {
	write_file(file_name, implode(note, ""));
    }
}
    
nomask string check_owner(string file) {
    string name;
    name = ((sscanf(read_file(file, 1, 1), "%*s[%s %*s]", name) == 3) ?
	    lower_case(name) : "");
    return name;
}

nomask void delete_note(string note_str_nr, string group) {
    string file, dir;
    int note_number;

    if (source_file_name(previous_object()) != "obj/news_reader")
	return;
    if (!note_str_nr) {
	printf("delete <note nr>\n");
	return;
    }
    note_number = atoi(note_str_nr);
    if (!note_number) {
	printf("Not an existing note.\n");
	return;
    }
    file = sprintf("%s%d", (dir=get_news_dir(group)), note_number);
    if (!stat(file)) {
	printf("Note doesn't exist.\n");
	return;
    }
    if (this_player()->query_exec() < SEC_EXEC)
	if (check_owner(file) != (string)this_player()->query_real_name()) {
	    printf("Not owner.\n");
	    return;
	}
    rm(file);
    if (member_array(note_number, m_indices(news_main_map[group][1]))!= -1)
	news_main_map[group][1]=m_delete(news_main_map[group][1], note_number);
    else write("Vaffan\n");
    note_number++;
    while(stat(file=sprintf("%s%d", dir, note_number))) {
	write_file(sprintf("%s%d", dir, note_number-1),read_file(file));
	rm(file);
//	mv(file, sprintf("%s%d", dir, note_number-1));
	if (member_array(note_number,m_indices(news_main_map[group][1]))!=-1) {
	    news_main_map[group][1] += ([ (note_number-1):
					 news_main_map[group][1][note_number]
					 ]);
	    news_main_map[group][1] = m_delete(news_main_map[group][1], note_number);
	}
	note_number += 1;
    }
    printf("Ok. Note deleted.\n");
//    save_object(NEWS_FILE);
}

nomask abort_note(note, group) {
    int number;
    string *ex;

    if (source_file_name(previous_object()) != "obj/news_reader")
	return;
    ex = explode(note, "/");
    if (!ex) return;
    number = ex[sizeof(ex)-1];
    if (stat(note)) rm(note);
    news_main_map[group][1] = m_delete(news_main_map[group][1], atoi(number));
}



