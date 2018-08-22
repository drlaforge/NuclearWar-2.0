#include <lines.h>
nomask query_line(linenumber) { return (line & linenumber); }

nomask set_line(linenumber) {
    if (line & linenumber)
        line &= ~linenumber;
	else
        line |= linenumber;
}

nomask line_send(str) { return "obj/daemons/lined"->line_send(str); }

init_lines() {
string *ind;
int i;
    add_action("lines_plus","lines+");
    add_action("lines_min","lines-");
    ind=m_indices(LINES);
    i=sizeof(ind);
    while (i--) {
        add_action("line_send",ind[i],1);
        if (!LINES[ind[i]][1]||
          (stringp(LINES[ind[i]][1])&&call_other(this_object(),LINES[ind[i]][1], this_object())))
            set_line(LINES[ind[i]][0]);
    }
}

lines_plus() {
    write("You turn all communication lines on.\n");
    line = 0xffffffff;
    return 1;
}
lines_min() {
    write("You turn all communiction lines off.\n");
    line = 0;
    return 1;
}
