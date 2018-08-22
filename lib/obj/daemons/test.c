#include <socket.h>

int s;

id(str)
{
    return str = "mtester"; 
}

get()
{
    return 1;
}

init()
{
    add_action("hum", "bah");
}

foo () {}

got_it(int fd, string st)
{
    tell_object(find_player("ergus"), st + "\n");
    call_out("closeup", 10);
}

closeup()
{
    socket_close(s);
    remove_call_out("closeup");
}

hum(str)
{
    int e;
    s = socket_create(1, "hum", "hum");
    if (s < 0)
	return write("Socket_create sket sig\n");
    e = socket_connect(s, "129.79.254.191 2627", "got_it", "foo");
    if (e < 0)
	return write("socket_connect sket sig\n");
    socket_write(s, "DEFINE who");
    return 1;
}
