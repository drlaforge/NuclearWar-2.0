/* 
   Webster 
   
   Code: Ergus (I think )

   */


#include <socket.h>
#include <socket_errors.h>

#define PORT "2627"
#define HOST "129.79.254.191"
#define LOGFILE "WEBSTER_ERRS"
#define INPUT 0x1
#define BUSY 0x2
#define CONNECTED 0x4

mapping sockets = ([ ]);

create_link(object tp, string mode, string word, string func)
{
    int socketfd;
    int err;
    if ((socketfd = socket_create(STREAM,"read_callback", "close_call_back")) 
	< 0) 
    {
	write("webster: socket_create: " + socket_error(socketfd) + "\n");
	return -1;
    }
    if ((err = socket_connect(socketfd, HOST + " " + PORT, "read_callback",
			  "write_callback")) != EESUCCESS)
    {
	write("webster: socket_connect: " + socket_error(err) + "\n");
	return -1;
    }
    
    sockets[socketfd] = (["user" : tp,
			  "word" : word,
			  "mode" : (stringp(mode) ? mode : "DEFINE"),
			  "obj" : previous_object(),
			  "func": (stringp(func) ? func : "get_input"),
			  "flags": INPUT
			  ]);
    return 1;
}

read_callback(int fd, string message)
{
    if (sockets[fd]) 
    {
	if (!user_living(sockets[fd]["user"]))
	{
	    close_callback(fd);
	    return;
	}
	
	/* filter the input some... */
	message = filter_message(message, fd);
	call_other(sockets[fd]["obj"],sockets[fd]["func"],sockets[fd]["user"],
		   message);
    }
}

filter_message(string message, int fd)
{
    string word;

    /* Filter wierd characters.. */
    message = implode(explode(message,sprintf("%c",128)),"");  /* \200 */
    message = implode(explode(message,sprintf("%c",13)),"");   /* ^M   */
    
    word = sockets[fd]["word"];
    if(sscanf(message, "SPELLING%s", message)) {
        if(message[0] == ' ')
	    message = message[1..-1];
        switch(message[0]) {
	case '0':
	    if(sockets[fd]["mode"] == "DEFINE")
	        return "No definition for '" + word + "'.\n";
	    return "'" + word + "' is not a correct spelling.\n";
	case '1':
	    return "'" + word + "' is spelled correctly.\n";
	default:
	    if(sockets[fd]["mode"] == "DEFINE")
	        return "No definition for '" + word +
		    "'. Maybe you mean:\n" + message[1..-1];
	    return "No spelling for '" + word +
		"'. Maybe you mean:\n" + message[1..-1];
	}
    } else {
        sscanf(message, "%s\n%s", message, message);
	return message;
    }
}

write_callback(int fd)
{
    int err;
    mapping sock;
    
    if (!(sock = sockets[fd]))
	return -1;
    if (!user_living(sock["user"]))
    {
	close_callback(fd);
	return;
    }
    
    if (!(sock["flags"] & CONNECTED))
    {
	sock["flags"] |= CONNECTED;
	call_other(sock["obj"], sock["func"], sock["user"], "Connected\n");
    }
    err  = socket_write(fd, sock["mode"] + " " + sock["word"] + "\n");
    if (err != EESUCCESS)
	call_other(sock["obj"], sock["func"], sock["user"], 
		   "websterd: socket_write: " + socket_error(err) + "\n");
}

close_callback(int fd)
{
    if(sockets[fd]) 
    {
        if(sockets[fd]["user"] && sockets[fd]["flags"] & INPUT)
	    tell_object(sockets[fd]["user"], "webster: Connection closed.\n"); 
	sockets = m_delete(sockets, fd);
    }
}

user_living(object user)
{
    return objectp(user) && interactive(user) && 1;
}
	
	
close_socket(object user)
{
    int *socket;
    int i, size;
    socket = m_indices(sockets);
    size = sizeof(socket);
    i = 0;
    while(i < size)
    {
	if (user == sockets[socket[i]]["user"])
	{
	    close_callback(socket[i]);
	    return;
	}
	i++;
    }
}

	
