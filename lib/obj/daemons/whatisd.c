/* whatisd.c -- Oct19/91 by Jubal@TMI */

/* whatisd emulates the UNIX whatis.
*/


#include <man.h>
inherit "adm/daemons/mand";


varargs string
whatis( mixed arg, string name )
{
	string *manent, rv;

	seteuid( "root" );
	manent = find_manpage( "man", arg, name );
	if( manent )
	{
		if( valid_man_read( manent ) )
		{
			rv = read_file( manent[2], 0, 1 );
			sscanf( rv, ".\\\"%s", rv );
			rv = manent[1] + "(" + manent[0] + ") - " + rv;
		}
		else
			rv = "whatis:  no permission\n";
	}
	else
	{
		if( name )
			rv = "whatis:  " + name + " not found in section " + arg + "\n";
		else
			rv = "whatis:  " + arg + " not found\n";
	}
	return rv;
}


/* EOF */
