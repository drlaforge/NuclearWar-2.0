#include <config.h>
/*
 * A VERY simple name server, used until we get a real one. :)
 * Huthar@Portals|MIRE|TMI|Genocide|OD
 * Recoded to fit compat mode.  / Bigfoot.
 */

mapping muds = ([ "DEFAULT" : ([]) ]);

void load_muds();
void load_services();
void error(int line, string file, string message) ;

void reset(int arg)
{
   if(arg)
       return;
   load_muds();
   load_services();
}

string get_host_name(string alias)
{
   if(!mappingp(muds[alias]))
      return "";
   return muds[alias]["address"];
}

int get_mudresource(string mud, string resource)
{
   int tmp;

   if(mud == "DEFAULT" || !mappingp(muds[mud]))
      return 0;
   if(resource == "DEFAULT")
      return muds[mud]["port"];

   if(muds[mud][resource])
      return muds[mud]["port"] + muds[mud][resource];
   if(muds["DEFAULT"][resource] >= 10000)
      return muds["DEFAULT"][resource] - 10000;
   tmp = muds[mud]["port"] + muds["DEFAULT"][resource];
   if(tmp == muds[mud]["port"])
      return 0;

   return tmp;
}

void load_muds()
{
   int i, pt;
   string addr, *line, md, ad;

   addr = read_file(MUD_ADDRESSES);
   if(!addr)
       return;

   line = explode(addr,"\n");
   
   for(i = 0; i < sizeof(line); i++)
   {
      if(line[i] == "" || line[i][0] == '#')
         continue;
      if(sscanf(line[i],"%s:%s:%d",md,ad,pt) != 3) {
         error(i,MUD_ADDRESSES,"Syntax Error");
         continue;
      }
      if(mappingp(muds[md])) {
         error(i,MUD_ADDRESSES,"Duplicate entry for mud: "+md);
         continue;
      }

      muds[md] = ([ "address" : ad, "port" : pt ]);
   }
}

void load_services()
{
   int i;
   string *line;
   string md,sv,tmp;
   int pt;

   tmp = read_file(MUD_SERVICES);
   if(!tmp)
       return;
   line = explode(tmp,"\n");

   for(i = 0; i < sizeof(line); i++)
   {
      if(line[i] == "" || line[i][0] == '#')
         continue;
      if(sscanf(line[i],"%s:%s:%d",md,sv,pt) != 3) {
         error(i,MUD_SERVICES,"Syntax Error");
         continue;
      }
      if(!mappingp(muds[md])) {
         error(i,MUD_SERVICES,"The mud '"+md+"' is not defined.");
         continue;
      }
      if(sv == "address" || sv == "port") {
         error(i,MUD_SERVICES,"The service '"+sv+"' is a reserved name.");
         continue;
      }
      if(stringp(muds[md][sv])) {
         error(i,MUD_SERVICES,"Duplicate service '"+sv+"' defined.");
         continue;
      }
      if(sscanf(line[i],"%s:%s:%s",tmp,tmp,tmp) && tmp[0]!='-' && tmp[0]!='+')
         pt = ((md != "DEFAULT") ? pt - muds[md]["port"] : pt + 10000);

      muds[md] += ([ sv : pt ]);
   }
}

void error(int line, string file, string message) 
{
   write("ERROR: Line "+line+" of "+file+": ");
   write(message+"\n");
}


mapping query_muds() { return muds + ([]); }

