#include <tune.h>

#define CASE(X,Y) case X: { return (Y); }

inherit "/obj/player";

finger(str)
{
    int lev;
    object ob;
    string msg;

    if(this_player() && this_player() == this_player(1))
        lev = this_player()->query_exec();

    if(!str)
        return finger_all(lev);


    name = mailaddr = variables["title"] = quests = plan_desc = test_char = 0;
    money = experience = money_in_bank = age = last_login_time = 0;
    Dex = Int = Con = Str = 0;
    if (!restore_object ("players/"+str))
        return ("That player doesn't exist.\n");

    msg = ("Name: "+capitalize(name)+
	   ((stringp(variables["title"]) && strlen(variables["title"])) ? " "+variables["title"] : ""))[0..75]+".\n";

    msg += ("Status: "+get_status()+"\tGender: "+
	    (gender==1 ? "Male" : "Female") +".\n");

    msg += ("Email: "+email()+"\n");

    if(lev >= POL_EXEC)
      if((ob=find_player(name))&& interactive(ob) && (ob->short() || lev>=exec))
          msg+=("On since "+last_login_time+" from host "+called_from_host+"\n");
      else
          msg+="Last login "+last_login_time+" from host "+called_from_host+"\n";
    else
      if((ob=find_player(name))&& interactive(ob) && (ob->short() || lev>=exec))
          msg+=("On since "+last_login_time+"\n");
      else
          msg+="Last login "+last_login_time+"\n";
    if(test_char)
        msg += (capitalize(name)+" is "+capitalize(test_char)+
		"'s test character.\n");
    
    msg += "Age: "+dstring(dtime(age*2,0))+"\n";
    msg += (capitalize(name)+" has "+
	    ("room/post"->query_new_mail(name) ? "" : "no ") +
	    "unread mail.\n"
	   );
    msg += ("Plan: "+(plan_desc ? plan_desc : "") +"\n");

    if(lev >= POL_EXEC) {
        msg+=sprintf("Exec: %d   Experience: %d   Money: %d   Bank: %d\n",
		     exec,  experience,  money,  money_in_bank
		    );
	msg += sprintf("Dex: %d   Int: %d   Con: %d   Str: %d\n",
			Dex, Int, Con, Str);
	msg += ("Quests: "+ (stringp(quests) ? quests : "") + "\n");
    }

    return msg;
}

static finger_all(lev){
  string msg;
  object ob;
  int i;
  
#ifdef MUDOS
  ob = sort_array(users(), "sort", this_object());
#else
  ob = m_values(mkmapping(map_array(users(),"sort",this_object()),users()));
#endif
    msg=sprintf(" %-23s%-12s%-7s%-8s%s\n","Name","Status","Idle","When","Where");
  for(i=0; i < sizeof(ob); ++i) 
    if(ob[i]->short() || lev >= ob[i]->query_exec())
      msg=sprintf("%s %-23s%-12s%-7s%-8s%s\n",
		  msg,
		  capitalize(ob[i]->query_real_name())+" ",
		  level(ob[i]),
		  idle(ob[i]),
		  ctime(load_time(ob[i]))[11..15],
		  query_ip_name(ob[i]));
  return msg;
}

static level(ob){
  int lev;
  if(!ob) return "hmm";
  lev = ob->query_exec();
  if(lev >= 1000) { return "Creator";} 
  if(lev >= MIN_EXEC) { return "Minister"; }
  if(lev >= SEC_EXEC) { return "Secretary"; }
  if (lev >= RET_EXEC) { return "Retired"; }
  if (lev >= SEN_EXEC) { return "Senior Politician"; }
  if(lev >= POL_EXEC) { return "Politician"; }
   if (lev>=APP_EXEC) { return "Applying Politician"; }
  return "Mortal";
}

static idle(ob) {
  return (query_idle(ob)<30) ? " ":(query_idle(ob)/60)+"."+(query_idle(ob)%60);
}

#ifdef MUDOS
sort(ob1, ob2) {
  return(ob1->query_real_name()[0] > ob2->query_real_name()[0]);
}
#else
sort(ob) {
  return ob->query_real_name()[0];
}
#endif


static get_status()
{
    if (!exec)
	return ("Mortal citizen.");
    else switch(exec) {
        CASE(APP_EXEC,"Applying Politician.");
        CASE(POL_EXEC,"Politician.");
        CASE(SEN_EXEC,"Senior Politician.");
        CASE(RET_EXEC,"Retired Cabinet member.");
         CASE(SEC_EXEC,"Secretary.");
        CASE(MIN_EXEC,"Minister.");
    default: 
	return ("Unknown.");
    }
}


static email(lev)
{
    if(!stringp(mailaddr) || !strlen(mailaddr))
        return "None";
    if(mailaddr[0..0] == "#")
        if(!this_player() || this_player()->query_exec() < MIN_EXEC)
	    return "None";
	else
	    return mailaddr[1..-1];
    return mailaddr;
}
