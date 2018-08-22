/* A standard soul object.
 *
 * AbacusMud 1993
 * -Contributor: Dan Solli (Menta)

 * -Default adverbs: Musse Pigg (Bigfoot)

 * -Made it a daemon: Musse again..

 * -Feelings at objects and a null (%) felling: Stor Gnun (Draconian)

 * -Demand storage for feelings (feelings are stored in memory first
 *   time they are used. We dont have to read the file from disk 
 *   more than once per used feeling, and unused feelings arent loaded 
 *   into memory): Mickey Mouse (Bigfoot)

 * -Demand storage for the adverbs as well.: Biggie.

 * -Added a profile option to see what feelings are used most etc..: Bigfoot.


 * -Nuked profiling and loads feelings into memory at loadtime.: Bigfoot
 */

/*
 * Where are the adverbs defined ?
 */
#define ADVERBS      "/etc/adverbs"

/*
 * Where are the feelings defined ?
 */
#define FEELINGS        "/etc/feelings" 

/*
 * Where is the feelings/advers saved ?
 */
#define SOULDATA	"obj/o/soul"

#define replace(XX,YY,ZZ)	implode(explode(XX,YY),ZZ)

mapping feelings = ([ ]);	/* mapping of demand stored feelings */
mapping adverbs = ([ ]);	/* mapping of demand stored adverbs */

void create()
{
    restore_object(SOULDATA);
}

nomask do_feeling(string str)
{
    object who;
    string *feeling;
    string act1, act2, adverb, dum, whom, xverb, def;

    if( !(feeling = feelings[query_verb()]) )
	return 0;
    
    if(this_player()->query_ghost())
        return !notify_fail("You fail.\n");

    if(environment(this_player()) &&
       file_name(environment(this_player())) == "room/post") /* Ugly hack..*/
	  return !notify_fail("Go outside to use feelings.\n");

    if(!strlen(feeling[3]))
        feeling[3] = query_verb()+"-"+query_verb()+"s";
    if(str) {
      if(sscanf(str,"%s %s",adverb,whom) != 2) {
	if(who = present(str,environment(this_player()))) {
	    if(strlen(feeling[2]))
	        return !action(replace(feeling[1],"#AD",feeling[2]),feeling[3],who);
	    return !action(replace(feeling[1]," #AD",""),feeling[3],who);
	} else if(adverb = find_adverb(str)) {
	  if(strlen(adverb))
	    return !action(replace(feeling[0],"#AD",adverb),feeling[3]);
	  return !action(replace(feeling[0]," #AD",""),feeling[3]);
	} else 
	  return !notify_fail("No such player or adverb: "+str+".\n");
      } else {
	if(!(who = present(whom,environment(this_player()))))
	  return !notify_fail(capitalize(whom)+" isn't here.\n");
        str = adverb;
	if(adverb = find_adverb(adverb)) {
	return !action(replace(feeling[1],(adverb==""?" ":"")+"#AD",adverb),
		       feeling[3],who);
	}
	return !notify_fail("No such adverb: "+str+".\n");
      }
    } else {
      if(strlen(feeling[2]))
        return !action(replace(feeling[0],"#AD",feeling[2]),feeling[3]);
      return !action(replace(feeling[0]," #AD",""),feeling[3]);
    }
}

nomask find_adverb(string adverb){
  string X;

  if(adverb == "%")
    return "";
  if( !(X = adverbs[adverb]))
    if(member_array(adverb, m_values(adverbs)) >= 0)
      X = adverb;
  return X;
}

reload(what)
{
  status both;
  string file, *lines;

  if (!what) {
    what = "feelings";
    both++;
  }
  
  switch(what) {
  case "feelings":
    feelings = ([]);
    file = read_file(FEELINGS);
    break;
  case "adverbs":
    adverbs = ([]);
    file = read_file(ADVERBS);
    break;
  default:
    return;
  }

  if (stringp(file) && pointerp((lines = explode(file, "\n"))))
    map_array(lines, "map_data", this_object(), what);

  save_object(SOULDATA);
  return(msizeof((what == "feelings" ? feelings : adverbs)));
}

map_data(arg, what)
{
  string *arr;

  if (!stringp(arg))
    return;
  if (pointerp(arr = explode(arg, ":"))) {
    switch(what) {
    case "feelings":
      feelings[arr[0]] = arr[1..sizeof(arr)-1];
      if (sizeof(arr) < 4)
	feelings[arr[0]] += ({ "" });
      break;
    case "adverbs":
      adverbs[arr[0]] = arr[1];
      break;
    default:
      return;
    }
  }
}


query_feelings() { return feelings; }
query_adverbs() { return adverbs; }


