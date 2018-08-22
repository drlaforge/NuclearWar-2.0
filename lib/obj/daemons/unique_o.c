/*
 * add_unique_object.c           5-93 : Bigfoot Inc.
 *
 *   This is a hack to handle unique object all over the mud. A call
 * to add_unique_object will clone up new unique object and move them to
 * the calling object if there isn't already an unique object of that kind
 * belonging to the caller.
 *   This object isn't really a part of the simul_efun object. That is
 * so the data wont be lost if the simul_efun object is updated. Instead
 * simul_efun simply do a call_other here.
 */

mapping uniques = ([]);
      
varargs nomask void
add_unique_object(string filename, int number, object caller)
{
    int i;

    if(!stringp(filename))
        return;

    if(!objectp(caller))
        caller = previous_object();

  /* If the caller has never called this function.. */
    if(!mappingp(uniques[caller]))
        uniques[caller] = ([]);

  /* If the caller has never called this for 'filename', allocate array */
    if(!pointerp(uniques[caller][filename])) {
        if(number <= 0)
	    number = 1;
        uniques[caller][filename] = allocate(number);
    }
    
  /* Now fill up the object/array with object pointers if empty.. */
    for(i=0; i < sizeof(uniques[caller][filename]); ++i)
        if(!uniques[caller][filename][i])
	    uniques[caller][filename][i] =
	              move_object(clone_object(filename),caller);
}

nomask void reset(int arg)
{
  if(arg)    /* Just clean up the mapping i bit.. */
      while(m_sizeof(uniques) != m_sizeof((uniques = m_delete(uniques,0))))
        /* Do nothing */ ;
}

   
nomask mapping query_uniques() { return uniques; }

