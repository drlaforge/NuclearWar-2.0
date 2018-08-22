/*
// These are functions useful for determining the actual time a player is
// in, based on an environment variable, TZONE.
//
// 20.Jan.92
//
// Written by DocZ @ TMI.
//
*/

int query_tzone(string str)
 {
  int offset; 
  switch (str)
   {
    case "EST": { offset =   2; break; }
    case "CST": { offset =   1; break; }
    case "MST": { offset =   0; break; }
    case "PST": { offset = - 1; break; }
    case "BST": { offset =   6; break; } /* OBO */
    case "GMT": { offset =   7; break; }
    default   : { offset = 999; break; }
   }
   if (offset > 24) {return 0;} /* returns 0 if error */
   offset = offset * 60 * 60;
   return (time()+offset);
}

void show_tzone_list()
{
    write("\tEST is Eastern Standard Time.");
    write("\tCST is Central Standard Time.\n");
    write("\tMST is Mountain Standard Time.");
    write("\tPST is Pacific Standard Time.\n");
    write("\tGMT is Greenwhich Mean Time.");
    write("\tBST is British Summer Time.\n");
    write("\n\tIf _YOUR_ time zone isn't listed, leave mail to DOCZ.\n"
    +"\tBe sure to include the abreviation, the full name, and the\n"
    +"\tnumber of hours difference from GST (Greenwhich Standard)!\n");
    return;
}
