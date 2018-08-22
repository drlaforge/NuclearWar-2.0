mapping feelings;
mapping adverbs;

create(){
    int i;
    string *foo;
    restore_object("obj/o/soul");
    for(foo=m_indices(feelings),i=0;i<sizeof(foo);i++)
        if(sizeof(feelings[foo[i]])<4)feelings[foo[i]]+=({""});
    save_object("obj/o/soul");
}
