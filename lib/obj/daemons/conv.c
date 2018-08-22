mapping feelings=([]);
mapping  adverbs=([]);

create(){
  string tmp, foo;
  int i;

  restore_object("obj/o/feelings");
 tmp = explode(read_file("/etc/adverbs"),"\n");
  for(i=0; i<sizeof(tmp);i++){
     foo=explode(tmp[i],":");
     adverbs[foo[0]] = foo[1];
  }
  save_object("obj/o/soul");
  write("ok\n");
}
  
