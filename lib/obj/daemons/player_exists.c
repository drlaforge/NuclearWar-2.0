int exec;
int Dex;
main(s) {
    if (!s) return 0;
    restore_object("players/"+s);
    if (!Dex) return 0;
    if (exec) return -1;
    return 1;
}
