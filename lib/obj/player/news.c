#include <news.h>

news(str) {
    object ob;
    if (!(ob=present("news_reader", this_player()))) {
	ob = move_object(clone_object(NEWSREADER), this_player());
    }
    ob->init_news(str, NEWS_group);
    return 1;
}

set_NEWS_group(group) {
    NEWS_group = group;
}

query_NEWS_ignore() {
    return (NEWS_ignore ? NEWS_ignore : ({}));
}

set_NEWS_ignore(arg) {
    if (pointerp(arg)) NEWS_ignore=arg;
    else NEWS_ignore=({});
}

query_in_news() {
    return (present("news_reader", this_object()) ? 1 : 0);
}
