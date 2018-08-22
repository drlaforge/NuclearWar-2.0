/* This file records outlaw ratings
 * I have to warn you, don't try to understand it.
 * All other comments are meant for me, so my head doesn't explode..
 * /Scarblac
 */

mapping ratings = ([ ]);
/* ratings consists of:
    "name" : ({ rating, ... })
    if rating = 0 (no rating yet) the '...' part is
       name1,evaldiff1,result, etc
       for logging to opponents he has had, evaldiff and kill(result 1)
       or death(result 0).
    if there IS a rating, the '...' part is
       kills, deaths, amount of events, date of last event, average rating
       of opponents
*/
string *namelist;
string update_time;
string update_name;

#define SAVE "obj/o/ratings"
#define RATING(x) (ratings[(x)]?ratings[(x)][0]:0)
#define EVAL_RATING  30         /* Eval influence on pk rating */
#define FACTOR       75         /* Speed */
#define STD_RATING  2000        /* What people are supposed to start with */
#include <tune.h>

query_rating(player) {
    return RATING(player);
}
query_rating_info(player) {
    return ratings[player];
}
reset(whee) {
    if (!whee) restore_object(SAVE);
}

/* Call this to log a pk of name1 killing name2, evaldiff = eval of
    name1 - eval of name2. */
log_pk(name1,name2,evaldiff) {
    object ob;
    int q,i;
    int score,avg,ravg;

    if (find_player(name1)->query_test_char() || find_player(name2)->query_test_char()) return;
    if (!ratings[name1]) ratings[name1] = ({ 0 });
    if (!ratings[name2]) ratings[name2] = ({ 0 });

    if (!RATING(name1)) ratings[name1] += ({ name2, evaldiff, 1 });
    else
        if (RATING(name2)) {
            i = RATING(name1);
            record_rating(name1,RATING(name2),evaldiff,1);
            record_rating(name2,i,-evaldiff,0);
            if (ob = find_player(name1))
                tell_object(ob,"Your new pk rating is "+ratings[name1][0]+"!\n");
            if (ob = find_player(name2))
                tell_object(ob,"Your new pk rating is "+ratings[name2][0]+"!\n");
            save_object(SAVE);
            return;
        }
    if (!RATING(name2)) ratings[name2] += ({ name1, -evaldiff, 0 });

/* Now, check if someone who didn't have a rating yet reached 3 events. */
    if (!RATING(name1) && (sizeof(ratings[name1]) == 10)) {
        q = 1; /* Name1 is given rating now */
        avg = 0;
        avg += (ratings[ratings[name1][1]][0] ? ratings[ratings[name1][1]][0]
                    : STD_RATING) - EVAL_RATING*ratings[name1][2];
        avg += (ratings[ratings[name1][4]][0] ? ratings[ratings[name1][4]][0]
                    : STD_RATING) - EVAL_RATING*ratings[name1][5];
        avg += (ratings[ratings[name1][7]][0] ? ratings[ratings[name1][7]][0]
                    : STD_RATING) - EVAL_RATING*ratings[name1][8];
        score = ratings[name1][3] + ratings[name1][6] + ratings[name1][9];
        avg /= 3;
        ravg = avg;
        /* Average is now average rating of opponents */

        switch (score) {
            case 0 : avg -= 300; break;
            case 1 : avg -= 100; break;
            case 2 : avg += 100; break;
            case 3 : avg += 300; break;
        }

        if (RATING(ratings[name1][1])) {
            record_rating(ratings[name1][1],avg,-ratings[name1][2],!ratings[name1][3]);
            if (ob = find_player(ratings[name1][1]))
                tell_object(ob,"Your pk rating is now "+ratings[ratings[name1][1]][0]+
                    ", because of "+(ratings[name1][3]?"your death to ":"your killing of ")+
                    capitalize(name1)+".\n");
        }
        if (RATING(ratings[name1][4])) {
            record_rating(ratings[name1][4],avg,-ratings[name1][5],!ratings[name1][6]);
            if (ob = find_player(ratings[name1][4]))
                tell_object(ob,"Your pk rating is now "+ratings[ratings[name1][4]][0]+
                    ", because of "+(ratings[name1][6]?"your death to ":"your killing of ")+
                    capitalize(name1)+".\n");
        }
        if (RATING(ratings[name1][7])) {
            record_rating(ratings[name1][7],avg,-ratings[name1][8],!ratings[name1][9]);
            if (ob = find_player(ratings[name1][7]))
                tell_object(ob,"Your pk rating is now "+ratings[ratings[name1][7]][0]+
                    ", because of "+(ratings[name1][9]?"your death to ":"your killing of ")+
                    capitalize(name1)+".\n");
        }
        ratings[name1] = ({ avg, score, 3-score, 3, time(), ravg });
        shout(capitalize(name1)+" now has an outlaw rating of "+avg+"!!\n");
    }

    if (!RATING(name2) && (sizeof(ratings[name2]) == 10)) {
        avg = 0;
        avg += ((ratings[ratings[name2][1]][0] && !(q && ratings[name2][1] == name1)) ? ratings[ratings[name2][1]][0]
                    : STD_RATING) - EVAL_RATING*ratings[name2][2];
        avg += ((ratings[ratings[name2][4]][0] && !(q && ratings[name2][4] == name1)) ? ratings[ratings[name2][4]][0]
                    : STD_RATING) - EVAL_RATING*ratings[name2][5];
        avg += ((ratings[ratings[name2][7]][0] && !(q && ratings[name2][7] == name1)) ? ratings[ratings[name2][7]][0]
                    : STD_RATING) - EVAL_RATING*ratings[name2][8];
        score = ratings[name2][3] + ratings[name2][6] + ratings[name2][9];
        avg /= 3;
        ravg = avg;
        /* Average is now average rating of opponents */

        switch (score) {
            case 0 : avg -= 300; break;
            case 1 : avg -= 100; break;
            case 2 : avg += 100; break;
            case 3 : avg += 300; break;
        }
        if (RATING(ratings[name2][1]) && !(q && ratings[name2][1] == name1)) {
            record_rating(ratings[name2][1],avg,-ratings[name2][2],!ratings[name2][3]);
            if (ob = find_player(ratings[name2][1]))
                tell_object(ob,"Your pk rating is now "+ratings[ratings[name2][1]][0]+
                    ", because of "+(ratings[name2][3]?"your death to ":"your killing of ")+
                    capitalize(name2)+".\n");
        }
        if (RATING(ratings[name2][4]) && !(q && ratings[name2][4] == name1)) {
            record_rating(ratings[name2][4],avg,-ratings[name2][5],!ratings[name2][6]);
            if (ob = find_player(ratings[name2][4]))
                tell_object(ob,"Your pk rating is now "+ratings[ratings[name2][4]][0]+
                    ", because of "+(ratings[name2][6]?"your death to ":"your killing of ")+
                    capitalize(name2)+".\n");
        }
        if (RATING(ratings[name2][7]) && !(q && ratings[name2][7] == name1)) {
            record_rating(ratings[name2][7],avg,-ratings[name2][8],!ratings[name2][9]);
            if (ob = find_player(ratings[name2][7]))
                tell_object(ob,"Your pk rating is now "+ratings[ratings[name2][7]][0]+
                    ", because of "+(ratings[name2][9]?"your death to ":"your killing of ")+
                    capitalize(name2)+".\n");
        }
        ratings[name2] = ({ avg, score, 3-score, 3, time(), ravg });
        shout(capitalize(name2)+" now has an outlaw rating of "+avg+"!!\n");
    }
    save_object(SAVE);
}

record_rating(player,opp_rating,evaldiff,result) {
    if (!ratings[player][0]) return;
    if (result) {
        ratings[player][0] += (get_probability(ratings[player][0],opp_rating-
                               EVAL_RATING*evaldiff)*FACTOR)/100;
        ratings[player][1]++;
    }
    else {
        ratings[player][0] -= (get_probability(opp_rating-EVAL_RATING*evaldiff,
                                ratings[player][0])*FACTOR)/100;
        ratings[player][2]++;
    }
    ratings[player][4] = time();
    ratings[player][5] = (ratings[player][5]*ratings[player][3]+opp_rating-
                            EVAL_RATING*evaldiff)/(++ratings[player][3]);
}
get_probability(rating1,rating2) {
    int i,j;

    i = (rating1-rating2);
    j = i >= 0;
    if (!j) i = -i;

    switch(i) {
        case 0..3       : return 50;
        case 4..10      : return j ? 49 : 51;
        case 11..17     : return j ? 48 : 52;
        case 18..25     : return j ? 47 : 53;
        case 26..32     : return j ? 46 : 54;
        case 33..39     : return j ? 45 : 55;
        case 40..46     : return j ? 44 : 56;
        case 47..53     : return j ? 43 : 57;
        case 54..61     : return j ? 42 : 58;
        case 62..68     : return j ? 41 : 59;
        case 69..76     : return j ? 40 : 60;
        case 77..83     : return j ? 39 : 61;
        case 84..91     : return j ? 38 : 62;
        case 92..98     : return j ? 37 : 63;
        case 99..106    : return j ? 36 : 64;
        case 107..113   : return j ? 35 : 65;
        case 114..121   : return j ? 34 : 66;
        case 122..129   : return j ? 33 : 67;
        case 130..137   : return j ? 32 : 68;
        case 138..145   : return j ? 31 : 69;
        case 146..153   : return j ? 30 : 70;
        case 154..162   : return j ? 29 : 71;
        case 163..170   : return j ? 28 : 72;
        case 171..179   : return j ? 27 : 73;
        case 180..188   : return j ? 26 : 74;
        case 189..197   : return j ? 25 : 75;
        case 198..206   : return j ? 24 : 76;
        case 207..215   : return j ? 23 : 77;
        case 216..225   : return j ? 22 : 78;
        case 226..235   : return j ? 21 : 79;
        case 236..245   : return j ? 20 : 80;
        case 246..256   : return j ? 19 : 81;
        case 257..267   : return j ? 18 : 82;
        case 268..278   : return j ? 17 : 83;
        case 279..290   : return j ? 16 : 84;
        case 291..302   : return j ? 15 : 85;
        case 303..315   : return j ? 14 : 86;
        case 316..328   : return j ? 13 : 87;
        case 329..341   : return j ? 12 : 88;
        case 342..357   : return j ? 11 : 89;
        case 358..374   : return j ? 10 : 90;
        case 375..391   : return j ? 9 : 91;
        case 392..411   : return j ? 8 : 92;
        case 412..432   : return j ? 7 : 93;
        case 433..456   : return j ? 6 : 94;
        case 457..481   : return j ? 5 : 95;
        case 482..517   : return j ? 4 : 96;
        case 518..559   : return j ? 3 : 97;
        case 560..619   : return j ? 2 : 98;
        case 920..735   : return j ? 1 : 99;
        default: return j ? 0 : 100;
    }
}

query_mapping() { return ratings; }
write_mapping() {
    int i;
    string indices;
    write("The mapping:\n");
    indices = m_indices(ratings);
    for (i=0; i < sizeof(indices); i++)
        printf("%15s   %4d   (%s)\n",indices[i],ratings[indices[i]][0],
            implode(ratings[indices[i]][1..sizeof(ratings[indices[i]])],","));
}

update_toplist() {
    if (this_player()->query_exec() < SEC_EXEC) return write("Sorry, can't do.\n");
    namelist = sort_array(filter_array(m_indices(ratings),"check_rating",this_object()),
                "on_rating",this_object());
    update_time = ctime(time());
    update_name = capitalize(this_player()->query_real_name());
    save_object(SAVE);
}

check_rating(s) {
    if (!RATING(s)) return;
    if (file_size("/players/"+s+"/") == -2) return 0;
    if (file_size("/players/"+s+".o") == -1) return 0;
    return 1;
}

on_rating(a,b) {
    if (ratings[a][0] > ratings[b][0]) return -1;
    if (ratings[a][0] < ratings[b][0]) return 1;
}

show_list(s) {
    int n,i;
    if (sizeof(namelist) < 15) {
        for (i=0; i<sizeof(namelist); i++)
            printf("%2d.  %-20s   [%4d]\n",i+1,
                capitalize(namelist[i]),ratings[namelist[i]][0]);
        return;
    }
    if (s)
        if (sscanf(s,"%d",n)) n--;
        else n = member_array(s,namelist);
    if (n < 12) n = 12;
    if (n >= sizeof(namelist)-2) n = sizeof(namelist)-3;
    write("The PK rating toplist, last updated "+update_time+" by "+update_name+":\n");
    for (i=0;i<10;i++)
        printf("%2d.  %-20s   [%4d]\n",i+1,
            capitalize(namelist[i]),ratings[namelist[i]][0]);
    for (i=n-2;i<n+3;i++)
        printf("%2d.  %-20s   [%4d]\n",i+1,
            capitalize(namelist[i]),ratings[namelist[i]][0]);
}

delete_rating(s) {
    if (this_player()->query_exec() < MIN_EXEC) return;
    ratings = m_delete(ratings,s);
    save_object(SAVE);
}
