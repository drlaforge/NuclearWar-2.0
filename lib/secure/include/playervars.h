/*
 * This file consists of the variables the player
 * object and is simply included by player.c
 */

int		bugreports;
int 		is_linkdead;
int 		tot_value;
int 		stats_is_updated;
int 		intoxicated;	/* How many ticks to stay intoxicated. */
int 		stuffed;	/* How many ticks to stay stuffed */
int	 	soaked;		/* How many ticks to stay soaked */
int 		money_in_bank;
int 		headache, max_headache;
int 		player_kills;	/* The total number pk's the player have */
int 		deaths;		/* The number of times we have been killed */
int             player_deaths;  /* The number of times we have been pked */
int             exec;           /* The amount of responsibility we have */

string		test_char; 	/* Declare this variable before including 
				 * of living.h */
string          surname;
string 		my_desc;	/* Our personal description. */
string 		password;	/* This players crypted password. */
string 		called_from_ip;	/* IP number was used last time */
string 		called_from_host;/* Hostname was used last time */
string 		last_login_time;/* When we last did login*/
string 		plan_desc;	/* A plan to the finger command */
string 		msgclone;	/* Remember clone message  */
string 		mdest;		/* Destruct message. */
string 		mhome;          /* Home message. */
string 		minvis, mvis;   /* Invis-vis message. */
string 		quests;		/* A list of all solved quests */
string 		mailaddr;	/* Email address of player */
string 		host;		/* Name of players remote host */
string          lifestyle;           /* What lifestyle do we have */
string          groupname;      /* What group we belong to */
string          alternate;      /* Who is our alternate character */
string          *NEWS_ignore;   /* What Newsgroups to ignore */
string          NEWS_group;     /* Last group visited */
mixed 		paths; 		/*Path map_array*/
int         idle_age;

//mapping         news_map;       /* News mapping */
mapping         skills;         /* Skills mapping */
mapping         variables;      /* Variables set by players */

/* Static varibles below this */

static int 	listen_to_shouts_from_level;
static int 	nologin;
static int 	num_users;
static int 	time_to_save;	/* Time to autosave. */
static int	wiz_line;	/* bits for arch,sci,vet lines */
static int      line;           /* new bits for global lines */
static int	vet_line;	/* bits vet-line */

static string 	current_path;
static string 	statue_type;
static string 	change_dir,change_file;
static string 	password2;	/* Temporary when setting new password */
static string 	saved_where;	/* Temp... */
static string 	xx_realname; 	/* Used to prevent some guys from
				 * cheating../pen */
static string 	it;		/* Last thing referenced. */

static object 	myself;		/* Ourselfs. */
static object 	team;      	/*the team flag*/
static int      teamxp;         /* XP gained since teaming */
static object 	follow;    	/* follow flag */
static object 	accept_team_member; /*accept what member */

static string   *tells;       /* Saving tells here */

/* Do not put any variables after this! */












