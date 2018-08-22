/*
 * Intermud mail server, used by Huthar's mailer daemon
 * Original author: Huthar
 * Rewritten to conform to new socket efun specs,  Blackthorn (10/31/92)
 * Small patches for TMI-2 mudlib, Buddha (11/6/92)
 */
 
#include <config.h>
#include <mailer.h>
#include <daemons.h>
#include <uid.h>
 
#define log(x) log_file("MS", x)
#define MS_SAVE "mail-queue"
#define FLUSH_TIME 86400
#define AGE_TIME 604800
#define EOF "%EOF%"
#define EOT "%EOT%"
 
static string upd;
static mapping new_mail_queue, sockets;
static string receiver, from, to, subject, message,cc;
static int date;
static string mud;
static mixed mqi;
static mixed outgoing;
 
mixed mail_queue;
int date_last_flushed;
 
void process_message(int fd);
void flush_mail_queue();
void age_queue();
void bad_port(string mud, string from, string message);
 
void reset()
{
  if ((time() - date_last_flushed) > FLUSH_TIME)
  {
    mqi = keys(mail_queue);
    flush_mail_queue();
    date_last_flushed = time();
  }
  age_queue();
}
 
void create()
{
  seteuid(ROOT_UID);
  mail_queue = ([ ]);
  sockets = ([ ]);
  mqi = ({ });
  restore_object(MAILDIR + MS_SAVE);
  mqi = keys(mail_queue);
  flush_mail_queue();
}
 
string convert_name(string lname, string lmud)
{
  string tmp, tmpaddr;
 
  if (sscanf(lname, "%s@%s", tmp, tmpaddr) != 2)
  {
    tmp = lname;
    tmpaddr = lmud;
  }
 
  if (tmpaddr != THIS_MUD) return tmp + "@" + tmpaddr;
  return tmp;
}
 
void remote_mail(string own, string mud, mixed lto, mixed lcc, 
                 string lfrom, string lsubject, int ldate, string lmessage)
{
  int i;
  string *tmp;
 
  if (file_name(previous_object()) != MAILER_D) return;
 
  if (!lmessage) lmessage = "\n";
 
  tmp = explode(lmessage, "\n");
  for (i = 0; i < sizeof(tmp); i++)
  {
    if (tmp[i] == EOF) 
     tmp[i] = EOF + "."; else 
      if (tmp[i] == EOT) tmp[i] == EOT + ".";
  }
 
  if (!tmp) tmp = ({ });
  lmessage = implode(tmp, "\n");
  if (!mail_queue[mud]) mail_queue[mud] = ({ });
 
  mail_queue[mud] += ({
      ([ "recipient" : own, "to" : lto, "cc" : lcc,
         "from" : lfrom, "subject" : lsubject, "date" : ldate, 
         "message" : lmessage ]) 
                     });
  save_object(MAILDIR + MS_SAVE);
  mqi += ({ mud });
}
 
void bad_port(string lmud, string lfrom, string msg)
{
  object ob;
 
  ob = find_player(lfrom);
  if (!ob) return;
 
  tell_object(ob,
"The mud " + lmud + " doesn't exist or has a bad port address.\n" +
"If the mud should exist, notify your local admins.\n");
  tell_object(ob,
"Saving letter in: " + TMP_DIR + "/" + lfrom + ".dead.letter\n");
  write_file(TMP_DIR + "/" + lfrom + ".dead.letter", msg);
  mail_queue[lmud] = 0;
}
 
remove()
{
  destruct(this_object()); 
}
 
void set_mqi(mixed m) { mqi = m; }
string *query_mqi() { return mqi; }
 
mapping query_mail_queue() { return mail_queue; }
void set_mail_queue(mixed a) { mail_queue = a; }
 
void clear_mail_queue() 
{
  mail_queue = ([ ]);
  save_object(MAILDIR + "mail-queue");
}
 
void age_queue()
{
  int i, j;
  string *key;
  mixed tmp;
 
  key = keys(mail_queue);
 
  for (i = 0; i < sizeof(key); i++)
  {
    tmp = mail_queue[key[i]];
    for (j = 0; j < sizeof(tmp); j++)
    {
      if (time() - tmp[j]["date"] > AGE_TIME)
      {
        log("Aging mail from: " +
            tmp[j]["from"] + ", dated: " + tmp[j]["date"] + "\n");
        exclude_array(tmp, j);
      }
    }
  }
}
 
void close_callback(int id)
{
  map_delete(sockets, id);
  return;
}
 
void service_request(int id)
{
  sockets[id] = ([ "msg" : "" ]);
}
 
void read_callback(int id, string data)
{
  if (data == (EOT + "\n"))
  {
    sockets[id]["msg"] += data;
    process_message(id);
    return;
  }
  sockets[id]["msg"] += data;
}
 
void process_message(int id)
{
  mixed tmp, tmp2;
  string *totmp, *cctmp;
  int i, j, res;
 
  sscanf(sockets[id]["msg"], "%s\n%s", mud, tmp);
  sockets[id]["msg"] = tmp;
 
  tmp = explode(sockets[id]["msg"], EOF);
  tmp = tmp[0..sizeof(tmp)-2];
  tmp2 = allocate(sizeof(tmp));
 
  for (i = 0; i < sizeof(tmp); i++) 
  {
    tmp2[i] = explode(tmp[i], "\n");
  }
  for (i = 0; i < sizeof(tmp2); i++)
  {
    receiver = convert_name(tmp2[i][0], THIS_MUD);
    totmp = explode(tmp2[i][1], " ");
    for (j = 0; j < sizeof(totmp); j++)
     totmp[j] = convert_name(totmp[j],mud);
    cctmp = explode(tmp2[i][2], " ");
    if (cctmp[0])
     for (j = 0; j < sizeof(cctmp); j++)
      cctmp[j] = convert_name(cctmp[j],mud); else
       cctmp = ({ });
    from = convert_name(tmp2[i][3], mud);
    subject = tmp2[i][4];
    sscanf(tmp2[i][5], "%d", date);
    message = implode(tmp2[i][6..sizeof(tmp2[i]) - 1], "\n");
    res = (int)MAILER_D->add_message(receiver, totmp, cctmp, from, 
                                     subject, date, 1, 0, message);
    if (!res)
    {
      MAILER_D->add_message(from, ({ from }), ({ }),
       "MS@" + THIS_MUD,
       capitalize(receiver) + " does not exist at this site. (MAIL BOUNCE)",
       time(), 1, 0, "BODY OF MESSAGE FOLLOWS:\n\n" + message);
      if (sizeof(explode(from, "@")) == 1)
      {
        MAILER_D->biff(from,"MS", capitalize(receiver) +
            " does not at exist at this site. (MAIL BOUNCE)");
        MAILER_D->flush_files();
      } else call_out("flush_mail_queue", 30);
      return;
    }
    MAILER_D->biff(receiver, from, subject);
    MAILER_D->flush_files();
  }
}
 
void flush_mail_queue()
{
  string *muds, address, port;
  int id;
 
  if (!sizeof(mqi)) return;
 
  outgoing = mail_queue[mqi[0]];
  id = INETD->open_service(mqi[0], "mail");
  if (id < 0)
  {
    log("flush_mail_queue: open_service: " + socket_error(id) + "\n");
    mqi -= ({ mqi[0] });
    flush_mail_queue();
  }
}
 
void service_callback(int id)
{
  int i;
 
  INETD->write_socket(id, THIS_MUD + "\n");
  for (i = 0; i < sizeof(outgoing); i++)
  {
    INETD->write_socket(id, outgoing[i]["recipient"] + "\n");
    INETD->write_socket(id, implode(outgoing[i]["to"], " ") + "\n");
    INETD->write_socket(id, implode(outgoing[i]["cc"], " ") + "\n");
    INETD->write_socket(id, outgoing[i]["from"] + "\n");
    INETD->write_socket(id, outgoing[i]["subject"] + "\n");
    INETD->write_socket(id, outgoing[i]["date"] + "\n");
    INETD->write_socket(id, outgoing[i]["message"] + "\n");
    INETD->write_socket(id, EOF + "\n");
  }
  INETD->write_socket(id, EOT + "\n");
  INETD->close_socket(id);
  map_delete(mail_queue, mqi[0]);
  mqi -= ({ mqi[0] });
  save_object(MAILDIR + MS_SAVE);
  call_out("flush_mail_queue", 60);
}
