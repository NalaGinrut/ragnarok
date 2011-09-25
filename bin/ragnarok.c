/*	
 *  Copyright (C) 2011
 *	"Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
 
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <libguile.h>
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>

#define RUNNING_DIR	"/var/log/ragnarok"
#define LOCK_FILE	"ragnarok.lock"
#define LOG_FILE	"ragnarok.log"

#define RAG_SYM(str) scm_string_to_symbol(scm_from_locale_string(str))

#define RAG_GET_FROM_MODULE(m ,s)		\
  scm_variable_ref(scm_c_public_lookup((m) ,(s)))

static void init_modules()
{
  //scm_c_use_module("ragnarok env");
  //scm_c_use_module("ragnarok server");
}

void ragnarok_log_message(char *filename ,char *message)
{
  FILE *logfile;
  
  logfile = fopen(filename ,"a");
  
  if(!logfile)
    return;

  fprintf(logfile,"%s\n" ,message);

  fclose(logfile);
}

void ragnarok_signal_handler(int sig)
{
  switch(sig)
    {
    case SIGHUP:
      ragnarok_log_message(LOG_FILE,"Ragnarok hangup");
      break;
    case SIGTERM:
      ragnarok_log_message(LOG_FILE,"Ragnarok exit!");
      exit(0);
      break;
    }
}	

void daemonize()
{
  int i ,lfp;
  char buf[10] = {0};

  if(getppid() == 1)
    {
      printf("Ragnarok's already run!\n");
      exit(5); /* already a daemon */
    }
 
  i = fork();

  if (i < 0)
    {
      printf("Ragnarok daemon fork error!");
      exit(1); /* fork error */
    }
  else if(i > 0)
    exit(0); /* parent exits */

  /* child (daemon) continues */
  setsid(); /* obtain a new process group */

  for (i=0 ;i<3 ;i++)
    close(i); /* close all descriptors */

  i = open("/dev/null",O_RDWR);
  dup(i);
  dup(i); /* TRICK: handle standard I/O */
  umask(022); /* set newly created file permissions */

  chdir(RUNNING_DIR); /* change running directory */
  lfp = open(LOCK_FILE ,O_RDWR|O_CREAT ,0640);

  if(lfp < 0)
    exit(2); /* can not open */

  if(lockf(lfp ,F_TLOCK ,0) < 0)
    exit(0); /* can not lock */

  /* first instance continues */
  sprintf(buf ,"%d\n" ,getpid());
  write(lfp ,buf ,strlen(buf));
  
  signal(SIGCHLD ,SIG_IGN); /* ignore child */
  signal(SIGTSTP ,SIG_IGN); /* ignore tty signals */
  signal(SIGTTOU ,SIG_IGN);
  signal(SIGTTIN ,SIG_IGN);
  signal(SIGHUP  ,ragnarok_signal_handler); /* catch hangup signal */
  signal(SIGTERM ,ragnarok_signal_handler); /* catch kill signal */
}

static void ragnarok_go()
{
  SCM main = RAG_GET_FROM_MODULE("ragnarok main" ,"main");
  SCM args = scm_program_arguments();

  scm_call_1(main ,args);
}

static void ragnarok_init()
{
  //scm_init_guile();
  daemonize();
}
  
static void inner_main(void *closure, int argc, char **argv)
{
  /* module initializations would go here */

  ragnarok_init();
  init_modules();
  ragnarok_go();
  //scm_shell (argc, argv);

}

int main(int argc, char **argv)
{
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0; /* never reached */
}
