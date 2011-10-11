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
#include <stdlib.h>

#define RAG_SYM(str) scm_string_to_symbol(scm_from_locale_string(str))

#define RAG_GET_FROM_MODULE(m ,s)		\
  scm_variable_ref(scm_c_public_lookup((m) ,(s)))

static void ragnarok_go()
{
  SCM main = RAG_GET_FROM_MODULE("ragnarok main" ,"main");
  SCM args = scm_program_arguments();

  scm_call_1(main ,args);
}

static void ragnarok_init()
{
  if(getppid() == 1)
    {
      printf("Ragnarok's already run!\n");
      exit(5); /* already a daemon */
    }	
}
  
static void inner_main(void *closure, int argc, char **argv)
{
  /* module initializations would go here */

  ragnarok_init();
  ragnarok_go();
}

int main(int argc, char **argv)
{
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0; /* never reached */
}
