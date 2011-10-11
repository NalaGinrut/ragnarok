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
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#ifdef __cplusplus
extern "C" {
#endif

#define PERMS_A	07   // anyone
#define PERMS_U 070  // user
#define PERMS_R 0700 // root
  
SCM scm_mmr_check_file_perms(SCM target ,SCM perms)
#define FUNC_NAME "check-file-perms"
{
  int p = 0;
  char *filename = NULL;
  SCM ret = SCM_BOOL_F;
  struct stat sb;
  int mode = 0;
  int pa = 0 ,pu = 0 ,pr = 0;
    
  SCM_VALIDATE_STRING(1 ,target);
  SCM_VALIDATE_NUMBER(2 ,perms);

  p = scm_to_int(perms);
  filename = scm_to_locale_string(target);

  scm_dynwind_begin(0);
  errno = 0;
  
  if(!stat(filename ,&sb))
    {
      goto end;
    }

  mode = sb.st_mode;
  pa = PERMS_A | p;
  pu = PERMS_U | p;
  pr = PERMS_R | p;
  
  if((!(mode ^ pa)) ||
     (!(mode ^ pu)) ||
     (!(mode ^ pr)))
    {
      ret = SCM_BOOL_T;
    }
  
 end:
  scm_dynwind_end();
  return ret;
}
#undef FUNC_NAME
  
#ifdef __cplusplus
}
#endif


