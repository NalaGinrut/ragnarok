/*	
 *  Copyright (C) 2011
 *	"Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
 
 *  Ragnarok is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 
 *  Ragnarok is distributed in the hope that it will be useful,
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
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <malloc.h>

#ifdef __cplusplus
extern "C" {
#endif

#define YES	0
#define NO	1
#define PATH_IS_THERE -1
#define ENOENT  2

static inline int get_path_levels(const char *path ,int len)
{
  const char *ptr = path;
  int cnt = 0;
  
  while(len-- > 0)
    {
      if('/' == *ptr && '/' != *(ptr+1))
	{
	  cnt++;
	}
      ptr++;
    }

  // we must count the last path if it doesn't have '/'
  if('/' != *(ptr-1))
    cnt++;

  return cnt;
}
  
static inline int path_exists(const char *path)
{
  struct stat st;

  return stat(path ,&st) == ENOENT ? NO : YES; 
}
  
static inline int do_create(const char *path ,int mode)
{
  if(NO == path_exists(path))
    {
      return mkdir(path ,mode);
    }
  
  return PATH_IS_THERE;
}
  
  /* If return -1 ,then path enumeration ends.
   * Or level will be changed for later usage.
   * The level starts from 1 ,not 0;
   */
static inline char* get_parent_path(const char *path ,int *level ,int len)
{
  const char *ptr = path;
  int n = *level;

  do
    {
      ptr = memchr(ptr+1 ,'/' ,len);
    }while(n-- > 0 && !ptr);

  *level--;
    
  return (char*)ptr;
}
  
SCM scm_mmr_create_this_path(SCM path ,SCM mode)
#define FUNC_NAME "create-this-path"
{
  char *p = NULL;
  char *b = NULL;
  char *buf = NULL;
  SCM ret = SCM_BOOL_F;
  int m = 0777;
  int len = 0;
  int n = 0;
  
  SCM_VALIDATE_STRING(1 ,path);

  if(!SCM_UNBNDP(mode))
    {
      SCM_VALIDATE_NUMBER(2 ,mode);
      m = scm_to_int(mode);
    }

  scm_dynwind_begin(0);
  
  p = scm_to_locale_string(path);
  scm_dynwind_free(p);
  
  len = strlen(p);
  buf = (char*)malloc(len+1); // Don't forget +1 for '\0'
  n = get_path_levels(p ,len);
  
  while(n >= 0)
    {
      int l = 0;
      b = get_parent_path(p ,&n ,len);
      l = b-p;
      memcpy(buf ,b ,l);
      buf[l+1] = '\0';
      do_create(buf ,len);
    }

  free(buf);
  buf = NULL;

  scm_dynwind_end();
  
  return ret; 
}
#undef FUNC_NAME
  
#ifdef __cplusplus
}
#endif

