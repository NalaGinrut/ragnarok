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
#include <unistd.h>

#ifdef __cplusplus
extern "C" {
#endif

SCM scm_mmr_sleep(SCM second ,SCM msecond)
#define FUNC_NAME "ragnarok-sleep"
{
  long second = 0L;
  long msecond = 0L;

  SCM_VALIDATE_INUM(1 ,second);

  s = scm_to_long(second);
  
  if(!SCM_UNBNDP(msecond))
    {
      SCM_VALIDATE_INUM(2 ,msecond);
      ms = scm_to_long(msecond);
    }
    
  if(s)
    {
      second(s);
    }

  if(ms)
    {
      usecond(ms);
    }

  return SCM_BOOL_T;
}
#undef FUNC_NAME

#ifdef __cplusplus
}
#endif
