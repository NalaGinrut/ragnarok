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

SCM scm_mmr_lockf(SCM fd ,SCM cmd ,SCM len)
#define FUNC_NAME "lockf"
{
  int l = 0;
  int d = 0;
  int c = 0;
  int ret;
  
  SCM_VALIDATE_NUMBER(1 ,fd);
  SCM_VALIDATE_NUMBER(2 ,cmd);

  if(!SCM_UNBNDP(len))
    {
      SCM_VALIDATE_NUMBER(3 ,len);
      l = scm_to_int(len);
    }

  d = scm_to_int(fd);
  c = scm_to_int(cmd);
  ret = lockf(d ,c ,l);
  
  return scm_from_int(ret);
}
#undef FUNC_NAME
  
#ifdef __cplusplus
}
#endif


