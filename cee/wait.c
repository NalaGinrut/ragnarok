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
#include <sys/types.h>
#include <sys/wait.h>

#ifdef __cplusplus
extern "C" {
#endif

SCM scm_mmr_waitpid(SCM pid ,SCM options)
#define FUNC_NAME "ragnarok-waitpid"
{
  int i;
  int status;
  int ioptions;
  if (SCM_UNBNDP(options))
    ioptions = 0;
  else
    {
      /* Flags are interned in scm_init_posix.  */
      ioptions = scm_to_int(options);
    }

  i = waitpid(scm_to_int(pid), &status, ioptions);

  if (i == -1)
    SCM_BOOL_F;

  return scm_cons (scm_from_int(i), scm_from_int(status));
}
#undef FUNC_NAME

#ifdef __cplusplus
}
#endif
