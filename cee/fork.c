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

  /* I wrap this fork becaust I don't want the error throwing.
   * Though I can catch the error and handle it. But there're something
   * wrong with the logger mechanism.
   */
SCM scm_mmr_fork()
#define FUNC_NAME "ragnarok-fork"
{
  int pid;
  pid = fork();

  return scm_from_int (pid);
}
#undef FUNC_NAME

#ifdef __cplusplus
}
#endif
