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

SCM scm_mmr_path_fix(SCM target);
SCM scm_mmr_scandir(SCM dir ,SCM filter);
SCM scm_mmr_check_file_perms(SCM target ,SCM perms);

void init_lib()
{
  scm_c_define_gsubr("path-fix" ,1 ,0 ,0 ,scm_mmr_path_fix);
  scm_c_define_gsubr("check-file-perm" ,2 ,0 ,0 ,scm_mmr_check_file_perms);
  scm_c_define_gsubr("scandir" ,1 ,1 ,0 ,scm_mmr_scandir);
}