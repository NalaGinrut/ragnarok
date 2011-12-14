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
#include <dirent.h>
#include <malloc.h>
#include <errno.h>

#ifdef __cplusplus
extern "C" {
#endif

#define NAMLEN(dirent) strlen((dirent)->d_name)
  
#if defined GUILE_USE_64_CALLS && GUILE_USE_64_CALLS && defined(HAVE_STAT64)
#define CHOOSE_LARGEFILE(foo,foo64)     foo64
#else
#define CHOOSE_LARGEFILE(foo,foo64)     foo
#endif


#define dirent_or_dirent64             	CHOOSE_LARGEFILE(dirent,dirent64)
#define scandir_or_scandir64           	CHOOSE_LARGEFILE(scandir,scandir64)
#define alphasort_or_alphasort64       	CHOOSE_LARGEFILE(alphasort,alphasort64)

  /* This scandir is a shrink version of the glibc version.
   * I believe we don't need versionsort or any other sort in the ragnarok.
   */
SCM scm_mmr_scandir(SCM dir, SCM filter)
#define FUNC_NAME "scandir"
{
    struct dirent_or_dirent64 **rdent;
    int has_filter = 0;
    int n = 0 ,i = 0;
    char *tmp_ptr = NULL;
    SCM flag;
    SCM ret = SCM_EOL;
    SCM *prev;
    SCM str;

    SCM_VALIDATE_STRING(1, dir);

    if(!SCM_UNBNDP(filter))
	{
	    SCM_ASSERT(scm_is_true(scm_procedure_p(filter)),
		       filter ,SCM_ARG2 ,FUNC_NAME);
	    has_filter = 1;
	}

    scm_dynwind_begin(0);
    errno = 0;

    tmp_ptr = scm_to_locale_string(dir);
    scm_dynwind_free(tmp_ptr);

    n = scandir_or_scandir64(tmp_ptr,
			     &rdent, NULL,
			     alphasort_or_alphasort64);

    if(has_filter)
	{
	    for(prev = &ret;i<n;i++)
		{
		    str = rdent[i]?
			scm_from_locale_stringn(rdent[i]->d_name ,NAMLEN(rdent[i]))
			:
			SCM_EOF_VAL;
		    flag = scm_call_1(filter ,str);
		    free(rdent[i]);

		    if(scm_is_true(flag))
			{
			    *prev = scm_cons(str ,SCM_EOL);
			    prev = SCM_CDRLOC(*prev);
			}
		}
	}
    else
	{
	    for(prev = &ret;i<n;i++)
		{
		    str = rdent[i]?
			scm_from_locale_stringn(rdent[i]->d_name ,NAMLEN(rdent[i]))
			:
			SCM_EOF_VAL;
		    *prev = scm_cons(str ,SCM_EOL);
		    prev = SCM_CDRLOC(*prev);
		    free(rdent[i]);
		}
	}

    if(errno != 0)
	SCM_SYSERROR;

    scm_dynwind_end();

    free(rdent);
    
    return ret;
}
#undef FUNC_NAME
  
#ifdef __cplusplus
}
#endif

    
