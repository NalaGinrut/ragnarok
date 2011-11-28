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

#ifdef __HAS_SYS_EPOLL_H__

#include <libguile.h>
#include "event.h"
#include "rag_struct.h"
#include "rag_epoll.h"
#include "lib_main.h"

#ifdef __cplusplus
extern "C" {
#endif

SCM scm_ragnarok_epoll_add_event(SCM event ,SCM event_set)
{
  
}
  
SCM scm_ragnarok_epoll_del_event(SCM event ,SCM event_set)
{
  
}
  
SCM scm_ragnarok_epoll_handler(SCM event ,SCM event_set ,SCM second ,SCM msecond)
{
  
}

void rag_ragnarok_epoll_init()
{
  scm_c_define_gsubr("ragnarok-epoll-handler",
		     2 ,2 ,0 ,scm_ragnarok_epoll_handler);
  scm_c_define_gsubr("ragnarok-epoll-add-event",
		     2 ,0 ,0 ,scm_ragnarok_epoll_add_event);
  scm_c_define_gsubr("ragnarok-epoll-del-event",
		     2 ,0 ,0 ,scm_ragnarok_epoll_del_event);
}


#ifdef __cplusplus
}
#endif

#endif // End of __HAS_SYS_EPOLL_H__;

