#ifndef __RAGNAROK_EPOLL_H__
#define __RAGNAROK_EPOLL_H__
/*	
 *  Copyright (C) 2011-2012
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
#ifdef __HAS_SYS_EPOLL_H__

#include <sys/epoll.h>
#include "generic.h"

#define RAGNAROK_NEW_EVENT scm_ragnarok_make_epoll_event
#define RAGNAROK_EVENT_ADD scm_ragnarok_epoll_add_event
#define RAGNAROK_EVENT_DEL scm_ragnarok_epoll_del_event
#define RAGNAROK_EVENT_HANDLER  scm_ragnarok_epoll_handler
#define RAGNAROK_EVENT_INIT scm_ragnarok_epoll_init
#define RAGNAROK_EVENT_MODULE_INIT() ragnarok_epoll_module_init()

#define RAG_TRUE_P(who)		\
  (SCM_EQ_P(who ,SCM_BOOL_T))

typedef struct Ragnarok_Epoll_Event_Set
{
  int type;
  unsigned int count;
  unsigned int size;
  int epfd;
  struct epoll_event **ee_set;
}scm_rag_epoll_event_set ,scm_rag_event_set;

#define RAG_LT	0
#define RAG_ET	1

typedef struct epoll_event scm_rag_epoll_event;

SCM scm_ragnarok_epoll_init(SCM size);
SCM scm_ragnarok_epoll_handler(SCM event_set_list ,SCM second ,SCM msecond);
SCM scm_ragnarok_epoll_del_event(SCM meta_event ,SCM event_set);
SCM scm_ragnarok_epoll_wait(SCM event_set ,SCM second ,SCM msecond);
SCM scm_ragnarok_epoll_add_event(SCM meta_event ,SCM event_set);
scm_sizet ragnarok_free_epoll_event_set(SCM ee_set);
SCM scm_rag_epoll_event_set2scm(scm_rag_epoll_event_set *ees);
static int ragnarok_print_epoll_event_set(SCM ees_smob ,SCM port,
					  scm_print_state *pstate);
static struct epoll_event* copy_valid_ee(scm_rag_epoll_event_set* es,
					 struct epoll_event* tmp_set);
SCM scm_ragnarok_make_epoll_event(SCM event_fd ,SCM type ,SCM status,
				  SCM mode ,SCM oneshot);
SCM scm_make_epoll_event_set(SCM size ,SCM type ,int epfd);
static inline void rag_epoll_event_set_del_ee(scm_rag_epoll_event_set *ees,
					      struct epoll_event* ee);
static inline void rag_epoll_event_set_add_ee(scm_rag_epoll_event_set *ees,
					      struct epoll_event* ee);
static inline SCM rag_epoll_set_append(SCM read_set ,SCM write_set);
static inline int rag_epoll_create();


#endif // End of __HAS_SYS_EPOLL_H__;

#endif // End of __RAGNAROK_EPOLL_H__;


