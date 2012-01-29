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

#ifndef __RAGNAROK_EVENT_H__
#define __RAGNAROK_EVENT_H__

#include "generic.h"

typedef enum Meta_Event_Type 
  { MET_READ = 0 ,MET_WRITE ,MET_EXCEPT ,MET_UNKNOWN
  }rag_met;

typedef enum Meta_Event_Status
  { MES_WAIT = 0 ,MES_BLOCK ,MES_SLEEP ,MES_DEAD ,MES_READY ,MES_CLEAR,
  }rag_mes;

typedef struct Ragnarok_Meta_Event
{
  rag_met type;
  rag_mes status;

  /* NOTE: mode is only between 'level-triger' and 'edge-triger'.
           So only kqueue/epoll uses it. select/poll doesn't.
	   And Ragnarok doesn't support poll.
  */
  int mode;

#ifdef __HAS_SYS_EPOLL_H__
  int one_shot; // is EPOLLONSHOT mode?
#endif // End of __HAS_SYS_EPOLL_H__;
  
  void* core;
}*ragnarok_meta_event ,scm_rag_mevent;

extern const char const *ragnarok_meta_type_string[];
extern const char const *ragnarok_meta_status_string[];
extern scm_t_bits rag_mevent_tag;

#define SCM_ASSERT_META_EVENT(x) \
  scm_assert_smob_type(rag_mevent_tag ,x)

#define RAG_GET_FD_CORE(mevent) (*(int*)((mevent)->core))

static inline void
RAG_ME_PRN_CORE(ragnarok_meta_event me ,SCM port) __attribute__((always_inline));

#define RAG_ME_GET_TYPE(me)			\
  ragnarok_meta_type_string[(me)->type]

#define RAG_ME_GET_STATUS(me) \
  ragnarok_meta_status_string[(me)->status]

static inline void RAG_ME_PRN_CORE(ragnarok_meta_event me ,SCM port)
{
  switch(me->type)
    {		
    case MET_READ:	
    case MET_WRITE:
    case MET_EXCEPT:
      scm_intprint(*(int*)me->core ,10 ,port);
      break;
    default:
      scm_puts("UNKNOWN meta event core" ,port);
    }
}

#define RAG_RETURN_MEVENT2SCM(me) SCM_RETURN_NEWSMOB(rag_mevent_tag ,me)

SCM ragnarok_make_meta_event(SCM type ,SCM status ,SCM core);
SCM ragnarok_meta_event_p(SCM event);
int ragnarok_print_meta_event(SCM me_smob ,SCM port ,scm_print_state *pstate);
SCM ragnarok_clear_meta_event(SCM me_smob);
scm_sizet ragnarok_free_meta_event(SCM me_smob);
void init_meta_event_type();
void init_event_module();

#endif // End of __RAGNAROK_EVENT_H__;
