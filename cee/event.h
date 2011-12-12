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

#ifndef __RAGNAROK_EVENT_H__
#define __RAGNAROK_EVENT_H__

typedef enum Meta_Event_Type 
  { READ_FD = 0 ,WRITE_FD ,ERR_MSG ,UNKNOWN
  }rag_met;

typedef enum Meta_Event_Status 
  { WAIT = 0 ,BLOCK ,SLEEP ,DEAD ,READY ,CLEAR
  }rag_mes;

typedef struct Ragnarok_Meta_Event
{
  rag_met type;
  rag_mes status;
  void* core;

#ifdef __HAS_SYS_EPOLL_H__
  int epfd; // epoll fd
#endif // End of __HAS_SYS_EPOLL_H__;
  
}*ragnarok_meta_event ,scm_rag_mevent;

extern const char const *ragnarok_meta_type_string[];
extern const char const *ragnarok_meta_status_string[];
extern scm_t_bits rag_mevent_tag;

#define SCM_ASSERT_META_EVENT(x) \
  scm_assert_smob_type(ragnarok_meta_event_tag ,x)

#define RAG_GET_FD_CORE(mevent) (*(int*)((mevent)->core))

#define RAG_ME_GET_TYPE(me) \
  ragnarok_meta_type_string[(me)->type]

#define RAG_ME_GET_STATUS(me) \
  ragnarok_meta_status_string[(me)->status]

static inline void
RAG_ME_PRN_CORE(ragnarok_meta_event me ,SCM port) __attribute__((always_inline));

static inline void RAG_ME_PRN_CORE(ragnarok_meta_event me ,SCM port)
{
  switch(me->type)
    {		
    case READ_FD:	
    case WRITE_FD:	
      scm_intprint(*(int*)me->core ,10 ,port);
      break;
    case ERR_MSG:
      scm_puts((char*)me->core ,port);
      break;
    default:
      scm_puts("UNKNOWN meta event core" ,port);
    }
}

#endif // End of __RAGNAROK_EVENT_H__;
