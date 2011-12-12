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

#ifndef __RAGNAROK_EPOLL_H__
#define __RAGNAROK_EPOLL_H__

#ifdef __HAS_SYS_EPOLL_H__

#include <sys/epoll.h>

#define RAGNAROK_EVENT_ADD scm_ragnarok_epoll_add_event
#define RAGNAROK_EVENT_DEL scm_ragnarok_select_del_event
#define RAGNAROK_EVENT_HANDLER scm_ragnarok_epoll_handler
#define RAGNAROK_EVENT_MODULE_INIT rag_ragnarok_epoll_init

typedef struct Ragnarok_Epoll_Event_Set
{
  unsigned int count;
  unsigned int size;
  struct epoll_event *set[];
}scm_rag_epoll_event_set;
  
typedef (struct epoll_event scm_rag_epoll_event);
typedef (struct epoll_event scm_epoll_event_set[]);

extern scm_t_bits scm_rag_epoll_event_tag;
extern scm_t_bits scm_rag_epoll_event_set_tag;

#define RAG_EPOLL_GET(es ,elem ,type)  (*((type)*)(es)->(elem))

#endif // End of __HAS_SYS_EPOLL_H__;

#endif // End of __RAGNAROK_EPOLL_H__;


