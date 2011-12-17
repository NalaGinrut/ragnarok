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

#ifndef __RAGNAROK_SELECT_H__
#define __RAGNAROK_SELECT_H__

#ifndef __HAS_SYS_EPOLL_H__
#ifndef __HAS_SYS_KQUEUE_H__

/* use select if neither epoll nor kqueue */ 
#include <sys/select.h>
/* According to earlier standards */
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#define RAGNAROK_EVENT_ADD scm_ragnarok_select_add_event
#define RAGNAROK_EVENT_DEL scm_ragnarok_select_del_event
#define RAGNAROK_EVENT_HANDLER scm_ragnarok_select_handler
#define RAGNAROK_EVENT_INIT scm_ragnarok_select_init
#define RAGNAROK_EVENT_MODULE_INIT rag_select_init

typedef struct Ragnarok_Select_Event_Set
{
  int type;
  unsigned int count;
  unsigned int size;
  int nfds;
  fd_set *set;
}scm_rag_select_event_set ,scm_rag_event_set;

typedef fd_set scm_rag_fd_set;

extern scm_t_bits scm_rag_select_event_set_tag;

void rag_select_init();


#endif // End of no __HAS_SYS_KQUEUE_H__;
#endif // End of no __HAS_SYS_EPOLL_H__;

#endif // __RAGNAROK_SELECT_H__;
