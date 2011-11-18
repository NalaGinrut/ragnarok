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
#include <malloc.h>
#include <errno.h>

#ifdef __HAS_SYS_EPOLL_H__
#define RAGNAROK_EVENT_HANDLER epoll_handler
#endif // End of __HAS_SYS_EPOLL_H__;

#ifdef __HAS_SYS_KQUEUE_H__
#define RAGNAROK_EVENT_HANDLER kqueue_handler
#endif // End of __HAS_SYS_KQUEUE_H__;

#ifndef __HAS_SYS_EPOLL_H__ && __HAS_SYS_KQUEUE_H__
#define RAGNAROK_EVENT_HANDLER select_handler
#endif // End of __HAS_SYS_EPOLL_H__ && __HAS_SYS_KQUEUE_H__;

#define NO_TIMEOUT 0

#ifdef __cplusplus
extern "C" {
#endif

static inline struct timeval *to_timeval(struct timeval *tp ,long timer)
{
  tp->tv_sec = (long) (timer / 1000);
  tp->tv_usec = (long) ((timer % 1000) * 1000);

  return tp;
}
  
#ifdef __HAS_SYS_EPOLL_H__
#include <sys/epoll.h>
/* use epoll if has epoll */
static inline SCM epoll_handler(int listen_socket,
				struct timeval *tp)
{
  
}
#endif // End of __HAS_SYS_EPOLL_H__;

#ifdef __HAS_SYS_KQUEUE_H__
#include <sys/kqueue.h>
/* use kqueue if has kqueue */
static inline SCM kqueue_handler(int listen_socket,
				 struct timeval *tp)
{

}
#endif // End of __HAS_SYS_KQUEUE_H__;

#ifndef __HAS_SYS_EPOLL_H__ && __HAS_SYS_KQUEUE_H__
/* use select if neither epoll nor kqueue */ 
#include <sys/select.h>
/* According to earlier standards */
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include "rag_select.h"
static inline SCM select_handler(SCM listen_socket,
				 struct timeval *tp)
{
  fd_set read_set ,ready_set;
  SCM ret = SCM_EOL;
  SCM ret = SCM_EOL;
  SCM *prev = ret;
  int listen_fd = scm_fileno(listen_socket);
  
  FD_ZERO(&read_set);
  FD_SET(listen_fd ,&read_set);
  
  ready_set = read_set;

  ready_set = select(listen_fd+1 ,&read_set ,NULL ,NULL ,tp);

  if(FD_ISSET(listen_fd ,&ready_set))
    {
      SCM connect_socket;
      
      connect_socket = scm_accept(listen_socket);
      *prev = scm_cons(connect_socket ,SCM_EOL);
      prev = SCM_CDRLOC(*prev);
    }

  return ret;
}
#endif // End of __HAS_SYS_EPOLL_H__ && __HAS_SYS_KQUEUE_H__;
 
SCM ragnarok_event_handler(SCM listen_socket ,SCM timeout)
{
  long to = 0;
  struct timeval tv ,*tp;
  
  if(!SCM_UNBNDP(timeout))
    {
      SCM_VALIDATE_INUM(2 ,timeout);
      to = scm_from_long(timeout);
    }
  else
    {
      to = NO_TIMEOUT;
    }

  tp = timeout? to_timeval(&tv ,timeout) : NULL;

  return RAGNAROK_EVENT_HANDLER(listen_socket ,tp);
}

#ifdef __cplusplus
}
#endif
