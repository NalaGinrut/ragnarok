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

#ifndef __HAS_SYS_EPOLL_H__
#ifndef __HAS_SYS_KQUEUE_H__

#include <libguile.h>
#include "event.h"
#include "rag_struct.h"
#include "rag_select.h"
#include "rag_error.h"
#include "lib_main.h"

#ifdef __cplusplus
extern "C" {
#endif

scm_t_bits scm_rag_fd_set_tag;

#define SCM_ASSERT_FD_SET(x) scm_assert_smob_type(scm_rag_fd_set_tag ,(x))

static SCM scm_rag_fd_set2scm(scm_rag_fd_set *fd_set)
{
  return SCM_RETURN_NEWSMOB(scm_rag_fd_set_tag ,fd_set);
}

static int scm_print_rag_fd_set(SCM fd_set_smob ,SCM port,
				scm_print_state *pstate)
{
  scm_rag_fd_set *fd_set = (scm_rag_fd_set *)SCM_SMOB_DATA(fd_set_smob);
  
  scm_puts("#<rag_fd_set_smob 0x" ,port);
  scm_intprint((long)fd_set ,16 ,port);
  scm_puts( ">", port );
  
  return 1;
}

SCM scm_make_fd_set()
#define FUNC_NAME "make-fd-set"
{
  scm_rag_fd_set *fsd = (scm_rag_fd_set*)scm_gc_malloc(sizeof(scm_rag_fd_set));

  return scm_rag_fd_set2scm(fsd);
}
#undef FUNC_NAME
  
SCM scm_ragnarok_select(SCM nfds ,SCM readfds ,SCM writefds,
		   SCM exceptfds ,SCM second ,SCM msecond)
#define FUNC_NAME "ragnarok-select"
{
  int n = 0;
  scm_rag_fd_set *rfd = NULL;
  scm_rag_fd_set *wfd = NULL;
  scm_rag_fd_set *efd = NULL;
  scm_rag_fd_set *ready_set = NULL;
  long s = 0L;
  long ms = 0L;
  struct timeval tv;

  SCM_VALIDATE_INUM(1 ,nfds);
  SCM_ASSERT_FD_SET(readfds);
  SCM_ASSERT_FD_SET(writefds);
  SCM_ASSERT_FD_SET(exceptfds);

  if(!SCM_UNBNDP(ms))
    {
      SCM_VALIDATE_INUM(5 ,second);
      s = (long)scm_from_long(second);

      if(!SCM_UNBNDP(msecond))
	{
	  SCM_VALIDATE_INUM(6 ,msecond);
	  ms = (long)scm_from_long(msecond);
	}
    }

  n = scm_from_int(nfds);
  rfd = (scm_rag_fd_set*)SMOB_DATA(readfds);
  wfd = (scm_rag_fd_set*)SMOB_DATA(writefds);
  efd = (scm_rag_fd_set*)SMOB_DATA(exceptfds);
    
  tv.tv_sec = (long)s;
  tv.tv_usec = (long)us;

  ready_set = select(n ,rfd ,wfd ,efd ,&tv);
  
  return scm_rag_fd_set2scm(ready_set);
}
#undef FUNC_NAME

SCM scm_FD_CLR(SCM fd ,SCM set)
#define FUNC_NAME "FD-CLR"
{
  int cfd = 0;
  fd_set *fset = NULL;

  SCM_ASSERT_FD_SET(set);
  SCM_VALIDATE_INUM(1 ,fd);

  cfd = scm_from_int(fd);
  fset = (fd_set*)SMOB_DATA(set);

  FD_CLR(cfd ,fset);

  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM scm_FD_ISSET(SCM fd ,SCM set);
#define FUNC_NAME "FD-ISSET"
{
  int cfd = 0;
  fd_set *fset = NULL;

  SCM_ASSERT_FD_SET(set);
  SCM_VALIDATE_INUM(1 ,fd);

  cfd = scm_from_int(fd);
  fset = (fd_set*)SMOB_DATA(set);
    
  return FD_ISSET(cfd ,fset) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM scm_FD_SET(SCM fd ,SCM set);
#define FUNC_NAME "FD-SET"
{
n  int cfd = 0;
  fd_set *fset = NULL;

  SCM_ASSERT_FD_SET(set);
  SCM_VALIDATE_INUM(1 ,fd);

  cfd = scm_from_int(fd);
  fset = (fd_set*)SMOB_DATA(set);

  FD_SET(cfd ,fset);

  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM scm_FD_ZERO(SCM set);
#define FUNC_NAME "FD-ZERO"
{
  fd_set *fset = NULL;

  SCM_ASSERT_FD_SET(set);
  
  fset = (fd_set*)SMOB_DATA(set);

  FD_ZERO(set);

  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM scm_ragnarok_select_handler(SCM event ,SCM fd_set,
				SCM second ,SCM msecond)
#define FUNC_NAME "ragnarok-select-handler"
{
  scm_rag_mevent *me = NULL;
  SCM fd;
  
  SCM_ASSERT_META_EVENT(event);

  me = (scm_rag_mevent*)SCM_SMOB_DATA(event);
  fd = RAG_GET_FD_CORE(me);
  /* FIXME: How can I handle other type?
   */

  switch(me->status)
    {	
    case WAIT:
      // OK ,do we really need this???
    case BLOCK:
      // TODO: BLOCK
    case SLEEP:
      return scm_mmr_sleep(second ,msecond);
      break;
    case DEAD:
      // delete event from event_set if DEAD
      fd_set *fset = SCM_SMOB_DATA(event_set);
      return fd_clr(fd ,fset);
      break;
    case READY:
      return scm_ragnarok_select(scm_from_int(fd) ,fd_set ,second ,msecond);
      break;
    case CLEAR:
      // TODO: CLEAR
    default:
      goto err;
    }
  
 err:
  return RAG_ERROR1("select" ,"invalid event status: %a~%" ,scm_from_int(me->status));
}
#undef FUNC_NAME

SCM scm_ragnarok_select_del_event(SCM event ,SCM fd_set)
#define FUNC_NAME "ragnarok-select-del-event"
{
  int fd;
  fd_set *fset = NULL;
  scm_rag_mevent *me = NULL;
  
  SCM_ASSERT_META_EVENT(event);
  SCM_ASSERT_FD_SET(fd_set);

  me = (scm_rag_mevent*)SCM_SMOB_DATA(event);
  fd = RAG_GET_FD_CORE(me);
  fset = (fd_set*)SMOB_DATA(fd_set);

  FD_CLR(fd ,fset);

  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM scm_ragnarok_select_add_event(SCM event ,SCM fd_set)
#define FUNC_NAME "ragnarok-select-add-event"
{
  int fd;
  fd_set *fset = NULL;
  scm_rag_mevent *me = NULL;
  
  SCM_ASSERT_META_EVENT(event);
  SCM_ASSERT_FD_SET(fd_set);

  me = (scm_rag_mevent*)SCM_SMOB_DATA(event);
  fd = RAG_GET_FD_CORE(me);
  fset = (fd_set*)SMOB_DATA(fd_set);

  FD_SET(fd ,fset);

  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM_RAG_OBJ_GETTER(mevent ,type ,type ,scm_from_int);
SCM_RAG_OBJ_SETTER(mevent ,type ,type ,scm_from_int ,scm_to_int);

SCM_RAG_OBJ_GETTER(mevent ,status ,status ,scm_from_int);
SCM_RAG_OBJ_SETTER(mevent ,status ,status ,scm_from_int ,scm_to_int);

SCM_RAG_OBJ_GETTER(mevent ,core ,core ,rag_scm_from_pointer);
SCM_RAG_OBJ_SETTER(mevent ,core ,core ,PTR2SCM ,SCM2PTR);
  
void rag_select_init()
{
  // fd_set SMOB init
  scm_set_smob_print(scm_rag_fd_set_tag ,scm_print_rag_fd_set);

  // procedure init
  scm_c_define_gsubr("make-fd-set" ,0 ,0 ,0 ,scm_make_fd_set);
  scm_c_define_gsubr("ragnarok-select" ,4 ,2 ,0 ,scm_ragnarok_select);
  scm_c_define_gsubr("FD-CLR" ,2 ,0 ,0 ,scm_FD_CLR);
  scm_c_define_gsubr("FD-ISSET" ,2 ,0 ,0 ,scm_FD_ISSET);
  scm_c_define_gsubr("FD-SET" ,2 ,0 ,0 ,scm_FD_SET);
  scm_c_define_gsubr("FD-ZERO" ,1 ,0 ,0 ,scm_FD_ZERO);
  scm_c_define_gsubr("ragnarok-select-handler",
		     2 ,2 ,0 ,scm_ragnarok_select_handler);
  scm_c_define_gsubr("ragnarok-select-add-event",
		     2 ,0 ,0 ,scm_ragnarok_select_add_event);
  scm_c_define_gsubr("ragnarok-select-del-event",
		     2 ,0 ,0 ,scm_ragnarok_select_del_event);
}

#ifdef __cplusplus
}
#endif

#endif // End of !__HAS_SYS_EPOLL_H__
#endif // End of !__HAS_SYS_KQUEUE_H__;



