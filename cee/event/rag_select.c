/*	
 *  Copyright (C) 2011
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
scm_t_bits scm_rag_select_event_set_tag;
  
#define SCM_ASSERT_EVENT_SET(x) 	\
  scm_assert_smob_type(scm_rag_select_event_set_tag ,(x))

static SCM scm_rag_select_event_set2scm(scm_rag_epoll_event_set *event_set,
					ses_type type)
{
  SCM_RETURN_NEWSMOB(scm_rag_select_event_set_tag ,event_set);
}

static int scm_print_rag_select_event_set(SCM event_set_smob ,SCM port,
					  scm_print_state *pstate)
{
  scm_rag_select_event_set *ses =
    (scm_rag_select_event_set*)SCM_SMOB_DATA(event_set_smob);
  
  scm_puts("#<rag_select_event_set_smob 0x" ,port);
  scm_intprint((long)ses ,16 ,port)					;
  scm_puts(" nfds:");
  scm_intprint((int)ses->nfds ,10 ,port);
  scm_puts(" size:");
  scm_intprint((unsigned int)ses->size ,10 ,port);
  scm_puts(" count:");
  scm_intprint((unsigned int)ses->count ,10 ,port);
  scm_puts(" >", port);
  
  return 1;
}

SCM scm_make_select_event_set(SCM nfds ,SCM size ,SCM type)
#define FUNC_NAME "make-event-set"
{
  int t;
  unsigned int n = 0;
  int fd;
  
  SCM_VALIDATE_NUMBER(1 ,nfds);
  SCM_VALIDATE_NUMBER(2 ,size);
  SCM_VALIDATE_NUMBER(3 ,type);
  
  t = scm_to_int(type);
  n = scm_to_uint(size);
  fd = scm_to_int(nfds);
  
  scm_rag_fd_set *rfd = (scm_rag_fd_set*)scm_gc_malloc(sizeof(scm_rag_fd_set));
  
  scm_rag_select_event_set *ses =
    (scm_rag_select_event_set*)scm_gc_malloc(sizeof(scm_rag_select_event_set),
					     "select-event-set");

  ses->type = t;
  ses->count = 0;
  ses->size = n;
  ses->nfds = fd;
  ses->set = rfd;
  
  return scm_rag_select_event_set2scm(ses);
}
#undef FUNC_NAME
  
SCM scm_ragnarok_select(SCM nfds ,SCM read_set ,SCM write_set,
			SCM except_set ,SCM second ,SCM msecond)
#define FUNC_NAME "ragnarok-select"
{
  int n = 0;
  scm_rag_fd_set *rs = NULL;
  scm_rag_fd_set *ws = NULL;
  scm_rag_fd_set *es = NULL;
  scm_rag_fd_set *ready_set = NULL;
  long s = 0L;
  long ms = 0L;
  int i;
  struct timeval tv;
  SCM ret = SCM_EOL;
  SCM *prev = &ret;

  SCM_VALIDATE_NUMBER(1 ,nfds);
  SCM_ASSERT_EVENT_SET(read_set);
  SCM_ASSERT_EVENT_SET(write_set);
  SCM_ASSERT_EVENT_SET(except_set);

  if(!SCM_UNBNDP(ms))
    {
      SCM_VALIDATE_NUMBER(5 ,second);
      s = (long)scm_from_long(second);

      if(!SCM_UNBNDP(msecond))
	{
	  SCM_VALIDATE_NUMBER(6 ,msecond);
	  ms = (long)scm_from_long(msecond);
	}
    }

  n = scm_from_int(nfds);
  rs = (scm_rag_event_set*)SMOB_DATA(read_set);
  ws = (scm_rag_event_set*)SMOB_DATA(write_set);
  es = (scm_rag_event_set*)SMOB_DATA(except_set);
    
  tv.tv_sec = (long)s;
  tv.tv_usec = (long)us;

  ready_set = select(n ,rs->set ,ws->set ,es->set ,&tv);
    
  for(i=0;i<n;i++)
    {
      if(FD_ISSET(i ,&ready_set))
	{
	  *prev = scm_cons(scm_from_int(i) ,SCM_EOL);
	  prev = SCM_CDRLOC(*prev);
	}
    }

  return ret;
}
#undef FUNC_NAME

SCM scm_FD_CLR(SCM fd ,SCM set)
#define FUNC_NAME "FD-CLR"
{
  int cfd = 0;
  fd_set *fset = NULL;

  SCM_ASSERT_FD_SET(set);
  SCM_VALIDATE_NUMBER(1 ,fd);

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
  SCM_VALIDATE_NUMBER(1 ,fd);

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
  SCM_VALIDATE_NUMBER(1 ,fd);

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

SCM scm_ragnarok_select_handler(SCM event_set_list ,SCM second ,SCM msecond)
#define FUNC_NAME "ragnarok-select-handler"
{
  scm_rag_mevent *me = NULL;
  SCM rs ,ws ,es;
  SCM fd;
  
  SCM_ASSERT_META_EVENT(event);
  SCM_VALIDATE_LIST(2 ,event_set_list);

  rs = scm_car(event_set_list);
  ws = scm_cadr(event_set_list);
  es = scm_caddr(event_set_list);
  
  me = (scm_rag_mevent*)SCM_SMOB_DATA(event);
  fd = RAG_GET_FD_CORE(me);
  /* FIXME: How can I handle other type?
   */

  // FIXME: select need us count the MAX fd then +1, now I haven't done this yet.
  return scm_ragnarok_select(scm_from_int(fd) ,rs ,ws ,es ,second ,msecond);
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

SCM scm_ragnarok_select_init(SCM size)
#define FUNC_NAME "ragnarok-select-add-event"
{
  SCM r = scm_from_int(READ);
  SCM w = scm_from_int(WRITE);
  SCM e = scm_from_int(EXCEPT);
  
  SCM read_set = scm_make_select_event_set(0 ,size ,r);
  SCM write_set = scm_make_select_event_set(0 ,size ,w);
  SCM except_set = scm_make_select_event_set(0 ,size ,e);
  
  return scm_values(scm_list_3(read_set ,write_set ,except_set));
}
#undef FUNC_NAME

// event_set 
SCM_RAG_OBJ_GETTER(event_set ,type ,type ,scm_from_int);
SCM_RAG_OBJ_SETTER(event_set ,type ,type ,scm_from_int ,scm_to_int);

SCM_RAG_OBJ_GETTER(event_set ,count ,count ,scm_from_uint);
SCM_RAG_OBJ_SETTER(event_set ,count ,count ,scm_from_uint ,scm_to_uint);

SCM_RAG_OBJ_GETTER(event_set ,size ,size ,scm_from_uint);
SCM_RAG_OBJ_SETTER(event_set ,size ,size ,scm_from_uint ,scm_to_uint);

SCM_RAG_OBJ_GETTER(event_set ,nfds ,nfds ,scm_from_int);
SCM_RAG_OBJ_SETTER(event_set ,nfds ,nfds ,scm_from_int ,scm_to_int);
// we don't need ref/set! of "set"

void rag_select_init()
{
  // fd_set SMOB init
  scm_set_smob_print(scm_rag_fd_set_tag ,scm_print_rag_fd_set);

  // procedure init
  scm_c_define_gsubr("make-fd-set" ,0 ,0 ,0 ,scm_make_fd_set);
  scm_c_define_gsubr("FD-CLR" ,2 ,0 ,0 ,scm_FD_CLR);
  scm_c_define_gsubr("FD-ISSET" ,2 ,0 ,0 ,scm_FD_ISSET);
  scm_c_define_gsubr("FD-SET" ,2 ,0 ,0 ,scm_FD_SET);
  scm_c_define_gsubr("FD-ZERO" ,1 ,0 ,0 ,scm_FD_ZERO);
  scm_c_define_gsubr("ragnarok-select" ,4 ,2 ,0 ,scm_ragnarok_select);
  scm_c_define_gsubr("ragnarok-select-handler",
		     1 ,2 ,0 ,scm_ragnarok_select_handler);
  scm_c_define_gsubr("ragnarok-select-add-event",
		     2 ,0 ,0 ,scm_ragnarok_select_add_event);
  scm_c_define_gsubr("ragnarok-select-del-event",
		     2 ,0 ,0 ,scm_ragnarok_select_del_event);
  scm_c_define_gsubr("ragnarok-select-init",
		     0 ,1 ,0 ,scm_ragnarok_select_init);

  SCM_MAKE_GSUBR_OBJ_GET(event_set ,type);
  SCM_MAKE_GSUBR_OBJ_SET(event_set ,type);

  SCM_MAKE_GSUBR_OBJ_GET(event_set ,count);
  SCM_MAKE_GSUBR_OBJ_SET(event_set ,count);

  SCM_MAKE_GSUBR_OBJ_GET(event_set ,size);
  SCM_MAKE_GSUBR_OBJ_SET(event_set ,size);

  SCM_MAKE_GSUBR_OBJ_GET(event_set ,nfds);
  SCM_MAKE_GSUBR_OBJ_SET(event_set ,nfds);

  // event module interface
  scm_c_define_gsubr("ragnarok-event-init" ,1 ,0 ,0 ,RAGNAROK_EVENT_INIT);
  scm_c_define_gsubr("ragnarok-event-handler" ,1 ,2 ,0 ,RAGNAROK_EVENT_HANDLER);
  scm_c_define_gsubr("ragnarok-event-add" ,2 ,0 ,0 ,RAGNAROK_EVENT_ADD);
  scm_c_define_gsubr("ragnarok-event-del" ,2 ,0 ,0 ,RAGNAROK_EVENT_DEL);
}

#include "event.c.in"
  
#ifdef __cplusplus
}
#endif

#endif // End of !__HAS_SYS_EPOLL_H__
#endif // End of !__HAS_SYS_KQUEUE_H__;



