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
#include "rag_error.h"
#include "lib_main.h"

#ifdef __cplusplus
extern "C" {
#endif

scm_t_bits scm_rag_epoll_event_tag;
scm_t_bits scm_rag_epoll_event_set_tag;
  
#define SCM_ASSERT_EPOLL_EVENT(x) scm_assert_smob_type(scm_rag_epoll_event_tag ,(x))
  
#define SCM_ASSERT_EPOLL_EVENT_SET(x) \
  scm_assert_smob_type(scm_rag_epoll_event_set_tag ,(x))

static inline SCM scm_rag_epoll_event_set2scm(scm_rag_epoll_event_set *ees)
{
  return SCM_RETURN_NEWSMOB(scm_rag_epoll_event_set_tag ,ees);
}

static int scm_print_rag_epoll_event(SCM ee_smob ,SCM port,
				     scm_print_state *pstate)
{
  scm_rag_epoll_event *ee = (scm_rag_epoll_event *)SCM_SMOB_DATA(ee_smob);
  
  scm_puts("#<rag_epoll_event_smob 0x" ,port);
  scm_intprint((long)ee ,16 ,port);
  scm_puts(">", port);
  
  return 1;
}

static int scm_print_rag_epoll_event_set(SCM ees_smob ,SCM port,
					 scm_print_state *pstate)
{
  scm_rag_epoll_event_set *ees = (scm_rag_epoll_event_set *)SCM_SMOB_DATA(ees_smob);
  
  scm_puts("#<rag_epoll_event_set_smob 0x" ,port);
  scm_intprint((long)ees ,16 ,port);
  scm_puts(" epfd:" ,port);
  scm_intprint((int)ees->epfd ,10 ,port);
  scm_puts(" size:" ,port);
  scm_intprint((unsigned int)ees->size ,10 ,port);
  scm_puts(" count:" ,port);
  scm_intprint((unsigned int)ees->count ,10 ,port);
  scm_puts(" >", port);
  
  return 1;
}

SCM scm_make_epoll_event(SCM oneshot)
#define FUNC_NAME "make-epoll-event"
{
  SCM_VALIDATE_BOOL(1 ,oneshot);
    
  scm_rag_epoll_event *ee =
    (scm_rag_epoll_event*)scm_gc_malloc(sizeof(scm_rag_epoll_event) ,"epoll-event");

  scm_rag_mevent *me =
    (scm_rag_mevent*)scm_gc_malloc(sizeof(scm_rag_mevent) ,"meta-event");
  
  if(!SCM_UNBNDP(oneshot))
    {
      if(RAG_USE_ONE_SHOT_P(oneshot))
	me->one_shot = TRUE;
      else
	me->one_shot = FALSE;
    }
  else
    {
      me->one_shot = FALSE;
    }

  me->core = (void*)ee;
  
  return scm_rag_mevent2scm(me);
}
#undef FUNC_NAME

SCM scm_make_epoll_event_set(SCM size ,SCM type)
#define FUNC_NAME "make-epoll-event-set"
{
  unsigned int n = 0;
  int t;
  scm_epoll_event_set *set = NULL;
  scm_rag_epoll_event_set *ees = NULL;
  int epfd = 0;
  
  SCM_VALIDATE_INT(1 ,size);
  SCM_VALIDATE_INT(2 ,type);
  
  n = scm_to_uint(size);
  t = scm_to_int(type);
  
  set = (scm_epoll_event_set*)scm_gc_malloc(sizeof(struct epoll_event)*n,
					   "epoll-event-set");

  ees = (scm_rag_epoll_event_set*)scm_gc_malloc(sizeof(scm_rag_epoll_event_set),
						"rag-epoll-event-set");
  epfd = epoll_create(size);

  if(0 > epfd)	
    {
      RAG_ERROR1("epoll_create" ,"epoll_create error! ret is %a~%",
		 scm_from_int(epfd));
    }	

  ees->type = t;
  ees->size = n;
  ees->count = 0;
  ees->set = set;
  ees->epfd = epfd;
  
  return scm_rag_epoll_event_set2scm(ees);
}
#undef FUNC_NAME

SCM scm_ragnarok_epoll_add_event(SCM event ,SCM event_set)
#define FUNC_NAME "ragnarok-epoll-add-event"
{
  scm_rag_epoll_event *ee = NULL;
  scm_rag_epoll_event_set *ees = NULL;
  int fd = 0;
  int oneshot;
  int mode;
  
  SCM_ASSERT_EPOLL_EVENT(event);
  SCM_ASSERT_EPOLL_EVENT_SET(event_set);

  ee = (scm_rag_epoll_event *)SCM_SMOB_DATA(event);
  ees = (scm_rag_epoll_event_set *)SCM_SMOB_DATA(event_set);
  fd = RAG_GET_FD_CORE(ee);

  // FIXME: this must be exclusive
  ees->set[ees->count] = ee;
  ees->count++;
  ee->data.fd = fd;
  oneshot = ee->one_shot ? EPOLLONESHOT : 0;
  
  switch(ee->type)
    {
    case READ:
      e->events = EPOLLIN | mode | oneshot;
    case WRITE:
      e->events = EPOLLOUT | mode | oneshot;
    default:
      RAG_ERROR1("epoll_add" ,"invalid event type: %a~%" ,scm_from_int(ee->type));
    }
        
  ret = epoll_ctl(ees->epfd ,EPOLL_CTL_ADD ,fd,
		  RAG_EPOLL_GET(ees ,set ,scm_rag_epoll_event));

  if(0 > ret)	
    {
      RAG_ERROR1("epoll_del" ,"epoll_del error! ret is %a~%" ,scm_from_int(ret));
    }	

  return scm_rag_epoll_event_set2scm(ees);
}
#undef FUNC_NAME
  
SCM scm_ragnarok_epoll_wait(SCM event_set ,SCM second ,SCM msecond)  
#define FUNC_NAME "ragnarok-epoll-wait"
{
  scm_rag_epoll_event_set *es = NULL;
  long s = 0L;
  long ms = 0L;
  int ret = 0;
  
  SCM_ASSERT_EPOLL_EVENT_SET(event_set);

  if(!SCM_UNBNDP(ms))
    {
      SCM_VALIDATE_INT(3 ,second);
      s = (long)scm_from_long(second);

      if(!SCM_UNBNDP(msecond))
	{
	  SCM_VALIDATE_INT(4 ,msecond);
	  ms = (long)scm_from_long(msecond);
	}
      
      ms += s*1000; // convert to mseconds since epoll_wait only accept msecond;
    }

  ms = ms ? ms : -1;

  es = (scm_rag_epoll_event_set*)SMOB_DATA(event_set);

  ret = epoll_wait(es->epfd,
		   RAG_EPOLL_GET(es ,set ,struct epoll_event), // event set
		   RAG_EPOLL_GET(es ,size ,int), // max events
		   ms);

  if(0 > ret)	
    {
      RAG_ERROR1("epoll_wait" ,"epoll_wait error! ret is %a~%" ,scm_from_int(ret));
    }	

  return scm_from_int(ret);
}
#undef FUNC_NAME
  
SCM scm_ragnarok_epoll_del_event(SCM event ,SCM event_set)
#define FUNC_NAME "ragnarok-epoll-del-event"
{
  scm_rag_epoll_event *ee = NULL;
  scm_rag_epoll_event_set *ees = NULL;
  int ret = 0;
    
  SCM_ASSERT_EPOLL_EVENT(event);
  SCM_ASSERT_EPOLL_EVENT_SET(event_set);

  ee = (scm_rag_epoll_event*)SMOB_DATA(event);
  ees = (scm_rag_epoll_event_set*)SMOB_DATA(event_set);

  ret = epoll_ctl(ees->epfd ,EPOLL_CTL_DEL,
		  RAG_GET_FD_CORE(ee),
		  RAG_EPOLL_GET(ees ,set ,struct epoll_event));

  if(0 > ret)	
    {
      RAG_ERROR1("epoll_del" ,"epoll_del error! errno is %a~%" ,scm_from_int(errno));
    }	
  
  return scm_from_int(ret);
}
#undef FUNC_NAME

SCM scm_ragnarok_epoll_handler(SCM event_set_list ,SCM second ,SCM msecond)
#define FUNC_NAME "ragnarok-epoll-handler"
{
  SCM read_set;
  SCM write_set;
  SCM event_set;
  
  SCM_VALIDATE_LIST(1 ,event_set_list);
  read_set = scm_car(event_set_list);
  write_set = scm_cadr(event_set_list);

  SCM_ASSERT_EPOLL_EVENT_SET(read_set);
  SCM_ASSERT_EPOLL_EVENT_SET(write_set);

  event_set = scm_append(read_set ,write_set);
  
  return scm_ragnarok_epoll(event_set ,second ,msecond);
}
#undef FUNC_NAME

SCM scm_ragnarok_epoll(SCM event_set ,SCM second ,SCM msecond)
#define FUNC_NAME "ragnarok-epoll"
{
  scm_rag_epoll_event_set *ees = NULL;
  int epfd;
  int fd;
  int nfds;
  long s = 0;
  long ms = 0;
  SCM ret = SCM_EOL;
  SCM *prev = &ret;

  SCM_ASSERT_EPOLL_EVENT_SET(event_set);

  if(!SCM_UNBNDP(ms))
    {
      SCM_VALIDATE_INT(2 ,second);
      s = (long)scm_from_long(second);

      if(!SCM_UNBNDP(msecond))
	{
	  SCM_VALIDATE_INT(3 ,msecond);
	  ms = (long)scm_from_long(msecond);
	}
      
      ms += s*1000; // convert to mseconds since epoll_wait only accept msecond;
    }

  ms = ms ? ms : -1;

  ees = (scm_rag_epoll_event_set*)SMOB_DATA(event_set);

  epfd = ees->epfd;

  nfds = epoll_wait(epfd ,event_set ,ees->size ,ms);
  if(0 > nfds)
    {
      RAG_ERROR1("epoll inner" ,"epoll_wait error! ret is %a~%" ,scm_from_int(nfds));
    }

  while(nfds)
    {
      fd = ees->set[nfds--].data.fd;
      *prev = scm_cons(scm_from_int(fd) ,SCM_EOL);
      prev = SCM_CDRLOC(*prev);
    }

  return ret;
}
#undef FUNC_NAME

SCM scm_ragnarok_epoll_init(SCM size)
#define FUNC_NAME "ragnarok-epoll-init"
{
  SCM read_set = scm_make_epoll_event_set(size ,SCM_RAG_READ);
  SCM write_set = scm_make_epoll_event_set(size ,SCM_RAG_WRITE);
  SCM except_set = SCM_BOOL_F; // no except set for epoll
  
  return scm_values(read_set ,write_set ,except_set);
}
#undef FUNC_NAME

// event_set 
SCM_RAG_OBJ_GETTER(event_set ,type ,type ,scm_from_int);
SCM_RAG_OBJ_SETTER(event_set ,type ,type ,scm_from_int ,scm_to_int);

SCM_RAG_OBJ_GETTER(event_set ,count ,count ,scm_from_uint);
SCM_RAG_OBJ_SETTER(event_set ,count ,count ,scm_from_uint ,scm_to_uint);

SCM_RAG_OBJ_GETTER(event_set ,size ,size ,scm_from_uint);
SCM_RAG_OBJ_SETTER(event_set ,size ,size ,scm_from_uint ,scm_to_uint);

SCM_RAG_OBJ_GETTER(event_set ,epfd ,epfd ,scm_from_int);
SCM_RAG_OBJ_SETTER(event_set ,epfd ,epfd ,scm_from_int ,scm_to_int);
// we don't need ref/set! of "set"

void rag_ragnarok_epoll_init()
{
  scm_c_define_gsubr("make-epoll-event" ,0 ,0 ,0 ,scm_make_epoll_event);
  
  scm_c_define_gsubr("ragnarok-epoll-init",
		     2 ,0 ,0 ,scm_ragnarok_epoll_init);
  scm_c_define_gsubr("ragnarok-epoll-handler",
		     1 ,2 ,0 ,scm_ragnarok_epoll_handler);
  scm_c_define_gsubr("ragnarok-epoll" ,1 ,2 ,0 ,scm_ragnarok_epoll);
  scm_c_define_gsubr("ragnarok-epoll-wait",
		     1 ,2 ,0 ,scm_ragnarok_epoll_wait);
  scm_c_define_gsubr("ragnarok-epoll-add-event",
		     2 ,0 ,0 ,scm_ragnarok_epoll_add_event);
  scm_c_define_gsubr("ragnarok-epoll-del-event",
		     2 ,0 ,0 ,scm_ragnarok_epoll_del_event);

  SCM_MAKE_GSUBR_OBJ_GET(event_set ,type);
  SCM_MAKE_GSUBR_OBJ_SET(event_set ,type);

  SCM_MAKE_GSUBR_OBJ_GET(event_set ,size);
  SCM_MAKE_GSUBR_OBJ_SET(event_set ,size);

  SCM_MAKE_GSUBR_OBJ_GET(event_set ,count);
  SCM_MAKE_GSUBR_OBJ_SET(event_set ,count);

  SCM_MAKE_GSUBR_OBJ_GET(event_set ,epfd);
  SCM_MAKE_GSUBR_OBJ_SET(event_set ,epfd);
}


#ifdef __cplusplus
}
#endif

#endif // End of __HAS_SYS_EPOLL_H__;

