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

static SCM scm_rag_epoll_event2scm(scm_rag_epoll_event *ee)
{
  return SCM_RETURN_NEWSMOB(scm_rag_epoll_event_tag ,ee);
}

static SCM scm_rag_epoll_event_set2scm(scm_rag_epoll_event_set *ees)
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
  scm_rag_epoll_event *ees = (scm_rag_epoll_event_set *)SCM_SMOB_DATA(ees_smob);
  
  scm_puts("#<rag_epoll_event_set_smob 0x" ,port);
  scm_intprint((long)ees ,16 ,port);
  scm_puts(" size:");
  scm_intprint((unsigned int)ees->size ,10 ,port);
  scm_puts(" count:");
  scm_intprint((unsigned int)ees->count ,10 ,port);
  scm_puts(" >", port);
  
  return 1;
}

SCM scm_make_epoll_event()
#define FUNC_NAME "make-epoll-event"
{
  scm_rag_epoll_event *ee =
    (scm_rag_epoll_event*)scm_gc_malloc(sizeof(scm_rag_epoll_event) ,"epoll-event");

  return scm_rag_epoll_event2scm(ee);
}
#undef FUNC_NAME

SCM scm_make_epoll_event_set(SCM size)
#define FUNC_NAME "make-epoll-event-set"
{
  int n = 0;
  scm_epoll_event_set *set = NULL;
  scm_rag_epoll_event_set *ees = NULL;
  
  SCM_VALIDATE_INT(size);

  n = scm_to_int(size);
  
  set = (scm_epoll_event_set)scm_gc_malloc(sizeof(struct epoll_event)*n,
					   "epoll-event-set");
  ees = (scm_rag_epoll_event_set*)scm_gc_malloc(sizeof(scm_rag_epoll_event_set),
						"rag-epoll-event-set");

  ees->size = n;
  ees->count = 0;
  ees->set = set;
  
  return scm_rag_epoll_event_set2scm(ees);
}
#undef FUNC_NAME

SCM_RAG_OBJ_GETTER(epoll_event_set ,size ,size ,scm_from_uint);
SCM_RAG_OBJ_SETTER(epoll_event_set ,size ,size ,scm_from_uint ,scm_to_uint);

SCM_RAG_OBJ_GETTER(epoll_event_set ,count ,count ,scm_from_uint);
SCM_RAG_OBJ_SETTER(epoll_event_set ,count ,count ,scm_from_uint ,scm_to_uint);

SCM scm_ragnarok_epoll_add_event(SCM event ,SCM event_set)
#define FUNC_NAME "ragnarok-epoll-add-event"
{
  scm_rag_epoll_event *ee = NULL;
  scm_rag_epoll_event_set *ees = NULL;

  SCM_ASSERT_EPOLL_EVENT(event);
  SCM_ASSERT_EPOLL_EVENT_SET(event_set);

  ee = (scm_rag_epoll_event *)SCM_SMOB_DATA(event);
  ees = (scm_rag_epoll_event_set *)SCM_SMOB_DATA(event_set);

  // FIXME: this must be exclusive
  ees->set[ees->count] = ee;
  ees->count++;

  ret = epoll_ctl(ee->epfd ,RAG_EPOLL_GET_MODE
		  RAG_GET_FD_CORE(ee),
		  RAG_EPOLL_GET(es ,set ,struct epoll_event));

  if(0 > ret)	
    {
      RAG_ERROR1("epoll_del" ,"epoll_del error! errno is %a~%" ,scm_from_int(ret));
    }	

  return scm_rag_epoll_event_set2scm(ees);
}
#undef FUNC_NAME
  
SCM scm_ragnarok_epoll_wait(SCM event ,SCM event_set ,SCM second ,SCM msecond)  
#define FUNC_NAME "ragnarok-epoll-wait"
{
  scm_rag_epoll_event *ee = NULL;
  scm_rag_epoll_event_set *es = NULL;
  long s = 0L;
  long ms = 0L;
  int ret = 0;
  
  SCM_ASSERT_EPOLL_EVENT(event);
  SCM_ASSERT_EPOLL_EVENT_SET(event_set);

  if(!SCM_UNBNDP(ms))
    {
      SCM_VALIDATE_INUM(3 ,second);
      s = (long)scm_from_long(second);

      if(!SCM_UNBNDP(msecond))
	{
	  SCM_VALIDATE_INUM(4 ,msecond);
	  ms = (long)scm_from_long(msecond);
	}
      
      ms += s*1000; // convert to mseconds since epoll_wait only accept msecond;
    }

  ms = ms ? ms : -1;

  ee = (scm_rag_epoll_event*)SMOB_DATA(event);
  es = (scm_rag_epoll_event_set*)SMOB_DATA(event_set);

  ret = epoll_wait(ee->epfd,
		   RAG_EPOLL_GET(es ,set ,struct epoll_event), // event set
		   RAG_EPOLL_GET(es ,size ,int), // max events
		   ms);

  if(0 > ret)	
    {
      RAG_ERROR1("epoll_wait" ,"epoll_wait error! errno is %a~%" ,scm_from_int(ret));
    }	

  return scm_from_int(ret);
}
#undef FUNC_NAME
  
SCM scm_ragnarok_epoll_del_event(SCM event ,SCM event_set)
#define FUNC_NAME "ragnarok-epoll-del-event"
{
  scm_rag_epoll_event *ee = NULL;
  scm_rag_epoll_event_set *es = NULL;
  int ret = 0;
    
  SCM_ASSERT_EPOLL_EVENT(event);
  SCM_ASSERT_EPOLL_EVENT_SET(event_set);

  ee = (scm_rag_epoll_event*)SMOB_DATA(event);
  es = (scm_rag_epoll_event_set*)SMOB_DATA(event_set);

  ret = epoll_ctl(ee->epfd ,EPOLL_CTL_DEL,
		  RAG_GET_FD_CORE(ee),
		  RAG_EPOLL_GET(es ,set ,struct epoll_event));

  if(0 > ret)	
    {
      RAG_ERROR1("epoll_del" ,"epoll_del error! errno is %a~%" ,scm_from_int(ret));
    }	
  
  return scm_from_int(ret);
}
#undef FUNC_NAME

SCM scm_ragnarok_epoll_handler(SCM event ,SCM event_set ,SCM second ,SCM msecond)
#define FUNC_NAME "ragnarok-epoll-handler"
{
  scm_rag_mevent *me = NULL;

  SCM_ASSERT_EPOLL_EVENT(event);
  SCM_ASSERT_EPOLL_EVENT_SET(event_set);

  me = (scm_rag_epoll_event*)SMOB_DATA(event);

  switch(me->status)
    {	
    case WAIT:
      // OK ,do we really need this???
    case BLOCK:
      // TODO: BLOCK
    case SLEEP:
      return scm_ragnarok_epoll_wait(event ,event_set ,second ,msecond);
      break;
    case DEAD:
      // delete event from event_set if DEAD
      return scm_ragnarok_epoll_del(event ,event_set);
      break;
    case READY:
      return scm_ragnarok_epoll(scm_from_int(fd) ,fd_set ,second ,msecond);
      break;
    case CLEAR:
      // TODO: CLEAR
    default:
      goto err;
    }

  return RAG_ERROR1("epoll" ,"invalid event status: %a~%" ,scm_from_int(me->status));
}
#undef FUNC_NAME

void rag_ragnarok_epoll_init()
{
  scm_c_define_gsubr("make-epoll-event" ,0 ,0 ,0 ,scm_make_epoll_event);
  
  scm_c_define_gsubr("ragnarok-epoll-handler",
		     2 ,2 ,0 ,scm_ragnarok_epoll_handler);
  scm_c_define_gsubr("ragnarok-epoll-wait",
		     2 ,2 ,0 ,scm_ragnarok_epoll_wait);
  scm_c_define_gsubr("ragnarok-epoll-add-event",
		     2 ,0 ,0 ,scm_ragnarok_epoll_add_event);
  scm_c_define_gsubr("ragnarok-epoll-del-event",
		     2 ,0 ,0 ,scm_ragnarok_epoll_del_event);

  SCM_MAKE_GSUBR_OBJ_GET(epoll_event_set ,size);
  SCM_MAKE_GSUBR_OBJ_SET(epoll_event_set ,size);

  SCM_MAKE_GSUBR_OBJ_GET(epoll_event_set ,count);
  SCM_MAKE_GSUBR_OBJ_SET(epoll_event_set ,count);
}


#ifdef __cplusplus
}
#endif

#endif // End of __HAS_SYS_EPOLL_H__;

