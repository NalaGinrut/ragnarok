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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#ifdef __HAS_SYS_EPOLL_H__

#include <libguile.h>
#include "event.h"
#include "rag_struct.h"
#include "rag_epoll.h"
#include "rag_error.h"

#ifdef __cplusplus
extern "C" {
#endif

scm_t_bits rag_epoll_event_set_tag;

#define SCM_ASSERT_EPOLL_EVENT(x) scm_assert_smob_type(rag_epoll_event_tag ,(x))
  
#define SCM_ASSERT_EPOLL_EVENT_SET(x) \
  scm_assert_smob_type(rag_epoll_event_set_tag ,(x))

static inline int rag_epoll_create()
{
  /* NOTE: The arg is ignored after linux-2.6.8
   *	   BUT CAN'T BE 0!!!
   */
  int epfd = epoll_create(1); // the arg is ignored after linux-2.6.8

  if(epfd < 0)
    {
      RAG_ERROR1("epoll_create" ,"epoll_create error! returned epfd is %a~%",
		 scm_from_int(epfd));
    }

  return epfd;
}
  
scm_sizet ragnarok_free_epoll_event_set(SCM ee_set)
#define FUNC_NAME "free-epoll-event-set"
{
  scm_rag_epoll_event_set *ees = (scm_rag_epoll_event_set*)SCM_SMOB_DATA(ee_set);

  // close epfd
  close(ees->epfd);

  // NOTE: The second para 'size' is always ignored in Guile 2.x.
  scm_gc_free(ees->ee_set ,0 ,"rag-epoll-event-inner-set");
  scm_gc_free(ee_set ,0 ,"rag-epoll-event-set");

  return 0;
}
#undef FUNC_NAME

SCM scm_rag_epoll_event_set2scm(scm_rag_epoll_event_set *ees)
{
  SCM_RETURN_NEWSMOB(rag_epoll_event_set_tag ,ees);
}

static int ragnarok_print_epoll_event_set(SCM ees_smob ,SCM port,
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
  scm_puts(" >" ,port);

  return 1;
}

SCM scm_ragnarok_make_epoll_event(SCM fd ,SCM type ,SCM status,
				  SCM triger ,SCM oneshot)
#define FUNC_NAME "make-epoll-event"
{
  SCM meta_event;
  scm_rag_mevent *me = NULL;
  scm_rag_epoll_event *ee = NULL;
  
  SCM_VALIDATE_NUMBER(1 ,fd);
  SCM_VALIDATE_NUMBER(2 ,type);
  SCM_VALIDATE_NUMBER(3 ,status);
  SCM_VALIDATE_NUMBER(4 ,triger);
  SCM_VALIDATE_BOOL(5 ,oneshot);

  ee = (scm_rag_epoll_event*)scm_gc_malloc(sizeof(scm_rag_epoll_event) ,"epoll-event");
  ee->data.fd = scm_to_int(fd);

  meta_event = ragnarok_make_meta_event(type ,status ,(void*)ee);
  me = (scm_rag_mevent*)SCM_SMOB_DATA(meta_event);
  me->one_shot = RAG_TRUE_P(oneshot) ? TRUE : FALSE;

  switch(scm_to_int(triger))
    {
    case RAG_ET:
      me->mode = EPOLLET;
      break;
    case RAG_LT:
      me->mode = 0;
      break;
    default:
      RAG_ERROR1("make-epoll-event" ,"invalid triger: %a~%" ,triger);
    }

  /* NOTE: return meta-event rather than epoll-event,
   *       and meta-event contains epoll-event.
   */
  RAG_RETURN_MEVENT2SCM(me);
}
#undef FUNC_NAME

SCM scm_make_epoll_event_set(SCM size ,int epfd)
#define FUNC_NAME "make-epoll-event-set"
{
  unsigned int n = 0;
  int i;
  int t;
  struct epoll_event *ee_set = NULL;
  scm_rag_epoll_event_set *ees = NULL;
  
  SCM_VALIDATE_NUMBER(1 ,size);
  
  n = scm_to_int(size);
  
  ee_set = (struct epoll_event*)scm_gc_malloc(n*sizeof(struct epoll_event),
					      "rag-epoll-event-inner-set");
  // NOTE: clear ee_set array to 0, it's CRITICAL!
  memset(ee_set ,0 ,n*sizeof(struct epoll_event));
  
  ees = (scm_rag_epoll_event_set*)scm_gc_malloc(sizeof(scm_rag_epoll_event_set),
  						"rag-epoll-event-set");
  ees->size = n;
  ees->count = 0;
  ees->ee_set = ee_set;
  ees->epfd = epfd;

  return scm_rag_epoll_event_set2scm(ees);
}
#undef FUNC_NAME

SCM scm_ragnarok_epoll_add_event(SCM meta_event ,SCM event_set)
#define FUNC_NAME "ragnarok-epoll-add-event"
{
  scm_rag_mevent *me = NULL;
  scm_rag_epoll_event *ee = NULL;
  scm_rag_epoll_event_set *ees = NULL;
  int fd;
  int oneshot;
  int mode;
  int ret = 0;
  
  SCM_ASSERT_META_EVENT(meta_event);
  SCM_ASSERT_EPOLL_EVENT_SET(event_set);

  me = (scm_rag_mevent*)SCM_SMOB_DATA(meta_event);
  ees = (scm_rag_epoll_event_set*)SCM_SMOB_DATA(event_set);
  ee = (scm_rag_epoll_event*)me->core;

  if(ees->count >= ees->size)
    {
      RAG_ERROR1("epoll_add" ,"event set exceed! count:~a" ,scm_from_int(ees->count));
    }

  fd = ee->data.fd;
  oneshot = me->one_shot ? EPOLLONESHOT : 0;
  mode = me->mode;
  
  switch(me->type)
    {
    case READ:
      ee->events = EPOLLIN | mode | oneshot;
      break;
    case WRITE:
      ee->events = EPOLLOUT | mode | oneshot;
      break;
    default:
      RAG_ERROR1("epoll_add" ,"invalid event type: %a~%" ,scm_from_int(me->type));
    }
        
  ret = epoll_ctl(ees->epfd ,EPOLL_CTL_ADD ,fd ,ee);
  
  if(ret < 0)
    {
      RAG_ERROR1("epoll_add" ,"epoll_add error! errno is %a~%" ,RAG_ERR2STR(errno));
    }

  ees->count++;
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM scm_ragnarok_epoll_wait(SCM event_set ,SCM second ,SCM msecond)
#define FUNC_NAME "ragnarok-epoll-wait"
{
  scm_rag_epoll_event_set *es = NULL;
  int fd;
  int op;
  int count;
  long s = 0L;
  long ms = 0L;
  SCM cons;
  int nfds;
  SCM ret = SCM_EOL;

  SCM_ASSERT_EPOLL_EVENT_SET(event_set);
  es = (scm_rag_epoll_event_set*)SCM_SMOB_DATA(event_set);

  if(!SCM_UNBNDP(second))
    {
      SCM_VALIDATE_NUMBER(3 ,second);
      s = (long)scm_to_long(second);

      if(!SCM_UNBNDP(msecond))
  	{
  	  SCM_VALIDATE_NUMBER(4 ,msecond);
  	  ms = (long)scm_to_long(msecond);
  	}
      
      ms += s*1000; // convert to mseconds since epoll_wait only accept msecond;
    }

  ms = ms ? ms : -1;

  count = es->count;
  if(!count)
    goto end;
  
  nfds = epoll_wait(es->epfd ,es->ee_set ,count ,ms);
 
  if(nfds < 0)
    {
      RAG_ERROR1("epoll_wait" ,"epoll_wait error! errno shows %a~%",
		 RAG_ERR2STR(errno));	
    }

  while(nfds > 0)
    {
      nfds--;
      fd = es->ee_set[nfds].data.fd;
      op = es->ee_set[nfds].events;
      cons = scm_cons(scm_from_int(fd) ,scm_from_int(op));
      ret = scm_cons(cons ,ret);
    }

  return ret;

 end:
  return SCM_EOL;
}
#undef FUNC_NAME
  
SCM scm_ragnarok_epoll_del_event(SCM meta_event ,SCM event_set)
#define FUNC_NAME "ragnarok-epoll-del-event"
{
  scm_rag_mevent *me = NULL;
  scm_rag_epoll_event_set *ees = NULL;
  struct epoll_event *ee = NULL;
  int fd;
  int ret = 0;
    
  SCM_ASSERT_META_EVENT(meta_event);
  SCM_ASSERT_EPOLL_EVENT_SET(event_set);

  me = (scm_rag_mevent*)SMOB_DATA(meta_event);
  ees = (scm_rag_epoll_event_set*)SMOB_DATA(event_set);
  ee = (scm_rag_epoll_event*)me->core;
  fd = ee->data.fd;

  ret = epoll_ctl(ees->epfd ,EPOLL_CTL_DEL ,fd ,NULL);

  if(ret < 0)
    {
      RAG_ERROR1("epoll_del" ,"epoll_del error! errno shows %a~%",
		 RAG_ERR2STR(errno));
    }

  return scm_from_int(ret);
}
#undef FUNC_NAME

SCM scm_ragnarok_epoll_handler(SCM event_set ,SCM second ,SCM msecond)
#define FUNC_NAME "ragnarok-epoll-handler"
{
  return scm_ragnarok_epoll_wait(event_set ,second ,msecond);
}
#undef FUNC_NAME

SCM scm_ragnarok_epoll_init(SCM size)
#define FUNC_NAME "ragnarok-epoll-init"
{
  int epfd = rag_epoll_create();
  SCM epoll_event_set = scm_make_epoll_event_set(size ,epfd);
  
  return epoll_event_set;
}
#undef FUNC_NAME

// event_set 
SCM_RAG_OBJ_GETTER(epoll_event_set ,count ,count ,scm_from_uint);
SCM_RAG_OBJ_SETTER(epoll_event_set ,count ,count ,scm_from_uint ,scm_to_uint);

SCM_RAG_OBJ_GETTER(epoll_event_set ,size ,size ,scm_from_uint);
SCM_RAG_OBJ_SETTER(epoll_event_set ,size ,size ,scm_from_uint ,scm_to_uint);

SCM_RAG_OBJ_GETTER(epoll_event_set ,epfd ,epfd ,scm_from_int);
SCM_RAG_OBJ_SETTER(epoll_event_set ,epfd ,epfd ,scm_from_int ,scm_to_int);
// we don't need ref/set! of "set"

void ragnarok_epoll_module_init()
{
  // epoll set SMOB init
  rag_epoll_event_set_tag = scm_make_smob_type("ragnarok-epoll-event-type",
					       sizeof(scm_rag_event_set));
  scm_set_smob_print(rag_epoll_event_set_tag ,ragnarok_print_epoll_event_set);
  scm_set_smob_free(rag_epoll_event_set_tag ,ragnarok_free_epoll_event_set);

  scm_c_define_gsubr("make-epoll-event" ,5 ,0 ,0 ,scm_ragnarok_make_epoll_event);
  scm_c_define_gsubr("make-epoll-event-set" ,2 ,0 ,0 ,scm_make_epoll_event_set);

  scm_c_define_gsubr("ragnarok-epoll-init",
  		     1 ,0 ,0 ,scm_ragnarok_epoll_init);
  scm_c_define_gsubr("ragnarok-epoll-handler",
  		     1 ,2 ,0 ,scm_ragnarok_epoll_handler);
  scm_c_define_gsubr("ragnarok-epoll-wait",
  		     1 ,2 ,0 ,scm_ragnarok_epoll_wait);
  scm_c_define_gsubr("ragnarok-epoll-add-event",
  		     2 ,0 ,0 ,scm_ragnarok_epoll_add_event);
  scm_c_define_gsubr("ragnarok-epoll-del-event",
  		     2 ,0 ,0 ,scm_ragnarok_epoll_del_event);

  SCM_MAKE_GSUBR_OBJ_GET(epoll_event_set ,size);
  SCM_MAKE_GSUBR_OBJ_SET(epoll_event_set ,size);

  SCM_MAKE_GSUBR_OBJ_GET(epoll_event_set ,count);
  SCM_MAKE_GSUBR_OBJ_SET(epoll_event_set ,count);

  SCM_MAKE_GSUBR_OBJ_GET(epoll_event_set ,epfd);
  SCM_MAKE_GSUBR_OBJ_SET(epoll_event_set ,epfd);

  //event module interface
  scm_c_define_gsubr("ragnarok-event-init" ,1 ,0 ,0 ,RAGNAROK_EVENT_INIT);
  scm_c_define_gsubr("ragnarok-event-handler" ,1 ,2 ,0 ,RAGNAROK_EVENT_HANDLER);
  scm_c_define_gsubr("ragnarok-event-add" ,2 ,0 ,0 ,RAGNAROK_EVENT_ADD);
  scm_c_define_gsubr("ragnarok-event-del" ,2 ,0 ,0 ,RAGNAROK_EVENT_DEL);
  scm_c_define_gsubr("ragnarok-make-new-event" ,5 ,0 ,0 ,RAGNAROK_NEW_EVENT);
}

#include "event.c.in"

#ifdef __cplusplus
}
#endif

#endif // End of __HAS_SYS_EPOLL_H__;

