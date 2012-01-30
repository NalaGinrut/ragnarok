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

  if(0 > epfd)
    {
      RAG_ERROR1("epoll_create" ,"epoll_create error! errno is %a~%",
  		 RAG_ERR2STR(errno));
    }

  return epfd;
}
  
  /* NOTE: I don't check type in this function since it's no need to
   *	   do this check twice. And one SHOULD ONLY use this function
   *	   just one time in scm_ragnarok_epoll_handler.
   */
static inline SCM rag_epoll_set_append(SCM read_set ,SCM write_set)
{
  scm_rag_epoll_event_set *res = (scm_rag_epoll_event_set*)SCM_SMOB_DATA(read_set);
  scm_rag_epoll_event_set *wes = (scm_rag_epoll_event_set*)SCM_SMOB_DATA(write_set);
  int rn = res->size;
  int wn = wes->size;
  SCM size = scm_from_int(rn + wn);
  SCM event_set = scm_make_epoll_event_set(size ,SCM_RAG_WRITE ,res->epfd);
  scm_rag_epoll_event_set *es = (scm_rag_epoll_event_set*)SCM_SMOB_DATA(event_set);

  /* NOTE: I can't use -std=c99 in Ragnarok since I've already written too many
   *	   C99-incompatible code. So an ugly pre-declared variables here.
   */
  int i,j; 
  
  /* FIXME: I wish these two 'for's can be optimized to parallelize.
   *	    But I'm not sure about it.
   */
  for(i=0 ;i<rn ;i++)
    es->fd_set[i] = res->fd_set[i];

  for(j=rn ;j<wn ;j++)
    es->fd_set[j] = wes->fd_set[j];

  return event_set;
}
  
static inline void rag_epoll_event_set_add_fd(scm_rag_epoll_event_set *ees ,int fd)
{
  unsigned int i = 0;

  // NOTE: this doesn't have to be exclusive, because redundent adding won't be harmful.

  // NOTE: this proc won't check event-set exceed situation.

  while(i < ees->count)
    {
      // find whether fd is existed
      if(ees->fd_set[i] == fd)
  	return;
      
      i++;
    }

  i = 0;

  // no sufficient fd, add to a void slot
  while(i < ees->count)
    {
      if(!ees->fd_set[i])
  	ees->fd_set[i] = fd;

      i++;
    }

  // no void slot, add to a new slot
  if(i >= ees->count)
    ees->fd_set[i] = fd;

  ees->count++;
}

static inline void rag_epoll_event_set_del_fd(scm_rag_epoll_event_set *ees ,int fd)
{
  unsigned int i = 0;

  // NOTE: this doesn't have to be exclusive, becaust redundent add won't do anything
  
  // NOTE: this proc won't do anything if no such fd found.
  while(i < ees->count)
    {
      if(ees->fd_set[i] == fd)
  	{
  	  ees->fd_set[i] = 0;
  	  ees->count--;
  	  return;
  	}

      i++;
    }
}

scm_sizet ragnarok_free_epoll_event_set(SCM ee_set)
#define FUNC_NAME "free-epoll-event-set"
{
  scm_rag_epoll_event_set *ees = (scm_rag_epoll_event_set*)SCM_SMOB_DATA(ee_set);

  // NOTE: The second para 'size' is always ignored in Guile 2.x.
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

SCM scm_ragnarok_make_epoll_event(SCM event_fd ,SCM type ,SCM status,
				  SCM triger ,SCM oneshot)
#define FUNC_NAME "make-epoll-event"
{
  int fd;
  SCM meta_event;
  scm_rag_mevent *me = NULL;
  scm_rag_epoll_event *ee = NULL;
  
  SCM_VALIDATE_NUMBER(1 ,event_fd);
  SCM_VALIDATE_NUMBER(2 ,type);
  SCM_VALIDATE_NUMBER(3 ,status);
  SCM_VALIDATE_NUMBER(4 ,triger);
  SCM_VALIDATE_BOOL(5 ,oneshot);

  fd = scm_to_int(event_fd);
  ee = (scm_rag_epoll_event*)scm_gc_malloc(sizeof(scm_rag_epoll_event) ,"epoll-event");
  meta_event = ragnarok_make_meta_event(type ,status ,event_fd);
  me = (scm_rag_mevent*)SCM_SMOB_DATA(meta_event);

  if(RAG_TRUE_P(oneshot))
    me->one_shot = TRUE;
  else
    me->one_shot = FALSE;

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
      
  ee->data.fd = fd;
  me->core = (void*)ee;

  /* NOTE: return meta-event rather than epoll-event,
   *       and meta-event contains epoll-event.
   */
  RAG_RETURN_MEVENT2SCM(me);
}
#undef FUNC_NAME

SCM scm_make_epoll_event_set(SCM size ,SCM type ,int epfd)
#define FUNC_NAME "make-epoll-event-set"
{
  unsigned int n = 0;
  int t;
  int *fd_set = NULL;
  scm_rag_epoll_event_set *ees = NULL;
  
  SCM_VALIDATE_NUMBER(1 ,size);
  SCM_VALIDATE_NUMBER(2 ,type);
  
  n = scm_to_uint(size);
  t = scm_to_int(type);
  
  fd_set = (int*)scm_gc_malloc(sizeof(int)*n ,"epoll-fd-set");
  // NOTE: clear fd_set array to 0, it's CRITICAL!
  memset(fd_set ,0 ,n);
  
  ees = (scm_rag_epoll_event_set*)scm_gc_malloc(sizeof(scm_rag_epoll_event_set),
  						"rag-epoll-event-set");
  ees->type = t;
  ees->size = n;
  ees->count = 0;
  ees->fd_set = fd_set;
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

  if(ees->count > ees->size)
    {
      RAG_ERROR1("epoll_add" ,"event set exceed! count:~a" ,scm_from_int(ees->count));
    }

  fd = ee->data.fd;
  oneshot = me->one_shot ? EPOLLONESHOT : 0;
  
  switch(ees->type)
    {
    case READ:
      ee->events = EPOLLIN | mode | oneshot;
      break;
    case WRITE:
      ee->events = EPOLLOUT | mode | oneshot;
      break;
    default:
      RAG_ERROR1("epoll_add" ,"invalid event type: %a~%" ,scm_from_int(ees->type));
    }
        
  ret = epoll_ctl(ees->epfd ,EPOLL_CTL_ADD ,fd ,ee);
  
  if(0 > ret)
    {
      RAG_ERROR1("epoll_add" ,"epoll_add error! errno is %a~%" ,RAG_ERR2STR(errno));
    }

  /* NOTE: I believe the fd_set adding operation must be later than epoll_ctl ADD.
   *       In case it's been scheduled to do any epoll_wait.
   */
  rag_epoll_event_set_add_fd(ees ,fd);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
  
SCM scm_ragnarok_epoll_wait(SCM event_set ,SCM second ,SCM msecond)
#define FUNC_NAME "ragnarok-epoll-wait"
{
  scm_rag_epoll_event_set *es = NULL;
  struct epoll_event *tmp_set = NULL;
  int fd;
  int op;
  int count;
  long s = 0L;
  long ms = 0L;
  SCM cons;
  int nfds;
  SCM ret;
  SCM *prev = &ret;
  
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
  tmp_set =
    (struct epoll_event*)scm_gc_malloc(count*sizeof(struct epoll_event) ,"epoll_set");
  
  nfds = epoll_wait(es->epfd ,tmp_set ,es->size ,ms);

  if(0 > nfds)
    {
      RAG_ERROR1("epoll_wait" ,"epoll_wait error! nfds is %a~%" ,scm_from_int(nfds));
    }

  while(nfds)
    {
      fd = tmp_set[nfds].data.fd;
      op = tmp_set[nfds].events;
      cons = scm_cons(scm_from_int(fd) ,scm_from_int(op));
      *prev = scm_cons(cons ,SCM_EOL);
      prev = SCM_CDRLOC(*prev);
      nfds--;
    }

  return ret;
}
#undef FUNC_NAME
  
SCM scm_ragnarok_epoll_del_event(SCM meta_event ,SCM event_set)
#define FUNC_NAME "ragnarok-epoll-del-event"
{
  scm_rag_mevent *me = NULL;
  scm_rag_epoll_event *ee = NULL;
  scm_rag_epoll_event_set *ees = NULL;
  int fd;
  int ret = 0;
    
  //SCM_ASSERT_META_EVENT(meta_event);
  SCM_ASSERT_EPOLL_EVENT_SET(event_set);

  me = (scm_rag_mevent*)SMOB_DATA(meta_event);
  ees = (scm_rag_epoll_event_set*)SMOB_DATA(event_set);
  ee = (scm_rag_epoll_event*)me->core;
  fd = ee->data.fd;

  /* NOTE: I believe the fd_set deleting operation must be before than epoll_ctl DEL.
   *       In case it's been scheduled to do any epoll_wait.
   */
  rag_epoll_event_set_add_fd(ees ,fd);

  ret = epoll_ctl(ees->epfd ,EPOLL_CTL_DEL ,fd ,ee);

  if(0 > ret)
    {
      RAG_ERROR1("epoll_del" ,"epoll_del error! errno is %a~%" ,RAG_ERR2STR(errno));
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

  /* FIXME: I believe this 'append' operation is not the best solution.
   *	    It's redundant to create a new event_set then append read/write set.
   *	    Maybe I should write a more effective solution.
   */
  event_set = rag_epoll_set_append(read_set ,write_set);
  
  return scm_ragnarok_epoll_wait(event_set ,second ,msecond);
}
#undef FUNC_NAME

SCM scm_ragnarok_epoll_init(SCM size)
#define FUNC_NAME "ragnarok-epoll-init"
{
  int epfd = rag_epoll_create();
  SCM read_set = scm_make_epoll_event_set(size ,SCM_RAG_READ ,epfd);
  SCM write_set = scm_make_epoll_event_set(size ,SCM_RAG_WRITE ,epfd);
  SCM except_set = SCM_BOOL_F; // no except set for epoll
  
  return scm_values(scm_list_3(read_set ,write_set ,except_set));
}
#undef FUNC_NAME

// event_set 
SCM_RAG_OBJ_GETTER(epoll_event_set ,type ,type ,scm_from_int);
SCM_RAG_OBJ_SETTER(epoll_event_set ,type ,type ,scm_from_int ,scm_to_int);

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

  SCM_MAKE_GSUBR_OBJ_GET(epoll_event_set ,type);
  SCM_MAKE_GSUBR_OBJ_SET(epoll_event_set ,type);

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

