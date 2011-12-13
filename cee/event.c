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
#include <string.h>
#include <errno.h>
#include "event.h"
#include "rag_struct.h"

#ifdef __HAS_SYS_EPOLL_H__
#include "rag_epoll.h"
#endif // End of __HAS_SYS_EPOLL_H__;

#ifdef __HAS_SYS_KQUEUE_H__
#include "rag_kqueue.h"
#endif // End of __HAS_SYS_KQUEUE_H__;

#ifndef __HAS_SYS_EPOLL_H__
#ifndef __HAS_SYS_KQUEUE_H__
#include "rag_select.h"
#endif // End of no __HAS_SYS_KQUEUE_H__;
#endif // End of no __HAS_SYS_EPOLL_H__;

#ifdef __cplusplus
extern "C" {
#endif

scm_t_bits rag_mevent_tag;

const char const *ragnarok_meta_type_string[] =
  {
    "READ_FD" ,"WRITE_FD" ,"ERR_MSG" ,"UNKNOWN"
  };

const char const *ragnarok_meta_status_string[] =
  {
    "WAIT" ,"BLOCK" ,"SLEEP" ,"DEAD" ,"READY" ,"CLEAR"
  };
  
static inline SCM scm_rag_meta_event2scm(ragnarok_meta_event event)
{
  SCM_RETURN_NEWSMOB(rag_mevent_tag ,event);
}

SCM ragnarok_make_meta_event(SCM type ,SCM status ,SCM core)
#define FUNC_NAME "ragnarok-make-meta-event"
{
  int t;
  int s;
  void *c = NULL;
  ragnarok_meta_event ret = NULL;
  
  SCM_VALIDATE_INT(1 ,type);
  SCM_VALIDATE_INT(2 ,status);

  t = scm_to_int(type);
  s = scm_to_int(status);

  switch(t)
    {
    case READ_FD:
    case WRITE_FD:
      SCM_VALIDATE_INT(3 ,core);
      c = (int*)scm_gc_malloc(sizeof(int) ,"event-fd");
      *(int*)c = scm_to_int(core);
      break;
    case ERR_MSG:
      SCM_VALIDATE_STRING(3 ,core);
      c = (SCM*)core;
      break;
    default:
      return SCM_BOOL_F;
	/* NOTE: Return #f if encounted invalid type.
	 *       Throw exception in Scheme code ,not in Cee code.
	 */
    }

  ret = (struct Ragnarok_Meta_Event*)
    scm_gc_malloc(sizeof(struct Ragnarok_Meta_Event) ,"meta-event");

  ret->type = t;
  ret->status = s;
  ret->core = (SCM*)c;

  return scm_rag_meta_event2scm(ret);
}
#undef FUNC_NAME

SCM ragnarok_meta_event_p(SCM event)
#define FUNC_NAME "ragnarok-meta-event?"
{
  return
    SCM_SMOB_PREDICATE(rag_mevent_tag ,event) ?
    SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

int ragnarok_print_meta_event(SCM me_smob ,SCM port ,scm_print_state *pstate)
{
  ragnarok_meta_event me = (ragnarok_meta_event)SCM_SMOB_DATA(me_smob);

  scm_puts("#<rag-meta-event 0x" ,port);
  scm_intprint((long)me ,16 ,port);
  scm_puts(" -" ,port);

  scm_puts(" type: " ,port);
  scm_puts(RAG_ME_GET_TYPE(me) ,port);

  scm_puts(" status: " ,port);
  scm_puts(RAG_ME_GET_STATUS(me) ,port);

  scm_puts(" core: " ,port);
  RAG_ME_PRN_CORE(me ,port);
	   
  scm_puts(">" ,port);

  return 1;
}

SCM ragnarok_clear_meta_event(SCM me_smob)
#define FUNC_NAME "ragnarok-clear-meta-event"
{
  ragnarok_meta_event me = (ragnarok_meta_event)SCM_SMOB_DATA(me_smob);

  me->type = UNKNOWN;
  me->status = CLEAR;
  me->core = NULL;

  return scm_rag_meta_event2scm(me);
}
#undef FUNC_NAME
  
scm_sizet ragnarok_free_meta_event(SCM me_smob)
{
  ragnarok_meta_event me = (ragnarok_meta_event)SCM_SMOB_DATA(me_smob);

  // NOTE: The second para 'size' is always ignored in Guile 2.x.
  scm_gc_free(me ,0 ,"meta-event");

  return 0;	
}

SCM_RAG_OBJ_GETTER(mevent ,type ,type ,scm_from_int);
SCM_RAG_OBJ_SETTER(mevent ,type ,type ,scm_from_int ,scm_to_int);

SCM_RAG_OBJ_GETTER(mevent ,status ,status ,scm_from_int);
SCM_RAG_OBJ_SETTER(mevent ,status ,status ,scm_from_int ,scm_to_int);

SCM_RAG_OBJ_GETTER(mevent ,core ,core ,PTR2SCM);
SCM_RAG_OBJ_SETTER(mevent ,core ,core ,PTR2SCM ,SCM2PTR);

void init_meta_event_type()
{
  // meta event SMOB functions define
  rag_mevent_tag = scm_make_smob_type("ragnarok-meta-event-type",
				      sizeof(struct Ragnarok_Meta_Event));
  scm_set_smob_print(rag_mevent_tag ,ragnarok_print_meta_event);
  scm_set_smob_free(rag_mevent_tag ,ragnarok_free_meta_event);

  // meta event handler define
  scm_c_define_gsubr("ragnarok-clear-meta-event" ,1 ,0 ,0 ,ragnarok_clear_meta_event);
  scm_c_define_gsubr("ragnarok-make-meta-event" ,3 ,0 ,0 ,ragnarok_make_meta_event);
  scm_c_define_gsubr("ragnarok-meta-event?" ,1 ,0 ,0 ,ragnarok_meta_event_p);

  SCM_MAKE_GSUBR_OBJ_GET(mevent ,type);
  SCM_MAKE_GSUBR_OBJ_SET(mevent ,type);
  
  SCM_MAKE_GSUBR_OBJ_GET(mevent ,status);
  SCM_MAKE_GSUBR_OBJ_SET(mevent ,status);

  SCM_MAKE_GSUBR_OBJ_GET(mevent ,core);
  SCM_MAKE_GSUBR_OBJ_SET(mevent ,core);
}
  
SCM scm_ragnarok_event_handler(SCM event ,SCM event_set ,SCM second ,SCM msecond)
#define FUNC_NAME "ragnarok-event-handler"
{
  /* NOTE: timeval is variable while type is different.
   *  SLEEP -> delay time;
   *  READY -> timeout;
   *  BLOCK -> timeout;
   */
  return RAGNAROK_EVENT_HANDLER(event ,event_set ,second ,msecond);
}
#undef FUNC_NAME
  
SCM scm_ragnarok_event_add(SCM event ,SCM event_set)
#define FUNC_NAME "ragnarok-event-add"
{
  return RAGNAROK_EVENT_ADD(event ,event_set);
}
#undef FUNC_NAME

SCM scm_ragnarok_event_del(SCM event ,SCM event_set)
#define FUNC_NAME "ragnarok-event-del"
{
  return RAGNAROK_EVENT_DEL(event ,event_set);
}
#undef FUNC_NAME

SCM scm_ragnarok_event_init(SCM init_arg)
#define FUNC_NAME "ragnarok-event-init"
{
  return RAGNAROK_EVENT_INIT(init_arg);
}
#undef FUNC_NAME
  
void init_event_module()
{
  RAGNAROK_EVENT_MODULE_INIT();

  init_meta_event_type();

  scm_c_define_gsubr("ragnarok-event-init" ,0 ,1 ,0 ,scm_ragnarok_event_init);
  scm_c_define_gsubr("ragnarok-event-handler" ,2 ,2 ,0 ,scm_ragnarok_event_handler);
  scm_c_define_gsubr("ragnarok-event-add" ,2 ,0 ,0 ,scm_ragnarok_event_add);
  scm_c_define_gsubr("ragnarok-event-del" ,2 ,0 ,0 ,scm_ragnarok_event_del);
}
  
#ifdef __cplusplus
}
#endif
