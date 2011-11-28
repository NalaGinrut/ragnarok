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

#ifndef __HAS_SYS_EPOLL_H__ && __HAS_SYS_KQUEUE_H__
#include "rag_select.h"
#endif // End of __HAS_SYS_EPOLL_H__ && __HAS_SYS_KQUEUE_H__;

#ifdef __cplusplus
extern "C" {
#endif

scm_t_bits ragnarok_meta_event_tag;

static inline SCM scm_rag_meta_event2scm(scm_ragnarok_meta_event* event)
{
  SCM_RETURN_NEWSMOB(ragnarok_meta_event_tag ,event);
}

SCM ragnarok_make_meta_event(SCM type ,SCM status ,SCM core)
#define FUNC_NAME "ragnarok-make-meta-event"
{
  int t;
  int s;
  void* ret = NULL;
  
  SCM_VALIDATE_INUM(1 ,type);
  SCM_VALIDATE_INUM(2 ,status);

  t = scm_to_int(type);
  s = scm_to_int(status);

  switch(type)
    {
    READ_FD:
    WRITE_FD:
      {
	int *c = NULL;
	SCM_VALIDATE_INUM(3 ,core);
	c = (int*)scm_gc_malloc(sizeof(int) ,"event-fd");
	*c = scm_to_int(core);
	break;
      }
    ERR_MSG:
      {
	char *c = NULL;
	char *tmp = NULL;
	SCM_VALIDATE_STRING(3 ,core);
	tmp = scm_to_locale_string(core);
	c = (char*)scm_gc_malloc(strlen(tmp)+1);
      }
    default:
      {
	return SCM_BOOL_F;
	/* NOTE: Return #f if encounted invalid type.
	 *       Throw exception in Scheme code ,not in Cee code.
	 */
      }
    }

  ret =
    (struct Ragnarok_Make_Meta_Event*)
    scm_gc_malloc(sizeof(struct Ragnarok_Make_Meta_Event) ,"meta-event");

  ret->type = t;
  ret->status = s;
  ret->core = (void*)c;

  return scm_rag_meta_event2scm(ret);
}
#undef FUNC_NAME

SCM ragnarok_meta_event_p(SCM event)
#define FUNC_NAME "ragnarok-meta-event?"
{
  return
    SCM_SMOB_PREDICATE(ragnarok_meta_event_tag ,event) ?
    SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_RAG_OBJ_GETTER(mevent ,type ,type ,scm_to_int);
SCM_RAG_OBJ_SETTER(mevent ,type ,type ,scm_from_int ,scm_to_int);

SCM_RAG_OBJ_GETTER(mevent ,status ,status ,scm_to_int);
SCM_RAG_OBJ_SETTER(mevent ,status ,status ,scm_from_int ,scm_to_int);

SCM_RAG_OBJ_GETTER(mevent ,core ,core ,PTR2SCM);
SCM_RAG_OBJ_SETTER(mevent ,core ,core ,PTR2SCM ,SCM2PTR);

void init_meta_event_type()
{
  ragnarok_meta_event_tag = scm_make_smob_type("ragnarok-meta-event-type",
					       sizeof(struct Ragnarok_Meta_Event));
  
  scm_set_smob_print(ragnarok_meta_event_tag ,ragnarok_print_meta_event);

  scm_c_define_gsubr("ragnarok-clear-event" ,1 ,0 ,0 ,ragnarok_clear_event);
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
  
void init_event_module()
{
  RAGNAROK_EVENT_MODULE_INIT();

  init_meta_event_type();
  
  scm_c_define_gsubr("ragnarok-event-handler" ,2 ,2 ,0 ,scm_ragnarok_event_handler);
  scm_c_define_gsubr("ragnarok-event-add" ,2 ,0 ,0 ,scm_ragnarok_event_add);
  scm_c_define_gsubr("ragnarok-event-del" ,2 ,0 ,0 ,scm_ragnarok_event_del);
}
  
#ifdef __cplusplus
}
#endif
