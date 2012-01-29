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

#ifndef __RAGNAROK_STRUCT_H__
#define __RAGNAROK_STRUCT_H__

/* NOTE: These macros are modified from guile-sdl.
 * Copyright (C) 2001 Joel Smith  <joels@mobyfoo.org>
 * Anyway, it's GPL.
 */

/* Here are some really kludgy macros to define standard getter/setter 
 * functions for various guile RAG objects.
 *
 *    obj    --  object type ie scm_sdl_OBJ.
 *    cm     --  C name for the structure member.
 *    gm     --  Name for structure member under guile.
 *    c_scm  --  Function used to convert C member type info SCM object.
 *    scm_c  --  Function used to convert SCM object into C type.
 *
 */
#define SCM_RAG_OBJ_GETTER( obj, cm, gm, c_scm ) \
        SCM_DEFINE( scm_rag_##obj##_get_##cm, "rag-" #obj ":" #gm, \
                    1, 0, 0, \
                    (SCM smob), \
                    "Returns the " #gm " value from a rag-" #obj " object.")\
        { \
                scm_rag_##obj *obj; \
                scm_assert_smob_type(rag_##obj##_tag,smob); \
                obj = (scm_rag_##obj *)SCM_SMOB_DATA( smob ); \
                return (c_scm( obj->cm )); \
        }
#define SCM_RAG_OBJ_SETTER( obj, cm, gm, c_scm, scm_c ) \
        SCM_DEFINE( scm_rag_##obj##_set_##cm, "rag-" #obj ":" #gm "-set!", \
                    2, 0, 0, \
                    (SCM smob, \
                     SCM value), \
                    "Sets the " #gm " value of a rag-" #obj " object.") \
        { \
                scm_rag_##obj *obj;                                  \
                scm_assert_smob_type(rag_##obj##_tag, smob);         \
                SCM_ASSERT( scm_is_integer( value ), smob, SCM_ARG2, \
                        s_scm_rag_##obj##_set_##cm ); \
                obj = (scm_rag_##obj *)SCM_SMOB_DATA( smob ); \
                obj->cm = scm_c( value ); \
                return (c_scm( obj->cm )); \
        }

#define SCM_MAKE_GSUBR_OBJ_GET( obj, x ) \
  scm_c_define_gsubr( s_scm_rag_##obj##_get_##x, 1, 0, 0, \
                        scm_rag_##obj##_get_##x )

#define SCM_MAKE_GSUBR_OBJ_SET( obj, x ) \
  scm_c_define_gsubr( s_scm_rag_##obj##_set_##x, 2, 0, 0, \
                        scm_rag_##obj##_set_##x )

#endif // End of __RAGNAROK_STRUCT_H__;
