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

#ifndef __RAGNAROK_GENERIC_H__
#define __RAGNAROK_GENERIC_H__

#define TRUE 1
#define FALSE 0

#define READ  0
#define WRITE 1
#define EXCEPT 2

#define SCM_RAG_READ scm_from_int(READ)
#define SCM_RAG_WRITE scm_from_int(WRITE)
#define SCM_RAG_EXCEPT scm_from_int(EXCEPT)

#define RAG_SYMBOL(str) scm_string_to_symbol(scm_from_locale_string(str))
#define RAG_STRING(str) scm_from_locale_string(str)

#endif // End of __RAGNAROK_GENERIC_H__;
