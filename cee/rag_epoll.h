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

#ifndef __RAGNAROK_EPOLL_H__
#define __RAGNAROK_EPOLL_H__

#ifdef __HAS_SYS_EPOLL_H__

#define RAGNAROK_EVENT_ADD ragnarok_epoll_add_event
#define RAGNAROK_EVENT_DEL scm_ragnarok_select_del_event
#define RAGNAROK_EVENT_HANDLER ragnarok_epoll_handler
#define RAGNAROK_EVENT_MODULE_INIT rag_ragnarok_epoll_init




#endif // End of __HAS_SYS_EPOLL_H__;

#endif // End of __RAGNAROK_EPOLL_H__;


