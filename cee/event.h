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

#ifndef __RAGNAROK_EVENT_H__
#define __RAGNAROK_EVENT_H__

typedef enum Meta_Event_Type =
  { READ_FD = 0 ,WRITE_FD ,ERR_MSG,
  }rag_met;

typedef enum Meta_Event_Status =
  { WAIT = 0 ,BLOCK ,SLEEP ,DEAD,
  }rag_mes;

typedef struct Ragnarok_Meta_Event
{
  rag_met type;
  rag_mes status;
  void* core;
}scm_ragnarok_meta_event*;

extern scm_t_bits ragnarok_meta_event_tag;


#endif // End of __RAGNAROK_EVENT_H__;
