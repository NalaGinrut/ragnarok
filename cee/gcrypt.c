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

#include <stdlib.h>
#include <string.h>
#include <libguile.h>
#include <gcrypt.h>

#define MDA_BUF_LEN 512
#define MDA_RD_LEN 9

SCM scm_mmr_gcrypt_mda(SCM ori_str ,SCM algo)
#define FUNC_NAME "gcrypt:mda"
{
  SCM_ASSERT(scm_string_p(ori_str) ,ori_str ,SCM_ARG1 ,FUNC_NAME);
  SCM_ASSERT(scm_integer_p(algo) ,algo ,SCM_ARG2 ,FUNC_NAME);

  gcry_md_hd_t mda;
  gcry_error_t err;
  unsigned char *digest = NULL;
  char out_buf[MDA_BUF_LEN] = {0};
  char *buf_ptr = out_buf;
  int algorithm = scm_to_int(algo);
  int mda_len = gcry_md_get_algo_dlen(algorithm);
  int cnt = 0;
  char *str_buf = scm_to_locale_string(ori_str); 
  size_t str_len = strlen(str_buf);
  int i = 0;
    
  gcry_control(GCRYCTL_SUSPEND_SECMEM_WARN);
  gcry_control(GCRYCTL_INIT_SECMEM, 16384, 0);
  gcry_control(GCRYCTL_RESUME_SECMEM_WARN);

  err = gcry_md_open(&mda ,algorithm ,GCRY_MD_FLAG_SECURE);
  
  if(err)
    {
      printf("Something wrong when called gcry_md_open!\n");
      scm_primitive_exit(scm_from_int(1));
    }

  gcry_md_write(mda ,str_buf ,str_len); /*<-- this should create the checksum*/
  
  digest = gcry_md_read(mda, algorithm);
  
  for (; i<mda_len; i++, buf_ptr+=cnt)
    {
      cnt = snprintf(buf_ptr ,MDA_RD_LEN ,"%02X" ,digest[i]);
    }

  
  gcry_md_close(mda);

  buf_ptr = NULL;
  
  return scm_from_locale_stringn(out_buf ,strlen(out_buf));

}
#undef FUNC_NAME

