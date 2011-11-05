#include <libguile.h>
#include <string.h>
#include <malloc.h>

SCM scm_mmr_test(SCM target)
#define FUNC_NAME "mmr-test"
{
  char *buf = NULL;//scm_to_locale_string(target);
  //char *me = (char*)malloc(50);
  char *ptr = NULL;
  SCM ret;

  SCM_VALIDATE_STRING(1 ,target);
  //memset(me ,0 ,50);
  buf = scm_to_locale_string(target);
  ptr = buf+2;
  ret = scm_from_locale_string(ptr);
  //free(me);
  //me = NULL;
  //ptr = NULL;

  return ret;
}
#undef FUNC_NAME
