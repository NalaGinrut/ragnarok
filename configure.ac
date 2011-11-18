#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT([Ragnarok], 
	m4_esyscmd([build-aux/git-version-gen .tarball-version]),
	[nalaginrut@gmail.com])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR(RAGNAROK-VERSION)
AC_CONFIG_HEADERS([config.h])

. $srcdir/RAGNAROK-VERSION
RAGNAROK_VERSION="$PACKAGE_VERSION"

# Checks for programs.
AC_PROG_CXX
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h malloc.h stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_FORK
AC_CHECK_FUNCS([memchr mkdir])

# Check for Guile
PKG_CHECK_MODULES([GUILE], [guile-2.0])

GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

LT_INIT([dlopen disable-fast-install disable-static win32-dll])

RAGNAROK_WITH_LIBGCRYPT=0
AC_ARG_ENABLE([enable-libgcrypt],
  [AS_HELP_STRING([--enable-libgcrypt],
	          [Build with libgcrypt enabled (using ETAG must enable this)])],
  if test "$enable_libgcrypt" = y || test "$enable_libgcrypt" = yes; then
    RAGNAROK_WITH_LIBGCRYPT=1
  fi)

AC_CHECK_HEADER([sys/epoll.h],
	        [AC_DEFINE([__HAVE_SYS_EPOLL_H__], [], [Has epoll.])],
                [],
		[])

AC_CHECK_HEADER([sys/kqueue.h],
	        [AC_DEFINE([__HAVE_SYS_KQUEUE_H__], [], [Has kqueue.])],
                [],
		[])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
