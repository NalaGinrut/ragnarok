#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT([HFG Ragnarok], 
	m4_esyscmd([build-aux/git-version-gen .tarball-version]),
	[nalaginrut@gmail.com])
AC_CONFIG_SRCDIR([bin/ragnarok.c])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CXX
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

AC_CONFIG_MACRO_DIR([m4])
# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h malloc.h stdlib.h string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE

# Checks for library functions.
AC_FUNC_FORK
AC_CHECK_FUNCS([memchr])

# Check for Guile
PKG_CHECK_MODULES([GUILE], [guile-2.0])

LT_INIT

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
