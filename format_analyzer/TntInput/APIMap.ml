open StringFun
(*
  [return type]
  clean_ret
  taint_ret
  dst_ret
  src_ret
  src_ret_alloc
*)

let read () =
(* <cstring> ( <string.h> ) *)
(* Copying *)
M.empty 
|> M.add "calloc" {args = [src; skip]; ret = src_ret_alloc}
|> M.add "memcpy" {args = [dst; src; skip]; ret = dst_ret}
|> M.add "memmove" {args = [dst; src; skip]; ret = dst_ret}
|> M.add "strcpy" {args = [dst; src]; ret = dst_ret}
|> M.add "strncpy" {args = [dst; src; skip]; ret = dst_ret}
(* Concatenation *)
|> M.add "strcat" {args = [dst; src]; ret = dst_ret}
|> M.add "strncat" {args = [dst; src; skip]; ret = dst_ret}
(* Comparison *)
|> M.add "memcmp" {args = [src; src; skip]; ret = src_ret}
|> M.add "strcmp" {args = [src; src]; ret = src_ret}
|> M.add "strcoll" {args = [src; src]; ret = src_ret}
|> M.add "strncmp" {args = [src; src; skip]; ret = src_ret}
|> M.add "strxfrm" {args = [dst; src; skip]; ret = dst_ret}
(* Searching *)
|> M.add "memchr" {args = [src; skip; skip]; ret = src_ret}
|> M.add "memrchr" {args = [src; skip; skip]; ret = src_ret}
|> M.add "rawmemchr" {args = [src; skip]; ret = src_ret}
|> M.add "strchr" {args = [src; skip]; ret = src_ret}
|> M.add "strcspn" {args = [src; skip; skip]; ret = src_ret}
|> M.add "strpbrk" {args = [src; skip]; ret = src_ret}
|> M.add "strrchr" {args = [src; skip]; ret = src_ret}
|> M.add "strspn" {args = [src; skip]; ret = src_ret}
|> M.add "strstr" {args = [src; skip]; ret = src_ret}
|> M.add "strtok" {args = [src; skip]; ret = src_ret}
|> M.add "strtok_r" {args = [src; skip; skip]; ret = src_ret}
(* wchar.h *)
|> M.add "wcrtomb" {args = [dst; src; skip]; ret = clean_ret}
|> M.add "mbrtowc" {args = [dst; src; skip]; ret = clean_ret}
(* Other *)
|> M.add "memset" {args = [dst; src; skip]; ret = dst_ret}
(*|> M.add "strlen" {args = [src]; ret = src_ret}*)
|> M.add "strlen" {args = [src]; ret = clean_ret}
|> M.add "tolower" {args = [src]; ret = src_ret}
|> M.add "toupper" {args = [src]; ret = src_ret}
|> M.add "log" {args = [src]; ret = src_ret}
|> M.add "sin" {args = [src]; ret = src_ret}
|> M.add "tan" {args = [src]; ret = src_ret}
|> M.add "cos" {args = [src]; ret = src_ret}
|> M.add "acos" {args = [src]; ret = src_ret}
|> M.add "asin" {args = [src]; ret = src_ret}
|> M.add "atan" {args = [src]; ret = src_ret}
|> M.add "atan2" {args = [src]; ret = src_ret}
|> M.add "pow" {args = [src; src]; ret = src_ret}
|> M.add "sqrt" {args = [src]; ret = src_ret}
|> M.add "abs" {args = [src]; ret = src_ret}
|> M.add "fabs" {args = [src]; ret = src_ret}
|> M.add "ceil" {args = [src]; ret = src_ret}
|> M.add "floor" {args = [src]; ret = src_ret}
|> M.add "exp" {args = [src]; ret = src_ret}
|> M.add "expf" {args = [src]; ret = src_ret}
|> M.add "expl" {args = [src]; ret = src_ret}
|> M.add "cosh" {args = [src]; ret = src_ret}
|> M.add "coshf" {args = [src]; ret = src_ret}
|> M.add "coshl" {args = [src]; ret = src_ret}
|> M.add "sinh" {args = [src]; ret = src_ret}
|> M.add "sinhf" {args = [src]; ret = src_ret}
|> M.add "sinhl" {args = [src]; ret = src_ret}
|> M.add "log10" {args = [src]; ret = src_ret}
|> M.add "log10f" {args = [src]; ret = src_ret}
|> M.add "log10l" {args = [src]; ret = src_ret}
|> M.add "lgamma" {args = [src]; ret = src_ret}
|> M.add "lgammaf" {args = [src]; ret = src_ret}
|> M.add "lgammal" {args = [src]; ret = src_ret}
|> M.add "erf" {args = [src]; ret = src_ret}
|> M.add "erff" {args = [src]; ret = src_ret}
|> M.add "erfl" {args = [src]; ret = src_ret}
|> M.add "erfc" {args = [src]; ret = src_ret}
|> M.add "erfcf" {args = [src]; ret = src_ret}
|> M.add "erfcl" {args = [src]; ret = src_ret}
|> M.add "round" {args = [src]; ret = src_ret}
|> M.add "roundl" {args = [src]; ret = src_ret}
|> M.add "roundf" {args = [src]; ret = src_ret}
|> M.add "lroundl" {args = [src]; ret = src_ret}
|> M.add "lroundf" {args = [src]; ret = src_ret}
|> M.add "llround" {args = [src]; ret = src_ret}
|> M.add "fmod" {args = [src]; ret = src_ret}
|> M.add "fmodf" {args = [src]; ret = src_ret}
|> M.add "atoi" {args = [src]; ret = src_ret}
|> M.add "atof" {args = [src]; ret = src_ret}
|> M.add "atol" {args = [src]; ret = src_ret}
|> M.add "strtod" {args = [src; skip]; ret = src_ret}
|> M.add "strtol" {args = [src; skip; skip]; ret = src_ret}
|> M.add "strtoul" {args = [src; skip; skip]; ret = src_ret}
|> M.add "realloc" {args = [src; skip]; ret = src_ret_alloc} 
|> M.add "gettext" {args = [src]; ret = src_ret}
|> M.add "ngettext" {args = [src;skip;skip]; ret = src_ret}
|> M.add "dgettext" {args = [src]; ret = src_ret}
|> M.add "dcgettext" {args = [src]; ret = src_ret}
|> M.add "sprintf" {args = [src; dst_va]; ret = src_ret}
|> M.add "fmodl" {args = [src]; ret = src_ret}
|> M.add "htonl" {args = [src]; ret = src_ret}
|> M.add "htons" {args = [src]; ret = src_ret}
|> M.add "ntohl" {args = [src]; ret = src_ret}
|> M.add "ntohs" {args = [src]; ret = src_ret}
|> M.add "strdup" {args = [src]; ret = src_ret_alloc}
|> M.add "xstrdup" {args = [src]; ret = src_ret_alloc}
|> M.add "g_strdup" {args = [src]; ret = src_ret_alloc}  
|> M.add "strtoimax" {args = [src; skip; skip]; ret = src_ret}
|> M.add "strtoumax" {args = [src; skip; skip]; ret = src_ret}
|> M.add "argz_next" {args = [src; skip; skip]; ret = src_ret} 
|> M.add "system" {args = [src]; ret = src_ret}
|> M.add "frexp" {args = [src; skip]; ret = src_ret}
|> M.add "ldexp" {args = [src; skip]; ret = src_ret}
|> M.add "tgetnum" {args = [src]; ret = src_ret}
|> M.add "tgetstr" {args = [src; dst]; ret = src_ret}
|> M.add "memmem" {args = [src; skip; skip; skip]; ret = src_ret}
|> M.add "mmap" {args = [src; skip; skip]; ret = src_ret_alloc} 
|> M.add "xmlStrdup" {args = [src]; ret = src_ret_alloc}
|> M.add "g_strdup" {args = [src]; ret = src_ret_alloc}  
(* <cstdio> ( <stdio.h> ) *)
|> M.add "sscanf" {args = [src; skip; dst_va]; ret = src_ret}
|> M.add "readlink" {args = [src; dst; skip]; ret = src_ret}
|> M.add "tgoto" {args = [src; skip; skip]; ret = src_ret}
|> M.add "towlower" {args = [src]; ret = src_ret}
|> M.add "mktime" {args = [skip]; ret = src_ret}
|> M.add "localtime" {args = [skip]; ret = src_ret}
|> M.add "ctime" {args = [skip]; ret = src_ret}
|> M.add "gmtime" {args = [skip]; ret = src_ret}
|> M.add "timegm" {args = [skip]; ret = src_ret}

(* Clean  *)
(*|> if !Options.opt_unsound then (fun x-> x)
else if !Options.opt_sound || not (BatSet.is_empty !Options.opt_unsound_lib) then *)
|> (fun x -> x
 (* Taint *)
|> M.add "sscanf" {args = [src; skip; dst_va]; ret = taint_ret}
|> M.add "fgets" {args = [dst_ext; skip; skip]; ret = taint_ret}
|> M.add "_IO_getc" {args = [skip]; ret = taint_ret}
|> M.add "read" {args = [skip; dst_ext; skip]; ret = taint_ret}
|> M.add "fread" {args = [dst_ext; skip; skip; skip]; ret = taint_ret}
|> M.add "getchar" {args = []; ret = taint_ret}
|> M.add "getenv" {args = [skip]; ret = taint_ret}
|> M.add "readlink" {args = [skip; dst_ext; skip]; ret = taint_ret}
|> M.add "recv" {args = [skip; dst_ext; skip; skip]; ret = taint_ret}
(*|> M.add "__errno_location" {args = []; ret = clean_ret}*)
)
(*else (fun x -> x
(* clean (manual) *)
|> M.add "wcrtomb" {args = [dst; src; skip]; ret = clean_ret}
|> M.add "mbrtowc" {args = [dst; src; skip]; ret = clean_ret}
|> M.add "strerror" {args = [skip]; ret = clean_ret}
|> M.add "socket" {args = [skip; skip; skip]; ret = clean_ret}
|> M.add "access" {args = [skip; skip]; ret = clean_ret}
|> M.add "chown" {args = [skip; skip; skip]; ret = clean_ret}
|> M.add "uname" {args = [skip]; ret = clean_ret}
|> M.add "mkdir" {args = [skip; skip]; ret = clean_ret}
|> M.add "mkfifo" {args = [skip; skip]; ret = clean_ret}
|> M.add "setgroups" {args = [skip; skip]; ret = clean_ret}
|> M.add "seteuid" {args = [skip]; ret = clean_ret}
|> M.add "setegid" {args = [skip]; ret = clean_ret}
|> M.add "getgid" {args = []; ret = clean_ret}
|> M.add "getegid" {args = []; ret = clean_ret}
|> M.add "pipe" {args = [skip]; ret = clean_ret}
|> M.add "time" {args = [skip]; ret = clean_ret}
|> M.add "ctime" {args = [skip]; ret = clean_ret}
|> M.add "drand48" {args = []; ret = clean_ret}
|> M.add "rand" {args = []; ret = clean_ret}
|> M.add "cuserid" {args = []; ret = clean_ret}
|> M.add "getlogin" {args = []; ret = clean_ret}
|> M.add "getlogin_r" {args = [skip; skip]; ret = clean_ret}
|> M.add "getpid" {args = []; ret = clean_ret}
|> M.add "stat" {args = [skip; skip]; ret = clean_ret}
|> M.add "fstat" {args = [skip; skip]; ret = clean_ret}
|> M.add "lstat" {args = [skip; skip]; ret = clean_ret}
|> M.add "waitpid" {args = [skip; skip; skip]; ret = clean_ret}
|> M.add "getrlimit" {args = [skip; skip]; ret = clean_ret}
|> M.add "pthread_create" {args = [skip; skip; skip; skip]; ret = clean_ret}
|> M.add "pthread_getspecific" {args = [skip; skip]; ret = clean_ret}
|> M.add "re_match" {args = [skip; skip; skip; skip; skip]; ret = clean_ret}
|> M.add "re_search" {args = [skip; skip; skip; skip; skip]; ret = clean_ret}
|> M.add "setsockopt" {args = [skip; skip; skip; skip]; ret = clean_ret}
|> M.add "setlocale" {args = [skip; skip]; ret = clean_ret}
|> M.add "fopen" {args = [skip; skip]; ret = clean_ret}
|> M.add "ftell" {args = [skip]; ret = clean_ret}
|> M.add "pclose" {args = [skip]; ret = clean_ret}
|> M.add "write" {args = [skip; skip; skip]; ret = clean_ret}
|> M.add "nl_langinfo" {args = [skip]; ret = clean_ret}
|> M.add "random" {args = []; ret = clean_ret}
|> M.add "open" {args = [skip; skip; skip]; ret = clean_ret}
|> M.add "close" {args = [skip]; ret = clean_ret}
|> M.add "unlink" {args = [skip]; ret = clean_ret}
|> M.add "select" {args = [skip; skip; skip; skip; skip]; ret = clean_ret}
|> M.add "scanf" {args = [skip; dst_va_ext]; ret = clean_ret}
|> M.add "fork" {args = []; ret = clean_ret}
|> M.add "inet_ntoa" {args = [skip]; ret = clean_ret}
|> M.add "lseek" {args = [skip; skip; skip]; ret = clean_ret}
|> M.add "catgets" {args = [skip; skip]; ret = clean_ret}
|> M.add "dlerror" {args = []; ret = clean_ret}
|> M.add "GWEN_I18N_Translate" {args = [skip; skip]; ret = clean_ret}
|> M.add "tgetflag" {args = [skip]; ret = clean_ret}
|> M.add "wcwidth" {args = [skip]; ret = clean_ret}
|> M.add "png_get_image_width" {args = [skip; skip]; ret = clean_ret} 
)*)
