#include "thispathdefn.h"


#define R_THIS_PATH_INITIALIZE_SYMBOLS
#include "symbols.h"


SEXP mynamespace = NULL,
     DocumentContextClass = NULL,
     ThisPathInAQUAErrorClass                      = NULL,
     ThisPathInZipFileErrorClass                   = NULL,
     ThisPathNotExistsErrorClass                   = NULL,
     ThisPathNotFoundErrorClass                    = NULL,
     ThisPathNotImplementedErrorClass              = NULL,
     ThisPathUnrecognizedConnectionClassErrorClass = NULL,
     ThisPathUnrecognizedMannerErrorClass          = NULL,
     last_condition = NULL,
     _custom_gui_path_character_environment = NULL,
     _custom_gui_path_function_environment  = NULL,
     makePROMISE_environment = NULL;


#if defined(R_THIS_PATH_NEED_BLANKSCALARSTRING)
SEXP R_BlankScalarString = NULL;
#endif

     /* commandArgs() */
SEXP expr_commandArgs                               = NULL,
     /* invisible() */
     expr_invisible                                 = NULL,
     /* parent.frame() */
     expr_parent_frame                              = NULL,
     /* sys.call() */
     expr_sys_call                                  = NULL,
     /* sys.call(which) */
     expr_sys_call_which                            = NULL,
     /* sys.function(which) */
     expr_sys_function_which                        = NULL,
     /* evalq(sys.function()) */
     eval_op                                        = NULL,
     /* sys.nframe() */
     expr_sys_nframe                                = NULL,
     /* sys.parents() */
     expr_sys_parents                               = NULL,
     /* missing(file) */
     expr_missing_file                              = NULL,
     /* missing(input) */
     expr_missing_input                             = NULL,
     /* missing(ofile) */
     expr_missing_ofile                             = NULL,
     /* info$source_path */
     expr_info_dollar_source_path                   = NULL,
     /* knitr::opts_knit[["get"]]("output.dir") */
     expr_knitr_output_dir                          = NULL,
     /* as.numeric_version(getNamespaceVersion("testthat")) >= "3.1.2" */
     expr_testthat_source_file_uses_brio_read_lines = NULL,
     /* .toplevel.nframe() */
     expr__toplevel_nframe                          = NULL,
     /* .isMethodsDispatchOn() */
     expr__isMethodsDispatchOn                      = NULL,
     /* UseMethod("lengths") */
     expr_UseMethod_lengths                         = NULL,
     /* delayedAssign("x", NULL, R_EmptyEnv, makePROMISE_environment) */
     expr_makePROMISE                               = NULL,
     /* getwd() */
     expr_getwd                                     = NULL;


LibExtern Rboolean mbcslocale;
// LibExtern Rboolean utf8locale;
// LibExtern Rboolean latin1locale;


SEXP do_mbcslocale do_formals
{
    do_start_no_call_op_rho("mbcslocale", 0);
    return Rf_ScalarLogical(mbcslocale);
}


// SEXP do_utf8locale do_formals
// {
//     do_start("utf8locale", 0);
//     return Rf_ScalarLogical(utf8locale);
// }


// SEXP do_latin1locale do_formals
// {
//     do_start("latin1locale", 0);
//     return Rf_ScalarLogical(latin1locale);
// }


#if R_version_at_least(4,2,0)
LibExtern int R_MB_CUR_MAX;
#endif
SEXP do_R_MB_CUR_MAX do_formals
{
    do_start_no_call_op_rho("R_MB_CUR_MAX", 0);
#if R_version_at_least(4,2,0)
    return Rf_ScalarInteger(R_MB_CUR_MAX);
#else
    return Rf_ScalarInteger(MB_CUR_MAX);
#endif
}


#if defined(R_CONNECTIONS_VERSION_1)
Rconnection (*ptr_R_GetConnection)(SEXP sConn);
#endif
#if defined(HAVE_SET_R_VISIBLE)
void (*ptr_set_R_Visible)(Rboolean x);
#endif
#if defined(HAVE_GET_UTF8LOCALE)
Rboolean (*ptr_get_utf8locale)(void);
#endif
Rboolean (*ptr_get_latin1locale)(void);
#if defined(NEED_R_4_5_0_FUNCTIONS)
SEXP (*ptr_PRCODE)(SEXP x);
SEXP (*ptr_PRENV)(SEXP x);
SEXP (*ptr_R_PromiseExpr)(SEXP x);
SEXP (*ptr_PRVALUE)(SEXP x);
void (*ptr_SET_PRCODE)(SEXP x, SEXP v);
void (*ptr_SET_PRENV)(SEXP x, SEXP v);
void (*ptr_SET_PRVALUE)(SEXP x, SEXP v);
#endif


#if defined(R_CONNECTIONS_VERSION_1)
    #if defined(R_THIS_PATH_DEVEL)
        #if R_version_less_than(3,3,0)
Rconnection R_GetConnection(SEXP sConn)
{
    if (!Rf_inherits(sConn, "connection")) Rf_error(_("invalid connection"));
    return getConnection(Rf_asInteger(sConn));
}
        #endif
    #endif
#endif


#if defined(HAVE_GET_UTF8LOCALE)
Rboolean ptr_get_utf8locale_default(void)
{
    int value = Rf_asLogical(getFromMyNS(_utf8localeSymbol));
    if (value == NA_INTEGER)
        Rf_error(_("missing value where TRUE/FALSE needed"));
    return value ? TRUE : FALSE;
}
#endif
Rboolean ptr_get_latin1locale_default(void)
{
    int value = Rf_asLogical(getFromMyNS(_latin1localeSymbol));
    if (value == NA_INTEGER)
        Rf_error(_("missing value where TRUE/FALSE needed"));
    return value ? TRUE : FALSE;
}


#if defined(NEED_R_4_5_0_FUNCTIONS)
/* create default values for the function pointers
 *
 * this works exclusively because listsxp_struct and promsxp_struct have the
 * same structure and so the promise members can be get/set using the list
 * member getters/setters
 *
 * struct listsxp_struct {
 *     struct SEXPREC *carval;
 *     struct SEXPREC *cdrval;
 *     struct SEXPREC *tagval;
 * };
 *
 * struct promsxp_struct {
 *     struct SEXPREC *value;
 *     struct SEXPREC *expr;
 *     struct SEXPREC *env;
 * };
 *
 * carval (CAR/SETCAR ) <--> value (PRVALUE/SET_PRVALUE)
 * cdrval (CDR/SETCDR ) <--> expr  (PRCODE /SET_PRCODE )
 * tagval (TAG/SET_TAG) <--> env   (PRENV  /SET_PRENV  )
 */
SEXP ptr_PRCODE_default(SEXP x)
{
    return CDR(x);
}
SEXP ptr_PRENV_default(SEXP x)
{
    return TAG(x);
}
SEXP ptr_R_PromiseExpr_default(SEXP x)
{
    return R_BytecodeExpr(CDR(x));
}
SEXP ptr_PRVALUE_default(SEXP x)
{
    return CAR(x);
}
void ptr_SET_PRCODE_default(SEXP x, SEXP v)
{
    SETCDR(x, v);
    return;
}
void ptr_SET_PRENV_default(SEXP x, SEXP v)
{
    SET_TAG(x, v);
    return;
}
void ptr_SET_PRVALUE_default(SEXP x, SEXP v)
{
    SETCAR(x, v);
    return;
}
#endif


#if !defined(R_THIS_PATH_DEVEL)
#include <R_ext/Rdynload.h>
SEXP do_get_ptrs do_formals
{
    do_start_no_call_op_rho("get_ptrs", 0);
#if defined(R_CONNECTIONS_VERSION_1)
    ptr_R_GetConnection = (Rconnection(*)(SEXP))
        R_GetCCallable("this_path_reg_ptrs", "R_GetConnection");
#endif
#if defined(HAVE_SET_R_VISIBLE)
    ptr_set_R_Visible = (void(*)(Rboolean))
        R_GetCCallable("this_path_reg_ptrs", "set_R_Visible");
#endif
#if defined(HAVE_GET_UTF8LOCALE)
    ptr_get_utf8locale = (Rboolean(*)(void))
        R_GetCCallable("this_path_reg_ptrs", "get_utf8locale");
#endif
#if defined(NEED_R_4_5_0_FUNCTIONS)
    ptr_PRCODE = (SEXP(*)(SEXP))
        R_GetCCallable("this_path_reg_ptrs", "PRCODE");
    ptr_PRENV = (SEXP(*)(SEXP))
        R_GetCCallable("this_path_reg_ptrs", "PRENV");
    ptr_R_PromiseExpr = (SEXP(*)(SEXP))
        R_GetCCallable("this_path_reg_ptrs", "R_PromiseExpr");
    ptr_PRVALUE = (SEXP(*)(SEXP))
        R_GetCCallable("this_path_reg_ptrs", "PRVALUE");
    ptr_SET_PRCODE = (void(*)(SEXP,SEXP))
        R_GetCCallable("this_path_reg_ptrs", "SET_PRCODE");
    ptr_SET_PRENV = (void(*)(SEXP,SEXP))
        R_GetCCallable("this_path_reg_ptrs", "SET_PRENV");
    ptr_SET_PRVALUE = (void(*)(SEXP,SEXP))
        R_GetCCallable("this_path_reg_ptrs", "SET_PRVALUE");
#endif
    return R_NilValue;
}
#endif


SEXP do_onLoad do_formals
{
    do_start_no_call_op_rho("onLoad", 2);


    /* these arguments are passed from .onLoad() */
    // SEXP libname = CAR(args);   // warning: unused variable 'libname'
    // SEXP pkgname = CADR(args);  // warning: unused variable 'pkgname'


    static int been_here_before = 0;
    if (been_here_before)
        Rf_error("cannot call 'onLoad' more than once (wtf are you doing\?\?\?)");
    been_here_before = 1;


#if defined(R_VERSION)
    {
        SEXP expr = Rf_lcons(Rf_install("getRversion"), R_NilValue);
        Rf_protect(expr);
        SEXP v = Rf_eval(expr, R_BaseEnv);
        Rf_protect(v);
        if (TYPEOF(v) == VECSXP && XLENGTH(v) == 1) {
            v = VECTOR_ELT(v, 0);
            if (TYPEOF(v) == INTSXP && XLENGTH(v) == 3) {
                int *iv = INTEGER(v);
                if (iv[0] == atoi(R_MAJOR) &&
                    iv[1] == atoi(R_MINOR));
                else Rf_warningcall_immediate(R_NilValue,
                    "package 'this.path' was built under R version %s.%s\n but is being loaded in R %d.%d.%d",
                                                                          R_MAJOR, R_MINOR,                iv[0], iv[1], iv[2]);
            }
        }
        Rf_unprotect(2);
    }
#endif


#define R_THIS_PATH_DEFINE_SYMBOLS
#include "symbols.h"


    /* get my namespace from the namespace registry */
    mynamespace = Rf_findVarInFrame(R_NamespaceRegistry, Rf_install("this.path"));
    if (TYPEOF(mynamespace) != ENVSXP)
        Rf_error(_("not an environment"));
    R_PreserveObject(mynamespace);


    INCREMENT_NAMED_defineVar(Rf_install(".mynamespace"), mynamespace, mynamespace);


#define LockCLOENV(symbol, bindings)                           \
    do {                                                       \
        SEXP sym = (symbol);                                   \
        SEXP tmp = getFromMyNS(sym);                           \
        if (TYPEOF(tmp) != CLOSXP)                             \
            Rf_error(_("object '%s' of mode '%s' was not found"), EncodeChar(PRINTNAME(sym)), "function");\
        R_LockEnvironment(CLOENV(tmp), (bindings));            \
    } while (0)


#if defined(HAVE_GET_UTF8LOCALE)
    ptr_get_utf8locale = ptr_get_utf8locale_default;
#endif
    ptr_get_latin1locale = ptr_get_latin1locale_default;


#if defined(NEED_R_4_5_0_FUNCTIONS)
    ptr_PRCODE = ptr_PRCODE_default;
    ptr_PRENV = ptr_PRENV_default;
    ptr_R_PromiseExpr = ptr_R_PromiseExpr_default;
    ptr_PRVALUE = ptr_PRVALUE_default;
    ptr_SET_PRCODE = ptr_SET_PRCODE_default;
    ptr_SET_PRENV = ptr_SET_PRENV_default;
    ptr_SET_PRVALUE = ptr_SET_PRVALUE_default;
#endif


#if defined(R_THIS_PATH_DEVEL)
    #if defined(R_CONNECTIONS_VERSION_1)
    ptr_R_GetConnection = R_GetConnection;
    #endif
#else
    {
        SEXP expr = Rf_lcons(Rf_install(".get_ptrs"), R_NilValue);
        Rf_protect(expr);
        Rf_eval(expr, mynamespace);
        Rf_unprotect(1);
        R_removeVarFromFrame(Rf_install(".get_ptrs"), mynamespace);
        R_removeVarFromFrame(Rf_install(".C_get_ptrs"), mynamespace);
        LockCLOENV(Rf_install(".maybe_dyn_unload"), TRUE);
    }
#endif


#define make_STRSXP_from_char_array(var, ...)                  \
    do {                                                       \
        const char *Class[] = __VA_ARGS__;                     \
        int nClass = 0;                                        \
        while (Class[nClass]) ++nClass;                        \
        var = Rf_allocVector(STRSXP, nClass);                  \
        R_PreserveObject(var);                                 \
        for (int i = 0; i < nClass; i++)                       \
            SET_STRING_ELT(var, i, Rf_mkChar(Class[i]));       \
        MARK_NOT_MUTABLE(var);                                 \
    } while (0)


    make_STRSXP_from_char_array(
        DocumentContextClass,
        { "ThisPathDocumentContext", "environment", NULL }
    );


/* this code is written this way on purpose, do not reformat */
#define NotImplementedErrorClass_string                        \
    "NotImplementedError"
#define ThisPathInAQUAErrorClass_string                        \
    "ThisPathInAQUAError"
#define ThisPathInZipFileErrorClass_string                     \
    "ThisPathInZipFileError"
#define ThisPathNotExistsErrorClass_string                     \
    "ThisPathNotExistsError"
#define ThisPathNotFoundErrorClass_string                      \
    "ThisPathNotFoundError"
#define ThisPathNotImplementedErrorClass_string                \
    "ThisPathNotImplementedError"
#define ThisPathUnrecognizedConnectionClassErrorClass_string   \
    "ThisPathUnrecognizedConnectionClassError"
#define ThisPathUnrecognizedMannerErrorClass_string            \
    "ThisPathUnrecognizedMannerError"


/* new names of the error classes along with the old names */
#define NotImplementedErrorClass_strings                       \
    NotImplementedErrorClass_string,                           \
    "notImplementedError"
#define ThisPathInAQUAErrorClass_strings                       \
    ThisPathInAQUAErrorClass_string,                           \
    "this.path::thisPathInAQUAError"
#define ThisPathInZipFileErrorClass_strings                    \
    ThisPathInZipFileErrorClass_string,                        \
    "this.path::thisPathInZipFileError"
#define ThisPathNotExistsErrorClass_strings                    \
    ThisPathNotExistsErrorClass_string,                        \
    "thisPathNotExistsError",                                  \
    "this.path::thisPathNotExistsError",                       \
    "this.path::thisPathNotExistError",                        \
    "this.path_this.path_not_exists_error"
#define ThisPathNotFoundErrorClass_strings                     \
    ThisPathNotFoundErrorClass_string,                         \
    "thisPathNotFoundError",                                   \
    "this.path::thisPathNotFoundError"
#define ThisPathNotImplementedErrorClass_strings               \
    ThisPathNotImplementedErrorClass_string,                   \
    "this.path::thisPathNotImplementedError",                  \
    "this.path_this.path_unimplemented_error"
#define ThisPathUnrecognizedConnectionClassErrorClass_strings  \
    ThisPathUnrecognizedConnectionClassErrorClass_string,      \
    "this.path::thisPathUnrecognizedConnectionClassError"
#define ThisPathUnrecognizedMannerErrorClass_strings           \
    ThisPathUnrecognizedMannerErrorClass_string,               \
    "this.path::thisPathUnrecognizedMannerError"


#define ErrorClass_strings                                     \
    "error", "condition", NULL


    make_STRSXP_from_char_array(
        ThisPathInAQUAErrorClass,
        {
            ThisPathInAQUAErrorClass_strings,
            ThisPathNotFoundErrorClass_strings,
            ThisPathNotImplementedErrorClass_strings,
            NotImplementedErrorClass_strings,
            ErrorClass_strings
        }
    );
    make_STRSXP_from_char_array(
        ThisPathInZipFileErrorClass,
        {
            ThisPathInZipFileErrorClass_strings,
            ThisPathNotFoundErrorClass_strings,
            ErrorClass_strings
        }
    );
    make_STRSXP_from_char_array(
        ThisPathNotExistsErrorClass,
        {
            ThisPathNotExistsErrorClass_strings,
            ThisPathNotFoundErrorClass_strings,
            ErrorClass_strings
        }
    );
    make_STRSXP_from_char_array(
        ThisPathNotFoundErrorClass,
        {
            ThisPathNotFoundErrorClass_strings,
            ErrorClass_strings
        }
    );
    make_STRSXP_from_char_array(
        ThisPathNotImplementedErrorClass,
        {
            ThisPathNotImplementedErrorClass_strings,
            NotImplementedErrorClass_strings,
            ErrorClass_strings
        }
    );
    make_STRSXP_from_char_array(
        ThisPathUnrecognizedConnectionClassErrorClass,
        {
            ThisPathUnrecognizedConnectionClassErrorClass_strings,
            ThisPathNotFoundErrorClass_strings,
            ErrorClass_strings
        }
    );
    make_STRSXP_from_char_array(
        ThisPathUnrecognizedMannerErrorClass,
        {
            ThisPathUnrecognizedMannerErrorClass_strings,
            ThisPathNotFoundErrorClass_strings,
            ErrorClass_strings
        }
    );


    /* it might seem more intuitive to say
     * last_condition = R_NilValue;
     *
     * but that means every time last_condition gets updated,
     * we must release the old SEXP and preserve the new one
     *
     * this is preferable because we only preserve and release one object
     */
    last_condition = Rf_cons(R_NilValue, R_NilValue);
    R_PreserveObject(last_condition);


    makePROMISE_environment =
        R_NewEnv(/* enclos */ R_EmptyEnv, /* hash */ TRUE, /* size */ 2);
    R_PreserveObject(makePROMISE_environment);
    Rf_defineVar(xSymbol, R_NilValue, makePROMISE_environment);
    R_LockEnvironment(makePROMISE_environment, FALSE);


#if defined(R_THIS_PATH_NEED_BLANKSCALARSTRING)
    R_BlankScalarString = Rf_ScalarString(R_BlankString);
    R_PreserveObject(R_BlankScalarString);
#endif


    /* rprojroot.R */
    LockCLOENV(Rf_install(".find_root"), TRUE);
    LockCLOENV(Rf_install(".proj"), FALSE);
    /* startup.R */
    LockCLOENV(_site_fileSymbol, TRUE);
    LockCLOENV(Rf_install(".in_site_file"), FALSE);
    LockCLOENV(_init_fileSymbol, TRUE);
    /* thispath.R */
    LockCLOENV(_shFILESymbol, TRUE);
    LockCLOENV(Rf_install(".vscode_path"), TRUE);
    LockCLOENV(_jupyter_pathSymbol, TRUE);
    LockCLOENV(Rf_install(".emacs_path"), TRUE);
    /* zzz.R */
    // LockCLOENV(Rf_install(".eval_with_message"), FALSE);


    {
        SEXP sym = Rf_install(".startup_info");
        Rboolean bindings = TRUE;
        SEXP tmp = getFromMyNS(sym);
        if (TYPEOF(tmp) != ENVSXP)
            Rf_error(_("object '%s' of mode '%s' was not found"), EncodeChar(PRINTNAME(sym)), "environment");
        R_LockEnvironment(tmp, bindings);
    }


    /* force the promise 'initwd' */
    getFromMyNS(Rf_install("initwd"));


    /* save HAVE_AQUA, PATH_MAX, and NAMEDMAX in my namespace */
#if defined(HAVE_AQUA)
    INCREMENT_NAMED_defineVar(Rf_install(".HAVE_AQUA"), R_TrueValue, mynamespace);
#else
    INCREMENT_NAMED_defineVar(Rf_install(".HAVE_AQUA"), R_FalseValue, mynamespace);
#endif


    INCREMENT_NAMED_defineVar(Rf_install(".PATH_MAX"), Rf_protect(Rf_ScalarInteger(PATH_MAX)), mynamespace);
    Rf_unprotect(1);


#if R_version_less_than(3,0,0)
    INCREMENT_NAMED_defineVar(Rf_install(".NAMEDMAX"), Rf_protect(Rf_ScalarInteger(NA_INTEGER)), mynamespace);
    Rf_unprotect(1);
#else
    INCREMENT_NAMED_defineVar(Rf_install(".NAMEDMAX"), Rf_protect(Rf_ScalarInteger(NAMEDMAX)), mynamespace);
    Rf_unprotect(1);
#endif


#define convertclosure2activebinding(symbol)                   \
    do {                                                       \
        SEXP sym = (symbol);                                   \
        SEXP fun = getFromMyNS(sym);                           \
        Rf_protect(fun);                                       \
        if (TYPEOF(fun) != CLOSXP)                             \
            Rf_error(_("object '%s' of mode '%s' was not found"), EncodeChar(sym), "function");\
        R_removeVarFromFrame(sym, mynamespace);                \
        R_MakeActiveBinding(sym, fun, mynamespace);            \
        Rf_unprotect(1);                                       \
    } while (0)


    /* ./R/ns-hooks.R */
    convertclosure2activebinding(Rf_install(".mbcslocale"));
    convertclosure2activebinding(_utf8localeSymbol);
    convertclosure2activebinding(Rf_install(".latin1locale"));
    convertclosure2activebinding(Rf_install(".R_MB_CUR_MAX"));
    /* ./R/startup.R */
    convertclosure2activebinding(Rf_install(".in_site_file"));
    /* ./R/trycatch.R */
    convertclosure2activebinding(Rf_install("last.condition"));


    SEXP value = Rf_allocVector(VECSXP, 13);
    Rf_protect(value);
    MARK_NOT_MUTABLE_defineVar(Rf_install("OS.type"), value, mynamespace);
    SEXP names = Rf_allocVector(STRSXP, 13);
    Rf_setAttrib(value, R_NamesSymbol, names);


    int i = -1;


    SET_STRING_ELT(names, ++i, Rf_mkChar("AIX"));
#if defined(_AIX)
    /* IBM AIX. ------------------------------------------------- */
    SET_VECTOR_ELT(value, i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i, R_FalseValue);
#endif


//     SET_STRING_ELT(names, ++i, Rf_mkChar("BSD"));
// #if defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
// #include <sys/param.h>
// #if defined(BSD)
//     /* BSD (DragonFly BSD, FreeBSD, OpenBSD, NetBSD). ----------- */
//     SET_VECTOR_ELT(value, i, R_TrueValue);
// #else
//     SET_VECTOR_ELT(value, i, R_FalseValue);
// #endif
// #else
//     SET_VECTOR_ELT(value, i, R_FalseValue);
// #endif


    SET_STRING_ELT(names, ++i, Rf_mkChar("HPUX"));
#if defined(__hpux)
    /* Hewlett-Packard HP-UX. ----------------------------------- */
    SET_VECTOR_ELT(value, i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i, R_FalseValue);
#endif


    SET_STRING_ELT(names, ++i, Rf_mkChar("linux"));
#if defined(__linux__)
    /* Linux. --------------------------------------------------- */
    SET_VECTOR_ELT(value, i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i, R_FalseValue);
#endif


    SET_STRING_ELT(names, i + 1, Rf_mkChar("darwin"));
    SET_STRING_ELT(names, i + 2, Rf_mkChar("iOS.simulator"));
    SET_STRING_ELT(names, i + 3, Rf_mkChar("iOS"));
    SET_STRING_ELT(names, i + 4, Rf_mkChar("macOS"));
#if defined(__APPLE__) && defined(__MACH__)
    /* Apple OSX and iOS (Darwin). ------------------------------ */
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
#include <TargetConditionals.h>
#if TARGET_IPHONE_SIMULATOR == 1
    /* iOS in Xcode simulator */
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
#elif TARGET_OS_IPHONE == 1
    /* iOS on iPhone, iPad, etc. */
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
#elif TARGET_OS_MAC == 1
    /* OSX */
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
#endif
#else /* #if defined(__APPLE__) && defined(__MACH__) */
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
#endif


    SET_STRING_ELT(names, ++i, Rf_mkChar("solaris"));
#if defined(__sun) && defined(__SVR4)
    /* Solaris. ------------------------------------------------- */
    SET_VECTOR_ELT(value, i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i, R_FalseValue);
#endif


    SET_STRING_ELT(names, ++i, Rf_mkChar("cygwin"));
#if defined(__CYGWIN__) && !defined(_WIN32)
    /* Cygwin POSIX under Microsoft Windows. -------------------- */
    SET_VECTOR_ELT(value, i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i, R_FalseValue);
#endif


    SET_STRING_ELT(names, i + 1, Rf_mkChar("windows"));
    SET_STRING_ELT(names, i + 2, Rf_mkChar("win64"));
    SET_STRING_ELT(names, i + 3, Rf_mkChar("win32"));
#if defined(_WIN64)
    /* Microsoft Windows (64-bit). ------------------------------ */
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
#elif defined(_WIN32)
    /* Microsoft Windows (32-bit). ------------------------------ */
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
    SET_VECTOR_ELT(value, ++i, R_FalseValue);
#endif


//     SET_STRING_ELT(names, i + 1, Rf_mkChar("UNIX"));
//     SET_STRING_ELT(names, i + 2, Rf_mkChar("POSIX"));
// #if !defined(_WIN32) && (defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__)))
//     /* UNIX-style OS. ------------------------------------------- */
//     SET_VECTOR_ELT(value, ++i, R_TrueValue);
// // #include <unistd.h>
// #if defined(_POSIX_VERSION)
//     /* POSIX compliant */
//     SET_VECTOR_ELT(value, ++i, R_TrueValue);
// #else
//     SET_VECTOR_ELT(value, ++i, R_FalseValue);
// #endif
// #else
//     SET_VECTOR_ELT(value, ++i, R_FalseValue);
//     SET_VECTOR_ELT(value, ++i, R_FalseValue);
// #endif


    SET_STRING_ELT(names, ++i, Rf_mkChar("UNIX"));
#if !defined(_WIN32) && (defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__)))
    /* UNIX-style OS. ------------------------------------------- */
    SET_VECTOR_ELT(value, i, R_TrueValue);
#else
    SET_VECTOR_ELT(value, i, R_FalseValue);
#endif


    Rf_unprotect(1);


    expr_commandArgs = Rf_lcons(getFromBase(commandArgsSymbol), R_NilValue);
    R_PreserveObject(expr_commandArgs);
    if (!Rf_isFunction(CAR(expr_commandArgs)))
        Rf_error(_("object '%s' of mode '%s' was not found"),
              R_CHAR(PRINTNAME(commandArgsSymbol)), "function");


    expr_invisible = Rf_lcons(getFromBase(invisibleSymbol), R_NilValue);
    R_PreserveObject(expr_invisible);
    if (!Rf_isFunction(CAR(expr_invisible)))
        Rf_error(_("object '%s' of mode '%s' was not found"),
              R_CHAR(PRINTNAME(invisibleSymbol)), "function");


    expr_parent_frame = Rf_lcons(getFromBase(parent_frameSymbol), R_NilValue);
    R_PreserveObject(expr_parent_frame);
    if (!Rf_isFunction(CAR(expr_parent_frame)))
        Rf_error(_("object '%s' of mode '%s' was not found"),
              R_CHAR(PRINTNAME(parent_frameSymbol)), "function");


    expr_sys_call = Rf_lcons(getFromBase(sys_callSymbol), R_NilValue);
    R_PreserveObject(expr_sys_call);
    if (!Rf_isFunction(CAR(expr_sys_call)))
        Rf_error(_("object '%s' of mode '%s' was not found"),
              R_CHAR(PRINTNAME(sys_callSymbol)), "function");


    expr_sys_call_which = Rf_lcons(CAR(expr_sys_call), Rf_cons(Rf_ScalarInteger(0), R_NilValue));
    R_PreserveObject(expr_sys_call_which);


    {
        expr_sys_function_which = Rf_lcons(getFromBase(sys_functionSymbol), CDR(expr_sys_call_which));
        R_PreserveObject(expr_sys_function_which);
    }


#if defined(R_THIS_PATH_DEVEL) || R_version_less_than(4,5,0)
    eval_op = INTERNAL(R_EvalSymbol);
#else
    {
        SEXP evalqSymbol = Rf_install("evalq");
        SEXP expr = Rf_lcons(evalqSymbol, Rf_cons(Rf_lcons(sys_functionSymbol, R_NilValue), R_NilValue));
        Rf_protect(expr);
        eval_op = Rf_eval(expr, R_BaseEnv);
        Rf_unprotect(1);
    }
#endif
    if (TYPEOF(eval_op) != BUILTINSXP)
        Rf_error(_("object '%s' of mode '%s' was not found"),
              R_CHAR(PRINTNAME(R_EvalSymbol)), "builtin");


    expr_sys_nframe = Rf_lcons(getFromBase(sys_nframeSymbol), R_NilValue);
    R_PreserveObject(expr_sys_nframe);
    if (!Rf_isFunction(CAR(expr_sys_nframe)))
        Rf_error(_("object '%s' of mode '%s' was not found"),
              R_CHAR(PRINTNAME(sys_nframeSymbol)), "function");


    expr_sys_parents = Rf_lcons(getFromBase(sys_parentsSymbol), R_NilValue);
    R_PreserveObject(expr_sys_parents);
    if (!Rf_isFunction(CAR(expr_sys_parents)))
        Rf_error(_("object '%s' of mode '%s' was not found"),
              R_CHAR(PRINTNAME(sys_parentsSymbol)), "function");


    {
        SEXP tmp;
        Rf_protect(tmp = getFromBase(missingSymbol));
        expr_missing_file = Rf_lcons(tmp, Rf_cons(fileSymbol, R_NilValue));
        R_PreserveObject(expr_missing_file);
        Rf_unprotect(1);
        if (!Rf_isFunction(CAR(expr_missing_file)))
            Rf_error(_("object '%s' of mode '%s' was not found"),
                  R_CHAR(PRINTNAME(missingSymbol)), "function");
    }


    {
        SEXP tmp;
        Rf_protect(tmp = getFromBase(missingSymbol));
        expr_missing_input = Rf_lcons(tmp, Rf_cons(inputSymbol, R_NilValue));
        R_PreserveObject(expr_missing_input);
        Rf_unprotect(1);
        if (!Rf_isFunction(CAR(expr_missing_input)))
            Rf_error(_("object '%s' of mode '%s' was not found"),
                  R_CHAR(PRINTNAME(missingSymbol)), "function");
    }


    {
        SEXP tmp;
        Rf_protect(tmp = getFromBase(missingSymbol));
        expr_missing_ofile = Rf_lcons(tmp, Rf_cons(ofileSymbol, R_NilValue));
        R_PreserveObject(expr_missing_ofile);
        Rf_unprotect(1);
        if (!Rf_isFunction(CAR(expr_missing_ofile)))
            Rf_error(_("object '%s' of mode '%s' was not found"),
                  R_CHAR(PRINTNAME(missingSymbol)), "function");
    }


    {
        SEXP tmp;
        Rf_protect(tmp = getFromBase(R_DollarSymbol));
        expr_info_dollar_source_path = Rf_lcons(
            tmp,
            Rf_cons(
                infoSymbol,
                Rf_cons(source_pathSymbol, R_NilValue)
            )
        );
        R_PreserveObject(expr_info_dollar_source_path);
        Rf_unprotect(1);
        if (!Rf_isFunction(CAR(expr_info_dollar_source_path)))
            Rf_error(_("object '%s' of mode '%s' was not found"),
                  R_CHAR(PRINTNAME(R_DollarSymbol)), "function");
    }


    expr_knitr_output_dir = Rf_allocLang(2);
    R_PreserveObject(expr_knitr_output_dir);
    {
        SEXP tmp = SETCAR(expr_knitr_output_dir, Rf_allocLang(3));
        SETCADR(expr_knitr_output_dir, Rf_mkString("output.dir"));
        {
            SETCAR(tmp, getFromBase(R_Bracket2Symbol));
            SEXP tmp2 = SETCADR(tmp, Rf_allocLang(3));
            SETCADDR(tmp, Rf_mkString("get"));
            {
                SETCAR  (tmp2, getFromBase(R_DoubleColonSymbol));
                SETCADR (tmp2, knitrSymbol);
                SETCADDR(tmp2, Rf_install("opts_knit"));
            }
        }
    }


    expr_testthat_source_file_uses_brio_read_lines = Rf_allocLang(3);
    R_PreserveObject(expr_testthat_source_file_uses_brio_read_lines);
    {
        SETCAR(expr_testthat_source_file_uses_brio_read_lines, getFromBase(Rf_install(">=")));
        SEXP tmp = SETCADR(expr_testthat_source_file_uses_brio_read_lines, Rf_allocLang(2));
        SETCADDR(expr_testthat_source_file_uses_brio_read_lines, Rf_mkString("3.1.2"));
        {
            SETCAR(tmp, getFromBase(Rf_install("as.numeric_version")));
            SEXP tmp2 = SETCADR(tmp, Rf_allocLang(2));

            {
                SETCAR (tmp2, getFromBase(Rf_install("getNamespaceVersion")));
                SETCADR(tmp2, Rf_ScalarString(PRINTNAME(testthatSymbol)));
            }
        }
    }


    expr__toplevel_nframe = Rf_lcons(getFromMyNS(Rf_install(".toplevel.nframe")), R_NilValue);
    R_PreserveObject(expr__toplevel_nframe);
    if (!Rf_isFunction(CAR(expr__toplevel_nframe)))
        Rf_error(_("object '%s' of mode '%s' was not found"), ".toplevel.nframe", "function");


    expr__isMethodsDispatchOn = Rf_lcons(getFromBase(_isMethodsDispatchOnSymbol), R_NilValue);
    R_PreserveObject(expr__isMethodsDispatchOn);
    if (!Rf_isFunction(CAR(expr__isMethodsDispatchOn)))
        Rf_error(_("object '%s' of mode '%s' was not found"),
            R_CHAR(PRINTNAME(_isMethodsDispatchOnSymbol)), "function");


#if R_version_less_than(3,2,0)
    expr_UseMethod_lengths = Rf_lcons(UseMethodSymbol, Rf_cons(Rf_mkString("lengths"), R_NilValue));
    R_PreserveObject(expr_UseMethod_lengths);
#endif


    expr_makePROMISE = Rf_allocLang(5);
    R_PreserveObject(expr_makePROMISE);
    SETCAR(expr_makePROMISE, getFromBase(Rf_install("delayedAssign")));
    SETCADR(expr_makePROMISE, /* x */ Rf_mkString("x"));
    // SETCADDR(expr_makePROMISE, /* value */ R_NilValue);
    SETCADDDR(expr_makePROMISE, /* eval.env */ R_EmptyEnv);
    SETCAD4R(expr_makePROMISE, /* assign.env */ makePROMISE_environment);
    if (!Rf_isFunction(CAR(expr_makePROMISE)))
        Rf_error(_("object '%s' of mode '%s' was not found"), "delayedAssign", "function");


    expr_getwd = Rf_lcons(getFromBase(Rf_install("getwd")), R_NilValue);
    R_PreserveObject(expr_getwd);
    if (!Rf_isFunction(CAR(expr_getwd)))
        Rf_error(_("object '%s' of mode '%s' was not found"),
            "getwd", "function");


    {
        /* if package:utils is loaded, call '.fix_utils' */
        if (!ISUNBOUND(Rf_findVarInFrame(R_NamespaceRegistry, utilsSymbol))) {
            SEXP expr = Rf_lcons(Rf_install(".fix_utils"), R_NilValue);
            Rf_protect(expr);
            Rf_eval(expr, mynamespace);
            Rf_unprotect(1);
        }


        /* for when package:utils is loaded (or possibly unloaded then reloaded), set as a hook */
        SEXP expr = Rf_lcons(Rf_install(".maybe_setHook_packageEvent_utils_fix_utils"), R_NilValue);
        Rf_protect(expr);
        Rf_eval(expr, mynamespace);
        Rf_unprotect(1);
    }


    {
        /* if package:plumber is loaded, call '.fix_plumber_parseUTF8' */
        if (!ISUNBOUND(Rf_findVarInFrame(R_NamespaceRegistry, plumberSymbol))) {
            SEXP expr = Rf_lcons(Rf_install(".fix_plumber_parseUTF8"), R_NilValue);
            Rf_protect(expr);
            Rf_eval(expr, mynamespace);
            Rf_unprotect(1);
        }


        /* for when package:plumber is loaded (or possibly unloaded then reloaded), set as a hook */
        SEXP expr = Rf_lcons(Rf_install(".maybe_setHook_packageEvent_plumber_fix_plumber_parseUTF8"), R_NilValue);
        Rf_protect(expr);
        Rf_eval(expr, mynamespace);
        Rf_unprotect(1);
    }


    _custom_gui_path_character_environment =
        R_NewEnv(/* enclos */ mynamespace, /* hash */ TRUE, /* size */ 10);
    R_PreserveObject(_custom_gui_path_character_environment);
    Rf_defineVar(guinameSymbol, R_MissingArg, _custom_gui_path_character_environment);
    {
        SEXP na = Rf_ScalarString(NA_STRING);
        Rf_protect(na);
        ENSURE_NAMEDMAX(na);
        Rf_defineVar(ofileSymbol, makeEVPROMISE(na, na), _custom_gui_path_character_environment);
        R_LockBinding(ofileSymbol, _custom_gui_path_character_environment);
        Rf_unprotect(1);
    }
    {
        SEXP expr = Rf_lcons(_normalizePath_not_dirSymbol, Rf_cons(ofileSymbol, R_NilValue));
        Rf_protect(expr);
        Rf_defineVar(
            fileSymbol,
            makePROMISE(expr, _custom_gui_path_character_environment),
            _custom_gui_path_character_environment
        );
        R_LockBinding(fileSymbol, _custom_gui_path_character_environment);
        Rf_unprotect(1);
    }
    Rf_defineVar(_get_contentsSymbol, R_NilValue, _custom_gui_path_character_environment);
    R_LockEnvironment(_custom_gui_path_character_environment, FALSE);


    _custom_gui_path_function_environment =
        R_NewEnv(/* enclos */ R_EmptyEnv, /* hash */ TRUE, /* size */ 2);
    R_PreserveObject(_custom_gui_path_function_environment);
    Rf_defineVar(_custom_gui_path_functionSymbol, R_NilValue, _custom_gui_path_function_environment);
    R_LockEnvironment(_custom_gui_path_function_environment, FALSE);


#if R_version_less_than(3,4,0)
    {
        SEXP sym = Rf_install("print.connection");
        SEXP val = Rf_findVarInFrame(mynamespace, sym);
        if (val != R_UnboundValue) {
            R_unLockBinding(sym, R_BaseEnv);
            Rf_defineVar(sym, val, R_BaseEnv);
            R_LockBinding(sym, R_BaseEnv);
        }
    }
#endif


    return R_NilValue;
}


SEXP do_onUnload do_formals
{
    do_start_no_call_op("onUnload", 1);


    SEXP libpath = CAR(args);


    {
        /* on.exit(library.dynam.unload("this.path", libpath)) */
        SEXP expr;
        PROTECT_INDEX indx;
        R_ProtectWithIndex(expr = Rf_cons(libpath, R_NilValue), &indx);
        R_Reprotect(expr = Rf_cons(Rf_mkString("this.path"), expr), indx);
        R_Reprotect(expr = Rf_lcons(getFromBase(Rf_install("library.dynam.unload")), expr), indx);
        R_Reprotect(expr = Rf_cons(expr, R_NilValue), indx);
        R_Reprotect(expr = Rf_lcons(getFromBase(on_exitSymbol), expr), indx);
        Rf_eval(expr, rho);
        Rf_unprotect(1);
    }


#if !defined(R_THIS_PATH_DEVEL)
    {
        SEXP expr = Rf_lcons(Rf_install(".maybe_dyn_unload"), R_NilValue);
        Rf_protect(expr);
        Rf_eval(expr, mynamespace);
        Rf_unprotect(1);
    }
#endif


#define maybe_release(var) if ((var)) R_ReleaseObject((var))


    maybe_release(mynamespace);
    maybe_release(DocumentContextClass);
    maybe_release(ThisPathInAQUAErrorClass);
    maybe_release(ThisPathInZipFileErrorClass);
    maybe_release(ThisPathNotExistsErrorClass);
    maybe_release(ThisPathNotFoundErrorClass);
    maybe_release(ThisPathNotImplementedErrorClass);
    maybe_release(ThisPathUnrecognizedConnectionClassErrorClass);
    maybe_release(ThisPathUnrecognizedMannerErrorClass);
    maybe_release(last_condition);
    maybe_release(_custom_gui_path_character_environment);
    maybe_release(_custom_gui_path_function_environment);
    maybe_release(makePROMISE_environment);


#if defined(R_THIS_PATH_NEED_BLANKSCALARSTRING)
    maybe_release(R_BlankScalarString);
#endif


    maybe_release(expr_commandArgs);
    maybe_release(expr_invisible);
    maybe_release(expr_parent_frame);
    maybe_release(expr_sys_call);
    maybe_release(expr_sys_call_which);
    maybe_release(expr_sys_function_which);
    maybe_release(expr_sys_nframe);
    maybe_release(expr_sys_parents);
    maybe_release(expr_missing_file);
    maybe_release(expr_missing_input);
    maybe_release(expr_missing_input);
    maybe_release(expr_info_dollar_source_path);
    maybe_release(expr_knitr_output_dir);
    maybe_release(expr_testthat_source_file_uses_brio_read_lines);
    maybe_release(expr__toplevel_nframe);
    maybe_release(expr__isMethodsDispatchOn);
    maybe_release(expr_UseMethod_lengths);
    maybe_release(expr_makePROMISE);
    maybe_release(expr_getwd);


    return R_NilValue;
}
