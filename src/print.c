#include "thispathdefn.h"





Rboolean my_isMethodDispatchOn(void)
{
    SEXP expr = LCONS(_isMethodsDispatchOnSymbol, R_NilValue);
    PROTECT(expr);
    Rboolean value = asLogical(eval(expr, R_BaseEnv));
    UNPROTECT(1);
    return value;
}


void my_PrintObjectS4(SEXP s, SEXP env)
{
    SEXP methods = findVarInFrame(R_NamespaceRegistry, methodsSymbol);
    if (methods == R_UnboundValue)
        error("missing methods namespace: this should not happen");


    SEXP show = getInFrame(showSymbol, methods, TRUE);
    if (show == R_UnboundValue)
        error("missing show() in methods namespace: this should not happen");


    SEXP expr = PROTECT(lang2(show, s));
    eval(expr, env);
    UNPROTECT(1);
}


void my_PrintObjectS3(SEXP s, SEXP env)
{
    SEXP mask = PROTECT(R_NewEnv(env, FALSE, 0));
    defineVar(xSymbol, s, mask);


    SEXP print = PROTECT(findFunction(printSymbol, R_BaseNamespace));
    SEXP expr = PROTECT(LCONS(print, CONS(xSymbol, R_NilValue)));


    eval(expr, mask);


    defineVar(xSymbol, R_NilValue, mask);
    UNPROTECT(3);
}


void my_PrintObject(SEXP s, SEXP env)
{
    if (my_isMethodDispatchOn() && IS_S4_OBJECT(s))
        my_PrintObjectS4(s, env);
    else
        my_PrintObjectS3(s, env);
}


void my_PrintValueRec(SEXP s, SEXP env)
{
    SEXP mask = PROTECT(R_NewEnv(env, FALSE, 0));
    defineVar(xSymbol, s, mask);


    SEXP print = PROTECT(findFunction(print_defaultSymbol, R_BaseNamespace));
    SEXP expr = PROTECT(LCONS(print, CONS(xSymbol, R_NilValue)));


    eval(expr, mask);


    defineVar(xSymbol, R_NilValue, mask);
    UNPROTECT(3);
}


void my_PrintDispatch(SEXP s, SEXP env)
{
    if (isObject(s))
        my_PrintObject(s, env);
    else
        my_PrintValueRec(s, env);
}


void my_PrintValueEnv(SEXP s, SEXP env)
{
    PROTECT(s);


    if (isFunction(s))
        my_PrintObject(s, env);
    else
        my_PrintDispatch(s, env);


    UNPROTECT(1);
}


SEXP do_PrintValueEnv do_formals
{
    do_start_no_call_op("PrintValueEnv", 2);
    my_PrintValueEnv(CAR(args), CADR(args));
    set_R_Visible(FALSE);
    return CAR(args);
}
