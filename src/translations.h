#ifndef R_THISPATH_TRANSLATIONS_H
#define R_THISPATH_TRANSLATIONS_H


#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("R", String)
#define _RGui(String) dgettext ("RGui", String)
#else
#define _(String) (String)
#define _RGui(String) (String)
#endif


#endif
