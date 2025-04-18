#ifndef R_THISPATH_FILES_H
#define R_THISPATH_FILES_H


extern int drive_width_windows(const char *s);
extern int drive_width_no_tilde_windows(const char *s);
extern int drive_width_unix   (const char *s);
extern int is_abs_path_windows(const char *s);
extern int is_abs_path_unix   (const char *s);

#define _drive_width(windows, s) ((windows) ? (drive_width_windows((s))) : (drive_width_unix((s))))
#define _drive_width_no_tilde(windows, s) ((windows) ? (drive_width_no_tilde_windows((s))) : (drive_width_unix((s))))
#define _is_abs_path(windows, s) ((windows) ? (is_abs_path_windows((s))) : (is_abs_path_unix((s))))
#if defined(_WIN32)
#define drive_width(s) (drive_width_windows((s)))
#define is_abs_path(s) (is_abs_path_windows((s)))
#else
#define drive_width(s) (drive_width_unix((s)))
#define is_abs_path(s) (is_abs_path_unix((s)))
#endif


extern int is_clipboard(const char *url);
extern const char *must_not_be_clipboard_message;
extern int is_file_uri(const char *url);
extern int is_url(const char *url);


#endif
