(defun bloomberg-c-mode ()
  (interactive)
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (modify-syntax-entry ?_ "w")
  (c-set-offset 'innamespace 0)
  (setq c-font-lock-extra-types (list "gboolean" "gsize" "gssize"
				      "gchar" "guchar"
				      "gint" "gint8" "gint16" "gint32"
				      "guint" "guint8" "guint16" "guint32"
				      "gshort" "gushort" "glong" "gulong"
				      "gfloat" "gdouble" "gpointer"
				      "gconstpointer"
				      "GList" "GSList" "GFunc" "GString" )))

(add-hook 'c-mode-common-hook 'bloomberg-c-mode)

(provide 'bbg-style)
