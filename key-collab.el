(require 'cl)
(require 'json)
(require 'simple-httpd)

(defvar key-collab-data-root "~/src/scratch/key-collab")

(defvar words
  (with-temp-buffer
    (url-insert-file-contents "http://pastebin.com/download.php?i=9aFn1r27")
    (split-string (buffer-string))))

(defvar words-json (json-encode words))

(defun encode (key word)
  (loop for char across word
        for encoded = (aref key (position char "abcdefghijklmnopqrstuvwxyz"))
        collect encoded into output
        finally (return (coerce output 'string))))

(defun sorted-p (word)
  (string= (sort* (copy-seq word) #'<) word))

(defun score (key)
  (count-if #'sorted-p (mapcar (apply-partially #'encode key) words)))

;; Server

(defvar best '(-1 "" nil))

(defservlet words application/json ()
  (insert words-json))

(defservlet report text/plain (path args request)
  (let* ((report (json-read-from-string (cadr (assoc "Content" request))))
         (key (cdr (assoc 'key report)))
         (name (cdr (assoc 'name report)))
         (score (score key)))
    (when (> score (first best))
      (setq best (list score key name)))))
