(require 'cl)
(require 'json)
(require 'simple-httpd)
(require 'cache-table)

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

(defvar best '(0 "--------------------------" nil))

(defservlet words application/json ()
  (insert words-json))

(defvar best-clients ())

(defun httpd/best (proc path args request)
  (let ((known (or (cadr (assoc "score" args)) "0")))
    (if (not (= (string-to-number known) (first best)))
        (with-httpd-buffer proc "application/json"
          (insert (json-encode best)))
      (push proc best-clients))))

(defun update-clients ()
  (while best-clients
    (ignore-errors
      (with-httpd-buffer (pop best-clients) "application/json"
        (insert (json-encode best))))))

(defun save-best ()
  (with-temp-file (expand-file-name "best" key-collab-data-root)
    (prin1 best (current-buffer))))

(defun load-best ()
  (with-temp-buffer
    (insert-file-contents-literally
     (expand-file-name "best" key-collab-data-root))
    (setq best (read (current-buffer)))))

(defservlet report text/plain (path args request)
  (let* ((report (json-read-from-string (cadr (assoc "Content" request))))
         (key (cdr (assoc 'key report)))
         (name (cdr (assoc 'name report)))
         (score (score key)))
    (when (> score (first best))
      (setq best (list score key name))
      (save-best)
      (update-clients))))

(defvar global-cpu (make-cache-table 30 :test 'equal))

(defservlet cpu application/json (path args request)
  (let* ((report (json-read-from-string (cadr (assoc "Content" request))))
         (rate (string-to-number (cdr (assoc 'rate report))))
         (id (cdr (assoc 'id report)))
         (total 0))
    (setf (get-cache-table id global-cpu) rate)
    (cache-table-map (lambda (k v) (incf total v)) global-cpu)
    (insert (json-encode `((rate . ,total)
                           (clients . ,(cache-table-count global-cpu)))))))

(load-best)
