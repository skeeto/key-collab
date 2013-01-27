;;; key-collab.el --- distributed computing with Emacs

;; Solution to the Daily Programmer Hard Challenge #118. Uses browsers
;; for distributed computing.

;; http://redd.it/178vsz

;;; Code:

(require 'cl)
(require 'json)
(require 'simple-httpd)
(require 'cache-table)

;; Challenge data

(defvar key-collab-data-root "~/src/scratch/key-collab")

(defvar words
  (with-temp-buffer
    (url-insert-file-contents "http://pastebin.com/download.php?i=9aFn1r27")
    (split-string (buffer-string))))

(defvar words-json (json-encode words))

;; Local verification. Don't trust the client's score.

(defun encode (key word)
  "Encode WORD using KEY."
  (loop for char across word
        for encoded = (aref key (position char "abcdefghijklmnopqrstuvwxyz"))
        collect encoded into output
        finally (return (coerce output 'string))))

(defun sorted-p (word)
  "Return T if WORD's letters are sorted."
  (string= (sort* (copy-seq word) #'<) word))

(defun score (key)
  "Determine the score for KEY. Return 0 if the key is invalid."
  (if (= 26 (length (remove-duplicates key)))
      (count-if #'sorted-p (mapcar (apply-partially #'encode key) words))
    0))

;; Server

(defvar best '(0 "--------------------------" nil)
  "Current best-known answer.")

(defvar best-clients ()
  "List of all client's waiting for updates.")

(defservlet words application/json ()
  "Return the list of words in JSON form."
  (insert words-json))

(defun httpd/best (proc path args request)
  "Return the current best solution, waiting if necessary (long poll)."
  (let ((known (or (cadr (assoc "score" args)) "0")))
    (if (not (= (string-to-number known) (first best)))
        (with-httpd-buffer proc "application/json"
          (insert (json-encode best)))
      (push proc best-clients))))

(defun update-clients ()
  "Update all waiting clients with the latest best solution."
  (while best-clients
    (ignore-errors
      (with-httpd-buffer (pop best-clients) "application/json"
        (insert (json-encode best))))))

(defun save-best ()
  "Save the current best solution to disk."
  (with-temp-file (expand-file-name "best" key-collab-data-root)
    (prin1 best (current-buffer))))

(defun load-best ()
  "Load the best known solution from disk."
  (with-temp-buffer
    (insert-file-contents-literally
     (expand-file-name "best" key-collab-data-root))
    (setq best (read (current-buffer)))))

(defservlet report text/plain (path args request)
  "Receive a report of a better solution from a client."
  (let* ((report (json-read-from-string (cadr (assoc "Content" request))))
         (key (cdr (assoc 'key report)))
         (name (cdr (assoc 'name report)))
         (score (score key)))
    (when (> score (first best))
      (setq best (list score key name))
      (save-best)
      (update-clients))))

(defvar global-cpu (make-cache-table 30 :test 'equal)
  "Current global CPU rate. Stale data is dropped after 30 seconds.")

(defvar key-collab-message nil)
(defvar key-collab-version 2)

(defservlet cpu application/json (path args request)
  "Accept a CPU rate from a client."
  (let* ((report (json-read-from-string (cadr (assoc "Content" request))))
         (rate (string-to-number (cdr (assoc 'rate report))))
         (id (cdr (assoc 'id report)))
         (total 0))
    (setf (get-cache-table id global-cpu) rate)
    (cache-table-map (lambda (k v) (incf total v)) global-cpu)
    (insert (json-encode `((rate . ,total)
                           (clients . ,(cache-table-count global-cpu))
                           (message . ,key-collab-message)
                           (version . ,key-collab-version))))))

(load-best)

(provide 'key-collab)

;;; key-collab.el ends here
