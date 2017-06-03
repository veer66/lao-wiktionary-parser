(require 'xmls)
(require 'arrow-macros)
(require 'cl-ppcre)

(import 'arrow-macros::->)
(import 'arrow-macros::->>)

(defparameter *word-pat*
  (format nil "^[~A-~A]+$"
          (code-char #x0E80)
          (code-char #x0EFF)))

(defparameter *tr-line-match-pat* "\\*\\{\\{([a-z][a-z])\\}\\}: (.+)")

(defparameter *cat-pat* (format nil "([~A-~A]+)"
                                (code-char #x0E80)
                                (code-char #x0EFF)))

(defun parse-tr-part (tr-part)
  (cl-ppcre:register-groups-bind (inner)
      ("\\{\\{([^\\}]+)\\}\\}" tr-part)
    (caddr (cl-ppcre:split "\\|" inner))))

(defun extract-from-tr-line (line)
  (let ((translates nil))
    (cl-ppcre:do-register-groups (lang tr-parts)
        (*tr-line-match-pat* line)
      (let ((tr-list (->> (cl-ppcre:all-matches-as-strings
                      "\\{\\{[^\\}]+\\}\\}" tr-parts)
                  (mapcar #'parse-tr-part))))
        (when tr-list
          (setq translates
                (cons (cons (intern (string-upcase lang)
                                    "KEYWORD")
                            tr-list)
                      translates)))))
    translates))

(defun extract-tr-from-text (text)
  (apply #'nconc
         (->> (cl-ppcre:split "[\\n\\r]+" text)
           (mapcar #'extract-from-tr-line)
           (remove-if #'null))))


(defun extract-category-value (lines)
  (loop for line in lines
     while (and (cl-ppcre:all-matches-as-strings *cat-pat* line)
                (null (cl-ppcre:all-matches-as-strings "=" line)))
     collect (cl-ppcre:register-groups-bind (cat)
                 (*cat-pat* line)
               cat)))

(defun extract-category-from-lines (lines)
  (cond
    ((null lines) nil)
    ((cl-ppcre:all-matches-as-strings "=\\s*ປະເພດ" (car lines))
     (let ((ext-cat (extract-category-value (cdr lines))))
       (if ext-cat
           ext-cat
           (extract-category-from-lines (cdr lines)))))
    (t (extract-category-from-lines (cdr lines)))))

(defun extract-category-from-text (text)
  (let ((lines (cl-ppcre:split "[\\n\\r]+" text)))
    (extract-category-from-lines lines)))

(defun extract-from-text (text)
  (append (list :translations (extract-tr-from-text text))
          (list :category (extract-category-from-text text))))

(defun extract-from-page (page)
  (let* ((title (xmls:xmlrep-find-child-tag "title" page))
         (title-text (caddr title))
         (rev (xmls:xmlrep-find-child-tag "revision" page))
         (text-elem (xmls:xmlrep-find-child-tag "text" rev))
         (text (caddr text-elem)))
    (append (list :headword title-text)
            (extract-from-text text))))

(defun trim (lst)
  (subseq lst 0 30))

(defun parse (xml-path)
  (->> (with-open-file (stream xml-path)
         (xmls:parse stream))
    (xmls:xmlrep-find-child-tags "page")
    (remove-if-not #'(lambda (page)
                       (let* ((title (xmls:xmlrep-find-child-tag "title" page))
                              (title-child (caddr title)))
                         (cl-ppcre:scan *word-pat* title-child))))
    (mapcar #'extract-from-page)
    write))


