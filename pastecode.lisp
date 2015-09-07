(in-package #:pastecode)

(create-with-db-macro with-paste-db db (asdf:system-relative-pathname '#:pastecode "db.sqlite"))

(defmacro web-entry (name uri (&rest params) &body body)
  (let ((uri (sconc "/paste" uri)))
    `(sql-easy-handler (,name :uri ,uri) ,params
       ,@body)))

(web-entry style-url "/style.css" ()
  (hunchentoot:handle-static-file (asdf:system-relative-pathname "pastecode" "style.css")))

(defsqlite-table languages
  (id :auto-key)
  (name :t)
  (handler-sym :t))

(defsqlite-table pastes
  (id :auto-key)
  (language-id :i :fk languages))

(defsqlite-table paste-entries
  (id :auto-key)
  (paste-id :i :fk pastes)
  (created-at :i)
  (description :t)
  (paste-text :t))

(defsqlite-table common-lisp-paste-packages
  (id :i :fk pastes :pk)
  (package :t))

(defvar +minute+ 60)
(defvar +hour+ (* +minute+ 60))
(defvar +day+ (* +hour+ 24))

(defun get-midnight (now)
  (bind ((:mv (second- minute- hour- date month year day- daylight-p- zone)
	      (decode-universal-time now)))
    (encode-universal-time 0 0 0 date month year zone)))

(defun fancy-datestring (universal-time)
  (bind ((:mv (second minute hour date month year day daylight-p- zone-)
	      (decode-universal-time universal-time))
	 (now (get-universal-time))
	 (delta (- now universal-time))
	 (midnight (get-midnight now))
	 (weekdays #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")))
    (flet ((time-string ()
	     (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))
      (sconc (cond
	       ((< delta +hour+)
		(format nil "~a minute~:p ago" (floor delta +minute+)))
	       ((< delta (* 12 +hour+))
		(format nil "~a hour~:p ago" (floor delta +hour+)))
	       ((< midnight universal-time)
		(format nil "Today"))
	       ((< (- midnight +day+) universal-time)
		(format nil "Yesterday"))
	       ((< (- midnight (* 6 +day+)) universal-time)
		(format nil "~a" (aref weekdays day)))
	       (t
		(format nil "~a/~a/~a" date month year)))
	     " at "
	     (time-string)))))

(def-logger log-message *logging-context*)

(defvar +logging-path+
  (asdf:system-relative-pathname '#:pastecode
				 "logging.sqlite"))

(setf *logging-context*
      (make-logging-context +logging-path+))

(defparameter +common-lisp-language+ nil)

(defmacro init-tables (&rest funcs)
  `(progn
     ,@ (mapcar (lambda (func)
		  `(,func db nil #'log-message))
		funcs)))

(with-paste-db
  (init-tables create-languages-table
	       create-pastes-table
	       create-paste-entries-table
	       create-common-lisp-paste-packages-table)
  (dolist (pair '(("Common-Lisp" common-lisp-handler)))
    (bind ((:db (name handler-sym)
		pair)
	   (handler-sym (symbol-name handler-sym)))
      (setf +common-lisp-language+
	    (select-or-insert db languages name handler-sym)))))

(defmacro page-template (title &body body)
  `(pjs-yaclml:with-yaclml-output-to-string
     (<:as-is "<!DOCTYPE html>" #\Newline)
     (<:html
       (<:head
	 (<:title (<:as-html ,title))
	 (<:link :rel "stylesheet" :href "/paste/style.css" :type "text/css"))
       (<:body
	 ,@body))))

(web-entry dash "/"
    ((widget :parameter-type (sql-table
			      :query (:select
				       (p.id l.name pe.description pe.created-at)
				       (:from
					(:left-join
					 (:as p pastes)
					 (:as pe
					      (:select
						(paste-entries.id
						 paste-id
						 description
						 (:as created-at
						      (min created-at)))
						(:from paste-entries)
						(:group-by paste-id)))
					 (= p.id pe.id)
					 (:as l languages)
					 (= p.language-id l.id))))
			      :columns (("Paste" 2 ("/paste/item/?id=~a" 0) pe.description)
					("Language" 1 nil l.name)
					("Created" (fancy-datestring 3) nil pe.created-at)
					("Delete" ((lambda (id)
						     (when (local-network-request-p)
						       (<:form :method "POST"
							 :action "/paste/delete/"
							 :enctype "multipart/form-data"
							 (<:input :type "hidden" :name "id" :value id)
							 (<:input :type "submit" :value "Delete")))
						     nil)
						   0))))))
  (with-paste-db
    (page-template "Paste List"
      (widget db))))

(web-entry delete-url "/delete/"
    ((id :parameter-type 'integer))
  (when (local-network-request-p)
    (with-paste-db
      (delete-pastes db id))
    (hunchentoot:redirect "/paste/")))

(defun insert-paste-for-slime (description text package)
  (with-paste-db
    (sqlite:with-transaction db
      (let ((key (insert-pastes db +common-lisp-language+)))
	(insert-paste-entries db key (get-universal-time) description text)
	(insert-common-lisp-paste-packages db key (string (read-from-string package)))))))

(defun expand-path (form path)
  ;(log-message 'debug "walking ~%~w~%path ~w~%" form path)
  (let ((i (pop path))
	(j 0))
    (if (null i)
	(let ((expanded (macroexpand-1 form)))
	  ;(log-message 'debug "expanding ~w~% ->~%~w~%" form expanded)
	  (if path
	      (expand-path expanded path)
	      ;; else
	      expanded))
	;; else
	(mapcar (lambda (form)
		  (if (listp form)
		      (prog1
			  (if (= i j)
			      (expand-path form path)
			      form)
			(incf j))
		      ;; else
		      form))
		form))))

(defun common-lisp-handler (db id)
  (bind ((package-name (sqlite:execute-one-row-m-v db "SELECT package FROM common_lisp_paste_packages WHERE id = ?" id))
	 (*package* (find-package package-name)))
    (do-sqlite-query db ("SELECT id, description, created_at, paste_text FROM paste_entries WHERE paste_id = ? ORDER BY id" id)
	(entry description created-at paste-text)
      (<:div :class "description"
	(<:p :class "time" (<:as-html (fancy-datestring created-at)))
	(<:as-html description))

      (<:div :class "paste"
	(<:pre
	  (<:as-html paste-text))
	(<:p (<:a :href (url-with-params "/paste/expand/" id entry)
	       "Expanded"))))))

(web-entry view-paste "/item/"
    ((id :parameter-type 'integer))
  (with-paste-db
    (bind ((:mv (db-id handler)
		(sqlite:execute-one-row-m-v db "SELECT p.id, l.handler_sym FROM pastes AS p LEFT JOIN languages AS l ON p.language_id = l.id WHERE p.id = ?" id)))
      (unless db-id
	(error "Must supply a valid id ~a" id))
      (page-template (format nil "View Paste ~a" db-id)
	(funcall (intern handler "PASTECODE") db db-id)))))

(defclass sublist-info ()
  ((path :initarg :path
	 :reader sublist-info-path)
   (start :initarg :start
	  :reader sublist-info-start)
   (end :initarg :end
	:reader sublist-info-end)
   (content :initarg :content
	    :reader sublist-info-content)))

;(pjs-debug-print-object:for-class sublist-info)

(defparameter +object-sentinel+
  (code-char #xe000))

(defparameter +first-delimiter+
  #xe001)

(defparameter +last-delimiter+
  #xf8ff)

(defun pretty-printed-form-positions (form)
  (unless (listp form)
    (error "must have list argument ~w" form))
  (bind ((original-dispatch (copy-pprint-dispatch))
	 (new-dispatch (copy-pprint-dispatch original-dispatch))
	 (next-marker +first-delimiter+)
	 (old-markers nil)
	 (*print-pretty* t)
	 (*print-escape* t)
	 (printed (with-output-to-string (*standard-output*)
		    (write form))))
    (macrolet ((with-marker (var &body body)
		 `(let ((,var (next-marker)))
		    ,@body
		    (return-marker ,var)))
	       (wrap-marker (stream &body body)
		 (let ((sym (gensym)))
		   `(with-marker ,sym
		      (write-char ,sym ,stream)
		      ,@body
		      (write-char ,sym ,stream)))))
      (labels ((next-marker ()
		 (if old-markers
		     (pop old-markers)
		     ;; else
		     (prog1
			 (code-char next-marker)
		       (when (< +last-delimiter+
				(incf next-marker))
			 (error "too many delimiters required")))))
	       (return-marker (marker)
		 (push marker old-markers))
	       (quotedp (sym form)
		 (and (eq (car form)
			  sym)
		      (null (cddr form))))
	       (output-element (s el)
		 (write el
			:stream s
			:pprint-dispatch (if (listp el)
					     new-dispatch
					     ;; else
					     original-dispatch)))
	       (output-delimited (s obj)
		 (wrap-marker s
		   (cond
		     #+sbcl
		     ((quotedp 'sb-int::quasiquote obj)
		      (write-char #\` s)
		      (write (second obj) :stream s))
		     ((quotedp 'quote obj)
		      (write-char #\' s)
		      (write (second obj) :stream s))
		     ((quotedp 'function obj)
		      (write-string "#'" s)
		      (write (second obj) :stream s))
		     (t
		      (write-char #\( s)
		      (while (consp obj)
			(output-element s (pop obj))
			(when obj
			  (write-char #\Space s)))
		      (when obj
			(write-string ". " s)
			(write obj
			       :stream s
			       :pprint-dispatch original-dispatch))
		      (write-char #\) s)))))
	       (delimiterp (ch)
		 (let ((code (char-code ch)))
		   (<= +first-delimiter+
		       code
		       +last-delimiter+))))
	(dolist (spec '(cons
			(cons (member flet))))
	  (set-pprint-dispatch spec
			       #'output-delimited
			       most-positive-fixnum
			       new-dispatch))
	(set-pprint-dispatch '(member nil)
			     (lambda (s obj)
			       (declare (ignore obj))
			       (wrap-marker s
					    (write-string "()" s)))
			     most-positive-fixnum
			     new-dispatch)
	(let ((munged (with-output-to-string (*standard-output*)
			(write form :pprint-dispatch new-dispatch)))
	      (i 0)
	      ;; assuming form is a list, the first char of munged will always be a delimiter
	      (j 1)
	      ;; might be printed as CL:NIL or even COMMON-LISP:NIL, or more variations
	      (nil-as-string (with-output-to-string (*standard-output*)
			       (write nil))))
					;(format t "munged: ~w~%printed ~w~%" munged printed)
	  (labels ((whitespacep (ch)
		     (member ch '(#\Space #\Tab #\Newline #\Return) :test #'char=))
		   (delimited-list (delimiter path)
		     ;; print sometimes injects leading whitespace
		     #+nil
		     (while (whitespacep (char printed i))
		       (incf i))
		     #+nil
		     (while (whitespacep (char munged j))
		       (incf j))
		     (let ((start i)
			   (subform-counter 0))
		       (with-collector* (subform)
			 (while t
			   (cond
			     ((char= (char munged j)
				     delimiter)
			      (incf j)
			      (return (make-instance 'sublist-info
						     :start start
						     :end i
						     :path (reverse path)
						     :content (subform))))
			     ((<= (length printed)
				  i)
			      (error "i too big"))
			     ((<= (length munged)
				  j)
			      (error "j too big"))
			     ((whitespacep (char printed i))
			      (incf i))
			     ((whitespacep (char munged j))
			      (incf j))
			     ((delimiterp (char munged j))
			      (let ((delimiter (char munged j)))
				(incf j)
				(subform (delimited-list delimiter
							 (cons subform-counter path)))
				(incf subform-counter)))
			     ((char= (char printed i)
				     (char munged j))
			      (incf i)
			      (incf j))
			     ((and (string= printed nil-as-string :start1 i :end1 (+ i (length nil-as-string)))
				   (string= munged "()" :start1 j :end1 (+ j 2)))
			      (incf i (length nil-as-string))
			      (incf j 2))
			     (t
			      (error "something bad happened! ~a (~a) ~a (~a)~%~a~%~a"
				     i
				     (char printed i)
				     j
				     (char munged j)
				     printed
				     munged))))))))
	    (values printed
		    (delimited-list (char munged 0) nil)
		    munged)))))))

(defun form-for-path (root path)
  (if path
      (let ((counter 0)
	    (target (first path)))
	(dolist (el root)
	  (when (listp el)
	    (if (= counter target)
		(return (form-for-path el
				       (rest path)))
		;; else
		(incf counter)))))
      ;; else
      root))

(defparameter +test-form+
  '(web-entry dash "/"
    ((widget :parameter-type (sql-table
			      :query (:select
				       (p.id l.name pe.description pe.created-at)
				       (:from
					(:left-join
					 (:as p pastes)
					 (:as pe
					      (:select
						(paste-entries.id
						 paste-id
						 description
						 (:as created-at
						      (min created-at)))
						(:from paste-entries)
						(:group-by paste-id)))
					 (= p.id pe.id)
					 (:as l languages)
					 (= p.language-id l.id))))
			      :columns (("Paste" 2 ("/paste/item/?id=~a" 0) pe.description)
					("Language" 1 nil l.name)
					("Created" (fancy-datestring 3) nil pe.created-at)))))
    (with-paste-db
      (page-template "Paste List"
	(widget db)))))

(defun test-pretty-printed-form-positions ()
  (bind ((*print-right-margin* 100)
	 (form +test-form+)
	 (:mv (str sub-forms)
	      (pretty-printed-form-positions form)))
    (labels ((rec (sub-form)
	       (format t "path ~w~%" (sublist-info-path sub-form))
	       (format t "start ~a~%" (sublist-info-start sub-form))
	       (format t "string ~w~%"
		       (subseq str
			       (sublist-info-start sub-form)
			       (sublist-info-end sub-form)))
	       (format t "form ~a~%" (form-for-path form
						    (sublist-info-path sub-form)))
	       (dolist (sub (sublist-info-content sub-form))
		 (rec sub))))
      (rec sub-forms))))

(defclass expansion-metadata ()
  ((package :initarg :package
	    :reader expansion-metadata-package)
   (entries :initarg :entries
	    :reader expansion-metadata-entries)))

(defclass expanded-entry ()
  ((form :initarg :form
     :reader expanded-entry-form)
   (parent :initarg :parent
	   :reader expanded-entry-parent)))

(defun potential-macro-positions (form sub-forms)
  (with-collector (collect)
    (labels ((rec (sub-form)
	       (bind ((path (sublist-info-path sub-form))
		      (el (form-for-path form path))
		      (first (first el)))
		 (when (and (symbolp first)
			    (not (member first '(quote
						 function
						 #+sbcl
						 sb-int::quasiquote)))
			    (or (special-operator-p first)
				(macro-function first)))
		   (collect (list first
				  (1+ (sublist-info-start sub-form))
				  (length (prin1-to-string first))
				  path)))
		 (map 'nil #'rec (sublist-info-content sub-form)))))
      (rec sub-forms))))

(defun test-potential-macro-positions ()
  (bind ((form +test-form+)
	 (:mv (str sub-forms)
	      (pretty-printed-form-positions form))
	 (positions (potential-macro-positions form sub-forms))
	 (end 0))
    (dolist (pos positions)
      (bind ((:db (sym start length path)
		  pos)
	     (next-end (+ start length)))
	(format t "sym ~w~%" sym)
	(format t "start ~a length ~a~%" start length)
	(format t "substr ~w~%" (subseq str start (+ start length)))
	(format t "subform ~w~%" (form-for-path form path))
	(format t "prefix ~w~%" (subseq str end start))
	(setf end next-end)))))

(defun render-expansion-page (id entry parent expansion)
  (bind ((form (expanded-entry-form expansion))
	 (:mv (str sub-forms munged)
	      (pretty-printed-form-positions form))
	 (positions (potential-macro-positions form sub-forms))
	 (end 0))
    (page-template "Macroexpansion"
      (let ((parent (expanded-entry-parent expansion)))
	(when parent
	  (<:a :href (url-with-params "/paste/expand/" id entry parent)
	    "Undo")))
      (<:pre
	(dolist (pos positions)
	  (bind ((:db (sym start length path)
		      pos)
		 (next-end (+ start length))
		 (text (subseq str
			       start
			       next-end)))
	    (<:as-html (subseq str end start))
	    (if (special-operator-p sym)
		(<:span :class "special"
		  (<:as-html text))
		;; else
		(<:a :href (url-with-params "/paste/expand-action/" id entry parent path)
		  (<:as-html text)))
	    (setf end next-end)))
	(<:as-html (subseq str end)))
      (<:div :class "left"
	(labels ((colour (depth)
		   (case (mod depth 3)
		     (0
		      "red")
		     (1
		      "green")
		     (2
		      "blue")))
		 (rec (sub-form depth)
		   (<:div :class (colour depth)
		     (<:pre (<:as-html (subseq str
					       (sublist-info-start sub-form)
					       (sublist-info-end sub-form))))
		     (let ((form (form-for-path form (sublist-info-path sub-form))))
		       (when (and form
				  (listp form))
			 
			 
			 (<:p (when (symbolp (first form))
				(<:as-html (prin1-to-string (first form))
					   " "
					   (or (special-operator-p (first form))
					       (and (macro-function (first form))
						    t))))
			   " "
			   (<:as-html (type-of (first form))))))
		     (map nil (lambda (s)
				(rec s (1+ depth)))
		       (sublist-info-content sub-form)))))
	  (rec sub-forms 0)))
      (<:div :class "right"
	(<:p (<:as-html munged))))))

(web-entry expand-entry "/expand/"
    ((id :parameter-type 'integer)
     (entry :parameter-type 'integer)
     (parent :parameter-type 'integer))
  (hunchentoot:start-session)
  (with-paste-db
    (bind ((session-hash (or (hunchentoot:session-value 'pastes)
			     (setf (hunchentoot:session-value 'pastes)
				   (make-hash-table))))
	   (paste-meta (or (gethash id session-hash)
			   (setf (gethash id session-hash)
				 (make-instance 'expansion-metadata
						:package (sqlite:execute-one-row-m-v db "SELECT package FROM common_lisp_paste_packages WHERE id = ?" id)
						:entries (make-hash-table)))))
	   (package (expansion-metadata-package paste-meta))
	   (*package* (find-package package))
	   (paste-hash (expansion-metadata-entries paste-meta))
	   (entry-hash (or (gethash entry paste-hash)
			   (setf (gethash entry paste-hash)
				 (make-hash-table)))))
      (unless parent
	(unless (gethash 0 entry-hash)
	  (setf (gethash 0 entry-hash)
		(let* ((str (sqlite:execute-one-row-m-v db "SELECT paste_text FROM paste_entries WHERE paste_id = ? AND id = ?" id entry))
		       (form (read-from-string str)))
		  (make-instance 'expanded-entry
				 :form form
				 :parent nil)))
	  (setf (gethash 'next entry-hash)
		1))
	(setf parent 0))
      (aif (gethash parent entry-hash)
	   (render-expansion-page id entry parent it)
	   ;; else
	   (hunchentoot:redirect (url-with-params "/paste/expand/" id entry))))))

(web-entry expand-action "/expand-action/"
    ((id :parameter-type 'integer)
     (entry :parameter-type 'integer)
     (parent :parameter-type 'integer)
     path)
  (hunchentoot:start-session)
  (setf path
	(when path
	  (let ((*read-eval* nil)
		(path (read-from-string path)))
	    (and (listp path)
		 (every #'integerp path)
		 path))))
  (flet ((red ()
	   (hunchentoot:redirect (url-with-params "/paste/expand/" id entry))))
    (bind ((session-hash (hunchentoot:session-value 'pastes))
	   (:progn
	     (unless session-hash
	       (red)))
	   (paste-meta (gethash id session-hash))
	   (:progn
	     (unless paste-meta
	       (red)))
	   (*package* (find-package (expansion-metadata-package paste-meta)))
	   (paste-hash (expansion-metadata-entries paste-meta))
	   (entry-hash (gethash entry paste-hash))
	   (:progn
	     (unless entry-hash
	       (red)))
	   (expanded (gethash parent entry-hash)))
      (unless expanded
	(red))
      (let ((old-parent parent)
	    (parent (incf (gethash 'next entry-hash))))
	(setf (gethash parent entry-hash)
	      (make-instance 'expanded-entry
			     :parent old-parent
			     :form (expand-path (expanded-entry-form expanded) path)))
	(hunchentoot:redirect (url-with-params "/paste/expand/" id entry parent))))))

(create-with-db-macro with-logging-db db +logging-path+)

(web-entry clear-log-url "/logging/clear/"
    ()
  (when (local-network-request-p)
    (with-logging-db
      (sqlite:execute-non-query db "DELETE FROM messages")))
  (hunchentoot:redirect "/paste/logging/"))

(web-entry logging-url "/logging/"
    ((widget :parameter-type (sql-table
			      :query (:select
				       (id message kind at thread)
				       (:from messages))
			      :columns (("id" 0)
					("message"
					 ((lambda (message)
					    (<:pre (<:as-html message))
					    nil)
					  1))
					("kind" 2)
					("at" 3)
					("thread" 4))
			      :html-class "logging")))
  (page-template "Log Messages"
    (with-logging-db
      (widget db))
    (when (local-network-request-p)
      (<:p "Clear " (<:a :href "/paste/logging/clear/" "All messages")
	" "))))
