(ql:quickload '(:drakma :yason))

(defun  stream->json (stream)
  (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
  (yason:parse stream :object-key-fn #'alexandria:make-keyword :object-as :plist))

(defun save-url-to-file (url file)
  (unless (uiop:file-exists-p file)
    (with-open-file (of file
			:direction :output
			:element-type '(unsigned-byte 8)
			:if-exists :supersede)
      (handler-case (write-sequence (drakma:http-request url :force-binary t) of)
	(t () (format t "~% Error Downloading ~a" url))))))

(defclass page ()
  ((pid :initarg :pid :accessor pid)
   (src :initarg :src :accessor src :initform nil)
   (file :initarg :file :accessor file :initform nil)
   (bookid :initarg :bookid :accessor bookid)))

(defun collect-pagelist (response &key hash-table bookid)
  (loop for p in (getf response :|page|)
     for pid = (getf p :|pid|)
     for page = (gethash pid hash-table nil) 
     collect (progn (when (or (not page) (not (src page)))
		      (setf (gethash pid hash-table)
			    (make-instance 'page :pid pid
					   :src (getf p :|src| nil)
					   :bookid bookid)))
		    pid)))


(defun ensure-src (page &optional pagedict bookid)
  (let ((stream (drakma:http-request (format nil "http://books.google.com.sg/books?id=~a&pg=~a&zoom=3&jscmd=click3"
					     (bookid page)
					     (pid page))
				     :want-stream t)))
    (collect-pagelist (stream->json stream) :hash-table pagedict :bookid bookid)))

(defun download-page (page location)
  (if (src page)
      (progn
	(format t "Downloading ~a~%" (pid page))
	(save-url-to-file (format nil "~a&w=1280" (src page)) (merge-pathnames (format nil "~a.png" (pid page)) location)))
      (format t "No src. Can't download ~a~%" (pid page))))

(defun download-book (bookid location) 
  (let ((stream (drakma:http-request (format nil "http://books.google.com.sg/books?id=~a&pg=1&jscmd=click3" bookid)
				     :want-stream t))
	(pagedict (make-hash-table :test 'equal))
	pids)
    (setf pids (collect-pagelist (stream->json stream)
				 :hash-table pagedict
				 :bookid bookid))
    (ensure-directories-exist location)
    (loop for pid in pids
       for page = (gethash pid pagedict) do
	 (ensure-src page pagedict bookid)
	 (download-page page location))))

    
      
