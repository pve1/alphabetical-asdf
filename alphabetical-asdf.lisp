;;;; This file defines an ASDF system class ALPHABETICAL-ASDF:SYSTEM
;;;; that automatically loads source files and directories (modules)
;;;; in alphabetical order. Therefore it is not necessary to manually
;;;; specify system components in the asd file, as they are inferred
;;;; from the directory structure and file names.
;;;;
;;;; This can come in handy when writing small utilities or prototyping.
;;;;
;;;; An ASD file should look like this:
;;;;
;;;;   (asdf:defsystem #:my-system
;;;;     :defsystem-depends-on ("alphabetical-asdf")
;;;;     :class "alphabetical-asdf:system"
;;;;     :root "src/"
;;;;     :depends-on ( ... ) ;; Whatever systems are required.
;;;;
;;;; The ROOT parameter indicates where the source files are located,
;;;; relative to the asd file. If this parameter is omitted, then it
;;;; is assumed that the source files can be found in the same
;;;; directory as the asd file.
;;;;
;;;; Here's what the directory structure might look like for a simple
;;;; rogue-like game. The names of files or directories can be
;;;; prefixed according to personal taste to enforce a specific load
;;;; order. Files without a prefix can be thought of as being loaded
;;;; in an unspecified order.
;;;;
;;;; nothack.asd (containing the above form)
;;;; A-package.lisp
;;;; B-macros.lisp
;;;; C-util.lisp
;;;; D-protocols/
;;;;     monsters.lisp
;;;;     player.lisp
;;;;     dungeon-generation.lisp
;;;; players/
;;;;     10-player-macros.lisp
;;;;     barbarian.lisp
;;;;     elf.lisp
;;;;     wizard.lisp
;;;;     valkyrie.lisp
;;;; simple-dungeon/
;;;;     10-dungeon-macros.lisp
;;;;     rooms.lisp
;;;;     shops.lisp
;;;; monsters/
;;;;     10-monster-macros.lisp
;;;;     20-monster-utils.lisp
;;;;     orc.lisp
;;;;     dragon.lisp
;;;;     gelatinous-cube.lisp
;;;;     shopkeeper.lisp
;;;;
;;;; If a file is added, deleted or renamed somewhere in the source
;;;; tree, evaluating (asdf:load-system "my-system") will rescan the
;;;; files and update the system's components accordingly.
;;;;
;;;; Tip: Instead of (asdf:defsystem #:my-system ...), you can write
;;;; (asdf:defsystem #.(pathname-name *load-pathname*) ... ) to have
;;;; the asd file's name determine the name of the system.
;;;;
;;;; Note: The component lists are not stored on disc, which means
;;;; that while deletes are detected fine within a running lisp
;;;; session, you may need to use ":force t" if you start up a fresh
;;;; image after having deleted a file that alters the compilation
;;;; output of subsequent files (e.g. a file containing macro
;;;; definitions).

(defpackage #:alphabetical-asdf
  (:use #:cl)
  (:export #:system))

(in-package #:alphabetical-asdf)

;;; ====================================================================
;;; System class

(defclass system (asdf:system)
  (%children
   (root :initarg :root :accessor system-root :initform nil)
   (file-class-name :initarg :file-class
                    :accessor file-class-name
                    :initform "asdf:cl-source-file")
   (%file-class-symbol)))

(defmethod file-class-symbol ((system system))
  (if (slot-boundp system '%file-class-symbol)
      (slot-value system '%file-class-symbol)
      (setf (slot-value system '%file-class-symbol)
            (read-from-string (file-class-name system)))))

;;; ====================================================================
;;; Scanning for components
;;;
;;; The important functions are
;;; - system-components/asdf-cache
;;; - components-changed-p/asdf-cache
;;; - component-list-equal.

(defun walk-directory (fn directory)
  (let ((files (directory (merge-pathnames
                           (make-pathname :name :wild
                                          :type :wild)
                           directory))))
    (dolist (file files)
      (if (asdf/driver:directory-pathname-p file)
          (walk-directory fn file)
          (funcall fn file)))))

(defun lisp-file-p (file)
  (member (pathname-type file)
          '("lisp" "lsp" "cl")
          :test #'equalp))

(defun file-to-component (system file)
  (make-instance (file-class-symbol system)
                 :pathname file
                 :parent system
                 :name (namestring
                        (merge-pathnames
                         (make-pathname :type :unspecific)
                         (enough-namestring
                          file
                          (asdf:component-pathname system))))))

;;; Sorted list of files that belong to the system.
(defun system-files (system)
  (let* ((files nil)
         (component-pathname (asdf:component-pathname system))
         (root (if (system-root system)
                   (merge-pathnames (system-root system)
                                    component-pathname)
                   component-pathname)))
    (unless (probe-file root)
      (error "System root does not exist (~S)." root))
    (walk-directory (lambda (file)
                      (when (lisp-file-p file)
                        (push file files)))
                    root)
    (sort files #'string< :key #'namestring)))

;;; Sorted list of source-file components with serial dependencies
;;; added.
(defun system-components (system)
  (let ((components (mapcar (lambda (file)
                              (file-to-component system file))
                            (system-files system))))

    ;; ":serial t"
    (loop :for (a b) :on components :by #'cdr
          :while b
          :do (setf (asdf:component-sideway-dependencies b)
                    (list (asdf:component-name a))))
    components))

(defun component-list-equal (component-list-1 component-list-2)
  (and (= (length component-list-1)
          (length component-list-2))
       (loop :for c1 :in component-list-1
             :for c2 :in component-list-2
             :always (equal (asdf:component-pathname c1)
                            (asdf:component-pathname c2)))))

;;; Have the source files changed compared to what was loaded last?
(defun components-changed-p (system)
  (let ((current-components (slot-value system '%children))
        (components-according-to-filesystem
          (system-components/asdf-cache system)))
    (not (component-list-equal current-components
                               components-according-to-filesystem))))

;;; SYSTEM-COMPONENTS consulted from the ASDF session cache (so it
;;; only gets computed once per ASDF session).
(defun system-components/asdf-cache (system)
  (if asdf/session:*asdf-session*
      (asdf/session:consult-asdf-cache
       (list 'system-components system)
       (lambda () (system-components system)))
      (system-components system)))

;;; Same as above but for COMPONENTS-CHANGED-P.
(defun components-changed-p/asdf-cache (system)
  (if asdf/session:*asdf-session*
      (asdf/session:consult-asdf-cache
       (list 'components-changed-p system)
       (lambda () (components-changed-p system)))
      (components-changed-p system)))

;;; ====================================================================
;;; ASDF methods

(defmethod (setf asdf:component-children) (new (system system))
  (unless (slot-boundp system '%children)
    (setf (slot-value system '%children)
          (system-components/asdf-cache system)))
  new)

(defmethod asdf:component-children ((system system))
  (if (slot-boundp system '%children)
      (slot-value system '%children)
      (setf (slot-value system '%children)
            (system-components/asdf-cache system))))

(defmethod asdf:operation-done-p ((op asdf:define-op) (system system))
  (if (slot-boundp system '%children)
      ;; If components have changed, we are *not* done.
      (if (components-changed-p/asdf-cache system)
          nil
          (call-next-method))
      nil))
