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
;;;;     :dependencies ( ... ) ;; Whatever systems are required.
;;;;
;;;; Here's what the directory structure might look like for a simple
;;;; rogue-like game. Files without a prefix can be thought of as being
;;;; loaded in an unspecified order.
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
;;;; Note: After adding, deleting or renaming a file, one must reload
;;;; the system with :force t, i.e. (asdf:load-system "my-system" :force t).

(defpackage #:alphabetical-asdf
  (:use #:cl)
  (:export "SYSTEM"))

(in-package #:alphabetical-asdf)

(defclass module (asdf:module)
  (children))

(defclass system (module asdf:system)
  ())

(defun compute-module-children (module)
  (let* ((dir (asdf:component-pathname module))
         (component-candidates
           (directory
            (merge-pathnames (make-pathname :name :wild :type :wild)
                             dir)))
         (components
           (loop :with previous = nil
                 :for pathname :in component-candidates
                 :if (asdf/driver:directory-pathname-p pathname)
                   :collect (make-instance 'module
                                           :pathname pathname
                                           :parent module
                                           :name (alexandria:last-elt
                                                  (pathname-directory pathname)))
                 :else
                   :if (let ((type (pathname-type pathname)))
                         (or (equalp "lisp" type)
                             (equalp "lsp" type)
                             (equalp "cl" type)))
                     :collect (make-instance 'asdf:cl-source-file
                                             :pathname pathname
                                             :parent module
                                             :name (pathname-name pathname))
                 :do (setf previous pathname))))

    (setf components (sort components #'string<
                           :key (alexandria:compose #'namestring
                                                    #'asdf:component-pathname)))

    (loop :for (a b) :on components :by #'cdr
          :while b
          :do (setf (asdf:component-sideway-dependencies b)
                    (list (asdf:component-name a))))

    components))

(defmethod (setf asdf:component-children) (new (module module))
  (setf (slot-value module 'children) (compute-module-children module)))

(defmethod asdf:component-children ((module module))
  (when (slot-boundp module 'children)
    (return-from asdf:component-children (slot-value module 'children)))

  (setf (slot-value module 'children) (compute-module-children module)))
