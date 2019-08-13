(ql:quickload '(clx zpng))

(defpackage #:cl-autogui
  (:use #:common-lisp #:xlib)
  (:export #:x-position
           #:x-size
           #:x-position
           #:x-move
           #:x-mouse-down
           #:x-mouse-up
           #:x-click
           #:x-dbclick
           #:x-vscroll
           #:x-hscroll
           #:x-scroll
           #:x-key-down
           #:x-key-up
           #:x-press
           #:x-snapshot
           #:x-snapsearch
           #:x-getcolor))

(in-package #:cl-autogui)

(defparameter *default-heght* 670)
(defparameter *default-x* 60)
(defparameter *default-y* 37)
(defparameter *default-width* 1295)
(defmacro with-display (host (display screen root-window) &body body)
  `(let* ((,display (xlib:open-display ,host))
          (,screen (first (xlib:display-roots ,display)))
          (,root-window (xlib:screen-root ,screen)))
     (unwind-protect (progn ,@body)
       (xlib:close-display ,display))))

(defmacro with-default-display ((display &key (force nil)) &body body)
  `(let ((,display (open-default-display)))
     (unwind-protect
          (unwind-protect
               ,@body
            (when ,force
              (display-force-output ,display)))
       (close-display ,display))))

(defmacro with-default-display-force ((display) &body body)
  `(with-default-display (,display :force t) ,@body))

(defmacro with-default-screen ((screen) &body body)
  (let ((display (gensym)))
    `(with-default-display (,display)
       (let ((,screen (display-default-screen ,display)))
         ,@body))))

(defmacro with-default-window ((window) &body body)
  (let ((screen (gensym)))
    `(with-default-screen (,screen)
       (let ((,window (screen-root ,screen)))
         ,@body))))

(defun x-size ()
  (with-default-screen (s)
    (values
     (screen-width s)
     (screen-height s))))

(defun raw-image->png (data width height)
  (let* ((png (make-instance 'zpng:png :width width :height height
                             :color-type :truecolor-alpha
                             :image-data data))
         (data (zpng:data-array png)))
    (dotimes (y height)
      (dotimes (x width)
        ;; BGR -> RGB, ref code: https://goo.gl/slubfW
        ;; diffs between RGB and BGR: https://goo.gl/si1Ft5
        (rotatef (aref data y x 0) (aref data y x 2))
        (setf (aref data y x 3) 255)))
    png))

(defun x-snapshot (&key (x *default-x*) (y *default-y*)
                       (width  *default-width*) (height *default-heght*)
                       (delay 0)
                        path)
;;         "Return RGB data array (The dimensions correspond to the height, width,
;; and pixel components, see comments in x-snapsearch for more details),
;; or write to file (PNG only), depend on if you provide the path keyword"
        (sleep delay)
        (with-default-window (w)
          (let ((image
                 (raw-image->png
                  (get-raw-image w :x x :y y
                                 :width width :height height
                                 :format :z-pixmap)
                  width height)))
            (if path
                (let* ((ext (pathname-type path))
                       (path
                        (if ext
                            path
                            (concatenate 'string path ".png")))
                       (png? (or (null ext) (equal ext "png"))))
                  (cond
                    (png? (zpng:write-png image path))
                    (t (error "Only PNG file is supported"))))
                (zpng:data-array image)))))

(block test
  (with-display "" (display screen root-window)
  (x-snapshot :path "snap.png")))
