(eval-when (:compile-toplevel :load-toplevel :execute)
  #-clx
  (ql:quickload 'clx)
  #-zpng
  (ql:quickload 'zpng)
  #-cffi
  (ql:quickload 'cffi))

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
           #:x-get-color
           #:x-find-color))

(in-package  #:cl-autogui)

(defparameter *image-pathname*
  "/home/sonja/repo/org/cl-dino-master/life.jpg")
(defparameter *default-heght* 670)
(defparameter *default-width* 1295)
(defparameter *default-x* 60)
(defparameter *default-y* 37)
(defparameter *image-array* nil)
(defparameter *mouse-left* 1)
(defparameter *mouse-middle* 2)
(defparameter *mouse-right* 3)

;; ;; тест получения скриншота
(block test-snap
  (perform-mouse-action t *mouse-left* :x 30 :y 450)
  (sleep .1)
  (perform-mouse-action nil *mouse-left* :x 30 :y 450)
  (sleep 1)
  ;; получаем скрин заданной области + заполненный массив *image-array*
  (x-snapshot :x 440 :y 380 :width 900 :height 350 :path "snap.png"))

;; определяем размеры тизера
(block test-border
;; пипеткой взято rgb бордюра = 241 200 70
;; тест получения значения кокретного пикселя:
;; передача параметров: Y, X, (цвет). 0 - доступ к зачению R, 1 - G, 2 - B

  (defun crop-teaser (image-array)
    (multiple-value-bind (border-height border-width x-begin y-begin)
        (find-border image-array)
      (let ((proc (sb-ext:run-program
                   "/usr/lib/i386-linux-gnu/ImageMagick-6.8.9/bin-Q16/convert"
                   `("-crop"
                     ,(format nil "~Ax~A+~A+~A"
                              border-width border-height
                              x-begin y-begin)
                     "/home/sonja/repo/org/cl-dino-master/snap.png"
                     "/home/sonja/repo/org/cl-dino-master/test.png")
                   :input :stream :output :stream)))
        (if proc
            (with-open-stream (input (sb-ext:process-input proc))
              (with-open-stream (output (sb-ext:process-output proc))
                (do ((a-line (read-line output nil 'eof)
                             (read-line output nil 'eof)))
                    ((eql a-line 'eof))
                  (format t "~A" a-line)
                  (force-output output))))
            (format t "~% didn't run imageMagic")))))

(defun find-border (image-array)
  ;; получаем первую точку границы тизера (левый верхний угол)
  (multiple-value-bind (x-begin y-begin)
      (first-point image-array)
    ;; получаем ширину и высоту тизера
    (multiple-value-bind (border-height border-width)
        (get-size x-begin y-begin image-array)
      (values border-height border-width x-begin y-begin))))

(defun first-point (image-array)
  (do ((x 0 (+ x 1)))
      ((= x 900))
    (do ((y 0 (+ y 1)))
        ((= y 350))
      (if (and (eql 241 (aref image-array y x 0))
               (eql 200 (aref image-array y x 1))
               (eql 70 (aref image-array y x 2)))
          (return-from first-point (values x y))))))

(defun get-size (x y image-array)
  (let ((height 0) (width 0))
    (do ((i y (+ i 1)))
        ((= i 350) height)
      (if (and (eql 241 (aref image-array i x 0))
               (eql 200 (aref image-array i x 1))
               (eql 70 (aref image-array i x 2)))
          (setf height (+ height 1))))
    (do ((i x (+ i 1)))
        ((= i 900) (values height width))
      (if (and (eql 241 (aref image-array y i 0))
               (eql 200 (aref image-array y i 1))
               (eql 70 (aref image-array y i 2)))
          (setf width (+ width 1))))))

(crop-teaser *image-array*))

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

(defun x-move (x y)
  (if (and (integerp x) (integerp y))
      (with-default-display-force (d)
        (xtest:fake-motion-event d x y))
      (error "Integer only for position, (x: ~S, y: ~S)" x y)))

(defun mklist (obj)
  (if (and
       (listp obj)
       (not (null obj)))
      obj (list obj)))

(defmacro defun-with-actions (name params actions &body body)
  ;;     "This macro defun a function which witch do mouse or keyboard actions,
  ;; body is called on each action."
  `(defun ,name ,params
     (mapcar
      #'(lambda (action)
          ,@body)
      (mklist ,actions))))

(defun perform-mouse-action (press? button &key x y)
  (and x y (x-move x y))
  (with-default-display-force (d)
    (xtest:fake-button-event d button press?)))

(macrolet ((def (name actions)
             `(defun-with-actions ,name
                  (&key (button 1) x y)
                  ,actions
                (funcall #'perform-mouse-action
                         action button :x x :y y))))
  (def x-mouse-down t)
  (def x-mouse-up nil)
  (def x-click '(t nil))
  (def x-dbclick '(t nil t nil)))

(defmacro with-scroll (pos neg clicks x y)
  `(let ((button (cond
                   ((= 0 ,clicks) nil)
                   ((> 0 ,clicks) ,pos) ; scroll up/right
                   ((< 0 ,clicks) ,neg)))) ; scroll down/left
     (dotimes (_ (abs ,clicks))
       (x-click :button button :x ,x :y ,y))))

(defun x-vscroll (clicks &key x y)
  (with-scroll 4 5 clicks x y))

(defun x-scroll (clicks &key x y)
  (x-vscroll clicks :x x :y y))

(defun x-hscroll (clicks &key x y)
  (with-scroll 7 6 clicks x y))

(defun perform-key-action (press? keycode) ; use xev to get keycode
  (with-default-display-force (d)
    (xtest:fake-key-event d keycode press?)))

(macrolet ((def (name actions)
             `(defun-with-actions ,name (keycode)
                  ,actions
                (funcall #'perform-key-action
                         action keycode))))
  (def x-key-down t)
  (def x-key-up nil)
  (def x-press '(t nil)))

(defun raw-image->png (data width height)
  (let ((png (make-instance 'zpng:png :width width :height height
                             :color-type :truecolor-alpha
                             :image-data data)))
         (setf *image-array* (zpng:data-array png))
    (dotimes (y height)
      (dotimes (x width)
        ;; BGR -> RGB, ref code: https://goo.gl/slubfW
        ;; diffs between RGB and BGR: https://goo.gl/si1Ft5
        (rotatef (aref *image-array* y x 0) (aref *image-array* y x 2))
        (setf (aref *image-array* y x 3) 255)))
    png))

(multiple-value-bind (default-width default-height) (x-size)

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
  )

;;
;; ;; тест конвертирования изображения
;; (block test
;;   (let ((proc (sb-ext:run-program "/usr/lib/i386-linux-gnu/ImageMagick-6.8.9/bin-Q16/convert"
;;                                   '("-resize"
;;                                     "2000X416"
;;                                     "/home/sonja/repo/org/cl-dino-master/test.png"
;;                                     "/home/sonja/repo/org/cl-dino-master/test-src.png")
;;                                   :input :stream :output :stream)))
;;     (if proc
;;         ;; открываем поток тесеракта на чтение
;;         (with-open-stream (input (sb-ext:process-input proc))
;;           ;; открыли поток тесеракта на запись
;;           (with-open-stream (output (sb-ext:process-output proc))
;;             ;; принудительно очищаем input
;;             ;; (force-output input)
;;             ;; (format file "~A" (read input))))
;;             (do ((line (read-line output nil 'eof)
;;                        (read-line output nil 'eof)))
;;                 ((eql line 'eof))
;;               (format t "~A" line)
;;               (force-output output))))
;;         (format t "~% didn't run imageMagic"))))

;; ;; тест тесеракта
;; (block test
;;   (defparameter *out-pathname* (make-pathname :name "out.txt"))

;;   (defmacro with-run-vfm ()
;;     ;; открываем файл на запись
;;     `(with-open-file (file *out-pathname* :direction :output
;;                            :if-exists :supersede)
;;        ;; запускаем тесеракт
;;        (let ((proc (sb-ext:run-program "/usr/bin/tesseract"
;;                                        '("/home/sonja/repo/org/cl-dino-master/life.jpg"
;;                                          "/home/sonja/repo/org/cl-dino-master/out")
;;                                        :input :stream :output :stream)))
;;          (if proc
;;              ;; открываем поток тесеракта на чтение
;;              (with-open-stream (input (sb-ext:process-input proc))
;;                ;; открыли поток тесеракта на запись
;;                (with-open-stream (output (sb-ext:process-output proc))
;;                  ;; принудительно очищаем input
;;                  ;; (force-output input)
;;                  ;; (format file "~A" (read input))))
                 ;; (do ((line (read-line output nil 'eof)
                 ;;            (read-line output nil 'eof)))
                 ;;     ((eql line 'eof))
                 ;;   (format t "~A" line)
                 ;;   (force-output output))))
;;              (format t "~% didn't run tesseract")))))

;;   (with-run-vfm))
