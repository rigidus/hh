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
(defparameter *image-indx* 0)
(defparameter *image-pathname*
  "/home/sonja/repo/org/cl-dino-master/test.jpg")
(defparameter *out-text* "/home/sonja/repo/org/cl-dino-master/out")
(defparameter *langs* "rus+eng")
(defparameter *default-heght* 670)
(defparameter *default-width* 1295)
(defparameter *teaser-width* 690)
(defparameter *default-x* 60)
(defparameter *default-y* 37)
(defparameter *image-array* nil)
(defparameter *mouse-left* 1)
(defparameter *mouse-middle* 2)
(defparameter *mouse-right* 3)

;; тест цикла:
;; переключиться в окно браузера -> сделать скрин ->
;; обрезать изображение ровно до размера тизера -> увеличить полученный тизер
;; передать изображение в тесеракт
(block test
  (defparameter *image-indx* 0)
  (perform-mouse-action t *mouse-left* :x 30 :y 450)
  (sleep .1)
  (perform-mouse-action nil *mouse-left* :x 30 :y 450)
  (sleep 1)
  (do ((i 0 (+ 1 i)))
      ((= i 6))
    (perform-key-action t 116)
    (perform-key-action nil 116))
  (sleep 1)
  (do ((i 0 (+ 1 i)))
      ((= i 7))
    (do ((j 0 (+ j 1)))
        ((= j 4))
      (perform-key-action t 116)
      (perform-key-action nil 116))
    (x-snapshot :path `,(format nil "test~A.png" *image-indx*))
    (sleep 1)
    (first-point *image-array*)
    (crop-teaser *image-array*
                 `,(format nil "/home/sonja/repo/org/cl-dino-master/test~A.png"
                           *image-indx*))
    (setf *image-indx* (+ *image-indx* 1))))

(defun append-image (image-pathname)
  (let ((proc (sb-ext:run-program
               "/usr/lib/i386-linux-gnu/ImageMagick-6.8.9/bin-Q16/convert"
               `("-append"
                 "*.png"
                 ,image-pathname)
               :input :stream :output :stream)))
    (if proc
        (with-open-stream (input (sb-ext:process-input proc))
          (with-open-stream (output (sb-ext:process-output proc))
            (do ((a-line (read-line output nil 'eof)
                         (read-line output nil 'eof)))
                ((eql a-line 'eof))
              (format t "~A" a-line)
              (force-output output))))
        (format t "~% resize: didn't run ImageMagic"))))

;;   (resize  *image-pathname*)
;;   (run-tess *image-pathname* *out-text* *langs*))

;; проблема скроллинга.
;; Вариант 1:
;; у необработанного изображения запоминаем RGB, скролим до тех пор, пока этот ряд не
;; окажется на самом верху. Недостаток: сравнивать 2000+ пикселей + перебор всего массива
;; на каждой итерации + возможность пропустить, когда нижний ряд окажется сверху, тогда
;; промотаем всю страницу
;; Вариант 2:
;; у блоков есть нижняя граница. Скролим до тех пор, пока не увидим еще одну нижнюю
;; границу. Тогда скриним от текущей нижней границы до следующей. Недостаток: как
;; определить, что это именно следующая нижняя граница, а не текущая?
;; Варинт 3:
;; есть размер текущего блока. Смещаем Y на высоту блока. (Построчный скролинг..)

(defun run-tess (input-image output-text &optional (langs "eng"))
  (let ((proc (sb-ext:run-program "/usr/bin/tesseract"
                                  `(,input-image ,output-text "-l" ,langs)
                                  :input :stream :output :stream)))
    (if proc
        ;; открываем поток тесеракта на чтение
        (with-open-stream (input (sb-ext:process-input proc))
          ;; открыли поток тесеракта на запись
          (with-open-stream (output (sb-ext:process-output proc))
            (do ((line (read-line output nil 'eof)
                       (read-line output nil 'eof)))
                ((eql line 'eof))
              (format t "~A" line)
              (force-output output))))
        (format t "~% didn't run tesseract"))))

(defun resize (image)
  (let ((proc (sb-ext:run-program
               "/usr/lib/i386-linux-gnu/ImageMagick-6.8.9/bin-Q16/mogrify"
               `("-resize"
                 "3000x624"
                 ,image)
               :input :stream :output :stream)))
    (if proc
        (with-open-stream (input (sb-ext:process-input proc))
          (with-open-stream (output (sb-ext:process-output proc))
            (do ((a-line (read-line output nil 'eof)
                         (read-line output nil 'eof)))
                ((eql a-line 'eof))
              (format t "~A" a-line)
              (force-output output))))
        (format t "~% resize: didn't run ImageMagic"))))

(defun crop-teaser (image-array image-path)
  (format t "~% crop")
  (multiple-value-bind (border-height x-begin y-begin)
      (find-border image-array)
    (format t "~% ~Ax~A+~A+~A"
            *teaser-width* border-height
            x-begin y-begin)
    (let ((proc (sb-ext:run-program
                 "/usr/lib/i386-linux-gnu/ImageMagick-6.8.9/bin-Q16/mogrify"
                 `("-crop"
                   ,(format nil "~Ax~A+~A+~A"
                            *teaser-width* border-height
                            x-begin y-begin)
                   ,image-path)
                 :input :stream :output :stream)))
      (if proc
          (with-open-stream (input (sb-ext:process-input proc))
            (with-open-stream (output (sb-ext:process-output proc))
              (do ((a-line (read-line output nil 'eof)
                           (read-line output nil 'eof)))
                  ((eql a-line 'eof))
                (format t "~A" a-line)
                (force-output output))))
          (format t "~% crop-teaser: didn't run ImageMagic")))))

(defun find-border (image-array)
  ;; получаем первую точку границы тизера (левый верхний угол)
  (multiple-value-bind (x-begin y-begin)
      (first-point image-array)
    (if (and x-begin y-begin)
    ;; получаем ширину и высоту тизера
    (multiple-value-bind (border-height)
        (get-size x-begin y-begin image-array)
      (values border-height x-begin y-begin))
    (format t "~% find-border: didn't find the border"))))

;; ищем верхний левый угол тизера
;; если текущий пиксель = цвет бордюра и следующий за ним справа имеет этот же цвет,
;; точка найдена
(defun first-point (image-array)
  (do ((x 0 (+ x 1)))
      ((= x 1294))
    (do ((y 0 (+ y 1)))
        ((= y 669))
      (if (and (eql 241 (aref image-array y x 0))
               (eql 200 (aref image-array y x 1))
               (eql 70 (aref image-array y x 2))
               (eql 241 (aref image-array y (+ x 1) 0))
               (eql 200 (aref image-array y (+ x 1) 1))
               (eql 70 (aref image-array y (+ x 1) 2)))
          (return-from first-point (values x y))))))

;; ищем высоту
(defun get-size (x y image-array)
  (format t "~% x ~A y ~A" x y)
  (let ((height 0) (cnt (- *default-heght* 1)))
    (do ((i (+ y 1) (+ i 1)))
        ((= i cnt) height)
      ;; если пиксель экрана имеет цвет бордюра
      (if (and (eql 241 (aref image-array i x 0))
               (eql 200 (aref image-array i x 1))
               (eql 70 (aref image-array i x 2)))
          ;; а соседний справа не имеет
          (if  (and (not (eql 241 (aref image-array i (+ x 1) 0)))
                    (not (eql 200 (aref image-array i (+ x 1) 1)))
                    (not (eql 70 (aref image-array i (+ x 1) 2))))
               ;; увеличиваем высоту
                 (setf height (+ height 1))
               ;; в противном случае мы достигли нижней границы = левый нижний угол
               (return-from get-size height))))))


;; ---------------------------------------


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
