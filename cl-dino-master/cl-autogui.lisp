
(eval-when (:compile-toplevel :load-toplevel :execute)
  #-clx
  (ql:quickload 'clx)
  #-zpng
  (ql:quickload 'zpng)
  #-cffi
  (ql:quickload 'cffi)
  (ql:quickload "png-read")
)

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
(defparameter *image-amount* 0)
(defparameter *image-indx* 0)
(defparameter *image-pathname*
  "/home/sonja/repo/org/cl-dino-master/test~A.png")
(defparameter *out-text* "/home/sonja/repo/org/cl-dino-master/out~A")
(defparameter *langs* "rus+eng")
(defparameter *default-height* 668)
(defparameter *default-width* 1295)
(defparameter *teaser-width* 690)
(defparameter *snap-width* 755)
(defparameter *snap-height* 670)
(defparameter *snap-x* 440)
(defparameter *default-x* 60)
(defparameter *default-y* 37)
(defparameter *image-array* nil)
(defparameter *mouse-left* 1)
(defparameter *mouse-middle* 2)
(defparameter *mouse-right* 3)
(defparameter *R-usual* 231)
(defparameter *G-usual* 231)
(defparameter *B-usual* 231)
(defparameter *R-priority* 241)
(defparameter *G-priority* 200)
(defparameter *B-priority* 70)
(defparameter *hh-teaser-url*
  "https://hh.ru/search/vacancy?L_is_autosearch=false&area=2&clusters=true&enable_snippets=true&items_on_page=100&only_with_salary=true&salary=165000&specialization=1.221&page=~A"
  "https://spb.hh.ru/search/vacancy?L_is_autosearch=false&area=1&clusters=true&enable_snippets=true&items_on_page=100&only_with_salary=true&salary=165000&specialization=1.221&page=~A")

(defparameter *browser-path*  "/usr/bin/firefox")

;; план:
;; - скрин
;; - нашли тизер
;; - двигаемся к правому верхнему углу (+ x teaser-width)
;; - область от бордюра фонового цвета? (+ y 40) (- x 40)
;; да! вырезаем , отправляем в тесеракт, результат записываем в зп
;; нет! записываем 0 в зп
;; - от заголовка вакансии двигаемся к источнику
;; - повторяем алгоритм как с зп
;; - копируем ссылку на вакансию из буфера обмена, записываем в id
;; - вырезаем заголовок вакансии, копируем в поле структуры
;; записываем полученную вакансию в хэш-таблицу, где ключ - id вакансии

;; область зп, нижний левый угол:
;; (+ 60 по Y от верхней границы тизера, высота) (- 220 от правого верхнего угла, ширина)
;; область заголовка:
;; (+ 70 к Х от верхнего левого угла) (+ 35 к Y от верхнего левого угла)
;; область источника:
;; (+ 25 к Y от заголовка) X тот же

;; (block test
;;   (defparameter *image-indx* 0)
;;   (perform-mouse-action t *mouse-left* :x 30 :y 450)
;;   (sleep .1)
;;   (perform-mouse-action nil *mouse-left* :x 30 :y 450)
;;   (sleep 1)
;;   (start))


(defun start ()
  (do ((i 0 (+ 1 i)))
      ((= i 6))
    (perform-key-action t 116)
    (perform-key-action nil 116))
  (sleep 1)
  (get-images)
  (sleep 5)
  (setf *image-amount* (*image-amount* -1))
  (do ((i 0 (+ i 1)))
      ((= i *image-amount*))
    (resize `,(format nil
                      "/home/sonja/repo/org/cl-dino-master/test~A.png"
                      i))
    (sleep 1)
    (run-tess `,(format nil
                        "/home/sonja/repo/org/cl-dino-master/test~A.png"
                        i) `,(format nil
                                     "/home/sonja/repo/org/cl-dino-master/out~A"
                                     i) *langs*)))

(defun get-images ()
  (let ((crop-marker) (cnt 4))
    (do ((j 0 (+ j 1)))
        (nil)
      (do ((j 0 (+ j 1)))
          ((= j cnt))
        (perform-key-action t 116)
        (perform-key-action nil 116))
      (sleep 1)
      (x-snapshot :x 440 :width  *snap-width*
                  :path `,(format nil "test~A.png" *image-indx*))
      (sleep 1)
      (setf crop-marker
            (crop-teaser *image-array*
                         `,(format nil
                                   "/home/sonja/repo/org/cl-dino-master/test~A.png"
                                   *image-indx*)))
      (if (eql crop-marker -2)
          (progn
            (format t "~% get-images: no teasers anymore")
            (return-from get-images 1))

          (if (eql crop-marker -1)
              ;; встретили блок рекламы, пропускаем, не анализируя
              (setf cnt 2)
              (progn
                (setf *image-indx* (+ *image-indx* 1))
                (setf cnt 4)))))))
;;---------------------------------------------------------------------------------------
;; принимает 2 массива изображений и пограничные координаты для поиска, т.е. ряд пикселей,
;; на котором поиск совпадающих рядов закончится
(defun find-row (image-up image-down y-border height)
  (do ((cur-y (- height 1) (- cur-y 1)))
      (( = cur-y y-border))
    (if (check-row image-up image-down cur-y 0 *snap-width*)
        (return-from find-row cur-y))))

;; проверяет 2 ряда пикселей на совпадение
;; если проверены все, возвращает Y координату
;; если хоть один не совпал - nil
(defun check-row (image-up image-down image-up-y image-down-y width-image)
  (do ((i 0 (+ i 1)))
      (( = i width-image) image-up-y)
    ;; если цвет писелей не одинаковый
    (if (not
         (and (eql (aref image-up image-up-y i 0)
                   (aref image-down image-down-y i 0))
              (eql (aref image-up image-up-y i 1)
                   (aref image-down image-down-y i 1))
              (eql (aref image-up image-up-y i 2)
                   (aref image-down image-down-y i 2))))
        ;; прерываем поиск и возвращаем nil
        (return-from check-row nil)
        )))

;; (block find-row-test
;;   (defparameter *image-indx* 1)
;;   (perform-mouse-action t *mouse-left* :x 30 :y 450)
;;     (sleep .1)
;;     (perform-mouse-action nil *mouse-left* :x 30 :y 450)
;;     (sleep 1)
;;     (x-snapshot :x 440 :y 100 :width *snap-width* :height 668
;;                 :path `,(format nil "/home/sonja/repo/org/cl-dino-master/test~A.png"
;;                                 *image-indx*))
;;     (setf *image-indx* (+ *image-indx* 1))
;;     (let ((image-array-old *image-array*))
;;             (perform-key-action t 117)
;;       (sleep .1)
;;       (perform-key-action nil 117)
;;       (sleep 1)
;;       (x-snapshot :x 440 :y 100 :width *snap-width* :height 668
;;                   :path `,(format nil "/home/sonja/repo/org/cl-dino-master/test~A.png"
;;                                   *image-indx*))
;;       (sleep 5)
;;       (find-row image-array-old *image-array* 300 668)))

;;-------------------------------------------------------------------------------------
(defun run-tess (input-image output-text &optional (langs "eng"))
  (let ((proc (sb-ext:run-program "/usr/bin/tesseract"
                                  ;; `(,input-image ,output-text "-l" ,langs)
                                  ;; :input :stream :output :stream)))
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
              (format t "~% ~A" line)
              (force-output output))))
        (format t "~% didn't run tesseract"))))

(defun resize (image)
  (let ((proc (sb-ext:run-program
               "/usr/lib/i386-linux-gnu/ImageMagick-6.8.9/bin-Q16/mogrify"
               `("-resize"
                 "1500x502"
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
    (if (eql border-height 0)
        ;; нулевая высота, встретили блок рекламы
        (progn
          (format t "~% crop -1")
          (return-from crop-teaser -1))
        (if (and (null x-begin) (null y-begin))
            ;; вообще не нашли бордюр, тизеры кончились
            (progn
              (format t "~% crop -2")
              (return-from crop-teaser -2))
            (progn
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
                          (force-output output))
                        ;; 0 = успех
                        (setf *image-amount* (+ *image-amount* 1))
                        (return-from crop-teaser 0)))
                    (format t "~% crop-teaser: didn't run ImageMagic"))))))))

(defun find-border (image-array)
  ;; ищем левый верхний угол у вакансии (сначала ищем выделенные)
  (multiple-value-bind (x-begin y-begin)
      (first-point image-array *R-priority* *G-priority* *B-priority*
                   *snap-height* *snap-width*)
    ;; нашли?
    (if (and x-begin y-begin)
        ;; да
        ;; получаем высоту тизера
        (multiple-value-bind (border-height)
            (get-size x-begin y-begin image-array
                      *R-priority* *G-priority* *B-priority*)
          (values border-height x-begin y-begin))
        ;; нет
        ;; ищем левый верхний угол у обычной вакансии
        (multiple-value-bind (x-begin y-begin)
            (first-point image-array *R-usual* *G-usual* *B-usual*
                         *snap-height* *snap-width*)
          ;; нашли?
          (if (and x-begin y-begin)
              ;; да
              ;; проверяем, точно ли это тизер
              (progn
                (format t "~% usual")
                (if (check-width x-begin y-begin *image-array* *teaser-width*
                                 *snap-width* *R-usual* *G-usual* *B-usual*)
                    ;; да, это тизер
                    (multiple-value-bind (border-height)
                        (get-size x-begin y-begin image-array
                                  *R-usual* *G-usual* *B-usual*)
                      (values border-height x-begin y-begin))
                    ;; нет, это другой элемент
                    (progn
                      (format t "~% find-border: didn't find the border")
                      (values nil nil nil))))
              ;; не нашли даже даже точку вхождения в бордюр
              (progn
                (format t "~% find-border: didn't find the first point")
                (values nil nil nil)))))))

;; ищем верхний левый угол тизера
;; если текущий пиксель = цвет бордюра и следующий за ним справа имеет этот же цвет,
;; точка найдена
(defun first-point (image-array r g b image-height image-width)
  (do ((x 0 (+ x 1)))
      ((= x image-width))
    (do ((y 0 (+ y 1)))
        ((= y image-height))
      (if (and (eql r (aref image-array y x 0))
               (eql g (aref image-array y x 1))
               (eql b (aref image-array y x 2))
               (eql r (aref image-array y (+ x 1) 0))
               (eql g (aref image-array y (+ x 1) 1))
               (eql b (aref image-array y (+ x 1) 2)))
          (return-from first-point (values x y)))))
  ;; если после выполнения циклов точка не найдена
  (return-from first-point (values nil nil)))


;; ищем высоту
(defun get-height (x y image-array r g b)
  (format t "~% x ~A y ~A" x y)
  (let ((height 0) (cnt (- *default-heght* 1)))
    (do ((i (+ y 1) (+ i 1)))
        ((= i cnt) height)
      ;; если пиксель экрана имеет цвет бордюра
      (if (and (eql r (aref image-array i x 0))
               (eql g (aref image-array i x 1))
               (eql b (aref image-array i x 2)))
          ;; а соседний справа не имеет
          (if  (and (not (eql r (aref image-array i (+ x 1) 0)))
                    (not (eql g (aref image-array i (+ x 1) 1)))
                    (not (eql b (aref image-array i (+ x 1) 2))))
               ;; увеличиваем высоту
               (setf height (+ height 1))
               ;; в противном случае мы достигли нижней границы = левый нижний угол
               (return-from get-height height))))))

(defun check-width (x y image-array teaser-width image-width r g b)
  (let ((width 0))
    (format t "~% x ~A y ~A" x y)
    (do ((i x (+ i 1)))
        ((= i (- image-width 1)) width)
      (if (and (eql r (aref image-array y i 0))
               (eql g (aref image-array y i 1))
               (eql b (aref image-array y i 2)))
          (setf width (+ width 1))))
    (if (eql width teaser-width)
        (progn
          ;;(format t "~% width ~A" width)
          (values width))
        (format t "~% check-width: it's not a teaser"))))


(defun open-browser (browser-path url)
  (let ((proc (sb-ext:run-program
               `,browser-path
               `(,url)
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



;; ---------------------------------------

;;_______________________________________________________________________
(defun vectorize-image (image)
  "Превращает массив size-X столбцов по size-Y точек в линейный,
   где сначала идут все X-точки нулевой строки, потом первой, итд"
  (let ((idx 0)
        (result (make-array (reduce #'* (array-dimensions image))
                            :element-type '(unsigned-byte 8))))
    (loop for dx from 0 to (- (array-dimension image 1) 1) :do
         (loop for dy from 0 to (- (array-dimension image 0) 1) :do
              (loop for dz from 0 to (- (array-dimension image 2) 1) :do
                   (setf (aref result idx)
                         (aref image dy dx dz))
                   (incf idx))))
    result))

;; (print (vectorize-image *test-image*))

(defun load-png (pathname-str)
  "Возвращает массив size-X столбцов по size-Y точек,
   где столбцы идут слева-направо, а точки в них - сверху-вниз"
  (png-read:image-data
   (png-read:read-png-file pathname-str)))

(defun save-png (width height pathname-str image)
  (let* ((png (make-instance 'zpng:png :width width :height height
                             :color-type :truecolor-alpha
                             :image-data image)))
    (zpng:write-png png pathname-str)))

;;________________________________________________________________________


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
