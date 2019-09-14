
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

(defparameter *out-text* "/home/sonja/repo/org/cl-dino-master/out~A")
(defparameter *langs* "rus+eng")
(defparameter *default-width* 1295)
(defparameter *teaser-width* 690)
(defparameter *snap-width* 755)
(defparameter *snap-height* 668)
(defparameter *snap-x* 440)
(defparameter *default-x* 60)
(defparameter *default-y* 37)
(defparameter *new-image-array* nil)
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

;; принимает 2 массива изображений и пограничные координаты для поиска, т.е. ряд пикселей,
;; на котором поиск совпадающих рядов закончится
(defun find-row (image-up image-down y-border)
  (do ((cur-y (- (array-dimension image-up 0) 1) (- cur-y 1)))
      (( = cur-y y-border))
    (if (check-row image-up image-down cur-y 0)
        (progn
        (format t "~% ~A cur-y" cur-y)
        (return-from find-row cur-y)))))

;; проверяет 2 ряда пикселей на совпадение
;; если проверены все, возвращает Y координату
;; если хоть один не совпал - nil
(defun check-row (image-up image-down image-up-y image-down-y)
  (let ((width (- (array-dimension image-up 1) 1)))
  (do ((x 0 (+ x 1)))
      (( = x width) image-up-y)
    ;; если цвет писелей не одинаковый
        (if (not
             (and (eql (aref image-up image-up-y x  0)
                       (aref image-down image-down-y x 0))
                  (eql (aref image-up image-up-y x 1)
                       (aref image-down image-down-y x 1))
                  (eql (aref image-up image-up-y x 2)
                       (aref image-down image-down-y x 2))))
            (progn
              (format t "~% image-up-y x 0 ~A image-down-y x 0 ~A
                         ~% image-up-y x 1 ~A image-down-y x 1 ~A
                         ~% image-up-y x 2 ~A image-down-y x 2 ~A ~&
                         x ~A y ~A y-down ~A"
                      (aref image-up image-up-y x  0)
                      (aref image-down image-down-y x 0)
                      (aref image-up image-up-y x 1)
                      (aref image-down image-down-y x 1)
                      (aref image-up image-up-y x 2)
                      (aref image-down image-down-y x 2)
                      x image-up-y image-down-y)
                      ;; прерываем поиск и возвращаем nil
        (return-from check-row nil))
        ))))

;; принимает 2 массива изображений и точку, от которой их рано склеить
;; возвращает склеенный массив
(defun append-image (image-up image-down y-point)
  ;; (format t "~% image-up ~A image-down ~A y-point ~A"
  ;;         (array-dimensions image-up) (array-dimensions image-down) y-point)
  (destructuring-bind (height width colors)
      (array-dimensions image-down)
    (format t "~% ~A ~A ~A " height width colors)
    (let* ((append-height (- (* height 2) 1))
           (append-image-array (make-array `(,append-height ,width ,colors)
                                           :element-type '(unsigned-byte 8))))
      ;; копируем первую картинку в новый массив до точки склейки
      (do ((y 0 (+ y 1)))
          ((= y y-point))
        (do ((x 0 (+ x 1)))
            ((= x width))
          (do ((z 0 (+ z 1)))
              ((= z colors))
            (setf (aref append-image-array y x z) (aref image-up y x z)))))
      ;; копируем втрую картинку
      (do ((y-new y-point (+ y-new 1))
           (y 0 (+ y 1)))
          ((= y height) append-image-array)
        (do ((x 0 (+ x 1)))
            ((= x width))
          (do ((z 0 (+ z 1)))
              ((= z colors))
            (setf (aref append-image-array y-new x z) (aref image-up y x z))))))))


(defun my-vectorize-image (image)
  "Превращает массив (высота; ширина; цвет),
   где сначала идут все X-точки нулевой строки, потом первой, итд"
  (let ((idx 0)
        (result (make-array (reduce #'* (array-dimensions image))
                            :element-type '(unsigned-byte 8))))
    (do ((y 0 (+ 1 y)))
        ((= y (array-dimension image 0)) result)
      (do ((x 0 (+ 1 x)))
          ((= x (array-dimension image 1)))
        (do ((z 0 (+ 1 z)))
            ((= z (array-dimension image 2)))
          (setf (aref result idx)
                (aref image y x z))
          (incf idx))))))

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

(defun save-png (width height pathname-str image)
  (let* ((png (make-instance 'zpng:png :width width :height height
                             :color-type :truecolor-alpha
                             :image-data image)))
    (zpng:write-png png pathname-str)))

(defun binarization (image &optional threshold)
  (let ((dims (array-dimensions image)))
    (let ((result (make-array (butlast dims) :element-type '(unsigned-byte 8))))
      (loop for dx from 0 to (- (array-dimension image 1) 1) :do
           (loop for dy from 0 to (- (array-dimension image 0) 1) :do
                (loop for dz from 0 to (- (array-dimension image 2) 1) :do
                     (let ((avg (floor (+ (aref image dy dx 1)
                                          (aref image dy dx 0)
                                          (aref image dy dx 2))
                                       3)))
                       (when threshold
                         (if (< threshold avg)
                             (setf avg 255)
                             (setf avg 0)))
                       (setf (aref result dy dx) avg)))))
      result)))


(defun vectorize-image-gray (image)
  "Превращает массив size-X столбцов по size-Y точек в линейный,
   где сначала идут все X-точки нулевой строки, потом первой, итд"
  (let ((idx 0)
        (result (make-array (reduce #'* (array-dimensions image))
                            :element-type '(unsigned-byte 8))))
    (loop for dx from 0 to (- (array-dimension image 1) 1) :do
         (loop for dy from 0 to (- (array-dimension image 0) 1) :do
              (setf (aref result idx)
                    (aref image dy dx))
              (incf idx)))
    result))

(defun save-png-gray (width height pathname-str image)
  (let* ((png (make-instance 'zpng:png :width width :height height
                             :color-type :grayscale
                             :image-data image)))
    (zpng:write-png png pathname-str)))



;; Ошибка, возникающая когда мы пытаемся прочитать png
;; в котором неизвестно сколько байт на точку
(define-condition unk-png-color-type (error)
  ((color :initarg :color :reader color))
  (:report
   (lambda (condition stream)
     (format stream "Error in LOAD-PNG: unknown color type: ~A"
             (color condition)))))

(defun load-png (pathname-str)
  "Возвращает массив size-X столбцов по size-Y точек,
     где столбцы идут слева-направо, а точки в них - сверху-вниз"
  (let* ((png (png-read:read-png-file pathname-str))
         (image-data (png-read:image-data png))
         (color (png-read:colour-type png))
         (result
          (make-array ;; меняем размерности X и Y местами
           `(,(array-dimension image-data 1)
              ,(array-dimension image-data 0)
              ,(array-dimension image-data 2))
           :element-type (cond ((equal color :TRUECOLOR-ALPHA) '(unsigned-byte 8))
                               (t (error 'unk-png-color-type :color color))))))
    ;; (format t "~% new-arr ~A "(array-dimensions result))
    ;; ширина, высота, цвет => высота, ширина, цвет
    (macrolet ((cycle (&body body)
                 `(do ((y 0 (incf y)))
                      ((= y (array-dimension result 0)))
                    (do ((x 0 (incf x)))
                        ((= x (array-dimension result 1)))
                      (do ((z 0 (incf z)))
                          ((= z (array-dimension result 2)))
                        ,@body)))))
      (cond ((equal color :TRUECOLOR-ALPHA)
             (cycle (setf (aref result y x z)
                          (aref image-data x y z))))
            (t (error 'unk-png-color-type :color color)))
      result)))

;; (assert (equalp (load-png *test-path*)
;;                 (x-snapshot)))



;; (let* ((image (load-png "cell.png"))
;;        (image (binarization image)))
;;   (destructuring-bind (dw dh)
;;       (array-dimensions image)
;;     (save-png-gray dw dh "cell1.png" (vectorize-image-gray image))))

(block ttt
(x-snapshot :x 440 :y 100 :width  *snap-width* :height 668
            :path "~/Pictures/test0.png")
(x-snapshot :x 440 :y 100 :width  *snap-width* :height 668
            :path "~/Pictures/test1.png")
(let* ((arr1 (load-png "~/Pictures/test0.png"))
       (arr2 (load-png "~/Pictures/test0.png"))
       (n (format t "~% arr1 ~A arr2 ~A" (array-dimensions arr1)
               (array-dimensions arr1)))
       (array (append-image arr1 arr2
                            (- (array-dimension arr1 0) 1)))
         (width (array-dimension array 1))
       (height (array-dimension array 0)))
  (format t "~% w ~A h ~A" width height)
  (save-png width height
            "~/Pictures/result.png"
            (my-vectorize-image
             array))))

;; (block test
;;   (open-browser "/usr/bin/firefox"
;;                  *hh-teaser-url*)
;;   (sleep 8)
;;   (do ((i 0 (+ 1 i)))
;;       ((= i 6))
;;     (perform-key-action t 116)
;;     (perform-key-action nil 116))
;;   (sleep 1)
;;  (x-snapshot :x 440 :y 100 :width  *snap-width* :height 668
;;              :path "/home/sonja/Pictures/test0.png")
;;  (let* ((arr (load-png "~/Pictures/test0.png"))
;;         (array (rewrite-array arr)))
;;     (sleep 1)
;;     (crop-teaser array
;;                  "/home/sonja/Pictures/test0.png")

;;     (let* ((image (load-png "~/Pictures/test0.png"))
;;            (image (binarization image)))
;;       (destructuring-bind (dw dh)
;;           (array-dimensions image)
;;         (save-png-gray dw dh "~/Pictures/test0.png" (vectorize-image-gray image))))

;;     (sleep 1)
;;     ;;(resize "/home/sonja/Pictures/test0.png")
;;     (run-tess "/home/sonja/Pictures/test0.png" "/home/sonja/repo/org/cl-dino-master/out"
;;               *langs*)))

;; (block little-test
;;   ;; (open-browser "/usr/bin/firefox"
;;   ;;               *hh-teaser-url*)
;;   ;; (sleep 8)
;;   (let ((image-array-up (x-snapshot :x 440 :y 100
;;                                     :width *snap-width*
;;                                     :height *snap-height*)))
;;     ;; (sleep 1)
;;     ;; (perform-key-action t 117)
;;     ;; (sleep .1)
;;     ;; (perform-key-action nil 117)
;;     ;; (sleep 1)
;;     (let ((image-array-down
;;            (x-snapshot :x 440 :y 100
;;                        :width *snap-width*
;;                        :height *snap-height*)))
;;       ;; (let ((point (find-row image-array-up image-array-down
;;       ;;                        (/ (array-dimension image-array-up 0) 2))))
;;       ;;  (format t "~% ~A" point)
;;       ;;)))))
;;       ;;(if point
;;       (progn
;;         (let* ((array (append-image image-array-up image-array-down
;;                                     (- *snap-height* 1)))
;;                (width (array-dimension array 1))
;;                (height (array-dimension array 0)))
;;           (save-png width height
;;                     "~/Pictures/result.png"
;;                     (my-vectorize-image
;;                      array)))))))

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

;; принимает массив пикселей картинки, которую должен вырезать
;; и путь для файла, куда сохранит новую картинку
;; путь для image-path должен быть абсолютным
(defun crop-teaser (image-array image-path)
  (multiple-value-bind (border-height x-begin y-begin)
      ;; ищем границы тизера
      (find-border image-array)
    (if (eql border-height 0)
        ;; нулевая высота, встретили блок рекламы
          (return-from crop-teaser -1)
        (if (and (null x-begin) (null y-begin))
            ;; вообще не нашли бордюр, тизеры кончились
              (return-from crop-teaser -2)
            (progn
              (format t "~% ~Ax~A+~A+~A"
                      (array-dimension image-array 1) border-height
                      x-begin y-begin)
              (let ((proc (sb-ext:run-program
                           "/usr/lib/i386-linux-gnu/ImageMagick-6.8.9/bin-Q16/mogrify"
                           `("-crop"
                             ,(format nil "~Ax~A+~A+~A"
                                      (array-dimension image-array 1) border-height
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
                        (return-from crop-teaser 0)))
                    (format t "~% crop-teaser: didn't run ImageMagic"))))))))

(defun find-border (image-array)
  ;; ищем левый верхний угол у вакансии (сначала ищем выделенные)
  (multiple-value-bind (x-begin y-begin)
      (first-point image-array *R-priority* *G-priority* *B-priority*)
    ;; нашли?
    (if (and x-begin y-begin)
        ;; да
        ;; получаем высоту тизера
        (multiple-value-bind (border-height)
            (get-height x-begin y-begin image-array
                      *R-priority* *G-priority* *B-priority*)
          (values border-height x-begin y-begin))
        ;; нет
        ;; ищем левый верхний угол у обычной вакансии
        (multiple-value-bind (x-begin y-begin)
            (first-point image-array *R-usual* *G-usual* *B-usual*)
          ;; нашли?
          (if (and x-begin y-begin)
              ;; да
              ;; проверяем, точно ли это тизер
              (progn
                (format t "~% usual")
                (if (check-width x-begin y-begin image-array *teaser-width*
                                 *R-usual* *G-usual* *B-usual*)
                    ;; да, это тизер
                    (multiple-value-bind (border-height)
                        (get-height x-begin y-begin image-array
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
(defun first-point (image-array r g b)
  (do ((x 0 (+ x 1)))
      ((= x  (array-dimension image-array 1)))
    (do ((y 0 (+ y 1)))
        ((= y (array-dimension image-array 0))
      (if (and (eql r (aref image-array y x 0))
               (eql g (aref image-array y x 1))
               (eql b (aref image-array y x 2))
               (eql r (aref image-array y (+ x 1) 0))
               (eql g (aref image-array y (+ x 1) 1))
               (eql b (aref image-array y (+ x 1) 2)))
          (return-from first-point (values x y)))))
  ;; если после выполнения циклов точка не найдена
  (return-from first-point (values nil nil))))


;; ищем высоту
(defun get-height (x y image-array r g b)
  (format t "~% x ~A y ~A" x y)
  (let ((height 0))
    (do ((i (+ y 1) (+ i 1)))
        ((= i (array-dimension image-array 0)) height)
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

;; принимает X и Y левого угла тизера, массив пикселей скрина
;;
(defun check-width (x y image-array teaser-width r g b)
  (let ((width 0))
    (format t "~% x ~A y ~A" x y)
    (do ((i x (+ i 1)))
        ((= i (array-dimension image-array 1)) width)
      (if (and (eql r (aref image-array y i 0))
               (eql g (aref image-array y i 1))
               (eql b (aref image-array y i 2)))
          (setf width (+ width 1))))
    (if (eql width teaser-width)
        (progn
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
    (let ((image-array (zpng:data-array png)))
    (dotimes (y height)
      (dotimes (x width)
        ;; BGR -> RGB, ref code: https://goo.gl/slubfW
        ;; diffs between RGB and BGR: https://goo.gl/si1Ft5
        (rotatef (aref image-array y x 0) (aref image-array y x 2))
        (setf (aref image-array y x 3) 255)))
    png)))

(multiple-value-bind (default-width default-height) (x-size)
  (defun x-snapshot (&key (x *default-x*) (y *default-y*)
                       (width  *default-width*) (height *default-height*)
                       path)
    ;; "Return RGB data array (The dimensions correspond to the height, width,
    ;; and pixel components, see comments in x-snapsearch for more details),
    ;; or write to file (PNG only), depend on if you provide the path keyword"
    (with-default-window (w)
      (let ((image
             (raw-image->png
              (xlib:get-raw-image w :x x :y y
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
            (zpng:data-array image))))))

;; (defun start ()
;;   (do ((i 0 (+ 1 i)))
;;       ((= i 6))
;;     (perform-key-action t 116)
;;     (perform-key-action nil 116))
;;   (sleep 1)
;;   (get-images)
;;   (sleep 5)
;;   (setf *image-amount* (*image-amount* -1))
;;   (do ((i 0 (+ i 1)))
;;       ((= i *image-amount*))
;;     (resize `,(format nil
;;                       "/home/sonja/repo/org/cl-dino-master/test~A.png"
;;                       i))
;;     (sleep 1)
;;     (run-tess `,(format nil
;;                         "/home/sonja/repo/org/cl-dino-master/test~A.png"
;;                         i) `,(format nil
;;                                      "/home/sonja/repo/org/cl-dino-master/out~A"
;;                                      i) *langs*)))

;; (defun get-images ()
;;   (let ((crop-marker) (cnt 4))
;;     (do ((j 0 (+ j 1)))
;;         (nil)
;;       (do ((j 0 (+ j 1)))
;;           ((= j cnt))
;;         (perform-key-action t 116)
;;         (perform-key-action nil 116))
;;       (sleep 1)
;;       (x-snapshot :x 440 :width  *snap-width*
;;                   :path `,(format nil "test~A.png" *image-indx*))
;;       (sleep 1)
;;       (setf crop-marker
;;             (crop-teaser *new-image-array*
;;                          `,(format nil
;;                                    "/home/sonja/repo/org/cl-dino-master/test~A.png"
;;                                    *image-indx*)))
;;       (if (eql crop-marker -2)
;;           (progn
;;             (format t "~% get-images: no teasers anymore")
;;             (return-from get-images 1))

;;           (if (eql crop-marker -1)
;;               ;; встретили блок рекламы, пропускаем, не анализируя
;;               (setf cnt 2)
;;               (progn
;;                 (setf *image-indx* (+ *image-indx* 1))
;;                 (setf cnt 4)))))))
