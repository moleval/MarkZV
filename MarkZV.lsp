;;;=====================================================================
;;; MarkZV.lsp
;;; Модуль сбора данных, построения 2D модели витража
;;; и автозаполнения атрибутов стоек и ригелей
;;;=====================================================================
(vl-load-com)

;;;=====================================================================
;;; 0. АВТОПРОВЕРКА СКОБОК
;;;=====================================================================
(defun mk:check-brackets (filepath / f line num depth in-str i c len err-line ch)
  (setq f (open filepath "r"))
  (if (null f)
    (progn
      (prompt (strcat "\n[CHECK] Файл не найден: " filepath))
      nil)
    (progn
      (setq num 0 depth 0 in-str nil err-line nil)
      (while (setq line (read-line f))
        (setq num (1+ num))
        (setq i 1)
        (setq len (strlen line))
        (while (<= i len)
          (setq ch (substr line i 1))
          (cond
            (in-str
             (if (and (= ch "\\") (< i len))
               (setq i (1+ i))  ; пропустить следующий символ после \
             )
             (if (= ch "\"")
               (setq in-str nil)
             )
            )
            ((= ch ";")
             (setq i len)  ; пропустить до конца строки
            )
            ((= ch "\"")
             (setq in-str t)
            )
            ((= ch "(")
             (setq depth (1+ depth))
            )
            ((= ch ")")
             (setq depth (1- depth))
             (if (and (< depth 0) (null err-line))
               (setq err-line num)
             )
            )
          )
          (setq i (1+ i))
        )
      )
      (close f)
      (cond
        (err-line
         (prompt (strcat "\n[CHECK ERROR] Лишняя ')' на строке: " (itoa err-line)))
         nil
        )
        ((> depth 0)
         (prompt (strcat "\n[CHECK ERROR] Не закрыто '(' : " (itoa depth) " шт."))
         nil
        )
        (in-str
         (prompt "\n[CHECK ERROR] Не закрыта кавычка")
         nil
        )
        (t
         (prompt (strcat "\n[CHECK OK] Скобки сбалансированы. Строк: " (itoa num)))
         t
        )
      )
    )
  )
)

;; Путь к исходнику: несколько кандидатов + findfile (донор: mark:find-source)
(defun mk:find-source (/ cand out)
  (setq out nil)
  (foreach cand (list "D:/MarkZV.lsp" "D:/MarkZV/MarkZV.lsp"
                      "C:/Work/MarkZV/MarkZV.lsp" "MarkZV.lsp")
    (if (and (null out) (findfile cand))
      (setq out (findfile cand))))
  out)

(defun c:МАРКАВСКОБКИ (/ result path)
  (prompt "\n[МАРКАВСКОБКИ] Проверка баланса скобок MarkZV.lsp...")
  (setq path (mk:find-source))
  (if path
    (prompt (strcat "\n  Файл: " path))
    (prompt "\n  [WARN] Файл не найден среди кандидатов."))
  (setq result (if path (mk:check-brackets path) nil))
  (if result
    (prompt "\n[OK] Файл корректен.")
    (prompt "\n[ERROR] Обнаружены проблемы. См. сообщение выше."))
  (princ))

;;;=====================================================================
;;; 1. КОНФИГУРАЦИЯ
;;;=====================================================================
;; Ред. <версия>.<билд>:  версия — крупные задачи, билд — итерация правок
(setq *mk:ver*            "1.3")

(setq *mk:block-fill*     "Заполнение в витраж")
(setq *mk:block-window*   "Окно КПТ60")
(setq *mk:block-door*     "Дверной блок КПТ74 двухстворчатый")
(setq *mk:block-vitrage*  "Атрибуты витража")
(setq *mk:block-post*     "стойка")
(setq *mk:block-beam*     "ригель")

(setq *mk:attr-article*   "Артикул")
(setq *mk:attr-thickness* "Толщина")
(setq *mk:attr-mark*      "Марка")
(setq *mk:attr-name*      "Название")
(setq *mk:attr-vitrage*   "Витраж")

;; Маски для поиска динамических свойств
(setq *mk:mask-length*    "*лин*")
(setq *mk:mask-height*    "*ысот*")
(setq *mk:mask-width*     "*ирин*")

(setq *mk:vis-candidates*
  '("Видимость1" "Видимость" "Visibility" "Visibility1"
    "Видимость состояния" "Visibility State" "Вид"))

(setq *mk:offset-fill*    25.0)
(setq *mk:tol-adjacency*  5.0)
(setq *mk:tol-skew*       5.0)
(setq *mk:tol-threshold*  50.0)
(setq *mk:tol-size*       0.5)
(setq *mk:vitrage-radius* 10000.0)
(setq *mk:slope-tol*      0.0033)
(setq *mk:out-file*       "D:/MARKZV_MODEL.txt")
(setq *mk:diag-file*      "D:/MARKZV_DIAG.txt")
(setq *mk:cached-data*    nil)
(setq *mk:last-ss*        nil)   ; выборка последней МАРКАВГЕОМЕТРИЯ
(setq *mk:dyn-cache*      nil)   ; кэш динамических свойств: ename -> alist

(setq *mk:suffix-window-one*    "ок")
(setq *mk:suffix-window-both*   "окх2")
(setq *mk:suffix-door-one*      "дв")
(setq *mk:suffix-threshold*     "н")
(setq *mk:suffix-warm-cold*     "тх")
(setq *mk:suffix-cold-warm*     "хт")
(setq *mk:suffix-mirror*        "зерк")
(setq *mk:suffix-small*         "м")     ; малый профиль
(setq *mk:suffix-big*           "б")     ; большой профиль
;; Ручная таблица артикул -> "м"/"б" (приоритет над автоопределением по габариту).
;; Заполняется из базы СИАЛ: (("КП45551" . "м") ("КП45364" . "б"))
(setq *mk:article-size*         nil)
(setq *mk:group-model*          "MK_MODEL")
(setq *mk:group-test*           "MK_TEST_MODEL")
(setq *mk:thick-warm-min*       42.0)
(setq *mk:thick-warm-max*       60.0)
(setq *mk:thick-cold-min*       4.0)
(setq *mk:thick-cold-max*       32.0)

;;;=====================================================================
;;; 2. УТИЛИТЫ
;;;=====================================================================
(defun mk:strp (x) (and x (eq (type x) 'STR)))

;; Обновление поля записи: замена через subst, а не append.
;; (append …) добавлял бы ВТОРУЮ пару с тем же ключом, а assoc
;; всегда возвращает первую — значение молча не менялось.
(defun mk:rec-put (rec key val / old)
  (setq old (assoc key rec))
  (if old
    (subst (cons key val) old rec)
    (append rec (list (cons key val)))))

(defun mk:rec-get (rec key)
  (cdr (assoc key rec)))

(defun mk:numval (x)
  (cond ((numberp x) (float x))
        ((and (mk:strp x) (distof x 2)) (distof x 2))
        (t nil)))

(defun mk:ax-get (obj prop / r)
  (setq r (vl-catch-all-apply 'vlax-get-property (list obj prop)))
  (if (vl-catch-all-error-p r) nil r))

(defun mk:trim (s)
  (if (mk:strp s) (vl-string-trim " \t\n\r" s) s))

(defun mk:name= (s1 s2)
  (and (mk:strp s1) (mk:strp s2)
       (= (strcase (mk:trim s1)) (strcase (mk:trim s2)))))

(defun mk:blk-match? (ename target / obj eff-name names nm found)
  (setq found nil names nil)
  (setq nm (cdr (assoc 2 (entget ename))))
  (if (mk:strp nm) (setq names (cons nm names)))
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (and (not (vl-catch-all-error-p obj)) obj)
    (progn
      (setq nm (mk:ax-get obj "EffectiveName"))
      (if (and (mk:strp nm) (not (member nm names)))
        (setq names (cons nm names)))))
  (foreach nm names
    (if (and (null found) (wcmatch (strcase nm) (strcase target)))
      (setq found t)))
  found)

;;;=====================================================================
;;; 3. ИЗВЛЕЧЕНИЕ АТРИБУТОВ
;;;=====================================================================
(defun mk:get-attr (ename tag / sub data val)
  (setq sub (entnext ename) val nil)
  (while sub
    (setq data (entget sub))
    (cond
      ((= (cdr (assoc 0 data)) "ATTRIB")
       (if (mk:name= (cdr (assoc 2 data)) tag)
         (setq val (cdr (assoc 1 data)))))
      ((= (cdr (assoc 0 data)) "SEQEND")
       (setq sub nil)))
    (if sub (setq sub (entnext sub))))
  (if (mk:strp val) (mk:trim val) nil))

(defun mk:get-all-attrs (ename / sub data attrs)
  (setq sub (entnext ename) attrs nil)
  (while sub
    (setq data (entget sub))
    (cond
      ((= (cdr (assoc 0 data)) "ATTRIB")
       (setq attrs (cons (cons (cdr (assoc 2 data))
                                (cdr (assoc 1 data))) attrs)))
      ((= (cdr (assoc 0 data)) "SEQEND")
       (setq sub nil)))
    (if sub (setq sub (entnext sub))))
  (reverse attrs))

(defun mk:has-attr? (ename tag / sub data found)
  (setq sub (entnext ename) found nil)
  (while (and sub (not found))
    (setq data (entget sub))
    (cond
      ((= (cdr (assoc 0 data)) "ATTRIB")
       (if (mk:name= (cdr (assoc 2 data)) tag)
         (setq found t)))
      ((= (cdr (assoc 0 data)) "SEQEND")
       (setq sub nil)))
    (if sub (setq sub (entnext sub))))
  found)

;;;=====================================================================
;;; 4. ДИНАМИЧЕСКИЕ СВОЙСТВА И ГАБАРИТЫ
;;;=====================================================================
(defun mk:dynamic? (obj / v)
  (setq v (mk:ax-get obj "IsDynamicBlock"))
  (and v (= v :vlax-true)))

;; Все пары (имя . значение) динамического блока; VARIANT развёрнут
(defun mk:dyn-pairs-raw (obj / props out p nm val)
  (setq out nil)
  (if (mk:dynamic? obj)
    (progn
      (setq props (vl-catch-all-apply 'vlax-invoke-method
                    (list obj "GetDynamicBlockProperties")))
      (if (not (vl-catch-all-error-p props))
        (progn
          (if (= (type props) 'VARIANT)
            (setq props (vl-catch-all-apply 'vlax-safearray->list
                          (list (vlax-variant-value props)))))
          (if (and props (listp props) (not (vl-catch-all-error-p props)))
            (foreach p props
              (setq nm  (mk:ax-get p "PropertyName")
                    val (mk:ax-get p "Value"))
              (if (= (type val) 'VARIANT)
                (progn
                  (setq val (vl-catch-all-apply 'vlax-variant-value (list val)))
                  (if (vl-catch-all-error-p val) (setq val nil))))
              (if (mk:strp nm)
                (setq out (cons (cons (mk:trim nm) val) out)))))))))
  (reverse out))

;; То же, но с кэшем по ename (сбор на большом фасаде идёт в разы быстрее)
(defun mk:dyn-pairs (obj / e hit pairs)
  (setq e (vl-catch-all-apply 'vlax-vla-object->ename (list obj)))
  (if (vl-catch-all-error-p e) (setq e nil))
  (setq hit (if e (assoc e *mk:dyn-cache*) nil))
  (if hit
    (cdr hit)
    (progn
      (setq pairs (mk:dyn-pairs-raw obj))
      (if e (setq *mk:dyn-cache* (cons (cons e pairs) *mk:dyn-cache*)))
      pairs)))

;; Числовое динамическое свойство по маске имени (*лин*, *ысот*, *ирин*)
(defun mk:get-dyn (obj prop-mask / val)
  (setq val nil)
  (foreach pr (mk:dyn-pairs obj)
    (if (and (null val)
             (wcmatch (strcase (car pr)) (strcase prop-mask)))
      (setq val (mk:numval (cdr pr)))))
  (if (numberp val) val nil))

;;;=====================================================================
;;; 5. ВИДИМОСТЬ
;;;=====================================================================
(defun mk:get-vis (obj / val)
  (setq val nil)
  (foreach pr (mk:dyn-pairs obj)
    (if (and (null val)
             (member (car pr) *mk:vis-candidates*)
             (mk:strp (cdr pr)))
      (setq val (mk:trim (cdr pr)))))
  val)

;;;=====================================================================
;;; 5a. ГАБАРИТ БЛОКА (фолбэк размеров, донор: mark:fill-e-bb из MarkZ)
;;;=====================================================================
(defun mk:e-bb (obj / r a b)
  (if (null obj)
    nil
    (progn
      (setq a nil b nil)
      (setq r (vl-catch-all-apply 'vlax-invoke-method
                (list obj "GetBoundingBox" 'a 'b)))
      (if (or (vl-catch-all-error-p r) (null a) (null b))
        nil
        (progn
          (setq a (vl-catch-all-apply 'vlax-safearray->list (list a))
                b (vl-catch-all-apply 'vlax-safearray->list (list b)))
          (if (or (vl-catch-all-error-p a) (vl-catch-all-error-p b)
                  (null a) (null b))
            nil
            (list (float (car a)) (float (cadr a))
                  (float (car b)) (float (cadr b)))))))))

(defun mk:bb-w (bb) (if bb (abs (- (nth 2 bb) (nth 0 bb))) nil))
(defun mk:bb-h (bb) (if bb (abs (- (nth 3 bb) (nth 1 bb))) nil))

;; Размер: сначала динамическое свойство, иначе габарит -> (значение источник)
(defun mk:dim-src (obj mask bb-fn / v bb)
  (setq v (mk:get-dyn obj mask))
  (if (numberp v)
    (list v "DYN")
    (progn
      (setq bb (mk:e-bb obj)
            v  (if bb (apply bb-fn (list bb)) nil))
      (if (and (numberp v) (> v 1e-6))
        (list v "BBOX")
        (list nil "НЕТ")))))

;;;=====================================================================
;;; 6. ИЗВЛЕЧЕНИЕ ГЕОМЕТРИИ
;;;=====================================================================
(defun mk:get-geom (obj / ins-pt rot xsc ysc pt-list)
  (if (null obj)
    (list (cons 'INS_PT nil) (cons 'ROTATION 0.0)
          (cons 'XSCALE 1.0) (cons 'YSCALE 1.0))
    (progn
      (setq ins-pt (vl-catch-all-apply 'vlax-get-property (list obj "InsertionPoint")))
      (setq rot    (mk:ax-get obj "Rotation"))
      (setq xsc    (mk:ax-get obj "XScale"))
      (setq ysc    (mk:ax-get obj "YScale"))
      (setq pt-list nil)
      (if (and ins-pt (not (vl-catch-all-error-p ins-pt)))
        (progn
          (cond
            ((= (type ins-pt) 'SAFEARRAY)
             (setq pt-list (vl-catch-all-apply 'vlax-safearray->list (list ins-pt))))
            ((= (type ins-pt) 'VARIANT)
             (setq pt-list (vl-catch-all-apply 'vlax-safearray->list (list (vlax-variant-value ins-pt)))))
            ((listp ins-pt)
             (setq pt-list ins-pt)))))
      (if (and pt-list (vl-catch-all-error-p pt-list))
        (setq pt-list nil))
      (list
        (cons 'INS_PT (if (and pt-list (listp pt-list) (>= (length pt-list) 3))
                        (list (float (nth 0 pt-list))
                              (float (nth 1 pt-list))
                              (float (nth 2 pt-list)))
                        nil))
        (cons 'ROTATION (if (numberp rot) rot 0.0))
        (cons 'XSCALE   (if (numberp xsc) xsc 1.0))
        (cons 'YSCALE   (if (numberp ysc) ysc 1.0))))))

;;;=====================================================================
;;; 7. ЧТЕНИЕ БЛОКА «АТРИБУТЫ ВИТРАЖА»
;;;=====================================================================
(defun mk:read-vitrage-block (/ ss i e attrs vitrage-data)
  (setq vitrage-data nil)
  (setq ss (ssget "_X" (list (cons 0 "INSERT"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i))
        (if (mk:blk-match? e (strcat "*" *mk:block-vitrage* "*"))
          (progn
            (setq attrs (mk:get-all-attrs e))
            (setq vitrage-data
              (list
                (cons 'TYPE "АТРИБУТЫ_ВИТРАЖА")
                (cons 'ENAME e)
                (cons 'INS_PT (cdr (assoc 'INS_PT (mk:get-geom
                  (vlax-ename->vla-object e)))))
                (cons 'ATTRS attrs)))))
        (setq i (1+ i)))))
  vitrage-data)

(defun mk:get-vitrage-prefix (/ vitrage attrs prefix)
  (setq vitrage (mk:read-vitrage-block))
  (if vitrage
    (progn
      (setq attrs (cdr (assoc 'ATTRS vitrage)))
      (foreach attr attrs
        (if (mk:name= (car attr) *mk:attr-vitrage*)
          (setq prefix (cdr attr))))
      (if (and (null prefix) attrs)
        (setq prefix (cdr (car attrs))))))
  (if (mk:strp prefix) prefix "В-1"))

;;;=====================================================================
;;; 8. ИЗВЛЕЧЕНИЕ МУЛЬТИЛИНИЙ
;;;=====================================================================
(defun mk:find-mlines (/ ss i e lst)
  (setq lst nil)
  (setq ss (ssget "_X" (list (cons 0 "MLINE"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i))
        (setq lst (cons e lst))
        (setq i (1+ i)))))
  lst)

(defun mk:get-mline-verts (ename / ed verts)
  (setq ed (entget ename) verts nil)
  (foreach p ed
    (if (= (car p) 10)
      (setq verts (cons (list (float (cadr p)) (float (caddr p))) verts))))
  (setq verts (reverse verts))
  (if (< (length verts) 2)
    (progn
      (setq verts nil)
      (foreach p ed
        (if (= (car p) 11)
          (setq verts (cons (list (float (cadr p)) (float (caddr p))) verts))))
      (setq verts (reverse verts))))
  verts)

(defun mk:verts-to-segs (verts / segs n i p0 p1)
  (setq segs nil n (length verts) i 0)
  (while (< i (1- n))
    (setq p0 (nth i verts) p1 (nth (1+ i) verts))
    (setq segs (cons (list (car p0) (cadr p0) (car p1) (cadr p1)) segs))
    (setq i (1+ i)))
  (reverse segs))

(defun mk:classify-seg (seg / dx dy adx ady len)
  (setq dx (- (nth 2 seg) (nth 0 seg))
        dy (- (nth 3 seg) (nth 1 seg))
        adx (abs dx) ady (abs dy)
        len (sqrt (+ (* dx dx) (* dy dy))))
  (if (< len 1e-6)
    nil
    (cond
      ((<= ady (* *mk:slope-tol* len)) 'h)
      ((<= adx (* *mk:slope-tol* len)) 'v)
      ((>= adx ady) 'h)
      (t 'v))))

;; Извлечение стоек и ригелей с нормализацией направления
(defun mk:extract-posts-beams-from-mlines (mlines / verts segs posts beams cls len ins-x ins-y)
  (setq posts nil beams nil)
  (foreach mline mlines
    (setq verts (mk:get-mline-verts mline))
    (if (and verts (>= (length verts) 2))
      (progn
        (setq segs (mk:verts-to-segs verts))
        (foreach seg segs
          (setq cls (mk:classify-seg seg))
          (if cls
            (progn
              (if (= cls 'v)
                (progn
                  (setq len (abs (- (nth 3 seg) (nth 1 seg))))
                  (if (> len 50.0)
                    (progn
                      (if (< (nth 3 seg) (nth 1 seg))
                        (setq ins-x (nth 2 seg) ins-y (nth 3 seg))
                        (setq ins-x (nth 0 seg) ins-y (nth 1 seg)))
                      (setq posts (cons (list
                        (cons 'TYPE "СТОЙКА")
                        (cons 'ENAME mline)
                        (cons 'INS_PT (list ins-x ins-y 0.0))
                        (cons 'LENGTH len)
                        (cons 'ARTICLE nil)
                        (cons 'VISIBILITY nil)
                        (cons 'PROTRUDING nil)
                        (cons 'LEFT_CONN nil)
                        (cons 'RIGHT_CONN nil)
                      ) posts)))))
                (progn
                  (setq len (abs (- (nth 2 seg) (nth 0 seg))))
                  (if (> len 50.0)
                    (progn
                      (if (< (nth 2 seg) (nth 0 seg))
                        (setq ins-x (nth 2 seg) ins-y (nth 3 seg))
                        (setq ins-x (nth 0 seg) ins-y (nth 1 seg)))
                      (setq beams (cons (list
                        (cons 'TYPE "РИГЕЛЬ")
                        (cons 'ENAME mline)
                        (cons 'INS_PT (list ins-x ins-y 0.0))
                        (cons 'LENGTH len)
                        (cons 'ARTICLE nil)
                        (cons 'VISIBILITY nil)
                        (cons 'TOP_ELEM nil)
                        (cons 'BOT_ELEM nil)
                        (cons 'SUFFIX nil)
                      ) beams))))))))))))
  (list (reverse posts) (reverse beams)))

;;;=====================================================================
;;; 9. СБОР ДАННЫХ ПО БЛОКАМ (маски *ысот* *ирин* *лин*)
;;;=====================================================================
(defun mk:collect-fill (ename / obj geom dh dw)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (vl-catch-all-error-p obj) (null obj)) (setq obj nil))
  (setq geom (mk:get-geom obj)
        dh   (mk:dim-src obj *mk:mask-height* 'mk:bb-h)
        dw   (mk:dim-src obj *mk:mask-width*  'mk:bb-w))
  (list
    (cons 'TYPE       "ЗАПОЛНЕНИЕ")
    (cons 'ENAME      ename)
    (cons 'INS_PT     (cdr (assoc 'INS_PT geom)))
    (cons 'ROTATION   (cdr (assoc 'ROTATION geom)))
    (cons 'XSCALE     (cdr (assoc 'XSCALE geom)))
    (cons 'YSCALE     (cdr (assoc 'YSCALE geom)))
    (cons 'HEIGHT     (car dh))
    (cons 'WIDTH      (car dw))
    (cons 'SIZE_SRC   (strcat (cadr dh) "/" (cadr dw)))
    (cons 'THICKNESS  (mk:get-attr ename *mk:attr-thickness*))
    (cons 'ARTICLE    (mk:get-attr ename *mk:attr-article*))
    (cons 'VISIBILITY (mk:get-vis obj))
    (cons 'CONN_POSTS nil)
  ))

(defun mk:collect-window (ename / obj geom dh dw)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (vl-catch-all-error-p obj) (null obj)) (setq obj nil))
  (setq geom (mk:get-geom obj)
        dh   (mk:dim-src obj *mk:mask-height* 'mk:bb-h)
        dw   (mk:dim-src obj *mk:mask-width*  'mk:bb-w))
  (list
    (cons 'TYPE       "ОКНО")
    (cons 'ENAME      ename)
    (cons 'INS_PT     (cdr (assoc 'INS_PT geom)))
    (cons 'ROTATION   (cdr (assoc 'ROTATION geom)))
    (cons 'XSCALE     (cdr (assoc 'XSCALE geom)))
    (cons 'YSCALE     (cdr (assoc 'YSCALE geom)))
    (cons 'HEIGHT     (car dh))
    (cons 'WIDTH      (car dw))
    (cons 'SIZE_SRC   (strcat (cadr dh) "/" (cadr dw)))
    (cons 'NAME       (mk:get-attr ename *mk:attr-name*))
    (cons 'VISIBILITY (mk:get-vis obj))
    (cons 'CONN_POSTS nil)
  ))

(defun mk:collect-door (ename / obj geom dh dw)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (vl-catch-all-error-p obj) (null obj)) (setq obj nil))
  (setq geom (mk:get-geom obj)
        dh   (mk:dim-src obj *mk:mask-height* 'mk:bb-h)
        dw   (mk:dim-src obj *mk:mask-width*  'mk:bb-w))
  (list
    (cons 'TYPE       "ДВЕРЬ")
    (cons 'ENAME      ename)
    (cons 'INS_PT     (cdr (assoc 'INS_PT geom)))
    (cons 'ROTATION   (cdr (assoc 'ROTATION geom)))
    (cons 'XSCALE     (cdr (assoc 'XSCALE geom)))
    (cons 'YSCALE     (cdr (assoc 'YSCALE geom)))
    (cons 'HEIGHT     (car dh))
    (cons 'WIDTH      (car dw))
    (cons 'SIZE_SRC   (strcat (cadr dh) "/" (cadr dw)))
    (cons 'NAME       (mk:get-attr ename *mk:attr-name*))
    (cons 'VISIBILITY (mk:get-vis obj))
    (cons 'CONN_POSTS nil)
  ))

(defun mk:collect-post (ename / obj geom dl)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (vl-catch-all-error-p obj) (null obj)) (setq obj nil))
  (setq geom (mk:get-geom obj)
        dl   (mk:dim-src obj *mk:mask-length* 'mk:bb-h))
  (list
    (cons 'TYPE       "СТОЙКА")
    (cons 'ENAME      ename)
    (cons 'INS_PT     (cdr (assoc 'INS_PT geom)))
    (cons 'ROTATION   (cdr (assoc 'ROTATION geom)))
    (cons 'XSCALE     (cdr (assoc 'XSCALE geom)))
    (cons 'LENGTH     (car dl))
    (cons 'SIZE_SRC   (cadr dl))
    (cons 'CROSS      (mk:bb-w (mk:e-bb obj)))
    (cons 'ARTICLE    (mk:get-attr ename *mk:attr-article*))
    (cons 'VISIBILITY (mk:get-vis obj))
    (cons 'LEFT_CONN  nil)
    (cons 'RIGHT_CONN nil)
    (cons 'PROTRUDING nil)
  ))

(defun mk:collect-beam (ename / obj geom dl)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (vl-catch-all-error-p obj) (null obj)) (setq obj nil))
  (setq geom (mk:get-geom obj)
        dl   (mk:dim-src obj *mk:mask-length* 'mk:bb-w))
  (list
    (cons 'TYPE       "РИГЕЛЬ")
    (cons 'ENAME      ename)
    (cons 'INS_PT     (cdr (assoc 'INS_PT geom)))
    (cons 'ROTATION   (cdr (assoc 'ROTATION geom)))
    (cons 'LENGTH     (car dl))
    (cons 'SIZE_SRC   (cadr dl))
    (cons 'CROSS      (mk:bb-h (mk:e-bb obj)))
    (cons 'ARTICLE    (mk:get-attr ename *mk:attr-article*))
    (cons 'VISIBILITY (mk:get-vis obj))
    (cons 'TOP_ELEM   nil)
    (cons 'BOT_ELEM   nil)
    (cons 'SUFFIX     nil)
  ))

;;;=====================================================================
;;; 10. ПОИСК БЛОКОВ В ВЫБОРКЕ
;;;=====================================================================
(defun mk:find-blocks-in-ss (ss mask / i e lst)
  (setq lst nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i))
        (if (mk:blk-match? e mask)
          (setq lst (cons e lst)))
        (setq i (1+ i)))))
  lst)

(defun mk:find-mlines-in-ss (ss / i e lst)
  (setq lst nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i))
        (if (= (cdr (assoc 0 (entget e))) "MLINE")
          (setq lst (cons e lst)))
        (setq i (1+ i)))))
  lst)

;;;=====================================================================
;;; 11. ОПРЕДЕЛЕНИЕ ГРАНИЦ ВИТРАЖА
;;;=====================================================================
(defun mk:find-vitrage-bounds (posts beams / min-x max-x min-y max-y x y len)
  (setq min-x nil max-x nil min-y nil max-y nil)
  (foreach post posts
    (if (cdr (assoc 'INS_PT post))
      (progn
        (setq x (car (cdr (assoc 'INS_PT post))))
        (setq y (cadr (cdr (assoc 'INS_PT post))))
        (setq len (if (cdr (assoc 'LENGTH post)) (cdr (assoc 'LENGTH post)) 0.0))
        (if (or (null min-x) (< x min-x)) (setq min-x x))
        (if (or (null max-x) (> x max-x)) (setq max-x x))
        (if (or (null min-y) (< y min-y)) (setq min-y y))
        (if (or (null max-y) (> (+ y len) max-y)) (setq max-y (+ y len))))))
  (foreach beam beams
    (if (cdr (assoc 'INS_PT beam))
      (progn
        (setq x (car (cdr (assoc 'INS_PT beam))))
        (setq y (cadr (cdr (assoc 'INS_PT beam))))
        (setq len (if (cdr (assoc 'LENGTH beam)) (cdr (assoc 'LENGTH beam)) 0.0))
        (if (or (null min-x) (< x min-x)) (setq min-x x))
        (if (or (null max-x) (> (+ x len) max-x)) (setq max-x (+ x len)))
        (if (or (null min-y) (< y min-y)) (setq min-y y))
        (if (or (null max-y) (> y max-y)) (setq max-y y)))))
  (list
    (cons 'MIN_X (if min-x min-x 0.0))
    (cons 'MAX_X (if max-x max-x 0.0))
    (cons 'MIN_Y (if min-y min-y 0.0))
    (cons 'MAX_Y (if max-y max-y 0.0))))

;;;=====================================================================
;;; 12. НОРМАЛИЗАЦИЯ ПАНЕЛЕЙ
;;;=====================================================================
(defun mk:normalize-panel (panel bounds / ins-pt h w xsc ysc new-x new-y min-x max-x min-y max-y)
  (setq ins-pt (cdr (assoc 'INS_PT panel))
        h      (if (cdr (assoc 'HEIGHT panel)) (cdr (assoc 'HEIGHT panel)) 0.0)
        w      (if (cdr (assoc 'WIDTH panel)) (cdr (assoc 'WIDTH panel)) 0.0)
        xsc    (if (cdr (assoc 'XSCALE panel)) (cdr (assoc 'XSCALE panel)) 1.0)
        ysc    (if (cdr (assoc 'YSCALE panel)) (cdr (assoc 'YSCALE panel)) 1.0)
        min-x  (cdr (assoc 'MIN_X bounds))
        max-x  (cdr (assoc 'MAX_X bounds))
        min-y  (cdr (assoc 'MIN_Y bounds))
        max-y  (cdr (assoc 'MAX_Y bounds)))
  (if ins-pt
    (progn
      (setq new-x (car ins-pt) new-y (cadr ins-pt))
      (if (< xsc 0.0)
        (progn (setq new-x (+ new-x w))
               (prompt (strcat "\n  [WARN] Панель скорректирована по X: " (rtos new-x 2 1)))))
      (if (< ysc 0.0)
        (progn (setq new-y (+ new-y h))
               (prompt (strcat "\n  [WARN] Панель скорректирована по Y: " (rtos new-y 2 1)))))
      (if (> (+ new-x w) (+ max-x *mk:tol-skew*))
        (progn (setq new-x (- new-x w))
               (prompt (strcat "\n  [WARN] Панель за правой границей: " (rtos new-x 2 1)))))
      (if (> (+ new-y h) (+ max-y *mk:tol-skew*))
        (progn (setq new-y (- new-y h))
               (prompt (strcat "\n  [WARN] Панель за верхней границей: " (rtos new-y 2 1)))))
      (setq panel (mk:rec-put panel 'INS_PT (list new-x new-y 0.0)))))
  panel)

;;;=====================================================================
;;; 13. ПРОВЕРКИ И ВАЛИДАЦИЯ
;;;=====================================================================
(defun mk:check-duplicates (elements / i j pt1 pt2 tol count)
  (setq tol *mk:tol-adjacency* count 0 i 0)
  (foreach el1 elements
    (setq j 0)
    (foreach el2 elements
      (if (> j i)
        (progn
          (setq pt1 (cdr (assoc 'INS_PT el1))
                pt2 (cdr (assoc 'INS_PT el2)))
          (if (and pt1 pt2
                   (< (abs (- (car pt1) (car pt2))) tol)
                   (< (abs (- (cadr pt1) (cadr pt2))) tol))
            (progn
              (prompt (strcat "\n  [ERROR] Дубликат: " (cdr (assoc 'TYPE el1))
                              " в точке " (rtos (car pt1) 2 1) "," (rtos (cadr pt1) 2 1)))
              (setq count (1+ count))))))
      (setq j (1+ j)))
    (setq i (1+ i)))
  (if (> count 0)
    (prompt (strcat "\n  [ИТОГО] Дубликатов: " (itoa count))))
  count)

(defun mk:check-overlaps (panels / i j count x1 y1 w1 h1 x2 y2 w2 h2)
  (setq count 0 i 0)
  (foreach p1 panels
    (setq j 0)
    (foreach p2 panels
      (if (> j i)
        (progn
          (setq x1 (car (cdr (assoc 'INS_PT p1)))
                y1 (cadr (cdr (assoc 'INS_PT p1)))
                w1 (if (cdr (assoc 'WIDTH p1)) (cdr (assoc 'WIDTH p1)) 0.0)
                h1 (if (cdr (assoc 'HEIGHT p1)) (cdr (assoc 'HEIGHT p1)) 0.0))
          (setq x2 (car (cdr (assoc 'INS_PT p2)))
                y2 (cadr (cdr (assoc 'INS_PT p2)))
                w2 (if (cdr (assoc 'WIDTH p2)) (cdr (assoc 'WIDTH p2)) 0.0)
                h2 (if (cdr (assoc 'HEIGHT p2)) (cdr (assoc 'HEIGHT p2)) 0.0))
          (if (and (< x1 (+ x2 w2)) (> (+ x1 w1) x2)
                   (< y1 (+ y2 h2)) (> (+ y1 h1) y2))
            (progn
              (prompt (strcat "\n  [ERROR] Перекрытие: " (cdr (assoc 'TYPE p1))
                              " и " (cdr (assoc 'TYPE p2))
                              " в области " (rtos x1 2 1) "," (rtos y1 2 1)))
              (setq count (1+ count))))))
      (setq j (1+ j)))
    (setq i (1+ i)))
  (if (> count 0)
    (prompt (strcat "\n  [ИТОГО] Перекрытий: " (itoa count))))
  count)

(defun mk:check-out-of-grid (elements bounds / min-x max-x min-y max-y count x y)
  (setq min-x (cdr (assoc 'MIN_X bounds))
        max-x (cdr (assoc 'MAX_X bounds))
        min-y (cdr (assoc 'MIN_Y bounds))
        max-y (cdr (assoc 'MAX_Y bounds))
        count 0)
  (foreach el elements
    (if (cdr (assoc 'INS_PT el))
      (progn
        (setq x (car (cdr (assoc 'INS_PT el)))
              y (cadr (cdr (assoc 'INS_PT el))))
        (if (or (< x (- min-x *mk:tol-skew*)) (> x (+ max-x *mk:tol-skew*))
                (< y (- min-y *mk:tol-skew*)) (> y (+ max-y *mk:tol-skew*)))
          (progn
            (prompt (strcat "\n  [WARN] Вне сетки: " (cdr (assoc 'TYPE el))
                            " в точке " (rtos x 2 1) "," (rtos y 2 1)))
            (setq count (1+ count)))))))
  (if (> count 0)
    (prompt (strcat "\n  [ИТОГО] Вне сетки: " (itoa count))))
  count)

(defun mk:identify-protruding-posts (posts bounds / min-y max-y new-posts protruding-count x y len)
  (setq min-y (cdr (assoc 'MIN_Y bounds))
        max-y (cdr (assoc 'MAX_Y bounds))
        new-posts nil protruding-count 0)
  (foreach post posts
    (if (cdr (assoc 'INS_PT post))
      (progn
        (setq x (car (cdr (assoc 'INS_PT post))))
        (setq y (cadr (cdr (assoc 'INS_PT post))))
        (setq len (if (cdr (assoc 'LENGTH post)) (cdr (assoc 'LENGTH post)) 0.0))
        (if (or (< y (- min-y *mk:tol-skew*))
                (> (+ y len) (+ max-y *mk:tol-skew*)))
          (progn
            (prompt (strcat "\n  [INFO] Выступающая стойка: " (rtos x 2 1) "," (rtos y 2 1)))
            (setq post (mk:rec-put post 'PROTRUDING t))
            (setq protruding-count (1+ protruding-count)))
          (setq post (mk:rec-put post 'PROTRUDING nil)))
        (setq new-posts (cons post new-posts)))
      (setq new-posts (cons post new-posts))))
  (list (reverse new-posts) protruding-count))

;; Валидация ригеля — проверка ОБОИХ концов.
;; Ригель вставляется с отступом от оси стойки (ширина профиля), поэтому
;; допуск по X = *mk:offset-fill* + *mk:tol-adjacency*, а не голый допуск 5 мм.
(defun mk:post-span (post / py plen)
  (setq py   (cadr (cdr (assoc 'INS_PT post)))
        plen (if (cdr (assoc 'LENGTH post)) (cdr (assoc 'LENGTH post)) 0.0))
  (list (- py *mk:tol-adjacency*) (+ py plen *mk:tol-adjacency*)))

(defun mk:beam-touches-post? (bx by post / px span tol)
  (setq tol  (+ *mk:offset-fill* *mk:tol-adjacency*)
        px   (car (cdr (assoc 'INS_PT post)))
        span (mk:post-span post))
  (and (<= (abs (- bx px)) tol)
       (>= by (car span))
       (<= by (cadr span))))

(defun mk:validate-beam (beam posts beams / has-connection bx by bx2 ox oy beam-len)
  (setq has-connection nil)
  (setq beam-len (if (cdr (assoc 'LENGTH beam)) (cdr (assoc 'LENGTH beam)) 0.0))
  (setq bx  (car (cdr (assoc 'INS_PT beam)))
        by  (cadr (cdr (assoc 'INS_PT beam)))
        bx2 (+ bx beam-len))
  (foreach post posts
    (if (and (cdr (assoc 'INS_PT post)) (not has-connection))
      (if (or (mk:beam-touches-post? bx  by post)
              (mk:beam-touches-post? bx2 by post))
        (setq has-connection t))))
  (if (not has-connection)
    (foreach other-beam beams
      (if (and (not (eq beam other-beam))
               (cdr (assoc 'INS_PT other-beam))
               (not has-connection))
        (progn
          (setq ox (car (cdr (assoc 'INS_PT other-beam)))
                oy (cadr (cdr (assoc 'INS_PT other-beam))))
          (if (or (and (< (abs (- bx ox)) *mk:tol-adjacency*)
                       (< (abs (- by oy)) *mk:tol-adjacency*))
                  (and (< (abs (- bx2 ox)) *mk:tol-adjacency*)
                       (< (abs (- by oy)) *mk:tol-adjacency*)))
            (setq has-connection t))))))
  (if (not has-connection)
    (prompt (strcat "\n  [ERROR] Ригель без примыкания: "
                    (rtos bx 2 1) "," (rtos by 2 1))))
  has-connection)

(defun mk:validate-fill (fill posts / left-post right-post)
  (setq left-post (mk:find-left-post fill posts)
        right-post (mk:find-right-post fill posts))
  (if (or (null left-post) (null right-post))
    (progn
      (prompt (strcat "\n  [ERROR] Заполнение вне ячейки: "
                      (rtos (car (cdr (assoc 'INS_PT fill))) 2 1) ","
                      (rtos (cadr (cdr (assoc 'INS_PT fill))) 2 1)))
      nil)
    t))

;;;=====================================================================
;;; 14. ПОСТРОЕНИЕ ТОПОЛОГИИ СВЯЗЕЙ
;;;=====================================================================
(defun mk:find-left-post (panel posts / panel-x left-post best-dist dist post-x)
  (setq panel-x (car (cdr (assoc 'INS_PT panel)))
        left-post nil best-dist nil)
  (foreach post posts
    (setq post-x (car (cdr (assoc 'INS_PT post))))
    (if (< post-x panel-x)
      (progn
        (setq dist (- panel-x post-x))
        (if (and (>= dist (- *mk:offset-fill* *mk:tol-skew*))
                 (<= dist (+ *mk:offset-fill* *mk:tol-skew*)))
          (if (or (null best-dist) (< dist best-dist))
            (progn (setq best-dist dist) (setq left-post post)))))))
  left-post)

(defun mk:find-right-post (panel posts / panel-x panel-w right-post best-dist dist post-x)
  (setq panel-x (car (cdr (assoc 'INS_PT panel)))
        panel-w (if (cdr (assoc 'WIDTH panel)) (cdr (assoc 'WIDTH panel)) 0.0)
        right-post nil best-dist nil)
  (foreach post posts
    (setq post-x (car (cdr (assoc 'INS_PT post))))
    (if (> post-x (+ panel-x panel-w))
      (progn
        (setq dist (- post-x (+ panel-x panel-w)))
        (if (and (>= dist (- *mk:offset-fill* *mk:tol-skew*))
                 (<= dist (+ *mk:offset-fill* *mk:tol-skew*)))
          (if (or (null best-dist) (< dist best-dist))
            (progn (setq best-dist dist) (setq right-post post)))))))
  right-post)

(defun mk:find-connected-posts (panel posts / left right)
  (setq left  (mk:find-left-post panel posts)
        right (mk:find-right-post panel posts))
  (list (cons 'LEFT left) (cons 'RIGHT right)))

(defun mk:find-left-elements (post panels beams / post-x left-list panel-x panel-w beam-x)
  (setq post-x (car (cdr (assoc 'INS_PT post))) left-list nil)
  (foreach panel panels
    (if (cdr (assoc 'INS_PT panel))
      (progn
        (setq panel-x (car (cdr (assoc 'INS_PT panel)))
              panel-w (if (cdr (assoc 'WIDTH panel)) (cdr (assoc 'WIDTH panel)) 0.0))
        (if (and (<= (+ panel-x panel-w) (+ post-x *mk:tol-skew*))
                 (>= (+ panel-x panel-w) (- post-x 5000.0)))
          (setq left-list (cons panel left-list))))))
  (foreach beam beams
    (if (cdr (assoc 'INS_PT beam))
      (progn
        (setq beam-x (car (cdr (assoc 'INS_PT beam))))
        (if (and (<= beam-x (+ post-x *mk:tol-skew*))
                 (>= beam-x (- post-x 5000.0)))
          (setq left-list (cons beam left-list))))))
  (reverse left-list))

(defun mk:find-right-elements (post panels beams / post-x right-list panel-x beam-x)
  (setq post-x (car (cdr (assoc 'INS_PT post))) right-list nil)
  (foreach panel panels
    (if (cdr (assoc 'INS_PT panel))
      (progn
        (setq panel-x (car (cdr (assoc 'INS_PT panel))))
        (if (and (>= panel-x (- post-x *mk:tol-skew*))
                 (<= panel-x (+ post-x 5000.0)))
          (setq right-list (cons panel right-list))))))
  (foreach beam beams
    (if (cdr (assoc 'INS_PT beam))
      (progn
        (setq beam-x (car (cdr (assoc 'INS_PT beam))))
        (if (and (>= beam-x (- post-x *mk:tol-skew*))
                 (<= beam-x (+ post-x 5000.0)))
          (setq right-list (cons beam right-list))))))
  (reverse right-list))

;; Панель считается примыкающей, только если она перекрывается с ригелем по X
(defun mk:beam-panel-overlap? (beam panel / br pr)
  (setq br (mk:el-xrange beam)
        pr (mk:el-xrange panel))
  (and (< (car br) (- (cadr pr) *mk:tol-adjacency*))
       (> (cadr br) (+ (car pr) *mk:tol-adjacency*))))

(defun mk:find-top-element (beam panels / beam-y top-elem best-dist dist panel-y)
  (setq beam-y (cadr (cdr (assoc 'INS_PT beam)))
        top-elem nil best-dist nil)
  (foreach panel panels
    (if (and (cdr (assoc 'INS_PT panel))
             (mk:beam-panel-overlap? beam panel))
      (progn
        (setq panel-y (cadr (cdr (assoc 'INS_PT panel))))
        (if (> panel-y beam-y)
          (progn
            (setq dist (- panel-y beam-y))
            (if (and (>= dist (- *mk:offset-fill* *mk:tol-skew*))
                     (<= dist (+ *mk:offset-fill* *mk:tol-skew*)))
              (if (or (null best-dist) (< dist best-dist))
                (progn (setq best-dist dist) (setq top-elem panel)))))))))
  top-elem)

(defun mk:find-bot-element (beam panels / beam-y bot-elem best-dist dist panel-y panel-h)
  (setq beam-y (cadr (cdr (assoc 'INS_PT beam)))
        bot-elem nil best-dist nil)
  (foreach panel panels
    (if (and (cdr (assoc 'INS_PT panel))
             (mk:beam-panel-overlap? beam panel))
      (progn
        (setq panel-y (cadr (cdr (assoc 'INS_PT panel)))
              panel-h (if (cdr (assoc 'HEIGHT panel)) (cdr (assoc 'HEIGHT panel)) 0.0))
        (if (< (+ panel-y panel-h) beam-y)
          (progn
            (setq dist (- beam-y (+ panel-y panel-h)))
            (if (and (>= dist (- *mk:offset-fill* *mk:tol-skew*))
                     (<= dist (+ *mk:offset-fill* *mk:tol-skew*)))
              (if (or (null best-dist) (< dist best-dist))
                (progn (setq best-dist dist) (setq bot-elem panel)))))))))
  bot-elem)

(defun mk:build-topology (posts beams fills windows doors bounds /
                          all-panels new-posts new-beams new-fills
                          new-windows new-doors post beam panel)
  (setq all-panels (append fills windows doors))
  (setq new-fills nil)
  (foreach panel fills
    (setq panel (mk:normalize-panel panel bounds))
    (setq panel (mk:rec-put panel 'CONN_POSTS (mk:find-connected-posts panel posts)))
    (setq new-fills (cons panel new-fills)))
  (setq fills (reverse new-fills))
  (setq new-windows nil)
  (foreach panel windows
    (setq panel (mk:normalize-panel panel bounds))
    (setq panel (mk:rec-put panel 'CONN_POSTS (mk:find-connected-posts panel posts)))
    (setq new-windows (cons panel new-windows)))
  (setq windows (reverse new-windows))
  (setq new-doors nil)
  (foreach panel doors
    (setq panel (mk:normalize-panel panel bounds))
    (setq panel (mk:rec-put panel 'CONN_POSTS (mk:find-connected-posts panel posts)))
    (setq new-doors (cons panel new-doors)))
  (setq doors (reverse new-doors))
  (setq all-panels (append fills windows doors))
  (setq new-posts nil)
  (foreach post posts
    (setq post (mk:rec-put post 'LEFT_CONN  (mk:find-left-elements post all-panels beams)))
    (setq post (mk:rec-put post 'RIGHT_CONN (mk:find-right-elements post all-panels beams)))
    (setq new-posts (cons post new-posts)))
  (setq posts (reverse new-posts))
  (setq new-beams nil)
  (foreach beam beams
    (setq beam (mk:rec-put beam 'TOP_ELEM (mk:find-top-element beam all-panels)))
    (setq beam (mk:rec-put beam 'BOT_ELEM (mk:find-bot-element beam all-panels)))
    (setq new-beams (cons beam new-beams)))
  (setq beams (reverse new-beams))
  (list posts beams fills windows doors))

;;;=====================================================================
;;; 15. ВИЗУАЛИЗАЦИЯ (БАЗОВАЯ)
;;;=====================================================================
;; Все создаваемые объекты копятся, чтобы собрать их в группу
(setq *mk:drawn* nil)

(defun mk:emk (dxf / e)
  (setq e (entmakex dxf))
  (if e (setq *mk:drawn* (cons e *mk:drawn*)))
  e)

;; Группа объектов модели — чтобы можно было удалить одним выбором
;; (донор: mark:ar-make-group из MarkZ)
(defun mk:make-group (name enames / doc groups old grp arr i n)
  (if (and enames (> (length enames) 1))
    (progn
      (setq doc    (mk:ax-get (vlax-get-acad-object) "ActiveDocument")
            groups (if doc (mk:ax-get doc "Groups") nil))
      (if (null groups)
        (prompt "\n  [WARN] Коллекция Groups недоступна — группа не создана.")
        (progn
          (setq old (vl-catch-all-apply 'vlax-invoke-method
                      (list groups "Item" name)))
          (if (and old (not (vl-catch-all-error-p old)))
            (vl-catch-all-apply 'vlax-invoke-method (list old "Delete")))
          (setq grp (vl-catch-all-apply 'vlax-invoke-method
                      (list groups "Add" name)))
          (if (or (vl-catch-all-error-p grp) (null grp))
            (prompt (strcat "\n  [WARN] Группа \"" name "\" не создана."))
            (progn
              (setq n   (length enames)
                    arr (vlax-make-safearray vlax-vbObject (cons 0 (1- n)))
                    i   0)
              (foreach e enames
                (vlax-safearray-put-element arr i (vlax-ename->vla-object e))
                (setq i (1+ i)))
              (vl-catch-all-apply 'vlax-invoke-method
                (list grp "AppendItems" arr))
              (prompt (strcat "\n[OK] Группа \"" name "\": "
                              (itoa n) " объект(ов)"))))))))
  nil)
(defun mk:draw-post (post / pt len)
  (setq pt  (cdr (assoc 'INS_PT post))
        len (if (cdr (assoc 'LENGTH post)) (cdr (assoc 'LENGTH post)) 0.0))
  (if pt
    (progn
      (mk:emk (list '(0 . "LINE")
                      (cons 10 (list (car pt) (cadr pt) 0.0))
                      (cons 11 (list (car pt) (+ (cadr pt) len) 0.0))
                      '(62 . 1)))
      (mk:emk (list '(0 . "POINT")
                      (cons 10 (list (car pt) (cadr pt) 0.0))
                      '(62 . 2))))))

(defun mk:draw-beam (beam / pt len)
  (setq pt  (cdr (assoc 'INS_PT beam))
        len (if (cdr (assoc 'LENGTH beam)) (cdr (assoc 'LENGTH beam)) 0.0))
  (if pt
    (mk:emk (list '(0 . "LINE")
                    (cons 10 (list (car pt) (cadr pt) 0.0))
                    (cons 11 (list (+ (car pt) len) (cadr pt) 0.0))
                    '(62 . 3)))))

(defun mk:draw-panel (panel / pt h w)
  (setq pt (cdr (assoc 'INS_PT panel))
        h  (if (cdr (assoc 'HEIGHT panel)) (cdr (assoc 'HEIGHT panel)) 0.0)
        w  (if (cdr (assoc 'WIDTH panel)) (cdr (assoc 'WIDTH panel)) 0.0))
  (if (and pt (> h 0) (> w 0))
    (mk:emk (list '(0 . "LWPOLYLINE") '(90 . 4) '(70 . 1) '(62 . 5)
                    (cons 10 (list (car pt) (cadr pt)))
                    (cons 10 (list (+ (car pt) w) (cadr pt)))
                    (cons 10 (list (+ (car pt) w) (+ (cadr pt) h)))
                    (cons 10 (list (car pt) (+ (cadr pt) h)))))))

(defun mk:draw-bounds (bounds / min-x max-x min-y max-y)
  (setq min-x (cdr (assoc 'MIN_X bounds))
        max-x (cdr (assoc 'MAX_X bounds))
        min-y (cdr (assoc 'MIN_Y bounds))
        max-y (cdr (assoc 'MAX_Y bounds)))
  (mk:emk (list '(0 . "LWPOLYLINE") '(90 . 4) '(70 . 1) '(62 . 2)
                  (cons 10 (list min-x min-y))
                  (cons 10 (list max-x min-y))
                  (cons 10 (list max-x max-y))
                  (cons 10 (list min-x max-y)))))

(defun mk:draw-model (posts beams fills windows doors bounds / layer prev-layer)
  (setq layer "MK_MODEL")
  (setq *mk:drawn* nil)
  (if (null (tblsearch "LAYER" layer))
    (command "_.LAYER" "_N" layer "_C" "7" layer ""))
  (setq prev-layer (getvar "CLAYER"))
  (setvar "CLAYER" layer)
  (mk:draw-bounds bounds)
  (foreach post posts (mk:draw-post post))
  (foreach beam beams (mk:draw-beam beam))
  (foreach panel (append fills windows doors) (mk:draw-panel panel))
  (setvar "CLAYER" prev-layer)
  (prompt (strcat "\n[OK] 2D модель отрисована на слое: " layer))
  (mk:make-group *mk:group-model* (reverse *mk:drawn*)))

;;;=====================================================================
;;; 16. ТЕСТОВАЯ ВИЗУАЛИЗАЦИЯ
;;;=====================================================================
(defun mk:draw-fill-cross (fill / pt h w x1 y1 x2 y2)
  (setq pt (cdr (assoc 'INS_PT fill))
        h  (if (cdr (assoc 'HEIGHT fill)) (cdr (assoc 'HEIGHT fill)) 0.0)
        w  (if (cdr (assoc 'WIDTH fill)) (cdr (assoc 'WIDTH fill)) 0.0))
  (if (and pt (> h 0) (> w 0))
    (progn
      (setq x1 (car pt) y1 (cadr pt)
            x2 (+ (car pt) w) y2 (+ (cadr pt) h))
      (mk:emk (list '(0 . "LWPOLYLINE") '(90 . 4) '(70 . 1) '(62 . 5)
                      (cons 10 (list x1 y1))
                      (cons 10 (list x2 y1))
                      (cons 10 (list x2 y2))
                      (cons 10 (list x1 y2))))
      (mk:emk (list '(0 . "LINE")
                      (cons 10 (list x1 y1 0.0))
                      (cons 11 (list x2 y2 0.0))
                      '(62 . 5)))
      (mk:emk (list '(0 . "LINE")
                      (cons 10 (list x2 y1 0.0))
                      (cons 11 (list x1 y2 0.0))
                      '(62 . 5))))))

(defun mk:draw-window-label (win / pt h w x1 y1 x2 y2 cx cy name)
  (setq pt (cdr (assoc 'INS_PT win))
        h  (if (cdr (assoc 'HEIGHT win)) (cdr (assoc 'HEIGHT win)) 0.0)
        w  (if (cdr (assoc 'WIDTH win)) (cdr (assoc 'WIDTH win)) 0.0)
        name (if (cdr (assoc 'NAME win)) (cdr (assoc 'NAME win)) "ОКНО"))
  (if (and pt (> h 0) (> w 0))
    (progn
      (setq x1 (car pt) y1 (cadr pt)
            x2 (+ (car pt) w) y2 (+ (cadr pt) h)
            cx (/ (+ x1 x2) 2.0) cy (/ (+ y1 y2) 2.0))
      (mk:emk (list '(0 . "LWPOLYLINE") '(90 . 4) '(70 . 1) '(62 . 4)
                      (cons 10 (list x1 y1))
                      (cons 10 (list x2 y1))
                      (cons 10 (list x2 y2))
                      (cons 10 (list x1 y2))))
      (mk:emk (list '(0 . "TEXT")
                      (cons 10 (list cx cy 0.0))
                      (cons 40 100.0) (cons 1 "ОКНО")
                      (cons 72 1) (cons 11 (list cx cy 0.0))
                      '(62 . 4)))
      (if (and name (mk:strp name))
        (mk:emk (list '(0 . "TEXT")
                        (cons 10 (list cx (- cy 120.0) 0.0))
                        (cons 40 80.0) (cons 1 name)
                        (cons 72 1) (cons 11 (list cx (- cy 120.0) 0.0))
                        '(62 . 4)))))))

(defun mk:draw-door-label (door / pt h w x1 y1 x2 y2 cx cy name)
  (setq pt (cdr (assoc 'INS_PT door))
        h  (if (cdr (assoc 'HEIGHT door)) (cdr (assoc 'HEIGHT door)) 0.0)
        w  (if (cdr (assoc 'WIDTH door)) (cdr (assoc 'WIDTH door)) 0.0)
        name (if (cdr (assoc 'NAME door)) (cdr (assoc 'NAME door)) "ДВЕРЬ"))
  (if (and pt (> h 0) (> w 0))
    (progn
      (setq x1 (car pt) y1 (cadr pt)
            x2 (+ (car pt) w) y2 (+ (cadr pt) h)
            cx (/ (+ x1 x2) 2.0) cy (/ (+ y1 y2) 2.0))
      (mk:emk (list '(0 . "LWPOLYLINE") '(90 . 4) '(70 . 1) '(62 . 6)
                      (cons 10 (list x1 y1))
                      (cons 10 (list x2 y1))
                      (cons 10 (list x2 y2))
                      (cons 10 (list x1 y2))))
      (mk:emk (list '(0 . "TEXT")
                      (cons 10 (list cx cy 0.0))
                      (cons 40 100.0) (cons 1 "ДВЕРЬ")
                      (cons 72 1) (cons 11 (list cx cy 0.0))
                      '(62 . 6)))
      (if (and name (mk:strp name))
        (mk:emk (list '(0 . "TEXT")
                        (cons 10 (list cx (- cy 120.0) 0.0))
                        (cons 40 80.0) (cons 1 name)
                        (cons 72 1) (cons 11 (list cx (- cy 120.0) 0.0))
                        '(62 . 6)))))))

(defun mk:draw-grid (posts beams / pt len)
  (foreach post posts
    (setq pt  (cdr (assoc 'INS_PT post))
          len (if (cdr (assoc 'LENGTH post)) (cdr (assoc 'LENGTH post)) 0.0))
    (if pt
      (progn
        (if (cdr (assoc 'PROTRUDING post))
          (mk:emk (list '(0 . "LINE")
                          (cons 10 (list (car pt) (cadr pt) 0.0))
                          (cons 11 (list (car pt) (+ (cadr pt) len) 0.0))
                          '(62 . 1) (cons 6 "DASHED")))
          (mk:emk (list '(0 . "LINE")
                          (cons 10 (list (car pt) (cadr pt) 0.0))
                          (cons 11 (list (car pt) (+ (cadr pt) len) 0.0))
                          '(62 . 1))))
        (mk:emk (list '(0 . "POINT")
                        (cons 10 (list (car pt) (cadr pt) 0.0))
                        '(62 . 2))))))
  (foreach beam beams
    (setq pt  (cdr (assoc 'INS_PT beam))
          len (if (cdr (assoc 'LENGTH beam)) (cdr (assoc 'LENGTH beam)) 0.0))
    (if pt
      (mk:emk (list '(0 . "LINE")
                      (cons 10 (list (car pt) (cadr pt) 0.0))
                      (cons 11 (list (+ (car pt) len) (cadr pt) 0.0))
                      '(62 . 3))))))

(defun mk:draw-test-model (posts beams fills windows doors bounds / layer prev-layer)
  (setq layer "MK_TEST_MODEL")
  (setq *mk:drawn* nil)
  (if (null (tblsearch "LAYER" layer))
    (command "_.LAYER" "_N" layer "_C" "7" layer ""))
  (setq prev-layer (getvar "CLAYER"))
  (setvar "CLAYER" layer)
  (mk:draw-bounds bounds)
  (mk:draw-grid posts beams)
  (foreach fill fills (mk:draw-fill-cross fill))
  (foreach win windows (mk:draw-window-label win))
  (foreach door doors (mk:draw-door-label door))
  (setvar "CLAYER" prev-layer)
  (prompt (strcat "\n[OK] Тестовая модель отрисована на слое: " layer))
  (mk:make-group *mk:group-test* (reverse *mk:drawn*)))

;;;=====================================================================
;;; 17. ВЫВОД ДАННЫХ В ФАЙЛ
;;;=====================================================================
(defun mk:dump-data (vitrage bounds posts beams fills windows doors / f fn)
  (setq fn *mk:out-file*)
  (setq f (open fn "w"))
  (if f
    (progn
      (write-line "=== ДАННЫЕ ВИТРАЖА (MarkZV) ===" f)
      (write-line "" f)
      (write-line "--- ГРАНИЦЫ ВИТРАЖА ---" f)
      (write-line (strcat "  MIN_X: " (rtos (cdr (assoc 'MIN_X bounds)) 2 1)) f)
      (write-line (strcat "  MAX_X: " (rtos (cdr (assoc 'MAX_X bounds)) 2 1)) f)
      (write-line (strcat "  MIN_Y: " (rtos (cdr (assoc 'MIN_Y bounds)) 2 1)) f)
      (write-line (strcat "  MAX_Y: " (rtos (cdr (assoc 'MAX_Y bounds)) 2 1)) f)
      (write-line "" f)
      (write-line "--- АТРИБУТЫ ВИТРАЖА ---" f)
      (if vitrage
        (progn
          (write-line (strcat "  INS_PT: " (vl-prin1-to-string (cdr (assoc 'INS_PT vitrage)))) f)
          (write-line "  АТРИБУТЫ:" f)
          (foreach attr (cdr (assoc 'ATTRS vitrage))
            (write-line (strcat "    <" (car attr) "> = \"" (cdr attr) "\"") f)))
        (write-line "  [НЕ НАЙДЕН]" f))
      (write-line "" f)
      (write-line "--- СТОЙКИ ---" f)
      (foreach post posts
        (write-line (strcat "  INS_PT: " (vl-prin1-to-string (cdr (assoc 'INS_PT post)))) f)
        (write-line (strcat "  LENGTH: " (vl-prin1-to-string (cdr (assoc 'LENGTH post)))
                            "  (источник: " (if (cdr (assoc 'SIZE_SRC post)) (cdr (assoc 'SIZE_SRC post)) "НЕТ") ")") f)
        (write-line (strcat "  АРТИКУЛ: " (if (cdr (assoc 'ARTICLE post)) (cdr (assoc 'ARTICLE post)) "НЕТ")) f)
        (write-line (strcat "  PROTRUDING: " (if (cdr (assoc 'PROTRUDING post)) "ДА" "НЕТ")) f)
        (write-line (strcat "  СЛЕВА:  " (if (cdr (assoc 'LEFT_TYPES post))  (cdr (assoc 'LEFT_TYPES post))  "?")) f)
        (write-line (strcat "  СПРАВА: " (if (cdr (assoc 'RIGHT_TYPES post)) (cdr (assoc 'RIGHT_TYPES post)) "?")) f)
        (write-line (strcat "  СЕЧЕНИЕ: " (vl-prin1-to-string (cdr (assoc 'CROSS post)))) f)
        (write-line (strcat "  LEFT_CONN: " (itoa (length (cdr (assoc 'LEFT_CONN post))))) f)
        (write-line (strcat "  RIGHT_CONN: " (itoa (length (cdr (assoc 'RIGHT_CONN post))))) f)
        (write-line "" f))
      (write-line "--- РИГЕЛИ ---" f)
      (foreach beam beams
        (write-line (strcat "  INS_PT: " (vl-prin1-to-string (cdr (assoc 'INS_PT beam)))) f)
        (write-line (strcat "  LENGTH: " (vl-prin1-to-string (cdr (assoc 'LENGTH beam)))
                            "  (источник: " (if (cdr (assoc 'SIZE_SRC beam)) (cdr (assoc 'SIZE_SRC beam)) "НЕТ") ")") f)
        (write-line (strcat "  АРТИКУЛ: " (if (cdr (assoc 'ARTICLE beam)) (cdr (assoc 'ARTICLE beam)) "НЕТ")) f)
        (write-line (strcat "  TOP_ELEM: " (if (cdr (assoc 'TOP_ELEM beam)) "Есть" "Нет")) f)
        (write-line (strcat "  BOT_ELEM: " (if (cdr (assoc 'BOT_ELEM beam)) "Есть" "Нет")) f)
        (write-line (strcat "  СУФФИКС: " (if (and (cdr (assoc 'SUFFIX beam))
                                                   (> (strlen (cdr (assoc 'SUFFIX beam))) 0))
                                            (cdr (assoc 'SUFFIX beam)) "нет")) f)
        (write-line (strcat "  СЕЧЕНИЕ: " (vl-prin1-to-string (cdr (assoc 'CROSS beam)))) f)
        (write-line (strcat "  СТОЛБЕЦ: " (if (cdr (assoc 'BAY beam))
                                            (itoa (cdr (assoc 'BAY beam))) "?")) f)
        (write-line (strcat "  РАЗМЕР: " (if (and (cdr (assoc 'SIZE_MARK beam))
                                                  (> (strlen (cdr (assoc 'SIZE_MARK beam))) 0))
                                           (cdr (assoc 'SIZE_MARK beam)) "нет")) f)
        (write-line "" f))
      (write-line "--- ЗАПОЛНЕНИЯ ---" f)
      (foreach fill fills
        (write-line (strcat "  INS_PT: " (vl-prin1-to-string (cdr (assoc 'INS_PT fill)))) f)
        (write-line (strcat "  HEIGHT: " (vl-prin1-to-string (cdr (assoc 'HEIGHT fill)))) f)
        (write-line (strcat "  WIDTH: " (vl-prin1-to-string (cdr (assoc 'WIDTH fill)))) f)
        (write-line (strcat "  THICKNESS: " (if (cdr (assoc 'THICKNESS fill)) (cdr (assoc 'THICKNESS fill)) "НЕТ")) f)
        (write-line (strcat "  АРТИКУЛ: " (if (cdr (assoc 'ARTICLE fill)) (cdr (assoc 'ARTICLE fill)) "НЕТ")) f)
        (write-line "" f))
      (write-line "--- ОКНА ---" f)
      (foreach win windows
        (write-line (strcat "  INS_PT: " (vl-prin1-to-string (cdr (assoc 'INS_PT win)))) f)
        (write-line (strcat "  HEIGHT: " (vl-prin1-to-string (cdr (assoc 'HEIGHT win)))) f)
        (write-line (strcat "  WIDTH: " (vl-prin1-to-string (cdr (assoc 'WIDTH win)))) f)
        (write-line (strcat "  NAME: " (if (cdr (assoc 'NAME win)) (cdr (assoc 'NAME win)) "НЕТ")) f)
        (write-line "" f))
      (write-line "--- ДВЕРИ ---" f)
      (foreach door doors
        (write-line (strcat "  INS_PT: " (vl-prin1-to-string (cdr (assoc 'INS_PT door)))) f)
        (write-line (strcat "  HEIGHT: " (vl-prin1-to-string (cdr (assoc 'HEIGHT door)))) f)
        (write-line (strcat "  WIDTH: " (vl-prin1-to-string (cdr (assoc 'WIDTH door)))) f)
        (write-line (strcat "  NAME: " (if (cdr (assoc 'NAME door)) (cdr (assoc 'NAME door)) "НЕТ")) f)
        (write-line "" f))
      (close f)
      (prompt (strcat "\n[OK] Данные сохранены в: " fn)))
    (prompt "\n[ERROR] Не удалось создать файл.")))

;;;=====================================================================
;;; 18. АВТОЗАПОЛНЕНИЕ АТРИБУТОВ
;;;=====================================================================
(defun mk:set-attr (ename tag newval / sub data done)
  (setq sub (entnext ename) done nil)
  (while (and sub (not done))
    (setq data (entget sub))
    (cond
      ((= (cdr (assoc 0 data)) "ATTRIB")
       (if (mk:name= (cdr (assoc 2 data)) tag)
         (progn
           (entmod (subst (cons 1 newval) (assoc 1 data) data))
           (entupd ename)
           (setq done t))))
      ((= (cdr (assoc 0 data)) "SEQEND")
       (setq sub nil)))
    (if (and sub (not done)) (setq sub (entnext sub))))
  done)

;; Ключ размера с допуском *mk:tol-size* (0.5 мм):
;; 1500.4 и 1500.6 должны попадать в одну группу.
(defun mk:size-key (v / step)
  (if (numberp v)
    (progn
      (setq step (* 2.0 *mk:tol-size*))
      (rtos (* step (fix (+ (/ (float v) step) 0.5))) 2 1))
    "0"))

;;;---------------------------------------------------------------------
;;; 18a. СЛУЖЕБНОЕ: сортировка, фильтры, уникальные ключи
;;;---------------------------------------------------------------------
;; Порядок обнаружения: слева направо, снизу вверх
(defun mk:xy< (a b / xa xb ya yb)
  (setq xa (car  (cdr (assoc 'INS_PT a)))
        xb (car  (cdr (assoc 'INS_PT b)))
        ya (cadr (cdr (assoc 'INS_PT a)))
        yb (cadr (cdr (assoc 'INS_PT b))))
  (cond
    ((null xa) nil)
    ((null xb) t)
    ((< xa (- xb *mk:tol-adjacency*)) t)
    ((> xa (+ xb *mk:tol-adjacency*)) nil)
    (t (< ya yb))))

(defun mk:insert-xy (sorted el / out done)
  (setq out nil done nil)
  (foreach x sorted
    (if (and (not done) (mk:xy< el x))
      (progn (setq out (cons el out)) (setq done t)))
    (setq out (cons x out)))
  (if (not done) (setq out (cons el out)))
  (reverse out))

(defun mk:sort-xy (lst / out el)
  (setq out nil)
  (foreach el lst (setq out (mk:insert-xy out el)))
  out)

(defun mk:filter-by (elements key val / out)
  (setq out nil)
  (foreach el elements
    (if (equal (mk:rec-get el key) val) (setq out (cons el out))))
  (reverse out))

(defun mk:reject-by (elements key val / out)
  (setq out nil)
  (foreach el elements
    (if (not (equal (mk:rec-get el key) val)) (setq out (cons el out))))
  (reverse out))

(defun mk:unique-keys (elements key / out v)
  (setq out nil)
  (foreach el elements
    (setq v (mk:rec-get el key))
    (if (not (member v out)) (setq out (cons v out))))
  (reverse out))

(defun mk:join (lst sep / out first)
  (setq out "" first t)
  (foreach s lst
    (if first (setq out s first nil) (setq out (strcat out sep s))))
  out)

;;;---------------------------------------------------------------------
;;; 18b. ТИП ПРИМЫКАНИЯ СЛЕВА / СПРАВА ОТ СТОЙКИ
;;;---------------------------------------------------------------------
;; Оси стоек: уникальные X с допуском кластеризации
(defun mk:post-axes (posts / xs sorted out last)
  (setq xs nil)
  (foreach p posts
    (if (cdr (assoc 'INS_PT p))
      (setq xs (cons (car (cdr (assoc 'INS_PT p))) xs))))
  (setq sorted (vl-sort xs '<) out nil last nil)
  (foreach v sorted
    (if (or (null out) (> (- v last) *mk:tol-adjacency*))
      (progn (setq out (cons v out)) (setq last v))))
  (reverse out))

(defun mk:axis-prev (axes x / out)
  (setq out nil)
  (foreach a axes
    (if (< a (- x *mk:tol-adjacency*)) (setq out a)))
  out)

(defun mk:axis-next (axes x / out)
  (setq out nil)
  (foreach a (reverse axes)
    (if (> a (+ x *mk:tol-adjacency*)) (setq out a)))
  out)

(defun mk:el-xrange (el / x w)
  (setq x (car (cdr (assoc 'INS_PT el))))
  (if (= (cdr (assoc 'TYPE el)) "РИГЕЛЬ")
    (setq w (if (cdr (assoc 'LENGTH el)) (cdr (assoc 'LENGTH el)) 0.0))
    (setq w (if (cdr (assoc 'WIDTH el))  (cdr (assoc 'WIDTH el))  0.0)))
  (list x (+ x w)))

(defun mk:el-yrange (el / y h)
  (setq y (cadr (cdr (assoc 'INS_PT el))))
  (if (= (cdr (assoc 'TYPE el)) "РИГЕЛЬ")
    (setq h 0.0)
    (setq h (if (cdr (assoc 'HEIGHT el)) (cdr (assoc 'HEIGHT el)) 0.0)))
  (list y (+ y h)))

(defun mk:overlap? (lo1 hi1 lo2 hi2)
  (and (<= lo1 (+ hi2 *mk:tol-adjacency*))
       (>= hi1 (- lo2 *mk:tol-adjacency*))))

;; Типы элементов в пролёте [lo .. hi], перекрывающихся со стойкой по высоте
(defun mk:side-types (post elements lo hi / out typ xr yr cx span)
  (setq span (mk:post-span post) out nil)
  (if (and lo hi)
    (foreach el elements
      (if (cdr (assoc 'INS_PT el))
        (progn
          (setq xr  (mk:el-xrange el)
                yr  (mk:el-yrange el)
                cx  (/ (+ (car xr) (cadr xr)) 2.0)
                typ (cdr (assoc 'TYPE el)))
          (if (and (> cx (- lo *mk:tol-adjacency*))
                   (< cx (+ hi *mk:tol-adjacency*))
                   (mk:overlap? (car yr) (cadr yr) (car span) (cadr span))
                   (not (member typ out)))
            (setq out (cons typ out)))))))
  (if out (mk:join (vl-sort out '<) "+") "ПУСТО"))

;; Разметка стоек: LEFT_TYPES / RIGHT_TYPES / ORIENT / GROUP_KEY
(defun mk:annotate-posts (posts elements / axes out x lt rt art len canon)
  (setq axes (mk:post-axes posts) out nil)
  (foreach post posts
    (setq x   (car (cdr (assoc 'INS_PT post)))
          lt  (mk:side-types post elements (mk:axis-prev axes x) x)
          rt  (mk:side-types post elements x (mk:axis-next axes x))
          len (mk:size-key (mk:rec-get post 'LENGTH))
          art (if (mk:rec-get post 'ARTICLE) (mk:rec-get post 'ARTICLE) "БЕЗ_АРТИКУЛА"))
    ;; канонический ключ не зависит от того, зеркальна стойка или нет
    (setq canon (if (<= (strcase lt) (strcase rt))
                  (strcat lt ">" rt)
                  (strcat rt ">" lt)))
    (setq post (mk:rec-put post 'LEFT_TYPES  lt))
    (setq post (mk:rec-put post 'RIGHT_TYPES rt))
    (setq post (mk:rec-put post 'ORIENT      (strcat lt ">" rt)))
    (setq post (mk:rec-put post 'GROUP_KEY   (strcat len "_" art "_" canon)))
    (setq out (cons post out)))
  (reverse out))

;;;---------------------------------------------------------------------
;;; 18c. СУФФИКС РИГЕЛЯ
;;;---------------------------------------------------------------------
(defun mk:get-beam-suffix (beam all-panels doors bounds /
                           top-elem bot-elem top-type bot-type
                           top-thick bot-thick suffix beam-y door-y)
  (setq top-elem (cdr (assoc 'TOP_ELEM beam))
        bot-elem (cdr (assoc 'BOT_ELEM beam))
        suffix "")
  (setq top-type (if top-elem (cdr (assoc 'TYPE top-elem)) "ПУСТО"))
  (setq bot-type (if bot-elem (cdr (assoc 'TYPE bot-elem)) "ПУСТО"))
  (setq top-thick nil bot-thick nil)
  (if (and top-elem (= top-type "ЗАПОЛНЕНИЕ"))
    (setq top-thick (mk:numval (cdr (assoc 'THICKNESS top-elem)))))
  (if (and bot-elem (= bot-type "ЗАПОЛНЕНИЕ"))
    (setq bot-thick (mk:numval (cdr (assoc 'THICKNESS bot-elem)))))
  (cond
    ((and (= top-type "ОКНО") (= bot-type "ОКНО"))
     (setq suffix *mk:suffix-window-both*))
    ((or (= top-type "ОКНО") (= bot-type "ОКНО"))
     (setq suffix *mk:suffix-window-one*))
    ((or (= top-type "ДВЕРЬ") (= bot-type "ДВЕРЬ"))
     (setq suffix *mk:suffix-door-one*))
    ((and bot-thick top-thick
          (>= bot-thick *mk:thick-warm-min*) (<= bot-thick *mk:thick-warm-max*)
          (>= top-thick *mk:thick-cold-min*) (<= top-thick *mk:thick-cold-max*))
     (setq suffix *mk:suffix-warm-cold*))
    ((and bot-thick top-thick
          (>= bot-thick *mk:thick-cold-min*) (<= bot-thick *mk:thick-cold-max*)
          (>= top-thick *mk:thick-warm-min*) (<= top-thick *mk:thick-warm-max*))
     (setq suffix *mk:suffix-cold-warm*))
    (t (setq suffix "")))
  (if (and (cdr (assoc 'INS_PT beam)) doors)
    (progn
      (setq beam-y (cadr (cdr (assoc 'INS_PT beam))))
      (foreach door doors
        (if (cdr (assoc 'INS_PT door))
          (progn
            (setq door-y (cadr (cdr (assoc 'INS_PT door))))
            (if (<= (abs (- beam-y door-y)) *mk:tol-threshold*)
              (if (not (vl-string-search *mk:suffix-threshold* suffix))
                (setq suffix (strcat suffix *mk:suffix-threshold*)))))))))
  suffix)

;;;---------------------------------------------------------------------
;;; 18c-1. ВЕРТИКАЛЬНЫЕ СТОЛБЦЫ (ПРОЛЁТЫ) И МАРКЕР РАЗМЕРА ПРОФИЛЯ
;;;---------------------------------------------------------------------
;; Номер пролёта (1..N-1), в который попадает X; 0 — вне сетки стоек
(defun mk:bay-index (x axes / i idx n)
  (setq i 0 idx 0 n (length axes))
  (while (< (1+ i) n)
    (if (and (= idx 0)
             (> x (- (nth i axes) *mk:tol-adjacency*))
             (< x (+ (nth (1+ i) axes) *mk:tol-adjacency*)))
      (setq idx (1+ i)))
    (setq i (1+ i)))
  idx)

(defun mk:el-center-x (el / r)
  (setq r (mk:el-xrange el))
  (/ (+ (car r) (cadr r)) 2.0))

;; Уникальные пары (артикул . габарит сечения) в порядке появления
(defun mk:article-cross-pairs (elements / out art cross)
  (setq out nil)
  (foreach el elements
    (setq art   (if (mk:rec-get el 'ARTICLE) (mk:rec-get el 'ARTICLE) "БЕЗ_АРТИКУЛА")
          cross (mk:rec-get el 'CROSS))
    (if (null (assoc art out))
      (setq out (cons (cons art (if (numberp cross) cross 0.0)) out))))
  (reverse out))

;; Таблица артикул -> "м"/"б": по габариту сечения (минимальный/максимальный).
;; Если артикул один — таблица пустая (маркер не нужен).
(defun mk:size-mark-table (elements / pairs sorted out)
  (setq pairs (mk:article-cross-pairs elements) out nil)
  (if (> (length pairs) 1)
    (progn
      (setq sorted (vl-sort pairs '(lambda (a b) (< (cdr a) (cdr b)))))
      (setq out (list (cons (car (car sorted))          *mk:suffix-small*)
                      (cons (car (last sorted))         *mk:suffix-big*)))
      (if (> (length pairs) 2)
        (prompt (strcat "\n  [WARN] Артикулов ригелей: " (itoa (length pairs))
                        " — маркеры м/б присвоены только крайним по габариту.")))))
  out)

(defun mk:size-mark (art table)
  (cond
    ((cdr (assoc art *mk:article-size*)) (cdr (assoc art *mk:article-size*)))
    ((cdr (assoc art table))             (cdr (assoc art table)))
    (t "")))

;; Разметка ригелей: TOP_ELEM / BOT_ELEM / SUFFIX / BAY / SIZE_MARK / GROUP_KEY
(defun mk:annotate-beams (beams all-panels doors bounds posts /
                          out suffix len art axes table mark bay)
  (setq out   nil
        axes  (mk:post-axes posts)
        table (mk:size-mark-table beams))
  (foreach beam beams
    (setq beam (mk:rec-put beam 'TOP_ELEM (mk:find-top-element beam all-panels)))
    (setq beam (mk:rec-put beam 'BOT_ELEM (mk:find-bot-element beam all-panels)))
    (setq suffix (mk:get-beam-suffix beam all-panels doors bounds))
    (setq beam (mk:rec-put beam 'SUFFIX suffix))
    (setq art  (if (mk:rec-get beam 'ARTICLE) (mk:rec-get beam 'ARTICLE) "БЕЗ_АРТИКУЛА")
          mark (mk:size-mark art table)
          bay  (mk:bay-index (mk:el-center-x beam) axes)
          len  (mk:size-key (mk:rec-get beam 'LENGTH)))
    (setq beam (mk:rec-put beam 'BAY bay))
    (setq beam (mk:rec-put beam 'SIZE_MARK mark))
    (setq beam (mk:rec-put beam 'GROUP_KEY (strcat len "_" art "_" suffix)))
    (setq out (cons beam out)))
  (reverse out))

;;;---------------------------------------------------------------------
;;; 18d. ПЛАН МАРОК (марка -> список элементов)
;;;---------------------------------------------------------------------
;; Стойки: номер по канонической группе, зеркальная ориентация — суффикс "зерк"
(defun mk:plan-posts (posts prefix / plan idx grp base-or base mirror k)
  (setq posts (mk:sort-xy posts) plan nil idx 1)
  (foreach k (mk:unique-keys posts 'GROUP_KEY)
    (setq grp     (mk:filter-by posts 'GROUP_KEY k)
          base-or (mk:rec-get (car grp) 'ORIENT)
          base    (mk:filter-by grp 'ORIENT base-or)
          mirror  (mk:reject-by grp 'ORIENT base-or))
    (setq plan (cons (list (strcat prefix " Ст" (itoa idx)) base) plan))
    (if mirror
      (setq plan (cons (list (strcat prefix " Ст" (itoa idx) *mk:suffix-mirror*) mirror) plan)))
    (setq idx (1+ idx)))
  (reverse plan))

;; Ригели: одна цифра на вертикальный столбец (пролёт), внутри столбца
;; различаются маркером размера профиля (м/б) и суффиксом окружения.
(defun mk:plan-beams (beams prefix / plan idx bays bay col k grp mark base used n)
  (setq beams (mk:sort-xy beams) plan nil idx 1 used nil)
  ;; столбцы в порядке слева направо; ригели вне сетки (BAY=0) — в конец
  (setq bays (vl-sort (mk:unique-keys beams 'BAY) '<))
  (if (member 0 bays)
    (setq bays (append (vl-remove 0 bays) (list 0))))
  (foreach bay bays
    (setq col (mk:filter-by beams 'BAY bay))
    (foreach k (mk:unique-keys col 'GROUP_KEY)
      (setq grp  (mk:filter-by col 'GROUP_KEY k)
            mark (strcat prefix " Рг" (itoa idx)
                         (if (mk:rec-get (car grp) 'SIZE_MARK)
                           (mk:rec-get (car grp) 'SIZE_MARK) "")
                         (if (mk:rec-get (car grp) 'SUFFIX)
                           (mk:rec-get (car grp) 'SUFFIX) "")))
      ;; защита от совпадения марок разных групп в одном столбце
      (if (member mark used)
        (progn
          (setq base mark n 2)
          (while (member (strcat base "-" (itoa n)) used) (setq n (1+ n)))
          (setq mark (strcat base "-" (itoa n)))
          (prompt (strcat "\n  [WARN] Столбец " (itoa idx)
                          ": разные группы дают одну марку — выдана " mark))))
      (setq used (cons mark used))
      (setq plan (cons (list mark grp) plan)))
    (setq idx (1+ idx)))
  (reverse plan))

;;;---------------------------------------------------------------------
;;; 18e. ОБЛАСТЬ ДЕЙСТВИЯ, UNDO, ЗАПИСЬ
;;;---------------------------------------------------------------------
(defun mk:ss-all-inserts ()
  (ssget "_X" (list (cons 0 "INSERT"))))

;; Область действия команды: своя выборка -> выборка последней МАРКАВГЕОМЕТРИЯ ->
;; (только если ничего нет) весь чертёж.
(defun mk:scope-ss (/ ss n)
  (prompt "\nВыберите элементы витража (Enter — выборка последней МАРКАВГЕОМЕТРИЯ): ")
  (setq ss (ssget (list (cons 0 "INSERT"))))
  (cond
    (ss
     (setq *mk:last-ss* ss)
     (prompt (strcat "\n  Область: выборка, объектов " (itoa (sslength ss))))
     ss)
    (t
     (setq n (vl-catch-all-apply 'sslength (list *mk:last-ss*)))
     (if (and *mk:last-ss* (not (vl-catch-all-error-p n)) (> n 0))
       (progn
         (prompt (strcat "\n  Область: выборка последней МАРКАВГЕОМЕТРИЯ, объектов " (itoa n)))
         *mk:last-ss*)
       (progn
         (prompt "\n  [WARN] Выборки нет — обрабатывается ВЕСЬ чертёж.")
         (mk:ss-all-inserts))))))

(defun mk:undo-begin (/ doc)
  (setq doc (mk:ax-get (vlax-get-acad-object) "ActiveDocument"))
  (if doc (vl-catch-all-apply 'vlax-invoke-method (list doc "StartUndoMark")))
  doc)

(defun mk:undo-end (doc)
  (if doc (vl-catch-all-apply 'vlax-invoke-method (list doc "EndUndoMark")))
  nil)

;; Запись марки по группе; возвращает (записано пропущено)
(defun mk:write-marks (elements mark-str / count skip-count e)
  (setq count 0 skip-count 0)
  (foreach el elements
    (setq e (mk:rec-get el 'ENAME))
    (if (and e (mk:has-attr? e *mk:attr-mark*))
      (progn
        (mk:set-attr e *mk:attr-mark* mark-str)
        (setq count (1+ count)))
      (setq skip-count (1+ skip-count))))
  (list count skip-count))

;; Сбор всех типов элементов из выборки
(defun mk:collect-scope (ss / posts beams fills windows doors)
  (setq posts   (mapcar 'mk:collect-post   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-post*   "*"))))
  (setq beams   (mapcar 'mk:collect-beam   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-beam*   "*"))))
  (setq fills   (mapcar 'mk:collect-fill   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-fill*   "*"))))
  (setq windows (mapcar 'mk:collect-window (mk:find-blocks-in-ss ss (strcat "*" *mk:block-window* "*"))))
  (setq doors   (mapcar 'mk:collect-door   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-door*   "*"))))
  (list (cons 'POSTS posts) (cons 'BEAMS beams) (cons 'FILLS fills)
        (cons 'WINDOWS windows) (cons 'DOORS doors)
        (cons 'PANELS (append fills windows doors))))

;;;---------------------------------------------------------------------
;;; 18f. КОМАНДЫ МАРКИРОВКИ
;;;---------------------------------------------------------------------
(defun c:МАРКАВСТ (/ ss data posts elements prefix plan res count skip-count doc)
  (prompt "\n[МАРКАВСТ] Марки стоек...")
  (setq *mk:dyn-cache* nil)
  (setq prefix (mk:get-vitrage-prefix))
  (prompt (strcat "\n  Префикс витража: " prefix))
  (setq ss   (mk:scope-ss))
  (setq data (mk:collect-scope ss))
  (setq posts (cdr (assoc 'POSTS data)))
  (if (null posts)
    (prompt "\n  [INFO] Блоки стоек не найдены.")
    (progn
      (setq elements (append (cdr (assoc 'PANELS data)) (cdr (assoc 'BEAMS data))))
      (prompt (strcat "\n  Стоек: " (itoa (length posts))
                      ", элементов окружения: " (itoa (length elements))))
      (setq posts (mk:annotate-posts posts elements))
      (setq plan  (mk:plan-posts posts prefix))
      (prompt (strcat "\n  Марок (групп): " (itoa (length plan))))
      (setq count 0 skip-count 0)
      (setq doc (mk:undo-begin))
      (foreach item plan
        (setq res (mk:write-marks (cadr item) (car item)))
        (prompt (strcat "\n  " (car item) " — шт.: " (itoa (car res))
                        "   [" (mk:rec-get (car (cadr item)) 'LEFT_TYPES)
                        " | " (mk:rec-get (car (cadr item)) 'RIGHT_TYPES) "]"))
        (setq count      (+ count (car res))
              skip-count (+ skip-count (cadr res))))
      (mk:undo-end doc)
      (prompt (strcat "\n[ГОТОВО] Заполнено: " (itoa count)
                      ", Пропущено: " (itoa skip-count)))))
  (princ))

(defun c:МАРКАВРГ (/ ss data beams posts panels doors bounds prefix plan
                     res count skip-count doc)
  (prompt "\n[МАРКАВРГ] Марки ригелей...")
  (setq *mk:dyn-cache* nil)
  (setq prefix (mk:get-vitrage-prefix))
  (prompt (strcat "\n  Префикс витража: " prefix))
  (setq ss   (mk:scope-ss))
  (setq data (mk:collect-scope ss))
  (setq beams  (cdr (assoc 'BEAMS data))
        posts  (cdr (assoc 'POSTS data))
        panels (cdr (assoc 'PANELS data))
        doors  (cdr (assoc 'DOORS data)))
  (if (null beams)
    (prompt "\n  [INFO] Блоки ригелей не найдены.")
    (progn
      (prompt (strcat "\n  Ригелей: " (itoa (length beams))
                      ", панелей: " (itoa (length panels))))
      (setq bounds (mk:find-vitrage-bounds posts beams))
      (setq beams  (mk:annotate-beams beams panels doors bounds posts))
      (setq plan   (mk:plan-beams beams prefix))
      (prompt (strcat "\n  Марок (групп): " (itoa (length plan))))
      (setq count 0 skip-count 0)
      (setq doc (mk:undo-begin))
      (foreach item plan
        (setq res (mk:write-marks (cadr item) (car item)))
        (prompt (strcat "\n  " (car item) " — шт.: " (itoa (car res))))
        (setq count      (+ count (car res))
              skip-count (+ skip-count (cadr res))))
      (mk:undo-end doc)
      (prompt (strcat "\n[ГОТОВО] Заполнено: " (itoa count)
                      ", Пропущено: " (itoa skip-count)))))
  (princ))

;;;=====================================================================
;;; 19. ГЛАВНАЯ КОМАНДА
;;;=====================================================================
(defun c:МАРКАВГЕОМЕТРИЯ (/ ss vitrage vitrage-pt posts beams fills windows doors
                      bounds topo-result all-panels dup-count overlap-count
                      out-count post-result protruding-count mlines
                      mline-result use-mlines mode-kw post-enames beam-enames)
  (prompt "\n[МАРКАВГЕОМЕТРИЯ] Сбор данных и построение 2D модели витража...")
  (prompt "\nВыберите элементы витража (рамкой): ")
  (setq *mk:dyn-cache* nil)
  (setq ss (ssget))
  (if ss (setq *mk:last-ss* ss))
  (if (null ss)
    (prompt "\n[INFO] Ничего не выбрано."))
  (if ss (progn
  (prompt (strcat "\n  Выбрано объектов: " (itoa (sslength ss))))
  (prompt "\n[1/8] Чтение блока Атрибуты витража...")
  (setq vitrage (mk:read-vitrage-block))
  (if vitrage
    (progn
      (setq vitrage-pt (cdr (assoc 'INS_PT vitrage)))
      (prompt "\n  Найдено блоков атрибутов витража: 1"))
    (prompt "\n  [WARN] Блок Атрибуты витража не найден!"))
  (prompt "\n[2/8] Поиск элементов каркаса в выборке...")
  (setq mlines (mk:find-mlines-in-ss ss))
  (prompt (strcat "\n  Мультилиний найдено: " (itoa (length mlines))))
  (initget "Блоки Мультилинии B M")
  (setq mode-kw (getkword "\nРежим сбора каркаса [Блоки/Мультилинии] <Мультилинии>: "))
  (if (or (null mode-kw) (= mode-kw "Мультилинии") (= mode-kw "M"))
    (setq use-mlines t)
    (setq use-mlines nil))
  (if use-mlines
    (progn
      (prompt "\n  Режим: МУЛЬТИЛИНИИ")
      (if (> (length mlines) 0)
        (progn
          (setq mline-result (mk:extract-posts-beams-from-mlines mlines))
          (setq posts (nth 0 mline-result))
          (setq beams (nth 1 mline-result)))
        (prompt "\n  [WARN] Мультилинии не найдены в выборке.")))
    (progn
      (prompt "\n  Режим: БЛОКИ")
      (setq post-enames (mk:find-blocks-in-ss ss (strcat "*" *mk:block-post* "*")))
      (setq beam-enames (mk:find-blocks-in-ss ss (strcat "*" *mk:block-beam* "*")))
      (setq posts (mapcar 'mk:collect-post post-enames))
      (setq beams (mapcar 'mk:collect-beam beam-enames))))
  (setq fills   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-fill* "*")))
  (setq windows (mk:find-blocks-in-ss ss (strcat "*" *mk:block-window* "*")))
  (setq doors   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-door* "*")))
  (setq fills   (mapcar 'mk:collect-fill fills))
  (setq windows (mapcar 'mk:collect-window windows))
  (setq doors   (mapcar 'mk:collect-door doors))
  (prompt (strcat "\n  Стойки: " (itoa (length posts))))
  (prompt (strcat "\n  Ригели: " (itoa (length beams))))
  (prompt (strcat "\n  Заполнения: " (itoa (length fills))))
  (prompt (strcat "\n  Окна: " (itoa (length windows))))
  (prompt (strcat "\n  Двери: " (itoa (length doors))))
  (prompt "\n[3/8] Определение границ витража...")
  (setq bounds (mk:find-vitrage-bounds posts beams))
  (prompt (strcat "\n  Границы: X=" (rtos (cdr (assoc 'MIN_X bounds)) 2 1)
                  ".." (rtos (cdr (assoc 'MAX_X bounds)) 2 1)
                  ", Y=" (rtos (cdr (assoc 'MIN_Y bounds)) 2 1)
                  ".." (rtos (cdr (assoc 'MAX_Y bounds)) 2 1)))
  (prompt "\n[4/8] Проверки и валидация...")
  (setq all-panels (append fills windows doors))
  (prompt "\n  Проверка дубликатов...")
  (setq dup-count (mk:check-duplicates (append posts beams all-panels)))
  (prompt "\n  Проверка перекрытий...")
  (setq overlap-count (mk:check-overlaps all-panels))
  (prompt "\n  Проверка выхода за пределы сетки...")
  (setq out-count (mk:check-out-of-grid (append posts beams all-panels) bounds))
  (prompt "\n  Определение выступающих стоек...")
  (setq post-result (mk:identify-protruding-posts posts bounds))
  (setq posts (nth 0 post-result))
  (setq protruding-count (nth 1 post-result))
  (prompt (strcat "\n  Выступающих стоек: " (itoa protruding-count)))
  (prompt "\n  Валидация ригелей...")
  (foreach beam beams (mk:validate-beam beam posts beams))
  (prompt "\n  Валидация заполнений...")
  (foreach fill fills (mk:validate-fill fill posts))
  (prompt "\n[5/8] Построение топологии связей...")
  (setq topo-result (mk:build-topology posts beams fills windows doors bounds))
  (setq posts   (nth 0 topo-result))
  (setq beams   (nth 1 topo-result))
  (setq fills   (nth 2 topo-result))
  (setq windows (nth 3 topo-result))
  (setq doors   (nth 4 topo-result))
  (setq all-panels (append fills windows doors))
  (setq posts (mk:annotate-posts posts (append all-panels beams)))
  (setq beams (mk:annotate-beams beams all-panels doors bounds posts))
  (prompt "\n[6/8] Визуализация и вывод данных...")
  (mk:draw-model posts beams fills windows doors bounds)
  (mk:dump-data vitrage bounds posts beams fills windows doors)
  (setq *mk:cached-data* (list posts beams fills windows doors bounds))
  (prompt "\n")
  (prompt "\n=== СВОДКА ПРОВЕРОК ===")
  (prompt (strcat "\n  Режим: " (if use-mlines "МУЛЬТИЛИНИИ" "БЛОКИ")))
  (prompt (strcat "\n  Дубликатов: " (itoa dup-count)))
  (prompt (strcat "\n  Перекрытий: " (itoa overlap-count)))
  (prompt (strcat "\n  Элементов вне сетки: " (itoa out-count)))
  (prompt (strcat "\n  Выступающих стоек: " (itoa protruding-count)))
  (prompt "\n[ГОТОВО] Сбор данных завершён.")
  ))                                    ; конец (if ss (progn …
  (princ))

;;;=====================================================================
;;; 20. КОМАНДА ТЕСТОВОЙ ОТРИСОВКИ
;;;=====================================================================
(defun c:МАРКАВМОДЕЛЬ (/ posts beams fills windows doors bounds)
  (prompt "\n[МАРКАВМОДЕЛЬ] Тестовая отрисовка модели витража...")
  (if (null *mk:cached-data*)
    (progn
      (prompt "\n  Данные не найдены. Запуск сбора...")
      (c:МАРКАВГЕОМЕТРИЯ)))
  (if *mk:cached-data*
    (progn
      (setq posts   (nth 0 *mk:cached-data*))
      (setq beams   (nth 1 *mk:cached-data*))
      (setq fills   (nth 2 *mk:cached-data*))
      (setq windows (nth 3 *mk:cached-data*))
      (setq doors   (nth 4 *mk:cached-data*))
      (setq bounds  (nth 5 *mk:cached-data*))
      (mk:draw-test-model posts beams fills windows doors bounds)
      (prompt "\n[ГОТОВО] Тестовая модель отрисована."))
    (prompt "\n[ERROR] Нет данных. Запустите МАРКАВГЕОМЕТРИЯ."))
  (princ))

;;;=====================================================================
;;; 20a. ДИАГНОСТИКА БЛОКА (почему LENGTH/WIDTH/АРТИКУЛ = nil)
;;;=====================================================================
(defun mk:fmt (v) (vl-princ-to-string v))

(defun mk:diag-block (ename / obj lines bb attrs pairs nm hit)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (vl-catch-all-error-p obj) (setq obj nil))
  (setq lines nil)
  (setq lines (cons (strcat "DXF 0        : " (mk:fmt (cdr (assoc 0 (entget ename))))) lines))
  (setq lines (cons (strcat "DXF 2 (имя)  : " (mk:fmt (cdr (assoc 2 (entget ename))))) lines))
  (setq lines (cons (strcat "Name         : " (mk:fmt (mk:ax-get obj "Name"))) lines))
  (setq lines (cons (strcat "EffectiveName: " (mk:fmt (mk:ax-get obj "EffectiveName"))) lines))
  (setq lines (cons (strcat "Слой         : " (mk:fmt (cdr (assoc 8 (entget ename))))) lines))
  (setq lines (cons (strcat "Динамический : " (if (mk:dynamic? obj) "ДА" "НЕТ")) lines))
  (setq lines (cons (strcat "InsertionPoint: " (mk:fmt (cdr (assoc 'INS_PT (mk:get-geom obj))))) lines))
  (setq lines (cons (strcat "Rotation     : " (mk:fmt (cdr (assoc 'ROTATION (mk:get-geom obj))))) lines))
  (setq lines (cons (strcat "XScale/YScale: "
                            (mk:fmt (cdr (assoc 'XSCALE (mk:get-geom obj)))) " / "
                            (mk:fmt (cdr (assoc 'YSCALE (mk:get-geom obj))))) lines))
  (setq bb (mk:e-bb obj))
  (setq lines (cons (strcat "Габарит BBOX : "
                            (if bb (strcat "Ш=" (rtos (mk:bb-w bb) 2 1)
                                           "  В=" (rtos (mk:bb-h bb) 2 1))
                              "не получен")) lines))
  (setq lines (cons "--- АТРИБУТЫ (тег = значение) ---" lines))
  (setq attrs (mk:get-all-attrs ename))
  (if attrs
    (foreach a attrs
      (setq lines (cons (strcat "   <" (mk:fmt (car a)) "> = \"" (mk:fmt (cdr a)) "\"") lines)))
    (setq lines (cons "   [нет атрибутов]" lines)))
  (setq lines (cons "--- ДИНАМИЧЕСКИЕ СВОЙСТВА (имя = значение) ---" lines))
  (setq pairs (mk:dyn-pairs-raw obj))
  (if pairs
    (foreach pr pairs
      (setq lines (cons (strcat "   <" (car pr) "> = " (mk:fmt (cdr pr))) lines)))
    (setq lines (cons "   [нет динамических свойств]" lines)))
  (setq lines (cons "--- ЧТО НАХОДЯТ ТЕКУЩИЕ МАСКИ ---" lines))
  (setq lines (cons (strcat "   " *mk:mask-length* " -> " (mk:fmt (mk:get-dyn obj *mk:mask-length*))) lines))
  (setq lines (cons (strcat "   " *mk:mask-height* " -> " (mk:fmt (mk:get-dyn obj *mk:mask-height*))) lines))
  (setq lines (cons (strcat "   " *mk:mask-width*  " -> " (mk:fmt (mk:get-dyn obj *mk:mask-width*)))  lines))
  (setq lines (cons (strcat "   Видимость -> " (mk:fmt (mk:get-vis obj))) lines))
  (setq lines (cons (strcat "   Атрибут «" *mk:attr-article* "» -> "
                            (mk:fmt (mk:get-attr ename *mk:attr-article*))) lines))
  (setq lines (cons (strcat "   Атрибут «" *mk:attr-mark* "» -> "
                            (if (mk:has-attr? ename *mk:attr-mark*) "ЕСТЬ" "НЕТ")) lines))
  (reverse lines))

(defun c:МАРКАВДИАГНОЗ (/ sel ename lines f)
  (prompt "\n[МАРКАВДИАГНОЗ] Диагностика блока.")
  (setq sel (entsel "\nУкажите блок (стойку, ригель, заполнение, окно, дверь): "))
  (if (null sel)
    (prompt "\n  [INFO] Блок не указан.")
    (progn
      (setq ename (car sel))
      (setq *mk:dyn-cache* nil)
      (setq lines (mk:diag-block ename))
      (prompt "\n---------------------------------------------")
      (foreach l lines (prompt (strcat "\n" l)))
      (prompt "\n---------------------------------------------")
      (setq f (open *mk:diag-file* "a"))
      (if f
        (progn
          (write-line "=== МАРКАВДИАГНОЗ ===" f)
          (foreach l lines (write-line l f))
          (write-line "" f)
          (close f)
          (prompt (strcat "\n[OK] Дописано в: " *mk:diag-file*)))
        (prompt "\n[WARN] Файл диагностики не создан."))))
  (princ))

;;;=====================================================================
;;; 21. ИНИЦИАЛИЗАЦИЯ С АВТОПРОВЕРКОЙ
;;;=====================================================================
(prompt (strcat "\n[MarkZV] Ред. " *mk:ver* " загружена."))
(prompt "\n  Команды:")
(prompt "\n    МАРКАВГЕОМЕТРИЯ - Сбор данных и построение 2D модели")
(prompt "\n    МАРКАВМОДЕЛЬ    - Тестовая отрисовка модели")
(prompt "\n    МАРКАВСТ        - Марки стоек")
(prompt "\n    МАРКАВРГ        - Марки ригелей")
(prompt "\n    МАРКАВДИАГНОЗ   - Диагностика блока (свойства, атрибуты, габарит)")
(prompt "\n    МАРКАВСКОБКИ    - Проверка баланса скобок файла")
(princ)
