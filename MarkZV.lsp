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

(defun c:MK_CHECK (/ result)
  (prompt "\n[MK_CHECK] Проверка баланса скобок MarkZV.lsp...")
  (setq result (mk:check-brackets "D:/MarkZV.lsp"))
  (if result
    (prompt "\n[OK] Файл корректен.")
    (prompt "\n[ERROR] Обнаружены проблемы. См. сообщение выше."))
  (princ))

;;;=====================================================================
;;; 1. КОНФИГУРАЦИЯ
;;;=====================================================================
(setq *mk:rev*            "r2")
(setq *mk:build*          "2026-09-28.b2")

(setq *mk:block-fill*     "Заполнение в витраж")
(setq *mk:block-window*   "Окно КПТ60")
(setq *mk:block-door*     "Дверной блок КПТ74 двухстворчатый")
(setq *mk:block-vitrage*  "Атрибуты витража")
(setq *mk:block-post*     "стойка")
(setq *mk:block-beam*     "ригель")

(setq *mk:attr-profile*   "Профиль")
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
(setq *mk:cached-data*    nil)

(setq *mk:suffix-window-one*    "ок")
(setq *mk:suffix-window-both*   "окх2")
(setq *mk:suffix-door-one*      "дв")
(setq *mk:suffix-threshold*     "н")
(setq *mk:suffix-warm-cold*     "тх")
(setq *mk:suffix-cold-warm*     "хт")
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

;; Накопление групп: (ключ (элементы …)) с сохранением порядка
(defun mk:group-add (groups key el / grp)
  (setq grp (assoc key groups))
  (if grp
    (subst (list key (cons el (cadr grp))) grp groups)
    (cons (list key (list el)) groups)))

;; Финализация: порядок групп и элементов внутри групп — как на чертеже
(defun mk:groups-finish (groups / out grp)
  (setq out nil)
  (foreach grp groups
    (setq out (cons (list (car grp) (reverse (cadr grp))) out)))
  out)

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
;;; 4. ИЗВЛЕЧЕНИЕ ДИНАМИЧЕСКИХ СВОЙСТВ (по маске + Variant)
;;;=====================================================================
(defun mk:get-dyn (obj prop-mask / props val p raw-val prop-name)
  (setq val nil)
  (if (and obj
           (vlax-property-available-p obj 'IsDynamicBlock)
           (= (vlax-get obj 'IsDynamicBlock) :vlax-true))
    (progn
      (setq props (vl-catch-all-apply 'vlax-invoke-method
                    (list obj "GetDynamicBlockProperties")))
      (if (not (vl-catch-all-error-p props))
        (progn
          (if (= (type props) 'VARIANT)
            (setq props (vlax-safearray->list (vlax-variant-value props))))
          (if (and props (listp props))
            (foreach p props
              (if (null val)
                (progn
                  (setq prop-name (vlax-get p 'PropertyName))
                  (if (and prop-name (mk:strp prop-name)
                           (wcmatch (strcase prop-name) (strcase prop-mask)))
                    (progn
                      (setq raw-val (vl-catch-all-apply 'vlax-get (list p 'Value)))
                      (if (and raw-val (not (vl-catch-all-error-p raw-val)))
                        (cond
                          ((numberp raw-val)
                           (setq val (float raw-val)))
                          ((= (type raw-val) 'VARIANT)
                           (setq raw-val (vl-catch-all-apply 'vlax-variant-value (list raw-val)))
                           (if (numberp raw-val) (setq val (float raw-val))))
                          ((and (mk:strp raw-val) (distof raw-val 2))
                           (setq val (distof raw-val 2)))))))))))))))
  (if (numberp val) val nil))

;;;=====================================================================
;;; 5. ИЗВЛЕЧЕНИЕ ВИДИМОСТИ (с Variant)
;;;=====================================================================
(defun mk:get-vis (obj / props val raw-val prop-name)
  (setq val nil)
  (if (and obj
           (vlax-property-available-p obj 'IsDynamicBlock)
           (= (vlax-get obj 'IsDynamicBlock) :vlax-true))
    (progn
      (setq props (vl-catch-all-apply 'vlax-invoke-method
                    (list obj "GetDynamicBlockProperties")))
      (if (not (vl-catch-all-error-p props))
        (progn
          (if (= (type props) 'VARIANT)
            (setq props (vlax-safearray->list (vlax-variant-value props))))
          (if (and props (listp props))
            (foreach p props
              (if (null val)
                (progn
                  (setq prop-name (vlax-get p 'PropertyName))
                  (if (and prop-name (mk:strp prop-name)
                           (member (mk:trim prop-name) *mk:vis-candidates*))
                    (progn
                      (setq raw-val (vl-catch-all-apply 'vlax-get (list p 'Value)))
                      (if (and raw-val (not (vl-catch-all-error-p raw-val)))
                        (progn
                          (if (= (type raw-val) 'VARIANT)
                            (setq raw-val (vlax-variant-value raw-val)))
                          (if (mk:strp raw-val)
                            (setq val (mk:trim raw-val)))))))))))))))
  val)

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
                        (cons 'PROFILE nil)
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
                        (cons 'PROFILE nil)
                        (cons 'VISIBILITY nil)
                        (cons 'TOP_ELEM nil)
                        (cons 'BOT_ELEM nil)
                        (cons 'SUFFIX nil)
                      ) beams))))))))))))
  (list (reverse posts) (reverse beams)))

;;;=====================================================================
;;; 9. СБОР ДАННЫХ ПО БЛОКАМ (маски *ысот* *ирин* *лин*)
;;;=====================================================================
(defun mk:collect-fill (ename / obj geom)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (vl-catch-all-error-p obj) (null obj)) (setq obj nil))
  (setq geom (mk:get-geom obj))
  (list
    (cons 'TYPE       "ЗАПОЛНЕНИЕ")
    (cons 'ENAME      ename)
    (cons 'INS_PT     (cdr (assoc 'INS_PT geom)))
    (cons 'ROTATION   (cdr (assoc 'ROTATION geom)))
    (cons 'XSCALE     (cdr (assoc 'XSCALE geom)))
    (cons 'YSCALE     (cdr (assoc 'YSCALE geom)))
    (cons 'HEIGHT     (mk:get-dyn obj *mk:mask-height*))
    (cons 'WIDTH      (mk:get-dyn obj *mk:mask-width*))
    (cons 'THICKNESS  (mk:get-attr ename *mk:attr-thickness*))
    (cons 'PROFILE    (mk:get-attr ename *mk:attr-profile*))
    (cons 'VISIBILITY (mk:get-vis obj))
    (cons 'CONN_POSTS nil)
  ))

(defun mk:collect-window (ename / obj geom)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (vl-catch-all-error-p obj) (null obj)) (setq obj nil))
  (setq geom (mk:get-geom obj))
  (list
    (cons 'TYPE       "ОКНО")
    (cons 'ENAME      ename)
    (cons 'INS_PT     (cdr (assoc 'INS_PT geom)))
    (cons 'ROTATION   (cdr (assoc 'ROTATION geom)))
    (cons 'XSCALE     (cdr (assoc 'XSCALE geom)))
    (cons 'YSCALE     (cdr (assoc 'YSCALE geom)))
    (cons 'HEIGHT     (mk:get-dyn obj *mk:mask-height*))
    (cons 'WIDTH      (mk:get-dyn obj *mk:mask-width*))
    (cons 'NAME       (mk:get-attr ename *mk:attr-name*))
    (cons 'VISIBILITY (mk:get-vis obj))
    (cons 'CONN_POSTS nil)
  ))

(defun mk:collect-door (ename / obj geom)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (vl-catch-all-error-p obj) (null obj)) (setq obj nil))
  (setq geom (mk:get-geom obj))
  (list
    (cons 'TYPE       "ДВЕРЬ")
    (cons 'ENAME      ename)
    (cons 'INS_PT     (cdr (assoc 'INS_PT geom)))
    (cons 'ROTATION   (cdr (assoc 'ROTATION geom)))
    (cons 'XSCALE     (cdr (assoc 'XSCALE geom)))
    (cons 'YSCALE     (cdr (assoc 'YSCALE geom)))
    (cons 'HEIGHT     (mk:get-dyn obj *mk:mask-height*))
    (cons 'WIDTH      (mk:get-dyn obj *mk:mask-width*))
    (cons 'NAME       (mk:get-attr ename *mk:attr-name*))
    (cons 'VISIBILITY (mk:get-vis obj))
    (cons 'CONN_POSTS nil)
  ))

(defun mk:collect-post (ename / obj geom)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (vl-catch-all-error-p obj) (null obj)) (setq obj nil))
  (setq geom (mk:get-geom obj))
  (list
    (cons 'TYPE       "СТОЙКА")
    (cons 'ENAME      ename)
    (cons 'INS_PT     (cdr (assoc 'INS_PT geom)))
    (cons 'ROTATION   (cdr (assoc 'ROTATION geom)))
    (cons 'XSCALE     (cdr (assoc 'XSCALE geom)))
    (cons 'LENGTH     (mk:get-dyn obj *mk:mask-length*))
    (cons 'PROFILE    (mk:get-attr ename *mk:attr-profile*))
    (cons 'VISIBILITY (mk:get-vis obj))
    (cons 'LEFT_CONN  nil)
    (cons 'RIGHT_CONN nil)
    (cons 'PROTRUDING nil)
  ))

(defun mk:collect-beam (ename / obj geom)
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (or (vl-catch-all-error-p obj) (null obj)) (setq obj nil))
  (setq geom (mk:get-geom obj))
  (list
    (cons 'TYPE       "РИГЕЛЬ")
    (cons 'ENAME      ename)
    (cons 'INS_PT     (cdr (assoc 'INS_PT geom)))
    (cons 'ROTATION   (cdr (assoc 'ROTATION geom)))
    (cons 'LENGTH     (mk:get-dyn obj *mk:mask-length*))
    (cons 'PROFILE    (mk:get-attr ename *mk:attr-profile*))
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

;; Валидация ригеля — проверка ОБА конца
(defun mk:validate-beam (beam posts beams / has-connection bx by bx2 px py ox oy beam-len)
  (setq has-connection nil)
  (setq beam-len (if (cdr (assoc 'LENGTH beam)) (cdr (assoc 'LENGTH beam)) 0.0))
  (setq bx (car (cdr (assoc 'INS_PT beam)))
        by (cadr (cdr (assoc 'INS_PT beam)))
        bx2 (+ bx beam-len))
  (foreach post posts
    (if (and (cdr (assoc 'INS_PT post)) (not has-connection))
      (progn
        (setq px (car (cdr (assoc 'INS_PT post)))
              py (cadr (cdr (assoc 'INS_PT post))))
        (if (and (< (abs (- bx px)) *mk:tol-adjacency*)
                 (>= by py)
                 (<= by (+ py (if (cdr (assoc 'LENGTH post)) (cdr (assoc 'LENGTH post)) 0.0))))
          (setq has-connection t)))))
  (if (not has-connection)
    (foreach post posts
      (if (and (cdr (assoc 'INS_PT post)) (not has-connection))
        (progn
          (setq px (car (cdr (assoc 'INS_PT post)))
                py (cadr (cdr (assoc 'INS_PT post))))
          (if (and (< (abs (- bx2 px)) *mk:tol-adjacency*)
                   (>= by py)
                   (<= by (+ py (if (cdr (assoc 'LENGTH post)) (cdr (assoc 'LENGTH post)) 0.0))))
            (setq has-connection t))))))
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

(defun mk:find-top-element (beam panels / beam-y top-elem best-dist dist panel-y)
  (setq beam-y (cadr (cdr (assoc 'INS_PT beam)))
        top-elem nil best-dist nil)
  (foreach panel panels
    (if (cdr (assoc 'INS_PT panel))
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
    (if (cdr (assoc 'INS_PT panel))
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
(defun mk:draw-post (post / pt len)
  (setq pt  (cdr (assoc 'INS_PT post))
        len (if (cdr (assoc 'LENGTH post)) (cdr (assoc 'LENGTH post)) 0.0))
  (if pt
    (progn
      (entmakex (list '(0 . "LINE")
                      (cons 10 (list (car pt) (cadr pt) 0.0))
                      (cons 11 (list (car pt) (+ (cadr pt) len) 0.0))
                      '(62 . 1)))
      (entmakex (list '(0 . "POINT")
                      (cons 10 (list (car pt) (cadr pt) 0.0))
                      '(62 . 2))))))

(defun mk:draw-beam (beam / pt len)
  (setq pt  (cdr (assoc 'INS_PT beam))
        len (if (cdr (assoc 'LENGTH beam)) (cdr (assoc 'LENGTH beam)) 0.0))
  (if pt
    (entmakex (list '(0 . "LINE")
                    (cons 10 (list (car pt) (cadr pt) 0.0))
                    (cons 11 (list (+ (car pt) len) (cadr pt) 0.0))
                    '(62 . 3)))))

(defun mk:draw-panel (panel / pt h w)
  (setq pt (cdr (assoc 'INS_PT panel))
        h  (if (cdr (assoc 'HEIGHT panel)) (cdr (assoc 'HEIGHT panel)) 0.0)
        w  (if (cdr (assoc 'WIDTH panel)) (cdr (assoc 'WIDTH panel)) 0.0))
  (if (and pt (> h 0) (> w 0))
    (entmakex (list '(0 . "LWPOLYLINE") '(90 . 4) '(70 . 1) '(62 . 5)
                    (cons 10 (list (car pt) (cadr pt)))
                    (cons 10 (list (+ (car pt) w) (cadr pt)))
                    (cons 10 (list (+ (car pt) w) (+ (cadr pt) h)))
                    (cons 10 (list (car pt) (+ (cadr pt) h)))))))

(defun mk:draw-bounds (bounds / min-x max-x min-y max-y)
  (setq min-x (cdr (assoc 'MIN_X bounds))
        max-x (cdr (assoc 'MAX_X bounds))
        min-y (cdr (assoc 'MIN_Y bounds))
        max-y (cdr (assoc 'MAX_Y bounds)))
  (entmakex (list '(0 . "LWPOLYLINE") '(90 . 4) '(70 . 1) '(62 . 2)
                  (cons 10 (list min-x min-y))
                  (cons 10 (list max-x min-y))
                  (cons 10 (list max-x max-y))
                  (cons 10 (list min-x max-y)))))

(defun mk:draw-model (posts beams fills windows doors bounds / layer prev-layer)
  (setq layer "MK_MODEL")
  (if (null (tblsearch "LAYER" layer))
    (command "_.LAYER" "_N" layer "_C" "7" layer ""))
  (setq prev-layer (getvar "CLAYER"))
  (setvar "CLAYER" layer)
  (mk:draw-bounds bounds)
  (foreach post posts (mk:draw-post post))
  (foreach beam beams (mk:draw-beam beam))
  (foreach panel (append fills windows doors) (mk:draw-panel panel))
  (setvar "CLAYER" prev-layer)
  (prompt (strcat "\n[OK] 2D модель отрисована на слое: " layer)))

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
      (entmakex (list '(0 . "LWPOLYLINE") '(90 . 4) '(70 . 1) '(62 . 5)
                      (cons 10 (list x1 y1))
                      (cons 10 (list x2 y1))
                      (cons 10 (list x2 y2))
                      (cons 10 (list x1 y2))))
      (entmakex (list '(0 . "LINE")
                      (cons 10 (list x1 y1 0.0))
                      (cons 11 (list x2 y2 0.0))
                      '(62 . 5)))
      (entmakex (list '(0 . "LINE")
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
      (entmakex (list '(0 . "LWPOLYLINE") '(90 . 4) '(70 . 1) '(62 . 4)
                      (cons 10 (list x1 y1))
                      (cons 10 (list x2 y1))
                      (cons 10 (list x2 y2))
                      (cons 10 (list x1 y2))))
      (entmakex (list '(0 . "TEXT")
                      (cons 10 (list cx cy 0.0))
                      (cons 40 100.0) (cons 1 "ОКНО")
                      (cons 72 1) (cons 11 (list cx cy 0.0))
                      '(62 . 4)))
      (if (and name (mk:strp name))
        (entmakex (list '(0 . "TEXT")
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
      (entmakex (list '(0 . "LWPOLYLINE") '(90 . 4) '(70 . 1) '(62 . 6)
                      (cons 10 (list x1 y1))
                      (cons 10 (list x2 y1))
                      (cons 10 (list x2 y2))
                      (cons 10 (list x1 y2))))
      (entmakex (list '(0 . "TEXT")
                      (cons 10 (list cx cy 0.0))
                      (cons 40 100.0) (cons 1 "ДВЕРЬ")
                      (cons 72 1) (cons 11 (list cx cy 0.0))
                      '(62 . 6)))
      (if (and name (mk:strp name))
        (entmakex (list '(0 . "TEXT")
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
          (entmakex (list '(0 . "LINE")
                          (cons 10 (list (car pt) (cadr pt) 0.0))
                          (cons 11 (list (car pt) (+ (cadr pt) len) 0.0))
                          '(62 . 1) (cons 6 "DASHED")))
          (entmakex (list '(0 . "LINE")
                          (cons 10 (list (car pt) (cadr pt) 0.0))
                          (cons 11 (list (car pt) (+ (cadr pt) len) 0.0))
                          '(62 . 1))))
        (entmakex (list '(0 . "POINT")
                        (cons 10 (list (car pt) (cadr pt) 0.0))
                        '(62 . 2))))))
  (foreach beam beams
    (setq pt  (cdr (assoc 'INS_PT beam))
          len (if (cdr (assoc 'LENGTH beam)) (cdr (assoc 'LENGTH beam)) 0.0))
    (if pt
      (entmakex (list '(0 . "LINE")
                      (cons 10 (list (car pt) (cadr pt) 0.0))
                      (cons 11 (list (+ (car pt) len) (cadr pt) 0.0))
                      '(62 . 3))))))

(defun mk:draw-test-model (posts beams fills windows doors bounds / layer prev-layer)
  (setq layer "MK_TEST_MODEL")
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
  (prompt (strcat "\n[OK] Тестовая модель отрисована на слое: " layer)))

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
        (write-line (strcat "  LENGTH: " (vl-prin1-to-string (cdr (assoc 'LENGTH post)))) f)
        (write-line (strcat "  PROFILE: " (if (cdr (assoc 'PROFILE post)) (cdr (assoc 'PROFILE post)) "НЕТ")) f)
        (write-line (strcat "  PROTRUDING: " (if (cdr (assoc 'PROTRUDING post)) "ДА" "НЕТ")) f)
        (write-line (strcat "  LEFT_CONN: " (itoa (length (cdr (assoc 'LEFT_CONN post))))) f)
        (write-line (strcat "  RIGHT_CONN: " (itoa (length (cdr (assoc 'RIGHT_CONN post))))) f)
        (write-line "" f))
      (write-line "--- РИГЕЛИ ---" f)
      (foreach beam beams
        (write-line (strcat "  INS_PT: " (vl-prin1-to-string (cdr (assoc 'INS_PT beam)))) f)
        (write-line (strcat "  LENGTH: " (vl-prin1-to-string (cdr (assoc 'LENGTH beam)))) f)
        (write-line (strcat "  PROFILE: " (if (cdr (assoc 'PROFILE beam)) (cdr (assoc 'PROFILE beam)) "НЕТ")) f)
        (write-line (strcat "  TOP_ELEM: " (if (cdr (assoc 'TOP_ELEM beam)) "Есть" "Нет")) f)
        (write-line (strcat "  BOT_ELEM: " (if (cdr (assoc 'BOT_ELEM beam)) "Есть" "Нет")) f)
        (write-line "" f))
      (write-line "--- ЗАПОЛНЕНИЯ ---" f)
      (foreach fill fills
        (write-line (strcat "  INS_PT: " (vl-prin1-to-string (cdr (assoc 'INS_PT fill)))) f)
        (write-line (strcat "  HEIGHT: " (vl-prin1-to-string (cdr (assoc 'HEIGHT fill)))) f)
        (write-line (strcat "  WIDTH: " (vl-prin1-to-string (cdr (assoc 'WIDTH fill)))) f)
        (write-line (strcat "  THICKNESS: " (if (cdr (assoc 'THICKNESS fill)) (cdr (assoc 'THICKNESS fill)) "НЕТ")) f)
        (write-line (strcat "  PROFILE: " (if (cdr (assoc 'PROFILE fill)) (cdr (assoc 'PROFILE fill)) "НЕТ")) f)
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

(defun mk:group-by-size-profile (elements / groups key)
  (setq groups nil)
  (foreach el elements
    (setq key (strcat
      (mk:size-key (mk:rec-get el 'LENGTH))
      "_"
      (if (mk:rec-get el 'PROFILE)
        (mk:rec-get el 'PROFILE) "БЕЗ_ПРОФИЛЯ")))
    (setq groups (mk:group-add groups key el)))
  (mk:groups-finish groups))

(defun mk:group-beams (beams / groups key)
  (setq groups nil)
  (foreach beam beams
    (setq key (strcat
      (mk:size-key (mk:rec-get beam 'LENGTH))
      "_"
      (if (mk:rec-get beam 'PROFILE)
        (mk:rec-get beam 'PROFILE) "БЕЗ_ПРОФИЛЯ")
      "_"
      (if (mk:rec-get beam 'SUFFIX)
        (mk:rec-get beam 'SUFFIX) "БЕЗ_СУФФИКСА")))
    (setq groups (mk:group-add groups key beam)))
  (mk:groups-finish groups))

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

;; Одна выборка чертежа на команду (раньше был ssget "_X" на каждый тип блока)
(defun mk:ss-all-inserts ()
  (ssget "_X" (list (cons 0 "INSERT"))))

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

(defun c:МАРКАЗАПОЛН-СТ (/ ss posts prefix groups idx mark-str
                           count skip-count res doc)
  (prompt "\n[МАРКАЗАПОЛН-СТ] Автозаполнение атрибутов стоек...")
  (setq prefix (mk:get-vitrage-prefix))
  (prompt (strcat "\n  Префикс витража: " prefix))
  (setq ss    (mk:ss-all-inserts))
  (setq posts (mk:find-blocks-in-ss ss (strcat "*" *mk:block-post* "*")))
  (if (null posts)
    (prompt "\n  [INFO] Блоки стоек не найдены.")
    (progn
      (setq posts (mapcar 'mk:collect-post posts))
      (prompt (strcat "\n  Найдено стоек: " (itoa (length posts))))
      (setq groups (mk:group-by-size-profile posts))
      (prompt (strcat "\n  Уникальных групп: " (itoa (length groups))))
      (setq idx 1 count 0 skip-count 0)
      (setq doc (mk:undo-begin))
      (foreach grp groups
        (setq mark-str (strcat prefix " Ст" (itoa idx)))
        (setq res (mk:write-marks (cadr grp) mark-str))
        (prompt (strcat "\n  " mark-str " — шт.: " (itoa (car res))))
        (setq count      (+ count (car res))
              skip-count (+ skip-count (cadr res))
              idx        (1+ idx)))
      (mk:undo-end doc)
      (prompt (strcat "\n[ГОТОВО] Заполнено: " (itoa count)
                      ", Пропущено: " (itoa skip-count)))))
  (princ))

(defun c:МАРКАЗАПОЛН-РГ (/ ss beams fills windows doors posts all-panels
                           prefix groups idx mark-str suffix count skip-count
                           bounds new-beams beam first-beam res doc)
  (prompt "\n[МАРКАЗАПОЛН-РГ] Автозаполнение атрибутов ригелей...")
  (setq prefix (mk:get-vitrage-prefix))
  (prompt (strcat "\n  Префикс витража: " prefix))
  (setq ss      (mk:ss-all-inserts))
  (setq posts   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-post* "*")))
  (setq beams   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-beam* "*")))
  (setq fills   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-fill* "*")))
  (setq windows (mk:find-blocks-in-ss ss (strcat "*" *mk:block-window* "*")))
  (setq doors   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-door* "*")))
  (if (null beams)
    (prompt "\n  [INFO] Блоки ригелей не найдены.")
    (progn
      (setq posts   (mapcar 'mk:collect-post posts))
      (setq beams   (mapcar 'mk:collect-beam beams))
      (setq fills   (mapcar 'mk:collect-fill fills))
      (setq windows (mapcar 'mk:collect-window windows))
      (setq doors   (mapcar 'mk:collect-door doors))
      (setq all-panels (append fills windows doors))
      (setq bounds (mk:find-vitrage-bounds posts beams))
      (prompt "\n  Построение топологии для суффиксов...")
      (setq new-beams nil)
      (foreach beam beams
        (setq beam (mk:rec-put beam 'TOP_ELEM (mk:find-top-element beam all-panels)))
        (setq beam (mk:rec-put beam 'BOT_ELEM (mk:find-bot-element beam all-panels)))
        (setq suffix (mk:get-beam-suffix beam all-panels doors bounds))
        (setq beam (mk:rec-put beam 'SUFFIX suffix))
        (setq new-beams (cons beam new-beams)))
      (setq beams (reverse new-beams))
      (setq groups (mk:group-beams beams))
      (prompt (strcat "\n  Уникальных групп: " (itoa (length groups))))
      (setq idx 1 count 0 skip-count 0)
      (setq doc (mk:undo-begin))
      (foreach grp groups
        (setq first-beam (car (cadr grp)))
        (setq suffix (if (mk:rec-get first-beam 'SUFFIX)
                       (mk:rec-get first-beam 'SUFFIX) ""))
        (if (and suffix (> (strlen suffix) 0))
          (setq mark-str (strcat prefix " Рг" (itoa idx) suffix))
          (setq mark-str (strcat prefix " Рг" (itoa idx))))
        (setq res (mk:write-marks (cadr grp) mark-str))
        (prompt (strcat "\n  " mark-str " — шт.: " (itoa (car res))))
        (setq count      (+ count (car res))
              skip-count (+ skip-count (cadr res))
              idx        (1+ idx)))
      (mk:undo-end doc)
      (prompt (strcat "\n[ГОТОВО] Заполнено: " (itoa count)
                      ", Пропущено: " (itoa skip-count)))))
  (princ))

;;;=====================================================================
;;; 19. ГЛАВНАЯ КОМАНДА
;;;=====================================================================
(defun c:МАРКАСБОР (/ ss vitrage vitrage-pt posts beams fills windows doors
                      bounds topo-result all-panels dup-count overlap-count
                      out-count post-result protruding-count mlines
                      mline-result use-mlines mode-kw post-enames beam-enames)
  (prompt "\n[МАРКАСБОР] Сбор данных и построение 2D модели витража...")
  (prompt "\nВыберите элементы витража (рамкой): ")
  (setq ss (ssget))
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
(defun c:МАРКАМОДЕЛЬ (/ posts beams fills windows doors bounds)
  (prompt "\n[МАРКАМОДЕЛЬ] Тестовая отрисовка модели витража...")
  (if (null *mk:cached-data*)
    (progn
      (prompt "\n  Данные не найдены. Запуск сбора...")
      (c:МАРКАСБОР)))
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
    (prompt "\n[ERROR] Нет данных. Запустите МАРКАСБОР."))
  (princ))

;;;=====================================================================
;;; 21. ИНИЦИАЛИЗАЦИЯ С АВТОПРОВЕРКОЙ
;;;=====================================================================
(prompt (strcat "\n[MarkZV] Загружен " *mk:rev* " build " *mk:build* "."))
(prompt "\n  Команды:")
(prompt "\n    МАРКАСБОР       - Сбор данных и построение 2D модели")
(prompt "\n    МАРКАМОДЕЛЬ     - Тестовая отрисовка модели")
(prompt "\n    МАРКАЗАПОЛН-СТ  - Автозаполнение атрибутов стоек")
(prompt "\n    МАРКАЗАПОЛН-РГ  - Автозаполнение атрибутов ригелей")
(prompt "\n    MK_CHECK        - Проверка баланса скобок файла")
(princ)
