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
(setq *mk:ver*            "2.5")

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
(setq *mk:mirror-suffixes*      '(".1" ".2"))  ; зеркальная пара: Ст1.1 / Ст1.2
(setq *mk:suffix-small*         "м")     ; малый профиль
(setq *mk:suffix-big*           "б")     ; большой профиль
;; Ручная таблица артикул -> "м"/"б" (приоритет над автоопределением по габариту).
;; Заполняется из базы СИАЛ: (("КП45551" . "м") ("КП45364" . "б"))
(setq *mk:article-size*         nil)
(setq *mk:layer-model*          "Сетка витража")
(setq *mk:layer-test*           "Сетка витража тест")
(setq *mk:group-model*          "Сетка_витража")        ; имя группы — без пробелов
(setq *mk:group-test*           "Сетка_витража_тест")
;; Припуск длины ригеля относительно светового проёма (СИАЛ КП50/КП50К):
;; +12.5 мм на сторону, итого +25 мм к длине мультилинии/динамики.
(setq *mk:beam-allowance*       25.0)
;; Пакетный режим (команда МАРКАВ): общая выборка, один UNDO, без повторных вопросов
(setq *mk:batch*                nil)
(setq *mk:batch-ss*             nil)
(setq *mk:batch-mode*           nil)   ; "Блоки" / "Мультилинии" / "Все-типы"
(setq *mk:scope-mode*           "Блоки") ; режим последнего сбора — его же берут МАРКАВСТ/МАРКАВРГ
(setq *mk:batch-keep-grid*      nil)   ; оставлять ли сетку после пакета
;; Режим «Все-типы»: слияние параллельных линий одного профиля и отсев мусора
(setq *mk:merge-width*          200.0) ; макс. расстояние между линиями профиля
(setq *mk:min-seg*              50.0)  ; короче — не элемент каркаса
;; Сшивать ли коллинеарные куски одного профиля, идущие встык (мм зазора).
;; 0 — не сшивать. Для стоек, нарисованных по ячейкам, поставьте 60.0.
(setq *mk:join-posts-gap*       0.0)
(setq *mk:join-beams-gap*       0.0)
;; Выноски марок для элементов без атрибута «Марка»
(setq *mk:layer-label*          "Обозначения")   ; слой выносок
(setq *mk:group-label*          "Марки_выноски")  ; группа выносок
(setq *mk:label-color*          2)                ; жёлтый
(setq *mk:label-style*          "Основной стиль (надписи без наклона)")
(setq *mk:label-height*         40.0)             ; высота текста
(setq *mk:label-offset*         25.0)             ; отступ от грани профиля
(setq *mk:label-end-gap*        100.0)            ; недоход до края профиля
(setq *mk:label-line-gap*       1.4)              ; межстрочие (x высоту)
(setq *mk:label-rot-post*       90.0)             ; поворот подписи стойки, град
(setq *mk:cross-fallback*       50.0)             ; габарит сечения, если неизвестен
;; Типы объектов, считающихся профилем (полилинии и линии игнорируются).
(setq *mk:seg-types*            '("MLINE"))
;;; Т-соединения
(setq *mk:tol-tjoint*           30.0)   ; допуск примыкания торца к ригелю
(setq *mk:tol-tcenter*          30.0)   ; допуск «Т строго по центру ригеля»
(setq *mk:suffix-tjoint-lo*     ".1")   ; ригель под вертикальным элементом
(setq *mk:suffix-tjoint-hi*     ".2")   ; ригель над вертикальным элементом
(setq *mk:suffix-vert-beam*     "т")    ; вертикальный элемент с двумя Т
(setq *mk:label-short-len*      400.0)  ; короткий ригель: подпись по центру
(setq *mk:thick-warm-min*       42.0)
(setq *mk:thick-warm-max*       60.0)
(setq *mk:thick-cold-min*       4.0)
(setq *mk:thick-cold-max*       32.0)

;;;=====================================================================
;;; 2a. БАЗЫ ПРОФИЛЬНЫХ СИСТЕМ — ГАБАРИТ СЕЧЕНИЯ, мм
;;;   Любое число систем: каждая регистрируется своим именем.
;;;   Внешние базы кладутся в файл MarkZV-bases.lsp рядом с модулем
;;;   (или в любую папку из путей поиска AutoCAD) и подхватываются при загрузке.
;;;   Формат файла базы:
;;;     (mk:register-base "АЛЮТЕХ" '(("ALT-W72-01" . 72.0) ("ALT-W72-05" . 116.0)))
;;;     (setq *mk:article-prefixes* (cons "ALT" *mk:article-prefixes*))
;;;=====================================================================
(setq *mk:size-bases* nil)          ; ((имя . таблица) ...)
(setq *mk:bases-file* "MarkZV-bases.lsp")   ; файл внешних баз

;; Префиксы артикулов, распознаваемые в именах стилей мультилиний и слоёв
(setq *mk:article-prefixes* '("КП" "KP"))

(defun mk:register-base (name tbl / hit)
  (setq hit (assoc name *mk:size-bases*))
  (if hit
    (setq *mk:size-bases* (subst (cons name tbl) hit *mk:size-bases*))
    (setq *mk:size-bases* (cons (cons name tbl) *mk:size-bases*)))
  (length tbl))

;;;=====================================================================
;;; 2b. БАЗА СИАЛ — ГАБАРИТ СЕЧЕНИЯ ПРОФИЛЯ, мм
;;;   Источник: "База СИАЛ.xlsx", листы "База СИАЛ КП50" и "База СИАЛ КП50К",
;;;   разделы "Стойка" и "Ригель" (колонка "Габарит").
;;;   Сгенерировано tools/sial_parse.py --emit.
;;;=====================================================================
(setq *mk:sial-sizes*
  '(
    ("КП45453" . 21.0)
    ("КПС993" . 23.0)
    ("КП45367" . 27.0)
    ("КП45371" . 46.0)
    ("КПС372" . 46.0)
    ("КП45388" . 48.0)
    ("КПС009БЕЗУСОВ" . 54.0)
    ("КП45369" . 68.0)
    ("КПС371" . 68.0)
    ("КП45303-2" . 70.0)
    ("КП45303-3" . 70.0)
    ("КПС180" . 70.0)
    ("КП45366" . 76.0)
    ("КПС998" . 76.0)
    ("КПС913" . 86.0)
    ("КП45304" . 88.0)
    ("КПС919" . 90.0)
    ("КПС921" . 90.0)
    ("КПС1209" . 94.0)
    ("КПС1067" . 98.0)
    ("КП45302-1" . 100.0)
    ("КП45302-2" . 100.0)
    ("КП45370" . 104.0)
    ("КПС1272ОБЛЕГЧ." . 104.0)
    ("КПС818" . 104.0)
    ("КПС1164" . 106.0)
    ("КПС1161" . 110.0)
    ("КПС1163" . 110.0)
    ("КПС298ГН.УСЫ" . 114.0)
    ("КП45551" . 116.0)
    ("КП45551-3" . 116.0)
    ("КП45548" . 120.0)
    ("КП45550" . 120.0)
    ("КПС1275ОБЛЕГЧ." . 120.0)
    ("КП45562" . 128.0)
    ("КПС1165" . 130.0)
    ("КПС299ГН.УСЫ" . 130.0)
    ("КП45387" . 144.0)
    ("КП45372" . 148.0)
    ("КПС344" . 148.0)
    ("КПС491УГЛ." . 148.0)
    ("КПС927" . 152.0)
    ("КПС924" . 155.0)
    ("КПС926" . 155.0)
    ("КПС492ГН.УСЫ" . 158.0)
    ("КПС584" . 165.0)
    ("КПС586" . 165.0)
    ("КП45364" . 172.0)
    ("КП45392" . 178.0)
    ("КПС345" . 178.0)
    ("КПС494ГН.УСЫ" . 187.0)
    ("КПС170" . 200.0)
    ("КПС634" . 205.0)
    ("КПС636" . 205.0)
    ("КПС015" . 210.0)
    ("КПС014" . 215.0)
    ("КПС475" . 215.0)
    ("КПС496ГН.УСЫ" . 224.0)
    ("КПС171" . 235.0)
    ("КПС370" . 240.0)
    ("КПС426" . 240.0)
    ("КПС718" . 240.0)
    ("КПС1025ГН.УСЫ" . 250.0)
    ("КПС633" . 270.0)
    ("КПС829" . 270.0)
    ("КПС437" . 280.0)
    ("КПС439" . 280.0)
    ("КПС801" . 280.0)
   )
)

;; Ключ артикула: без пробелов, в верхнем регистре ("КПС 993" -> "КПС993")
(defun mk:art-key (s / out i c)
  (if (null s)
    ""
    (progn
      (setq out "" i 1)
      (while (<= i (strlen s))
        (setq c (substr s i 1))
        (if (and (/= c " ") (/= c "\t")) (setq out (strcat out c)))
        (setq i (1+ i)))
      (strcase out))))

;; Габарит сечения профиля по базе СИАЛ; nil, если артикул не найден.
;; Сначала точное совпадение, затем исполнение профиля: "КП45303" -> "КП45303-2".
(mk:register-base "СИАЛ" *mk:sial-sizes*)

;; Поиск артикула в одной таблице: точное совпадение, затем исполнение
;; профиля («КП45303» -> «КП45303-2»)
(defun mk:size-in-table (k tbl / hit out)
  (setq out nil hit (assoc k tbl))
  (if hit
    (setq out (cdr hit))
    (foreach pair tbl
      (if (and (null out)
               (= (substr (car pair) 1 (1+ (strlen k))) (strcat k "-")))
        (setq out (cdr pair)))))
  out)

;; Габарит сечения по всем зарегистрированным базам; nil, если не найден
(defun mk:sial-size (art / k out)
  (setq k (mk:art-key art) out nil)
  (if (> (strlen k) 0)
    (foreach b *mk:size-bases*
      (if (null out) (setq out (mk:size-in-table k (cdr b))))))
  out)

;; Габарит сечения: база СИАЛ в приоритете, иначе замер по блоку
(defun mk:cross-size (el / s)
  (setq s (mk:sial-size (mk:rec-get el 'ARTICLE)))
  (if s s (if (numberp (mk:rec-get el 'CROSS)) (mk:rec-get el 'CROSS) 0.0)))

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

(defun mk:blk-match? (ename target / obj eff-name names nm found ed)
  (setq found nil names nil ed (entget ename))
  ;; ВАЖНО: только вставки блоков. У MLINE в DXF 2 лежит имя стиля мультилинии
  ;; (например «Стойка (вертикальный профиль)») — без этой проверки
  ;; мультилинии принимались за блоки.
  (if (not (= (cdr (assoc 0 ed)) "INSERT"))
    (setq names nil ed nil))
  (if (null ed)
    nil
    (progn
  (setq nm (cdr (assoc 2 ed)))
  (if (mk:strp nm) (setq names (cons nm names)))
  (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ename)))
  (if (and (not (vl-catch-all-error-p obj)) obj)
    (progn
      (setq nm (mk:ax-get obj "EffectiveName"))
      (if (and (mk:strp nm) (not (member nm names)))
        (setq names (cons nm names)))))
  (foreach nm names
    (if (and (null found) (wcmatch (strcase nm) (strcase target)))
      (setq found t)))))
  found)

;;;=====================================================================
;;; 3. ИЗВЛЕЧЕНИЕ АТРИБУТОВ
;;;=====================================================================
;; У обычных объектов (MLINE, LINE, LWPOLYLINE…) entnext возвращает СЛЕДУЮЩИЙ
;; объект чертежа, а не подобъект. Без этой проверки обход уходил в соседние
;; блоки и читал/писал ЧУЖИЕ атрибуты «Марка».
(defun mk:has-attrs-insert? (ename / ed)
  (setq ed (if ename (entget ename) nil))
  (and ed
       (= (cdr (assoc 0 ed)) "INSERT")
       (equal (cdr (assoc 66 ed)) 1)))

(defun mk:get-attr (ename tag / sub data val)
  (if (not (mk:has-attrs-insert? ename))
    (setq sub nil val nil)
    (setq sub (entnext ename) val nil))
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
  (if (not (mk:has-attrs-insert? ename))
    (setq sub nil found nil)
    (setq sub (entnext ename) found nil))
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

;;;---------------------------------------------------------------------
;;; 8a. РЕЖИМ «ВСЕ-ТИПЫ»: КАРКАС ИЗ ЛЮБОЙ ГЕОМЕТРИИ ВЫБОРКИ
;;;   LINE / LWPOLYLINE / POLYLINE / MLINE + блоки стоек и ригелей.
;;;   Донор идеи: mark:fill-extract-segs / mark:fill-verts-to-segs (MarkZ).
;;;---------------------------------------------------------------------
;; Артикул профиля из строки: «Стойка КП45302 …» -> «КП45302», «КПС 1155» -> «КПС1155»
(defun mk:article-from-name (s / up n i c out j ch pfx plen k)
  (setq out nil)
  (if (mk:strp s)
    (progn
      (setq up (strcase s) n (strlen up) i 1)
      (while (and (<= i n) (null out))
        ;; какой из известных префиксов начинается в позиции i?
        (setq pfx nil)
        (foreach pr *mk:article-prefixes*
          (if (and (null pfx)
                   (= (substr up i (strlen pr)) (strcase pr)))
            (setq pfx (strcase pr))))
        (if pfx
          (progn
            (setq plen (strlen pfx) out pfx j (+ i plen) k 0)
            ;; до двух необязательных букв исполнения (КПС, КПБ и т. п.)
            (while (and (< k 2)
                        (not (member (substr up j 1)
                                     '("" " " "0" "1" "2" "3" "4" "5" "6" "7"
                                       "8" "9" "-" "(" ")" "." ",")))) 
              (setq out (strcat out (substr up j 1)) j (1+ j) k (1+ k)))
            (while (= (substr up j 1) " ") (setq j (1+ j)))
            (setq ch (substr up j 1))
            (if (or (= ch "") (null (member ch '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))))
              (setq out nil)                     ; префикс без цифр — не артикул
              (progn
                (while (member (substr up j 1)
                               '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "-"))
                  (setq out (strcat out (substr up j 1)) j (1+ j)))
                ;; хвостовой дефис не нужен
                (if (= (substr out (strlen out) 1) "-")
                  (setq out (substr out 1 (1- (strlen out)))))))))
        (setq i (1+ i)))))
  out)

;; Артикул источника: имя стиля мультилинии (DXF 2), иначе имя слоя
(defun mk:ent-article (e / ed a)
  (setq ed (if e (entget e) nil) a nil)
  (if ed
    (progn
      (if (= (cdr (assoc 0 ed)) "MLINE")
        (setq a (mk:article-from-name (cdr (assoc 2 ed)))))
      (if (null a) (setq a (mk:article-from-name (cdr (assoc 8 ed)))))))
  a)

;; Отрезки одного объекта: список (x0 y0 x1 y1)
(defun mk:ent-segs (e / ed typ verts closed segs p sub)
  (setq ed (entget e) segs nil)
  (if (null ed)
    nil
    (progn
      (setq typ (cdr (assoc 0 ed)))
      (cond
        ((= typ "LINE")
         (setq p (cdr (assoc 10 ed)) sub (cdr (assoc 11 ed)))
         (if (and p sub)
           (setq segs (list (list (float (car p))   (float (cadr p))
                                  (float (car sub)) (float (cadr sub)))))))
        ((= typ "LWPOLYLINE")
         (setq verts nil closed nil)
         (if (and (cdr (assoc 70 ed)) (= 1 (logand 1 (cdr (assoc 70 ed)))))
           (setq closed t))
         (foreach p ed
           (if (= (car p) 10)
             (setq verts (cons (list (float (cadr p)) (float (caddr p))) verts))))
         (setq verts (reverse verts))
         (if closed (setq verts (append verts (list (car verts)))))
         (if (>= (length verts) 2) (setq segs (mk:verts-to-segs verts))))
        ((= typ "POLYLINE")
         (setq verts nil closed nil)
         (if (and (cdr (assoc 70 ed)) (= 1 (logand 1 (cdr (assoc 70 ed)))))
           (setq closed t))
         (setq sub (entnext e))
         (while sub
           (setq p (entget sub))
           (if (or (null p) (= "SEQEND" (cdr (assoc 0 p))))
             (setq sub nil)
             (progn
               (if (= "VERTEX" (cdr (assoc 0 p)))
                 (setq verts (cons (list (float (cadr (assoc 10 p)))
                                         (float (caddr (assoc 10 p)))) verts)))
               (setq sub (entnext sub)))))
         (setq verts (reverse verts))
         (if closed (setq verts (append verts (list (car verts)))))
         (if (>= (length verts) 2) (setq segs (mk:verts-to-segs verts))))
        ((= typ "MLINE")
         (setq verts (mk:get-mline-verts e))
         (if (and verts (>= (length verts) 2))
           (setq segs (mk:verts-to-segs verts))))
        (t nil))
      segs)))

;; Все отрезки выборки (кроме блоков)
(defun mk:collect-segs-in-ss (ss / i e typ out segs)
  (setq out nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e   (ssname ss i)
              typ (cdr (assoc 0 (entget e))))
        (if (member typ *mk:seg-types*)
          (progn
            (setq segs (mk:ent-segs e))
            (foreach sg segs (setq out (cons (cons e sg) out)))))
        (setq i (1+ i)))))
  (reverse out))

;; Слияние параллельных отрезков одного профиля:
;; ось = среднее, протяжённость = объединение, сечение = разброс осей.
;; rec: (ename ось нач кон)
(defun mk:merge-tracks (recs gap / sorted out cur base ax a b)
  (setq sorted (vl-sort recs '(lambda (p q) (< (nth 1 p) (nth 1 q))))
        out nil cur nil)
  (foreach r sorted
    (if (and cur
             (<= (- (nth 1 r) (nth 1 (car cur))) *mk:merge-width*)
             (> (+ (min (nth 3 r) (apply 'max (mapcar '(lambda (x) (nth 3 x)) cur))) gap)
                (max (nth 2 r) (apply 'min (mapcar '(lambda (x) (nth 2 x)) cur)))))
      (setq cur (cons r cur))
      (progn
        (if cur (setq out (cons (reverse cur) out)))
        (setq cur (list r)))))
  (if cur (setq out (cons (reverse cur) out)))
  (reverse out))

;; Габарит сечения мультилинии: (max-min смещений стиля) x масштаб элемента
(defun mk:mline-cross (e / ed sd offs w sc)
  (setq ed (if e (entget e) nil) w nil)
  (if (and ed (= (cdr (assoc 0 ed)) "MLINE") (cdr (assoc 340 ed)))
    (progn
      (setq sc   (if (numberp (cdr (assoc 40 ed))) (abs (cdr (assoc 40 ed))) 1.0)
            sd   (entget (cdr (assoc 340 ed)))
            offs nil)
      (foreach pair sd
        (if (= (car pair) 49) (setq offs (cons (cdr pair) offs))))
      (if (> (length offs) 1)
        (setq w (* sc (- (apply 'max offs) (apply 'min offs)))))))
  (if (and (numberp w) (> w 1.0)) w nil))

(defun mk:track-elem (grp etype / axs a0 a1 s0 s1 cross)
  (setq axs   (mapcar '(lambda (x) (nth 1 x)) grp)
        a0    (apply 'min axs)
        a1    (apply 'max axs)
        s0    (apply 'min (mapcar '(lambda (x) (nth 2 x)) grp))
        s1    (apply 'max (mapcar '(lambda (x) (nth 3 x)) grp))
        cross (- a1 a0))
  (list (nth 0 (car grp))            ; ename (первый объект группы)
        (/ (+ a0 a1) 2.0)            ; ось
        s0 s1                        ; протяжённость
        ;; габарит сечения: из геометрии, иначе из стиля мультилинии
        (cond
          ((> cross 1.0) cross)
          ((mk:mline-cross (nth 0 (car grp))) (mk:mline-cross (nth 0 (car grp))))
          (t 0.0))
        etype))

;; Каркас из геометрии + блоков. Блоки в приоритете (у них артикул и «Марка»).
(defun mk:extract-posts-beams-all (ss / segs vrecs hrecs cls len posts beams
                                     bposts bbeams tr e ax s0 s1 cross)
  (setq bposts (mapcar 'mk:collect-post
                 (mk:find-blocks-in-ss ss (strcat "*" *mk:block-post* "*")))
        bbeams (mapcar 'mk:collect-beam
                 (mk:find-blocks-in-ss ss (strcat "*" *mk:block-beam* "*"))))
  (setq segs (mk:collect-segs-in-ss ss) vrecs nil hrecs nil)
  (foreach r segs
    (setq e   (car r)
          cls (mk:classify-seg (cdr r)))
    (cond
      ((= cls 'v)
       (setq len (abs (- (nth 4 r) (nth 2 r))))
       (if (> len *mk:min-seg*)
         (setq vrecs (cons (list e (/ (+ (nth 1 r) (nth 3 r)) 2.0)
                                 (min (nth 2 r) (nth 4 r))
                                 (max (nth 2 r) (nth 4 r))) vrecs))))
      ((= cls 'h)
       (setq len (abs (- (nth 3 r) (nth 1 r))))
       (if (> len *mk:min-seg*)
         (setq hrecs (cons (list e (/ (+ (nth 2 r) (nth 4 r)) 2.0)
                                 (min (nth 1 r) (nth 3 r))
                                 (max (nth 1 r) (nth 3 r))) hrecs))))))
  (setq posts nil beams nil)
  (foreach grp (mk:merge-tracks vrecs *mk:join-posts-gap*)
    (setq tr (mk:track-elem grp "СТОЙКА")
          ax (nth 1 tr) s0 (nth 2 tr) s1 (nth 3 tr) cross (nth 4 tr))
    (if (not (mk:elem-covered? ax s0 s1 bposts "СТОЙКА"))
      (setq posts (cons (list
        (cons 'TYPE "СТОЙКА") (cons 'ENAME (nth 0 tr))
        (cons 'INS_PT (list ax s0 0.0))
        (cons 'LENGTH (- s1 s0)) (cons 'SIZE_SRC "GEOM")
        (cons 'CROSS cross) (cons 'ARTICLE (mk:ent-article (nth 0 tr)))
        (cons 'VISIBILITY nil)
        (cons 'LEFT_CONN nil) (cons 'RIGHT_CONN nil) (cons 'PROTRUDING nil)
      ) posts))))
  (foreach grp (mk:merge-tracks hrecs *mk:join-beams-gap*)
    (setq tr (mk:track-elem grp "РИГЕЛЬ")
          ax (nth 1 tr) s0 (nth 2 tr) s1 (nth 3 tr) cross (nth 4 tr))
    (if (not (mk:elem-covered? ax s0 s1 bbeams "РИГЕЛЬ"))
      (setq beams (cons (list
        (cons 'TYPE "РИГЕЛЬ") (cons 'ENAME (nth 0 tr))
        (cons 'INS_PT (list s0 ax 0.0))
        (cons 'LENGTH (- s1 s0)) (cons 'SIZE_SRC "GEOM")
        (cons 'CROSS cross) (cons 'ARTICLE (mk:ent-article (nth 0 tr)))
        (cons 'VISIBILITY nil)
        (cons 'TOP_ELEM nil) (cons 'BOT_ELEM nil) (cons 'SUFFIX nil)
      ) beams))))
  (prompt (strcat "\n  Все-типы: отрезков " (itoa (length segs))
                  ", из геометрии стоек " (itoa (length posts))
                  ", ригелей " (itoa (length beams))
                  "; из блоков " (itoa (length bposts))
                  "/" (itoa (length bbeams))))
  (list (append bposts (reverse posts))
        (append bbeams (reverse beams))))

;; Совпадает ли геометрический элемент с уже собранным блоком
(defun mk:elem-covered? (ax s0 s1 blocks etype / hit p bx b0 b1)
  (setq hit nil)
  (foreach b blocks
    (if (null hit)
      (progn
        (setq p (cdr (assoc 'INS_PT b)))
        (if p
          (progn
            (if (= etype "СТОЙКА")
              (setq bx (car p)
                    b0 (cadr p)
                    b1 (+ (cadr p) (if (cdr (assoc 'LENGTH b)) (cdr (assoc 'LENGTH b)) 0.0)))
              (setq bx (cadr p)
                    b0 (car p)
                    b1 (+ (car p) (if (cdr (assoc 'LENGTH b)) (cdr (assoc 'LENGTH b)) 0.0))))
            (if (and (<= (abs (- ax bx)) *mk:merge-width*)
                     (> (min s1 b1) (max s0 b0)))
              (setq hit t)))))))
  hit)

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
;; Элемент каркаса пригоден, если есть точка вставки и положительная длина
(defun mk:valid-frame? (el / p l)
  (setq p (cdr (assoc 'INS_PT el))
        l (cdr (assoc 'LENGTH el)))
  (and p (listp p) (numberp (car p)) (numberp (cadr p))
       (numberp l) (> l 0.0)))

;; Панель пригодна, если есть точка вставки и положительные габариты
(defun mk:valid-panel? (el / p w h)
  (setq p (cdr (assoc 'INS_PT el))
        w (cdr (assoc 'WIDTH el))
        h (cdr (assoc 'HEIGHT el)))
  (and p (listp p) (numberp (car p)) (numberp (cadr p))
       (numberp w) (> w 0.0) (numberp h) (> h 0.0)))

(defun mk:sanitize-panels (els label / out bad)
  (setq out nil bad 0)
  (foreach el els
    (if (mk:valid-panel? el)
      (setq out (cons el out))
      (setq bad (1+ bad))))
  (if (> bad 0)
    (prompt (strcat "\n  [WARN] " label ": отброшено без габаритов — " (itoa bad))))
  (reverse out))

(defun mk:sanitize-frame (els label / out bad)
  (setq out nil bad 0)
  (foreach el els
    (if (mk:valid-frame? el)
      (setq out (cons el out))
      (setq bad (1+ bad))))
  (if (> bad 0)
    (prompt (strcat "\n  [WARN] " label ": отброшено без размеров — " (itoa bad))))
  (reverse out))

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

;; Создание слоя через таблицу символов: имя может содержать пробелы,
;; поэтому (command "_.LAYER" ...) не годится — пробел там = Enter.
(defun mk:ensure-layer (name color)
  (if (null (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name)
                   (cons 70 0)
                   (cons 62 color)
                   '(6 . "Continuous"))))
  name)

(defun mk:emk (dxf / e)
  (setq e (entmakex dxf))
  (if e (setq *mk:drawn* (cons e *mk:drawn*)))
  e)

;; Группа объектов модели — чтобы можно было удалить одним выбором
;; (донор: mark:ar-make-group из MarkZ)
(defun mk:make-group (name enames / doc groups old grp arr i n)
  (if (and enames (> (length enames) 0))
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
  (setq layer *mk:layer-model*)
  (setq *mk:drawn* nil)
  (mk:ensure-layer layer 7)
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
  (setq layer *mk:layer-test*)
  (setq *mk:drawn* nil)
  (mk:ensure-layer layer 7)
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
        (write-line (strcat "  СЕЧЕНИЕ: " (vl-prin1-to-string (cdr (assoc 'CROSS post)))
                            "  (СИАЛ: " (if (mk:sial-size (cdr (assoc 'ARTICLE post)))
                                          (rtos (mk:sial-size (cdr (assoc 'ARTICLE post))) 2 1)
                                          "нет") ")") f)
        (write-line (strcat "  LEFT_CONN: " (itoa (length (cdr (assoc 'LEFT_CONN post))))) f)
        (write-line (strcat "  RIGHT_CONN: " (itoa (length (cdr (assoc 'RIGHT_CONN post))))) f)
        (write-line "" f))
      (write-line "--- РИГЕЛИ ---" f)
      (foreach beam beams
        (write-line (strcat "  INS_PT: " (vl-prin1-to-string (cdr (assoc 'INS_PT beam)))) f)
        (write-line (strcat "  LENGTH: " (vl-prin1-to-string (cdr (assoc 'LENGTH beam)))
                            "  (источник: " (if (cdr (assoc 'SIZE_SRC beam)) (cdr (assoc 'SIZE_SRC beam)) "НЕТ") ")"
                            "  ПРОФИЛЬ: " (if (cdr (assoc 'LENGTH beam))
                                            (rtos (mk:beam-cut-len (cdr (assoc 'LENGTH beam))) 2 1) "?")) f)
        (write-line (strcat "  АРТИКУЛ: " (if (cdr (assoc 'ARTICLE beam)) (cdr (assoc 'ARTICLE beam)) "НЕТ")) f)
        (write-line (strcat "  TOP_ELEM: " (if (cdr (assoc 'TOP_ELEM beam)) "Есть" "Нет")) f)
        (write-line (strcat "  BOT_ELEM: " (if (cdr (assoc 'BOT_ELEM beam)) "Есть" "Нет")) f)
        (write-line (strcat "  Т-СОЕДИНЕНИЕ: "
                            (if (mk:rec-get beam 'TJOINT) (mk:rec-get beam 'TJOINT) "нет")
                            "   ВЕРТИКАЛЬНЫЙ: "
                            (if (mk:rec-get beam 'VERT) "да" "нет")) f)
        (write-line (strcat "  СУФФИКС: " (if (and (cdr (assoc 'SUFFIX beam))
                                                   (> (strlen (cdr (assoc 'SUFFIX beam))) 0))
                                            (cdr (assoc 'SUFFIX beam)) "нет")) f)
        (write-line (strcat "  СЕЧЕНИЕ: " (vl-prin1-to-string (cdr (assoc 'CROSS beam)))
                            "  (СИАЛ: " (if (mk:sial-size (cdr (assoc 'ARTICLE beam)))
                                          (rtos (mk:sial-size (cdr (assoc 'ARTICLE beam))) 2 1)
                                          "нет") ")") f)
        (write-line (strcat "  ДЛИНА №: " (if (cdr (assoc 'LEN_IDX beam))
                                            (itoa (cdr (assoc 'LEN_IDX beam))) "?")) f)
        (write-line (strcat "  РЯД: " (if (cdr (assoc 'ROW beam))
                                        (itoa (cdr (assoc 'ROW beam))) "?")
                            "   СТОЛБЕЦ: " (if (cdr (assoc 'BAY beam))
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
  (if (not (mk:has-attrs-insert? ename))
    (setq sub nil done nil)
    (setq sub (entnext ename) done nil))
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
  (cond
    ((mk:rec-get el 'VERT) (setq w 0.0))            ; вертикальный ригель (импост)
    ((= (cdr (assoc 'TYPE el)) "РИГЕЛЬ")
     (setq w (if (cdr (assoc 'LENGTH el)) (cdr (assoc 'LENGTH el)) 0.0)))
    (t (setq w (if (cdr (assoc 'WIDTH el)) (cdr (assoc 'WIDTH el)) 0.0))))
  (list x (+ x w)))

(defun mk:el-yrange (el / y h)
  (setq y (cadr (cdr (assoc 'INS_PT el))))
  (cond
    ((mk:rec-get el 'VERT)
     (setq h (if (cdr (assoc 'LENGTH el)) (cdr (assoc 'LENGTH el)) 0.0)))
    ((= (cdr (assoc 'TYPE el)) "РИГЕЛЬ") (setq h 0.0))
    (t (setq h (if (cdr (assoc 'HEIGHT el)) (cdr (assoc 'HEIGHT el)) 0.0))))
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

;; Подробная «подпись» стороны: тип + отметки примыкания относительно низа
;; стойки. Две стойки зеркальны, только если подписи сторон совпадают
;; накрест (одинаковое число примыканий на тех же отметках).
(defun mk:side-sig (post elements lo hi / span base out typ xr yr cx)
  (setq span (mk:post-span post)
        base (car span)
        out  nil)
  (if (and lo hi)
    (foreach el elements
      (if (cdr (assoc 'INS_PT el))
        (progn
          (setq xr (mk:el-xrange el)
                yr (mk:el-yrange el)
                cx (/ (+ (car xr) (cadr xr)) 2.0)
                typ (cdr (assoc 'TYPE el)))
          (if (and (> cx (- lo *mk:tol-adjacency*))
                   (< cx (+ hi *mk:tol-adjacency*))
                   (mk:overlap? (car yr) (cadr yr) (car span) (cadr span)))
            (setq out (cons (strcat typ ":" (mk:size-key (- (car yr) base))
                                    "-" (mk:size-key (- (cadr yr) base)))
                            out)))))))
  (if out (mk:join (vl-sort out '<) "+") "ПУСТО"))

;; Разметка стоек: LEFT_TYPES / RIGHT_TYPES / ORIENT / GROUP_KEY
(defun mk:annotate-posts (posts elements / axes out x lt rt ls rs art len canon)
  (setq axes (mk:post-axes posts) out nil)
  (foreach post posts
    (setq x   (car (cdr (assoc 'INS_PT post)))
          lt  (mk:side-types post elements (mk:axis-prev axes x) x)
          rt  (mk:side-types post elements x (mk:axis-next axes x))
          ls  (mk:side-sig   post elements (mk:axis-prev axes x) x)
          rs  (mk:side-sig   post elements x (mk:axis-next axes x))
          len (mk:size-key (mk:rec-get post 'LENGTH))
          art (if (mk:rec-get post 'ARTICLE) (mk:rec-get post 'ARTICLE) "БЕЗ_АРТИКУЛА"))
    ;; канонический ключ не зависит от того, зеркальна стойка или нет
    (setq canon (if (<= (strcase ls) (strcase rs))
                  (strcat ls ">" rs)
                  (strcat rs ">" ls)))
    (setq post (mk:rec-put post 'LEFT_TYPES  lt))
    (setq post (mk:rec-put post 'RIGHT_TYPES rt))
    (setq post (mk:rec-put post 'LEFT_SIG    ls))
    (setq post (mk:rec-put post 'RIGHT_SIG   rs))
    (setq post (mk:rec-put post 'ORIENT      (strcat ls ">" rs)))
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
;; Шкала размеров «в свету»: уникальные длины ригелей и ширины заполнений,
;; по возрастанию. Индекс в этой шкале и есть цифра марки ригеля —
;; так она совпадает с цифрой марки заполнения (в MarkZ: номер = ширина).
(defun mk:length-scale (beams panels / vals v out)
  (setq vals nil)
  (foreach b beams
    (setq v (mk:rec-get b 'LENGTH))
    ;; вертикальные ригели («т») нумеруются отдельно, после всех обычных,
    ;; чтобы не сдвигать соответствие номеров с заполнениями
    (if (and (numberp v) (null (mk:rec-get b 'VERT)))
      (setq vals (cons (atof (mk:size-key v)) vals))))
  (foreach pn panels
    (if (= (mk:rec-get pn 'TYPE) "ЗАПОЛНЕНИЕ")
      (progn
        (setq v (mk:rec-get pn 'WIDTH))
        (if (numberp v) (setq vals (cons (atof (mk:size-key v)) vals))))))
  (setq out nil)
  (foreach v (vl-sort vals '<)
    (if (not (member v out)) (setq out (cons v out))))
  (reverse out))

;; Шкала длин вертикальных ригелей (только они, по возрастанию)
(defun mk:vert-scale (beams / vals v out)
  (setq vals nil)
  (foreach b beams
    (setq v (mk:rec-get b 'LENGTH))
    (if (and (numberp v) (mk:rec-get b 'VERT))
      (setq vals (cons (atof (mk:size-key v)) vals))))
  (setq out nil)
  (foreach v (vl-sort vals '<)
    (if (not (member v out)) (setq out (cons v out))))
  (reverse out))

(defun mk:scale-index (v scale / i idx)
  (setq i 1 idx 0)
  (foreach s scale
    (if (and (= idx 0) (equal s v *mk:tol-size*)) (setq idx i))
    (setq i (1+ i)))
  idx)

;; Уровни ригелей (горизонтальные ряды) снизу вверх
(defun mk:row-levels (beams / ys sorted out last)
  (setq ys nil)
  (foreach b beams
    (if (cdr (assoc 'INS_PT b))
      (setq ys (cons (cadr (cdr (assoc 'INS_PT b))) ys))))
  (setq sorted (vl-sort ys '<) out nil last nil)
  (foreach v sorted
    (if (or (null out) (> (- v last) *mk:tol-adjacency*))
      (progn (setq out (cons v out)) (setq last v))))
  (reverse out))

;; Номер ряда (1 — самый нижний); 0 — уровень не распознан
(defun mk:row-index (y levels / i idx)
  (setq i 1 idx 0)
  (foreach lv levels
    (if (and (= idx 0) (<= (abs (- y lv)) *mk:tol-adjacency*)) (setq idx i))
    (setq i (1+ i)))
  idx)

(defun mk:article-cross-pairs (elements / out art cross)
  (setq out nil)
  (foreach el elements
    (setq art   (if (mk:rec-get el 'ARTICLE) (mk:rec-get el 'ARTICLE) "БЕЗ_АРТИКУЛА")
          cross (mk:cross-size el))
    (if (null (assoc art out))
      (setq out (cons (cons art cross) out))))
  (reverse out))

;; Таблица артикул -> "м"/"б": по габариту сечения (минимальный/максимальный).
;; Если артикул один — таблица пустая (маркер не нужен).
(defun mk:size-mark-table (elements / pairs sorted out)
  (setq pairs (mk:article-cross-pairs elements) out nil)
  (if (> (length pairs) 1)
    (progn
      (setq sorted (vl-sort pairs '(lambda (a b) (< (cdr a) (cdr b)))))
      (if (or (< (length sorted) 2)
              (equal (cdr (car sorted)) (cdr (last sorted)) *mk:tol-size*))
        (prompt (strcat "\n  [WARN] Габариты сечений артикулов ригелей одинаковы"
                        " — маркеры м/б не присвоены. Задайте *mk:article-size*."))
        (progn
          (setq out (list (cons (car (car sorted))  *mk:suffix-small*)
                          (cons (car (last sorted)) *mk:suffix-big*)))
          (if (> (length pairs) 2)
            (prompt (strcat "\n  [WARN] Артикулов ригелей: " (itoa (length pairs))
                            " — маркеры м/б присвоены только крайним по габариту.")))))))
  out)

(defun mk:size-mark (art table)
  (cond
    ((cdr (assoc art *mk:article-size*)) (cdr (assoc art *mk:article-size*)))
    ((cdr (assoc art table))             (cdr (assoc art table)))
    (t "")))

;; Разметка ригелей: TOP_ELEM / BOT_ELEM / SUFFIX / BAY / SIZE_MARK / GROUP_KEY
(defun mk:annotate-beams (beams all-panels doors bounds posts /
                          out suffix len art axes table mark bay rows row
                          scale vscale lq)
  (setq out   nil
        axes  (mk:post-axes posts)
        rows  (mk:row-levels beams)
        scale (mk:length-scale beams all-panels)
        vscale (mk:vert-scale beams)
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
    (setq row (mk:row-index (cadr (cdr (assoc 'INS_PT beam))) rows))
    (setq beam (mk:rec-put beam 'BAY bay))
    (setq beam (mk:rec-put beam 'ROW row))
    (setq lq (atof len))
    (setq beam (mk:rec-put beam 'LEN_Q lq))
    (setq beam (mk:rec-put beam 'LEN_IDX
                 (if (mk:rec-get beam 'VERT)
                   (+ (length scale) (mk:scale-index lq vscale))
                   (mk:scale-index lq scale))))
    (setq beam (mk:rec-put beam 'SIZE_MARK mark))
    (setq beam (mk:rec-put beam 'GROUP_KEY
                 (strcat len "_" art "_" suffix
                         "_" (if (mk:rec-get beam 'TJOINT) (mk:rec-get beam 'TJOINT) "")
                         "_" (if (mk:rec-get beam 'VERT) *mk:suffix-vert-beam* ""))))
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
    ;; есть зеркальная пара -> Ст{N}.1 и Ст{N}.2, иначе просто Ст{N}
    (if mirror
      (progn
        (setq plan (cons (list (strcat prefix " Ст" (itoa idx)
                                       (nth 0 *mk:mirror-suffixes*)) base) plan))
        (setq plan (cons (list (strcat prefix " Ст" (itoa idx)
                                       (nth 1 *mk:mirror-suffixes*)) mirror) plan)))
      (setq plan (cons (list (strcat prefix " Ст" (itoa idx)) base) plan)))
    (setq idx (1+ idx)))
  (reverse plan))


;; Ригели: цифра марки = индекс длины ригеля в шкале размеров «в свету»
;; (от минимальной к максимальной) — та же цифра, что у заполнения в MarkZ
;; ("номер = ширина"). Далее маркер размера профиля (м/б) и суффикс окружения.
(defun mk:plan-beams (beams prefix / plan idxs idx grp k sub mark base used n)
  (setq beams (mk:sort-xy beams) plan nil used nil)
  (setq idxs (vl-sort (mk:unique-keys beams 'LEN_IDX) '<))
  (if (member 0 idxs)
    (setq idxs (append (vl-remove 0 idxs) (list 0))))
  (foreach idx idxs
    (setq grp (mk:filter-by beams 'LEN_IDX idx))
    (foreach k (mk:unique-keys grp 'GROUP_KEY)
      (setq sub  (mk:filter-by grp 'GROUP_KEY k)
            mark (strcat prefix " Рг" (itoa idx)
                         (if (mk:rec-get (car sub) 'VERT) *mk:suffix-vert-beam* "")
                         (if (mk:rec-get (car sub) 'TJOINT)
                           (mk:rec-get (car sub) 'TJOINT) "")
                         (if (mk:rec-get (car sub) 'SIZE_MARK)
                           (mk:rec-get (car sub) 'SIZE_MARK) "")
                         (if (mk:rec-get (car sub) 'SUFFIX)
                           (mk:rec-get (car sub) 'SUFFIX) "")))
      ;; защита от совпадения марок разных групп
      (if (member mark used)
        (progn
          (setq base mark n 2)
          (while (member (strcat base "*" (itoa n)) used) (setq n (1+ n)))
          (setq mark (strcat base "*" (itoa n)))
          (prompt (strcat "\n  [WARN] Длина №" (itoa idx)
                          ": разные группы дают одну марку — выдана " mark))))
      (setq used (cons mark used))
      (setq plan (cons (list mark sub) plan))))
  (reverse plan))

;;;---------------------------------------------------------------------
;;; 18e. ОБЛАСТЬ ДЕЙСТВИЯ, UNDO, ЗАПИСЬ
;;;---------------------------------------------------------------------
(defun mk:ss-all-inserts ()
  (ssget "_X" (list (cons 0 "INSERT"))))

;; Область действия команды: своя выборка -> выборка последней МАРКАВГЕОМЕТРИЯ ->
;; (только если ничего нет) весь чертёж.
;; Нормализация ключевого слова режима каркаса
(defun mk:norm-mode (kw)
  (cond
    ((null kw) "Мультилинии")
    ((= kw "M") "Мультилинии")
    ((= kw "B") "Блоки")
    ((= kw "A") "Все-типы")
    (t kw)))

(defun mk:scope-ss (/ ss n)
  (if (and *mk:batch* *mk:last-ss*)
    (progn
      (prompt "\n  Область: выборка пакета МАРКАВ")
      (setq ss *mk:last-ss*))
    (progn
      (prompt "\nВыберите элементы витража (Enter — выборка последней МАРКАВГЕОМЕТРИЯ): ")
      (setq ss (if (= *mk:scope-mode* "Блоки")
                 (ssget (list (cons 0 "INSERT")))
                 (ssget)))))
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
  (if *mk:batch*
    (setq doc nil)                       ; в пакете UNDO ставит сама МАРКАВ
    (setq doc (mk:ax-get (vlax-get-acad-object) "ActiveDocument")))
  (if doc (vl-catch-all-apply 'vlax-invoke-method (list doc "StartUndoMark")))
  doc)

(defun mk:undo-end (doc)
  (if doc (vl-catch-all-apply 'vlax-invoke-method (list doc "EndUndoMark")))
  nil)

;; Запись марки по группе; возвращает (записано пропущено)
;;;---------------------------------------------------------------------
;;; Выноски марок и артикулов (для элементов без атрибута «Марка»)
;;; Слой «Обозначения», жёлтый, стиль «Основной стиль (надписи без наклона)»,
;;; высота 40, выравнивание вправо.
;;;   ригель — у правого конца, не доходя *mk:label-end-gap* мм, над профилем
;;;            на *mk:label-offset* мм;
;;;   стойка — у верхнего конца, не доходя *mk:label-end-gap* мм, сбоку
;;;            на *mk:label-offset* мм, текст повёрнут на *mk:label-rot-post*.
;;;---------------------------------------------------------------------
(setq *mk:labels* nil)
(setq *mk:style-warned* nil)

;; Имя текстового стиля, если он есть в чертеже
(defun mk:label-style-name ()
  (cond
    ((null (mk:strp *mk:label-style*)) nil)
    ((tblsearch "STYLE" *mk:label-style*) *mk:label-style*)
    (t
     (if (null *mk:style-warned*)
       (progn
         (setq *mk:style-warned* t)
         (prompt (strcat "\n  [WARN] Текстовый стиль «" *mk:label-style*
                         "» не найден — выноски текущим стилем."))))
     nil)))

;; Точка и поворот подписи: (точка поворот шаг-строки-по-X шаг-по-Y)
;; Окно над ригелем -> подпись зеркалится под ригель
(defun mk:beam-window-above? (el / top)
  (setq top (mk:rec-get el 'TOP_ELEM))
  (and top (= (cdr (assoc 'TYPE top)) "ОКНО")))

(defun mk:label-anchor (el / p len cross etype x y rot dx dy step just below)
  (setq p     (cdr (assoc 'INS_PT el))
        len   (if (numberp (cdr (assoc 'LENGTH el))) (cdr (assoc 'LENGTH el)) 0.0)
        cross (if (numberp (mk:rec-get el 'CROSS)) (mk:rec-get el 'CROSS) 0.0)
        etype (cdr (assoc 'TYPE el))
        step  (* *mk:label-line-gap* *mk:label-height*))
  (if (<= cross 1.0) (setq cross *mk:cross-fallback*))   ; отступ от грани, не от оси
  (if (null p)
    nil
    (progn
      (setq just 2)                                  ; по умолчанию — вправо
      (if (or (= etype "СТОЙКА") (mk:rec-get el 'VERT))
        (progn
          (setq x   (- (car p) (/ cross 2.0) *mk:label-offset*)
                rot *mk:label-rot-post*
                dx  (- step)
                dy  0.0)
          (if (< len *mk:label-short-len*)            ; короткий — по центру
            (setq y (+ (cadr p) (/ len 2.0)) just 1)
            (setq y (+ (cadr p) (- len *mk:label-end-gap*)))))
        (progn
          (setq below (mk:beam-window-above? el)      ; окно сверху -> текст снизу
                rot   0.0
                dx    0.0)
          (if below
            (setq y  (- (cadr p) (/ cross 2.0) *mk:label-offset* *mk:label-height*)
                  dy (- step))
            (setq y  (+ (cadr p) (/ cross 2.0) *mk:label-offset*)
                  dy step))
          (if (< len *mk:label-short-len*)            ; короткий — по центру
            (setq x (+ (car p) (/ len 2.0)) just 1)
            (setq x (+ (car p) (- len *mk:label-end-gap*))))))
      (list (list x y 0.0) rot dx dy just))))

;; Одна строка текста с выравниванием вправо
(defun mk:label-text (pt rot str just / dxf sty)
  (setq dxf (list '(0 . "TEXT")
                  (cons 8 *mk:layer-label*)
                  (cons 10 pt)
                  (cons 40 *mk:label-height*)
                  (cons 1 str)
                  (cons 50 (* pi (/ rot 180.0)))
                  (cons 62 *mk:label-color*)
                  (cons 72 just)                  ; 2 - вправо, 1 - по центру
                  '(73 . 0)
                  (cons 11 pt)))
  (if (setq sty (mk:label-style-name))
    (setq dxf (append dxf (list (cons 7 sty)))))
  (entmakex dxf))

;; Выноска: марка, а следом артикул (если он известен)
(defun mk:label-mark (el mark-str / anc pt rot dx dy just art e n)
  (setq anc (mk:label-anchor el)
        art (mk:rec-get el 'ARTICLE)
        n   0)
  (if (null anc)
    nil
    (progn
      (mk:ensure-layer *mk:layer-label* *mk:label-color*)
      (setq pt   (nth 0 anc) rot (nth 1 anc)
            dx   (nth 2 anc) dy  (nth 3 anc)
            just (if (nth 4 anc) (nth 4 anc) 2))
      (if (setq e (mk:label-text pt rot mark-str just))
        (progn (setq *mk:labels* (cons e *mk:labels*)) (setq n (1+ n))))
      (if (and (mk:strp art) (> (strlen art) 0))
        (if (setq e (mk:label-text (list (+ (car pt) dx) (+ (cadr pt) dy) 0.0)
                                   rot art just))
          (progn (setq *mk:labels* (cons e *mk:labels*)) (setq n (1+ n)))))
      (> n 0))))

;; Собрать все выноски чертежа в группу
(defun mk:group-labels (/ ss i lst)
  (setq ss (ssget "_X" (list (cons 8 *mk:layer-label*) (cons 0 "TEXT"))) lst nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq lst (cons (ssname ss i) lst))
        (setq i (1+ i)))
      (mk:make-group *mk:group-label* (reverse lst))))
  (if ss (sslength ss) 0))

(defun mk:write-marks (elements mark-str / count skip-count lab-count e)
  (setq count 0 skip-count 0 lab-count 0)
  (foreach el elements
    (setq e (mk:rec-get el 'ENAME))
    (if (and e (mk:has-attr? e *mk:attr-mark*))
      (progn
        (mk:set-attr e *mk:attr-mark* mark-str)
        (setq count (1+ count)))
      (progn
        (if (mk:label-mark el mark-str)
          (setq lab-count (1+ lab-count))
          (setq skip-count (1+ skip-count))))))
  (list count skip-count lab-count))

;; Сбор всех типов элементов из выборки
;;;---------------------------------------------------------------------
;;; Т-СОЕДИНЕНИЯ
;;; Т-соединение — вертикальный элемент упирается торцом в ригель внутри его
;;; пролёта (не на конце). Такому ригелю даётся суффикс:
;;;   .1 — если Т строго по центру длины ригеля (и у пары ригелей один артикул
;;;        либо артикула нет) или если ригель расположен снизу от стойки;
;;;   .2 — ригель сверху от стойки в несимметричном узле.
;;; Вертикальный элемент с двумя Т-соединениями считается ригелем (суффикс «т»),
;;; с одним — остаётся стойкой.
;;;---------------------------------------------------------------------
(defun mk:beam-hit (x y beams / out p bx0 bx1 by len)
  (setq out nil)
  (foreach b beams
    (if (null out)
      (progn
        (setq p   (cdr (assoc 'INS_PT b))
              len (if (numberp (cdr (assoc 'LENGTH b))) (cdr (assoc 'LENGTH b)) 0.0))
        (if p
          (progn
            (setq bx0 (car p) bx1 (+ (car p) len) by (cadr p))
            (if (and (<= (abs (- y by)) *mk:tol-tjoint*)
                     (> x (+ bx0 *mk:tol-tjoint*))
                     (< x (- bx1 *mk:tol-tjoint*)))
              (setq out b)))))))
  out)

;; Т-точка по центру длины ригеля?
(defun mk:t-centered? (beam x / p len)
  (setq p   (cdr (assoc 'INS_PT beam))
        len (if (numberp (cdr (assoc 'LENGTH beam))) (cdr (assoc 'LENGTH beam)) 0.0))
  (and p (> len 0.0)
       (<= (abs (- x (+ (car p) (/ len 2.0)))) *mk:tol-tcenter*)))

(defun mk:art-of (el)
  (if (and el (mk:strp (mk:rec-get el 'ARTICLE))) (strcase (mk:rec-get el 'ARTICLE)) ""))

;; Карта суффиксов: ключ — ENAME ригеля; «.1» имеет приоритет над «.2»
(defun mk:tj-put (map beam sfx / e hit)
  (setq e (mk:rec-get beam 'ENAME))
  (if (null e)
    map
    (progn
      (setq hit (assoc e map))
      (cond
        ((null hit) (cons (cons e sfx) map))
        ((= (cdr hit) *mk:suffix-tjoint-lo*) map)
        (t (subst (cons e sfx) hit map))))))

;; Разбор узлов: возвращает (стойки ригели)
(defun mk:split-tjoints (posts beams / map newposts newbeams p len vx vy0 vy1
                                       bb ba cb ca nT e hit)
  (setq map nil newposts nil newbeams nil)
  (foreach v posts
    (setq p   (cdr (assoc 'INS_PT v))
          len (if (numberp (cdr (assoc 'LENGTH v))) (cdr (assoc 'LENGTH v)) 0.0))
    (if (null p)
      (setq newposts (cons v newposts))
      (progn
        (setq vx  (car p) vy0 (cadr p) vy1 (+ (cadr p) len)
              bb  (mk:beam-hit vx vy0 beams)      ; ригель снизу
              ba  (mk:beam-hit vx vy1 beams)      ; ригель сверху
              cb  (and bb (mk:t-centered? bb vx))
              ca  (and ba (mk:t-centered? ba vx)))
        (if (and bb ba cb ca (= (mk:art-of bb) (mk:art-of ba)))
          (setq map (mk:tj-put (mk:tj-put map bb *mk:suffix-tjoint-lo*)
                               ba *mk:suffix-tjoint-lo*))
          (progn
            (if bb (setq map (mk:tj-put map bb *mk:suffix-tjoint-lo*)))
            (if ba (setq map (mk:tj-put map ba *mk:suffix-tjoint-hi*)))))
        (setq nT (+ (if bb 1 0) (if ba 1 0)))
        (if (= nT 2)
          ;; вертикальный элемент между двумя ригелями -> это ригель
          (setq newbeams (cons (mk:rec-put (mk:rec-put v 'TYPE "РИГЕЛЬ") 'VERT t)
                               newbeams))
          (setq newposts (cons v newposts))))))
  ;; проставить суффиксы Т-соединений ригелям
  (setq beams (mapcar
                '(lambda (b / e hit)
                   (setq e   (mk:rec-get b 'ENAME)
                         hit (if e (assoc e map) nil))
                   (if hit (mk:rec-put b 'TJOINT (cdr hit)) b))
                beams))
  (list (reverse newposts) (append beams (reverse newbeams))))

(defun mk:collect-scope (ss / posts beams fills windows doors res mlines)
  (cond
    ;; режим последнего сбора: «Все-типы» — каркас из любой геометрии + блоков
    ((= *mk:scope-mode* "Все-типы")
     (setq res   (mk:extract-posts-beams-all ss)
           posts (nth 0 res)
           beams (nth 1 res)))
    ;; «Мультилинии» — каркас из мультилиний выборки
    ((= *mk:scope-mode* "Мультилинии")
     (setq mlines (mk:find-mlines-in-ss ss))
     (if mlines
       (progn
         (setq res   (mk:extract-posts-beams-from-mlines mlines)
               posts (nth 0 res)
               beams (nth 1 res)))
       (setq posts nil beams nil)))
    (t
     (setq posts (mapcar 'mk:collect-post
                   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-post* "*"))))
     (setq beams (mapcar 'mk:collect-beam
                   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-beam* "*"))))))
  (setq posts (mk:sanitize-frame posts "Стойки"))
  (setq beams (mk:sanitize-frame beams "Ригели"))
  (setq res   (mk:split-tjoints posts beams)
        posts (nth 0 res)
        beams (nth 1 res))
  (setq fills   (mk:sanitize-panels
                  (mapcar 'mk:collect-fill (mk:find-blocks-in-ss ss (strcat "*" *mk:block-fill* "*")))
                  "Заполнения"))
  (setq windows (mk:sanitize-panels
                  (mapcar 'mk:collect-window (mk:find-blocks-in-ss ss (strcat "*" *mk:block-window* "*")))
                  "Окна"))
  (setq doors   (mk:sanitize-panels
                  (mapcar 'mk:collect-door (mk:find-blocks-in-ss ss (strcat "*" *mk:block-door* "*")))
                  "Двери"))
  (list (cons 'POSTS posts) (cons 'BEAMS beams) (cons 'FILLS fills)
        (cons 'WINDOWS windows) (cons 'DOORS doors)
        (cons 'PANELS (append fills windows doors))))

;;;---------------------------------------------------------------------
;;; 18f. КОМАНДЫ МАРКИРОВКИ
;;;---------------------------------------------------------------------
(defun c:МАРКАВСТ (/ ss data posts elements prefix plan res count skip-count
                     lab-count doc)
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
      (setq count 0 skip-count 0 lab-count 0)
      (setq doc (mk:undo-begin))
      (foreach item plan
        (setq res (mk:write-marks (cadr item) (car item)))
        (prompt (strcat "\n  " (car item) " — шт.: " (itoa (length (cadr item)))
                        "   [" (mk:rec-get (car (cadr item)) 'LEFT_TYPES)
                        " | " (mk:rec-get (car (cadr item)) 'RIGHT_TYPES) "]"))
        (setq count      (+ count (car res))
              skip-count (+ skip-count (cadr res))
              lab-count  (+ lab-count (caddr res))))
      (if (> lab-count 0)
        (progn
          (prompt (strcat "\n  Выносок (нет атрибута «" *mk:attr-mark* "»): "
                          (itoa lab-count)))
          (mk:group-labels)))
      (mk:undo-end doc)
      (prompt (strcat "\n[ГОТОВО] Заполнено: " (itoa count)
                      ", выносок: " (itoa lab-count)
                      ", пропущено: " (itoa skip-count)))))
  (princ))

(defun c:МАРКАВРГ (/ ss data beams posts panels doors bounds prefix plan
                     res count skip-count lab-count doc)
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
      (setq count 0 skip-count 0 lab-count 0)
      (setq doc (mk:undo-begin))
      (foreach item plan
        (setq res (mk:write-marks (cadr item) (car item)))
        (prompt (strcat "\n  " (car item) " — шт.: " (itoa (length (cadr item)))))
        (setq count      (+ count (car res))
              skip-count (+ skip-count (cadr res))
              lab-count  (+ lab-count (caddr res))))
      (if (> lab-count 0)
        (progn
          (prompt (strcat "\n  Выносок (нет атрибута «" *mk:attr-mark* "»): "
                          (itoa lab-count)))
          (mk:group-labels)))
      (mk:undo-end doc)
      (prompt (strcat "\n[ГОТОВО] Заполнено: " (itoa count)
                      ", выносок: " (itoa lab-count)
                      ", пропущено: " (itoa skip-count)))))
  (princ))

;;;=====================================================================
;;; 19. ГЛАВНАЯ КОМАНДА
;;;=====================================================================
(defun c:МАРКАВГЕОМЕТРИЯ (/ ss vitrage vitrage-pt posts beams fills windows doors
                      bounds topo-result all-panels dup-count overlap-count
                      out-count post-result protruding-count mlines
                      mline-result use-mlines mode-kw post-enames beam-enames)
  (prompt "\n[МАРКАВГЕОМЕТРИЯ] Сбор данных и построение 2D модели витража...")
  (setq *mk:dyn-cache* nil)
  (if (and *mk:batch* *mk:batch-ss*)
    (progn
      (setq ss *mk:batch-ss*)
      (prompt "\n  Область: выборка пакета МАРКАВ"))
    (progn
      (prompt "\nВыберите элементы витража (рамкой): ")
      (setq ss (ssget))))
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
  (if (and *mk:batch* *mk:batch-mode*)
    (setq mode-kw *mk:batch-mode*)
    (progn
      (initget "Блоки Мультилинии Все-типы B M A")
      (setq mode-kw (getkword
        "\nРежим сбора каркаса [Блоки/Мультилинии/Все-типы] <Мультилинии>: "))))
  (setq mode-kw (mk:norm-mode mode-kw))
  (setq *mk:scope-mode* mode-kw)
  (setq use-mlines (= mode-kw "Мультилинии"))
  (cond
    ((= mode-kw "Все-типы")
     (prompt "\n  Режим: ВСЕ ТИПЫ (мультилинии + линии/полилинии + блоки)")
     (setq mline-result (mk:extract-posts-beams-all ss))
     (setq posts (nth 0 mline-result))
     (setq beams (nth 1 mline-result)))
    (use-mlines
     (prompt "\n  Режим: МУЛЬТИЛИНИИ")
     (if (> (length mlines) 0)
       (progn
         (setq mline-result (mk:extract-posts-beams-from-mlines mlines))
         (setq posts (nth 0 mline-result))
         (setq beams (nth 1 mline-result)))
       (prompt "\n  [WARN] Мультилинии не найдены в выборке.")))
    (t
     (prompt "\n  Режим: БЛОКИ")
     (setq post-enames (mk:find-blocks-in-ss ss (strcat "*" *mk:block-post* "*")))
     (setq beam-enames (mk:find-blocks-in-ss ss (strcat "*" *mk:block-beam* "*")))
     (setq posts (mapcar 'mk:collect-post post-enames))
     (setq beams (mapcar 'mk:collect-beam beam-enames))))
  (setq posts (mk:sanitize-frame posts "Стойки"))
  (setq beams (mk:sanitize-frame beams "Ригели"))
  (setq res   (mk:split-tjoints posts beams)
        posts (nth 0 res)
        beams (nth 1 res))
  (setq fills   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-fill* "*")))
  (setq windows (mk:find-blocks-in-ss ss (strcat "*" *mk:block-window* "*")))
  (setq doors   (mk:find-blocks-in-ss ss (strcat "*" *mk:block-door* "*")))
  (setq fills   (mk:sanitize-panels (mapcar 'mk:collect-fill fills)     "Заполнения"))
  (setq windows (mk:sanitize-panels (mapcar 'mk:collect-window windows) "Окна"))
  (setq doors   (mk:sanitize-panels (mapcar 'mk:collect-door doors)     "Двери"))
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
  (prompt (strcat "\n  Режим: " (strcase mode-kw)))
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
;;; 20a. ВЕДОМОСТЬ ПРОФИЛЕЙ — МАРКАВТАБЛ
;;;   Таблица AutoCAD + XLS (SpreadsheetML 2003).
;;;   Шапка: № | Артикул | Марка | Длина, мм | Кол-во, шт. | Всего, м.п.
;;;   Сначала стойки, потом ригели; сортировка по артикулу, затем по марке
;;;   и длине. Доноры: mtab:* (MARKZ.lsp, MARKTABLE).
;;;=====================================================================
(setq *mk:tab-title* "Ведомость профилей")

;; Число с двумя знаками и запятой-разделителем: 12.5 -> "12,5"
(defun mk:tab-num (x / s i c out)
  (setq s (rtos (float x) 2 2) out "" i 1)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (setq out (strcat out (if (= c ".") "," c)))
    (setq i (1+ i)))
  out)

;; Число для SpreadsheetML: разделитель — точка
(defun mk:tab-xnum (x / s i c out)
  (setq s (rtos (float x) 2 2) out "" i 1)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (setq out (strcat out (if (= c ",") "." c)))
    (setq i (1+ i)))
  out)

(defun mk:tab-int (x) (itoa (fix (+ (float x) 0.5))))

;; Длина профиля ригеля = размер в свету + припуск (12.5 мм на сторону)
(defun mk:beam-cut-len (len)
  (if (numberp len) (+ (float len) *mk:beam-allowance*) 0.0))

(defun mk:tab-mp (len cnt) (/ (* (float len) cnt) 1000.0))

;; Записи: (ранг раздел артикул марка длина)
(defun mk:tab-rows (posts beams / out)
  (setq out nil)
  (foreach el posts
    (setq out (cons (list 1 "Стойки"
                          (if (mk:rec-get el 'ARTICLE) (mk:rec-get el 'ARTICLE) "—")
                          (mk:read-mark el)
                          (if (mk:rec-get el 'LENGTH) (mk:rec-get el 'LENGTH) 0.0))
                    out)))
  (foreach el beams
    (setq out (cons (list 2 "Ригели"
                          (if (mk:rec-get el 'ARTICLE) (mk:rec-get el 'ARTICLE) "—")
                          (mk:read-mark el)
                          (mk:beam-cut-len (mk:rec-get el 'LENGTH)))
                    out)))
  (reverse out))

;; Карта «ename -> расчётная марка» для элементов без атрибута «Марка»
(setq *mk:mark-map* nil)

(defun mk:mark-map-put (plan)
  (foreach item plan
    (foreach el (cadr item)
      (if (mk:rec-get el 'ENAME)
        (setq *mk:mark-map*
          (cons (cons (mk:rec-get el 'ENAME) (car item)) *mk:mark-map*)))))
  *mk:mark-map*)

(defun mk:read-mark (el / e v hit)
  (setq e (cdr (assoc 'ENAME el))
        v (if (and e (mk:has-attr? e *mk:attr-mark*))
            (mk:get-attr e *mk:attr-mark*) nil))
  (cond
    ((and v (> (strlen v) 0)) v)
    ((setq hit (assoc e *mk:mark-map*)) (cdr hit))
    (t "—")))

;; Натуральный ключ сортировки: числа дополняются нулями слева,
;; поэтому Ст2 < Ст10, Рг9 < Рг10.
(defun mk:nat-key (s / up n i c out num)
  (if (not (mk:strp s))
    ""
    (progn
      (setq up (strcase s) n (strlen up) i 1 out "" num "")
      (while (<= i n)
        (setq c (substr up i 1))
        (if (member c '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
          (setq num (strcat num c))
          (progn
            (if (> (strlen num) 0)
              (progn
                (while (< (strlen num) 6) (setq num (strcat "0" num)))
                (setq out (strcat out num) num "")))
            (setq out (strcat out c))))
        (setq i (1+ i)))
      (if (> (strlen num) 0)
        (progn
          (while (< (strlen num) 6) (setq num (strcat "0" num)))
          (setq out (strcat out num))))
      out)))

(defun mk:tab-less (a b / aa ab ma mb)
  (cond
    ((< (nth 0 a) (nth 0 b)) t)
    ((> (nth 0 a) (nth 0 b)) nil)
    (t
     (setq aa (mk:nat-key (nth 2 a)) ab (mk:nat-key (nth 2 b)))
     (cond
       ((< aa ab) t)
       ((> aa ab) nil)
       (t
        (setq ma (mk:nat-key (nth 3 a)) mb (mk:nat-key (nth 3 b)))
        (cond
          ((< ma mb) t)
          ((> ma mb) nil)
          ((< (nth 4 a) (nth 4 b)) t)
          (t nil)))))))

;; Агрегация -> (ранг раздел артикул марка длина кол-во)
(defun mk:tab-aggregate (rows / acc key found out)
  (setq acc nil)
  (foreach r rows
    (setq key (strcat (itoa (nth 0 r)) "|" (strcase (nth 2 r)) "|"
                      (strcase (nth 3 r)) "|" (mk:tab-int (nth 4 r))))
    (setq found (assoc key acc))
    (if found
      (setq acc (subst (list key (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r)
                             (nth 4 r) (1+ (nth 6 found)))
                       found acc))
      (setq acc (cons (list key (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r)
                            (nth 4 r) 1)
                      acc))))
  (setq out (mapcar '(lambda (r) (cdr r)) acc))
  (vl-sort out 'mk:tab-less))

;;;--- Таблица AutoCAD ---------------------------------------------------
(defun mk:tab-merge (tbl r / res)
  (setq res (vl-catch-all-apply 'vlax-invoke-method
              (list tbl "MergeCells" r r 1 3)))
  (if (vl-catch-all-error-p res)
    (prompt "\n  [WARN] Объединение ячеек не выполнено."))
  res)

(defun mk:tab-set (tbl r c val)
  (vl-catch-all-apply 'vlax-invoke-method (list tbl "SetText" r c val)))

(defun mk:tab-align (tbl r c a)
  (vl-catch-all-apply 'vlax-invoke-method (list tbl "SetCellAlignment" r c a)))

(defun mk:tab-create (data / pt doc space tbl nRows nCols row n-row i hdr
                        cur cnt-sub mp-sub n-sec rec sect art mark len cnt mp
                        total-cnt total-mp old-echo)
  (if (null data)
    (progn (prompt "\n  [INFO] Нет данных для таблицы.") nil)
    (progn
      (setq pt (getpoint "\nУкажите точку вставки таблицы: "))
      (if (null pt)
        (progn (prompt "\n  [INFO] Таблица пропущена.") nil)
        (progn
          (setq n-sec 0 cur nil)
          (foreach rec data
            (if (not (and cur (= (nth 1 rec) cur)))
              (progn (setq n-sec (1+ n-sec)) (setq cur (nth 1 rec)))))
          (setq doc      (vla-get-ActiveDocument (vlax-get-acad-object))
                space    (vla-get-ModelSpace doc)
                pt       (trans pt 1 0)
                nCols    6
                nRows    (+ 3 (length data) n-sec)
                old-echo (getvar "CMDECHO"))
          (vl-catch-all-apply 'setvar (list "CMDECHO" 0))
          (setq tbl (vl-catch-all-apply 'vla-AddTable
                      (list space (vlax-3d-point pt) nRows nCols 10.0 35.0)))
          (if (vl-catch-all-error-p tbl)
            (progn
              (prompt (strcat "\n  [ERROR] AddTable: "
                              (vl-catch-all-error-message tbl)))
              (vl-catch-all-apply 'setvar (list "CMDECHO" old-echo))
              nil)
            (progn
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 0 14.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 1 42.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 2 42.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 3 32.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 4 32.0))
              (vl-catch-all-apply 'vla-SetColumnWidth (list tbl 5 32.0))
              (mk:tab-set tbl 0 0 *mk:tab-title*)
              (vl-catch-all-apply 'vla-SetRowHeight (list tbl 0 8.0))
              (setq i 0)
              (foreach hdr '("№" "Артикул" "Марка" "Длина, мм"
                             "Кол-во, шт." "Всего, м.п.")
                (mk:tab-set tbl 1 i hdr)
                (mk:tab-align tbl 1 i 5)
                (setq i (1+ i)))
              (vl-catch-all-apply 'vla-SetRowHeight (list tbl 1 8.0))
              (setq row 2 n-row 1 cur nil cnt-sub 0 mp-sub 0.0
                    total-cnt 0 total-mp 0.0)
              (foreach rec data
                (setq sect (nth 1 rec)
                      art  (nth 2 rec)
                      mark (nth 3 rec)
                      len  (nth 4 rec)
                      cnt  (nth 5 rec)
                      mp   (mk:tab-mp len cnt))
                (if (and cur (/= sect cur))
                  (progn
                    (mk:tab-set tbl row 0 "")
                    (mk:tab-set tbl row 1 (strcat "   " cur))
                    (mk:tab-merge tbl row)
                    (mk:tab-align tbl row 1 4)
                    (mk:tab-set tbl row 4 (itoa cnt-sub))
                    (mk:tab-set tbl row 5 (mk:tab-num mp-sub))
                    (mk:tab-align tbl row 4 5)
                    (mk:tab-align tbl row 5 5)
                    (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
                    (setq row (1+ row) cnt-sub 0 mp-sub 0.0)))
                (setq cur sect
                      cnt-sub (+ cnt-sub cnt)
                      mp-sub  (+ mp-sub mp)
                      total-cnt (+ total-cnt cnt)
                      total-mp  (+ total-mp mp))
                (mk:tab-set tbl row 0 (itoa n-row))
                (mk:tab-set tbl row 1 art)
                (mk:tab-set tbl row 2 mark)
                (mk:tab-set tbl row 3 (mk:tab-int len))
                (mk:tab-set tbl row 4 (itoa cnt))
                (mk:tab-set tbl row 5 (mk:tab-num mp))
                (mk:tab-align tbl row 0 5)
                (mk:tab-align tbl row 1 4)
                (mk:tab-align tbl row 2 4)
                (mk:tab-align tbl row 3 5)
                (mk:tab-align tbl row 4 5)
                (mk:tab-align tbl row 5 5)
                (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
                (setq row (1+ row) n-row (1+ n-row)))
              (if cur
                (progn
                  (mk:tab-set tbl row 0 "")
                  (mk:tab-set tbl row 1 (strcat "   " cur))
                  (mk:tab-merge tbl row)
                  (mk:tab-align tbl row 1 4)
                  (mk:tab-set tbl row 4 (itoa cnt-sub))
                  (mk:tab-set tbl row 5 (mk:tab-num mp-sub))
                  (mk:tab-align tbl row 4 5)
                  (mk:tab-align tbl row 5 5)
                  (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
                  (setq row (1+ row))))
              (mk:tab-set tbl row 0 "")
              (mk:tab-set tbl row 1 "   {\\LИтого:}")
              (mk:tab-merge tbl row)
              (mk:tab-align tbl row 1 4)
              (mk:tab-set tbl row 4 (itoa total-cnt))
              (mk:tab-set tbl row 5 (mk:tab-num total-mp))
              (mk:tab-align tbl row 4 5)
              (mk:tab-align tbl row 5 5)
              (vl-catch-all-apply 'vla-SetRowHeight (list tbl row 8.0))
              (vl-catch-all-apply 'vla-Update (list tbl))
              (vl-catch-all-apply 'setvar (list "CMDECHO" old-echo))
              (prompt (strcat "\n  [OK] Таблица создана, строк данных: "
                              (itoa (length data))))
              t)))))))

;;;--- XLS (SpreadsheetML 2003) -----------------------------------------
(defun mk:xml-esc (str / out ch)
  (setq out "")
  (if (null str) (setq str ""))
  (while (/= str "")
    (setq ch (substr str 1 1))
    (cond
      ((= ch "&")  (setq out (strcat out "&amp;")))
      ((= ch "<")  (setq out (strcat out "&lt;")))
      ((= ch ">")  (setq out (strcat out "&gt;")))
      ((= ch "\"") (setq out (strcat out "&quot;")))
      ((= ch "'")  (setq out (strcat out "&apos;")))
      (t (setq out (strcat out ch))))
    (setq str (substr str 2)))
  out)

(defun mk:xcell (f style formula dtype val)
  (if formula
    (write-line
      (strcat "    <Cell ss:StyleID=\"" style
              "\" ss:Formula=\"" (mk:xml-esc formula) "\">"
              "<Data ss:Type=\"" dtype "\">" (mk:xml-esc val)
              "</Data></Cell>") f)
    (write-line
      (strcat "    <Cell ss:StyleID=\"" style "\">"
              "<Data ss:Type=\"" dtype "\">" (mk:xml-esc val)
              "</Data></Cell>") f)))

(defun mk:xcell-m (f style across dtype val)
  (write-line
    (strcat "    <Cell ss:StyleID=\"" style
            "\" ss:MergeAcross=\"" (itoa across) "\">"
            "<Data ss:Type=\"" dtype "\">" (mk:xml-esc val)
            "</Data></Cell>") f))

;; SUM по строкам r0..r1 в текущей колонке (формула пишется в строку r1+1)
(defun mk:xsum (r0 r1 / n0)
  (setq n0 (1- (- r0 r1)))
  (if (= n0 -1)
    "=R[-1]C"
    (strcat "=SUM(R[" (itoa n0) "]C:R[-1]C)")))

(defun mk:xsum-abs (rows col / out r)
  (if (null rows)
    "=0"
    (progn
      (setq out "")
      (foreach r rows
        (setq out (if (= out "")
                    (strcat "R" (itoa r) "C" (itoa col))
                    (strcat out ",R" (itoa r) "C" (itoa col)))))
      (strcat "=SUM(" out ")"))))

(defun mk:xstyle (f id bold fill align / )
  (write-line (strcat "  <Style ss:ID=\"" id "\">") f)
  (if bold  (write-line "   <Font ss:Bold=\"1\"/>" f))
  (if fill  (write-line "   <Interior ss:Color=\"#D9D9D9\" ss:Pattern=\"Solid\"/>" f))
  (if align (write-line (strcat "   <Alignment ss:Horizontal=\"" align "\"/>") f))
  (write-line "   <Borders>" f)
  (foreach pos '("Bottom" "Left" "Right" "Top")
    (write-line (strcat "    <Border ss:Position=\"" pos
                        "\" ss:LineStyle=\"Continuous\" ss:Weight=\"1\" ss:Color=\"#000000\"/>") f))
  (write-line "   </Borders>" f)
  (write-line "  </Style>" f))

(defun mk:tab-xls (data file / f rec sect art mark len cnt mp cur
                     xl-row grp-start sub-rows n-no cnt-sub mp-sub
                     total-cnt total-mp hdr w0)
  (setq f (open file "w"))
  (if (null f)
    (progn (prompt (strcat "\n  [ERROR] Нет доступа: " file)) nil)
    (progn
      (write-line "<?xml version=\"1.0\" encoding=\"windows-1251\"?>" f)
      (write-line "<?mso-application progid=\"Excel.Sheet\"?>" f)
      (write-line "<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:o=\"urn:schemas-microsoft-com:office:office\"" f)
      (write-line " xmlns:x=\"urn:schemas-microsoft-com:office:excel\"" f)
      (write-line " xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\"" f)
      (write-line " xmlns:html=\"http://www.w3.org/TR/REC-html40\">" f)
      (write-line " <Styles>" f)
      (write-line "  <Style ss:ID=\"Default\" ss:Name=\"Normal\">" f)
      (write-line "   <Font ss:FontName=\"Calibri\" ss:Size=\"11\"/>" f)
      (write-line "  </Style>" f)
      (mk:xstyle f "D"     nil nil nil)
      (mk:xstyle f "H"     t   t   "Center")
      (mk:xstyle f "TITLE" t   t   "Center")
      (mk:xstyle f "S"     t   t   "Left")
      (mk:xstyle f "SN"    t   t   "Right")
      (write-line " </Styles>" f)
      (write-line " <Worksheet ss:Name=\"Профили\">" f)
      (write-line "  <Table>" f)
      (foreach w0 '("30" "110" "110" "80" "80" "80")
        (write-line (strcat "   <Column ss:Width=\"" w0 "\"/>") f))
      (write-line (strcat "   <Row><Cell ss:StyleID=\"TITLE\" ss:MergeAcross=\"5\">"
                          "<Data ss:Type=\"String\">" (mk:xml-esc *mk:tab-title*)
                          "</Data></Cell></Row>") f)
      (write-line "   <Row>" f)
      (foreach hdr '("№" "Артикул" "Марка" "Длина, мм" "Кол-во, шт." "Всего, м.п.")
        (mk:xcell f "H" nil "String" hdr))
      (write-line "   </Row>" f)
      (setq xl-row 3 n-no 0 cur nil cnt-sub 0 mp-sub 0.0
            total-cnt 0 total-mp 0.0 grp-start 3 sub-rows nil)
      (foreach rec data
        (setq sect (nth 1 rec)
              art  (nth 2 rec)
              mark (nth 3 rec)
              len  (nth 4 rec)
              cnt  (nth 5 rec)
              mp   (mk:tab-mp len cnt))
        (if (and cur (/= sect cur))
          (progn
            (write-line "   <Row>" f)
            (mk:xcell f "S" nil "String" "")
            (mk:xcell-m f "S" 2 "String" (strcat "   " cur))
            (mk:xcell f "SN" (mk:xsum grp-start (1- xl-row)) "Number" (itoa cnt-sub))
            (mk:xcell f "SN" (mk:xsum grp-start (1- xl-row)) "Number" (mk:tab-xnum mp-sub))
            (write-line "   </Row>" f)
            (setq sub-rows (cons xl-row sub-rows)
                  cnt-sub 0 mp-sub 0.0
                  xl-row (1+ xl-row)
                  grp-start xl-row)))
        (setq cur sect
              cnt-sub (+ cnt-sub cnt)
              mp-sub  (+ mp-sub mp)
              total-cnt (+ total-cnt cnt)
              total-mp  (+ total-mp mp)
              n-no (1+ n-no))
        (write-line "   <Row>" f)
        (mk:xcell f "D" nil "Number" (itoa n-no))
        (mk:xcell f "D" nil "String" art)
        (mk:xcell f "D" nil "String" mark)
        (mk:xcell f "D" nil "Number" (mk:tab-int len))
        (mk:xcell f "D" nil "Number" (itoa cnt))
        (mk:xcell f "D" "=ROUND(RC[-2]*RC[-1]/1000,2)" "Number" (mk:tab-xnum mp))
        (write-line "   </Row>" f)
        (setq xl-row (1+ xl-row)))
      (if cur
        (progn
          (write-line "   <Row>" f)
          (mk:xcell f "S" nil "String" "")
          (mk:xcell-m f "S" 2 "String" (strcat "   " cur))
          (mk:xcell f "SN" (mk:xsum grp-start (1- xl-row)) "Number" (itoa cnt-sub))
          (mk:xcell f "SN" (mk:xsum grp-start (1- xl-row)) "Number" (mk:tab-xnum mp-sub))
          (write-line "   </Row>" f)
          (setq sub-rows (cons xl-row sub-rows)
                xl-row (1+ xl-row))))
      (write-line "   <Row>" f)
      (mk:xcell f "S" nil "String" "")
      (mk:xcell-m f "S" 2 "String" "   Итого:")
      (mk:xcell f "SN" (mk:xsum-abs sub-rows 5) "Number" (itoa total-cnt))
      (mk:xcell f "SN" (mk:xsum-abs sub-rows 6) "Number" (mk:tab-xnum total-mp))
      (write-line "   </Row>" f)
      (write-line "  </Table>" f)
      (write-line " </Worksheet>" f)
      (write-line "</Workbook>" f)
      (close f)
      (prompt (strcat "\n  [OK] XLS: " file))
      t)))

;;;--- Команда -----------------------------------------------------------
(defun c:МАРКАВТАБЛ (/ ss data posts beams rows agg do-tbl do-xls file doc
                       prefix bounds)
  (prompt "\n[МАРКАВТАБЛ] Ведомость профилей (стойки + ригели)...")
  (setq *mk:dyn-cache* nil)
  (setq ss   (mk:scope-ss))
  (setq data (mk:collect-scope ss))
  (setq posts (cdr (assoc 'POSTS data))
        beams (cdr (assoc 'BEAMS data)))
  (if (and (null posts) (null beams))
    (prompt "\n  [INFO] Стойки и ригели не найдены.")
    (progn
      (prompt (strcat "\n  Стоек: " (itoa (length posts))
                      ", ригелей: " (itoa (length beams))))
      ;; расчётные марки — для элементов, у которых нет атрибута «Марка»
      (setq *mk:mark-map* nil)
      (setq prefix (mk:get-vitrage-prefix))
      (setq bounds (mk:find-vitrage-bounds posts beams))
      (if posts
        (mk:mark-map-put
          (mk:plan-posts
            (mk:annotate-posts posts (append (cdr (assoc 'PANELS data)) beams))
            prefix)))
      (if beams
        (mk:mark-map-put
          (mk:plan-beams
            (mk:annotate-beams beams (cdr (assoc 'PANELS data))
                               (cdr (assoc 'DOORS data)) bounds posts)
            prefix)))
      (setq rows (mk:tab-rows posts beams)
            agg  (mk:tab-aggregate rows))
      (prompt (strcat "\n  Уникальных позиций: " (itoa (length agg))))
      (initget "Да Нет")
      (setq do-tbl (getkword "\nТаблица в чертеже? [Да/Нет] <Да>: "))
      (setq do-tbl (if (= do-tbl "Нет") nil t))
      (initget "Да Нет")
      (setq do-xls (getkword "\nЭкспорт в XLS? [Да/Нет] <Да>: "))
      (setq do-xls (if (= do-xls "Нет") nil t))
      (setq doc (mk:undo-begin))
      (if do-xls
        (progn
          (setq file (strcat (getvar "dwgprefix")
                             (vl-filename-base (getvar "dwgname"))
                             " Профили Марки.xls"))
          (mk:tab-xls agg file)))
      (if do-tbl (mk:tab-create agg))
      (mk:undo-end doc)
      (prompt "\n[ГОТОВО] Ведомость сформирована.")))
  (princ))

;;;=====================================================================
;;; 20b. ПАКЕТНЫЙ ПРОГОН — МАРКАВ
;;;   Одна выборка -> сбор и сетка -> марки стоек -> марки ригелей ->
;;;   удаление сетки -> ведомость. Один UNDO на весь пакет.
;;;=====================================================================
;; Удалить группу по имени (если существует)
(defun mk:group-delete (name / doc groups g)
  (setq doc    (mk:ax-get (vlax-get-acad-object) "ActiveDocument")
        groups (if doc (mk:ax-get doc "Groups") nil))
  (if groups
    (progn
      (setq g (vl-catch-all-apply 'vlax-invoke-method (list groups "Item" name)))
      (if (and g (not (vl-catch-all-error-p g)))
        (vl-catch-all-apply 'vlax-invoke-method (list g "Delete")))))
  nil)

;; Удалить всю отрисовку модели: объекты слоя + одноимённую группу
(defun mk:erase-model (layer group / ss i n)
  (mk:group-delete group)
  (setq ss (ssget "_X" (list (cons 8 layer))))
  (setq n 0)
  (if ss
    (progn
      (setq i (sslength ss))
      (repeat i
        (setq i (1- i))
        (if (entdel (ssname ss i)) (setq n (1+ n))))))
  (prompt (strcat "\n  [OK] Сетка удалена, объектов: " (itoa n)))
  n)

(defun c:МАРКАВ (/ ss keep doc n-posts n-beams)
  (prompt (strcat "\n[МАРКАВ] Пакетный прогон, Ред. " *mk:ver* "."))
  (prompt "\n  Этапы: сбор и сетка -> марки стоек -> марки ригелей -> ведомость.")
  (setq *mk:batch* nil *mk:batch-ss* nil *mk:batch-mode* nil)
  (setq *mk:dyn-cache* nil)
  (prompt "\nВыберите элементы витража (рамкой): ")
  (setq ss (ssget))
  (if (null ss)
    (prompt "\n[INFO] Ничего не выбрано — пакет прерван.")
    (progn
      (prompt (strcat "\n  Выбрано объектов: " (itoa (sslength ss))))
      ;; режим каркаса спрашиваем один раз на весь пакет
      (initget "Блоки Мультилинии Все-типы B M A")
      (setq *mk:batch-mode*
        (getkword "\nРежим сбора каркаса [Блоки/Мультилинии/Все-типы] <Блоки>: "))
      (if (null *mk:batch-mode*) (setq *mk:batch-mode* "Блоки"))
      (setq *mk:batch-mode* (mk:norm-mode *mk:batch-mode*))
      ;; судьба сетки
      (initget "Да Нет")
      (setq keep (getkword "\nОставить сетку витража в чертеже? [Да/Нет] <Нет>: "))
      (setq *mk:batch-keep-grid* (= keep "Да"))
      ;; один UNDO на весь пакет
      (setq doc (mk:ax-get (vlax-get-acad-object) "ActiveDocument"))
      (if doc (vl-catch-all-apply 'vlax-invoke-method (list doc "StartUndoMark")))
      (setq *mk:batch* t *mk:batch-ss* ss *mk:last-ss* ss)
      ;; --- 1/4 сбор, проверки, сетка, дамп
      (prompt "\n\n===== [ЭТАП 1/4] МАРКАВГЕОМЕТРИЯ =====")
      (c:МАРКАВГЕОМЕТРИЯ)
      ;; --- 2/4 стойки
      (prompt "\n\n===== [ЭТАП 2/4] МАРКАВСТ =====")
      (c:МАРКАВСТ)
      ;; --- 3/4 ригели
      (prompt "\n\n===== [ЭТАП 3/4] МАРКАВРГ =====")
      (c:МАРКАВРГ)
      ;; --- сетка больше не нужна: марки записаны в атрибуты блоков,
      ;;     а ведомость дальше просит указать точку вставки таблицы —
      ;;     чертёж к этому моменту должен быть чистым.
      (if *mk:batch-keep-grid*
        (prompt (strcat "\n\n  [INFO] Сетка оставлена на слое «" *mk:layer-model*
                        "» (группа " *mk:group-model* ")."))
        (progn
          (prompt "\n\n  Удаление сетки витража перед вставкой таблицы...")
          (mk:erase-model *mk:layer-model* *mk:group-model*)
          (mk:erase-model *mk:layer-test*  *mk:group-test*)))
      ;; --- 4/4 ведомость
      (prompt "\n\n===== [ЭТАП 4/4] МАРКАВТАБЛ =====")
      (c:МАРКАВТАБЛ)
      (if (> (mk:group-labels) 0)
        (prompt (strcat "\n  [INFO] Выноски марок собраны в группу "
                        *mk:group-label* ".")))
      (setq *mk:batch* nil *mk:batch-ss* nil *mk:batch-mode* nil)
      (if doc (vl-catch-all-apply 'vlax-invoke-method (list doc "EndUndoMark")))
      (prompt "\n\n[ГОТОВО] Пакет МАРКАВ завершён. Откат всего пакета — один U.")))
  (setq *mk:batch* nil)
  (princ))

;;;=====================================================================
;;; 20b. ВНЕШНИЕ БАЗЫ ПРОФИЛЬНЫХ СИСТЕМ
;;;=====================================================================
;; Подхват файла MarkZV-bases.lsp (рядом с модулем или в путях поиска AutoCAD)
(defun mk:load-bases (/ f res)
  (setq f (findfile *mk:bases-file*))
  (if f
    (progn
      (setq res (vl-catch-all-apply 'load (list f)))
      (if (vl-catch-all-error-p res)
        (prompt (strcat "\n  [WARN] Файл баз не загружен: " f))
        (prompt (strcat "\n  [OK] Базы профилей: " f)))))
  f)

(defun c:МАРКАВБАЗЫ (/ n)
  (prompt "\n[МАРКАВБАЗЫ] Зарегистрированные базы профилей:")
  (if (null *mk:size-bases*)
    (prompt "\n  (пусто)")
    (foreach b *mk:size-bases*
      (prompt (strcat "\n  " (car b) " — записей: " (itoa (length (cdr b)))))))
  (prompt (strcat "\n  Префиксы артикулов: "
                  (mk:join *mk:article-prefixes* ", ")))
  (prompt (strcat "\n  Файл внешних баз: " *mk:bases-file*
                  (if (findfile *mk:bases-file*) " (найден)" " (не найден)")))
  (princ))

;;;=====================================================================
;;; 21. ИНИЦИАЛИЗАЦИЯ С АВТОПРОВЕРКОЙ
;;;=====================================================================
(prompt (strcat "\n[MarkZV] Ред. " *mk:ver* " загружена."))
(prompt "\n  Команды:")
(prompt "\n    МАРКАВ          - Пакет: сбор -> марки стоек -> марки ригелей -> ведомость")
(prompt "\n    МАРКАВГЕОМЕТРИЯ - Сбор данных и построение 2D модели")
(prompt "\n    МАРКАВМОДЕЛЬ    - Тестовая отрисовка модели")
(prompt "\n    МАРКАВСТ        - Марки стоек")
(prompt "\n    МАРКАВРГ        - Марки ригелей")
(prompt "\n    МАРКАВТАБЛ      - Ведомость профилей: таблица в чертеже + XLS")
(prompt "\n    МАРКАВДИАГНОЗ   - Диагностика блока (свойства, атрибуты, габарит)")
(prompt "\n    МАРКАВБАЗЫ      - Список баз профильных систем")
(prompt "\n    МАРКАВСКОБКИ    - Проверка баланса скобок файла")
(mk:load-bases)
(princ)
