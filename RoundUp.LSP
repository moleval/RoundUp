;;; ============================================================
;;; RoundUp.LSP
;;; Округление значений линейных параметров динамических блоков и
;;; обнуление координаты Z у объектов модели и листов.
;;; Зависимости: Visual LISP ActiveX.
;;;
;;; РЕДАКЦИЯ 6: модуль ZeroZ.LSP объединён в этот файл. Добавлены
;;;   команды обнуления координаты Z: ОБНУЛИТЬЗЕТ / ZEROZ и
;;;   ОБНУЛИТЬЗЕТТЕСТ / ZEROZTEST, диагностика ZZINFO. Модуль стал
;;;   единственным файлом проекта; логика ред. 5 не изменялась.
;;;
;;; РЕДАКЦИЯ 5:
;;;   - добавлена команда КРАТНОБЛОК / KRATNOBLOCK;
;;;   - КРАТНОБЛОК строго округляет вверх до следующего кратного:
;;;     уже кратные значения увеличиваются на шаг;
;;;   - логика ОКРУГЛИТЬБЛОК не изменялась.
;;;
;;; ВАЖНО: модуль сгенерирован ИИ, требуется автоматическая и ручная проверка.
;;; ============================================================

(setq *rb-revision* 6)

(setq *rb-lower-cyr* "абвгдеёжзийклмнопрстуфхцчшщъыьэюя")
(setq *rb-upper-cyr* "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ")

;; Шаг по умолчанию.
(setq *rb-default-step* 1.0)

;; Показывать подробную таблицу по параметрам.
(setq *rb-detail* t)

;; Маски имён линейных параметров (регистр не учитывается).
(setq *rb-masks*
  (list
    "*ДЛИНА*"
    "*ВЫСОТА*"
    "*ШИРИНА*"
    "*LENGTH*"
    "*HEIGHT*"
    "*WIDTH*"
  )
)

(setq *rb-undo-level* 0)

(vl-load-com)

;; ============================================================
;; СТРОКИ И ТАБЛИЦЫ
;; ============================================================

(defun rb-string-p (x)
  (eq (type x) 'STR)
)

(defun rb-repeat (s n / out)
  (setq out "")
  (if (> n 0)
    (repeat n
      (setq out (strcat out s))
    )
  )
  out
)

(defun rb-pad (s w / diff)
  (if (null s)
    (setq s "")
  )
  (setq diff (- w (strlen s)))
  (if (> diff 0)
    (strcat s (rb-repeat " " diff))
    s
  )
)

(defun rb-to-string (v)
  (cond
    ((null v)
     ""
    )

    ((rb-string-p v)
     v
    )

    ((numberp v)
     (if (= v (fix v))
       (itoa (fix v))
       (rtos v 2 6)
     )
    )

    (t
     ""
    )
  )
)

;; Число для диагностической таблицы: 8 знаков после запятой.
(defun rb-fmt (v)
  (if (null v)
    "-"
    (rtos v 2 8)
  )
)

(defun rb-grid-line (vals widths / s)
  (setq s "\n  |")
  (while (and vals widths)
    (setq s
      (strcat
        s
        " "
        (rb-pad (rb-to-string (car vals)) (car widths))
        " |"
      )
    )
    (setq vals (cdr vals))
    (setq widths (cdr widths))
  )
  s
)

(defun rb-grid (headers rows / widths line)
  (setq widths nil)

  (foreach h headers
    (setq widths (append widths (list (strlen h))))
  )

  (foreach row rows
    (setq widths
      (mapcar
        'max
        widths
        (mapcar '(lambda (c) (strlen (rb-to-string c))) row)
      )
    )
  )

  (setq line "+")
  (foreach w widths
    (setq line (strcat line (rb-repeat "-" (+ w 2)) "+"))
  )

  (princ "\n  ")
  (princ line)
  (princ (rb-grid-line headers widths))
  (princ "\n  ")
  (princ line)

  (foreach row rows
    (princ (rb-grid-line row widths))
  )

  (princ "\n  ")
  (princ line)
  (princ)
)

(defun rb-print-startup (/ rows)
  (princ
    (strcat
      "\nRoundUp.LSP загружен (ред. "
      (itoa *rb-revision*)
      ": округление параметров блоков и обнуление координаты Z)."
    )
  )

  (princ
    "\nОКРУГЛИТЬБЛОК округляет параметры блоков вверх до шага, КРАТНОБЛОК добавляет шаг к кратным."
  )

  (princ
    "\nОБНУЛИТЬЗЕТ убирает Z у объектов модели и листов, внутрь блоков не заходит."
  )

  (princ
    "\nШаг округления по умолчанию 1, запятая в числе допустима. Изменения в одной операции UNDO."
  )

  (setq rows
    (list
      (list
        "ОКРУГЛИТЬБЛОК / ROUNDDBLOCK"
        "параметры блоков вверх до шага"
      )
      (list
        "КРАТНОБЛОК / KRATNOBLOCK"
        "строго кратно: кратное + шаг"
      )
      (list
        "ОБНУЛИТЬЗЕТ / ZEROZ"
        "все объекты модели и листов в Z = 0"
      )
      (list
        "ТЕСТОКРУГЛ / ROUNDUPTEST"
        "самопроверка округления"
      )
      (list
        "ОБНУЛИТЬЗЕТТЕСТ / ZEROZTEST"
        "самопроверка обнуления Z"
      )
      (list
        "ZZINFO"
        "проверка загрузки модуля и команд"
      )
    )
  )

  (rb-grid (list "КОМАНДА" "ЧТО ДЕЛАЕТ") rows)
)

;; ============================================================
;; МАСКИ И НОРМАЛИЗАЦИЯ ИМЁН
;; ============================================================

(defun rb-str-upper (s)
  (if (null s)
    ""
    (vl-string-translate
      *rb-lower-cyr*
      *rb-upper-cyr*
      (strcase s)
    )
  )
)

(defun rb-char-digit-p (c / code)
  (if (or (null c) (= c ""))
    nil
    (progn
      (setq code (ascii c))
      (and (>= code 48) (<= code 57))
    )
  )
)

(defun rb-char-letter-p (c / up)
  (if (or (null c) (= c ""))
    nil
    (progn
      (setq up (rb-str-upper c))
      (or
        (and (>= (ascii up) 65) (<= (ascii up) 90))
        (not (null (vl-string-search up *rb-upper-cyr*)))
      )
    )
  )
)

(defun rb-wc-match (s p / pc)
  (cond
    ((= p "")
     (= s "")
    )

    (t
     (setq pc (substr p 1 1))

     (cond
       ((= pc "*")
        (if (= (substr p 2 1) "*")
          (rb-wc-match s (substr p 2))
          (or
            (rb-wc-match s (substr p 2))
            (and
              (/= s "")
              (rb-wc-match (substr s 2) p)
            )
          )
        )
       )

       ((= s "")
        nil
       )

       ((= pc "?")
        (rb-wc-match (substr s 2) (substr p 2))
       )

       ((= pc "#")
        (and
          (rb-char-digit-p (substr s 1 1))
          (rb-wc-match (substr s 2) (substr p 2))
        )
       )

       ((= pc "@")
        (and
          (rb-char-letter-p (substr s 1 1))
          (rb-wc-match (substr s 2) (substr p 2))
        )
       )

       (t
        (and
          (= (substr s 1 1) pc)
          (rb-wc-match (substr s 2) (substr p 2))
        )
       )
     )
    )
  )
)

(defun rb-wildcard-match (txt mask)
  (if (or (null txt) (null mask))
    nil
    (rb-wc-match (rb-str-upper txt) (rb-str-upper mask))
  )
)

(defun rb-match-linear-name (name / result)
  (if (null name)
    nil
    (progn
      (setq result nil)
      (foreach m *rb-masks*
        (if (and (not result) (rb-wildcard-match name m))
          (setq result t)
        )
      )
      result
    )
  )
)

;; ============================================================
;; БЕЗОПАСНОЕ ПРЕОБРАЗОВАНИЕ В ЧИСЛО
;; ============================================================

(defun rb-comma-to-dot (s / i n c out)
  (setq i 1)
  (setq n (strlen s))
  (setq out "")
  (while (<= i n)
    (setq c (substr s i 1))
    (if (= c ",")
      (setq c ".")
    )
    (setq out (strcat out c))
    (setq i (1+ i))
  )
  out
)

(defun rb-string-number-p (s / i n c dot digit ok)
  (setq n (strlen s))
  (setq i 1)
  (setq dot nil)
  (setq digit nil)
  (setq ok t)

  (if (and
        (> n 0)
        (or (= (substr s 1 1) "+") (= (substr s 1 1) "-"))
      )
    (setq i 2)
  )

  (while (and ok (<= i n))
    (setq c (substr s i 1))
    (cond
      ((rb-char-digit-p c)
       (setq digit t)
      )

      ((and (= c ".") (not dot))
       (setq dot t)
      )

      (t
       (setq ok nil)
      )
    )
    (setq i (1+ i))
  )

  (and ok digit)
)

(defun rb-value-to-number (val / x str)
  (cond
    ((null val)
     nil
    )

    ((numberp val)
     (float val)
    )

    ((eq (type val) 'VARIANT)
     (setq x (vl-catch-all-apply 'vlax-variant-value (list val)))
     (if (vl-catch-all-error-p x)
       nil
       (rb-value-to-number x)
     )
    )

    ((eq (type val) 'SAFEARRAY)
     (setq x (vl-catch-all-apply 'vlax-safearray->list (list val)))
     (if (vl-catch-all-error-p x)
       nil
       (rb-value-to-number x)
     )
    )

    ((listp val)
     (if (= (length val) 1)
       (rb-value-to-number (car val))
       nil
     )
    )

    ((rb-string-p val)
     (setq str (rb-comma-to-dot val))
     (setq str (vl-string-trim " " str))
     (if (or (= str "") (not (rb-string-number-p str)))
       nil
       (atof str)
     )
    )

    (t
     nil
    )
  )
)

;; ============================================================
;; СВОЙСТВА ДИНАМИЧЕСКОГО БЛОКА
;; ============================================================

;; Один вызов GetDynamicBlockProperties на блок.
(defun rb-get-props (obj / res val)
  (setq res
    (vl-catch-all-apply
      'vlax-invoke
      (list obj 'GetDynamicBlockProperties)
    )
  )

  (if (vl-catch-all-error-p res)
    nil
    (cond
      ((listp res)
       res
      )

      ((eq (type res) 'SAFEARRAY)
       (setq val (vl-catch-all-apply 'vlax-safearray->list (list res)))
       (if (vl-catch-all-error-p val) nil val)
      )

      ((eq (type res) 'VARIANT)
       (setq val (vl-catch-all-apply 'vlax-variant-value (list res)))
       (if (vl-catch-all-error-p val)
         nil
         (cond
           ((eq (type val) 'SAFEARRAY)
            (setq res (vl-catch-all-apply 'vlax-safearray->list (list val)))
            (if (vl-catch-all-error-p res) nil res)
           )

           ((listp val)
            val
           )

           (t
            nil
           )
         )
       )
      )

      (t
       nil
      )
    )
  )
)

(defun rb-prop-name (prop / r)
  (setq r (vl-catch-all-apply 'vla-get-PropertyName (list prop)))

  (if (vl-catch-all-error-p r)
    nil
    (progn
      (if (eq (type r) 'VARIANT)
        (progn
          (setq r (vl-catch-all-apply 'vlax-variant-value (list r)))
          (if (vl-catch-all-error-p r)
            (setq r nil)
          )
        )
      )
      (if (rb-string-p r) r nil)
    )
  )
)

(defun rb-prop-number (prop / raw)
  (setq raw (vl-catch-all-apply 'vla-get-Value (list prop)))
  (if (vl-catch-all-error-p raw)
    nil
    (rb-value-to-number raw)
  )
)

(defun rb-prop-put (prop val / res var)
  (setq res (vl-catch-all-apply 'vla-put-Value (list prop (float val))))

  (if (vl-catch-all-error-p res)
    (progn
      (setq var
        (vl-catch-all-apply
          'vlax-make-variant
          (list
            (float val)
            (if (boundp 'vlax-vbDouble) vlax-vbDouble 5)
          )
        )
      )
      (if (not (vl-catch-all-error-p var))
        (setq res (vl-catch-all-apply 'vla-put-Value (list prop var)))
      )
    )
  )

  (not (vl-catch-all-error-p res))
)

(defun rb-object-name (obj / r)
  (setq r (vl-catch-all-apply 'vla-get-EffectiveName (list obj)))

  (if (vl-catch-all-error-p r)
    (progn
      (setq r (vl-catch-all-apply 'vla-get-Name (list obj)))
      (if (or (vl-catch-all-error-p r) (not (rb-string-p r)))
        (setq r "<блок>")
      )
    )
  )

  (if (rb-string-p r) r "<блок>")
)

;; Допуск в ЕДИНИЦАХ ЗНАЧЕНИЯ, а не в отношении val/step.
(defun rb-value-tol (v / a)
  (setq a (abs (float v)))
  (max 1.0e-9 (* a 1.0e-12))
)

;; Запись + проверка фактического значения.
(defun rb-apply-value (obj prop new / back)
  (if (not (rb-prop-put prop new))
    nil
    (progn
      (setq back (rb-prop-number prop))

      (if (or
            (null back)
            (> (abs (- back new)) (rb-value-tol new))
          )
        (progn
          (vl-catch-all-apply 'vla-update (list obj))
          (setq back (rb-prop-number prop))
        )
      )

      (and
        back
        (<= (abs (- back new)) (rb-value-tol new))
      )
    )
  )
)

;; ============================================================
;; ОКРУГЛЕНИЕ ВВЕРХ ДО КРАТНОГО
;; ============================================================

(defun rb-round-up-to-step (val step / a r k res)
  (cond
    ((or
       (null val)
       (null step)
       (not (numberp val))
       (not (numberp step))
     )
     nil
    )

    ((<= step 0.0)
     nil
    )

    ((= val 0.0)
     0.0
    )

    (t
     (setq step (float step))
     (setq a (abs (float val)))
     (setq r (/ a step))

     ;; Защита от переполнения fix при аномальных значениях.
     (if (> r 1.0e14)
       nil
       (progn
         (setq k (fix r))

         ;; Если значение выше k*step больше чем на допуск - следующий кратный.
         (if (> (- a (* (float k) step)) (rb-value-tol val))
           (setq k (1+ k))
         )

         (setq res (* (float k) step))

         ;; Режим ROUNDUP: от нуля для отрицательных значений.
         (if (< val 0.0)
           (setq res (- res))
         )

         res
       )
     )
    )
  )
)

;; ============================================================
;; ШАГ, СТАТУСЫ, ОТЧЁТ
;; ============================================================

(defun rb-get-step-from-user (/ s step)
  (setq s (getstring t "\nШаг округления <1>: "))

  (if (null s)
    'cancel
    (progn
      (setq s (vl-string-trim " " s))

      (if (= s "")
        *rb-default-step*
        (progn
          (setq step (rb-value-to-number s))
          (if (and step (> step 0.0))
            step
            'error
          )
        )
      )
    )
  )
)

(defun rb-status-text (st)
  (cond
    ((eq st 'changed)     "изменено")
    ((eq st 'same)        "без изменений")
    ((eq st 'nonnum)      "не число")
    ((eq st 'error)       "ошибка записи")
    ((eq st 'notapplied)  "не применилось")
    (t                    "?")
  )
)

(defun rb-report-summary
  (total dyn nondyn found changed same nonnum errors notapplied / rows)

  (setq rows
    (list
      (list "Выбрано блоков" (itoa total))
      (list "Обработано динамических блоков" (itoa dyn))
      (list "Пропущено не динамических блоков" (itoa nondyn))
      (list "Найдено подходящих параметров" (itoa found))
      (list "Изменено параметров" (itoa changed))
      (list "Без изменений" (itoa same))
      (list "Пропущено нечисловых значений" (itoa nonnum))
      (list "Ошибок записи" (itoa errors))
      (list "Не применилось (ограничения/зависимости)" (itoa notapplied))
    )
  )

  (rb-grid (list "Показатель" "Итог") rows)
)

;; ============================================================
;; ВЫБОР ОБЪЕКТОВ
;; ============================================================

(defun rb-get-implied-selection (flt / ss)
  (setq ss (vl-catch-all-apply 'ssget (list "_I" flt)))

  (if (or (vl-catch-all-error-p ss) (null ss) (= (sslength ss) 0))
    (progn
      (setq ss (vl-catch-all-apply 'ssget (list "I" flt)))
      (if (or (vl-catch-all-error-p ss) (null ss) (= (sslength ss) 0))
        nil
        ss
      )
    )
    ss
  )
)

;; ============================================================
;; UNDO
;; ============================================================

(defun rb-undo-begin (/ doc r)
  (setq *rb-undo-level* 0)
  (setq doc
    (vl-catch-all-apply
      'vla-get-ActiveDocument
      (list (vlax-get-acad-object))
    )
  )

  (if (not (vl-catch-all-error-p doc))
    (progn
      (setq r (vl-catch-all-apply 'vla-startUndoMark (list doc)))
      (if (not (vl-catch-all-error-p r))
        (setq *rb-undo-level* 1)
      )
    )
  )
)

(defun rb-undo-end (/ doc)
  (if (> *rb-undo-level* 0)
    (progn
      (setq doc
        (vl-catch-all-apply
          'vla-get-ActiveDocument
          (list (vlax-get-acad-object))
        )
      )
      (if (not (vl-catch-all-error-p doc))
        (vl-catch-all-apply 'vla-endUndoMark (list doc))
      )
      (setq *rb-undo-level* 0)
    )
  )
)

;; ============================================================
;; ОБРАБОТКА ОДНОГО БЛОКА ДЛЯ ОКРУГЛИТЬБЛОК
;; Возвращает: (T rows) для динамического блока, nil иначе.
;; row = (имя-блока имя-параметра было стало статус)
;; ============================================================

(defun rb-process-block (obj step / props bname prop pname old new status rows)
  (setq props (rb-get-props obj))

  (if (null props)
    nil
    (progn
      (setq bname (rb-object-name obj))
      (setq rows nil)

      (foreach prop props
        (setq pname (rb-prop-name prop))

        (if (and pname (rb-match-linear-name pname))
          (progn
            (setq old (rb-prop-number prop))
            (setq new nil)
            (setq status nil)

            (cond
              ((null old)
               (setq status 'nonnum)
              )

              (t
               (setq new (rb-round-up-to-step old step))

               (cond
                 ((null new)
                  (setq status 'error)
                 )

                 ((<= (abs (- new old)) (rb-value-tol old))
                  (setq status 'same)
                 )

                 (t
                  (if (rb-apply-value obj prop new)
                    (setq status 'changed)
                    (setq status 'notapplied)
                  )
                 )
               )
              )
            )

            (setq rows
              (cons
                (list bname pname (rb-fmt old) (rb-fmt new) status)
                rows
              )
            )
          )
        )
      )

      (list t rows)
    )
  )
)

;; ============================================================
;; ОСНОВНАЯ КОМАНДА ОКРУГЛИТЬБЛОК
;; ============================================================

(defun rb-run
  (/ *error* old-error ss flt total i ent obj r rows all row st
     step step-res dyn nondyn found changed same nonnum errors notapplied)

  (setq old-error *error*)

  (defun *error* (msg)
    (rb-undo-end)
    (if (and
          msg
          (/= msg "Function cancelled")
          (/= msg "quit / exit abort")
          (/= msg "cancel")
        )
      (princ (strcat "\nОшибка: " msg))
    )
    (setq *error* old-error)
    (princ)
  )

  (setq flt '((0 . "INSERT")))
  (setq ss (rb-get-implied-selection flt))

  (if (null ss)
    (progn
      (princ "\nВыберите динамические блоки для округления параметров: ")
      (setq ss (ssget flt))
    )
  )

  (if (null ss)
    (princ "\nБлоки не выбраны.")
    (progn
      (setq total (sslength ss))

      (if (= total 0)
        (princ "\nБлоки не выбраны.")
        (progn
          (setq step-res (rb-get-step-from-user))

          (cond
            ((eq step-res 'cancel)
             (princ "\nКоманда отменена.")
            )

            ((eq step-res 'error)
             (princ
               "\nНеверный шаг округления. Шаг должен быть положительным числом."
             )
            )

            (t
             (setq step step-res)

             (setq dyn 0)
             (setq nondyn 0)
             (setq found 0)
             (setq changed 0)
             (setq same 0)
             (setq nonnum 0)
             (setq errors 0)
             (setq notapplied 0)
             (setq all nil)

             (rb-undo-begin)

             (princ
               (strcat "\nШаг округления: " (rb-fmt step))
             )

             (setq i 0)

             (while (< i total)
               (setq ent (ssname ss i))
               (setq obj
                 (vl-catch-all-apply 'vlax-ename->vla-object (list ent))
               )

               (if (vl-catch-all-error-p obj)
                 (setq nondyn (1+ nondyn))
                 (progn
                   (setq r (rb-process-block obj step))

                   (if (null r)
                     (setq nondyn (1+ nondyn))
                     (progn
                       (setq dyn (1+ dyn))
                       (setq rows (cadr r))

                       (foreach row rows
                         (setq found (1+ found))
                         (setq st (nth 4 row))

                         (cond
                           ((eq st 'changed)
                            (setq changed (1+ changed))
                           )
                           ((eq st 'same)
                            (setq same (1+ same))
                           )
                           ((eq st 'nonnum)
                            (setq nonnum (1+ nonnum))
                           )
                           ((eq st 'error)
                            (setq errors (1+ errors))
                           )
                           (t
                            (setq notapplied (1+ notapplied))
                           )
                         )
                       )

                       (setq all (append all rows))
                     )
                   )
                 )
               )

               (setq i (1+ i))
             )

             (rb-undo-end)

             (princ)

             (rb-report-summary
               total dyn nondyn found changed same nonnum errors notapplied
             )

             (cond
               ((= found 0)
                (princ
                  "\nНе найдены параметры, подходящие под маски длины, высоты или ширины."
                )
               )

               ((and (= changed 0) (> same 0))
                (princ
                  "\nЗначения уже кратны заданному шагу; изменять нечего."
                )
               )
             )

             (if (and *rb-detail* all)
               (progn
                 (princ
                   "\nПодробно (значения в единицах чертежа):"
                 )

                 (rb-grid
                   (list "БЛОК" "ПАРАМЕТР" "БЫЛО" "СТАЛО" "РЕЗУЛЬТАТ")
                   (mapcar
                     '(lambda (x)
                        (list
                          (car x)
                          (nth 1 x)
                          (nth 2 x)
                          (nth 3 x)
                          (rb-status-text (nth 4 x))
                        )
                     )
                     all
                   )
                 )
               )
             )

             (if (> notapplied 0)
               (princ
                 "\nВнимание: часть значений блок не принял. Проверьте ограничения и зависимости параметров."
               )
             )
            )
          )
        )
      )
    )
  )

  (setq *error* old-error)
  (princ)
)

;; ============================================================
;; САМОПРОВЕРКА ОКРУГЛЕНИЯ
;; ============================================================

(defun rb-self-test (/ cases row got ok rows)
  (setq cases
    (list
      (list 1234.56 0.1 1234.6)
      (list 253.0   0.1 253.0)
      (list 12.34   0.5 12.5)
      (list 1234.1  1.0 1235.0)
      (list 1234.0  1.0 1234.0)
      (list 1231.0  10.0 1240.0)
      (list 1240.0  10.0 1240.0)
      (list -1234.1 1.0 -1235.0)
      (list 0.0     0.1 0.0)
    )
  )

  (setq rows nil)

  (foreach row cases
    (setq got (rb-round-up-to-step (car row) (nth 1 row)))
    (setq ok
      (and
        got
        (<= (abs (- got (nth 2 row))) 1.0e-6)
      )
    )

    (setq rows
      (cons
        (list
          (rb-fmt (car row))
          (rb-fmt (nth 1 row))
          (rb-fmt (nth 2 row))
          (rb-fmt got)
          (if ok "OK" "ОШИБКА")
        )
        rows
      )
    )
  )

  (princ "\nСамопроверка округления вверх до кратного:")
  (rb-grid
    (list "ЗНАЧЕНИЕ" "ШАГ" "ОЖИДАЛОСЬ" "ПОЛУЧИЛОСЬ" "РЕЗУЛЬТАТ")
    (reverse rows)
  )
  (princ)
)

;; ============================================================
;; КРАТНОБЛОК: СТРОГОЕ ОКРУГЛЕНИЕ ВВЕРХ
;; ============================================================

;; Если значение уже кратно шагу, увеличиваем его на один шаг.
(defun rb-round-up-strict-to-step (val step / base)
  (setq base (rb-round-up-to-step val step))

  (if (null base)
    nil
    (if (<= (abs (- base val)) (rb-value-tol val))
      (if (< val 0.0)
        (- base step)
        (+ base step)
      )
      base
    )
  )
)

;; Обработка одного блока для КРАТНОБЛОК.
(defun rb-process-block-strict (obj step / props bname prop pname old new status rows)
  (setq props (rb-get-props obj))

  (if (null props)
    nil
    (progn
      (setq bname (rb-object-name obj))
      (setq rows nil)

      (foreach prop props
        (setq pname (rb-prop-name prop))

        (if (and pname (rb-match-linear-name pname))
          (progn
            (setq old (rb-prop-number prop))
            (setq new nil)
            (setq status nil)

            (cond
              ((null old)
               (setq status 'nonnum)
              )

              (t
               (setq new (rb-round-up-strict-to-step old step))

               (cond
                 ((null new)
                  (setq status 'error)
                 )

                 ((<= (abs (- new old)) (rb-value-tol old))
                  (setq status 'same)
                 )

                 (t
                  (if (rb-apply-value obj prop new)
                    (setq status 'changed)
                    (setq status 'notapplied)
                  )
                 )
               )
              )
            )

            (setq rows
              (cons
                (list bname pname (rb-fmt old) (rb-fmt new) status)
                rows
              )
            )
          )
        )
      )

      (list t (reverse rows))
    )
  )
)

;; Основная команда КРАТНОБЛОК.
(defun rb-run-strict
  (/ *error* old-error ss flt total i ent obj r rows all row st
     step step-res dyn nondyn found changed same nonnum errors notapplied)

  (setq old-error *error*)

  (defun *error* (msg)
    (rb-undo-end)
    (if (and
          msg
          (/= msg "Function cancelled")
          (/= msg "quit / exit abort")
          (/= msg "cancel")
        )
      (princ (strcat "\nОшибка: " msg))
    )
    (setq *error* old-error)
    (princ)
  )

  (setq flt '((0 . "INSERT")))
  (setq ss (rb-get-implied-selection flt))

  (if (null ss)
    (progn
      (princ "\nВыберите динамические блоки для кратного округления параметров: ")
      (setq ss (ssget flt))
    )
  )

  (if (null ss)
    (princ "\nБлоки не выбраны.")
    (progn
      (setq total (sslength ss))

      (if (= total 0)
        (princ "\nБлоки не выбраны.")
        (progn
          (setq step-res (rb-get-step-from-user))

          (cond
            ((eq step-res 'cancel)
             (princ "\nКоманда отменена.")
            )

            ((eq step-res 'error)
             (princ
               "\nНеверный шаг округления. Шаг должен быть положительным числом."
             )
            )

            (t
             (setq step step-res)

             (setq dyn 0)
             (setq nondyn 0)
             (setq found 0)
             (setq changed 0)
             (setq same 0)
             (setq nonnum 0)
             (setq errors 0)
             (setq notapplied 0)
             (setq all nil)

             (rb-undo-begin)

             (princ "\nРежим: строгое кратное округление вверх.")
             (princ
               (strcat "\nШаг округления: " (rb-fmt step))
             )

             (setq i 0)

             (while (< i total)
               (setq ent (ssname ss i))
               (setq obj
                 (vl-catch-all-apply 'vlax-ename->vla-object (list ent))
               )

               (if (vl-catch-all-error-p obj)
                 (setq nondyn (1+ nondyn))
                 (progn
                   (setq r (rb-process-block-strict obj step))

                   (if (null r)
                     (setq nondyn (1+ nondyn))
                     (progn
                       (setq dyn (1+ dyn))
                       (setq rows (cadr r))

                       (foreach row rows
                         (setq found (1+ found))
                         (setq st (nth 4 row))

                         (cond
                           ((eq st 'changed)
                            (setq changed (1+ changed))
                           )
                           ((eq st 'same)
                            (setq same (1+ same))
                           )
                           ((eq st 'nonnum)
                            (setq nonnum (1+ nonnum))
                           )
                           ((eq st 'error)
                            (setq errors (1+ errors))
                           )
                           (t
                            (setq notapplied (1+ notapplied))
                           )
                         )
                       )

                       (setq all (append all rows))
                     )
                   )
                 )
               )

               (setq i (1+ i))
             )

             (rb-undo-end)

             (princ)

             (rb-report-summary
               total dyn nondyn found changed same nonnum errors notapplied
             )

             (cond
               ((= found 0)
                (princ
                  "\nНе найдены параметры, подходящие под маски длины, высоты или ширины."
                )
               )

               ((and (= changed 0) (> found 0))
                (princ
                  "\nИзменений не выполнено."
                )
               )
             )

             (if (and *rb-detail* all)
               (progn
                 (princ
                   "\nПодробно (значения в единицах чертежа):"
                 )

                 (rb-grid
                   (list "БЛОК" "ПАРАМЕТР" "БЫЛО" "СТАЛО" "РЕЗУЛЬТАТ")
                   (mapcar
                     '(lambda (x)
                        (list
                          (car x)
                          (nth 1 x)
                          (nth 2 x)
                          (nth 3 x)
                          (rb-status-text (nth 4 x))
                        )
                     )
                     all
                   )
                 )
               )
             )

             (if (> notapplied 0)
               (princ
                 "\nВнимание: часть значений блок не принял. Проверьте ограничения и зависимости параметров."
               )
             )
            )
          )
        )
      )
    )
  )

  (setq *error* old-error)
  (princ)
)

;; ============================================================
;; ОБНУЛЕНИЕ КООРДИНАТЫ Z — ПАРАМЕТРЫ
;; ============================================================

;; Редакция части обнуления Z отслеживается отдельно от общей шапки.
(setq *zz-revision* 2)

;; Показывать таблицу по типам объектов.
(setq *zz-detail* t)

;; Допуск сравнения координат.
(setq *zz-tol* 1.0e-9)

;; Уровень Undo-марки: 1 - открыта, 0 - закрыта.
(setq *zz-undo* 0)

;; ============================================================
;; ДОКУМЕНТ, UNDO, ЗАВЕРШЕНИЕ
;; ============================================================

(defun zz-doc (/ doc)
  (setq doc
    (vl-catch-all-apply
      'vla-get-ActiveDocument
      (list (vlax-get-acad-object))
    )
  )
  (if (vl-catch-all-error-p doc)
    nil
    doc
  )
)

(defun zz-undo-begin (/ doc r)
  (setq *zz-undo* 0)
  (setq doc (zz-doc))
  (if doc
    (progn
      (setq r (vl-catch-all-apply 'vla-StartUndoMark (list doc)))
      (if (not (vl-catch-all-error-p r))
        (setq *zz-undo* 1)
      )
    )
  )
)

(defun zz-undo-end (/ doc)
  (if (> *zz-undo* 0)
    (progn
      (setq doc (zz-doc))
      (if doc
        (vl-catch-all-apply 'vla-EndUndoMark (list doc))
      )
      (setq *zz-undo* 0)
    )
  )
)

(defun zz-regen (/ doc)
  (setq doc (zz-doc))
  (if doc
    (vl-catch-all-apply 'vla-Regen (list doc 1))
  )
)

;; ============================================================
;; ОПРЕДЕЛЕНИЕ ТИПА И ПРОПУСКИ
;; ============================================================

;; Объекты, которые нельзя выправить обнулением Z: объёмные тела,
;; составные объекты с собственной системой координат, видовые экраны.
(defun zz-skip-type-p (typ flags)
  (or
    (member typ
      '("VIEWPORT" "MULTILEADER" "MLINE" "3DSOLID" "REGION" "BODY"
        "SURFACE" "MESH" "OLE2FRAME" "ACAD_PROXY_ENTITY")
    )
    ;; Полигональная сеть (polyface mesh): вершины хранят грани,
    ;; обнуление Z уничтожает форму.
    (and (= typ "POLYLINE") (/= 0 (logand 64 flags)))
  )
)

(defun zz-locked-p (ed / lay ld)
  (setq lay (cdr (assoc 8 ed)))
  (if (null lay)
    nil
    (progn
      (setq ld (entget (tblobjname "LAYER" lay)))
      (if (null ld)
        nil
        (/= 0 (logand 4 (cdr (assoc 70 ld))))
      )
    )
  )
)

;; Наклонный вектор выдавливания: обнуление Z в OCS не переводит
;; объект в плоскость Z = 0, а сброс 210 меняет его ориентацию.
(defun zz-nonplanar-p (ed / n)
  (setq n (cdr (assoc 210 ed)))
  (and
    n
    (not (equal n '(0.0 0.0 1.0) 1.0e-8))
    (not (equal n '(0.0 0.0 -1.0) 1.0e-8))
  )
)

;; Типы, у которых координаты задаются набором точек и правятся entmod.
(defun zz-simple-type-p (typ)
  (member typ
    '("LINE" "POINT" "CIRCLE" "ARC" "IMAGE" "WIPEOUT" "SHAPE"
      "TOLERANCE" "TEXT" "ATTDEF" "MTEXT" "SOLID" "TRACE" "3DFACE"
      "SPLINE" "RAY" "XLINE" "LEADER")
  )
)

;; ============================================================
;; ТОЧКИ: OCS И WCS
;; ============================================================

(defun zz-put-code (ed code val)
  (subst (cons code val) (assoc code ed) ed)
)

;; Точечная группа хранится в OCS: переводим в WCS, обнуляем Z,
;; возвращаем в OCS того же объекта.
(defun zz-zero-ocs (ent ed code / p w)
  (setq p (cdr (assoc code ed)))
  (if (null p)
    ed
    (progn
      (setq w (trans p ent 0))
      (zz-put-code ed code (trans (list (car w) (cadr w) 0.0) 0 ent))
    )
  )
)

(defun zz-zero-wcs (ed code / p)
  (setq p (cdr (assoc code ed)))
  (if (null p)
    ed
    (zz-put-code ed code (list (car p) (cadr p) 0.0))
  )
)

;; Обнуление всех вхождений кодов: у SPLINE и LEADER точки повторяются.
(defun zz-zero-wcs-all (ed codes / out pair val)
  (setq out nil)
  (foreach pair ed
    (setq val (cdr pair))
    (setq out
      (cons
        (if (and
              (member (car pair) codes)
              (listp val)
              (= (length val) 3)
            )
          (cons (car pair) (list (car val) (cadr val) 0.0))
          pair
        )
        out
      )
    )
  )
  (reverse out)
)

(defun zz-entmod (ed)
  (not (vl-catch-all-error-p (vl-catch-all-apply 'entmod (list ed))))
)

(defun zz-entupd (ent)
  (vl-catch-all-apply 'entupd (list ent))
)

(defun zz-move-by-z (obj dz / r)
  (setq r
    (vl-catch-all-apply
      'vla-Move
      (list obj
        (vlax-3d-point 0.0 0.0 0.0)
        (vlax-3d-point 0.0 0.0 dz)
      )
    )
  )
  (not (vl-catch-all-error-p r))
)

;; ============================================================
;; ОБРАБОТКА ПО ТИПАМ
;; ============================================================

;; Простые типы: все точки приводятся к Z = 0.
(defun zz-flatten-ed (ent ed typ / e)
  (cond
    ((= typ "LINE")
     (zz-zero-wcs (zz-zero-wcs ed 10) 11)
    )

    ((member typ '("CIRCLE" "ARC" "IMAGE" "WIPEOUT" "SHAPE" "TOLERANCE"))
     (zz-zero-ocs ent ed 10)
    )

    ((= typ "POINT")
     (zz-zero-wcs ed 10)
    )

    ((member typ '("TEXT" "ATTDEF"))
     (zz-zero-ocs ent (zz-zero-ocs ent ed 10) 11)
    )

    ((= typ "MTEXT")
     (zz-zero-wcs ed 10)
    )

    ((member typ '("SOLID" "TRACE" "3DFACE"))
     (zz-zero-ocs ent
       (zz-zero-ocs ent
         (zz-zero-ocs ent (zz-zero-ocs ent ed 10) 11)
         12
       )
       13
     )
    )

    ((member typ '("SPLINE" "RAY" "XLINE"))
     (zz-zero-wcs-all ed '(10 11))
    )

    ((= typ "LEADER")
     (zz-zero-wcs-all ed '(10))
    )

    (t
     ed
    )
  )
)

(defun zz-do-flatten (ent ed typ / ed2)
  (setq ed2 (zz-flatten-ed ent ed typ))
  (cond
    ((equal ed2 ed)
     'same
    )

    ((zz-entmod ed2)
     (zz-entupd ent)
     'changed
    )

    (t
     'error
    )
  )
)

;; Высота LW-полилинии: вершины в плоскости высоты, форма не меняется.
(defun zz-do-lwpolyline (ent ed / e)
  (setq e (cdr (assoc 38 ed)))
  (cond
    ((null e)
     'same
    )

    ((equal e 0.0 *zz-tol*)
     'same
    )

    ((zz-entmod (zz-put-code ed 38 0.0))
     (zz-entupd ent)
     'changed
    )

    (t
     'error
    )
  )
)

;; 2D-полилиния: высота в группе 30, вершины в OCS полилинии.
;; 3D-полилиния: Z у каждой вершины, координаты в WCS.
(defun zz-do-polyline (ent ed / is3d changed ok e v ve p w)
  (setq is3d (/= 0 (logand 8 (cdr (assoc 70 ed)))))
  (setq changed nil)
  (setq ok t)

  (if (not is3d)
    (progn
      (setq e (cdr (assoc 30 ed)))
      (if (and e (not (equal e 0.0 *zz-tol*)))
        (if (zz-entmod (zz-put-code ed 30 0.0))
          (setq changed t)
          (setq ok nil)
        )
      )
    )
  )

  (setq v (if ok (entnext ent) nil))

  (while (and ok v)
    (setq ve (entget v))

    (cond
      ((= "SEQEND" (cdr (assoc 0 ve)))
       (setq v nil)
      )

      ((= "VERTEX" (cdr (assoc 0 ve)))
       (setq p (cdr (assoc 10 ve)))

       (cond
         ((null p)
          nil
         )

         (is3d
          (if (not (equal (caddr p) 0.0 *zz-tol*))
            (if (zz-entmod (zz-put-code ve 10 (list (car p) (cadr p) 0.0)))
              (setq changed t)
              (setq ok nil)
            )
          )
         )

         (t
          (setq w (trans p ent 0))
          (if (not (equal (caddr w) 0.0 *zz-tol*))
            (if (zz-entmod
                  (zz-put-code ve 10 (trans (list (car w) (cadr w) 0.0) 0 ent))
                )
              (setq changed t)
              (setq ok nil)
            )
          )
         )
       )

       (setq v (if ok (entnext v) nil))
      )

      (t
       (setq v (entnext v))
      )
    )
  )

  (cond
    ((not ok)
     'error
    )

    (changed
     (zz-entupd ent)
     'changed
    )

    (t
     'same
    )
  )
)

;; Объекты, у которых Z задаётся единственной опорной точкой, а
;; определение лежит в определении блока или в ассоциативной связи:
;; сдвиг через ActiveX двигает объект целиком (у вставки - вместе
;; с атрибутами), не разрушая связей.
(defun zz-ref-wcs-z (ent ed typ / p)
  (setq p (cdr (assoc 10 ed)))
  (cond
    ((null p)
     nil
    )

    ;; INSERT, HATCH, ACAD_TABLE: группа 10 в OCS.
    ((member typ '("INSERT" "HATCH" "ACAD_TABLE"))
     (caddr (trans p ent 0))
    )

    ;; DIMENSION: группа 10 в WCS.
    ((= typ "DIMENSION")
     (caddr p)
    )

    (t
     nil
    )
  )
)

(defun zz-do-move (obj ent ed typ / z)
  (setq z (zz-ref-wcs-z ent ed typ))
  (cond
    ((null z)
     'skip-type
    )

    ((equal z 0.0 *zz-tol*)
     'same
    )

    ((zz-move-by-z obj (- z))
     'changed
    )

    (t
     'error
    )
  )
)

;; Возвращает список (СТАТУС ТИП).
(defun zz-process-entity (obj / ent ed typ flags st)
  (setq ent (vl-catch-all-apply 'vlax-vla-object->ename (list obj)))
  (if (vl-catch-all-error-p ent)
    (list 'error "?")
    (progn
      (setq ed (entget ent))
      (setq typ (cdr (assoc 0 ed)))
      (setq flags (if (cdr (assoc 70 ed)) (cdr (assoc 70 ed)) 0))

      (setq st
        (cond
          ((zz-skip-type-p typ flags)
           'skip-type
          )

          ((zz-locked-p ed)
           'skip-locked
          )

          ((zz-nonplanar-p ed)
           'skip-normal
          )

          ((= typ "LWPOLYLINE")
           (zz-do-lwpolyline ent ed)
          )

          ((= typ "POLYLINE")
           (zz-do-polyline ent ed)
          )

          ((member typ '("INSERT" "HATCH" "ACAD_TABLE" "DIMENSION"))
           (zz-do-move obj ent ed typ)
          )

          ((zz-simple-type-p typ)
           (zz-do-flatten ent ed typ)
          )

          (t
           'skip-type
          )
        )
      )

      (list st typ)
    )
  )
)

;; ============================================================
;; ОТЧЁТЫ
;; ============================================================

(defun zz-status-name (st)
  (cond
    ((eq st 'changed)
     "изменено"
    )

    ((eq st 'same)
     "уже Z = 0"
    )

    ((eq st 'skip-type)
     "тип не поддерживается"
    )

    ((eq st 'skip-locked)
     "слой заблокирован"
    )

    ((eq st 'skip-normal)
     "наклонный объект"
    )

    (t
     "Ошибки обработки"
    )
  )
)

(defun zz-report (total changed same stype slock snorm errs lrows trows)
  (rb-grid
    (list "ПОКАЗАТЕЛЬ" "ОБЪЕКТОВ")
    (list
      (list "Всего объектов" (itoa total))
      (list "Изменено" (itoa changed))
      (list "Уже в плоскости Z = 0" (itoa same))
      (list (zz-status-name 'skip-type) (itoa stype))
      (list (zz-status-name 'skip-locked) (itoa slock))
      (list (zz-status-name 'skip-normal) (itoa snorm))
      (list (zz-status-name 'error) (itoa errs))
    )
  )

  (rb-grid
    (list "ПРОСТРАНСТВО" "ОБЪЕКТОВ" "ИЗМЕНЕНО")
    lrows
  )

  (if (and *zz-detail* trows)
    (rb-grid
      (list "ТИП" "ОБЪЕКТОВ" "ИЗМЕНЕНО")
      trows
    )
  )
)

;; ============================================================
;; ДОБАВЛЕНИЕ В СВОДКУ ПО ТИПАМ
;; ============================================================

(defun zz-tally-add (tally typ inc-changed / hit)
  (setq hit (assoc typ tally))
  (if hit
    (subst
      (list typ (1+ (cadr hit)) (+ (caddr hit) inc-changed))
      hit
      tally
    )
    (append tally (list (list typ 1 inc-changed)))
  )
)

(defun zz-tally-rows (tally / rows)
  (setq rows nil)
  (foreach item (vl-sort tally '(lambda (a b) (< (car a) (car b))))
    (setq rows
      (append rows
        (list
          (list (car item) (itoa (cadr item)) (itoa (caddr item)))
        )
      )
    )
  )
  rows
)

;; ============================================================
;; ОСНОВНАЯ КОМАНДА
;; ============================================================

(defun zz-run (/ *error* old-error doc lays lay lname blk objs obj res st typ
                 total changed same stype slock snorm errs lrows trows tally
                 ltotal lchanged)

  (setq old-error *error*)

  (defun *error* (msg)
    (zz-undo-end)
    (if (and
          msg
          (/= msg "Function cancelled")
          (/= msg "quit / exit abort")
          (/= msg "cancel")
        )
      (princ (strcat "\nОшибка: " msg))
    )
    (setq *error* old-error)
    (princ)
  )

  (setq doc (zz-doc))

  (if (null doc)
    (princ "\nНет доступа к документу AutoCAD.")
    (progn
      (setq lays (vl-catch-all-apply 'vla-get-Layouts (list doc)))

      (if (vl-catch-all-error-p lays)
        (princ "\nНе удалось получить список листов чертежа.")
        (progn
          (setq total 0)
          (setq changed 0)
          (setq same 0)
          (setq stype 0)
          (setq slock 0)
          (setq snorm 0)
          (setq errs 0)
          (setq lrows nil)
          (setq tally nil)

          (zz-undo-begin)

          (princ "\nОбнуление координаты Z...")

          (vlax-for lay lays
            (setq lname (vl-catch-all-apply 'vla-get-Name (list lay)))
            (setq blk (vl-catch-all-apply 'vla-get-Block (list lay)))

            (if (or
                  (vl-catch-all-error-p lname)
                  (vl-catch-all-error-p blk)
                )
              nil
              (progn
                (setq objs nil)
                (vlax-for obj blk
                  (setq objs (cons obj objs))
                )

                (setq ltotal 0)
                (setq lchanged 0)

                (foreach obj (reverse objs)
                  (setq res (zz-process-entity obj))
                  (setq st (car res))
                  (setq typ (cadr res))

                  (setq total (1+ total))
                  (setq ltotal (1+ ltotal))

                  (cond
                    ((eq st 'changed)
                     (setq changed (1+ changed))
                     (setq lchanged (1+ lchanged))
                    )

                    ((eq st 'same)
                     (setq same (1+ same))
                    )

                    ((eq st 'skip-type)
                     (setq stype (1+ stype))
                    )

                    ((eq st 'skip-locked)
                     (setq slock (1+ slock))
                    )

                    ((eq st 'skip-normal)
                     (setq snorm (1+ snorm))
                    )

                    (t
                     (setq errs (1+ errs))
                    )
                  )

                  (if (member st '(changed same))
                    (setq tally
                      (zz-tally-add tally typ
                        (if (eq st 'changed) 1 0)
                      )
                    )
                  )
                )

                (setq lrows
                  (append lrows
                    (list
                      (list lname (itoa ltotal) (itoa lchanged))
                    )
                  )
                )
              )
            )
          )

          (zz-undo-end)
          (zz-regen)

          (if (= total 0)
            (princ "\nВ чертеже не найдено объектов для обработки.")
            (progn
              (princ
                (strcat
                  "\nГотово. Обработано объектов: "
                  (itoa total)
                  ". Изменено: "
                  (itoa changed)
                  "."
                )
              )

              (zz-report total changed same stype slock snorm errs
                lrows (zz-tally-rows tally)
              )
            )
          )
        )
      )
    )
  )

  (setq *error* old-error)
  (princ)
)

;; ============================================================
;; САМОПРОВЕРКА
;; ============================================================

;; Z опорной точки в WCS: для типов с OCS trans учитывает 210.
(defun zz-obj-z (ent ed typ / p)
  (cond
    ((= typ "LWPOLYLINE")
     (caddr (trans (list 0.0 0.0 (cond ((cdr (assoc 38 ed))) (0.0))) ent 0))
    )

    ((member typ '("LINE" "POLYLINE" "SPLINE"))
     (setq p (cdr (assoc 10 ed)))
     (if p (caddr p) 0.0)
    )

    (t
     (setq p (cdr (assoc 10 ed)))
     (if p (caddr (trans p ent 0)) 0.0)
    )
  )
)

(defun zz-self-test (/ *error* old-error doc en ed typ lst rows z0 z1 ok all)

  (setq old-error *error*)

  (defun *error* (msg)
    (zz-undo-end)
    (if (and
          msg
          (/= msg "Function cancelled")
          (/= msg "quit / exit abort")
        )
      (princ (strcat "\nОшибка: " msg))
    )
    (setq *error* old-error)
    (princ)
  )

  (setq doc (zz-doc))

  (if (null doc)
    (princ "\nНет доступа к документу AutoCAD.")
    (progn
      (zz-undo-begin)

      (setq lst nil)

      ;; Линия в плоскости Z = 120.
      (if (entmake '((0 . "LINE") (10 0.0 0.0 120.0) (11 10.0 10.0 120.0)))
        (setq lst (cons (entlast) lst))
      )

      ;; Окружность в плоскости Z = 75.
      (if (entmake '((0 . "CIRCLE") (10 50.0 50.0 75.0) (40 . 10.0)))
        (setq lst (cons (entlast) lst))
      )

      ;; Текст в плоскости Z = 40.
      (if (entmake '((0 . "TEXT") (10 100.0 100.0 40.0) (40 . 2.5)
                     (1 . "ТЕСТ Z") (7 . "Standard")))
        (setq lst (cons (entlast) lst))
      )

      ;; LW-полилиния с высотой 30.
      (if (entmake '((0 . "LWPOLYLINE") (100 . "AcDbEntity")
                     (100 . "AcDbPolyline") (90 . 2) (70 . 0)
                     (38 . 30.0)
                     (10 0.0 0.0) (10 10.0 0.0)))
        (setq lst (cons (entlast) lst))
      )

      (if (null lst)
        (princ "\nНе удалось создать тестовые объекты.")
        (progn
          (setq rows nil)
          (setq all t)

          (foreach en (reverse lst)
            (setq ed (entget en))
            (setq typ (cdr (assoc 0 ed)))
            (setq z0 (zz-obj-z en ed typ))

            (zz-process-entity (vlax-ename->vla-object en))

            (setq ed (entget en))
            (setq z1 (zz-obj-z en ed typ))
            (setq ok (equal z1 0.0 1.0e-6))
            (if (not ok)
              (setq all nil)
            )

            (setq rows
              (append rows
                (list
                  (list typ
                    (rtos z0 2 3)
                    (rtos z1 2 3)
                    (if ok "OK" "ОШИБКА")
                  )
                )
              )
            )
          )

          ;; Тестовые объекты удаляются: чертёж остаётся чистым.
          (foreach en (reverse lst)
            (entdel en)
          )

          (princ "\nСамопроверка обнуления Z:")
          (rb-grid
            (list "ТИП" "Z ДО" "Z ПОСЛЕ" "РЕЗУЛЬТАТ")
            rows
          )
          (princ
            (if all
              "\nСамопроверка пройдена."
              "\nСамопроверка не пройдена, см. таблицу."
            )
          )
        )
      )

      (zz-undo-end)
    )
  )

  (setq *error* old-error)
  (princ)
)

;; ============================================================
;; ДИАГНОСТИКА ЗАГРУЗКИ
;; ============================================================

;; Имена команд выводятся через vl-symbol-name: сравнение строк не
;; зависит от локали, а список символов показывает, зарегистрирована
;; ли команда в текущем чертеже.
(defun zz-cmd-defined-p (name / names)
  (setq names (mapcar 'vl-symbol-name (atoms-family 0)))
  (if (member name names) "есть" "НЕТ")
)

(defun zz-info (/ rows doc fname)
  (setq doc (zz-doc))
  (setq fname (findfile "ZeroZ.LSP"))

  (setq rows
    (list
      (list "Редакция модуля" (itoa *zz-revision*))
      (list "Файл в путях поддержки" (if fname fname "не найден"))
      (list "Чертёж"
        (if doc
          (vl-filename-base (vla-get-Name doc))
          "нет доступа"
        )
      )
      (list "ОБНУЛИТЬЗЕТ" (zz-cmd-defined-p "C:ОБНУЛИТЬЗЕТ"))
      (list "ZEROZ" (zz-cmd-defined-p "C:ZEROZ"))
      (list "ОБНУЛИТЬЗЕТТЕСТ" (zz-cmd-defined-p "C:ОБНУЛИТЬЗЕТТЕСТ"))
      (list "ZEROZTEST" (zz-cmd-defined-p "C:ZEROZTEST"))
    )
  )

  (princ "\nПроверка модуля RoundUp:")
  (rb-grid (list "ПОКАЗАТЕЛЬ" "ЗНАЧЕНИЕ") rows)

  (princ "\nКоманды AutoCAD действуют только в том чертеже, где модуль загружен.")
  (princ "\nЕсли кириллические имена показывают НЕТ, используйте ZEROZ и ZEROZTEST.")
  (princ)
)

;; ============================================================
;; КОМАНДЫ
;; ============================================================

(defun C:ОКРУГЛИТЬБЛОК ()
  (rb-run)
)

(defun C:ROUNDDBLOCK ()
  (rb-run)
)

(defun C:КРАТНОБЛОК ()
  (rb-run-strict)
)

(defun C:KRATNOBLOCK ()
  (rb-run-strict)
)

(defun C:ТЕСТОКРУГЛ ()
  (rb-self-test)
)

(defun C:ROUNDUPTEST ()
  (rb-self-test)
)

;; ------------------------------------------------------------
;; ОБНУЛЕНИЕ Z
;; ------------------------------------------------------------

(defun C:ОБНУЛИТЬЗЕТ ()
  (zz-run)
)

(defun C:ZEROZ ()
  (zz-run)
)

(defun C:ОБНУЛИТЬЗЕТТЕСТ ()
  (zz-self-test)
)

(defun C:ZEROZTEST ()
  (zz-self-test)
)

(defun C:ZZINFO ()
  (zz-info)
)

;; ============================================================
;; ЗАГРУЗКА
;; ============================================================

(rb-print-startup)
(princ)