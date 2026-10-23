;;; ============================================================
;;; RoundUp.LSP
;;; Округление значений линейных параметров динамических блоков.
;;; Зависимости: Visual LISP ActiveX.
;;;
;;; РЕДАКЦИЯ 5:
;;;   - добавлена команда КРАТНОБЛОК / KRATNOBLOCK;
;;;   - КРАТНОБЛОК строго округляет вверх до следующего кратного:
;;;     уже кратные значения увеличиваются на шаг;
;;;   - логика ОКРУГЛИТЬБЛОК не изменялась.
;;;
;;; ВАЖНО: модуль сгенерирован ИИ, требуется автоматическая и ручная проверка.
;;; ============================================================

(setq *rb-revision* 5)

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
      ": две команды округления линейных параметров)."
    )
  )

  (princ
    "\nОКРУГЛИТЬБЛОК округляет вверх до кратного шага. КРАТНОБЛОК строго увеличивает до следующего кратного."
  )

  (princ
    "\nШаг по умолчанию: 1. Ввод шага поддерживает запятую как десятичный разделитель."
  )

  (setq rows
    (list
      (list
        "ОКРУГЛИТЬБЛОК / ROUNDDBLOCK"
        "округляет параметры блоков вверх"
      )
      (list
        "КРАТНОБЛОК / KRATNOBLOCK"
        "строго кратно: кратные значения + шаг"
      )
      (list
        "ТЕСТОКРУГЛ / ROUNDUPTEST"
        "самопроверка функции округления"
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

;; ============================================================
;; ЗАГРУЗКА
;; ============================================================

(rb-print-startup)
(princ)