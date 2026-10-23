;;; ============================================================
;;; ZeroZ.LSP
;;; Обнуление координаты Z у объектов модели и всех листов.
;;; Зависимости: Visual LISP ActiveX.
;;;
;;; РЕДАКЦИЯ 1: первая редакция. Обход только по пространствам
;;;   (модель и каждый лист). Внутрь определений блоков не
;;;   заходим: у вставки обнуляется только её точка вставки.
;;;
;;; ВАЖНО: модуль сгенерирован ИИ, требуется автоматическая и ручная проверка.
;;; ============================================================

(setq *zz-revision* 1)

;; Показывать таблицу по типам объектов.
(setq *zz-detail* t)

;; Допуск сравнения координат.
(setq *zz-tol* 1.0e-9)

;; Уровень Undo-марки: 1 - открыта, 0 - закрыта.
(setq *zz-undo* 0)

(vl-load-com)

;; ============================================================
;; СТРОКИ И ТАБЛИЦЫ
;; ============================================================

(defun zz-repeat (s n / out)
  (setq out "")
  (if (> n 0)
    (repeat n
      (setq out (strcat out s))
    )
  )
  out
)

(defun zz-pad (s w / diff)
  (if (null s)
    (setq s "")
  )
  (setq diff (- w (strlen s)))
  (if (> diff 0)
    (strcat s (zz-repeat " " diff))
    s
  )
)

(defun zz-to-string (v)
  (cond
    ((null v)
     ""
    )

    ((eq (type v) 'STR)
     v
    )

    ((numberp v)
     (itoa (fix v))
    )

    (t
     ""
    )
  )
)

(defun zz-grid-line (vals widths / s)
  (setq s "\n  |")
  (while (and vals widths)
    (setq s
      (strcat
        s
        " "
        (zz-pad (zz-to-string (car vals)) (car widths))
        " |"
      )
    )
    (setq vals (cdr vals))
    (setq widths (cdr widths))
  )
  s
)

(defun zz-grid (headers rows / widths line)
  (setq widths nil)

  (foreach h headers
    (setq widths (append widths (list (strlen h))))
  )

  (foreach row rows
    (setq widths
      (mapcar
        'max
        widths
        (mapcar '(lambda (c) (strlen (zz-to-string c))) row)
      )
    )
  )

  (setq line "+")
  (foreach w widths
    (setq line (strcat line (zz-repeat "-" (+ w 2)) "+"))
  )

  (princ "\n  ")
  (princ line)
  (princ (zz-grid-line headers widths))
  (princ "\n  ")
  (princ line)

  (foreach row rows
    (princ (zz-grid-line row widths))
  )

  (princ "\n  ")
  (princ line)
  (princ)
)

(defun zz-print-startup (/ rows)
  (princ
    (strcat
      "\nZeroZ.LSP загружен (ред. "
      (itoa *zz-revision*)
      ": обнуление координаты Z)."
    )
  )

  (princ
    "\nОбнуляет координату Z у объектов модели и всех листов: точки вставки, вершины и определяющие точки."
  )

  (princ
    "\nВнутрь определений блоков не заходит: у вставки меняется только её точка вставки."
  )

  (princ
    "\nИзменения объединяются в одну операцию UNDO. Повторный запуск безопасен."
  )

  (setq rows
    (list
      (list
        "ОБНУЛИТЬЗЕТ / ZEROZ"
        "все объекты модели и листов в плоскость Z = 0"
      )
      (list
        "ОБНУЛИТЬЗЕТТЕСТ / ZEROZTEST"
        "самопроверка на временных объектах"
      )
    )
  )

  (zz-grid (list "КОМАНДА" "ЧТО ДЕЛАЕТ") rows)
)

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
  (zz-grid
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

  (zz-grid
    (list "ПРОСТРАНСТВО" "ОБЪЕКТОВ" "ИЗМЕНЕНО")
    lrows
  )

  (if (and *zz-detail* trows)
    (zz-grid
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
          (zz-grid
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
;; КОМАНДЫ
;; ============================================================

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

;; ============================================================
;; ЗАГРУЗКА
;; ============================================================

(zz-print-startup)
(princ)
