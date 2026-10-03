;;; ------------------------------------------------------------------
;;; AL_Core.lsp - CAD 공통 함수 (오류 처리, 설정, 선택, 출력 등)
;;; ------------------------------------------------------------------

(setq al:*version* "1.1.0")

(if vl-load-com (vl-load-com))

;; 결과를 돌려주고, 오류가 나면 nil
(defun al:catch (fn args / r)
  (setq r (vl-catch-all-apply fn args))
  (if (vl-catch-all-error-p r) nil r)
)

;; ActiveX(vla-*) 를 쓸 수 있는지 (Windows 용 AutoCAD/GstarCAD/ZWCAD/BricsCAD)
(defun al:activex-p ()
  (and vlax-get-acad-object (al:catch 'vlax-get-acad-object nil) T)
)

(defun al:doc () (vla-get-ActiveDocument (vlax-get-acad-object)))

;;; --- 명령 시작/종료, 오류 처리 -----------------------------------

(defun al:undo-begin ()
  (if (al:activex-p)
    (vla-StartUndoMark (al:doc))
    (command "_.UNDO" "_BEGIN")
  )
)

(defun al:undo-end ()
  (if (al:activex-p)
    (vla-EndUndoMark (al:doc))
    (command "_.UNDO" "_END")
  )
)

(defun al:*error* (msg)
  (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*,*취소*")))
    (princ (strcat "\n[AutoLisp] 오류: " msg))
  )
  (al:end)
)

(defun al:begin ()
  (setq al:*olderr* *error*
        *error* al:*error*
        al:*oldecho* (getvar "CMDECHO")
  )
  (setvar "CMDECHO" 0)
  (al:catch 'al:undo-begin nil)
  (setq al:*undo-open* T)
  (princ)
)

(defun al:end ()
  (if al:*relock* (al:catch 'al:layer-relock nil))
  (if al:*undo-open*
    (progn (al:catch 'al:undo-end nil) (setq al:*undo-open* nil))
  )
  (if al:*oldecho* (setvar "CMDECHO" al:*oldecho*))
  (setq *error* al:*olderr* al:*olderr* nil al:*oldecho* nil)
  (princ)
)

;;; --- 잠긴 레이어 잠시 풀기 / 다시 잠그기 -------------------------

(defun al:layer-unlock (name / r ed fl)
  (if (and name (setq r (tblobjname "LAYER" name)))
    (progn
      (setq ed (entget r) fl (cdr (assoc 70 ed)))
      (if (and (= 4 (logand 4 fl))
               (entmod (subst (cons 70 (- fl 4)) (assoc 70 ed) ed)))
        (setq al:*relock* (cons name al:*relock*))
      )
    )
  )
)

(defun al:layer-relock (/ r ed)
  (foreach name al:*relock*
    (if (setq r (tblobjname "LAYER" name))
      (progn
        (setq ed (entget r))
        (entmod (subst (cons 70 (logior 4 (cdr (assoc 70 ed)))) (assoc 70 ed) ed))
      )
    )
  )
  (setq al:*relock* nil)
)

;; 엔티티(와 소유 블록)의 레이어 잠금을 잠시 푼다
(defun al:unlock-for (e owner)
  (if e (al:layer-unlock (cdr (assoc 8 (entget e)))))
  (if (= (type owner) 'ENAME) (al:layer-unlock (cdr (assoc 8 (entget owner)))))
)

;;; --- 설정 저장 (레지스트리/환경변수) ------------------------------

(defun al:cfg-get (key def / v)
  (setq v (al:catch 'getenv (list (strcat "AutoLisp_" key))))
  (if (and v (/= v "")) v def)
)

(defun al:cfg-set (key val)
  (al:catch 'setenv (list (strcat "AutoLisp_" key) val))
  val
)

;;; --- 입력 도우미 ---------------------------------------------------

;; kws 예: "Yes No". Enter 는 def
(defun al:kword (msg kws def / r)
  (initget kws)
  (setq r (getkword msg))
  (if r r def)
)

(defun al:yes-p (msg def)
  (= (al:kword (strcat msg " [Yes/No] <" def ">: ") "Yes No" def) "Yes")
)

(defun al:getreal-def (msg def / r)
  (initget 6)
  (setq r (getreal (strcat msg " <" (rtos def 2 3) ">: ")))
  (if r r def)
)

;; 번호 목록에서 고르기. 고른 문자열 반환 (취소 시 nil)
(defun al:choose (title items def / i n r)
  (princ (strcat "\n" title))
  (setq i 0)
  (foreach it items
    (setq i (1+ i))
    (princ (strcat "\n  " (al:lpad (itoa i) 3) ". " it))
  )
  (setq n (vl-position def items))
  (while (null r)
    (initget 6)
    (setq r (getint (strcat "\n번호 선택" (if n (strcat " <" (itoa (1+ n)) ">") "") ": ")))
    (cond
      ((and (null r) n) (setq r (1+ n)))
      ((null r) (princ "\n번호를 입력하세요."))
      ((> r (length items)) (princ "\n범위를 벗어났습니다.") (setq r nil))
    )
  )
  (nth (1- r) items)
)

;;; --- 선택 도우미 ---------------------------------------------------

(defun al:ss->list (ss / i l)
  (if ss
    (progn
      (setq i (sslength ss))
      (while (> i 0)
        (setq i (1- i) l (cons (ssname ss i) l))
      )
    )
  )
  l
)

(defun al:list->ss (l / ss)
  (setq ss (ssadd))
  (foreach e l (ssadd e ss))
  ss
)

;; 객체 선택. Enter 를 누르면 현재 공간 전체 (filter 적용)
(defun al:select-or-all (msg filter / ss)
  (princ msg)
  (setq ss (if filter (ssget filter) (ssget)))
  (if (null ss)
    (progn
      (princ "\n선택이 없어 현재 공간 전체를 대상으로 합니다.")
      (setq ss (ssget "X" (append filter (list (cons 410 (getvar "CTAB"))))))
    )
  )
  ss
)

;;; --- 객체 정보 -----------------------------------------------------

(defun al:bbox (e / mn mx)
  (if (and (al:activex-p)
           (not (vl-catch-all-error-p
                  (vl-catch-all-apply 'vla-GetBoundingBox
                    (list (vlax-ename->vla-object e) 'mn 'mx)))))
    (list (vlax-safearray->list mn) (vlax-safearray->list mx))
  )
)

;; 동적 블록이면 원래 이름
(defun al:block-name (e / r)
  (if (al:activex-p)
    (setq r (al:catch 'vla-get-EffectiveName (list (vlax-ename->vla-object e))))
  )
  (if r r (cdr (assoc 2 (entget e))))
)

(defun al:mtext-string (ed / s)
  (setq s "")
  (foreach p ed (if (= (car p) 3) (setq s (strcat s (cdr p)))))
  (strcat s (cdr (assoc 1 ed)))
)

;; ed 의 내용(1/3 그룹)을 str 로 바꾼 새 엔티티 목록
(defun al:mtext-set (ed str / chunks new out done)
  (setq chunks (al:str-chunks str 250))
  (foreach c (reverse (cdr (reverse chunks))) (setq new (cons (cons 3 c) new)))
  (setq new (reverse (cons (cons 1 (last chunks)) new)))
  (foreach p ed
    (if (member (car p) '(1 3))
      (if (not done) (setq out (append (reverse new) out) done T))
      (setq out (cons p out))
    )
  )
  (reverse out)
)

(defun al:ent-text (ed / typ)
  (setq typ (cdr (assoc 0 ed)))
  (cond
    ((= typ "MTEXT") (al:mtext-string ed))
    ((member typ '("TEXT" "ATTRIB" "ATTDEF" "DIMENSION")) (cdr (assoc 1 ed)))
  )
)

(defun al:ent-text-set (ed str)
  (if (= (cdr (assoc 0 ed)) "MTEXT")
    (entmod (al:mtext-set ed str))
    (entmod (subst (cons 1 str) (assoc 1 ed) ed))
  )
)

(defun al:curve-length (e / r)
  (setq r (vl-catch-all-apply
            '(lambda () (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))
            nil))
  (if (or (vl-catch-all-error-p r) (not (numberp r))) nil r)
)

(defun al:closed-p (e / typ sp ep)
  (setq typ (cdr (assoc 0 (entget e))))
  (cond
    ((= typ "CIRCLE") T)
    ((al:catch 'vlax-curve-isClosed (list e)) T)
    ((member typ '("LWPOLYLINE" "POLYLINE" "SPLINE" "ELLIPSE"))
     (setq sp (al:catch 'vlax-curve-getStartPoint (list e))
           ep (al:catch 'vlax-curve-getEndPoint (list e)))
     (and sp ep (equal sp ep 1e-6) (> (cond ((al:curve-length e)) (0.0)) 0.0)))
  )
)

(defun al:ent-area (e / typ)
  (setq typ (cdr (assoc 0 (entget e))))
  (cond
    ((member typ '("HATCH" "REGION"))
     (if (al:activex-p) (al:catch 'vla-get-Area (list (vlax-ename->vla-object e)))))
    ((member typ '("CIRCLE" "ELLIPSE" "LWPOLYLINE" "POLYLINE" "SPLINE"))
     (if (al:closed-p e) (al:catch 'vlax-curve-getArea (list e))))
  )
)

;;; --- 좌표 ----------------------------------------------------------

(defun al:rot (p a)
  (list (- (* (car p) (cos a)) (* (cadr p) (sin a)))
        (+ (* (car p) (sin a)) (* (cadr p) (cos a))))
)
(defun al:v+ (a b) (list (+ (car a) (car b)) (+ (cadr a) (cadr b))))
(defun al:v- (a b) (list (- (car a) (car b)) (- (cadr a) (cadr b))))
(defun al:mid (a b) (list (/ (+ (car a) (car b)) 2.0) (/ (+ (cadr a) (cadr b)) 2.0)))
(defun al:p3 (p) (list (car p) (cadr p) 0.0))

;;; --- 결과 출력 (명령행 / CSV / 도면 표) ----------------------------

(defun al:text-width (s h / tb)
  (if (= s "")
    0.0
    (progn
      (setq tb (al:catch 'textbox (list (list (cons 1 s) (cons 40 h) (cons 7 (getvar "TEXTSTYLE"))))))
      (if tb (- (car (cadr tb)) (car (car tb))) (* (strlen s) h 0.8))
    )
  )
)

(defun al:make-line (p1 p2)
  (entmake (list '(0 . "LINE") (cons 10 (al:p3 p1)) (cons 11 (al:p3 p2))))
)

;; just: 0 = 왼쪽, 2 = 오른쪽 (세로 중앙 정렬)
(defun al:make-text (pt h s just)
  (entmake (list '(0 . "TEXT") (cons 10 (al:p3 pt)) (cons 11 (al:p3 pt))
                 (cons 40 h) (cons 1 s) (cons 7 (getvar "TEXTSTYLE"))
                 (cons 72 just) '(73 . 2)))
)

(defun al:draw-table (pt h rows / widths w x y rh ncol i r)
  (setq ncol (length (car rows)) rh (* h 2.0))
  (setq i 0)
  (repeat ncol
    (setq w 0.0)
    (foreach r rows (setq w (max w (al:text-width (nth i r) h))))
    (setq widths (cons (+ w (* h 2.0)) widths) i (1+ i))
  )
  (setq widths (reverse widths) w (apply '+ widths))
  (setq y (cadr pt))
  (foreach r rows
    (al:make-line (list (car pt) y) (list (+ (car pt) w) y))
    (setq x (car pt) i 0)
    (foreach cw widths
      (if (= i 0)
        (al:make-text (list (+ x h) (- y (/ rh 2.0))) h (nth i r) 0)
        (al:make-text (list (- (+ x cw) h) (- y (/ rh 2.0))) h (nth i r) 2)
      )
      (setq x (+ x cw) i (1+ i))
    )
    (setq y (- y rh))
  )
  (al:make-line (list (car pt) y) (list (+ (car pt) w) y))
  (setq x (car pt))
  (al:make-line (list x (cadr pt)) (list x y))
  (foreach cw widths
    (setq x (+ x cw))
    (al:make-line (list x (cadr pt)) (list x y))
  )
)

(defun al:write-csv (fname rows / f)
  (if (setq f (open fname "w"))
    (progn
      (foreach r rows (write-line (al:str-join (mapcar 'al:csv-field r) ",") f))
      (close f)
      T
    )
  )
)

(defun al:dwg-base () (vl-filename-base (getvar "DWGNAME")))

;; rows 첫 줄은 제목 행. 명령행에 표시하고 내보내기 방법을 묻는다
(defun al:report (title rows / widths i w out fname pt h)
  (setq i 0)
  (repeat (length (car rows))
    (setq w 0)
    (foreach r rows (setq w (max w (strlen (nth i r)))))
    (setq widths (cons w widths) i (1+ i))
  )
  (setq widths (reverse widths))
  (princ (strcat "\n\n=== " title " ==="))
  (foreach r rows
    (princ "\n")
    (setq i 0)
    (foreach cw widths
      (princ (if (= i 0) (al:pad (nth i r) cw) (strcat "  " (al:lpad (nth i r) cw))))
      (setq i (1+ i))
    )
  )
  (princ "\n")
  (setq out (al:kword "\n결과 내보내기 - 도면에 표(T)/CSV 파일(C)/안 함(N) [Table/Csv/None] <None>: "
                      "Table Csv None" "None"))
  (cond
    ((= out "Csv")
     (setq fname (getfiled "CSV 파일 저장" (strcat (getvar "DWGPREFIX") (al:dwg-base) "_" title ".csv") "csv" 1))
     (if fname
       (if (al:write-csv fname rows)
         (princ (strcat "\nCSV 저장 완료: " fname))
         (princ "\nCSV 파일을 쓸 수 없습니다. (파일이 열려 있는지 확인)")
       )
     ))
    ((= out "Table")
     (setq h (al:getreal-def "\n표 문자 높이" (atof (al:cfg-get "TableHeight" "2.5"))))
     (al:cfg-set "TableHeight" (rtos h 2 3))
     (if (setq pt (getpoint "\n표 삽입 위치(왼쪽 위): "))
       (al:draw-table pt h rows)
     ))
  )
)

(defun al:fmt (x prec) (rtos x 2 prec))
