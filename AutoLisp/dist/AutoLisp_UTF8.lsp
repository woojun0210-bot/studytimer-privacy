;;; ==================================================================
;;; AutoLisp - CAD 작업 도우미 (출력, 찾기/바꾸기, 정리, 수량, 문자 변환)
;;; AutoCAD / GstarCAD / ZWCAD / BricsCAD 공용 AutoLISP
;;; 이 파일은 build.py 로 생성됩니다. 수정은 src/ 폴더에서 하세요.
;;; ==================================================================

;;;; ---- AL_String.lsp ----
;;; ------------------------------------------------------------------
;;; AL_String.lsp - 문자열 처리 공통 함수 (CAD 객체에 의존하지 않음)
;;; ------------------------------------------------------------------

;; ANSI(DBCS) 모드이면 한글 1자가 2바이트로 계산된다.
(setq al:*dbcs* (= (strlen "가") 2))

;; 문자열 끝의 백슬래시 개수가 홀수이면 T (다음 문자가 이스케이프된 상태)
(defun al:odd-bs-end-p (s / n i)
  (setq n 0 i (strlen s))
  (while (and (> i 0) (= (substr s i 1) "\\"))
    (setq n (1+ n) i (1- i))
  )
  (= (rem n 2) 1)
)

;; 문자열 치환. cs=T 대소문자 구분, mt=T 이면 MTEXT 서식 코드(\P 등)는 건드리지 않음
;; 반환: (새문자열 치환개수)
(defun al:str-replace (str old new cs mt / out cnt lo s o pos)
  (setq out "" cnt 0 lo (strlen old))
  (if (= lo 0)
    (list str 0)
    (progn
      (setq s (if cs str (strcase str))
            o (if cs old (strcase old))
      )
      (while (setq pos (vl-string-search o s))
        (if (and mt (al:odd-bs-end-p (strcat out (substr str 1 pos))))
          (setq out (strcat out (substr str 1 (1+ pos)))
                str (substr str (+ pos 2))
                s   (substr s (+ pos 2))
          )
          (setq out (strcat out (substr str 1 pos) new)
                cnt (1+ cnt)
                str (substr str (+ pos lo 1))
                s   (substr s (+ pos lo 1))
          )
        )
      )
      (list (strcat out str) cnt)
    )
  )
)

(defun al:str-split (str delim / pos lst dl)
  (setq dl (strlen delim))
  (while (setq pos (vl-string-search delim str))
    (setq lst (cons (substr str 1 pos) lst)
          str (substr str (+ pos dl 1))
    )
  )
  (reverse (cons str lst))
)

(defun al:str-join (lst delim / r)
  (if lst
    (progn
      (setq r (car lst))
      (foreach s (cdr lst) (setq r (strcat r delim s)))
      r
    )
    ""
  )
)

(defun al:str-trim (s) (vl-string-trim " \t\r\n" s))

(defun al:pad (s n)
  (while (< (strlen s) n) (setq s (strcat s " ")))
  s
)

(defun al:lpad (s n)
  (while (< (strlen s) n) (setq s (strcat " " s)))
  s
)

;; DBCS 문자를 쪼개지 않는 n 이하의 자르기 위치
(defun al:safe-cut (str n / i)
  (if (not al:*dbcs*)
    n
    (progn
      (setq i 1)
      (while (<= i n)
        (if (> (ascii (substr str i 1)) 127)
          (setq i (+ i 2))
          (setq i (1+ i))
        )
      )
      (if (= i (+ n 2)) (1- n) n)
    )
  )
)

;; MTEXT 그룹코드(3/1)용으로 n 글자 단위로 나눈다
(defun al:str-chunks (str n / l k)
  (while (> (strlen str) n)
    (setq k (al:safe-cut str n)
          l (cons (substr str 1 k) l)
          str (substr str (1+ k))
    )
  )
  (reverse (cons str l))
)

(defun al:find-char (str ch start / p)
  (setq p (vl-string-search ch str (1- start)))
  (if p (1+ p))
)

;; 스택 문자 \S1^2; -> 1/2
(defun al:mt-stack (s)
  (setq s (car (al:str-replace s "^" "/" T nil))
        s (car (al:str-replace s "#" "/" T nil))
  )
  (al:str-trim s)
)

;; MTEXT 내용에서 서식 코드를 제거하고 문단 목록으로 돌려준다
(defun al:mt-paragraphs (str / i n c nx j out paras)
  (setq i 1 n (strlen str) out "")
  (while (<= i n)
    (setq c (substr str i 1))
    (cond
      ((or (= c "{") (= c "}")) (setq i (1+ i)))
      ((and (= c "\\") (< i n))
       (setq nx (substr str (1+ i) 1))
       (cond
         ((member nx '("P" "N" "X"))
          (setq paras (cons out paras) out "" i (+ i 2)))
         ((member nx '("\\" "{" "}"))
          (setq out (strcat out nx) i (+ i 2)))
         ((= nx "~")
          (setq out (strcat out " ") i (+ i 2)))
         ((member nx '("L" "l" "O" "o" "K" "k"))
          (setq i (+ i 2)))
         ((= nx "S")
          (setq j (al:find-char str ";" (+ i 2)))
          (if j
            (setq out (strcat out (al:mt-stack (substr str (+ i 2) (- j i 2))))
                  i (1+ j))
            (setq out (strcat out (al:mt-stack (substr str (+ i 2))))
                  i (1+ n))
          ))
         ((member nx '("f" "F" "H" "W" "Q" "T" "A" "C" "c" "p"))
          (setq j (al:find-char str ";" (+ i 2))
                i (if j (1+ j) (1+ n))))
         ;; \U+XXXX, \M+ 등은 TEXT 에서도 쓰이므로 그대로 둔다
         (T (setq out (strcat out c nx) i (+ i 2)))
       ))
      (T (setq out (strcat out c) i (1+ i)))
    )
  )
  (reverse (cons out paras))
)

(defun al:mt-plain (str) (al:str-join (al:mt-paragraphs str) " "))

;; 일반 문자열을 MTEXT 내용으로 쓸 수 있게 이스케이프
(defun al:mt-escape (str / i n c out)
  (setq i 1 n (strlen str) out "")
  (while (<= i n)
    (setq c (substr str i 1))
    (cond
      ((or (= c "{") (= c "}")) (setq out (strcat out "\\" c)))
      ((= c "\\")
       (if (member (substr str (1+ i) 2) '("U+" "M+"))
         (setq out (strcat out c))
         (setq out (strcat out "\\\\"))
       ))
      (T (setq out (strcat out c)))
    )
    (setq i (1+ i))
  )
  out
)

(defun al:digit-p (c) (and (= (strlen c) 1) (<= 48 (ascii c) 57)))

;; 문자열에서 첫 번째 숫자를 찾아 실수로 돌려준다 (천단위 쉼표 무시). 없으면 nil
(defun al:str-number (str / i n c num started dot done)
  (setq str (car (al:str-replace str "," "" T nil))
        i 1 n (strlen str) num "")
  (while (and (<= i n) (not done))
    (setq c (substr str i 1))
    (cond
      ((al:digit-p c) (setq num (strcat num c) started T))
      ((and (= c ".") (not dot) (al:digit-p (substr str (1+ i) 1)))
       (setq num (strcat num c) dot T))
      ((and (= c "-") (not started) (= num "")
            (or (al:digit-p (substr str (1+ i) 1)) (= (substr str (1+ i) 1) ".")))
       (setq num "-"))
      (started (setq done T))
      (T (setq num "" dot nil))
    )
    (setq i (1+ i))
  )
  (if started (atof num))
)

;; "1,3-5" -> (1 3 4 5), 범위를 벗어난 번호는 제외
(defun al:parse-ranges (str maxn / out part p a b)
  (foreach part (al:str-split str ",")
    (setq part (al:str-trim part))
    (if (setq p (vl-string-search "-" part))
      (progn
        (setq a (atoi (substr part 1 p)) b (atoi (substr part (+ p 2))))
        (if (> a b) (setq p a a b b p))
        (while (<= a b)
          (if (and (>= a 1) (<= a maxn) (not (member a out))) (setq out (cons a out)))
          (setq a (1+ a))
        )
      )
      (progn
        (setq a (atoi part))
        (if (and (>= a 1) (<= a maxn) (not (member a out))) (setq out (cons a out)))
      )
    )
  )
  (reverse out)
)

;; 집계 목록에 값 더하기: acc = ((key cnt len area) ...)
(defun al:acc-add (acc key cnt len area / rec)
  (if (setq rec (assoc key acc))
    (subst (list key (+ (nth 1 rec) cnt) (+ (nth 2 rec) len) (+ (nth 3 rec) area)) rec acc)
    (cons (list key cnt len area) acc)
  )
)

;; 항목 ((pt . data) ...) 을 행 단위로 묶는다: 위->아래, 행 안에서는 좌->우
(defun al:group-rows (items tol / sorted rows row y0)
  (setq sorted (vl-sort items '(lambda (a b) (> (cadr (car a)) (cadr (car b))))))
  (foreach it sorted
    (if (and row (<= (abs (- y0 (cadr (car it)))) tol))
      (setq row (cons it row))
      (progn
        (if row (setq rows (cons row rows)))
        (setq row (list it) y0 (cadr (car it)))
      )
    )
  )
  (if row (setq rows (cons row rows)))
  (mapcar '(lambda (r) (vl-sort r '(lambda (a b) (< (car (car a)) (car (car b))))))
          (reverse rows))
)

;; 순서 정렬. mode "Z" = 행 우선(좌->우, 위->아래), "N" = 열 우선(위->아래, 좌->우)
(defun al:sort-order (items tol mode)
  (if (= mode "N")
    (mapcar 'cdr
      (apply 'append
        (al:group-rows
          (mapcar '(lambda (it) (cons (list (- (cadr (car it))) (- (car (car it)))) it)) items)
          tol)))
    (apply 'append (al:group-rows items tol))
  )
)

(defun al:csv-field (s)
  (if (or (vl-string-search "," s) (vl-string-search "\"" s))
    (strcat "\"" (car (al:str-replace s "\"" "\"\"" T nil)) "\"")
    s
  )
)

;;;; ---- AL_Match.lsp ----
;;; ------------------------------------------------------------------
;;; AL_Match.lsp - 찾기 패턴 처리 (CAD 객체에 의존하지 않음)
;;;
;;;   와일드카드 사용 시   *   : 아무 문자 여러 개
;;;                        #   : 숫자 한 개
;;;                        {*} : 아무 문자 여러 개, 이 부분만 바꿀 문자로 치환
;;;   항상                 찾을문자(?!제외1|제외2) : 바로 뒤에 제외 문자가 오면 제외
;;; ------------------------------------------------------------------

;; s 의 i 번째 위치 문자의 길이 (ANSI 모드의 한글은 2)
(defun al:char-len (s i)
  (if (and al:*dbcs* (> (ascii (substr s i 1)) 127)) 2 1)
)

(defun al:remove-spaces (s) (car (al:str-replace s " " "" T nil)))

;; 마지막으로 나오는 위치 (0 부터), 없으면 nil
(defun al:str-last-pos (pat str / p r)
  (setq p 0)
  (while (setq p (vl-string-search pat str p))
    (setq r p p (1+ p))
  )
  r
)

;; 패턴 해석. 반환: (토큰목록 제외목록 전체일치)
;;   토큰: 문자열(그대로 비교) / D(숫자 1개) / S(*) / C({*}) / E(끝)
(defun al:pm-compile (pat wild whole cs / p excl toks lit i n c)
  (if (and (= (substr pat (strlen pat)) ")")
           (setq p (al:str-last-pos "(?!" pat)))
    (setq excl (vl-remove "" (al:str-split (substr pat (+ p 4) (- (strlen pat) p 4)) "|"))
          pat (substr pat 1 p))
  )
  (if (not cs)
    (setq pat (strcase pat) excl (mapcar 'strcase excl))
  )
  (if wild
    (progn
      (setq i 1 n (strlen pat) lit "")
      (while (<= i n)
        (setq c (substr pat i 1))
        (cond
          ((= (substr pat i 3) "{*}")
           (if (/= lit "") (setq toks (cons lit toks) lit ""))
           (setq toks (cons 'C toks) i (+ i 3)))
          ((member c '("*" "#"))
           (if (/= lit "") (setq toks (cons lit toks) lit ""))
           (setq toks (cons (if (= c "*") 'S 'D) toks) i (1+ i)))
          (T
           (setq c (substr pat i (al:char-len pat i))
                 lit (strcat lit c)
                 i (+ i (strlen c))))
        )
      )
      (if (/= lit "") (setq toks (cons lit toks)))
      (setq toks (reverse toks))
    )
    (if (/= pat "") (setq toks (list pat)))
  )
  (if (and toks whole) (setq toks (append toks (list 'E))))
  (list toks excl whole)
)

;; 위치 i 에서 토큰이 맞으면 (끝위치 캡처목록), 아니면 nil.  캡처: ((시작 끝) ...) 역순
(defun al:pm-match (toks s i n caps / tk r ps j)
  (if (null toks)
    (list i caps)
    (progn
      (setq tk (car toks))
      (cond
        ((= (type tk) 'STR)
         (if (= (substr s i (strlen tk)) tk)
           (al:pm-match (cdr toks) s (+ i (strlen tk)) n caps)
         ))
        ((eq tk 'E)
         (if (= i (1+ n)) (al:pm-match (cdr toks) s i n caps)))
        ((eq tk 'D)
         (if (and (<= i n) (al:digit-p (substr s i 1)))
           (al:pm-match (cdr toks) s (1+ i) n caps)
         ))
        (T
         ;; * , {*} : 가장 길게 먼저 시도
         (setq j i)
         (while (<= j n)
           (setq ps (cons j ps) j (+ j (al:char-len s j)))
         )
         (setq ps (cons (1+ n) ps))
         (while (and ps (null r))
           (setq r (al:pm-match (cdr toks) s (car ps) n
                     (if (eq tk 'C) (cons (list i (car ps)) caps) caps))
                 ps (cdr ps))
         )
         r)
      )
    )
  )
)

(defun al:pm-excluded-p (excl s e)
  (vl-some '(lambda (x) (= (substr s e (strlen x)) x)) excl)
)

;; start 위치부터 찾기. 반환 (시작 끝 캡처목록) 또는 nil. s 는 이미 대소문자 처리된 문자열
(defun al:pm-find (cp s start / toks excl whole n i r)
  (setq toks (car cp) excl (cadr cp) whole (caddr cp) n (strlen s) i start)
  (if toks
    (while (and (null r) (<= i (1+ n)) (or (not whole) (= i 1)))
      (setq r (al:pm-match toks s i n nil))
      (if (and r (or (= (car r) i) (al:pm-excluded-p excl s (car r))))
        (setq r nil)
      )
      (if r (setq r (cons i r)))
      (setq i (+ i (if (<= i n) (al:char-len s i) 1)))
    )
  )
  r
)

(defun al:pm-test (cp s cs)
  (if (al:pm-find cp (if cs s (strcase s)) 1) T)
)

;; 찾아 바꾸기. 반환 (새문자열 바꾼개수)
(defun al:pm-replace (cp s repl cs / ms out pos m st en caps cur cnt k)
  (setq ms (if cs s (strcase s)) out "" pos 1 cnt 0)
  (while (setq m (al:pm-find cp ms pos))
    (setq st (car m) en (cadr m) caps (reverse (caddr m)))
    (if (vl-some '(lambda (c) (= (car c) (cadr c))) caps)
      ;; {*} 부분이 비어 있으면 바꾸지 않는다
      (setq k (al:char-len s st)
            out (strcat out (substr s pos (- (+ st k) pos)))
            pos (+ st k))
      (progn
        (setq out (strcat out (substr s pos (- st pos))) cur st)
        (if caps
          (progn
            (foreach c caps
              (setq out (strcat out (substr s cur (- (car c) cur)) repl) cur (cadr c))
            )
            (setq out (strcat out (substr s cur (- en cur))))
          )
          (setq out (strcat out repl))
        )
        (setq cnt (1+ cnt) pos en)
      )
    )
  )
  (list (strcat out (substr s pos)) cnt)
)

;; 숫자를 불필요한 0 없이 문자열로 (엑셀 값 변환용)
(defun al:num->str (x / s)
  (if (and (< (abs x) 2.0e9) (equal x (fix x) 1e-9))
    (itoa (fix x))
    (progn
      (setq s (rtos x 2 8))
      (while (= (substr s (strlen s)) "0") (setq s (substr s 1 (1- (strlen s)))))
      (if (= (substr s (strlen s)) ".") (setq s (substr s 1 (1- (strlen s)))))
      s
    )
  )
)

;;;; ---- AL_Core.lsp ----
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

;;;; ---- AL_Plot.lsp ----
;;; ------------------------------------------------------------------
;;; AL_Plot.lsp - 출력(Plot)
;;;   ALPLOT  : 도곽(블록/사각형/창 지정) 일괄 출력, PDF 장별 저장
;;;   ALPLOTL : 레이아웃 일괄 출력
;;; ------------------------------------------------------------------

(defun al:vlist (v) (vlax-safearray->list (vlax-variant-value v)))

(defun al:pt2d (p / sa)
  (setq sa (vlax-make-safearray vlax-vbDouble '(0 . 1)))
  (vlax-safearray-fill sa (list (car p) (cadr p)))
  sa
)

(defun al:media-name (lay m / r)
  (setq r (al:catch 'vla-GetLocaleMediaName (list lay m)))
  (if r r m)
)

(defun al:pdf-device-p (dev) (wcmatch (strcase dev) "*PDF*"))

(defun al:dir-slash (d)
  (if (and (/= d "") (not (wcmatch d "*\\,*/"))) (strcat d "\\") d)
)

;; 출력 장치/용지/플롯 스타일 선택. 반환 (장치 용지 스타일) 또는 nil
(defun al:plot-setup (lay / devs dev media names filt sel ctbs ctb)
  (vla-RefreshPlotDeviceInfo lay)
  (setq devs (vl-remove-if '(lambda (d) (= (strcase d) "NONE")) (al:vlist (vla-GetPlotDeviceNames lay))))
  (setq dev (al:choose "출력 장치 목록:" devs (al:cfg-get "PlotDevice" (vla-get-ConfigName lay))))
  (al:cfg-set "PlotDevice" dev)
  (vla-put-ConfigName lay dev)
  (vla-RefreshPlotDeviceInfo lay)
  ;; 용지
  (setq media (al:vlist (vla-GetCanonicalMediaNames lay)))
  (setq filt (getstring T (strcat "\n용지 이름 필터 (예: A3, A1) <" (al:cfg-get "PlotPaperFilter" "A3") ">: ")))
  (if (= filt "") (setq filt (al:cfg-get "PlotPaperFilter" "A3")))
  (al:cfg-set "PlotPaperFilter" filt)
  (setq sel (vl-remove-if-not
              '(lambda (m) (wcmatch (strcase (al:media-name lay m)) (strcat "*" (strcase filt) "*")))
              media))
  (if (null sel) (progn (princ "\n필터에 맞는 용지가 없어 전체 목록을 표시합니다.") (setq sel media)))
  (setq names (mapcar '(lambda (m) (al:media-name lay m)) sel))
  (setq media (nth (vl-position
                     (al:choose "용지 목록:" names (al:media-name lay (al:cfg-get "PlotMedia" "")))
                     names)
                   sel))
  (al:cfg-set "PlotMedia" media)
  ;; 플롯 스타일
  (setq ctbs (al:vlist (vla-GetPlotStyleTableNames lay)))
  (setq ctb (if ctbs (al:choose "플롯 스타일 목록:" ctbs (al:cfg-get "PlotStyle" "monochrome.ctb"))))
  (if ctb (al:cfg-set "PlotStyle" ctb))
  (list dev media ctb)
)

(defun al:plot-apply (lay setup)
  (vla-put-ConfigName lay (car setup))
  (vla-RefreshPlotDeviceInfo lay)
  (vla-put-CanonicalMediaName lay (cadr setup))
  (if (caddr setup)
    (progn
      (al:catch 'vla-put-StyleSheet (list lay (caddr setup)))
      (al:catch 'vla-put-PlotWithPlotStyles (list lay :vlax-true))
    )
  )
  (al:catch 'vla-put-PaperUnits (list lay 1))          ; 1 = mm
  (al:catch 'vla-put-UseStandardScale (list lay :vlax-true))
  (al:catch 'vla-put-StandardScale (list lay 0))       ; 0 = 용지에 맞춤
  (al:catch 'vla-put-CenterPlot (list lay :vlax-true))
)

;; 창 크기와 용지 방향을 비교해 회전값 결정 (0 = 0도, 1 = 90도)
(defun al:plot-rotation (lay ll ur orient / pw ph wide)
  (cond
    ((= orient "Normal") 0)
    ((= orient "Rotate") 1)
    (T
     (setq wide (> (- (car ur) (car ll)) (- (cadr ur) (cadr ll))))
     (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-GetPaperSize (list lay 'pw 'ph)))
       0
       (if (eq wide (> pw ph)) 0 1)
     ))
  )
)

;; 한 영역 출력. fname 이 있으면 파일로 출력
(defun al:plot-window (doc lay ll ur orient fname / plot r)
  (vla-put-PlotRotation lay (al:plot-rotation lay ll ur orient))
  (vla-SetWindowToPlot lay (al:pt2d ll) (al:pt2d ur))
  (vla-put-PlotType lay 4)                             ; 4 = 창(Window)
  (setq plot (vla-get-Plot doc))
  (setq r (if fname
            (vl-catch-all-apply 'vla-PlotToFile (list plot fname))
            (vl-catch-all-apply 'vla-PlotToDevice (list plot))))
  (not (or (vl-catch-all-error-p r) (= r :vlax-false)))
)

(defun al:collect-blocks (/ e name scope ss out)
  (while (and (setq e (entsel "\n도곽 블록 하나 선택: "))
              (/= (cdr (assoc 0 (entget (car e)))) "INSERT"))
    (princ "\n블록이 아닙니다.")
  )
  (if e
    (progn
      (setq name (strcase (al:block-name (car e))))
      (princ (strcat "\n도곽 블록 이름: " name))
      (setq scope (al:kword "\n대상 - 현재 공간 전체(A)/직접 선택(S) [All/Select] <All>: " "All Select" "All"))
      (setq ss (if (= scope "All")
                 (ssget "X" (list '(0 . "INSERT") (cons 410 (getvar "CTAB"))))
                 (ssget '((0 . "INSERT")))))
      (foreach b (al:ss->list ss)
        (if (= (strcase (al:block-name b)) name) (setq out (cons (al:bbox b) out)))
      )
    )
  )
  (vl-remove nil out)
)

(defun al:collect-rects (/ e lay scope ss out ed)
  (while (and (setq e (entsel "\n도곽 사각형(폴리선) 하나 선택: "))
              (/= (cdr (assoc 0 (entget (car e)))) "LWPOLYLINE"))
    (princ "\n폴리선이 아닙니다.")
  )
  (if e
    (progn
      (setq lay (cdr (assoc 8 (entget (car e)))))
      (princ (strcat "\n도곽 레이어: " lay))
      (setq scope (al:kword "\n대상 - 현재 공간 전체(A)/직접 선택(S) [All/Select] <All>: " "All Select" "All"))
      (setq ss (if (= scope "All")
                 (ssget "X" (list '(0 . "LWPOLYLINE") (cons 8 lay) (cons 410 (getvar "CTAB"))))
                 (ssget (list '(0 . "LWPOLYLINE") (cons 8 lay)))))
      (foreach p (al:ss->list ss)
        (setq ed (entget p))
        (if (or (= 1 (logand 1 (cdr (assoc 70 ed)))) (member (cdr (assoc 90 ed)) '(4 5)))
          (setq out (cons (al:bbox p) out))
        )
      )
    )
  )
  (vl-remove nil out)
)

(defun al:collect-windows (/ p1 p2 out)
  (while (setq p1 (getpoint "\n출력 영역 첫 번째 구석 (Enter=끝): "))
    (if (setq p2 (getcorner p1 "\n반대쪽 구석: "))
      (setq out (cons (list (list (min (car p1) (car p2)) (min (cadr p1) (cadr p2)))
                            (list (max (car p1) (car p2)) (max (cadr p1) (cadr p2))))
                      out))
    )
  )
  (reverse out)
)

(defun c:ALPLOT (/ doc lay mode areas order tol items setup orient pdf folder bg i ok fname)
  (if (not (al:activex-p))
    (princ "\n출력 기능은 Windows 용 CAD(ActiveX 지원)에서만 사용할 수 있습니다.")
    (progn
      (al:begin)
      (setq doc (al:doc) lay (vla-get-ActiveLayout doc))
      (setq mode (al:kword "\n출력 영역 - 도곽 블록(B)/사각형 폴리선(R)/창 지정(W) [Block/Rect/Window] <Block>: "
                           "Block Rect Window" "Block"))
      (setq areas (cond ((= mode "Block") (al:collect-blocks))
                        ((= mode "Rect") (al:collect-rects))
                        (T (al:collect-windows))))
      (if (null areas)
        (princ "\n출력할 영역이 없습니다.")
        (progn
          (princ (strcat "\n출력 영역 " (itoa (length areas)) "개"))
          (if (and (> (length areas) 1) (/= mode "Window"))
            (progn
              (setq order (al:kword "\n출력 순서 - 좌→우 먼저(Z)/위→아래 먼저(N) [Z/N] <Z>: " "Z N" (al:cfg-get "PlotOrder" "Z")))
              (al:cfg-set "PlotOrder" order)
              (setq tol (* 0.3 (apply 'min (mapcar '(lambda (a) (- (cadr (cadr a)) (cadr (car a)))) areas))))
              (setq items (mapcar '(lambda (a) (cons (al:mid (car a) (cadr a)) a)) areas))
              (setq areas (mapcar 'cdr (al:sort-order items tol order)))
            )
          )
          (setq setup (al:plot-setup lay))
          (setq orient (al:kword "\n용지 방향 - 자동(A)/그대로(N)/90도 회전(R) [Auto/Normal/Rotate] <Auto>: "
                                 "Auto Normal Rotate" "Auto"))
          (if (setq pdf (al:pdf-device-p (car setup)))
            (progn
              (setq folder (getstring T (strcat "\nPDF 저장 폴더 <" (getvar "DWGPREFIX") ">: ")))
              (if (= folder "") (setq folder (getvar "DWGPREFIX")))
              (setq folder (al:dir-slash folder))
            )
          )
          (if (al:yes-p (strcat "\n" (itoa (length areas)) "장을 출력합니다. 계속할까요?") "Yes")
            (progn
              (setq bg (getvar "BACKGROUNDPLOT"))
              (if bg (setvar "BACKGROUNDPLOT" 0))
              (al:plot-apply lay setup)
              (setq i 0 ok 0)
              (foreach a areas
                (setq i (1+ i))
                (if pdf
                  (setq fname (strcat folder (al:dwg-base) "_" (if (< i 10) "0" "") (itoa i) ".pdf"))
                )
                (princ (strcat "\n출력 중 " (itoa i) "/" (itoa (length areas)) (if fname (strcat " → " fname) "")))
                (if (al:plot-window doc lay (car a) (cadr a) orient fname)
                  (setq ok (1+ ok))
                  (princ " ... 실패")
                )
              )
              (if bg (setvar "BACKGROUNDPLOT" bg))
              (princ (strcat "\n출력 완료: " (itoa ok) "/" (itoa (length areas)) "장"))
            )
          )
        )
      )
      (al:end)
    )
  )
  (princ)
)

(defun c:ALPLOTL (/ doc lays sel cur bg ok fname folder l i)
  (if (not (al:activex-p))
    (princ "\n출력 기능은 Windows 용 CAD(ActiveX 지원)에서만 사용할 수 있습니다.")
    (progn
      (al:begin)
      (setq doc (al:doc) cur (vla-get-ActiveLayout doc))
      (vlax-for l (vla-get-Layouts doc)
        (if (/= (vla-get-ModelType l) :vlax-true) (setq lays (cons l lays)))
      )
      (setq lays (vl-sort lays '(lambda (a b) (< (vla-get-TabOrder a) (vla-get-TabOrder b)))))
      (if (null lays)
        (princ "\n레이아웃이 없습니다.")
        (progn
          (princ "\n레이아웃 목록 (각 레이아웃의 페이지 설정대로 출력):")
          (setq i 0)
          (foreach l lays
            (setq i (1+ i))
            (princ (strcat "\n  " (al:lpad (itoa i) 3) ". " (vla-get-Name l) "  [" (vla-get-ConfigName l) "]"))
          )
          (setq sel (getstring T "\n출력할 번호 (예: 1,3-5 / Enter=전체): "))
          (setq sel (if (= sel "")
                      lays
                      (mapcar '(lambda (n) (nth (1- n) lays)) (al:parse-ranges sel (length lays)))))
          (if (vl-some '(lambda (l) (al:pdf-device-p (vla-get-ConfigName l))) sel)
            (progn
              (setq folder (getstring T (strcat "\nPDF 저장 폴더 <" (getvar "DWGPREFIX") ">: ")))
              (if (= folder "") (setq folder (getvar "DWGPREFIX")))
              (setq folder (al:dir-slash folder))
            )
          )
          (setq bg (getvar "BACKGROUNDPLOT"))
          (if bg (setvar "BACKGROUNDPLOT" 0))
          (setq ok 0)
          (foreach l sel
            (vla-put-ActiveLayout doc l)
            (setq fname (if (al:pdf-device-p (vla-get-ConfigName l))
                          (strcat folder (al:dwg-base) "_" (vla-get-Name l) ".pdf")))
            (princ (strcat "\n출력 중: " (vla-get-Name l)))
            (if (not (vl-catch-all-error-p
                       (if fname
                         (vl-catch-all-apply 'vla-PlotToFile (list (vla-get-Plot doc) fname))
                         (vl-catch-all-apply 'vla-PlotToDevice (list (vla-get-Plot doc))))))
              (setq ok (1+ ok))
              (princ " ... 실패")
            )
          )
          (vla-put-ActiveLayout doc cur)
          (if bg (setvar "BACKGROUNDPLOT" bg))
          (princ (strcat "\n출력 완료: " (itoa ok) "/" (itoa (length sel)) "개 레이아웃"))
        )
      )
      (al:end)
    )
  )
  (princ)
)

;;;; ---- AL_Find.lsp ----
;;; ------------------------------------------------------------------
;;; AL_Find.lsp - 문자 찾기 / 바꾸기
;;;   -ALFIND : (명령행 버전) 문자 찾아 하나씩 확대해 보기, 찾은 객체 모두 선택
;;;   -ALFR   : (명령행 버전) 문자 찾아 바꾸기
;;;   대화상자 버전(ALFTE/ALFIND/ALFR)은 AL_FTE.lsp
;;; ------------------------------------------------------------------

(setq al:*text-types* "TEXT,MTEXT,INSERT,DIMENSION,MULTILEADER,ATTDEF")

;; 객체(속성 포함)의 문자 목록
(defun al:ent-strings (e / ed typ a out s)
  (setq ed (entget e) typ (cdr (assoc 0 ed)))
  (cond
    ((= typ "MTEXT") (list (al:mt-plain (al:mtext-string ed))))
    ((member typ '("TEXT" "ATTRIB" "ATTDEF" "DIMENSION")) (list (cdr (assoc 1 ed))))
    ((= typ "INSERT")
     (if (= (cdr (assoc 66 ed)) 1)
       (progn
         (setq a (entnext e))
         (while (and a (= (cdr (assoc 0 (entget a))) "ATTRIB"))
           (setq out (cons (cdr (assoc 1 (entget a))) out) a (entnext a))
         )
       )
     )
     out)
    ((and (= typ "MULTILEADER") (al:activex-p))
     (if (setq s (al:catch 'vla-get-TextString (list (vlax-ename->vla-object e))))
       (list (al:mt-plain s))
     ))
  )
)

(defun al:ent-contains-p (e str cs)
  (if (not cs) (setq str (strcase str)))
  (vl-some '(lambda (s) (and s (vl-string-search str (if cs s (strcase s)))))
           (al:ent-strings e))
)

;; 객체 하나에서 바꾸기. 바꾼 개수 반환. 잠긴 레이어 등으로 실패하면 al:*fail* 증가
(defun al:replace-in-ent (e old new cs / ed typ s r n a o)
  (setq ed (entget e) typ (cdr (assoc 0 ed)) n 0)
  (cond
    ((member typ '("TEXT" "MTEXT" "ATTRIB" "ATTDEF" "DIMENSION"))
     (setq s (al:ent-text ed))
     (if (and s (/= s ""))
       (progn
         (setq r (al:str-replace s old new cs (= typ "MTEXT")))
         (if (> (cadr r) 0)
           (if (al:ent-text-set ed (car r))
             (setq n (cadr r))
             (setq al:*fail* (1+ al:*fail*))
           )
         )
       )
     ))
    ((= typ "INSERT")
     (if (= (cdr (assoc 66 ed)) 1)
       (progn
         (setq a (entnext e))
         (while (and a (= (cdr (assoc 0 (entget a))) "ATTRIB"))
           (setq n (+ n (al:replace-in-ent a old new cs)) a (entnext a))
         )
         (if (> n 0) (entupd e))
       )
     ))
    ((and (= typ "MULTILEADER") (al:activex-p))
     (setq o (vlax-ename->vla-object e)
           s (al:catch 'vla-get-TextString (list o)))
     (if (and s (/= s ""))
       (progn
         (setq r (al:str-replace s old new cs T))
         (if (> (cadr r) 0)
           (if (vl-catch-all-error-p (vl-catch-all-apply 'vla-put-TextString (list o (car r))))
             (setq al:*fail* (1+ al:*fail*))
             (setq n (cadr r))
           )
         )
       )
     ))
  )
  n
)

;; 블록 정의 내부 (외부참조, 익명 블록 제외)
(defun al:replace-in-blockdefs (old new cs / b e n)
  (setq n 0 b (tblnext "BLOCK" T))
  (while b
    (if (and (/= (substr (cdr (assoc 2 b)) 1 1) "*")
             (= 0 (logand 36 (cdr (assoc 70 b)))))
      (progn
        (setq e (cdr (assoc -2 b)))
        (while e
          (setq n (+ n (al:replace-in-ent e old new cs)) e (entnext e))
        )
      )
    )
    (setq b (tblnext "BLOCK"))
  )
  n
)

(defun c:-ALFR (/ old new cs scope inblk ss total nb)
  (al:begin)
  (setq old (getstring T "\n찾을 문자: "))
  (if (= old "")
    (princ "\n찾을 문자가 없습니다.")
    (progn
      (setq new (getstring T (strcat "\n\"" old "\" 을(를) 바꿀 문자 (Enter=삭제): ")))
      (setq cs (al:yes-p "\n대소문자 구분" "No"))
      (setq scope (al:kword "\n범위 - 도면 전체(A)/선택(S) [All/Select] <Select>: " "All Select" "Select"))
      (if (= scope "All")
        (setq ss (ssget "X" (list (cons 0 al:*text-types*)))
              inblk (al:yes-p "\n블록 정의 내부 문자도 바꿀까요" "No"))
        (setq ss (ssget (list (cons 0 al:*text-types*))))
      )
      (setq total 0 nb 0 al:*fail* 0)
      (foreach e (al:ss->list ss)
        (setq total (+ total (al:replace-in-ent e old new cs)))
      )
      (if inblk
        (progn
          (setq nb (al:replace-in-blockdefs old new cs))
          (if (> nb 0) (command "_.REGEN"))
        )
      )
      (princ (strcat "\n바꾸기 완료: " (itoa (+ total nb)) "곳"
                     (if (> nb 0) (strcat " (블록 정의 내부 " (itoa nb) "곳 포함)") "")))
      (if (> al:*fail* 0)
        (princ (strcat "\n잠긴 레이어 등으로 바꾸지 못한 객체: " (itoa al:*fail*) "개"))
      )
    )
  )
  (al:end)
)

(defun al:zoom-to (e / bb pt ht ed)
  (if (setq bb (al:bbox e))
    (setq pt (al:mid (car bb) (cadr bb))
          ht (max (* 6.0 (- (cadr (cadr bb)) (cadr (car bb))))
                  (* 1.5 (- (car (cadr bb)) (car (car bb))))))
    (setq ed (entget e)
          pt (cdr (assoc 10 ed))
          ht (* 10.0 (cond ((cdr (assoc 40 ed))) (1.0))))
  )
  (if (> ht 0.0) (command "_.ZOOM" "_C" (al:p3 pt) ht))
)

(defun c:-ALFIND (/ str cs ss found n i k e)
  (al:begin)
  (setq str (getstring T "\n찾을 문자: "))
  (if (= str "")
    (princ "\n찾을 문자가 없습니다.")
    (progn
      (setq cs (al:yes-p "\n대소문자 구분" "No"))
      (setq ss (ssget "X" (list (cons 0 al:*text-types*) (cons 410 (getvar "CTAB")))))
      (foreach e (al:ss->list ss)
        (if (al:ent-contains-p e str cs) (setq found (cons e found)))
      )
      (setq found (reverse found) n (length found))
      (if (= n 0)
        (princ "\n현재 공간에서 찾은 문자가 없습니다.")
        (progn
          (princ (strcat "\n" (itoa n) "개 찾음."))
          (setq i 0 k "Next")
          (while (and (< i n) (/= k "Exit"))
            (setq e (nth i found))
            (al:zoom-to e)
            (redraw e 3)
            (initget "Next Prev Exit")
            (setq k (getkword (strcat "\n[" (itoa (1+ i)) "/" (itoa n) "] 다음(N)/이전(P)/끝내기(E) [Next/Prev/Exit] <Next>: ")))
            (redraw e 4)
            (cond
              ((= k "Prev") (setq i (max 0 (1- i))))
              ((= k "Exit"))
              (T (setq i (1+ i)))
            )
          )
          (princ (strcat "\n찾은 객체 " (itoa n) "개를 선택해 두었습니다."))
        )
      )
    )
  )
  (al:end)
  (if found (sssetfirst nil (al:list->ss found)))
  (princ)
)

;;;; ---- AL_FTE.dcl (대화상자 정의) ----
(setq al:*dcl-fte* '(
  "al_fte : dialog {"
  "  label = \"AutoLisp - 문자 찾기/바꾸기\";"
  "  : row {"
  "    : column {"
  "      : boxed_row {"
  "        label = \"검색 위치\";"
  "        : popup_list { key = \"scope\"; width = 22; }"
  "        : button { key = \"selobj\"; label = \"객체 선택 <\"; fixed_width = true; }"
  "      }"
  "      : boxed_column {"
  "        label = \"문자 찾기\";"
  "        : row {"
  "          : edit_box { key = \"ftext\"; label = \"내용\"; edit_width = 26; allow_accept = true; }"
  "          : button { key = \"fpick\"; label = \"선택<\"; fixed_width = true; }"
  "          : button { key = \"fpickm\"; label = \"여러 개<\"; fixed_width = true; }"
  "        }"
  "        : row {"
  "          : popup_list { key = \"fhist\"; label = \"최근 내용\"; width = 36; }"
  "          : button { key = \"fhclr\"; label = \"기록 삭제\"; fixed_width = true; }"
  "        }"
  "        : toggle { key = \"fwhole\"; label = \"전체 내용 일치 문자만 찾기\"; }"
  "        : toggle { key = \"fmulti\"; label = \"찾는 내용 쉼표(,) 구분해 여러 문자 찾기\"; }"
  "        : toggle { key = \"fcase\"; label = \"대소문자 구분\"; }"
  "        : button { key = \"dofind\"; label = \"찾기\"; is_default = true; }"
  "      }"
  "    }"
  "    : boxed_column {"
  "      label = \"문자 바꾸기\";"
  "      : row {"
  "        : edit_box { key = \"rfind\"; label = \"찾을 문자\"; edit_width = 22; }"
  "        : button { key = \"rpick\"; label = \"선택<\"; fixed_width = true; }"
  "      }"
  "      : edit_box { key = \"rrepl\"; label = \"바꿀 문자\"; edit_width = 22; }"
  "      : row {"
  "        : button { key = \"radd\"; label = \"목록에 추가\"; }"
  "        : button { key = \"rdel\"; label = \"선택 삭제\"; }"
  "        : button { key = \"rclr\"; label = \"전체 삭제\"; }"
  "      }"
  "      : list_box { key = \"rlist\"; height = 6; width = 50; multiple_select = true; tabs = \"24\"; }"
  "      : row {"
  "        : button { key = \"rtxt\"; label = \"텍스트 파일...\"; }"
  "        : edit_box { key = \"rdelim\"; label = \"구분 문자\"; edit_width = 3; }"
  "        : button { key = \"rxls\"; label = \"엑셀 선택 영역...\"; }"
  "      }"
  "      : row {"
  "        : popup_list { key = \"rsaved\"; label = \"저장 목록\"; width = 30; }"
  "        : button { key = \"rload\"; label = \"불러오기\"; fixed_width = true; }"
  "        : button { key = \"rsdel\"; label = \"삭제\"; fixed_width = true; }"
  "      }"
  "      : row {"
  "        : edit_box { key = \"rsname\"; label = \"저장 이름\"; edit_width = 16; }"
  "        : button { key = \"rsave\"; label = \"현재 목록 저장\"; fixed_width = true; }"
  "      }"
  "      : toggle { key = \"rwhole\"; label = \"전체 내용 일치 문자만 바꾸기\"; }"
  "      : toggle { key = \"rcase\"; label = \"대소문자 구분\"; }"
  "      : button { key = \"doreplace\"; label = \"바꾸기 실행\"; }"
  "    }"
  "  }"
  "  : boxed_column {"
  "    label = \"결과 (더블 클릭하면 확대)\";"
  "    : list_box { key = \"results\"; height = 12; width = 110; multiple_select = true; tabs = \"6 16 36\"; }"
  "    : text { key = \"status\"; width = 100; }"
  "    : row {"
  "      : button { key = \"rzoom\"; label = \"확대\"; }"
  "      : button { key = \"rsel\"; label = \"선택 항목 도면에서 선택\"; }"
  "      : button { key = \"rselall\"; label = \"찾은 문자 모두 선택\"; }"
  "      : button { key = \"rline\"; label = \"연결선 그리기\"; }"
  "      : button { key = \"rerase\"; label = \"선택 항목 삭제\"; }"
  "      : button { key = \"rremove\"; label = \"목록에서 제거\"; }"
  "    }"
  "  }"
  "  : row {"
  "    : button { key = \"settings\"; label = \"검색 설정 / 찾은 문자 처리...\"; fixed_width = true; }"
  "    : text { key = \"procinfo\"; width = 46; }"
  "    : button { key = \"cancel\"; label = \"닫기\"; is_cancel = true; fixed_width = true; }"
  "  }"
  "}"
  "al_fte_set : dialog {"
  "  label = \"검색 설정\";"
  "  : row {"
  "    : boxed_column {"
  "      label = \"검색 대상 객체\";"
  "      : toggle { key = \"t_text\"; label = \"단일행 문자 (TEXT)\"; }"
  "      : toggle { key = \"t_mtext\"; label = \"여러 줄 문자 (MTEXT)\"; }"
  "      : toggle { key = \"t_attr\"; label = \"블록 속성, 속성 정의\"; }"
  "      : toggle { key = \"t_block\"; label = \"블록 내부 문자\"; }"
  "      : toggle { key = \"t_dim\"; label = \"치수 (재지정 문자만)\"; }"
  "      : toggle { key = \"t_mlead\"; label = \"다중 지시선\"; }"
  "      : toggle { key = \"t_table\"; label = \"표 (테이블)\"; }"
  "    }"
  "    : boxed_column {"
  "      label = \"옵션\";"
  "      : toggle { key = \"oneblock\"; label = \"동일 블록은 결과에 항목 하나로 표기\"; }"
  "      : toggle { key = \"fspace\"; label = \"찾기 띄어쓰기 무시\"; }"
  "      : toggle { key = \"rspace\"; label = \"바꾸기 띄어쓰기 무시 (바꾼 문자는 띄어쓰기 삭제됨)\"; }"
  "      : toggle { key = \"foff\"; label = \"찾기 꺼진/동결 레이어 제외\"; }"
  "      : toggle { key = \"roff\"; label = \"바꾸기 꺼진/동결 레이어 제외\"; }"
  "      : toggle { key = \"wild\"; label = \"와일드카드 사용  ( *  #  {*} )\"; }"
  "      : edit_box { key = \"zoom\"; label = \"찾은 문자 Zoom 단계 (작을수록 크게)\"; edit_width = 6; }"
  "      : toggle { key = \"rect\"; label = \"Zoom 할 때 문자 영역에 가상 사각형 표시\"; }"
  "    }"
  "  }"
  "  : boxed_column {"
  "    label = \"찾은 문자 처리 (찾기/바꾸기 공통)\";"
  "    : radio_row {"
  "      key = \"proc\";"
  "      : radio_button { key = \"p0\"; label = \"처리 없음\"; }"
  "      : radio_button { key = \"p1\"; label = \"찾은 문자 삭제\"; }"
  "      : radio_button { key = \"p2\"; label = \"찾은 문자 특성 변경\"; }"
  "    }"
  "    : row {"
  "      : toggle { key = \"ph\"; label = \"높이\"; }"
  "      : edit_box { key = \"phv\"; edit_width = 8; }"
  "      : toggle { key = \"pw\"; label = \"폭 비율\"; }"
  "      : edit_box { key = \"pwv\"; edit_width = 8; }"
  "      : toggle { key = \"pc\"; label = \"색상 (0~256)\"; }"
  "      : edit_box { key = \"pcv\"; edit_width = 5; }"
  "    }"
  "    : row {"
  "      : toggle { key = \"ps\"; label = \"문자 스타일\"; }"
  "      : popup_list { key = \"psv\"; width = 22; }"
  "      : toggle { key = \"pl\"; label = \"레이어\"; }"
  "      : popup_list { key = \"plv\"; width = 26; }"
  "    }"
  "  }"
  "  ok_cancel;"
  "}"
))

;;;; ---- AL_FTE.lsp ----
;;; ------------------------------------------------------------------
;;; AL_FTE.lsp - 문자 찾기/바꾸기 (대화상자)
;;;   ALFTE / ALFIND / ALFR
;;;
;;;   결과 레코드: (종류 엔티티 부가정보 문자)
;;;     종류     : TEXT MTEXT ATTRIB ATTDEF DIM MLEADER TABLE BLOCK
;;;     부가정보 : ATTRIB/BLOCK 은 소유 블록(INSERT), TABLE 은 (행 열)
;;;     문자     : 서식 없는 내용, 여러 줄은 "\n" 으로 연결
;;; ------------------------------------------------------------------

(setq al:*fte-defaults*
  '(("scope" . "1") ("fwhole" . "0") ("fmulti" . "0") ("fcase" . "0")
    ("rwhole" . "0") ("rcase" . "0") ("rdelim" . ",")
    ("t_text" . "1") ("t_mtext" . "1") ("t_attr" . "1") ("t_block" . "1")
    ("t_dim" . "1") ("t_mlead" . "1") ("t_table" . "1")
    ("oneblock" . "0") ("fspace" . "0") ("rspace" . "0") ("foff" . "0") ("roff" . "0")
    ("wild" . "0") ("zoom" . "15") ("rect" . "1")
    ("proc" . "p0") ("ph" . "0") ("phv" . "2.5") ("pw" . "0") ("pwv" . "1.0")
    ("pc" . "0") ("pcv" . "1") ("ps" . "0") ("psv" . "") ("pl" . "0") ("plv" . "")))

(setq al:*fte-main-toggles* '("fwhole" "fmulti" "fcase" "rwhole" "rcase"))
(setq al:*fte-set-toggles*
  '("t_text" "t_mtext" "t_attr" "t_block" "t_dim" "t_mlead" "t_table"
    "oneblock" "fspace" "rspace" "foff" "roff" "wild" "rect" "ph" "pw" "pc" "ps" "pl"))

(setq al:*fte-scopes* '("전체 도면 (모형+배치)" "현재 공간" "모형" "배치" "객체 선택"))

(setq al:*fte-kind-names*
  '(("TEXT" . "문자") ("MTEXT" . "여러줄") ("ATTRIB" . "속성") ("ATTDEF" . "속성정의")
    ("DIM" . "치수") ("MLEADER" . "지시선") ("TABLE" . "표") ("BLOCK" . "블록내부")))

(defun al:fo (k) (al:cfg-get (strcat "FTE_" k) (cdr (assoc k al:*fte-defaults*))))
(defun al:fo-set (k v) (al:cfg-set (strcat "FTE_" k) v))
(defun al:fob (k) (= (al:fo k) "1"))

(defun al:mt-lines (s) (al:str-join (al:mt-paragraphs s) "\n"))
(defun al:lines->mt (s) (al:str-join (mapcar 'al:mt-escape (al:str-split s "\n")) "\\P"))

;;; --- 저장 폴더 (찾기/바꾸기 목록) -----------------------------------

(defun al:data-dir (/ d)
  (setq d (al:catch 'getenv (list "APPDATA")))
  (if (or (null d) (= d "")) (setq d (getvar "TEMPPREFIX")))
  (setq d (strcat (vl-string-right-trim "\\/" d) "\\AutoLisp"))
  (vl-mkdir d)
  (setq d (strcat d "\\FTE"))
  (vl-mkdir d)
  (strcat d "\\")
)

;;; --- 검색 대상 수집 -------------------------------------------------

(defun al:fte-types (/ l)
  (if (al:fob "t_text") (setq l (cons "TEXT" l)))
  (if (al:fob "t_mtext") (setq l (cons "MTEXT" l)))
  (if (or (al:fob "t_attr") (al:fob "t_block")) (setq l (cons "INSERT" l)))
  (if (al:fob "t_attr") (setq l (cons "ATTDEF" l)))
  (if (al:fob "t_dim") (setq l (cons "DIMENSION" l)))
  (if (al:fob "t_mlead") (setq l (cons "MULTILEADER" l)))
  (if (al:fob "t_table") (setq l (cons "ACAD_TABLE" l)))
  (al:str-join l ",")
)

(defun al:fte-ents (/ types scope flt ss)
  (setq types (al:fte-types) scope (al:fo "scope"))
  (if (/= types "")
    (progn
      (setq flt (list (cons 0 types)))
      (setq ss (cond
                 ((= scope "0") (ssget "X" flt))
                 ((= scope "1") (ssget "X" (append flt (list (cons 410 (getvar "CTAB"))))))
                 ((= scope "2") (ssget "X" (append flt '((410 . "Model")))))
                 ((= scope "3") (ssget "X" (append flt '((-4 . "<NOT") (410 . "Model") (-4 . "NOT>")))))
                 (T al:*fte-ss*)))
      (vl-remove-if-not
        '(lambda (e) (and (entget e) (wcmatch (cdr (assoc 0 (entget e))) types)))
        (al:ss->list ss))
    )
  )
)

(defun al:off-layers (/ r l)
  (setq r (tblnext "LAYER" T))
  (while r
    (if (or (< (cdr (assoc 62 r)) 0) (= 1 (logand 1 (cdr (assoc 70 r)))))
      (setq l (cons (strcase (cdr (assoc 2 r))) l))
    )
    (setq r (tblnext "LAYER"))
  )
  l
)

(defun al:fte-off-p (e offl)
  (and offl (member (strcase (cdr (assoc 8 (entget e)))) offl))
)

;; ActiveX 로 문자 읽기/쓰기 (다중 지시선, 표)
(defun al:fte-ax-get (e cell / o)
  (if (al:activex-p)
    (progn
      (setq o (vlax-ename->vla-object e))
      (if cell
        (al:catch 'vla-GetText (list o (car cell) (cadr cell)))
        (al:catch 'vla-get-TextString (list o))
      )
    )
  )
)

(defun al:fte-ax-set (e cell s / o)
  (if (al:activex-p)
    (progn
      (setq o (vlax-ename->vla-object e))
      (not (vl-catch-all-error-p
             (if cell
               (vl-catch-all-apply 'vla-SetText (list o (car cell) (cadr cell) s))
               (vl-catch-all-apply 'vla-put-TextString (list o s)))))
    )
  )
)

(defun al:fte-table-cells (e / o nr nc r c s out)
  (if (al:activex-p)
    (progn
      (setq o (vlax-ename->vla-object e)
            nr (al:catch 'vla-get-Rows (list o))
            nc (al:catch 'vla-get-Columns (list o))
            r 0)
      (if (and nr nc)
        (repeat nr
          (setq c 0)
          (repeat nc
            (setq s (al:catch 'vla-GetText (list o r c)))
            (if (and s (/= s ""))
              (setq out (cons (list "TABLE" e (list r c) (al:mt-lines s)) out))
            )
            (setq c (1+ c))
          )
          (setq r (1+ r))
        )
      )
    )
  )
  (reverse out)
)

;; 블록 정의 안의 문자: ((엔티티 문자) ...)  중첩 블록 4단계까지
(defun al:fte-block-texts (name depth / c b e ed typ out)
  (if (setq c (assoc name al:*fte-blkcache*))
    (cdr c)
    (progn
      (setq b (tblsearch "BLOCK" name))
      (if (and b (= 0 (logand 4 (cdr (assoc 70 b)))))
        (progn
          (setq e (cdr (assoc -2 b)))
          (while e
            (setq ed (entget e) typ (cdr (assoc 0 ed)))
            (if (not (al:fte-off-p e al:*fte-offl*))
              (cond
                ((= typ "TEXT") (setq out (cons (list e (cdr (assoc 1 ed))) out)))
                ((= typ "MTEXT") (setq out (cons (list e (al:mt-lines (al:mtext-string ed))) out)))
                ((and (= typ "DIMENSION") (/= (cdr (assoc 1 ed)) ""))
                 (setq out (cons (list e (cdr (assoc 1 ed))) out)))
                ((and (= typ "INSERT") (< depth 4))
                 (setq out (append (reverse (al:fte-block-texts (cdr (assoc 2 ed)) (1+ depth))) out)))
              )
            )
            (setq e (entnext e))
          )
        )
      )
      (setq out (reverse out)
            al:*fte-blkcache* (cons (cons name out) al:*fte-blkcache*))
      out
    )
  )
)

;; 검색 대상 레코드 목록
(defun al:fte-cands (offl / out ed typ s a)
  (setq al:*fte-blkcache* nil al:*fte-offl* offl)
  (foreach e (al:fte-ents)
    (setq ed (entget e) typ (cdr (assoc 0 ed)))
    (if (not (al:fte-off-p e offl))
      (cond
        ((= typ "TEXT") (setq out (cons (list "TEXT" e nil (cdr (assoc 1 ed))) out)))
        ((= typ "ATTDEF") (setq out (cons (list "ATTDEF" e nil (cdr (assoc 1 ed))) out)))
        ((= typ "MTEXT") (setq out (cons (list "MTEXT" e nil (al:mt-lines (al:mtext-string ed))) out)))
        ((= typ "DIMENSION")
         (if (/= (cdr (assoc 1 ed)) "") (setq out (cons (list "DIM" e nil (cdr (assoc 1 ed))) out))))
        ((= typ "MULTILEADER")
         (if (setq s (al:fte-ax-get e nil)) (setq out (cons (list "MLEADER" e nil (al:mt-lines s)) out))))
        ((= typ "ACAD_TABLE")
         (setq out (append (reverse (al:fte-table-cells e)) out)))
        ((= typ "INSERT")
         (if (and (al:fob "t_attr") (= (cdr (assoc 66 ed)) 1))
           (progn
             (setq a (entnext e))
             (while (and a (= (cdr (assoc 0 (entget a))) "ATTRIB"))
               (if (not (al:fte-off-p a offl))
                 (setq out (cons (list "ATTRIB" a e (cdr (assoc 1 (entget a)))) out))
               )
               (setq a (entnext a))
             )
           )
         )
         (if (al:fob "t_block")
           (foreach x (al:fte-block-texts (cdr (assoc 2 ed)) 0)
             (setq out (cons (list "BLOCK" (car x) e (cadr x)) out))
           )
         ))
      )
    )
  )
  (reverse out)
)

;;; --- 찾기 -----------------------------------------------------------

(defun al:fte-prep (s sp cs)
  (if sp (setq s (al:remove-spaces s)))
  (if cs s (strcase s))
)

(defun al:fte-do-find (text / cs sp terms cps res seen ms)
  (setq cs (al:fob "fcase") sp (al:fob "fspace"))
  (setq terms (if (al:fob "fmulti")
                (vl-remove "" (mapcar 'al:str-trim (al:str-split text ",")))
                (list text)))
  (setq cps (mapcar '(lambda (x) (al:pm-compile (if sp (al:remove-spaces x) x) (al:fob "wild") (al:fob "fwhole") cs))
                    terms))
  (foreach c (al:fte-cands (if (al:fob "foff") (al:off-layers)))
    (setq ms (al:fte-prep (nth 3 c) sp cs))
    (if (and (vl-some '(lambda (cp) (al:pm-find cp ms 1)) cps)
             (not (and (al:fob "oneblock") (= (car c) "BLOCK") (member (cadr c) seen))))
      (progn
        (if (= (car c) "BLOCK") (setq seen (cons (cadr c) seen)))
        (setq res (cons c res))
      )
    )
  )
  (reverse res)
)

;;; --- 바꾸기 ---------------------------------------------------------

;; cps: ((패턴 바꿀문자 대소문자 원래찾을문자) ...)
(defun al:fte-apply-pairs (s cps sp / total r base)
  (setq total 0)
  (foreach p cps
    (setq base (if sp (al:remove-spaces s) s)
          r (al:pm-replace (car p) base (cadr p) (caddr p)))
    (if (> (cadr r) 0) (setq s (car r) total (+ total (cadr r))))
  )
  (list s total)
)

;; 서식을 유지한 채 바꾸기 (와일드카드/전체일치/띄어쓰기무시/제외 없을 때)
(defun al:fte-raw-replace (raw cps / total r)
  (setq total 0)
  (foreach p cps
    (setq r (al:str-replace raw (cadddr p) (cadr p) (caddr p) T))
    (if (> (cadr r) 0) (setq raw (car r) total (+ total (cadr r))))
  )
  (list raw total)
)

(defun al:fte-entmod-1 (e s / ed)
  (setq ed (entget e))
  (entmod (subst (cons 1 s) (assoc 1 ed) ed))
)

;; 한 레코드 바꾸기. 반환 (바꾼개수 새문자) 또는 nil
(defun al:fte-replace-cand (c cps simple sp / kind e typ raw r ok)
  (setq kind (car c) e (cadr c)
        typ (if (= kind "BLOCK") (cdr (assoc 0 (entget e))) kind))
  (if (member typ '("MTEXT" "MLEADER" "TABLE"))
    (progn
      (setq raw (cond ((= typ "MTEXT") (al:mtext-string (entget e)))
                      ((= typ "MLEADER") (al:fte-ax-get e nil))
                      (T (al:fte-ax-get e (caddr c)))))
      (if raw
        (if simple
          (setq r (al:fte-raw-replace raw cps))
          (progn
            (setq r (al:fte-apply-pairs (al:mt-lines raw) cps sp))
            (if (> (cadr r) 0) (setq r (list (al:lines->mt (car r)) (cadr r))))
          )
        )
      )
    )
    (setq r (al:fte-apply-pairs (nth 3 c) cps sp))
  )
  (if (and r (> (cadr r) 0))
    (progn
      (al:unlock-for e (caddr c))
      (setq ok (cond
                 ((= typ "MTEXT") (entmod (al:mtext-set (entget e) (car r))))
                 ((= typ "MLEADER") (al:fte-ax-set e nil (car r)))
                 ((= typ "TABLE") (al:fte-ax-set e (caddr c) (car r)))
                 (T (al:fte-entmod-1 e (car r)))))
      (if ok
        (list (cadr r) (if (member typ '("MTEXT" "MLEADER" "TABLE")) (al:mt-lines (car r)) (car r)))
        (progn (setq al:*fail* (1+ al:*fail*)) nil)
      )
    )
  )
)

(defun al:fte-do-replace (/ cs sp wild whole cps simple done res r n cnt regen owners)
  (setq cs (al:fob "rcase") sp (al:fob "rspace") wild (al:fob "wild") whole (al:fob "rwhole"))
  (setq cps (mapcar '(lambda (p)
                       (list (al:pm-compile (if sp (al:remove-spaces (car p)) (car p)) wild whole cs)
                             (cadr p) cs (car p)))
                    al:*fte-pairs*))
  (setq simple (and (not wild) (not whole) (not sp)
                    (not (vl-some '(lambda (p) (cadr (car p))) cps))))
  (setq al:*fail* 0 n 0 cnt 0)
  (foreach c (al:fte-cands (if (al:fob "roff") (al:off-layers)))
    (if (not (and (= (car c) "BLOCK") (member (cadr c) done)))
      (if (setq r (al:fte-replace-cand c cps simple sp))
        (progn
          (setq n (1+ n) cnt (+ cnt (car r))
                res (cons (list (car c) (cadr c) (caddr c) (cadr r)) res))
          (if (= (car c) "BLOCK") (setq done (cons (cadr c) done) regen T))
          (if (= (car c) "ATTRIB") (setq owners (cons (caddr c) owners)))
        )
      )
    )
  )
  (foreach o owners (entupd o))
  (if regen (command "_.REGEN"))
  (al:layer-relock)
  (setq al:*fte-status*
         (strcat "바꾸기 완료: 객체 " (itoa n) "개, " (itoa cnt) "곳"
                 (if (> al:*fail* 0) (strcat " / 바꾸지 못한 객체 " (itoa al:*fail*) "개") "")))
  (reverse res)
)

;;; --- 찾은 문자 처리 (삭제 / 특성 변경) ------------------------------

(defun al:set-dxf (ed code val)
  (if (assoc code ed)
    (subst (cons code val) (assoc code ed) ed)
    (append ed (list (cons code val)))
  )
)

(defun al:fte-change-props (c / e ed typ)
  (setq e (cadr c) ed (entget e) typ (cdr (assoc 0 ed)))
  (if (member typ '("TEXT" "MTEXT" "ATTRIB" "ATTDEF"))
    (progn
      (al:unlock-for e (caddr c))
      (if (and (al:fob "ph") (> (atof (al:fo "phv")) 0.0)) (setq ed (al:set-dxf ed 40 (atof (al:fo "phv")))))
      (if (and (al:fob "pw") (/= typ "MTEXT") (> (atof (al:fo "pwv")) 0.0)) (setq ed (al:set-dxf ed 41 (atof (al:fo "pwv")))))
      (if (and (al:fob "ps") (tblsearch "STYLE" (al:fo "psv"))) (setq ed (al:set-dxf ed 7 (al:fo "psv"))))
      (if (al:fob "pc") (setq ed (al:set-dxf ed 62 (max 0 (min 256 (atoi (al:fo "pcv")))))))
      (if (and (al:fob "pl") (tblsearch "LAYER" (al:fo "plv"))) (setq ed (al:set-dxf ed 8 (al:fo "plv"))))
      (if (entmod ed)
        (progn (if (= (type (caddr c)) 'ENAME) (entupd (caddr c))) T)
      )
    )
  )
)

;; 반환: 처리 후 남은 결과 목록
(defun al:fte-process (res / p n keep regen)
  (setq p (al:fo "proc") n 0)
  (cond
    ((= p "p1")
     (foreach c res
       (if (and (member (car c) '("TEXT" "MTEXT")) (progn (al:unlock-for (cadr c) nil) (entdel (cadr c))))
         (setq n (1+ n))
         (setq keep (cons c keep))
       )
     )
     (al:layer-relock)
     (setq al:*fte-status* (strcat (cond (al:*fte-status*) ("")) " / 찾은 문자 " (itoa n) "개 삭제"))
     (reverse keep))
    ((= p "p2")
     (foreach c res
       (if (al:fte-change-props c)
         (progn (setq n (1+ n)) (if (= (car c) "BLOCK") (setq regen T)))
       )
     )
     (if regen (command "_.REGEN"))
     (al:layer-relock)
     (setq al:*fte-status* (strcat (cond (al:*fte-status*) ("")) " / 특성 변경 " (itoa n) "개"))
     res)
    (T res)
  )
)

;;; --- 위치, 확대, 선택 -----------------------------------------------

(defun al:fte-target (c)
  (if (member (car c) '("ATTRIB" "BLOCK")) (caddr c) (cadr c))
)

;; 블록 내부 문자의 WCS 위치 (1단계 변환)
(defun al:fte-inner-pt (inner ins / ied ed bp p sx sy)
  (setq ied (entget inner) ed (entget ins)
        bp (cdr (assoc 10 (tblsearch "BLOCK" (cdr (assoc 2 ed)))))
        p (cdr (assoc 10 ied))
        sx (cond ((cdr (assoc 41 ed))) (1.0))
        sy (cond ((cdr (assoc 42 ed))) (1.0)))
  (al:v+ (cdr (assoc 10 ed))
         (al:rot (list (* sx (- (car p) (car bp))) (* sy (- (cadr p) (cadr bp))))
                 (cdr (assoc 50 ed))))
)

;; 결과의 영역: ((최소점) (최대점))
(defun al:fte-extent (c / e bb p h ed sy)
  (setq e (cadr c))
  (cond
    ((= (car c) "BLOCK")
     (setq ed (entget (caddr c))
           sy (abs (cond ((cdr (assoc 42 ed))) (1.0)))
           h (* sy (cond ((cdr (assoc 40 (entget e)))) (1.0)))
           p (al:fte-inner-pt e (caddr c)))
     (list p (al:v+ p (list (* h 0.8 (max 1 (strlen (nth 3 c)))) h))))
    ((setq bb (al:bbox e)) bb)
    (T
     (setq p (cdr (assoc 10 (entget e))) h (cond ((cdr (assoc 40 (entget e)))) (1.0)))
     (list p (al:v+ p (list (* h 0.8 (max 1 (strlen (nth 3 c)))) h))))
  )
)

(defun al:fte-height (c / ed)
  (setq ed (entget (cadr c)))
  (cond ((cdr (assoc 40 ed))) (1.0))
)

(defun al:grbox (p1 p2)
  (grdraw p1 (list (car p2) (cadr p1)) 1 1)
  (grdraw (list (car p2) (cadr p1)) p2 1 1)
  (grdraw p2 (list (car p1) (cadr p2)) 1 1)
  (grdraw (list (car p1) (cadr p2)) p1 1 1)
)

(defun al:fte-zoom (c / tgt lay ex h)
  (setq tgt (al:fte-target c)
        lay (cdr (assoc 410 (entget tgt))))
  (if (and lay (/= (strcase lay) (strcase (getvar "CTAB"))))
    (setvar "CTAB" lay)
  )
  (setq ex (al:fte-extent c)
        h (max (al:fte-height c) (- (cadr (cadr ex)) (cadr (car ex)))))
  (command "_.ZOOM" "_C" (al:p3 (al:mid (car ex) (cadr ex))) (* h (max 1.0 (atof (al:fo "zoom")))))
  (if (al:fob "rect") (al:grbox (car ex) (cadr ex)))
)

(defun al:fte-in-ctab (c / lay)
  (setq lay (cdr (assoc 410 (entget (al:fte-target c)))))
  (or (null lay) (= (strcase lay) (strcase (getvar "CTAB"))))
)

(defun al:fte-select (lst / out)
  (foreach c lst
    (if (and (entget (al:fte-target c)) (al:fte-in-ctab c) (not (member (al:fte-target c) out)))
      (setq out (cons (al:fte-target c) out))
    )
  )
  (reverse out)
)

;;; --- 바꾸기 목록 (텍스트 파일, 엑셀, 저장 목록) ---------------------

(defun al:fte-read-pairs (fname delim / f ln p out)
  (if (setq f (open fname "r"))
    (progn
      (while (setq ln (read-line f))
        (if (setq p (vl-string-search delim ln))
          (setq out (cons (list (substr ln 1 p) (substr ln (+ p 1 (strlen delim)))) out))
        )
      )
      (close f)
    )
  )
  (reverse out)
)

(defun al:var->str (v)
  (if (= (type v) 'VARIANT) (setq v (vlax-variant-value v)))
  (cond
    ((null v) "")
    ((= (type v) 'STR) v)
    ((numberp v) (al:num->str v))
    (T "")
  )
)

(defun al:fte-from-excel (/ xl rng v rows out)
  (setq xl (al:catch 'vlax-get-object (list "Excel.Application")))
  (if (null xl)
    (progn (alert "실행 중인 엑셀이 없습니다.\n엑셀 파일을 먼저 여세요.") nil)
    (progn
      (princ "\n엑셀에서 [찾을 문자 | 바꿀 문자] 두 열을 선택한 뒤 CAD 로 돌아와 Enter: ")
      (getstring)
      (setq rng (al:catch 'vlax-get-property (list xl 'Selection))
            v (if rng (al:catch 'vlax-get-property (list rng 'Value2))))
      (if (and v (= (type (vlax-variant-value v)) 'SAFEARRAY))
        (progn
          (setq rows (vlax-safearray->list (vlax-variant-value v)))
          (foreach r rows
            (if (and (>= (length r) 2) (/= (al:var->str (car r)) ""))
              (setq out (cons (list (al:var->str (car r)) (al:var->str (cadr r))) out))
            )
          )
        )
        (alert "엑셀에서 두 열 이상을 선택해야 합니다.")
      )
      (if rng (vlax-release-object rng))
      (vlax-release-object xl)
      (reverse out)
    )
  )
)

(defun al:fte-saved-names ()
  (mapcar '(lambda (f) (substr f 4 (- (strlen f) 7)))
          (vl-sort (vl-directory-files (al:data-dir) "FR_*.txt" 1) '<))
)

(defun al:fte-save-list (name / f)
  (if (setq f (open (strcat (al:data-dir) "FR_" name ".txt") "w"))
    (progn
      (foreach p al:*fte-pairs* (write-line (strcat (car p) "\t" (cadr p)) f))
      (close f)
      T
    )
  )
)

;;; --- 대화상자 -------------------------------------------------------

(defun al:dcl-file (/ fn f)
  (if (and al:*dcl-path* (findfile al:*dcl-path*))
    al:*dcl-path*
    (progn
      (setq fn (vl-filename-mktemp "al_fte.dcl"))
      (if (setq f (open fn "w"))
        (progn
          (foreach l al:*dcl-fte* (write-line l f))
          (close f)
          (setq al:*dcl-path* fn)
        )
      )
    )
  )
)

(defun al:fill-list (key items)
  (start_list key)
  (mapcar 'add_list items)
  (end_list)
)

(defun al:fte-short (s / n)
  (setq s (car (al:str-replace s "\n" " " T nil)))
  (if (> (strlen s) 60)
    (strcat (substr s 1 (al:safe-cut s 60)) "...")
    s
  )
)

(defun al:fte-row (i c / lay)
  (setq lay (if (entget (al:fte-target c)) (cdr (assoc 8 (entget (al:fte-target c)))) ""))
  (strcat (itoa i) "\t" (cdr (assoc (car c) al:*fte-kind-names*)) "\t" lay "\t" (al:fte-short (nth 3 c)))
)

(defun al:fte-fill-results (/ i)
  (setq i 0)
  (al:fill-list "results" (mapcar '(lambda (c) (al:fte-row (setq i (1+ i)) c)) al:*fte-results*))
  (if al:*fte-rsel* (set_tile "results" al:*fte-rsel*))
  (set_tile "status" (cond (al:*fte-status*) ("")))
)

(defun al:fte-fill-pairs ()
  (al:fill-list "rlist" (mapcar '(lambda (p) (strcat (car p) "\t→ " (cadr p))) al:*fte-pairs*))
)

(defun al:fte-fill-saved (/ names)
  (setq al:*fte-saved* (al:fte-saved-names))
  (al:fill-list "rsaved" al:*fte-saved*)
)

(defun al:fte-history () (vl-remove "" (al:str-split (al:cfg-get "FTE_hist" "") (chr 31))))

(defun al:fte-history-add (s / h)
  (setq h (cons s (vl-remove s (al:fte-history))))
  (if (> (length h) 15) (setq h (reverse (cdr (reverse h)))))
  (al:cfg-set "FTE_hist" (al:str-join h (chr 31)))
)

(defun al:fte-procinfo ()
  (cond
    ((= (al:fo "proc") "p1") "※ 찾은 문자 처리: 삭제 (검색 설정에서 변경)")
    ((= (al:fo "proc") "p2") "※ 찾은 문자 처리: 특성 변경 (검색 설정에서 변경)")
    (T "")
  )
)

(defun al:sel-indices (str)
  (if (and str (/= str "")) (mapcar 'atoi (al:str-split (al:str-trim str) " ")))
)

(defun al:remove-indices (lst idx / i out)
  (setq i 0)
  (foreach x lst
    (if (not (member i idx)) (setq out (cons x out)))
    (setq i (1+ i))
  )
  (reverse out)
)

;; 대화상자 값을 전역/설정에 보관
(defun al:fte-keep-tiles ()
  (al:fo-set "scope" (get_tile "scope"))
  (foreach k al:*fte-main-toggles* (al:fo-set k (get_tile k)))
  (al:fo-set "rdelim" (get_tile "rdelim"))
  (setq al:*fte-ftext* (get_tile "ftext")
        al:*fte-rfind* (get_tile "rfind")
        al:*fte-rrepl* (get_tile "rrepl")
        al:*fte-rsname* (get_tile "rsname")
        al:*fte-rsel* (get_tile "results"))
)

(defun al:fte-close (code)
  (al:fte-keep-tiles)
  (setq al:*fte-pos* (done_dialog code))
)

(defun al:fte-init ()
  (al:fill-list "scope" al:*fte-scopes*)
  (set_tile "scope" (al:fo "scope"))
  (mode_tile "selobj" (if (= (al:fo "scope") "4") 0 1))
  (foreach k al:*fte-main-toggles* (set_tile k (al:fo k)))
  (set_tile "rdelim" (al:fo "rdelim"))
  (set_tile "ftext" (cond (al:*fte-ftext*) ("")))
  (set_tile "rfind" (cond (al:*fte-rfind*) ("")))
  (set_tile "rrepl" (cond (al:*fte-rrepl*) ("")))
  (set_tile "rsname" (cond (al:*fte-rsname*) ("")))
  (al:fill-list "fhist" (al:fte-history))
  (al:fte-fill-pairs)
  (al:fte-fill-saved)
  (al:fte-fill-results)
  (set_tile "procinfo" (al:fte-procinfo))
  (mode_tile "ftext" 2)

  (action_tile "scope" "(mode_tile \"selobj\" (if (= $value \"4\") 0 1))")
  (action_tile "selobj" "(al:fte-close 10)")
  (action_tile "fpick" "(al:fte-close 11)")
  (action_tile "fpickm" "(al:fte-close 12)")
  (action_tile "rpick" "(al:fte-close 13)")
  (action_tile "dofind" "(al:fte-close 14)")
  (action_tile "doreplace" "(al:fte-close 15)")
  (action_tile "rtxt" "(al:fte-close 16)")
  (action_tile "rxls" "(al:fte-close 17)")
  (action_tile "rzoom" "(al:fte-close 20)")
  (action_tile "results" "(if (= $reason 4) (al:fte-close 20))")
  (action_tile "rsel" "(al:fte-close 21)")
  (action_tile "rselall" "(al:fte-close 22)")
  (action_tile "rline" "(al:fte-close 23)")
  (action_tile "rerase" "(al:fte-close 24)")
  (action_tile "rremove" "(al:fte-act-remove)")
  (action_tile "fhist" "(al:fte-act-hist $value)")
  (action_tile "fhclr" "(al:cfg-set \"FTE_hist\" \"\")(al:fill-list \"fhist\" nil)")
  (action_tile "radd" "(al:fte-act-add)")
  (action_tile "rdel" "(al:fte-act-pairdel)")
  (action_tile "rclr" "(setq al:*fte-pairs* nil)(al:fte-fill-pairs)")
  (action_tile "rload" "(al:fte-act-load)")
  (action_tile "rsdel" "(al:fte-act-sdel)")
  (action_tile "rsave" "(al:fte-act-save)")
  (action_tile "settings" "(al:fte-settings)(set_tile \"procinfo\" (al:fte-procinfo))")
  (action_tile "cancel" "(al:fte-close 0)")
)

(defun al:fte-act-hist (v / h)
  (setq h (al:fte-history))
  (if (and v (/= v "")) (set_tile "ftext" (nth (atoi v) h)))
)

(defun al:fte-act-remove ()
  (setq al:*fte-results* (al:remove-indices al:*fte-results* (al:sel-indices (get_tile "results")))
        al:*fte-rsel* nil
        al:*fte-status* (strcat "목록 " (itoa (length al:*fte-results*)) "개"))
  (al:fte-fill-results)
)

(defun al:fte-act-add (/ f r)
  (setq f (get_tile "rfind") r (get_tile "rrepl"))
  (if (= f "")
    (set_tile "status" "찾을 문자를 입력하세요.")
    (progn
      (setq al:*fte-pairs* (append al:*fte-pairs* (list (list f r))))
      (al:fte-fill-pairs)
      (set_tile "rfind" "")
      (set_tile "rrepl" "")
      (mode_tile "rfind" 2)
    )
  )
)

(defun al:fte-act-pairdel ()
  (setq al:*fte-pairs* (al:remove-indices al:*fte-pairs* (al:sel-indices (get_tile "rlist"))))
  (al:fte-fill-pairs)
)

(defun al:fte-act-load (/ v)
  (setq v (get_tile "rsaved"))
  (if (and v (/= v "") al:*fte-saved*)
    (progn
      (setq al:*fte-pairs* (al:fte-read-pairs (strcat (al:data-dir) "FR_" (nth (atoi v) al:*fte-saved*) ".txt") "\t"))
      (al:fte-fill-pairs)
      (set_tile "status" (strcat "목록 불러옴: " (nth (atoi v) al:*fte-saved*)))
    )
  )
)

(defun al:fte-act-sdel (/ v)
  (setq v (get_tile "rsaved"))
  (if (and v (/= v "") al:*fte-saved*)
    (progn
      (vl-file-delete (strcat (al:data-dir) "FR_" (nth (atoi v) al:*fte-saved*) ".txt"))
      (al:fte-fill-saved)
    )
  )
)

(defun al:fte-act-save (/ name)
  (setq name (al:str-trim (get_tile "rsname")))
  (cond
    ((= name "") (set_tile "status" "저장 이름을 입력하세요."))
    ((wcmatch name "*[\\/:`*`?\"<>|]*") (set_tile "status" "이름에 \\ / : * ? \" < > | 는 쓸 수 없습니다."))
    ((null al:*fte-pairs*) (set_tile "status" "저장할 목록이 없습니다."))
    ((and (>= (length (al:fte-saved-names)) 20) (not (member name (al:fte-saved-names))))
     (set_tile "status" "저장 목록은 20개까지입니다. 먼저 하나를 삭제하세요."))
    ((al:fte-save-list name)
     (al:fte-fill-saved)
     (set_tile "status" (strcat "목록 저장: " name)))
    (T (set_tile "status" "저장하지 못했습니다."))
  )
)

;; 검색 설정 (중첩 대화상자)
(defun al:fte-settings (/ styles layers r i)
  (if (new_dialog "al_fte_set" al:*fte-dclid*)
    (progn
      (foreach k al:*fte-set-toggles* (set_tile k (al:fo k)))
      (foreach k '("zoom" "phv" "pwv" "pcv") (set_tile k (al:fo k)))
      (set_tile "proc" (al:fo "proc"))
      (setq r (tblnext "STYLE" T))
      (while r
        (if (/= (cdr (assoc 2 r)) "") (setq styles (cons (cdr (assoc 2 r)) styles)))
        (setq r (tblnext "STYLE"))
      )
      (setq r (tblnext "LAYER" T))
      (while r (setq layers (cons (cdr (assoc 2 r)) layers) r (tblnext "LAYER")))
      (setq styles (vl-sort styles '<) layers (vl-sort layers '<))
      (setq al:*fte-styles* styles al:*fte-layers* layers)
      (al:fill-list "psv" styles)
      (al:fill-list "plv" layers)
      (if (setq i (vl-position (al:fo "psv") styles)) (set_tile "psv" (itoa i)))
      (if (setq i (vl-position (al:fo "plv") layers)) (set_tile "plv" (itoa i)))
      (action_tile "accept" "(if (al:fte-settings-save) (done_dialog 1))")
      (start_dialog)
    )
  )
)

(defun al:fte-settings-save (/ z v)
  (setq z (atof (get_tile "zoom")))
  (if (<= z 0.0)
    (progn (alert "Zoom 단계는 0보다 커야 합니다.") nil)
    (progn
      (foreach k al:*fte-set-toggles* (al:fo-set k (get_tile k)))
      (foreach k '("zoom" "phv" "pwv" "pcv") (al:fo-set k (get_tile k)))
      (al:fo-set "proc" (get_tile "proc"))
      (if (and (setq v (get_tile "psv")) (/= v "")) (al:fo-set "psv" (nth (atoi v) al:*fte-styles*)))
      (if (and (setq v (get_tile "plv")) (/= v "")) (al:fo-set "plv" (nth (atoi v) al:*fte-layers*)))
      T
    )
  )
)

;;; --- 대화상자 밖에서 실행하는 동작 ----------------------------------

(defun al:pick-text-string (/ s e ed typ)
  (if (setq s (nentsel "\n문자 선택: "))
    (progn
      (setq e (car s) ed (entget e) typ (cdr (assoc 0 ed)))
      (cond
        ((= typ "MTEXT") (al:str-join (al:mt-paragraphs (al:mtext-string ed)) " "))
        ((member typ '("TEXT" "ATTRIB" "ATTDEF")) (cdr (assoc 1 ed)))
        (T (princ "\n문자가 아닙니다.") nil)
      )
    )
  )
)

(defun al:fte-sel-results ()
  (mapcar '(lambda (i) (nth i al:*fte-results*)) (al:sel-indices al:*fte-rsel*))
)

;; 반환: 다시 대화상자를 열면 T
(defun al:fte-run (st / s ss l n pt)
  (cond
    ((= st 10)
     (princ "\n검색할 객체 선택: ")
     (if (setq ss (ssget)) (setq al:*fte-ss* ss al:*fte-status* (strcat "객체 " (itoa (sslength ss)) "개 선택됨")))
     T)
    ((= st 11)
     (if (setq s (al:pick-text-string)) (setq al:*fte-ftext* s))
     T)
    ((= st 12)
     (princ "\n찾을 문자들 선택: ")
     (if (setq ss (ssget '((0 . "TEXT,MTEXT"))))
       (progn
         (foreach e (al:ss->list ss)
           (setq s (al:ent-text (entget e)))
           (if (= (cdr (assoc 0 (entget e))) "MTEXT") (setq s (al:mt-plain s)))
           (if (and (/= s "") (not (member s l))) (setq l (cons s l)))
         )
         (setq al:*fte-ftext* (al:str-join (reverse l) ","))
         (al:fo-set "fmulti" "1")
       )
     )
     T)
    ((= st 13)
     (if (setq s (al:pick-text-string)) (setq al:*fte-rfind* s))
     T)
    ((= st 14)
     (cond
       ((= al:*fte-ftext* "") (setq al:*fte-status* "찾을 내용을 입력하세요."))
       ((and (= (al:fo "scope") "4") (null al:*fte-ss*)) (setq al:*fte-status* "검색할 객체를 먼저 선택하세요."))
       (T
        (al:fte-history-add al:*fte-ftext*)
        (setq al:*fte-results* (al:fte-do-find al:*fte-ftext*)
              al:*fte-rsel* nil
              al:*fte-status* (strcat "찾은 문자: " (itoa (length al:*fte-results*)) "개"))
        (if (and al:*fte-results* (/= (al:fo "proc") "p0"))
          (progn
            (setq al:*fte-results* (al:fte-process al:*fte-results*))
            (al:undo-end) (al:undo-begin)
          )
        ))
     )
     T)
    ((= st 15)
     (cond
       ((null al:*fte-pairs*) (setq al:*fte-status* "바꾸기 목록이 비어 있습니다. [목록에 추가]를 누르세요."))
       ((and (= (al:fo "scope") "4") (null al:*fte-ss*)) (setq al:*fte-status* "검색할 객체를 먼저 선택하세요."))
       (T
        (setq al:*fte-results* (al:fte-do-replace) al:*fte-rsel* nil)
        (if (and al:*fte-results* (/= (al:fo "proc") "p0"))
          (setq al:*fte-results* (al:fte-process al:*fte-results*))
        )
        (al:undo-end) (al:undo-begin))
     )
     T)
    ((= st 16)
     (if (setq s (getfiled "찾기/바꾸기 목록 텍스트 파일" (getvar "DWGPREFIX") "txt;csv" 0))
       (progn
         (setq l (al:fte-read-pairs s (if (= (al:fo "rdelim") "") "," (al:fo "rdelim"))))
         (setq al:*fte-pairs* (append al:*fte-pairs* l)
               al:*fte-status* (strcat "텍스트 파일에서 " (itoa (length l)) "개 추가"))
       )
     )
     T)
    ((= st 17)
     (setq l (al:fte-from-excel))
     (setq al:*fte-pairs* (append al:*fte-pairs* l)
           al:*fte-status* (strcat "엑셀에서 " (itoa (length l)) "개 추가"))
     T)
    ((= st 20)
     (setq l (al:fte-sel-results))
     (cond
       ((null l) (setq al:*fte-status* "확대할 항목을 선택하세요.") T)
       ((not (entget (al:fte-target (car l)))) (setq al:*fte-status* "삭제된 객체입니다.") T)
       (T
        (al:fte-zoom (car l))
        (princ "\n확인 후 Enter 를 누르면 대화상자로 돌아갑니다: ")
        (getstring)
        T)
     ))
    ((member st '(21 22))
     (setq l (al:fte-select (if (= st 21) (al:fte-sel-results) al:*fte-results*)))
     (if l
       (progn
         (setq al:*fte-pickfirst* (al:list->ss l))
         (princ (strcat "\n" (itoa (length l)) "개 객체를 선택했습니다. (현재 공간의 객체만)"))
         nil)
       (progn (setq al:*fte-status* "현재 공간에서 선택할 객체가 없습니다.") T)
     ))
    ((= st 23)
     (setq l (vl-remove-if-not 'al:fte-in-ctab (al:fte-sel-results)))
     (if (null l)
       (setq al:*fte-status* "현재 공간의 항목을 선택하세요.")
       (if (setq pt (getpoint "\n연결선 기준점: "))
         (progn
           (foreach c l
             (setq s (al:fte-extent c))
             (al:make-line pt (al:mid (car s) (cadr s)))
           )
           (al:undo-end) (al:undo-begin)
           (setq al:*fte-status* (strcat "연결선 " (itoa (length l)) "개 작성"))
         )
       )
     )
     T)
    ((= st 24)
     (setq l (al:fte-sel-results) n 0)
     (foreach c l
       (if (and (member (car c) '("TEXT" "MTEXT")) (entget (cadr c)))
         (progn
           (al:unlock-for (cadr c) nil)
           (if (entdel (cadr c))
             (setq n (1+ n) al:*fte-results* (vl-remove c al:*fte-results*))
           )
         )
       )
     )
     (al:layer-relock)
     (al:undo-end) (al:undo-begin)
     (setq al:*fte-rsel* nil
           al:*fte-status* (strcat (itoa n) "개 삭제 (TEXT, MTEXT 만 삭제됩니다)"))
     T)
    (T nil)
  )
)

(defun al:fte-dialog (/ dcl again st)
  (setq al:*fte-pickfirst* nil al:*fte-ss* nil al:*fte-status* nil)
  (if (not (setq dcl (al:dcl-file)))
    (princ "\n대화상자 파일을 만들 수 없습니다.")
    (progn
      (setq al:*fte-dclid* (load_dialog dcl) again T)
      (while again
        (if (not (new_dialog "al_fte" al:*fte-dclid* "" (cond (al:*fte-pos*) ('(-1 -1)))))
          (progn (princ "\n대화상자를 열 수 없습니다.") (setq again nil))
          (progn
            (al:fte-init)
            (setq st (start_dialog))
            (setq again (al:fte-run st))
          )
        )
      )
      (unload_dialog al:*fte-dclid*)
    )
  )
)

(defun al:fte-command ()
  (al:begin)
  (al:fte-dialog)
  (al:end)
  (if al:*fte-pickfirst* (sssetfirst nil al:*fte-pickfirst*))
  (princ)
)

(defun c:ALFTE () (al:fte-command))
(defun c:ALFIND () (al:fte-command))
(defun c:ALFR () (al:fte-command))

;;;; ---- AL_Purge.lsp ----
;;; ------------------------------------------------------------------
;;; AL_Purge.lsp - 도면 정리
;;;   ALPURGE     : 사용하지 않는 항목 모두 소거 (+ 빈 문자, 길이 0 객체 삭제)
;;;   ALAUTOPURGE : 저장할 때마다 자동 소거 켜기/끄기
;;; ------------------------------------------------------------------

(defun al:table-count (/ n rec)
  (setq n 0)
  (foreach tb '("BLOCK" "LAYER" "LTYPE" "STYLE" "DIMSTYLE" "APPID")
    (setq rec (tblnext tb T))
    (while rec (setq n (1+ n) rec (tblnext tb)))
  )
  n
)

(defun al:purge-all (/ doc)
  (if (al:activex-p)
    (progn
      (setq doc (al:doc))
      (repeat 3 (al:catch 'vla-PurgeAll (list doc)))
    )
    (repeat 3 (command "_.-PURGE" "_All" "*" "_No"))
  )
)

(defun al:clean-empty-text (/ n ed s)
  (setq n 0)
  (foreach e (al:ss->list (ssget "X" '((0 . "TEXT,MTEXT"))))
    (setq ed (entget e) s (al:ent-text ed))
    (if (= (cdr (assoc 0 ed)) "MTEXT") (setq s (al:mt-plain s)))
    (if (and (= (al:str-trim s) "") (entdel e)) (setq n (1+ n)))
  )
  n
)

(defun al:clean-zero-length (/ n len)
  (setq n 0)
  (foreach e (al:ss->list (ssget "X" '((0 . "LINE,LWPOLYLINE,ARC,SPLINE"))))
    (setq len (al:curve-length e))
    (if (and len (< len 1e-9) (entdel e)) (setq n (1+ n)))
  )
  n
)

(defun c:ALPURGE (/ extra before after ne nz)
  (al:begin)
  (setq extra (al:yes-p "\n빈 문자, 길이 0 객체도 삭제할까요" "Yes"))
  (setq before (al:table-count) ne 0 nz 0)
  (if extra (setq ne (al:clean-empty-text) nz (al:clean-zero-length)))
  (al:purge-all)
  (setq after (al:table-count))
  (princ (strcat "\n소거 완료: 블록/레이어/선종류/문자스타일/치수스타일/등록앱 "
                 (itoa (- before after)) "개 정리"))
  (if extra
    (princ (strcat "\n빈 문자 " (itoa ne) "개, 길이 0 객체 " (itoa nz) "개 삭제"))
  )
  (al:end)
)

;;; --- 저장 시 자동 소거 ----------------------------------------------

(defun al:autopurge-on-save (reactor params)
  (if (= (al:cfg-get "AutoPurge" "Off") "On")
    (if (al:catch '(lambda () (repeat 3 (vla-PurgeAll (al:doc))) T) nil)
      (princ "\n[AutoLisp] 저장 전 자동 소거 완료.")
    )
  )
)

(defun al:autopurge-install ()
  (if (and vlr-dwg-reactor (al:activex-p) (not al:*autopurge-reactor*))
    (setq al:*autopurge-reactor*
           (vlr-dwg-reactor "AutoLisp-AutoPurge" '((:vlr-beginSave . al:autopurge-on-save))))
  )
)

(defun c:ALAUTOPURGE (/ cur new)
  (setq cur (al:cfg-get "AutoPurge" "Off"))
  (if (not (and vlr-dwg-reactor (al:activex-p)))
    (princ "\n이 CAD 는 리액터를 지원하지 않아 자동 소거를 쓸 수 없습니다. ALPURGE 를 사용하세요.")
    (progn
      (setq new (al:kword (strcat "\n저장 시 자동 소거 (현재 " cur ") [On/Off] <" cur ">: ") "On Off" cur))
      (al:cfg-set "AutoPurge" new)
      (al:autopurge-install)
      (princ (strcat "\n저장 시 자동 소거: " new))
      (if (= new "On")
        (princ "\n※ 모든 도면에서 동작하려면 AutoLisp.lsp 를 시작하기(Startup Suite)에 등록하세요.")
      )
    )
  )
  (princ)
)

;;;; ---- AL_Qty.lsp ----
;;; ------------------------------------------------------------------
;;; AL_Qty.lsp - 수량 산출
;;;   ALQTY  : 레이어/객체 종류별 개수, 길이, 면적 집계
;;;   ALBCNT : 블록 이름별 개수
;;;   ALTSUM : 선택한 문자/치수의 숫자 합계
;;; ------------------------------------------------------------------

(setq al:*type-names*
  '(("LINE" . "선") ("LWPOLYLINE" . "폴리선") ("POLYLINE" . "폴리선(2D/3D)")
    ("ARC" . "호") ("CIRCLE" . "원") ("ELLIPSE" . "타원") ("SPLINE" . "스플라인")
    ("HATCH" . "해치") ("REGION" . "영역") ("INSERT" . "블록") ("TEXT" . "문자")
    ("MTEXT" . "여러 줄 문자") ("DIMENSION" . "치수") ("POINT" . "점")))

(defun al:type-name (typ / r)
  (if (setq r (assoc typ al:*type-names*)) (cdr r) typ)
)

(defun al:unit-factor (u)
  (cond ((= u "MM") 1000.0) ((= u "CM") 100.0) (T 1.0))
)

(defun c:ALQTY (/ ss group unit f acc ed key len area rows tc tl ta)
  (al:begin)
  (setq ss (al:select-or-all "\n수량을 산출할 객체 선택 (Enter=현재 공간 전체): " nil))
  (if ss
    (progn
      (setq group (al:kword "\n집계 기준 - 레이어(L)/객체 종류(T)/레이어+종류(B) [Layer/Type/Both] <Layer>: "
                            "Layer Type Both" (al:cfg-get "QtyGroup" "Layer")))
      (al:cfg-set "QtyGroup" group)
      (setq unit (al:kword "\n도면 단위 [MM/CM/M] <MM>: " "MM CM M" (al:cfg-get "QtyUnit" "MM")))
      (al:cfg-set "QtyUnit" unit)
      (setq f (al:unit-factor unit))
      (foreach e (al:ss->list ss)
        (setq ed (entget e)
              key (cond
                    ((= group "Type") (al:type-name (cdr (assoc 0 ed))))
                    ((= group "Both") (strcat (cdr (assoc 8 ed)) " / " (al:type-name (cdr (assoc 0 ed)))))
                    (T (cdr (assoc 8 ed))))
              len (if (member (cdr (assoc 0 ed)) '("LINE" "LWPOLYLINE" "POLYLINE" "ARC" "CIRCLE" "ELLIPSE" "SPLINE"))
                    (cond ((al:curve-length e)) (0.0))
                    0.0)
              area (cond ((al:ent-area e)) (0.0))
              acc (al:acc-add acc key 1 len area))
      )
      (setq acc (vl-sort acc '(lambda (a b) (< (strcase (car a)) (strcase (car b))))))
      (setq tc 0 tl 0.0 ta 0.0)
      (foreach r acc
        (setq tc (+ tc (nth 1 r)) tl (+ tl (nth 2 r)) ta (+ ta (nth 3 r))
              rows (cons (list (car r) (itoa (nth 1 r))
                               (al:fmt (/ (nth 2 r) f) 3) (al:fmt (/ (nth 3 r) f f) 3))
                         rows))
      )
      (setq rows (append (list (list (if (= group "Layer") "레이어" "구분") "개수" "길이(m)" "면적(m2)"))
                         (reverse rows)
                         (list (list "합계" (itoa tc) (al:fmt (/ tl f) 3) (al:fmt (/ ta f f) 3)))))
      (al:report "수량집계" rows)
    )
  )
  (al:end)
)

(defun c:ALBCNT (/ ss acc rows tc)
  (al:begin)
  (setq ss (al:select-or-all "\n개수를 셀 블록 선택 (Enter=현재 공간 전체): " '((0 . "INSERT"))))
  (if ss
    (progn
      (foreach e (al:ss->list ss)
        (setq acc (al:acc-add acc (al:block-name e) 1 0.0 0.0))
      )
      (setq acc (vl-sort acc '(lambda (a b) (< (strcase (car a)) (strcase (car b))))))
      (setq tc 0)
      (foreach r acc
        (setq tc (+ tc (nth 1 r)) rows (cons (list (car r) (itoa (nth 1 r))) rows))
      )
      (al:report "블록개수"
                 (append (list (list "블록 이름" "개수")) (reverse rows) (list (list "합계" (itoa tc)))))
    )
  )
  (al:end)
)

;; 문자/치수에서 숫자 하나 읽기
(defun al:ent-number (e / ed typ s)
  (setq ed (entget e) typ (cdr (assoc 0 ed)))
  (cond
    ((= typ "DIMENSION")
     (setq s (cdr (assoc 1 ed)))
     (if (or (= s "") (vl-string-search "<>" s))
       (cdr (assoc 42 ed))
       (al:str-number s)
     ))
    ((= typ "MTEXT") (al:str-number (al:mt-plain (al:mtext-string ed))))
    (T (al:str-number (cdr (assoc 1 ed))))
  )
)

(defun c:ALTSUM (/ ss n sum v prec h pt)
  (al:begin)
  (princ "\n합계를 낼 문자/치수 선택: ")
  (if (setq ss (ssget '((0 . "TEXT,MTEXT,DIMENSION"))))
    (progn
      (setq n 0 sum 0.0)
      (foreach e (al:ss->list ss)
        (if (setq v (al:ent-number e)) (setq n (1+ n) sum (+ sum v)))
      )
      (setq prec (atoi (al:cfg-get "SumPrec" "3")))
      (princ (strcat "\n숫자 " (itoa n) "개 / 합계 = " (al:fmt sum prec)
                     (if (> n 0) (strcat " / 평균 = " (al:fmt (/ sum n) prec)) "")))
      (if (and (> n 0) (al:yes-p "\n합계를 도면에 쓸까요" "No"))
        (progn
          (setq h (cond ((cdr (assoc 40 (entget (ssname ss 0))))) (2.5)))
          (if (setq pt (getpoint "\n합계 문자 위치: "))
            (entmake (list '(0 . "TEXT") (cons 10 (al:p3 pt)) (cons 40 h)
                           (cons 1 (al:fmt sum prec)) (cons 7 (getvar "TEXTSTYLE"))))
          )
        )
      )
    )
  )
  (al:end)
)

;;;; ---- AL_MText.lsp ----
;;; ------------------------------------------------------------------
;;; AL_MText.lsp - 문자 변환
;;;   ALT2M    : TEXT -> MTEXT (여러 줄을 하나로 합치기 / 하나씩 변환)
;;;   ALM2T    : MTEXT -> TEXT (줄마다 TEXT 하나)
;;;   ALMCLEAN : MTEXT 서식(글꼴, 색, 높이 등) 지우기
;;; ------------------------------------------------------------------

;; TEXT 의 왼쪽 위 점 (WCS)
(defun al:text-topleft (ed / tb ang p)
  (setq ang (cdr (assoc 50 ed)) p (cdr (assoc 10 ed)))
  (setq tb (al:catch 'textbox (list ed)))
  (if (null tb) (setq tb (list '(0.0 0.0) (list 0.0 (cdr (assoc 40 ed))))))
  (al:v+ p (al:rot (list (car (car tb)) (cadr (cadr tb))) ang))
)

(defun al:make-mtext (src pt content h ang spacing / chunks ed)
  (setq chunks (al:str-chunks content 250))
  (setq ed (list '(0 . "MTEXT") '(100 . "AcDbEntity") (assoc 8 src)))
  (if (assoc 62 src) (setq ed (append ed (list (assoc 62 src)))))
  (setq ed (append ed (list '(100 . "AcDbMText") (cons 10 (al:p3 pt)) (cons 40 h)
                            '(41 . 0.0) '(71 . 1) '(72 . 1))))
  (foreach c (reverse (cdr (reverse chunks))) (setq ed (append ed (list (cons 3 c)))))
  (setq ed (append ed (list (cons 1 (last chunks)) (assoc 7 src) (cons 50 ang))))
  (if spacing (setq ed (append ed (list '(73 . 2) (cons 44 spacing)))))
  (entmake ed)
)

(defun al:text-content (ed / wf s)
  (setq s (al:mt-escape (cdr (assoc 1 ed))) wf (cdr (assoc 41 ed)))
  (if (and wf (not (equal wf 1.0 1e-6)))
    (strcat "{\\W" (rtos wf 2 3) ";" s "}")
    s
  )
)

(defun c:ALT2M (/ ss lst mode del ed0 ang0 org h items rows content tl ys gap sp n)
  (al:begin)
  (princ "\nMTEXT 로 바꿀 TEXT 선택: ")
  (if (setq ss (ssget '((0 . "TEXT"))))
    (progn
      (setq lst (al:ss->list ss))
      (setq mode (if (> (length lst) 1)
                   (al:kword "\n변환 방식 - 하나로 합치기(M)/각각 변환(E) [Merge/Each] <Merge>: " "Merge Each" "Merge")
                   "Each"))
      (setq del (al:yes-p "\n원본 TEXT 를 삭제할까요" "Yes"))
      (setq n 0)
      (if (= mode "Each")
        (foreach e lst
          (setq ed0 (entget e))
          (if (al:make-mtext ed0 (al:text-topleft ed0) (al:text-content ed0)
                             (cdr (assoc 40 ed0)) (cdr (assoc 50 ed0)) nil)
            (progn (setq n (1+ n)) (if del (entdel e)))
          )
        )
        (progn
          ;; 첫 문자의 회전 방향을 기준 좌표계로 사용
          (setq ed0 (entget (car lst))
                ang0 (cdr (assoc 50 ed0))
                h (cdr (assoc 40 ed0))
                org (al:text-topleft ed0))
          (setq items (mapcar '(lambda (e / ed)
                                 (setq ed (entget e))
                                 (cons (al:rot (al:v- (al:text-topleft ed) org) (- ang0)) ed))
                              lst))
          (setq rows (al:group-rows items (* h 0.5)))
          (setq content
                 (al:str-join
                   (mapcar '(lambda (r) (al:str-join (mapcar '(lambda (it) (al:text-content (cdr it))) r) " "))
                           rows)
                   "\\P"))
          ;; 왼쪽 위 기준점
          (setq tl (list (apply 'min (mapcar 'caar items)) (apply 'max (mapcar 'cadar items))))
          ;; 줄 간격 (원래 문자 간격 유지)
          (if (> (length rows) 1)
            (progn
              (setq ys (mapcar '(lambda (r) (cadr (car (car r)))) rows)
                    gap (/ (- (car ys) (last ys)) (1- (length ys)))
                    sp (max 0.25 (min 4.0 (/ gap (* h 1.6667)))))
            )
          )
          (if (al:make-mtext ed0 (al:v+ org (al:rot tl ang0)) content h ang0 sp)
            (progn
              (setq n 1)
              (if del (foreach e lst (entdel e)))
            )
          )
        )
      )
      (princ (strcat "\nMTEXT " (itoa n) "개 생성"))
    )
  )
  (al:end)
)

(defun al:mtext-angle (ed / d)
  (cond
    ((assoc 50 ed) (cdr (assoc 50 ed)))
    ((setq d (cdr (assoc 11 ed))) (angle '(0.0 0.0) (list (car d) (cadr d))))
    (T 0.0)
  )
)

;; MTEXT 하나를 TEXT 여러 개로. 만든 개수 반환
(defun al:mtext-to-texts (e / ed h ang att lines sp dist w hh col row tl i n p0)
  (setq ed (entget e)
        h (cdr (assoc 40 ed))
        ang (al:mtext-angle ed)
        att (cond ((cdr (assoc 71 ed))) (1))
        p0 (cdr (assoc 10 ed))
        lines (al:mt-paragraphs (al:mtext-string ed))
        sp (cond ((cdr (assoc 44 ed))) (1.0))
        dist (* h 1.6667 sp)
        w (cond ((cdr (assoc 42 ed))) (0.0))
        hh (cond ((cdr (assoc 43 ed))) ((+ h (* dist (1- (length lines))))))
        col (rem (1- att) 3)
        row (/ (1- att) 3)
        tl (list (- (* col w 0.5)) (* row hh 0.5))
        i 0 n 0)
  (foreach ln lines
    (if (/= (al:str-trim ln) "")
      (if (entmake (list '(0 . "TEXT") (assoc 8 ed)
                         (cons 10 (al:p3 (al:v+ p0 (al:rot (list (car tl) (- (cadr tl) h (* i dist))) ang))))
                         (cons 40 h) (cons 1 ln) (cons 50 ang) (assoc 7 ed)))
        (progn
          (setq n (1+ n))
          (if (assoc 62 ed) (entmod (append (entget (entlast)) (list (assoc 62 ed)))))
        )
      )
    )
    (setq i (1+ i))
  )
  n
)

(defun c:ALM2T (/ ss del n k)
  (al:begin)
  (princ "\nTEXT 로 바꿀 MTEXT 선택: ")
  (if (setq ss (ssget '((0 . "MTEXT"))))
    (progn
      (setq del (al:yes-p "\n원본 MTEXT 를 삭제할까요" "Yes") n 0)
      (foreach e (al:ss->list ss)
        (setq k (al:mtext-to-texts e) n (+ n k))
        (if (and del (> k 0)) (entdel e))
      )
      (princ (strcat "\nTEXT " (itoa n) "개 생성"))
    )
  )
  (al:end)
)

(defun c:ALMCLEAN (/ ss n ed s)
  (al:begin)
  (princ "\n서식을 지울 MTEXT 선택: ")
  (if (setq ss (ssget '((0 . "MTEXT"))))
    (progn
      (setq n 0)
      (foreach e (al:ss->list ss)
        (setq ed (entget e)
              s (al:str-join (mapcar 'al:mt-escape (al:mt-paragraphs (al:mtext-string ed))) "\\P"))
        (if (and (/= s (al:mtext-string ed)) (al:ent-text-set ed s)) (setq n (1+ n)))
      )
      (princ (strcat "\nMTEXT " (itoa n) "개 서식 제거"))
    )
  )
  (al:end)
)

;;;; ---- AL_Main.lsp ----
;;; ------------------------------------------------------------------
;;; AL_Main.lsp - 도움말, 로드 메시지
;;; ------------------------------------------------------------------

(setq al:*commands*
  '(("ALPLOT"      "도곽(블록/사각형/창) 일괄 출력, PDF 장별 저장")
    ("ALPLOTL"     "레이아웃 일괄 출력")
    ("ALFTE"       "문자 찾기/바꾸기 대화상자 (ALFIND, ALFR 도 같음)")
    ("-ALFIND"     "문자 찾기 (명령행 버전)")
    ("-ALFR"       "문자 찾아 바꾸기 (명령행 버전)")
    ("ALPURGE"     "도면 정리 (소거 + 빈 문자, 길이 0 객체 삭제)")
    ("ALAUTOPURGE" "저장할 때 자동 소거 켜기/끄기")
    ("ALQTY"       "레이어/종류별 개수, 길이, 면적 집계")
    ("ALBCNT"      "블록 이름별 개수")
    ("ALTSUM"      "선택한 문자/치수의 숫자 합계")
    ("ALT2M"       "TEXT -> MTEXT (합치기/각각)")
    ("ALM2T"       "MTEXT -> TEXT (줄마다)")
    ("ALMCLEAN"    "MTEXT 서식 지우기")
    ("ALHELP"      "명령어 목록")))

(defun c:ALHELP ()
  (princ (strcat "\n===== AutoLisp " al:*version* " 명령어 ====="))
  (foreach c al:*commands*
    (princ (strcat "\n  " (al:pad (car c) 12) (cadr c)))
  )
  (princ "\n=======================================")
  (princ)
)

(al:autopurge-install)
(princ (strcat "\nAutoLisp " al:*version* " 로드 완료. 명령어 목록: ALHELP"))
(princ)
