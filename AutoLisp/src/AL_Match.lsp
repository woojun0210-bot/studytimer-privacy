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
