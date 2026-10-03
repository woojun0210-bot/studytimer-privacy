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
