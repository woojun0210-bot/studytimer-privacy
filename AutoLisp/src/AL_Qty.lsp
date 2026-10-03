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
