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
