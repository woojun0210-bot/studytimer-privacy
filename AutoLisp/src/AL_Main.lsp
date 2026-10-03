;;; ------------------------------------------------------------------
;;; AL_Main.lsp - 도움말, 로드 메시지
;;; ------------------------------------------------------------------

(setq al:*commands*
  '(("ALPLOT"      "도곽(블록/사각형/창) 일괄 출력, PDF 장별 저장")
    ("ALPLOTL"     "레이아웃 일괄 출력")
    ("ALFIND"      "문자 찾기 (하나씩 확대, 모두 선택)")
    ("ALFR"        "문자 찾아 바꾸기 (문자/속성/치수/지시선/블록 내부)")
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
