"""src/*.lsp 를 하나로 합쳐 dist/ 에 배포용 파일을 만든다.

  dist/AutoLisp.lsp       : CP949(ANSI) - 한국어 Windows 의 모든 CAD 에서 기본으로 사용
  dist/AutoLisp_UTF8.lsp  : UTF-8(BOM)  - AutoCAD 2021 이상(LISPSYS=1)에서 한글이 깨질 때 사용

사용법: python build.py
"""
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parent
ORDER = ["AL_String", "AL_Match", "AL_Core", "AL_Plot", "AL_Find", "@AL_FTE.dcl", "AL_FTE",
         "AL_Purge", "AL_Qty", "AL_MText", "AL_Main"]

HEADER = """;;; ==================================================================
;;; AutoLisp - CAD 작업 도우미 (출력, 찾기/바꾸기, 정리, 수량, 문자 변환)
;;; AutoCAD / GstarCAD / ZWCAD / BricsCAD 공용 AutoLISP
;;; 이 파일은 build.py 로 생성됩니다. 수정은 src/ 폴더에서 하세요.
;;; ==================================================================
"""


def check_parens(name, text):
    """괄호 짝, 문자열 닫힘을 검사한다."""
    depth = 0
    i = 0
    line = 1
    in_str = False
    str_line = 0
    while i < len(text):
        c = text[i]
        if c == "\n":
            line += 1
        if in_str:
            if c == "\\":
                i += 2
                continue
            if c == '"':
                in_str = False
        elif c == ";":
            while i < len(text) and text[i] != "\n":
                i += 1
            continue
        elif c == '"':
            in_str = True
            str_line = line
        elif c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth < 0:
                raise SystemExit(f"{name}:{line}: 닫는 괄호가 너무 많습니다")
        i += 1
    if in_str:
        raise SystemExit(f"{name}:{str_line}: 문자열이 닫히지 않았습니다")
    if depth != 0:
        raise SystemExit(f"{name}: 여는 괄호 {depth}개가 닫히지 않았습니다")


def dcl_to_lisp(name):
    """DCL 파일을 (setq al:*dcl-fte* '("줄" ...)) 형태의 LISP 코드로."""
    lines = (ROOT / "src" / name).read_text(encoding="utf-8").splitlines()
    out = ["(setq al:*dcl-fte* '("]
    for ln in lines:
        if ln.strip().startswith("//") or not ln.strip():
            continue
        out.append('  "' + ln.replace("\\", "\\\\").replace('"', '\\"') + '"')
    out.append("))")
    return "\n".join(out) + "\n"


def build():
    parts = [HEADER]
    for name in ORDER:
        if name.startswith("@"):
            parts.append(f"\n;;;; ---- {name[1:]} (대화상자 정의) ----\n")
            parts.append(dcl_to_lisp(name[1:]))
            continue
        text = (ROOT / "src" / f"{name}.lsp").read_text(encoding="utf-8")
        check_parens(name, text)
        parts.append(f"\n;;;; ---- {name}.lsp ----\n")
        parts.append(text)
    out = "".join(parts).replace("\r\n", "\n").replace("\n", "\r\n")
    dist = ROOT / "dist"
    dist.mkdir(exist_ok=True)
    (dist / "AutoLisp.lsp").write_bytes(out.encode("cp949"))
    (dist / "AutoLisp_UTF8.lsp").write_bytes(out.encode("utf-8-sig"))
    print("dist/AutoLisp.lsp, dist/AutoLisp_UTF8.lsp 생성 완료")


if __name__ == "__main__":
    build()
    sys.exit(0)
