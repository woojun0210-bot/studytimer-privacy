"""AL_String.lsp 단위 테스트.  실행: python -m pytest tests  또는  python tests/test_string.py"""
import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from minilisp import Interp, Sym, is_nil  # noqa: E402

SRC = (pathlib.Path(__file__).resolve().parents[1] / "src" / "AL_String.lsp").read_text(encoding="utf-8")


def to_ansi(s):
    """유니코드 문자열을 CP949 바이트 문자열(바이트 1개 = 문자 1개)로."""
    return s.encode("cp949").decode("latin-1")


def from_ansi(s):
    return s.encode("latin-1").decode("cp949")


def lisp(ansi=False):
    it = Interp()
    it.load(to_ansi(SRC) if ansi else SRC)
    return it


def call(it, name, *args):
    return it.call(Sym(name.upper()), list(args))


def test_dbcs_detect():
    assert is_nil(lisp().g["AL:*DBCS*"])
    assert lisp(ansi=True).g["AL:*DBCS*"] == "T"


def test_replace():
    it = lisp()
    assert call(it, "al:str-replace", "abcABCabc", "abc", "X", "T", None) == ["XABCX", 2]
    assert call(it, "al:str-replace", "abcABCabc", "abc", "X", None, None) == ["XXX", 3]
    assert call(it, "al:str-replace", "aaa", "a", "aa", "T", None) == ["aaaaaa", 3]
    assert call(it, "al:str-replace", "abc", "", "X", "T", None) == ["abc", 0]
    assert call(it, "al:str-replace", "도면-01 도면", "도면", "DWG", "T", None) == ["DWG-01 DWG", 2]
    # MTEXT: \P 의 P 는 바꾸지 않는다
    assert call(it, "al:str-replace", "P1\\PP2", "P", "Q", "T", "T") == ["Q1\\PQ2", 2]
    # 이스케이프된 백슬래시 뒤의 P 는 일반 문자
    assert call(it, "al:str-replace", "a\\\\P", "P", "Q", "T", "T") == ["a\\\\Q", 1]


def test_split_join():
    it = lisp()
    assert call(it, "al:str-split", "a,b,,c", ",") == ["a", "b", "", "c"]
    assert call(it, "al:str-split", "a\\Pb", "\\P") == ["a", "b"]
    assert call(it, "al:str-join", ["", "b", "c"], "-") == "-b-c"
    assert call(it, "al:str-join", None, "-") == ""


def test_chunks_unicode():
    it = lisp()
    s = "x" * 600
    chunks = call(it, "al:str-chunks", s, 250)
    assert [len(c) for c in chunks] == [250, 250, 100]
    assert call(it, "al:str-chunks", "abc", 250) == ["abc"]


def test_chunks_ansi_never_split_hangul():
    it = lisp(ansi=True)
    text = "a" + "가나다라" * 100  # 1 + 800 바이트
    chunks = call(it, "al:str-chunks", to_ansi(text), 250)
    assert "".join(chunks) == to_ansi(text)
    for c in chunks:
        assert len(c) <= 250
        from_ansi(c)  # 깨진 글자가 있으면 UnicodeDecodeError


def test_mt_paragraphs():
    it = lisp()
    p = lambda s: call(it, "al:mt-paragraphs", s)  # noqa: E731
    assert p("{\\fArial|b1|i0;Hello} \\PWorld\\~X") == ["Hello ", "World X"]
    assert p("\\A1;\\H2.5x;\\C1;빨강\\P\\P끝") == ["빨강", "", "끝"]
    assert p("1\\S1^2;인치") == ["11/2인치"]
    assert p("a\\\\b\\{c\\}") == ["a\\b{c}"]
    assert p("\\U+00B0C \\Lunder\\l") == ["\\U+00B0C under"]
    assert p("") == [""]
    assert p("끝\\") == ["끝\\"]


def test_mt_paragraphs_ansi():
    it = lisp(ansi=True)
    r = call(it, "al:mt-paragraphs", to_ansi("{\\f굴림|b0;도면}\\P번호"))
    assert [from_ansi(x) for x in r] == ["도면", "번호"]


def test_mt_escape_roundtrip():
    it = lisp()
    for s in ["a{b}c", "x\\y", "\\U+00B0", "보통 문자", ""]:
        esc = call(it, "al:mt-escape", s)
        assert call(it, "al:mt-paragraphs", esc) == [s]
    assert call(it, "al:mt-escape", "a{b}\\c") == "a\\{b\\}\\\\c"


def test_str_number():
    it = lisp()
    n = lambda s: call(it, "al:str-number", s)  # noqa: E731
    assert n("L=1,234.5m") == 1234.5
    assert n("-.5") == -0.5
    assert n("EL.-12.3") == -12.3
    assert n("abc") is None
    assert n("1.2.3") == 1.2
    assert n("A-B 7") == 7.0
    assert n("") is None
    assert n("길이 25") == 25.0


def test_parse_ranges():
    it = lisp()
    assert call(it, "al:parse-ranges", "1,3-5,9", 6) == [1, 3, 4, 5]
    assert call(it, "al:parse-ranges", "5-3, 3", 10) == [3, 4, 5]
    assert is_nil(call(it, "al:parse-ranges", "0", 10))


def test_acc_add():
    it = lisp()
    acc = call(it, "al:acc-add", None, "A", 1, 2.0, 3.0)
    acc = call(it, "al:acc-add", acc, "B", 1, 1.0, 0.0)
    acc = call(it, "al:acc-add", acc, "A", 1, 2.0, 1.0)
    assert sorted(acc) == [["A", 2, 4.0, 4.0], ["B", 1, 1.0, 0.0]]


def test_sort_order():
    it = lisp()
    # 2행 2열 배치:  a(0,10) b(10,10) / c(0,0) d(10,0)  (y 가 조금씩 어긋남)
    items = [[[10, 0.2], "d"], [[0, 10], "a"], [[0, -0.1], "c"], [[10, 10.3], "b"]]
    z = call(it, "al:sort-order", items, 1.0, "Z")
    assert [x[1] for x in z] == ["a", "b", "c", "d"]
    n = call(it, "al:sort-order", items, 1.0, "N")
    assert [x[1] for x in n] == ["a", "c", "b", "d"]


def test_csv_field():
    it = lisp()
    assert call(it, "al:csv-field", "abc") == "abc"
    assert call(it, "al:csv-field", "a,b") == '"a,b"'
    assert call(it, "al:csv-field", 'a"b') == '"a""b"'


if __name__ == "__main__":
    failed = 0
    for name, fn in list(globals().items()):
        if name.startswith("test_"):
            try:
                fn()
                print("ok  ", name)
            except Exception as e:  # noqa: BLE001
                failed += 1
                print("FAIL", name, repr(e))
    sys.exit(1 if failed else 0)
