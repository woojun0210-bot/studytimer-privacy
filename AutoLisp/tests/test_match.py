"""AL_Match.lsp (찾기 패턴) 테스트. DreamPlus FTE 매뉴얼의 예시를 그대로 검증한다."""
import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from minilisp import Interp, Sym, is_nil  # noqa: E402

SRC_DIR = pathlib.Path(__file__).resolve().parents[1] / "src"
SRC = "".join((SRC_DIR / f).read_text(encoding="utf-8") for f in ["AL_String.lsp", "AL_Match.lsp"])


def to_ansi(s):
    return s.encode("cp949").decode("latin-1")


def from_ansi(s):
    return s.encode("latin-1").decode("cp949")


class L:
    def __init__(self, ansi=False):
        self.ansi = ansi
        self.it = Interp()
        self.it.load(to_ansi(SRC) if ansi else SRC)

    def enc(self, s):
        return to_ansi(s) if self.ansi else s

    def dec(self, s):
        return from_ansi(s) if self.ansi else s

    def call(self, name, *args):
        return self.it.call(Sym(name.upper()), list(args))

    def cp(self, pat, wild=False, whole=False, cs=False):
        t = lambda b: Sym("T") if b else None  # noqa: E731
        return self.call("al:pm-compile", self.enc(pat), t(wild), t(whole), t(cs))

    def find(self, pat, text, **kw):
        cs = kw.get("cs", False)
        return not is_nil(self.call("al:pm-test", self.cp(pat, **kw), self.enc(text), Sym("T") if cs else None))

    def replace(self, pat, text, repl, **kw):
        cs = kw.get("cs", False)
        r = self.call("al:pm-replace", self.cp(pat, **kw), self.enc(text), self.enc(repl), Sym("T") if cs else None)
        return self.dec(r[0]), r[1]


def both(fn):
    """유니코드 모드와 ANSI(CP949) 모드 둘 다 실행."""
    def run():
        fn(L())
        fn(L(ansi=True))
    run.__name__ = fn.__name__
    return run


@both
def test_plain_find(l):
    assert l.find("가나다", "가나다하하하")
    assert l.find("abc", "xxABCxx")
    assert not l.find("abc", "xxABCxx", cs=True)
    assert not l.find("없음", "가나다")


@both
def test_whole_match(l):
    # 매뉴얼: '캐드 드림플러스' 에서 '캐드' 는 전체 일치일 때 못 찾음
    assert l.find("캐드", "캐드 드림플러스")
    assert not l.find("캐드", "캐드 드림플러스", whole=True)
    assert l.find("캐드 드림플러스", "캐드 드림플러스", whole=True)


@both
def test_exclusion(l):
    # 매뉴얼: 가나다(?!하하) -> '가나다하하하' 제외, 나머지는 찾음
    assert not l.find("가나다(?!하하)", "가나다하하하")
    assert l.find("가나다(?!하하)", "가나다호호")
    assert l.find("가나다(?!하하)", "가나다라마")
    assert not l.find("가나다(?!하하|호호)", "가나다호호")
    assert l.find("가나다(?!하하|호호)", "가나다라마")


@both
def test_wild_digits(l):
    # 매뉴얼: ### 은 연속 세 자리 숫자, #.12 는 0.12 / 1.123 / 2.12
    assert l.find("###", "A-123", wild=True)
    assert not l.find("###", "A-12", wild=True)
    for t in ["0.12", "1.123", "2.12"]:
        assert l.find("#.12", t, wild=True)
    assert not l.find("#.12", "1.21", wild=True)
    # 와일드카드를 끄면 # 은 일반 문자
    assert not l.find("###", "A-123")
    assert l.find("#1", "번호#1")


@both
def test_wild_star_replace(l):
    # 매뉴얼: '가나다*' -> 'XXX'
    assert l.replace("가나다*", "가나다하하하", "XXX", wild=True) == ("XXX", 1)
    assert l.replace("가나다*", "가나다호호", "XXX", wild=True) == ("XXX", 1)
    assert l.replace("가나다*", "차카타파하", "XXX", wild=True) == ("차카타파하", 0)
    assert l.find("A*", "ABC", wild=True)


@both
def test_wild_digit_replace(l):
    # 매뉴얼: "x###x" -> "x150x"
    assert l.replace("x###x", "1x200x3.000", "x150x", wild=True) == ("1x150x3.000", 1)
    assert l.replace("x###x", "1x210x4.000", "x150x", wild=True) == ("1x150x4.000", 1)


@both
def test_capture_replace(l):
    # 매뉴얼: '가나다{*}' -> 뒤 부분만 치환, '가나다' 뿐이면 바꾸지 않음
    assert l.replace("가나다{*}", "가나다하하하", "XXX", wild=True) == ("가나다XXX", 1)
    assert l.replace("가나다{*}", "가나다", "XXX", wild=True) == ("가나다", 0)
    assert l.replace("{*}가나다", "앞가나다", "XXX", wild=True) == ("XXX가나다", 1)
    assert l.replace("{*}가나다{*}", "앞가나다뒤", "XXX", wild=True) == ("XXX가나다XXX", 1)


@both
def test_plain_replace(l):
    assert l.replace("ab", "abcABcab", "Z") == ("ZcZcZ", 3)
    assert l.replace("ab", "abcABcab", "Z", cs=True) == ("ZcABcZ", 2)
    assert l.replace("도면", "도면-01 도면", "DWG") == ("DWG-01 DWG", 2)
    assert l.replace("캐드", "캐드 드림", "X", whole=True) == ("캐드 드림", 0)
    assert l.replace("캐드 드림", "캐드 드림", "X", whole=True) == ("X", 1)
    assert l.replace("가나다(?!하하)", "가나다하하 가나다라", "X") == ("가나다하하 X라", 1)


def test_ansi_no_false_match_inside_hangul():
    l = L(ansi=True)
    # CP949 에서 '각'(B0 A2) + '가'(B0 A1) 처럼 바이트 경계가 어긋난 가짜 일치가 없어야 한다
    text = "각가"
    raw = l.enc(text)
    for i in range(1, len(raw)):
        piece = raw[i:i + 2]
        try:
            ch = from_ansi(piece)
        except UnicodeDecodeError:
            continue
        if ch not in text:
            assert not l.find(ch, text, wild=True)


def test_num_to_str():
    l = L()
    assert l.call("al:num->str", 150.0) == "150"
    assert l.call("al:num->str", 3.25) == "3.25"
    assert l.call("al:num->str", 12) == "12"


if __name__ == "__main__":
    failed = 0
    for name, fn in list(globals().items()):
        if name.startswith("test_"):
            try:
                fn()
                print("ok  ", name)
            except Exception as e:  # noqa: BLE001
                failed += 1
                import traceback
                traceback.print_exc()
                print("FAIL", name, repr(e))
    sys.exit(1 if failed else 0)
