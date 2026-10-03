"""찾기/바꾸기 엔진(AL_FTE.lsp)을 가짜 도면에서 끝까지 실행해 보는 테스트."""
import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from fakecad import FakeCad  # noqa: E402
from minilisp import Sym, is_nil  # noqa: E402


def make():
    fc = FakeCad()
    fc.load("AL_String.lsp", "AL_Match.lsp", "AL_Core.lsp", "AL_FTE.lsp")
    fc.add_layer("LOCK", locked=True)
    fc.add_layer("OFF", off=True)
    fc.t1 = fc.text("가나다하하하")
    fc.t2 = fc.text("가나다호호")
    fc.t3 = fc.text("가나다라마", layer="LOCK")
    fc.t4 = fc.text("가나다 숨김", layer="OFF")
    fc.mt = fc.mtext("{\\fArial|b1;도면}\\P번호 A-123")
    fc.inner = fc.block("TB", lambda ch: fc.text("블록안 가나다", chain=ch))[0]
    fc.i1 = fc.insert("TB", pt=(100.0, 0.0, 0.0))
    fc.i2 = fc.insert("TB", pt=(200.0, 0.0, 0.0), attribs=[("NO", "가나다-01")])
    fc.dim = fc.add("DIMENSION", c1="가나다<>")
    fc.paper = fc.text("배치의 가나다", space="Layout1")
    return fc


def kinds(res):
    return sorted(r[0] for r in (res or []))


def find(fc, text, **opt):
    fc.set_opt(**opt)
    return fc.call("al:fte-do-find", text) or []


def test_find_default_current_space():
    fc = make()
    res = find(fc, "가나다")
    # 현재 공간(모형): 문자 4, 속성 1, 블록 내부 2(블록 2개), 치수 1
    assert kinds(res) == sorted(["TEXT"] * 4 + ["ATTRIB", "BLOCK", "BLOCK", "DIM"])


def test_find_scopes():
    fc = make()
    assert len(find(fc, "가나다", scope="0")) == 9      # 전체 도면: 배치 문자 포함
    assert len(find(fc, "가나다", scope="3")) == 1      # 배치만
    fc.vars["CTAB"] = "Layout1"
    assert len(find(fc, "가나다", scope="1")) == 1      # 현재 공간 = 배치


def test_find_options():
    fc = make()
    assert len(find(fc, "가나다", foff=True)) == 7                      # 꺼진 레이어 제외
    assert kinds(find(fc, "가나다", foff=False, oneblock=True)).count("BLOCK") == 1
    fc.set_opt(oneblock=False)
    ex = find(fc, "가나다(?!하하)")
    assert fc.t1 not in [r[1] for r in ex] and fc.t2 in [r[1] for r in ex]
    whole = find(fc, "가나다호호", fwhole=True)
    assert [r[1] for r in whole] == [fc.t2]
    fc.set_opt(fwhole=False)
    multi = find(fc, "도면, A-123", fmulti=True)
    assert [r[1] for r in multi] == [fc.mt]
    fc.set_opt(fmulti=False)
    assert [r[1] for r in find(fc, "가나다숨김", fspace=True)] == [fc.t4]
    fc.set_opt(fspace=False)
    assert [r[1] for r in find(fc, "A-###", wild=True)] == [fc.mt]
    fc.set_opt(wild=False)
    assert find(fc, "A-###") == []


def test_find_target_types():
    fc = make()
    res = find(fc, "가나다", t_text=False, t_block=False)
    assert kinds(res) == ["ATTRIB", "DIM"]


def replace(fc, pairs, **opt):
    fc.set_opt(**opt)
    fc.it.g["AL:*FTE-PAIRS*"] = [[a, b] for a, b in pairs]
    return fc.call("al:fte-do-replace") or []


def text_of(fc, e):
    d = fc.ents[e]
    return d.get("chunks", d.get(1))


def test_replace_everything_and_relock():
    fc = make()
    res = replace(fc, [("가나다", "ABC")])
    assert text_of(fc, fc.t1) == "ABC하하하"
    assert text_of(fc, fc.t3) == "ABC라마"            # 잠긴 레이어도 바꿈
    assert fc.layer_locked("LOCK")                     # 다시 잠김
    assert text_of(fc, fc.inner) == "블록안 ABC"       # 블록 정의 한 번만
    assert text_of(fc, fc.dim) == "ABC<>"
    attrib = fc.next[fc.i2]
    assert fc.ents[attrib][1] == "ABC-01"
    assert text_of(fc, fc.paper) == "배치의 가나다"    # 현재 공간 밖은 그대로
    assert len(res) == 7
    assert ("_.REGEN",) in fc.commands
    assert "객체 7개" in fc.it.g["AL:*FTE-STATUS*"]


def test_replace_mtext_keeps_format_when_simple():
    fc = make()
    replace(fc, [("번호", "NO")])
    assert text_of(fc, fc.mt) == "{\\fArial|b1;도면}\\PNO A-123"


def test_replace_mtext_wildcard_drops_format_keeps_lines():
    fc = make()
    replace(fc, [("A-###", "B-999")], wild=True)
    assert text_of(fc, fc.mt) == "도면\\P번호 B-999"


def test_replace_whole_case_and_capture():
    fc = make()
    replace(fc, [("가나다", "X")], rwhole=True)
    assert text_of(fc, fc.t1) == "가나다하하하"
    replace(fc, [("가나다호호", "X")], rwhole=True)
    assert text_of(fc, fc.t2) == "X"
    fc2 = make()
    replace(fc2, [("가나다{*}", "XXX")], wild=True, rwhole=False)
    assert text_of(fc2, fc2.t1) == "가나다XXX"
    assert text_of(fc2, fc2.dim) == "가나다XXX"


def test_replace_ignore_spaces_and_off_layers():
    fc = make()
    replace(fc, [("가나다숨김", "보임")], rspace=True)
    assert text_of(fc, fc.t4) == "보임"
    fc2 = make()
    replace(fc2, [("가나다", "Z")], roff=True)
    assert text_of(fc2, fc2.t4) == "가나다 숨김"


def test_process_delete_and_props():
    fc = make()
    res = find(fc, "가나다", proc="p1")
    left = fc.call("al:fte-process", res)
    assert fc.t1 not in fc.ents and fc.t3 not in fc.ents    # 잠긴 레이어도 삭제
    assert fc.layer_locked("LOCK")
    assert kinds(left) == ["ATTRIB", "BLOCK", "BLOCK", "DIM"]  # TEXT, MTEXT 만 삭제
    fc2 = make()
    res = find(fc2, "가나다호호", proc="p2", ph=True, phv="5", pc=True, pcv="1")
    fc2.call("al:fte-process", res)
    assert fc2.ents[fc2.t2][40] == 5.0 and fc2.ents[fc2.t2][62] == 1


def test_saved_list_names_and_rows():
    fc = make()
    res = find(fc, "가나다호호")
    row = fc.call("al:fte-row", 1, res[0])
    assert row == "1\t문자\t0\t가나다호호"


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
