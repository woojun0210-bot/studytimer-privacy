"""AL_FTE.dcl 문법 검사 + LSP 에서 쓰는 타일 키가 DCL 에 모두 있는지 확인."""
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[1]
DCL = (ROOT / "src" / "AL_FTE.dcl").read_text(encoding="utf-8")
LSP = (ROOT / "src" / "AL_FTE.lsp").read_text(encoding="utf-8")

KNOWN_TILES = {"dialog", "row", "column", "boxed_row", "boxed_column", "radio_row", "radio_column",
               "boxed_radio_row", "button", "edit_box", "list_box", "popup_list", "toggle",
               "radio_button", "text", "spacer", "image", "image_button", "slider", "concatenation",
               "paragraph", "text_part", "errtile"}
KNOWN_ATTRS = {"label", "key", "width", "height", "edit_width", "edit_limit", "fixed_width",
               "fixed_height", "is_default", "is_cancel", "allow_accept", "multiple_select",
               "tabs", "alignment", "value", "mnemonic", "is_enabled", "list", "children_alignment"}
KNOWN_PREDEF = {"ok_cancel", "ok_only", "ok_cancel_help", "spacer", "spacer_1", "spacer_0", "errtile"}


def tokenize(text):
    text = re.sub(r"//[^\n]*", "", text)
    tok_re = r'"(?:\\.|[^"\\])*"|[A-Za-z_][A-Za-z0-9_]*|-?\d+(?:\.\d+)?|[:{}=;]'
    rest = re.sub(tok_re, "", text).strip()
    assert not rest, f"DCL 에 알 수 없는 문자가 있습니다: {rest[:20]!r}"
    return re.findall(tok_re, text)


class Parser:
    def __init__(self, toks):
        self.t, self.i = toks, 0
        self.keys = []
        self.dialogs = []

    def peek(self):
        return self.t[self.i] if self.i < len(self.t) else None

    def take(self, expect=None):
        tok = self.t[self.i]
        if expect and tok != expect:
            raise AssertionError(f"'{expect}' 가 필요한데 '{tok}' (토큰 {self.i})")
        self.i += 1
        return tok

    def file(self):
        while self.peek():
            name = self.take()
            self.take(":")
            kind = self.take()
            assert kind == "dialog", kind
            self.dialogs.append(name)
            self.body()

    def body(self):
        self.take("{")
        while self.peek() != "}":
            if self.peek() == ":":
                self.take(":")
                kind = self.take()
                assert kind in KNOWN_TILES, f"알 수 없는 타일: {kind}"
                self.body()
            else:
                name = self.take()
                if self.peek() == ";":
                    assert name in KNOWN_PREDEF, f"알 수 없는 미리 정의 타일: {name}"
                    self.take(";")
                    continue
                assert name in KNOWN_ATTRS, f"알 수 없는 속성: {name}"
                self.take("=")
                val = self.take()
                self.take(";")
                if name == "key":
                    self.keys.append(val.strip('"'))
        self.take("}")


def parse():
    p = Parser(tokenize(DCL))
    p.file()
    return p


def test_dcl_syntax_and_unique_keys():
    p = parse()
    assert p.dialogs == ["al_fte", "al_fte_set"]
    dup = {k for k in p.keys if p.keys.count(k) > 1}
    assert not dup, f"중복 키: {dup}"


def lisp_keys():
    keys = set()
    pat = r'\((?:action_tile|set_tile|get_tile|mode_tile|al:fill-list|start_list)\s+\\?"([a-z_0-9]+)\\?"'
    keys |= set(re.findall(pat, LSP))
    for var in ["al:\\*fte-main-toggles\\*", "al:\\*fte-set-toggles\\*"]:
        m = re.search(r"\(setq " + var + r"\s+'\(([^)]*)\)", LSP)
        keys |= set(re.findall(r'"([a-z_0-9]+)"', m.group(1)))
    keys |= {"zoom", "phv", "pwv", "pcv"}
    keys -= {"accept", "cancel"} if "cancel" not in parse().keys else {"accept"}
    return keys


def test_all_lisp_keys_exist_in_dcl():
    dcl_keys = set(parse().keys)
    missing = lisp_keys() - dcl_keys
    assert not missing, f"DCL 에 없는 키: {sorted(missing)}"


def test_main_buttons_have_actions():
    dcl = parse().keys
    actions = set(re.findall(r'\(action_tile\s+"([a-z_0-9]+)"', LSP))
    buttons = set(re.findall(r':\s*button\s*\{\s*key\s*=\s*"([a-z_0-9]+)"', DCL))
    assert buttons <= actions | {"accept"}, f"동작이 없는 버튼: {sorted(buttons - actions)}"
    assert dcl


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
