"""테스트용 가짜 CAD 도면 데이터베이스.

minilisp 인터프리터에 entget / entmod / ssget / tblsearch 등을 흉내 낸 함수를 넣어
CAD 없이 찾기/바꾸기 엔진을 끝까지 실행해 본다. (ActiveX 는 없는 것으로 동작)
"""
import fnmatch
import re

from minilisp import Interp, Sym, LispError, is_nil

T = Sym("T")


class Ename(int):
    def __repr__(self):
        return f"<Entity name: {int(self):x}>"


class CatchErr:
    def __init__(self, msg):
        self.msg = msg


def wcmatch(s, pat):
    for alt in pat.split(","):
        rx = ""
        i = 0
        while i < len(alt):
            c = alt[i]
            if c == "`" and i + 1 < len(alt):
                rx += re.escape(alt[i + 1])
                i += 2
                continue
            if c == "*":
                rx += ".*"
            elif c == "?":
                rx += "."
            elif c == "#":
                rx += r"\d"
            elif c == "[":
                j = alt.index("]", i)
                rx += "[" + re.escape(alt[i + 1:j]).replace("\\-", "-") + "]"
                i = j
            else:
                rx += re.escape(c)
            i += 1
        if re.fullmatch(rx, s, re.S):
            return T
    return None


class FakeCad:
    def __init__(self):
        self.it = Interp()
        self.ents = {}       # ename -> dict(code -> value), 순서 유지
        self.next = {}       # ename -> 다음 ename (같은 체인)
        self.top = []        # 도면 공간 엔티티 순서
        self.blocks = {}     # name -> dict(flags, base, ents)
        self.layers = {}     # name -> dict(flags, color, ename)
        self.env = {}
        self.vars = {"CTAB": "Model", "CMDECHO": 1, "TEXTSTYLE": "Standard", "DWGPREFIX": "C:\\dwg\\",
                     "DWGNAME": "test.dwg", "TEMPPREFIX": "C:\\tmp\\"}
        self.commands = []
        self.seq = 0x1000
        self.add_layer("0")
        self._install()

    # --- 데이터 만들기 ---
    def new_ename(self):
        self.seq += 1
        return Ename(self.seq)

    def add_layer(self, name, locked=False, off=False, frozen=False):
        e = self.new_ename()
        self.layers[name] = {"flags": (4 if locked else 0) | (1 if frozen else 0),
                             "color": -7 if off else 7, "ename": e}
        return e

    def _link(self, chain, e):
        # 최상위 공간은 self.top 순서로 entnext 를 처리하므로 블록 정의 안에서만 연결한다
        if chain and chain is not self.top:
            self.next[chain[-1]] = e
        chain.append(e)

    def add(self, typ, layer="0", space="Model", chain=None, **codes):
        e = self.new_ename()
        d = {0: typ, 8: layer, 410: space}
        d.update({int(k[1:]): v for k, v in codes.items()})
        self.ents[e] = d
        self._link(self.top if chain is None else chain, e)
        return e

    def text(self, s, layer="0", pt=(0.0, 0.0, 0.0), h=2.5, chain=None, space="Model"):
        return self.add("TEXT", layer, space, chain, c1=s, c10=list(pt), c40=h, c41=1.0, c50=0.0, c7="Standard")

    def mtext(self, s, layer="0", pt=(0.0, 0.0, 0.0), h=2.5, chain=None):
        e = self.add("MTEXT", layer, "Model", chain, c10=list(pt), c40=h, c41=0.0, c71=1, c7="Standard", c50=0.0)
        self.ents[e]["chunks"] = s
        return e

    def block(self, name, ents_fn, base=(0.0, 0.0, 0.0)):
        chain = []
        ents_fn(chain)
        self.blocks[name] = {"flags": 0, "base": list(base), "ents": chain}
        return chain

    def insert(self, name, layer="0", pt=(0.0, 0.0, 0.0), attribs=()):
        e = self.add("INSERT", layer, "Model", None, c2=name, c10=list(pt), c41=1.0, c42=1.0, c50=0.0)
        if attribs:
            self.ents[e][66] = 1
            last = e
            for tag, val in attribs:
                a = self.new_ename()
                self.ents[a] = {0: "ATTRIB", 8: layer, 410: "Model", 2: tag, 1: val, 10: list(pt), 40: 2.5}
                self.next[last] = a
                last = a
            s = self.new_ename()
            self.ents[s] = {0: "SEQEND", 8: layer}
            self.next[last] = s
            # SEQEND 다음은 원래 다음 최상위 엔티티 -> top 순서로 처리
            self.ents[e]["_seqend"] = s
        return e

    # --- 엔티티 목록 <-> LISP ---
    def to_lisp(self, e):
        d = self.ents.get(e)
        if d is None:
            return None
        out = [(-1, e)]
        for k, v in d.items():
            if k == "chunks":
                s = v
                chunks = [s[i:i + 250] for i in range(0, len(s), 250)] or [""]
                for c in chunks[:-1]:
                    out.append((3, c))
                out.append((1, chunks[-1]))
            elif isinstance(k, int):
                out.append([k] + v if isinstance(v, list) else (k, v))
        return out

    def from_lisp(self, ed):
        e = None
        d = {}
        chunks = ""
        has_text = False
        for p in ed:
            k = p[0]
            v = p[1] if isinstance(p, tuple) else list(p[1:])
            if k == -1:
                e = v
            elif k == 3:
                chunks += v
            elif k == 1 and d.get(0) == "MTEXT":
                chunks += v
                has_text = True
            else:
                d[k] = v
        if d.get(0) == "MTEXT" or has_text:
            d["chunks"] = chunks
        return e, d

    def layer_locked(self, name):
        return bool(self.layers.get(name, {"flags": 0})["flags"] & 4)

    # --- LISP 함수 ---
    def _install(self):
        g = self.it.g
        fc = self

        def entget(e, *a):
            if e in fc.ents:
                return fc.to_lisp(e)
            for name, l in fc.layers.items():
                if l["ename"] == e:
                    return [(-1, e), (0, "LAYER"), (2, name), (70, l["flags"]), (62, l["color"])]
            return None

        def entmod(ed):
            e = dict(p if isinstance(p, tuple) else (p[0], p[1:]) for p in ed if not isinstance(p, list) or True).get(-1)
            e = next(p[1] for p in ed if p[0] == -1)
            for name, l in fc.layers.items():
                if l["ename"] == e:
                    l["flags"] = next(p[1] for p in ed if p[0] == 70)
                    return ed
            old = fc.ents[e]
            if old[0] != "LAYER" and fc.layer_locked(old[8]):
                return None
            _, d = fc.from_lisp(ed)
            for k in ("_seqend",):
                if k in old:
                    d[k] = old[k]
            fc.ents[e] = d
            return ed

        def entnext(e=None):
            if e is None:
                return fc.top[0] if fc.top else None
            if e in fc.next:
                return fc.next[e]
            # SEQEND 뒤 / 최상위 체인
            for ins, d in fc.ents.items():
                if d.get("_seqend") == e:
                    e = ins
                    break
            if e in fc.top:
                i = fc.top.index(e)
                return fc.top[i + 1] if i + 1 < len(fc.top) else None
            return None

        def ssget(*args):
            if args and args[0] == "X":
                flt = args[1] if len(args) > 1 else []
                out = []
                neg = False
                conds = []
                for p in flt or []:
                    if p == (-4, "<NOT"):
                        neg = True
                    elif p == (-4, "NOT>"):
                        neg = False
                    else:
                        conds.append((p[0], p[1], neg))
                for e in fc.top:
                    d = fc.ents[e]
                    ok = True
                    for k, v, n in conds:
                        if k == 0:
                            r = wcmatch(d[0], v) is not None
                        else:
                            r = str(d.get(k, "")).upper() == str(v).upper()
                        if r == n:
                            ok = False
                    if ok:
                        out.append(e)
                return out or None
            return fc.user_selection

        def tblsearch(tb, name, *a):
            if tb == "BLOCK" and name in fc.blocks:
                b = fc.blocks[name]
                return [(0, "BLOCK"), (2, name), (70, b["flags"]), [10] + b["base"],
                        (-2, b["ents"][0] if b["ents"] else None)]
            if tb == "LAYER" and name in fc.layers:
                return entget(fc.layers[name]["ename"])[1:]
            if tb == "STYLE" and name == "Standard":
                return [(0, "STYLE"), (2, "Standard")]
            return None

        iters = {}

        def tblnext(tb, rewind=None):
            if not is_nil(rewind) or tb not in iters:
                names = list(fc.layers) if tb == "LAYER" else list(fc.blocks) if tb == "BLOCK" else ["Standard"]
                iters[tb] = iter(names)
            try:
                return tblsearch(tb, next(iters[tb]))
            except StopIteration:
                return None

        def tblobjname(tb, name):
            return fc.layers[name]["ename"] if tb == "LAYER" and name in fc.layers else None

        def catch_apply(f, args):
            try:
                return fc.it.call(f, list(args or []))
            except Exception as e:  # noqa: BLE001
                return CatchErr(str(e))

        def entdel(e):
            if e in fc.ents and not fc.layer_locked(fc.ents[e][8]):
                del fc.ents[e]
                if e in fc.top:
                    fc.top.remove(e)
                return e
            return None

        g.update({
            "ENTGET": entget, "ENTMOD": entmod, "ENTNEXT": entnext, "ENTUPD": lambda e: e,
            "ENTDEL": entdel,
            "SSGET": ssget, "SSLENGTH": lambda ss: len(ss), "SSNAME": lambda ss, i: ss[i] if i < len(ss) else None,
            "SSADD": lambda *a: (a[1].append(a[0]) or a[1]) if len(a) == 2 else [],
            "TBLSEARCH": tblsearch, "TBLNEXT": tblnext, "TBLOBJNAME": tblobjname,
            "WCMATCH": lambda s, p: wcmatch(s, p),
            "LOGAND": lambda *a: __import__("functools").reduce(lambda x, y: x & y, a),
            "LOGIOR": lambda *a: __import__("functools").reduce(lambda x, y: x | y, a),
            "GETVAR": lambda v: fc.vars.get(v.upper()),
            "SETVAR": lambda v, x: fc.vars.__setitem__(v.upper(), x) or x,
            "GETENV": lambda k: fc.env.get(k), "SETENV": lambda k, v: fc.env.__setitem__(k, v) or v,
            "COMMAND": lambda *a: fc.commands.append(a),
            "VL-CATCH-ALL-APPLY": catch_apply,
            "VL-CATCH-ALL-ERROR-P": lambda x: T if isinstance(x, CatchErr) else None,
            "VL-STRING-RIGHT-TRIM": lambda chars, s: s.rstrip(chars),
            "ALERT": lambda m: None, "REDRAW": lambda *a: None, "PRINC": lambda *a: a[0] if a else None,
        })
        g["TYPE"] = self._type_with_ename(g["TYPE"])
        self.user_selection = None

    @staticmethod
    def _type_with_ename(orig):
        def f(x):
            if isinstance(x, Ename):
                return Sym("ENAME")
            return orig(x)
        return f

    def load(self, *files):
        import pathlib
        src = pathlib.Path(__file__).resolve().parents[1] / "src"
        for f in files:
            self.it.load((src / f).read_text(encoding="utf-8"))

    def call(self, name, *args):
        return self.it.call(Sym(name.upper()), list(args))

    def set_opt(self, **kw):
        for k, v in kw.items():
            self.env["AutoLisp_FTE_" + k] = ("1" if v else "0") if isinstance(v, bool) else str(v)


__all__ = ["FakeCad", "Ename", "LispError", "is_nil"]
