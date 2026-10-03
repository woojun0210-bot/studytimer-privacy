"""AutoLISP 의 일부만 흉내 내는 테스트용 인터프리터.

CAD 없이 src/AL_String.lsp 의 순수 문자열/목록 함수를 검증하는 데 쓴다.
ansi=True 이면 문자열을 CP949 바이트 단위로 다뤄 ANSI(DBCS) 모드를 흉내 낸다.
"""
import math


class Sym(str):
    pass


class LispError(Exception):
    pass


T = Sym("T")


def is_nil(x):
    return x is None or x == []


class Reader:
    def __init__(self, text):
        self.s = text
        self.i = 0

    def skip(self):
        s = self.s
        while self.i < len(s):
            c = s[self.i]
            if c in " \t\r\n":
                self.i += 1
            elif c == ";":
                while self.i < len(s) and s[self.i] != "\n":
                    self.i += 1
            else:
                break

    def read_all(self):
        out = []
        while True:
            self.skip()
            if self.i >= len(self.s):
                return out
            out.append(self.read())

    def read(self):
        self.skip()
        c = self.s[self.i]
        if c == "(":
            self.i += 1
            items = []
            while True:
                self.skip()
                if self.s[self.i] == ")":
                    self.i += 1
                    return items
                items.append(self.read())
        if c == ")":
            raise LispError("unexpected )")
        if c == "'":
            self.i += 1
            return [Sym("QUOTE"), self.read()]
        if c == '"':
            return self.read_string()
        start = self.i
        while self.i < len(self.s) and self.s[self.i] not in " \t\r\n()';\"":
            self.i += 1
        tok = self.s[start:self.i]
        try:
            return int(tok)
        except ValueError:
            pass
        try:
            return float(tok)
        except ValueError:
            pass
        return Sym(tok.upper())

    def read_string(self):
        self.i += 1
        out = []
        s = self.s
        while s[self.i] != '"':
            c = s[self.i]
            if c == "\\":
                n = s[self.i + 1]
                if n in "01234567":
                    j = self.i + 1
                    while j < len(s) and j < self.i + 4 and s[j] in "01234567":
                        j += 1
                    out.append(chr(int(s[self.i + 1:j], 8)))
                    self.i = j
                    continue
                out.append({"n": "\n", "t": "\t", "r": "\r", "e": "\x1b"}.get(n, n))
                self.i += 2
                continue
            out.append(c)
            self.i += 1
        self.i += 1
        return out and "".join(out) or ""


class Lambda:
    def __init__(self, params, locs, body):
        self.params, self.locs, self.body = params, locs, body


class Interp:
    def __init__(self):
        self.g = {"T": T, "NIL": None}
        b = self.g

        def lst(x):
            return [] if x is None else x

        def num_eq(a, b_):
            return a == b_

        def truth(x):
            return T if x else None

        b["+"] = lambda *a: sum(a) if a else 0
        b["-"] = lambda a, *r: (a - sum(r)) if r else -a
        b["*"] = lambda *a: math.prod(a)
        b["/"] = self._div
        b["1+"] = lambda a: a + 1
        b["1-"] = lambda a: a - 1
        b["REM"] = lambda a, c: math.fmod(a, c) if isinstance(a, float) or isinstance(c, float) else int(math.fmod(a, c))
        b["ABS"] = abs
        b["MIN"] = min
        b["MAX"] = max
        b["="] = lambda a, *r: truth(all(num_eq(a, x) for x in r))
        b["/="] = lambda a, c: truth(a != c)
        b["<"] = lambda *a: truth(all(a[i] < a[i + 1] for i in range(len(a) - 1)))
        b[">"] = lambda *a: truth(all(a[i] > a[i + 1] for i in range(len(a) - 1)))
        b["<="] = lambda *a: truth(all(a[i] <= a[i + 1] for i in range(len(a) - 1)))
        b[">="] = lambda *a: truth(all(a[i] >= a[i + 1] for i in range(len(a) - 1)))
        b["NOT"] = lambda a: truth(is_nil(a))
        b["NULL"] = b["NOT"]
        b["EQ"] = lambda a, c: truth(a is c or (not isinstance(a, list) and a == c))
        b["EQUAL"] = lambda a, c, *f: truth(a == c if not f else abs(a - c) <= f[0])
        b["NUMBERP"] = lambda a: truth(isinstance(a, (int, float)))
        b["STRCAT"] = lambda *a: "".join(a)
        b["STRLEN"] = lambda *a: sum(len(x) for x in a)
        b["SUBSTR"] = lambda s, st, n=None: s[st - 1:] if n is None else s[st - 1:st - 1 + n]
        b["STRCASE"] = lambda s, lower=None: s.lower() if not is_nil(lower) else s.upper()
        b["VL-STRING-SEARCH"] = self._search
        b["VL-STRING-TRIM"] = lambda chars, s: s.strip(chars)
        b["ASCII"] = lambda s: ord(s[0]) if s else 0
        b["CHR"] = chr
        b["ATOI"] = self._atoi
        b["ATOF"] = self._atof
        b["ITOA"] = lambda n: str(n)
        b["RTOS"] = lambda x, mode=2, prec=4: f"{x:.{prec}f}"
        b["CONS"] = lambda a, c: [a] + lst(c) if isinstance(c, list) or c is None else (a, c)
        b["LIST"] = lambda *a: list(a) if a else None
        b["CAR"] = lambda a: None if is_nil(a) else a[0]
        b["CDR"] = lambda a: None if is_nil(a) or len(a) == 1 else a[1:]
        b["CADR"] = lambda a: b["CAR"](b["CDR"](a))
        b["CADDR"] = lambda a: b["CAR"](b["CDR"](b["CDR"](a)))
        b["CDDR"] = lambda a: b["CDR"](b["CDR"](a))
        b["CAAR"] = lambda a: b["CAR"](b["CAR"](a))
        b["CADAR"] = lambda a: b["CADR"](b["CAR"](a))
        b["LAST"] = lambda a: lst(a)[-1] if not is_nil(a) else None
        b["NTH"] = lambda n, a: lst(a)[n] if n < len(lst(a)) else None
        b["LENGTH"] = lambda a: len(lst(a))
        b["REVERSE"] = lambda a: list(reversed(lst(a))) or None
        b["APPEND"] = lambda *a: [x for l in a for x in lst(l)] or None
        b["MEMBER"] = lambda x, l: next((lst(l)[i:] for i, y in enumerate(lst(l)) if y == x), None)
        b["ASSOC"] = lambda k, l: next((y for y in lst(l) if isinstance(y, (list, tuple)) and y and y[0] == k), None)
        b["SUBST"] = lambda new, old, l: [new if y == old else y for y in lst(l)] or None
        b["VL-POSITION"] = lambda x, l: next((i for i, y in enumerate(lst(l)) if y == x), None)
        b["VL-SORT"] = lambda l, f: self._sort(lst(l), f)
        b["VL-REMOVE-IF"] = lambda f, l: [x for x in lst(l) if is_nil(self.call(f, [x]))] or None
        b["VL-REMOVE-IF-NOT"] = lambda f, l: [x for x in lst(l) if not is_nil(self.call(f, [x]))] or None
        b["VL-SOME"] = lambda f, l: next((r for r in (self.call(f, [x]) for x in lst(l)) if not is_nil(r)), None)
        b["MAPCAR"] = lambda f, *ls: [self.call(f, list(a)) for a in zip(*[lst(x) for x in ls])] or None
        b["APPLY"] = lambda f, a: self.call(f, lst(a))
        b["PRINC"] = lambda *a: a[0] if a else None

    def _div(self, a, *r):
        for x in r:
            if isinstance(a, int) and isinstance(x, int):
                a = int(a / x)
            else:
                a = a / x
        return a

    def _search(self, pat, s, start=0):
        r = s.find(pat, start)
        return None if r < 0 else r

    def _atoi(self, s):
        s = s.strip()
        n = ""
        for i, c in enumerate(s):
            if c.isdigit() or (i == 0 and c in "+-"):
                n += c
            else:
                break
        try:
            return int(n)
        except ValueError:
            return 0

    def _atof(self, s):
        try:
            return float(s)
        except ValueError:
            return 0.0

    def _sort(self, l, f):
        import functools

        def cmp(a, c):
            if not is_nil(self.call(f, [a, c])):
                return -1
            if not is_nil(self.call(f, [c, a])):
                return 1
            return 0
        return sorted(l, key=functools.cmp_to_key(cmp)) or None

    # --- 평가 ---
    def call(self, f, args):
        if isinstance(f, Sym):
            f = self.g[f.upper()]
        if isinstance(f, list) and f and f[0] == "LAMBDA":
            f = self.make_lambda(f[1], f[2:])
        if isinstance(f, Lambda):
            names = f.params + f.locs
            saved = {n: self.g.get(n, None) for n in names}
            for n in f.locs:
                self.g[n] = None
            for n, v in zip(f.params, args + [None] * (len(f.params) - len(args))):
                self.g[n] = v
            try:
                r = None
                for form in f.body:
                    r = self.eval(form)
                return r
            finally:
                self.g.update(saved)
        return f(*args)

    def make_lambda(self, plist, body):
        plist = plist or []
        if Sym("/") in plist:
            k = plist.index(Sym("/"))
            return Lambda(plist[:k], plist[k + 1:], body)
        return Lambda(plist, [], body)

    def eval(self, x):
        if isinstance(x, Sym):
            return self.g.get(x, None)
        if not isinstance(x, list):
            return x
        if not x:
            return None
        h = x[0]
        if h == "QUOTE":
            return x[1]
        if h == "FUNCTION":
            return x[1]
        if h == "SETQ":
            v = None
            for i in range(1, len(x), 2):
                v = self.eval(x[i + 1])
                self.g[x[i]] = v
            return v
        if h == "DEFUN":
            self.g[x[1]] = self.make_lambda(x[2], x[3:])
            return x[1]
        if h == "LAMBDA":
            return self.make_lambda(x[1], x[2:])
        if h == "IF":
            if not is_nil(self.eval(x[1])):
                return self.eval(x[2])
            return self.eval(x[3]) if len(x) > 3 else None
        if h == "COND":
            for clause in x[1:]:
                v = self.eval(clause[0])
                if not is_nil(v):
                    for form in clause[1:]:
                        v = self.eval(form)
                    return v
            return None
        if h == "WHILE":
            r = None
            while not is_nil(self.eval(x[1])):
                for form in x[2:]:
                    r = self.eval(form)
            return r
        if h == "REPEAT":
            r = None
            for _ in range(self.eval(x[1])):
                for form in x[2:]:
                    r = self.eval(form)
            return r
        if h == "PROGN":
            r = None
            for form in x[1:]:
                r = self.eval(form)
            return r
        if h == "AND":
            for form in x[1:]:
                if is_nil(self.eval(form)):
                    return None
            return T
        if h == "OR":
            for form in x[1:]:
                if not is_nil(self.eval(form)):
                    return T
            return None
        if h == "FOREACH":
            var, seq = x[1], self.eval(x[2])
            saved = self.g.get(var)
            r = None
            try:
                for item in seq or []:
                    self.g[var] = item
                    for form in x[3:]:
                        r = self.eval(form)
            finally:
                self.g[var] = saved
            return r
        f = self.g.get(h) if isinstance(h, Sym) else self.eval(h)
        if f is None:
            raise LispError(f"no function: {h}")
        return self.call(f, [self.eval(a) for a in x[1:]])

    def load(self, text):
        for form in Reader(text).read_all():
            self.eval(form)

    def run(self, src):
        r = None
        for form in Reader(src).read_all():
            r = self.eval(form)
        return r
