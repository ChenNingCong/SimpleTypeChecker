"""
Sparse Backward Value Flow — interactive demo.

A tiny imperative language is parsed, def-use edges are built, and a
type query traverses the use-def graph backward on demand. Every step
of the traversal is recorded and surfaced to the UI so the algorithm's
structure is visible.

Run:
    pip install flask
    python app.py
    # open http://127.0.0.1:5000

The whole demo lives in this one file plus templates/index.html and
static/style.css. See README.md for the algorithm.
"""

import re
from flask import Flask, render_template, request, jsonify


# ============================================================
# Types — the abstract value lattice
# ============================================================

class Type:
    def __eq__(self, other): return type(self) is type(other)
    def __hash__(self): return hash(type(self).__name__)


class IntType(Type):
    def __repr__(self): return 'Int'


class StrType(Type):
    def __repr__(self): return 'Str'


class BoolType(Type):
    def __repr__(self): return 'Bool'


class AnyType(Type):
    def __repr__(self): return 'Any'


class BottomType(Type):
    """The "unknown yet" placeholder returned when cycle-break fires.
    Acts as the identity element for union, which is why cycles widen
    correctly without explicit fixpoint iteration in phase 2."""
    def __repr__(self): return '⊥'


class UnionType(Type):
    def __init__(self, members):
        self.members = frozenset(members)

    def __repr__(self):
        return 'Union[' + ', '.join(sorted(repr(m) for m in self.members)) + ']'

    def __eq__(self, other):
        return isinstance(other, UnionType) and self.members == other.members

    def __hash__(self):
        return hash(self.members)


INT = IntType()
STR = StrType()
BOOL = BoolType()
ANY = AnyType()
BOT = BottomType()


def union(*types):
    """Lattice join. ⊥ is the identity (drops out); Any is absorbing."""
    flat = []
    for t in types:
        if isinstance(t, UnionType):
            flat.extend(t.members)
        elif isinstance(t, BottomType):
            continue
        else:
            flat.append(t)
    if not flat:
        return BOT
    if any(isinstance(t, AnyType) for t in flat):
        return ANY
    distinct = set(flat)
    if len(distinct) == 1:
        return next(iter(distinct))
    return UnionType(distinct)


def apply_binop(op, lt, rt):
    """Operator type signatures. Loose on purpose — the focus is the
    value-flow walk, not arithmetic typing."""
    if op in ('+', '-', '*', '/'):
        if isinstance(lt, IntType) and isinstance(rt, IntType):
            return INT
        if op == '+' and isinstance(lt, StrType) and isinstance(rt, StrType):
            return STR
        return ANY
    if op in ('<', '>', '<=', '>=', '==', '!='):
        return BOOL
    return ANY


# ============================================================
# AST nodes — every node gets a numeric id used by the UI
# ============================================================

class Node:
    _counter = 0

    def __init__(self):
        Node._counter += 1
        self.id = Node._counter
        self.line = 0
        # Character offsets into the source string. The UI uses these to
        # highlight exactly the span being analyzed at each step.
        self.start_off = 0
        self.end_off = 0


class IntLit(Node):
    def __init__(self, value):
        super().__init__()
        self.value = value


class StrLit(Node):
    def __init__(self, value):
        super().__init__()
        self.value = value


class BoolLit(Node):
    def __init__(self, value):
        super().__init__()
        self.value = value


class Var(Node):
    def __init__(self, name):
        super().__init__()
        self.name = name
        # Filled in by build_reaching_defs() — the set of Let/Assign/ParamDef
        # nodes whose value may flow into this use. The whole sparse-backward
        # query rides this list.
        self.reaching_defs = []


class Binop(Node):
    def __init__(self, op, left, right):
        super().__init__()
        self.op = op
        self.left = left
        self.right = right


class Call(Node):
    def __init__(self, fname, args):
        super().__init__()
        self.fname = fname
        self.args = args


class TypePred(Node):
    """`expr is TypeName` — a type predicate. When used as the condition of
    an `if`, it narrows the variable's type inside the branches: the then-
    arm refines to `TypeName`; the else-arm refines away from it. This is
    the simplest form of type narrowing — only `<Var> is <Type>` triggers
    narrowing on the named variable; anything more complex is parsed but
    has no narrowing effect."""
    def __init__(self, expr, tname):
        super().__init__()
        self.expr = expr
        self.tname = tname  # one of 'Int', 'Str', 'Bool'


class Let(Node):
    def __init__(self, name, expr):
        super().__init__()
        self.name = name
        self.expr = expr


class Assign(Node):
    def __init__(self, name, expr):
        super().__init__()
        self.name = name
        self.expr = expr


class If(Node):
    def __init__(self, cond, then_block, else_block):
        super().__init__()
        self.cond = cond
        self.then_block = then_block
        self.else_block = else_block


class While(Node):
    def __init__(self, cond, body):
        super().__init__()
        self.cond = cond
        self.body = body


class Return(Node):
    def __init__(self, expr):
        super().__init__()
        self.expr = expr


class FunDef(Node):
    def __init__(self, name, params, body):
        super().__init__()
        self.name = name
        self.params = params
        self.body = body


class Program(Node):
    def __init__(self, funcs):
        super().__init__()
        self.funcs = funcs


class ParamDef(Node):
    """Synthesized at fn entry. Represents the formal parameter's def-site."""
    def __init__(self, name, fn):
        super().__init__()
        self.name = name
        self.fn = fn


# ============================================================
# Lexer + Parser
# ============================================================

TOKEN_SPEC = [
    ('STR',     r'"[^"]*"'),
    ('NUM',     r'\d+'),
    ('IDENT',   r'[a-zA-Z_][a-zA-Z_0-9]*'),
    ('OP',      r'==|!=|<=|>=|<|>|=|\+|-|\*|/'),
    ('LBRACE',  r'\{'),
    ('RBRACE',  r'\}'),
    ('LPAREN',  r'\('),
    ('RPAREN',  r'\)'),
    ('COMMA',   r','),
    ('COMMENT', r'//[^\n]*'),
    ('WS',      r'[ \t\n]+'),
]

KEYWORDS = {'fn', 'let', 'if', 'else', 'while', 'return', 'true', 'false', 'is'}

# Type names allowed on the RHS of an `is` predicate.
TYPENAME_TO_TYPE = {'Int': INT, 'Str': STR, 'Bool': BOOL}


def tokenize(src):
    pat = '|'.join(f'(?P<{n}>{p})' for n, p in TOKEN_SPEC)
    out = []
    for m in re.finditer(pat, src):
        kind = m.lastgroup
        val = m.group()
        if kind in ('WS', 'COMMENT'):
            continue
        line = src.count('\n', 0, m.start()) + 1
        if kind == 'IDENT' and val in KEYWORDS:
            kind = val.upper()
        # Tuple shape: (kind, val, line, start_off, end_off). The offsets
        # propagate into every AST node so the UI can underline the exact
        # source span the analyzer is looking at.
        out.append((kind, val, line, m.start(), m.end()))
    out.append(('EOF', None, 0, len(src), len(src)))
    return out


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def peek(self, k=0):
        return self.tokens[self.pos + k]

    def consume(self):
        t = self.tokens[self.pos]
        self.pos += 1
        return t

    def expect(self, kind):
        t = self.consume()
        if t[0] != kind:
            raise SyntaxError(f"expected {kind}, got {t[0]} ({t[1]!r}) at line {t[2]}")
        return t

    def parse_program(self):
        funcs = []
        while self.peek()[0] != 'EOF':
            funcs.append(self.parse_fundef())
        return Program(funcs)

    def _last(self):
        """Return the most recently consumed token (used for end_off)."""
        return self.tokens[self.pos - 1]

    def parse_fundef(self):
        ftok = self.expect('FN')
        name = self.expect('IDENT')[1]
        self.expect('LPAREN')
        params = []
        if self.peek()[0] != 'RPAREN':
            params.append(self.expect('IDENT')[1])
            while self.peek()[0] == 'COMMA':
                self.consume()
                params.append(self.expect('IDENT')[1])
        self.expect('RPAREN')
        body = self.parse_block()
        node = FunDef(name, params, body)
        node.line = ftok[2]
        node.start_off = ftok[3]
        node.end_off = self._last()[4]   # the '}' that parse_block consumed
        return node

    def parse_block(self):
        self.expect('LBRACE')
        stmts = []
        while self.peek()[0] != 'RBRACE':
            stmts.append(self.parse_stmt())
        self.expect('RBRACE')
        return stmts

    def parse_stmt(self):
        t = self.peek()
        if t[0] == 'LET':
            kw = self.consume()
            name_tok = self.expect('IDENT')
            eq = self.consume()
            if eq[0] != 'OP' or eq[1] != '=':
                raise SyntaxError(f"expected '=' after let, got {eq}")
            expr = self.parse_expr()
            n = Let(name_tok[1], expr)
            n.line = kw[2]
            n.start_off = kw[3]
            n.end_off = expr.end_off
            return n
        if t[0] == 'IF':
            kw = self.consume()
            cond = self.parse_expr()
            then_block = self.parse_block()
            end_off = self._last()[4]
            else_block = []
            if self.peek()[0] == 'ELSE':
                self.consume()
                else_block = self.parse_block()
                end_off = self._last()[4]
            n = If(cond, then_block, else_block)
            n.line = kw[2]
            n.start_off = kw[3]
            n.end_off = end_off
            return n
        if t[0] == 'WHILE':
            kw = self.consume()
            cond = self.parse_expr()
            body = self.parse_block()
            n = While(cond, body)
            n.line = kw[2]
            n.start_off = kw[3]
            n.end_off = self._last()[4]
            return n
        if t[0] == 'RETURN':
            kw = self.consume()
            expr = self.parse_expr()
            n = Return(expr)
            n.line = kw[2]
            n.start_off = kw[3]
            n.end_off = expr.end_off
            return n
        if t[0] == 'IDENT':
            # assignment `x = ...` vs expression statement (e.g. call)
            nxt = self.peek(1)
            if nxt[0] == 'OP' and nxt[1] == '=':
                name_tok = self.consume()
                self.consume()  # '='
                expr = self.parse_expr()
                n = Assign(name_tok[1], expr)
                n.line = name_tok[2]
                n.start_off = name_tok[3]
                n.end_off = expr.end_off
                return n
            return self.parse_expr()
        raise SyntaxError(f"unexpected token {t}")

    def parse_expr(self):
        return self.parse_cmp()

    def _binop_chain(self, sub, ops):
        left = sub()
        while self.peek()[0] == 'OP' and self.peek()[1] in ops:
            op = self.consume()[1]
            right = sub()
            n = Binop(op, left, right)
            n.line = left.line
            n.start_off = left.start_off
            n.end_off = right.end_off
            left = n
        return left

    def parse_cmp(self):
        left = self._binop_chain(self.parse_add, ('<', '>', '<=', '>=', '==', '!='))
        # Optional `is TypeName` suffix — produces a TypePred node which the
        # analyzer recognizes as the condition of an enclosing `if`.
        if self.peek()[0] == 'IS':
            kw = self.consume()
            type_tok = self.expect('IDENT')
            tname = type_tok[1]
            if tname not in TYPENAME_TO_TYPE:
                raise SyntaxError(
                    f"`is` predicate expects one of "
                    f"{sorted(TYPENAME_TO_TYPE)}, got {tname!r} "
                    f"at line {kw[2]}")
            n = TypePred(left, tname)
            n.line = left.line
            n.start_off = left.start_off
            n.end_off = type_tok[4]
            return n
        return left

    def parse_add(self):
        return self._binop_chain(self.parse_mul, ('+', '-'))

    def parse_mul(self):
        return self._binop_chain(self.parse_atom, ('*', '/'))

    def parse_atom(self):
        t = self.peek()
        if t[0] == 'NUM':
            self.consume()
            n = IntLit(int(t[1]))
            n.line = t[2]; n.start_off = t[3]; n.end_off = t[4]; return n
        if t[0] == 'STR':
            self.consume()
            n = StrLit(t[1][1:-1])
            n.line = t[2]; n.start_off = t[3]; n.end_off = t[4]; return n
        if t[0] == 'TRUE':
            self.consume()
            n = BoolLit(True)
            n.line = t[2]; n.start_off = t[3]; n.end_off = t[4]; return n
        if t[0] == 'FALSE':
            self.consume()
            n = BoolLit(False)
            n.line = t[2]; n.start_off = t[3]; n.end_off = t[4]; return n
        if t[0] == 'IDENT':
            self.consume()
            if self.peek()[0] == 'LPAREN':
                self.consume()
                args = []
                if self.peek()[0] != 'RPAREN':
                    args.append(self.parse_expr())
                    while self.peek()[0] == 'COMMA':
                        self.consume()
                        args.append(self.parse_expr())
                self.expect('RPAREN')
                n = Call(t[1], args)
                n.line = t[2]; n.start_off = t[3]; n.end_off = self._last()[4]
                return n
            n = Var(t[1])
            n.line = t[2]; n.start_off = t[3]; n.end_off = t[4]; return n
        if t[0] == 'LPAREN':
            self.consume()
            e = self.parse_expr()
            self.expect('RPAREN')
            return e
        raise SyntaxError(f"unexpected token in expression: {t}")


# ============================================================
# Phase 1 — build reaching defs (forward, one pass with loop fixpoint)
# ============================================================

def build_reaching_defs(program):
    """For each Var node, set `reaching_defs` to the list of Let/Assign/
    ParamDef nodes whose value may flow into the use.

    The env maps variable name -> list of def nodes. Branches snapshot
    and merge; loops iterate the merge until stable.
    """
    for fn in program.funcs:
        env = {}
        for p in fn.params:
            env[p] = [ParamDef(p, fn)]
        analyze_block(fn.body, env)


def analyze_block(block, env):
    for stmt in block:
        analyze_stmt(stmt, env)


def analyze_stmt(stmt, env):
    if isinstance(stmt, Let):
        analyze_expr(stmt.expr, env)
        env[stmt.name] = [stmt]
        return
    if isinstance(stmt, Assign):
        analyze_expr(stmt.expr, env)
        env[stmt.name] = [stmt]
        return
    if isinstance(stmt, If):
        analyze_expr(stmt.cond, env)

        # If the cond is `<Var> is Type`, snapshot the variable's reaching
        # defs at branch entry. After analyzing each arm we tag every Var use
        # of that name whose defs still match the snapshot — the snapshot
        # equality check correctly suppresses narrowing when the branch body
        # reassigns the variable before the use.
        pred = stmt.cond if isinstance(stmt.cond, TypePred) else None
        pred_var = (pred.expr.name
                    if pred is not None and isinstance(pred.expr, Var)
                    else None)
        orig_defs = list(env.get(pred_var, [])) if pred_var is not None else None

        env_then = {k: list(v) for k, v in env.items()}
        env_else = {k: list(v) for k, v in env.items()}
        analyze_block(stmt.then_block, env_then)
        analyze_block(stmt.else_block, env_else)

        if pred_var is not None:
            _tag_narrowings(stmt.then_block, pred_var, orig_defs,
                            ('to', pred.tname, pred))
            _tag_narrowings(stmt.else_block, pred_var, orig_defs,
                            ('not', pred.tname, pred))

        merged = {}
        for k in set(env_then) | set(env_else):
            merged[k] = merge_defs(env_then.get(k, []), env_else.get(k, []))
        env.clear()
        env.update(merged)
        return
    if isinstance(stmt, While):
        # Iterate the merge of pre-loop env and body-end env until stable.
        # On convergence, env already holds the post-merge state (from the
        # previous iteration's update) and each Var inside the loop has had
        # its reaching_defs set against that env — no re-pass needed.
        for _ in range(20):
            body_env = {k: list(v) for k, v in env.items()}
            analyze_expr(stmt.cond, body_env)
            analyze_block(stmt.body, body_env)
            new_env = {}
            for k in set(env) | set(body_env):
                new_env[k] = merge_defs(env.get(k, []), body_env.get(k, []))
            if envs_equal(new_env, env):
                return
            env.clear()
            env.update(new_env)
        return
    if isinstance(stmt, Return):
        analyze_expr(stmt.expr, env)
        return
    # bare expression statement (e.g. a call)
    analyze_expr(stmt, env)


def merge_defs(a, b):
    seen = set()
    out = []
    for d in a + b:
        if id(d) in seen:
            continue
        seen.add(id(d))
        out.append(d)
    return out


def envs_equal(a, b):
    if set(a) != set(b):
        return False
    for k in a:
        if {id(d) for d in a[k]} != {id(d) for d in b[k]}:
            return False
    return True


def analyze_expr(expr, env):
    if isinstance(expr, (IntLit, StrLit, BoolLit)):
        return
    if isinstance(expr, Var):
        expr.reaching_defs = list(env.get(expr.name, []))
        return
    if isinstance(expr, Binop):
        analyze_expr(expr.left, env)
        analyze_expr(expr.right, env)
        return
    if isinstance(expr, Call):
        for a in expr.args:
            analyze_expr(a, env)
        return
    if isinstance(expr, TypePred):
        analyze_expr(expr.expr, env)
        return


def _tag_narrowings(block, name, orig_defs, narrowing):
    """After analyzing a branch arm, attach `narrowing` to each Var use of
    `name` whose reaching defs still match the snapshot at branch entry.

    The snapshot equality check is the load-bearing piece: if the arm body
    reassigns the variable before the use, the use's reaching defs no
    longer match `orig_defs` and the narrowing is correctly suppressed.

    Inner narrowings win — once a Var carries a narrowing tag, an outer
    pass doesn't overwrite it. The result is a single narrowing per use
    site (good enough for the "simple cases" this demo covers; combining
    nested predicates would be a separate layer)."""
    orig_ids = {id(d) for d in orig_defs or []}
    for stmt in block:
        for n in walk_all(stmt):
            if (isinstance(n, Var)
                    and n.name == name
                    and {id(d) for d in n.reaching_defs} == orig_ids
                    and getattr(n, 'narrowing', None) is None):
                n.narrowing = narrowing


def narrow_type(base, mode, target):
    """Refine `base` against the type predicate's target type.

    mode='to'  → keep union members compatible with `target`.
    mode='not' → drop union members equal to `target`.

    For a non-union base, behaves as if the base were Union[base]. Any
    narrowed `to T` becomes T; Any narrowed `not T` stays Any (the demo
    doesn't track structured negation)."""
    if isinstance(base, BottomType):
        return BOT
    if isinstance(base, UnionType):
        if mode == 'to':
            kept = [m for m in base.members if m == target]
        else:
            kept = [m for m in base.members if m != target]
        if not kept:
            return BOT
        if len(kept) == 1:
            return next(iter(kept))
        return UnionType(kept)
    if isinstance(base, AnyType):
        return target if mode == 'to' else ANY
    # singleton concrete type
    if mode == 'to':
        return base if base == target else BOT
    return BOT if base == target else base


# ============================================================
# Phase 2 — demand-driven type query, with step logging
# ============================================================

class Analyzer:
    def __init__(self, program):
        self.program = program
        self.cache = {}              # id(node) -> Type
        self.signature_cache = {}    # function name -> Type
        self.resolving = set()       # ids currently being computed
        self.steps = []              # log of every rule application
        self.visited = set()         # ids ever touched by the query

    def step(self, node, rule, why, action, result=None):
        """Record one step. The UI renders these in the format the user
        asked for: rule cited + "we infer X because Y"."""
        nid = node.id if node is not None and hasattr(node, 'id') else None
        self.steps.append({
            'node_id': nid,
            'rule': rule,
            'why': why,
            'action': action,
            'result': repr(result) if result is not None else None,
            # visited_so_far is the snapshot of every node touched up to and
            # including this step. The UI uses it to render the expanding
            # "trail" of visited nodes in a lighter shade.
            'visited_so_far': list(self.visited),
            'cache': {
                str(k): {
                    'label': node_label(self._node_by_id(k)),
                    'line': getattr(self._node_by_id(k), 'line', 0),
                    'type': repr(v),
                }
                for k, v in self.cache.items()
            },
            'resolving': sorted(self.resolving, key=str),
        })

    def _node_by_id(self, target_id):
        # Walk the program to find a node by id. O(N) per step but the
        # tree is tiny and this is for UI display only.
        if not hasattr(self, '_id_index'):
            self._id_index = {}
            for n in walk_all(self.program):
                self._id_index[n.id] = n
        return self._id_index.get(target_id)

    # --- the entry point: get_type(node) ---

    def get_type(self, node):
        nid = node.id
        if nid in self.cache:
            t = self.cache[nid]
            self.step(node, 'CACHE-HIT',
                      f"the cache already holds {node_label(node)}.",
                      "Reusing the stored answer; no re-computation.",
                      t)
            return t
        if nid in self.resolving:
            self.step(node, 'CYCLE-BREAK',
                      f"we re-entered {node_label(node)} while it is still "
                      f"being computed — this is the back edge of a while loop.",
                      "Return ⊥ (Bottom) so the outer union widens without "
                      "looping forever.",
                      BOT)
            return BOT

        self.visited.add(nid)
        self.resolving.add(nid)
        try:
            t = self._compute(node)
            self.cache[nid] = t
            return t
        finally:
            self.resolving.discard(nid)

    # --- per-rule dispatch ---

    def _compute(self, node):
        if isinstance(node, IntLit):
            self.step(node, 'LITERAL-INT',
                      f"the source token {node.value} is an integer literal.",
                      "Map the token to its primitive type Int.",
                      INT)
            return INT
        if isinstance(node, StrLit):
            self.step(node, 'LITERAL-STR',
                      f"the source token {node.value!r} is a string literal.",
                      "Map the token to its primitive type Str.",
                      STR)
            return STR
        if isinstance(node, BoolLit):
            self.step(node, 'LITERAL-BOOL',
                      f"the source token {node.value} is a boolean literal.",
                      "Map the token to its primitive type Bool.",
                      BOOL)
            return BOOL
        if isinstance(node, Var):
            return self._compute_var(node)
        if isinstance(node, Binop):
            return self._compute_binop(node)
        if isinstance(node, Call):
            return self._compute_call(node)
        if isinstance(node, (Let, Assign)):
            self.step(node, 'ASSIGN-TYPE',
                      f"{node_label(node)} carries the type of its RHS.",
                      "Recurse into the RHS expression.")
            return self.get_type(node.expr)
        if isinstance(node, ParamDef):
            self.step(node, 'PARAM',
                      f"parameter {node.name!r} of fn {node.fn.name} has no "
                      "annotation and this demo skips parameter inference.",
                      "Return Any as a placeholder.",
                      ANY)
            return ANY
        if isinstance(node, Return):
            self.step(node, 'RETURN',
                      "a return statement's type is the type of its expression.",
                      "Recurse into the returned expression.")
            return self.get_type(node.expr)
        if isinstance(node, TypePred):
            # As a value, `e is T` is Bool. The interesting use is as an
            # `if` condition where Phase 1 has already tagged the matching
            # Var uses in each arm with a narrowing.
            self.step(node, 'TYPEPRED',
                      f"a type-predicate expression `... is {node.tname}`.",
                      "Its value-as-expression is Bool. (Its narrowing effect "
                      "is recorded on Var uses inside the enclosing branch.)",
                      BOOL)
            return BOOL

    def _compute_var(self, node):
        base = self._compute_var_base(node)
        narrowing = getattr(node, 'narrowing', None)
        if narrowing is None:
            return base
        return self._apply_narrowing(node, base, narrowing)

    def _compute_var_base(self, node):
        defs = node.reaching_defs
        if not defs:
            self.step(node, 'VAR-UNDEF',
                      f"variable {node.name!r} has no reaching def.",
                      "Return Any.",
                      ANY)
            return ANY
        if len(defs) == 1:
            d = defs[0]
            self.step(node, 'VAR-SINGLE-DEF',
                      f"variable {node.name!r} has exactly one reaching def "
                      f"({node_label(d)} on line {d.line}).",
                      "Recurse into that single def.")
            return self.get_type(d)
        self.step(node, 'PHI-JOIN-ENTER',
                  f"variable {node.name!r} has {len(defs)} reaching defs from "
                  "different control-flow paths.",
                  "Recurse into each def in turn, then take the union of "
                  "their types.")
        types = [self.get_type(d) for d in defs]
        result = union(*types)
        self.step(node, 'PHI-JOIN',
                  "every reaching def has been typed; combine them.",
                  f"union({', '.join(repr(t) for t in types)}) = {result}.",
                  result)
        return result

    def _apply_narrowing(self, node, base, narrowing):
        """Apply a branch-scoped type predicate to the Var's computed type.

        Two steps are emitted so the trace makes the new dependency visible:
        NARROW-INPUT highlights the predicate (and marks it visited — this
        is what "sparseness now takes the condition as input" means in
        concrete terms), then NARROW-THEN/-ELSE refines the type."""
        mode, tname, pred = narrowing
        target = TYPENAME_TO_TYPE[tname]
        self.visited.add(pred.id)
        arm = 'then' if mode == 'to' else 'else'
        sign = 'true' if mode == 'to' else 'false'
        op_repr = '∩' if mode == 'to' else '−'
        self.step(pred, 'NARROW-INPUT',
                  f"this use of {node.name!r} sits inside the {arm}-arm of "
                  f"`if {node.name} is {tname}`, so the predicate is {sign} "
                  "here.",
                  "Pull the predicate into the query as an extra input — "
                  "the sparse walk now depends on the condition, not just "
                  "the def-use edges.")
        narrowed = narrow_type(base, mode, target)
        self.step(node, f'NARROW-{arm.upper()}',
                  f"the predicate constrains {node.name!r} at this site.",
                  f"Refine the computed type: {base} {op_repr} {tname} → "
                  f"{narrowed}.",
                  narrowed)
        return narrowed

    def _compute_binop(self, node):
        self.step(node, 'BINOP-ENTER',
                  f"binary operator {node.op!r}; the result depends on both "
                  "operand types.",
                  "Recurse into the left operand, then the right operand.")
        lt = self.get_type(node.left)
        rt = self.get_type(node.right)
        result = apply_binop(node.op, lt, rt)
        self.step(node, f'BINOP-{node.op}',
                  f"both operand types are known: left={lt}, right={rt}.",
                  f"Apply the operator's type signature: {lt} {node.op} {rt} → {result}.",
                  result)
        return result

    def _compute_call(self, node):
        self.step(node, 'CALL-ENTER',
                  f"call to {node.fname}(...); the result is the callee's "
                  "return type.",
                  "Look up or compute the callee's signature.")
        sig = self._sig_of(node.fname)
        self.step(node, 'CALL-RESULT',
                  f"the callee {node.fname} has return type {sig}.",
                  "Use the signature as this call's type.",
                  sig)
        return sig

    def _sig_of(self, fname):
        if fname in self.signature_cache:
            t = self.signature_cache[fname]
            self.step(None, 'SIG-CACHE-HIT',
                      f"signature of {fname} is already cached.",
                      "Reuse the cached return type.",
                      t)
            return t
        key = f'__sig__:{fname}'
        if key in self.resolving:
            self.step(None, 'SIG-CYCLE-BREAK',
                      f"recursive call back into {fname} while computing its "
                      "signature.",
                      "Return ⊥; the outer union widens past it.",
                      BOT)
            return BOT
        fn = next((f for f in self.program.funcs if f.name == fname), None)
        if fn is None:
            self.step(None, 'SIG-MISSING',
                      f"no function named {fname} in the program.",
                      "Treat as Any.",
                      ANY)
            return ANY
        self.resolving.add(key)
        try:
            self.step(fn, 'SIG-COMPUTE-ENTER',
                      f"first request for {fname}'s signature.",
                      "Scan body for return statements; recurse into each.")
            returns = find_returns(fn.body)
            ret_types = [self.get_type(r.expr) for r in returns]
            t = union(*ret_types) if ret_types else ANY
            self.signature_cache[fname] = t
            self.step(fn, 'SIG-CACHE-WRITE',
                      f"every return in {fname} has been typed.",
                      f"union of return types is {t}; store it on the function symbol.",
                      t)
            return t
        finally:
            self.resolving.discard(key)


def find_returns(block):
    out = []
    for s in block:
        if isinstance(s, Return):
            out.append(s)
        elif isinstance(s, If):
            out.extend(find_returns(s.then_block))
            out.extend(find_returns(s.else_block))
        elif isinstance(s, While):
            out.extend(find_returns(s.body))
    return out


def walk_all(node):
    """Yield every Node in the tree."""
    yield node
    if isinstance(node, Program):
        for f in node.funcs:
            yield from walk_all(f)
    elif isinstance(node, FunDef):
        for s in node.body:
            yield from walk_all(s)
    elif isinstance(node, If):
        yield from walk_all(node.cond)
        for s in node.then_block:
            yield from walk_all(s)
        for s in node.else_block:
            yield from walk_all(s)
    elif isinstance(node, While):
        yield from walk_all(node.cond)
        for s in node.body:
            yield from walk_all(s)
    elif isinstance(node, (Let, Assign, Return)):
        yield from walk_all(node.expr)
    elif isinstance(node, Binop):
        yield from walk_all(node.left)
        yield from walk_all(node.right)
    elif isinstance(node, Call):
        for a in node.args:
            yield from walk_all(a)
    elif isinstance(node, TypePred):
        yield from walk_all(node.expr)


def node_label(n):
    if n is None:
        return '?'
    if isinstance(n, IntLit): return f"IntLit({n.value})"
    if isinstance(n, StrLit): return f"StrLit({n.value!r})"
    if isinstance(n, BoolLit): return f"BoolLit({n.value})"
    if isinstance(n, Var): return f"Var({n.name})"
    if isinstance(n, Binop): return f"Binop({n.op})"
    if isinstance(n, Call): return f"Call({n.fname})"
    if isinstance(n, TypePred): return f"TypePred(is {n.tname})"
    if isinstance(n, Let): return f"Let({n.name})"
    if isinstance(n, Assign): return f"Assign({n.name})"
    if isinstance(n, If): return "If"
    if isinstance(n, While): return "While"
    if isinstance(n, Return): return "Return"
    if isinstance(n, FunDef): return f"FunDef({n.name})"
    if isinstance(n, ParamDef): return f"ParamDef({n.name})"
    return type(n).__name__


def serialize_ast(program):
    """Flat node list for the UI to draw the visited / unvisited map.

    Includes start_off/end_off so the UI can highlight the exact source
    span of the node being analyzed at each step.
    """
    out = []
    for n in walk_all(program):
        out.append({
            'id': n.id,
            'kind': type(n).__name__,
            'label': node_label(n),
            'line': getattr(n, 'line', 0),
            'start_off': getattr(n, 'start_off', 0),
            'end_off': getattr(n, 'end_off', 0),
        })
    return out


# ============================================================
# Flask app
# ============================================================

def _load_example(name):
    """Read example source from disk so the .fake files are the source of
    truth (instead of duplicating them inline)."""
    import os
    path = os.path.join(os.path.dirname(__file__), 'examples', f'{name}.fake')
    with open(path) as f:
        return f.read().rstrip('\n')


EXAMPLE_NAMES = [
    # A. Linear data flow
    '01_linear_int', '02_linear_str', '03_arith_chain',
    '04_var_reassign', '05_unused_let',
    # B. Branches and PHI-JOIN
    '06_if_else_widen', '07_no_else', '08_nested_branch',
    '09_branch_other_var', '10_branch_same_type',
    # C. While loops
    '11_while_counter', '12_while_widen', '13_while_with_if',
    '14_nested_while', '15_while_no_widen',
    # D. Single function-call patterns
    '16_call_once', '17_call_twice', '18_call_nested',
    '19_call_in_expr', '20_call_in_let',
    # E. Genuine interprocedural
    '21_chain_3level', '22_diamond', '23_wide_diamond',
    '24_branch_calls_diff', '25_fn_union_returns',
    # F. Recursion
    '26_recur_base', '27_mutual_recur', '28_recur_one_branch',
    '29_recur_via_loop', '30_recur_with_arith',
    # G. Mixed control flow
    '31_if_in_while', '32_while_in_if', '33_call_in_loop',
    '34_call_in_branch', '35_complex_synth',
    # H. Sparseness showcases
    '36_wide_dead_calls', '37_dead_function', '38_dead_variable',
    '39_long_program', '40_final_showcase',
    # I. Type narrowing — the condition becomes an input to the query
    '41_narrow_then', '42_narrow_else', '43_narrow_flow_join',
    '44_narrow_both_arms', '45_narrow_in_callee', '46_narrow_after_loop',
    '47_narrow_reassign_guard', '48_narrow_two_vars',
]

EXAMPLES = {n: _load_example(n) for n in EXAMPLE_NAMES}

app = Flask(__name__)


@app.route('/')
def index():
    return render_template('index.html', examples=EXAMPLES)


@app.route('/analyze', methods=['POST'])
def analyze():
    data = request.get_json()
    src = data.get('src', '')
    try:
        Node._counter = 0
        tokens = tokenize(src)
        parser = Parser(tokens)
        program = parser.parse_program()
        build_reaching_defs(program)

        main = next((f for f in program.funcs if f.name == 'main'), None)
        if main is None:
            return jsonify({'error': "program has no `main` function"})
        returns = find_returns(main.body)
        if not returns:
            return jsonify({'error': "`main` has no `return` statement"})

        analyzer = Analyzer(program)
        # The program's type is main's signature — same SIG-COMPUTE/union
        # machinery the analyzer already uses for any other callee. This
        # correctly unions across multiple return statements in main, which
        # matters for branchy mains (and for narrowing examples that put
        # different return types in each arm).
        result = analyzer._sig_of('main')
        ast = serialize_ast(program)
        return jsonify({
            'steps': analyzer.steps,
            'final_type': repr(result),
            'visited': list(analyzer.visited),
            'all_node_ids': [n['id'] for n in ast],
            'ast': ast,
        })
    except SyntaxError as e:
        return jsonify({'error': f"parse error: {e}"})
    except Exception as e:
        return jsonify({'error': f"{type(e).__name__}: {e}"})


if __name__ == '__main__':
    app.run(debug=True, port=5000)
