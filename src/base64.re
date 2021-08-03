type nat =
  | O
  | S(nat);

type sum('a, 'b) =
  | Inl('a)
  | Inr('b);

type prod('a, 'b) =
  | Pair('a, 'b);

/** val fst : ('a1, 'a2) prod -> 'a1 **/;

let fst =
  fun
  | [@implicit_arity] Pair(x, _y) => x;

/** val snd : ('a1, 'a2) prod -> 'a2 **/;

let snd =
  fun
  | [@implicit_arity] Pair(_x, y) => y;

type _sig0('a) = 'a;
/* singleton inductive, whose constructor was exist */

/** val plus : nat -> nat -> nat **/;

let rec plus = (n, m) =>
  switch (n) {
  | O => m
  | S(p) => S(plus(p, m))
  };

/** val mult : nat -> nat -> nat **/;

let rec mult = (n, m) =>
  switch (n) {
  | O => O
  | S(p) => plus(m, mult(p, m))
  };

/** val minus : nat -> nat -> nat **/;

let rec minus = (n, m) =>
  switch (n) {
  | O => n
  | S(k) =>
    switch (m) {
    | O => n
    | S(l) => minus(k, l)
    }
  };

/** val bool_dec : bool -> bool -> bool **/;

let bool_dec = (b1, b2) =>
  if (b1) {
    if (b2) {true} else {false};
  } else if (b2) {
    false;
  } else {
    true;
  };

/** val length : 'a1 list -> nat **/;

let rec length =
  fun
  | [] => O
  | [_a, ...m] => S(length(m));

/** val app : 'a1 list -> 'a1 list -> 'a1 list **/;

let rec app = (l, m) =>
  switch (l) {
  | [] => m
  | [a, ...l1] => [a, ...app(l1, m)]
  };

/** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **/;

let rec map = f =>
  fun
  | [] => []
  | [a, ...t] => [f(a), ...map(f, t)];

type positive =
  | XI(positive)
  | XO(positive)
  | XH;

/** val psucc : positive -> positive **/;

let rec psucc =
  fun
  | XI(p) => XO(psucc(p))
  | XO(p) => XI(p)
  | XH => XO(XH);

/** val p_of_succ_nat : nat -> positive **/;

let rec p_of_succ_nat =
  fun
  | O => XH
  | S(x) => psucc(p_of_succ_nat(x));

type ascii =
  | Ascii(bool, bool, bool, bool, bool, bool, bool, bool);

/** val zero : ascii **/;

let zero =
  [@implicit_arity]
  Ascii(false, false, false, false, false, false, false, false);

/** val one : ascii **/;

let one =
  [@implicit_arity]
  Ascii(true, false, false, false, false, false, false, false);

/** val app2 : (bool -> bool -> bool) -> ascii -> ascii -> ascii **/;

let app2 = (f, a, b) => {
  let [@implicit_arity] Ascii(a1, a2, a3, a4, a5, a6, a7, a8) = a;
  let [@implicit_arity] Ascii(b1, b2, b3, b4, b5, b6, b7, b8) = b;
  [@implicit_arity]
  Ascii(
    f(a1, b1),
    f(a2, b2),
    f(a3, b3),
    f(a4, b4),
    f(a5, b5),
    f(a6, b6),
    f(a7, b7),
    f(a8, b8),
  );
};

/** val shift : bool -> ascii -> ascii **/;

let shift = c =>
  fun
  | [@implicit_arity] Ascii(a1, a2, a3, a4, a5, a6, a7, _a8) =>
    [@implicit_arity] Ascii(c, a1, a2, a3, a4, a5, a6, a7);

/** val ascii_dec : ascii -> ascii -> bool **/;

let ascii_dec = (a, b) => {
  let [@implicit_arity] Ascii(x, x0, x1, x2, x3, x4, x5, x6) = a;
  let [@implicit_arity] Ascii(b8, b9, b10, b11, b12, b13, b14, b15) = b;
  if (bool_dec(x, b8)) {
    if (bool_dec(x0, b9)) {
      if (bool_dec(x1, b10)) {
        if (bool_dec(x2, b11)) {
          if (bool_dec(x3, b12)) {
            if (bool_dec(x4, b13)) {
              if (bool_dec(x5, b14)) {
                bool_dec(x6, b15);
              } else {
                false;
              };
            } else {
              false;
            };
          } else {
            false;
          };
        } else {
          false;
        };
      } else {
        false;
      };
    } else {
      false;
    };
  } else {
    false;
  };
};

/** val ascii_of_pos_aux : ascii -> ascii -> positive -> nat -> ascii **/;

let rec ascii_of_pos_aux = (res, acc, z) =>
  fun
  | O => res
  | S(n1) =>
    switch (z) {
    | XI(z') =>
      ascii_of_pos_aux(
        app2((b1, b2) => if (b1) {true} else {b2}, res, acc),
        shift(false, acc),
        z',
        n1,
      )
    | XO(z') => ascii_of_pos_aux(res, shift(false, acc), z', n1)
    | XH => app2((b1, b2) => if (b1) {true} else {b2}, res, acc)
    };

/** val ascii_of_pos : positive -> ascii **/;

let ascii_of_pos = a =>
  ascii_of_pos_aux(zero, one, a, S(S(S(S(S(S(S(S(O)))))))));

/** val ascii_of_nat : nat -> ascii **/;

let ascii_of_nat =
  fun
  | O => zero
  | S(a') => ascii_of_pos(p_of_succ_nat(a'));

/** val nat_of_ascii : ascii -> nat **/;

let nat_of_ascii =
  fun
  | [@implicit_arity] Ascii(a1, a2, a3, a4, a5, a6, a7, a8) =>
    plus(
      mult(
        S(S(O)),
        plus(
          mult(
            S(S(O)),
            plus(
              mult(
                S(S(O)),
                plus(
                  mult(
                    S(S(O)),
                    plus(
                      mult(
                        S(S(O)),
                        plus(
                          mult(
                            S(S(O)),
                            plus(
                              mult(S(S(O)), if (a8) {S(O)} else {O}),
                              if (a7) {S(O)} else {O},
                            ),
                          ),
                          if (a6) {S(O)} else {O},
                        ),
                      ),
                      if (a5) {S(O)} else {O},
                    ),
                  ),
                  if (a4) {S(O)} else {O},
                ),
              ),
              if (a3) {S(O)} else {O},
            ),
          ),
          if (a2) {S(O)} else {O},
        ),
      ),
      if (a1) {S(O)} else {O},
    );

/** val eq_nat_dec : nat -> nat -> bool **/;

let rec eq_nat_dec = (n, m) =>
  switch (n) {
  | O =>
    switch (m) {
    | O => true
    | S(_m0) => false
    }
  | S(n0) =>
    switch (m) {
    | O => false
    | S(m0) => eq_nat_dec(n0, m0)
    }
  };

/** val le_lt_dec : nat -> nat -> bool **/;

let rec le_lt_dec = (n, m) =>
  switch (n) {
  | O => true
  | S(n0) =>
    switch (m) {
    | O => false
    | S(m0) => le_lt_dec(n0, m0)
    }
  };

/** val replicate : nat -> 'a1 -> 'a1 list **/;

let rec replicate = (n, x) =>
  switch (n) {
  | O => []
  | S(m) => [x, ...replicate(m, x)]
  };

/** val delete : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list **/;

let rec delete = (eq_dec, a) =>
  fun
  | [] => []
  | [x, ...xs0] =>
    if (eq_dec(x, a)) {
      delete(eq_dec, a, xs0);
    } else {
      [x, ...delete(eq_dec, a, xs0)];
    };

type vec('a) =
  | VNil
  | VCons(nat, 'a, vec('a));

/** val vec_of_list : nat -> 'a1 list -> 'a1 vec **/;

let rec vec_of_list = _n =>
  fun
  | [] => VNil
  | [x, ...xs0] =>
    [@implicit_arity] VCons(length(xs0), x, vec_of_list(length(xs0), xs0));

/** val list_of_vec : nat -> 'a1 vec -> 'a1 list **/;

let rec list_of_vec = _n =>
  fun
  | VNil => []
  | [@implicit_arity] VCons(n0, x, xs) => [x, ...list_of_vec(n0, xs)];

/** val split_dec :
    nat -> 'a1 list -> (('a1 vec, 'a1 list) prod, 'a1 list) sum **/;

let rec split_dec = (n, xs) =>
  switch (n) {
  | O => Inl([@implicit_arity] Pair(VNil, xs))
  | S(m) =>
    switch (xs) {
    | [] => Inr(xs)
    | [x, ...xs0] =>
      switch (split_dec(m, xs0)) {
      | Inl(p) =>
        let [@implicit_arity] Pair(xs1, xs2) = p;
        Inl(
          [@implicit_arity] Pair([@implicit_arity] VCons(m, x, xs1), xs2),
        );
      | Inr(s) => Inr([x, ...s])
      }
    }
  };

/** val grp_aux_terminate :
    nat -> 'a1 list -> ('a1 vec list, 'a1 list) prod **/;

let rec grp_aux_terminate = n =>
  fun
  | [] => [@implicit_arity] Pair([], [])
  | [x, ...xs0] =>
    switch (split_dec(n, [x, ...xs0])) {
    | Inl(p) =>
      let [@implicit_arity] Pair(v, tail) = p;
      let [@implicit_arity] Pair(vs, rest) = grp_aux_terminate(n, tail);
      [@implicit_arity] Pair([v, ...Obj.magic(vs)], rest);
    | Inr(r) => [@implicit_arity] Pair([], r)
    };

/** val grp_aux : nat -> 'a1 list -> ('a1 vec list, 'a1 list) prod **/;

let grp_aux = (x, x0) => grp_aux_terminate(x, x0);

/** val g1 : nat -> 'a1 list -> 'a1 vec list **/;

let g1 = (n, xs) => fst(grp_aux(n, xs));

/** val g2 : nat -> 'a1 list -> 'a1 list **/;

let g2 = (n, xs) => snd(grp_aux(n, xs));

/** val gcut : nat -> 'a1 list -> 'a1 vec list **/;

let gcut = (n, xs) => g1(n, xs);

/** val gfill : 'a1 -> nat -> 'a1 list -> 'a1 vec list **/;

let gfill = (default, n, xs) => {
  let ys = g2(n, xs);
  app(
    g1(n, xs),
    [vec_of_list(n, app(ys, replicate(minus(n, length(ys)), default)))],
  );
};

/** val cm : nat -> 'a1 vec list -> 'a1 list **/;

let rec cm = n =>
  fun
  | [] => []
  | [v, ...vs0] => app(list_of_vec(n, v), cm(n, vs0));

type _mlchar = char;

type _mlint = int;

type _mlstring = string;

/** val mlint_of_nat : nat -> mlint **/;

let mlint_of_nat = {
  let rec iter =
    fun
    | O => 0
    | S(p) => succ(iter(p));
  iter;
};

/** val nat_of_mlint : mlint -> nat **/;

let nat_of_mlint = {
  let rec iter =
    fun
    | 0 => O
    | n => S(iter(pred(n)));
  iter;
};

/** val mlchar_of_mlint : mlint -> mlchar **/;

let mlchar_of_mlint = char_of_int;

/** val mlint_of_mlchar : mlchar -> mlint **/;

let mlint_of_mlchar = int_of_char;

/** val ascii_of_mlchar : mlchar -> ascii **/;

let ascii_of_mlchar = x => ascii_of_nat(nat_of_mlint(mlint_of_mlchar(x)));

/** val mlchar_of_ascii : ascii -> mlchar **/;

let mlchar_of_ascii = x => mlchar_of_mlint(mlint_of_nat(nat_of_ascii(x)));

/** val mlstring_of_list : ('a1 -> mlchar) -> 'a1 list -> mlstring **/;

let mlstring_of_list = (f, s) =>
  String.concat("", List.map(x => String.make(1, f(x)), s));

/** val list_of_mlstring : (mlchar -> 'a1) -> mlstring -> 'a1 list **/;

let list_of_mlstring = (f, s) => {
  let rec explode_rec = n =>
    if (n >= String.length(s)) {
      [];
    } else {
      [f(s.[n]), ...explode_rec(succ(n))];
    };

  explode_rec(0);
};

/** val mlstring_of_asciilist : ascii list -> mlstring **/;

let mlstring_of_asciilist = x => mlstring_of_list(mlchar_of_ascii, x);

/** val asciilist_of_mlstring : mlstring -> ascii list **/;

let asciilist_of_mlstring = x => list_of_mlstring(ascii_of_mlchar, x);

/** val ascii_of_bool6 : bool vec -> ascii **/;

let ascii_of_bool6 =
  fun
  | VNil => assert(false) /* absurd case */
  | [@implicit_arity] VCons(_n, h, h0) =>
    switch (h0) {
    | VNil => assert(false) /* absurd case */
    | [@implicit_arity] VCons(_n0, h2, h3) =>
      switch (h3) {
      | VNil => assert(false) /* absurd case */
      | [@implicit_arity] VCons(_n1, h5, h6) =>
        switch (h6) {
        | VNil => assert(false) /* absurd case */
        | [@implicit_arity] VCons(_n2, h8, h9) =>
          switch (h9) {
          | VNil => assert(false) /* absurd case */
          | [@implicit_arity] VCons(_n3, h11, h12) =>
            switch (h12) {
            | VNil => assert(false) /* absurd case */
            | [@implicit_arity] VCons(_n4, h14, _h15) =>
              [@implicit_arity] Ascii(h14, h11, h8, h5, h2, h, false, false)
            }
          }
        }
      }
    };

/** val bool6_of_ascii : ascii -> bool vec **/;

let bool6_of_ascii =
  fun
  | [@implicit_arity] Ascii(a, b, c0, d, e, f, _b0, _b1) =>
    vec_of_list(S(S(S(S(S(S(O)))))), [f, e, d, c0, b, a]);

/** val ascii_of_bool8 : bool vec -> ascii **/;

let ascii_of_bool8 =
  fun
  | VNil => assert(false) /* absurd case */
  | [@implicit_arity] VCons(_n, h, h0) =>
    switch (h0) {
    | VNil => assert(false) /* absurd case */
    | [@implicit_arity] VCons(_n0, h2, h3) =>
      switch (h3) {
      | VNil => assert(false) /* absurd case */
      | [@implicit_arity] VCons(_n1, h5, h6) =>
        switch (h6) {
        | VNil => assert(false) /* absurd case */
        | [@implicit_arity] VCons(_n2, h8, h9) =>
          switch (h9) {
          | VNil => assert(false) /* absurd case */
          | [@implicit_arity] VCons(_n3, h11, h12) =>
            switch (h12) {
            | VNil => assert(false) /* absurd case */
            | [@implicit_arity] VCons(_n4, h14, h15) =>
              switch (h15) {
              | VNil => assert(false)
              /* absurd case */
              | [@implicit_arity] VCons(_n5, h17, h18) =>
                switch (h18) {
                | VNil => assert(false)
                /* absurd case */

                | [@implicit_arity] VCons(_n6, h20, _h21) =>
                  [@implicit_arity] Ascii(h20, h17, h14, h11, h8, h5, h2, h)
                }
              }
            }
          }
        }
      }
    };

/** val le_lt_dec_aux : nat -> nat -> bool **/;

let le_lt_dec_aux = (x, y) => le_lt_dec(x, y);

/** val tblaux : nat -> nat **/;

let tblaux = n =>
  if (le_lt_dec_aux(
        n,
        S(
          S(
            S(
              S(
                S(
                  S(
                    S(
                      S(
                        S(
                          S(
                            S(
                              S(
                                S(
                                  S(
                                    S(
                                      S(
                                        S(S(S(S(S(S(S(S(S(O))))))))),
                                      ),
                                    ),
                                  ),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      )) {
    plus(
      nat_of_ascii(
        [@implicit_arity]
        Ascii(true, false, false, false, false, false, true, false),
      ),
      n,
    );
  } else if (le_lt_dec_aux(
               n,
               S(
                 S(
                   S(
                     S(
                       S(
                         S(
                           S(
                             S(
                               S(
                                 S(
                                   S(
                                     S(
                                       S(
                                         S(
                                           S(
                                             S(
                                               S(
                                                 S(
                                                   S(
                                                     S(
                                                       S(
                                                         S(
                                                           S(
                                                             S(
                                                               S(
                                                                 S(
                                                                   S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(S(O)),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                   ),
                                                                 ),
                                                               ),
                                                             ),
                                                           ),
                                                         ),
                                                       ),
                                                     ),
                                                   ),
                                                 ),
                                               ),
                                             ),
                                           ),
                                         ),
                                       ),
                                     ),
                                   ),
                                 ),
                               ),
                             ),
                           ),
                         ),
                       ),
                     ),
                   ),
                 ),
               ),
             )) {
    plus(
      nat_of_ascii(
        [@implicit_arity]
        Ascii(true, false, false, false, false, true, true, false),
      ),
      minus(
        n,
        S(
          S(
            S(
              S(
                S(
                  S(
                    S(
                      S(
                        S(
                          S(
                            S(
                              S(
                                S(
                                  S(
                                    S(
                                      S(
                                        S(
                                          S(
                                            S(S(S(S(S(S(S(S(O)))))))),
                                          ),
                                        ),
                                      ),
                                    ),
                                  ),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    );
  } else if (le_lt_dec_aux(
               n,
               S(
                 S(
                   S(
                     S(
                       S(
                         S(
                           S(
                             S(
                               S(
                                 S(
                                   S(
                                     S(
                                       S(
                                         S(
                                           S(
                                             S(
                                               S(
                                                 S(
                                                   S(
                                                     S(
                                                       S(
                                                         S(
                                                           S(
                                                             S(
                                                               S(
                                                                 S(
                                                                   S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(S(O)),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                   ),
                                                                 ),
                                                               ),
                                                             ),
                                                           ),
                                                         ),
                                                       ),
                                                     ),
                                                   ),
                                                 ),
                                               ),
                                             ),
                                           ),
                                         ),
                                       ),
                                     ),
                                   ),
                                 ),
                               ),
                             ),
                           ),
                         ),
                       ),
                     ),
                   ),
                 ),
               ),
             )) {
    plus(
      nat_of_ascii(
        [@implicit_arity]
        Ascii(false, false, false, false, true, true, false, false),
      ),
      minus(
        n,
        S(
          S(
            S(
              S(
                S(
                  S(
                    S(
                      S(
                        S(
                          S(
                            S(
                              S(
                                S(
                                  S(
                                    S(
                                      S(
                                        S(
                                          S(
                                            S(
                                              S(
                                                S(
                                                  S(
                                                    S(
                                                      S(
                                                        S(
                                                          S(
                                                            S(
                                                              S(
                                                                S(
                                                                  S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(S(O)),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                  ),
                                                                ),
                                                              ),
                                                            ),
                                                          ),
                                                        ),
                                                      ),
                                                    ),
                                                  ),
                                                ),
                                              ),
                                            ),
                                          ),
                                        ),
                                      ),
                                    ),
                                  ),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    );
  } else if (eq_nat_dec(
               n,
               S(
                 S(
                   S(
                     S(
                       S(
                         S(
                           S(
                             S(
                               S(
                                 S(
                                   S(
                                     S(
                                       S(
                                         S(
                                           S(
                                             S(
                                               S(
                                                 S(
                                                   S(
                                                     S(
                                                       S(
                                                         S(
                                                           S(
                                                             S(
                                                               S(
                                                                 S(
                                                                   S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(S(O)),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                   ),
                                                                 ),
                                                               ),
                                                             ),
                                                           ),
                                                         ),
                                                       ),
                                                     ),
                                                   ),
                                                 ),
                                               ),
                                             ),
                                           ),
                                         ),
                                       ),
                                     ),
                                   ),
                                 ),
                               ),
                             ),
                           ),
                         ),
                       ),
                     ),
                   ),
                 ),
               ),
             )) {
    nat_of_ascii(
      [@implicit_arity]
      Ascii(true, true, false, true, false, true, false, false),
    );
  } else {
    nat_of_ascii(
      [@implicit_arity]
      Ascii(true, true, true, true, false, true, false, false),
    );
  };

/** val tbl : bool vec -> ascii **/;

let tbl = bs => ascii_of_nat(tblaux(nat_of_ascii(ascii_of_bool6(bs))));

/** val tblaux_inv : nat -> nat **/;

let tblaux_inv = n =>
  if (le_lt_dec_aux(
        S(
          S(
            S(
              S(
                S(
                  S(
                    S(
                      S(
                        S(
                          S(
                            S(
                              S(
                                S(
                                  S(
                                    S(
                                      S(
                                        S(
                                          S(
                                            S(
                                              S(
                                                S(
                                                  S(
                                                    S(
                                                      S(
                                                        S(
                                                          S(
                                                            S(
                                                              S(
                                                                S(
                                                                  S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(S(O)),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                  ),
                                                                ),
                                                              ),
                                                            ),
                                                          ),
                                                        ),
                                                      ),
                                                    ),
                                                  ),
                                                ),
                                              ),
                                            ),
                                          ),
                                        ),
                                      ),
                                    ),
                                  ),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
        n,
      )) {
    minus(
      plus(
        S(
          S(
            S(
              S(
                S(
                  S(
                    S(
                      S(
                        S(
                          S(
                            S(
                              S(
                                S(
                                  S(
                                    S(
                                      S(
                                        S(
                                          S(
                                            S(S(S(S(S(S(S(S(O)))))))),
                                          ),
                                        ),
                                      ),
                                    ),
                                  ),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
        n,
      ),
      nat_of_ascii(
        [@implicit_arity]
        Ascii(true, false, false, false, false, true, true, false),
      ),
    );
  } else if (le_lt_dec_aux(
               S(
                 S(
                   S(
                     S(
                       S(
                         S(
                           S(
                             S(
                               S(
                                 S(
                                   S(
                                     S(
                                       S(
                                         S(
                                           S(
                                             S(
                                               S(
                                                 S(
                                                   S(
                                                     S(
                                                       S(
                                                         S(
                                                           S(
                                                             S(
                                                               S(
                                                                 S(
                                                                   S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(S(O)),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                   ),
                                                                 ),
                                                               ),
                                                             ),
                                                           ),
                                                         ),
                                                       ),
                                                     ),
                                                   ),
                                                 ),
                                               ),
                                             ),
                                           ),
                                         ),
                                       ),
                                     ),
                                   ),
                                 ),
                               ),
                             ),
                           ),
                         ),
                       ),
                     ),
                   ),
                 ),
               ),
               n,
             )) {
    minus(
      n,
      nat_of_ascii(
        [@implicit_arity]
        Ascii(true, false, false, false, false, false, true, false),
      ),
    );
  } else if (le_lt_dec_aux(
               S(
                 S(
                   S(
                     S(
                       S(
                         S(
                           S(
                             S(
                               S(
                                 S(
                                   S(
                                     S(
                                       S(
                                         S(
                                           S(
                                             S(
                                               S(
                                                 S(
                                                   S(
                                                     S(
                                                       S(
                                                         S(
                                                           S(
                                                             S(
                                                               S(
                                                                 S(
                                                                   S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(S(O)),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                   ),
                                                                 ),
                                                               ),
                                                             ),
                                                           ),
                                                         ),
                                                       ),
                                                     ),
                                                   ),
                                                 ),
                                               ),
                                             ),
                                           ),
                                         ),
                                       ),
                                     ),
                                   ),
                                 ),
                               ),
                             ),
                           ),
                         ),
                       ),
                     ),
                   ),
                 ),
               ),
               n,
             )) {
    minus(
      plus(
        S(
          S(
            S(
              S(
                S(
                  S(
                    S(
                      S(
                        S(
                          S(
                            S(
                              S(
                                S(
                                  S(
                                    S(
                                      S(
                                        S(
                                          S(
                                            S(
                                              S(
                                                S(
                                                  S(
                                                    S(
                                                      S(
                                                        S(
                                                          S(
                                                            S(
                                                              S(
                                                                S(
                                                                  S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(S(O)),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                  ),
                                                                ),
                                                              ),
                                                            ),
                                                          ),
                                                        ),
                                                      ),
                                                    ),
                                                  ),
                                                ),
                                              ),
                                            ),
                                          ),
                                        ),
                                      ),
                                    ),
                                  ),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
        n,
      ),
      nat_of_ascii(
        [@implicit_arity]
        Ascii(false, false, false, false, true, true, false, false),
      ),
    );
  } else if (eq_nat_dec(
               n,
               nat_of_ascii(
                 [@implicit_arity]
                 Ascii(true, true, false, true, false, true, false, false),
               ),
             )) {
    S(
      S(
        S(
          S(
            S(
              S(
                S(
                  S(
                    S(
                      S(
                        S(
                          S(
                            S(
                              S(
                                S(
                                  S(
                                    S(
                                      S(
                                        S(
                                          S(
                                            S(
                                              S(
                                                S(
                                                  S(
                                                    S(
                                                      S(
                                                        S(
                                                          S(
                                                            S(
                                                              S(
                                                                S(
                                                                  S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(S(O)),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                  ),
                                                                ),
                                                              ),
                                                            ),
                                                          ),
                                                        ),
                                                      ),
                                                    ),
                                                  ),
                                                ),
                                              ),
                                            ),
                                          ),
                                        ),
                                      ),
                                    ),
                                  ),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    );
  } else {
    S(
      S(
        S(
          S(
            S(
              S(
                S(
                  S(
                    S(
                      S(
                        S(
                          S(
                            S(
                              S(
                                S(
                                  S(
                                    S(
                                      S(
                                        S(
                                          S(
                                            S(
                                              S(
                                                S(
                                                  S(
                                                    S(
                                                      S(
                                                        S(
                                                          S(
                                                            S(
                                                              S(
                                                                S(
                                                                  S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(
                                                                    S(S(O)),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                    ),
                                                                  ),
                                                                ),
                                                              ),
                                                            ),
                                                          ),
                                                        ),
                                                      ),
                                                    ),
                                                  ),
                                                ),
                                              ),
                                            ),
                                          ),
                                        ),
                                      ),
                                    ),
                                  ),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    );
  };

/** val tbl_inv : ascii -> bool vec **/;

let tbl_inv = c =>
  bool6_of_ascii(ascii_of_nat(tblaux_inv(nat_of_ascii(c))));

/** val digit : ascii -> bool vec **/;

let digit =
  fun
  | [@implicit_arity] Ascii(a, b, c, d, e, f, g, h) =>
    vec_of_list(S(S(S(S(S(S(S(S(O)))))))), [h, g, f, e, d, c, b, a]);

/** val encode : mlstring -> mlstring **/;

let encode = s =>
  mlstring_of_asciilist(
    cm(
      S(S(S(S(O)))),
      gfill(
        [@implicit_arity]
        Ascii(true, false, true, true, true, true, false, false),
        S(S(S(S(O)))),
        map(
          tbl,
          gfill(
            false,
            S(S(S(S(S(S(O)))))),
            cm(
              S(S(S(S(S(S(S(S(O)))))))),
              map(digit, asciilist_of_mlstring(s)),
            ),
          ),
        ),
      ),
    ),
  );

/** val decode : mlstring -> mlstring **/;

let decode = s =>
  mlstring_of_asciilist(
    map(
      ascii_of_bool8,
      gcut(
        S(S(S(S(S(S(S(S(O)))))))),
        cm(
          S(S(S(S(S(S(O)))))),
          map(
            tbl_inv,
            delete(
              ascii_dec,
              [@implicit_arity]
              Ascii(true, false, true, true, true, true, false, false),
              asciilist_of_mlstring(s),
            ),
          ),
        ),
      ),
    ),
  );
