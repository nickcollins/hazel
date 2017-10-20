/* based closely on the paper "Strictly Pretty" by Christian Lindig
   *
   * URL: http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=A7B1EF1668A1983E747286BB1A68FD19?doi=10.1.1.34.2200&rep=rep1&type=pdf
 */
module Util = General_util;

module UTF8 = CamomileLibrary.UTF8;

module PP: {
  type doc;
  type tag = string;
  let empty: doc;
  let (^^): doc => doc => doc;
  let nestRelative: int => doc => doc;
  let nestAbsolute: int => doc => doc;
  let text: string => doc;
  let tagged: tag => doc => doc;
  let blockBoundary: doc;
  let optionalBreak: string => doc;
  let mandatoryBreak: doc;
  type sdoc =
    | SEmpty
    | SText string sdoc
    | STagStart tag sdoc
    | STagEnd sdoc
    | SWhitespace int sdoc
    | SLine int sdoc;
  let sdoc_of_doc: int => doc => sdoc;
  let string_of_sdoc: sdoc => string;
  let sdoc_cursor_index: sdoc => option int;
  let sdoc_with_text_cursor: sdoc => int => sdoc;
} = {
  type tag = string;
  type doc =
    | Empty
    | Concat doc doc
    | NestRelative int doc
    | NestAbsolute int doc
    | Text string
    | TagStart tag
    | TagEnd
    | BlockBoundary
    | OptionalBreak string
    | MandatoryBreak;
  let empty = Empty;
  let (^^) x y => Concat x y [@implicit_arity];
  let nestRelative n x => NestRelative n x [@implicit_arity];
  let nestAbsolute n x => NestAbsolute n x [@implicit_arity];
  let text s => Text s;
  let tagged tag x => Concat (TagStart tag) (Concat x TagEnd [@implicit_arity]) [@implicit_arity];
  let blockBoundary = BlockBoundary;
  let optionalBreak s => OptionalBreak s;
  let mandatoryBreak = MandatoryBreak;
  type sdoc =
    | SEmpty
    | SText string sdoc
    | STagStart tag sdoc
    | STagEnd sdoc
    | SWhitespace int sdoc
    | SLine int sdoc;
  let strlen = UTF8.length;
  /* let strlen = String.length */
  let rec sdoc_of_doc' width k zs =>
    switch zs {
    | [] => SEmpty
    | [(i, x), ...zs'] =>
      switch x {
      | Empty => sdoc_of_doc' width k zs'
      | Concat x1 x2 [@implicit_arity] => sdoc_of_doc' width k [(i, x1), (i, x2), ...zs']
      | NestRelative n x' [@implicit_arity] => sdoc_of_doc' width k [(n + k, x'), ...zs']
      | NestAbsolute n x' [@implicit_arity] => sdoc_of_doc' width k [(n + i, x'), ...zs']
      | Text s => SText s (sdoc_of_doc' width (k + strlen s) zs') [@implicit_arity]
      | TagStart tag => STagStart tag (sdoc_of_doc' width k zs') [@implicit_arity]
      | TagEnd => STagEnd (sdoc_of_doc' width k zs')
      | BlockBoundary =>
        if (i === k) {
          sdoc_of_doc' width k zs'
        } else {
          SLine i (sdoc_of_doc' width i zs') [@implicit_arity]
        }
      | OptionalBreak s =>
        if (width - k <= 0) {
          SLine i (sdoc_of_doc' width i zs') [@implicit_arity]
        } else {
          SText s (sdoc_of_doc' width (k + strlen s) zs') [@implicit_arity]
        }
      | MandatoryBreak => SLine i (sdoc_of_doc' width i zs') [@implicit_arity]
      }
    };
  let sdoc_of_doc width x => sdoc_of_doc' width 0 [(0, x)];
  let rec string_of_sdoc x =>
    switch x {
    | SEmpty => ""
    | SText s x' [@implicit_arity] => s ^ string_of_sdoc x'
    | STagStart tag x' [@implicit_arity] => string_of_sdoc x'
    | STagEnd x' => string_of_sdoc x'
    | SWhitespace n x' => String.make n ' ' ^ string_of_sdoc x'
    | SLine n x' [@implicit_arity] => "\n" ^ String.make n ' ' ^ string_of_sdoc x'
    };
  let rec sdoc_cursor_index x => {
    let sum n c => Util.option_map ((+) n) (sdoc_cursor_index c);
    switch x {
    | SEmpty => None
    | SText s x' => sum (strlen s) x'
    | STagStart tag x' => tag == "cursor" ? Some 0 : sdoc_cursor_index x'
    | STagEnd x' => sdoc_cursor_index x'
    | SWhitespace n x' => sum n x'
    | SLine n x' => sum (1 + n) x'
    }
  };
  let rec sdoc_with_text_cursor sd tc_ind => {
    let cursor txt rst => STagStart "text-cursor" (SText txt (STagEnd rst));
    switch sd {
    | SEmpty => SEmpty
    | SText s x' =>
      UTF8.validate s;
      let s_len = strlen s;
      if (tc_ind < s_len) {
        let tc_byte_ind = UTF8.nth s tc_ind;
        let next_byte_ind = UTF8.next s tc_byte_ind;
        let byte_len = String.length s;
        let substr sInd eInd => String.sub s sInd (eInd - sInd);
        let before = substr 0 tc_byte_ind;
        let cursor_char = substr tc_byte_ind next_byte_ind;
        let after = substr next_byte_ind byte_len;
        SText before (cursor cursor_char (SText after x'))
      } else {
        /* TODO if tc_ind == s_len, but x' doesn't contain any text (e.g. it's SEmpty),
           then the cursor is at the very last position and we should return
           (SText s (cursor " " x'))*/
        SText
          s (sdoc_with_text_cursor x' (tc_ind - s_len))
      }
    | STagStart tag x' => STagStart tag (sdoc_with_text_cursor x' tc_ind)
    | STagEnd x' => STagEnd (sdoc_with_text_cursor x' tc_ind)
    | SWhitespace n x' =>
      if (tc_ind < n) {
        SWhitespace tc_ind (cursor " " (SWhitespace (n - tc_ind - 1) x'))
      } else {
        SWhitespace n (sdoc_with_text_cursor x' (tc_ind - n))
      }
    | SLine n x' =>
      if (tc_ind == 0) {
        /* The cursor is on the '\n' itself -
           so we just add an extra space to the end of the line,
           and then the next line is exactly as it would have been otherwise */
        cursor " " sd
      } else if (
        tc_ind <= n
      ) {
        SLine (tc_ind - 1) (cursor " " (SWhitespace (n - tc_ind) x'))
      } else {
        SLine n (sdoc_with_text_cursor x' (tc_ind - n - 1))
      }
    }
  };
};

module HTML_Of_SDoc = {
  open Tyxml_js;
  open PP;
  let rec html_of_sdoc'' x => {
    let indentation n => Html5.(span a::[a_class ["SIndentation"]] [pcdata (String.make n ' ')]);
    switch x {
    | SEmpty => ([Html5.(span a::[a_class ["SEmpty"]] [])], None)
    | SText s x' [@implicit_arity] =>
      let (h, x'') = html_of_sdoc'' x';
      let h' = [Html5.(span a::[a_class ["SText"]] [pcdata s]), ...h];
      (h', x'')
    | STagStart tag x' [@implicit_arity] =>
      let (h, x'') = html_of_sdoc'' x';
      let (tl, rem) =
        switch x'' {
        | Some x'' => html_of_sdoc'' x''
        | None => ([], None)
        };
      let h' = [Html5.(span a::[a_class [tag]] h), ...tl];
      (h', rem)
    | STagEnd x' => ([], Some x')
    | SWhitespace n x' =>
      let (tl, rem) = html_of_sdoc'' x';
      let h = [indentation n, ...tl];
      (h, rem)
    | SLine n x' [@implicit_arity] =>
      let newline = Html5.br ();
      let (tl, rem) = html_of_sdoc'' x';
      let h = [newline, indentation n, ...tl];
      (h, rem)
    }
  };
  let rec html_of_sdoc x => {
    let (h, _) = html_of_sdoc'' x;
    Html5.(div a::[a_class ["SDoc"]] h)
  };
};
