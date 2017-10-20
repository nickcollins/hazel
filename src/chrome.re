open View;

open Tyxml_js;

open Semantics.Core;

module UText = CamomileLibrary.UText;

module Util = General_util;

module TextBox: {
  type t;
  type action =
    | Insert char
    | Backspace
    | Del
    | Left
    | Up
    | Right
    | Down;
  let create: UText.t => int => t;
  let getUText: t => UText.t;
  let getIndex: t => int;
  let performAction: t => action => t;
} = {
  type t = (UText.t, int);
  type action =
    | Insert char
    | Backspace
    | Del
    | Left
    | Up
    | Right
    | Down;
  let create uTxt ind => (uTxt, ind);
  let getUText (uTxt, ind) => uTxt;
  let getIndex (uTxt, ind) => ind;
  let uSubstr s start end_ => UText.sub s start (end_ - start);
  let text_info_insert uTxt ind chr => {
    let len = UText.length uTxt;
    let newUChar = CamomileLibrary.UChar.of_char chr;
    let newUStr = UText.init 1 (Util.const newUChar);
    (UText.append (UText.append (uSubstr uTxt 0 ind) newUStr) (uSubstr uTxt ind len), ind + 1)
  };
  let is_newline uTxt ind => CamomileLibrary.UChar.char_of (UText.get uTxt ind) == '\n';
  let get_col uTxt ind => {
    let rec get_col_ acc i => i == 0 || is_newline uTxt (i - 1) ? acc : get_col_ (acc + 1) (i - 1);
    get_col_ 0 ind
  };
  let get_row uTxt ind => {
    let rec nl_count acc i =>
      i == ind ? acc : nl_count (is_newline uTxt i ? acc + 1 : acc) (i + 1);
    nl_count 0 0
  };
  let performAction tbox action => {
    let (uTxt, ind) = tbox;
    let len = UText.length uTxt;
    switch action {
    | Insert c => text_info_insert uTxt ind c
    | Backspace =>
      ind == 0 ?
        (uTxt, ind) :
        (UText.append (uSubstr uTxt 0 (ind - 1)) (uSubstr uTxt ind (UText.length uTxt)), ind - 1)
    | Del =>
      ind == len ?
        (uTxt, ind) : (UText.append (uSubstr uTxt 0 ind) (uSubstr uTxt (ind + 1) len), ind)
    | Left => (uTxt, max 0 (ind - 1))
    | Right => (uTxt, min (UText.length uTxt) (ind + 1))
    | Up =>
      let col = get_col uTxt ind;
      let prev_row_last = ind - col - 1;
      (
        uTxt,
        if (prev_row_last < 0) {
          ind
        } else {
          let prev_row_len = get_col uTxt prev_row_last;
          prev_row_last - prev_row_len + min col prev_row_len
        }
      )
    | Down =>
      let col = get_col uTxt ind;
      let rec get_row_end i => i == len || is_newline uTxt i ? i : get_row_end (i + 1);
      let next_row_start = 1 + get_row_end ind;
      (
        uTxt,
        if (next_row_start > len) {
          ind
        } else {
          let next_row_end = get_row_end next_row_start;
          min (next_row_start + col) next_row_end
        }
      )
    }
  };
};

type projState =
  | HZ
  | Textual TextBox.t
  | Shadow TextBox.t TextBox.t;

type projStateAction =
  | TextAction TextBox.action
  | Toggle
  | DitchShadow;

let view ((rs, rf): Model.rp) => {
  /* pp view */
  let pp_view_width = 50;
  let expr_sdoc_rs =
    React.S.map
      (fun ((zexp, _), _) => Pretty.PP.sdoc_of_doc pp_view_width (PPView.of_zexp zexp)) rs;
  /* TODO unfactor this if it's only used once */
  let utext_of_sdoc = Util.compose UText.of_string Pretty.PP.string_of_sdoc;
  let (proj_view_state_rs, proj_view_state_rf) = React.S.create HZ;
  let state_after_update cur_state state_action expr_sdoc => {
    /* TODO: does_it_match actually needs to return false if the tbox index isn't in a valid place. in this case, it should also tell us what HZ action to perform in order
       to appropriately update the hexp */
    let utext_of_expr = utext_of_sdoc expr_sdoc;
    let does_it_match tbox => utext_of_expr == TextBox.getUText tbox;
    let assert_matches tbox => {
      assert (does_it_match tbox);
      tbox
    };
    let update_view last_valid_tbox cur_tbox =>
      switch state_action {
      | TextAction a =>
        let tbox_after_action = TextBox.performAction cur_tbox a;
        does_it_match tbox_after_action ?
          Textual tbox_after_action : Shadow (assert_matches last_valid_tbox) tbox_after_action
      | Toggle => HZ
      | DitchShadow => Textual (assert_matches last_valid_tbox)
      };
    switch cur_state {
    | HZ =>
      switch state_action {
      | Toggle =>
        let ind =
          switch (Pretty.PP.sdoc_cursor_index expr_sdoc) {
          | Some x => x
          | None => assert false
          };
        Textual (assert_matches (TextBox.create utext_of_expr ind))
      | _ => HZ
      }
    | Textual tbox => update_view tbox tbox
    | Shadow last_valid_tbox tbox => update_view last_valid_tbox tbox
    }
  };
  let perform_view_action a =>
    proj_view_state_rf (
      state_after_update (React.S.value proj_view_state_rs) a (React.S.value expr_sdoc_rs)
    );
  let expr_view_styles_rs =
    React.S.map
      (
        fun
        | HZ => ["ModelExpHZ"]
        | Textual _ => ["ModelExpTextual"]
        | Shadow _ _ => ["ModelExpShadow"]
      )
      proj_view_state_rs;
  let are_actions_enabled_rs =
    React.S.map
      (
        fun
        | HZ => true
        | Textual _ => false
        | Shadow _ _ => false
      )
      proj_view_state_rs;
  let (action_palette, hz_action_evt_handlers) =
    Action_palette.make_palette (rs, rf) are_actions_enabled_rs;
  let _ =
    Js_util.listen_to
      Dom_html.Event.keydown
      Dom_html.document
      (
        fun evt => {
          let key_string = Js_util.get_key evt;
          let is_shift = Js.to_bool evt##.shiftKey;
          let is_ctrl = Js.to_bool evt##.ctrlKey;
          let kcs = Js_util.KeyCombo.to_string;
          module KCs = Js_util.KeyCombos;
          let perform_view_action_f = Util.do_and_return perform_view_action Js._false;
          if (is_shift && key_string == kcs KCs.enter) {
            perform_view_action_f Toggle
          } else if (
            Hashtbl.mem hz_action_evt_handlers key_string && React.S.value are_actions_enabled_rs
          ) {
            Hashtbl.find hz_action_evt_handlers key_string evt
          } else if (
            key_string == kcs KCs.esc || is_ctrl && key_string == kcs KCs.open_bracket
          ) {
            perform_view_action_f DitchShadow
          } else if (
            key_string == kcs KCs.backspace
          ) {
            perform_view_action_f (TextAction TextBox.Backspace)
          } else if (
            key_string == kcs KCs.del
          ) {
            perform_view_action_f (TextAction TextBox.Del)
          } else if (
            key_string == kcs KCs.left
          ) {
            perform_view_action_f (TextAction TextBox.Left)
          } else if (
            key_string == kcs KCs.up
          ) {
            perform_view_action_f (TextAction TextBox.Up)
          } else if (
            key_string == kcs KCs.right
          ) {
            perform_view_action_f (TextAction TextBox.Right)
          } else if (
            key_string == kcs KCs.down
          ) {
            perform_view_action_f (TextAction TextBox.Down)
          } else if (
            key_string == kcs KCs.enter
          ) {
            perform_view_action_f (TextAction (TextBox.Insert '\n'))
          } else if (
            String.length key_string == 1
          ) {
            perform_view_action_f (TextAction (TextBox.Insert key_string.[0]))
          } else {
            Js._true
          }
        }
      );
  let expr_proj_view_rs =
    React.S.l2
      (
        fun sdoc proj_view_state =>
          [
            /* [
               Html5.(
                 div
                   a::[
                     a_tabindex 0,
                     a_onkeypress (
                       fun evt => {
                         let evt_key = Js_util.get_keyCode evt;
                         if (evt_key == Js_util.KeyCombo.keyCode Js_util.KeyCombos.enter) {
                           switch (React.S.value proj_mode_rs) {
                           | HZ => proj_view_state_rf (Some (React.S.value utext_of_expr_rs, 0))
                           | Textual => proj_view_state_rf (Some (UText.of_string " dog", 0))
                           | Shadow => proj_view_state_rf None
                           };
                           false
                         } else {
                           true
                         }
                       }
                     )
                   ]
                   */
            switch proj_view_state {
            | HZ => Pretty.HTML_Of_SDoc.html_of_sdoc sdoc
            | Textual tbox =>
              Pretty.HTML_Of_SDoc.html_of_sdoc (
                Pretty.PP.sdoc_with_text_cursor sdoc (TextBox.getIndex tbox)
              )
            | Shadow _ tbox =>
              let uTxt = TextBox.getUText tbox;
              let cInd = TextBox.getIndex tbox;
              let to_string sInd eInd =>
                CamomileLibrary.UTF8.init
                  (eInd - sInd)
                  (
                    fun i => {
                      let ind = sInd + i;
                      assert (ind != cInd);
                      UText.get uTxt ind
                    }
                  );
              let cursor_string = CamomileLibrary.UTF8.init 1 (Util.const (UText.get uTxt cInd));
              Html5.(
                pre
                  a::[a_class ["SDoc"]]
                  [
                    pcdata (to_string 0 cInd),
                    span a::[a_class ["text-cursor"]] [pcdata cursor_string],
                    pcdata (to_string (cInd + 1) (UText.length uTxt))
                  ]
              )
            /*| Shadow _ tbox => Pretty.HTML_Of_SDoc.html_of_sdoc (Pretty.PP.sdoc_with_text_cursor sdoc (SText (Utext.to_string (TextBox.getUText tbox)) SEnd) (TextBox.getIndex tbox))*/
            }
          ]
          /* )
             ] */
      )
      expr_sdoc_rs
      proj_view_state_rs;
  let expr_proj_view_rhtml = R.Html5.div (ReactiveData.RList.from_signal expr_proj_view_rs);
  /* htype view */
  let htype_rs =
    React.S.map
      (
        fun ((_, htype), _) => {
          let pp_view = PPView.of_htype htype;
          let sdoc = Pretty.PP.sdoc_of_doc pp_view_width pp_view;
          let prettified = Pretty.HTML_Of_SDoc.html_of_sdoc sdoc;
          [prettified]
        }
      )
      rs;
  let htype_view = R.Html5.div (ReactiveData.RList.from_signal htype_rs);
  /* result view */
  let result_rs =
    React.S.map
      (
        fun ((zexp, _), _) => {
          let e = ZExp.erase zexp;
          let expanded = Dynamics.DHExp.syn_expand () Ctx.empty e;
          switch expanded {
          | Dynamics.DHExp.DoesNotExpand => [Html5.(pcdata "(does not expand)")] /* should never happen! */
          | Dynamics.DHExp.Expands d ty delta =>
            let result = Dynamics.Evaluator.evaluate () delta d;
            switch result {
            | Dynamics.Evaluator.InvalidInput => [Html5.pcdata "(internal error: invalid input)"]
            | Dynamics.Evaluator.CastError => [Html5.pcdata "(cast error)"]
            | Dynamics.Evaluator.Value d_val
            | Dynamics.Evaluator.Indet d_val =>
              let pp_view = PPView.of_dhexp d_val;
              let sdoc = Pretty.PP.sdoc_of_doc pp_view_width pp_view;
              let prettified = Pretty.HTML_Of_SDoc.html_of_sdoc sdoc;
              [prettified]
            }
          }
        }
      )
      rs;
  let ((show_hole_names_checkbox_rs, _), show_hole_names_checkbox, _) =
    Js_util.r_checkbox "show_hole_names_checkbox" "Show hole names" true;
  let ((show_hole_envs_checkbox_rs, _), show_hole_envs_checkbox, _) =
    Js_util.r_checkbox "show_hole_envs_checkbox" "Show hole environments" false;
  let root_classes =
    React.S.l2
      (
        fun show_hole_names show_hole_envs => {
          let show_hole_names_class =
            if show_hole_names {"show-hole-names"} else {"hide-hole-names"};
          let show_hole_envs_class = if show_hole_envs {"show-hole-envs"} else {"hide-hole-envs"};
          [show_hole_names_class, show_hole_envs_class]
        }
      )
      show_hole_names_checkbox_rs
      show_hole_envs_checkbox_rs;
  let result_view = R.Html5.div (ReactiveData.RList.from_signal result_rs);
  Tyxml_js.To_dom.of_div
    Html5.(
      div
        a::[R.Html5.a_class root_classes]
        [
          div a::[a_class ["top-bar"]] [span a::[a_class ["logo-text"]] [pcdata "Hazel"]],
          div
            a::[a_class ["main-area"]]
            [
              div [
                div
                  a::[a_class ["page-area"]]
                  [
                    div
                      a::[a_class ["page"]]
                      [
                        h1 [pcdata "Welcome to Hazel"],
                        hr (),
                        p [
                          pcdata "Hazel is an experimental structure editor for a simple typed expression language."
                        ],
                        div a::[R.Html5.a_class expr_view_styles_rs] [expr_proj_view_rhtml],
                        div
                          a::[a_class ["cell-status"]]
                          [
                            div a::[a_class ["result-label"]] [pcdata "Result: "],
                            div
                              a::[a_class ["type-indicator"]]
                              [
                                div a::[a_class ["type-label"]] [pcdata "Type: "],
                                div a::[a_class ["htype-view"]] [htype_view]
                              ]
                          ],
                        div a::[a_class ["result-view"]] [result_view]
                      ]
                  ],
                div
                  a::[a_class ["sidebar"]]
                  [
                    action_palette,
                    div a::[a_class ["panel-title"]] [pcdata "Options"],
                    show_hole_names_checkbox,
                    show_hole_envs_checkbox
                  ]
              ]
            ]
        ]
    )
};
