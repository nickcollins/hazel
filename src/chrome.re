open View;

open Tyxml_js;

open Semantics.Core;

module UText = CamomileLibrary.UText;

module Util = General_util;

type projMode =
  | HZ
  | Textual
  | Shadow;

let view ((rs, rf): Model.rp) => {
  /* pp view */
  let pp_view_width = 50;
  let (proj_view_state_rs, proj_view_state_rf) = React.S.create None;
  let expr_sdoc_rs =
    React.S.map
      (fun ((zexp, _), _) => Pretty.PP.sdoc_of_doc pp_view_width (PPView.of_zexp zexp)) rs;
  let utext_of_expr_rs =
    React.S.map (Util.compose UText.of_string Pretty.PP.string_of_sdoc) expr_sdoc_rs;
  let proj_mode_rs =
    React.S.l2
      (
        fun proj_view_state utext_of_expr =>
          switch proj_view_state {
          | None => HZ
          | Some (txt, index) => UText.compare txt utext_of_expr == 0 ? Textual : Shadow
          }
      )
      proj_view_state_rs
      utext_of_expr_rs;
  Js_util.listen_to
        Dom_html.Event.keypress
        Dom_html.document
        (
          fun evt => {
            let evt_key = Js_util.get_keyCode evt;
            let is_shift = evt##.shiftKey;
            if (is_shift && evt_key == Js_util.KeyCombo.keyCode Js_util.KeyCombos.enter) {
                      switch (React.S.value proj_mode_rs) {
                              /* TODO: set the proper index, instead of just 0 */
                      | HZ => proj_view_state_rf (Some (React.S.value utext_of_expr_rs, 0))
                      | Textual => proj_view_state_rf None
                      | Shadow => proj_view_state_rf None
                      };
              Dom_html.stopPropagation evt;
              Js._false
            } else {
              Js._true
            }
          }
        );
  let expr_view_styles_rs =
    React.S.map
      (
        fun
        | HZ => ["ModelExpHZ"]
        | Textual => ["ModelExpTextual"]
        | Shadow => ["ModelExpShadow"]
      )
      proj_mode_rs;
  let are_actions_enabled_rs =
    React.S.map
      (
        fun
        | HZ => true
        | Textual => false
        | Shadow => false
      )
      proj_mode_rs;
  let expr_proj_view_rs =
    React.S.map
      (
        fun sdoc => /* [
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
              [Pretty.HTML_Of_SDoc.html_of_sdoc sdoc]
         /* )
        ] */
      )
      expr_sdoc_rs;
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
                    Action_palette.make_palette (rs, rf) are_actions_enabled_rs,
                    div a::[a_class ["panel-title"]] [pcdata "Options"],
                    show_hole_names_checkbox,
                    show_hole_envs_checkbox
                  ]
              ]
            ]
        ]
    )
};
