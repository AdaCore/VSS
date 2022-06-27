--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;

package VSS.XML.Implementation.HTML_Writer_Data is

   --  pragma Preelaborate;

   --  HTML elements are reordered to group together all elements that appear
   --  in the tag omitting conditions.

   type HTML_Element_Kind is
     (Anonymous_Custom_Element,

      a_Element,
      address_Element,
      article_Element,
      aside_Element,
      audio_Element,
      blockquote_Element,
      col_Element,
      colgroup_Element,
      dd_Element,
      del_Element,
      details_Element,
      div_Element,
      dl_Element,
      dt_Element,
      fieldset_Element,
      figcaption_Element,
      figure_Element,
      footer_Element,
      form_Element,
      h1_Element,
      h2_Element,
      h3_Element,
      h4_Element,
      h5_Element,
      h6_Element,
      header_Element,
      hgroup_Element,
      hr_Element,
      ins_Element,
      li_Element,
      link_Element,
      main_Element,
      map_Element,
      menu_Element,
      meta_Element,
      nav_Element,
      noscript_Element,
      ol_Element,
      optgroup_Element,
      option_Element,
      p_Element,
      pre_Element,
      rp_Element,
      rt_Element,
      script_Element,
      section_Element,
      style_Element,
      table_Element,
      tbody_Element,
      td_Element,
      template_Element,
      tfoot_Element,
      th_Element,
      thead_Element,
      tr_Element,
      ul_Element,
      video_Element,

      abbr_Element,
      area_Element,
      b_Element,
      base_Element,
      bdi_Element,
      bdo_Element,
      body_Element,
      br_Element,
      button_Element,
      canvas_Element,
      caption_Element,
      cite_Element,
      code_Element,
      data_Element,
      datalist_Element,
      dfn_Element,
      dialog_Element,
      em_Element,
      embed_Element,
      head_Element,
      html_Element,
      i_Element,
      iframe_Element,
      img_Element,
      input_Element,
      kbd_Element,
      label_Element,
      legend_Element,
      mark_Element,
      meter_Element,
      object_Element,
      output_Element,
      picture_Element,
      progress_Element,
      q_Element,
      ruby_Element,
      s_Element,
      samp_Element,
      select_Element,
      slot_Element,
      small_Element,
      source_Element,
      span_Element,
      strong_Element,
      sub_Element,
      summary_Element,
      sup_Element,
      textarea_Element,
      time_Element,
      title_Element,
      track_Element,
      u_Element,
      var_Element,
      wbr_Element,

      Foreign);

   function To_HTML_Element
     (Name : VSS.Strings.Virtual_String) return HTML_Element_Kind;
   --  Convert element's tag name into known HTML element.

   subtype Condition_HTML_Element_Kind is HTML_Element_Kind
     range Anonymous_Custom_Element .. video_Element;

   type Condition_Element_Flags is
     array (Condition_HTML_Element_Kind) of Boolean with Pack;

   type Start_Tag_First_Child_Conditions is record
      Text       : Boolean := False;
      Whitespace : Boolean := False;
      Comment    : Boolean := False;
      Element    : Boolean := False;
      HTML       : Condition_Element_Flags := (others => False);
   end record;

   type Omit_Start_Tag_Rules (May_Be_Omitted : Boolean := False) is record
      case May_Be_Omitted is
         when False =>
            null;

         when True =>
            Previous_Sibling_End_Omitted : Condition_Element_Flags;
            --  Whether the start tag of the current element may be omitted
            --  if the end tag of the immediately preciding element has been
            --  omitted.

            First_Child                  : Start_Tag_First_Child_Conditions;
            --  Whether the start tag of the current element may be omitted
            --  when first thing inside the element is given event. It apply
            --  only when Previous_Sibling is allowed to omit start tag too.

            Is_Empty                     : Boolean;
            --  Whether the start tag of the current element may be omitted
            --  when element is empty. Previous_Sibling and First_Child is
            --  not used in this case.
      end case;
   end record;

   type Omit_End_Tag_Rules (May_Be_Omitted : Boolean := False) is record
      case May_Be_Omitted is
         when False =>
            null;

         when True =>
            Next_Sibling       : Start_Tag_First_Child_Conditions;
            End_Of_Parent      : Boolean;
            End_Of_HTML_Parent : Condition_Element_Flags;
      end case;
   end record;

   --  Elements, used in omitting conditions
   --
   --   +        preciding
   --     +      parent
   --       +    child
   --         +  following
   --
   --     +      a
   --         +  address
   --         +  article
   --         +  aside
   --     +      audio
   --         +  blockquote
   --       +    col
   --   +        colgroup
   --         +  dd
   --     +      del
   --         +  details
   --         +  div
   --         +  dl
   --         +  dt
   --         +  fieldset
   --         +  figcaption
   --         +  figure
   --         +  footer
   --         +  form
   --         +  h1
   --         +  h2
   --         +  h3
   --         +  h4
   --         +  h5
   --         +  h6
   --         +  header
   --         +  hgroup
   --         +  hr
   --     +      ins
   --         +  li
   --       +    link
   --         +  main
   --     +      map
   --         +  menu
   --       +    meta
   --         +  nav
   --     +      noscript
   --         +  ol
   --         +  option
   --         +  optgroup
   --         +  p
   --         +  pre
   --         +  rp
   --         +  rt
   --       +    script
   --         +  section
   --       +    style
   --         +  table
   --   +     +  tbody
   --         +  td
   --       +    template
   --   +     +  tfoot
   --         +  th
   --   +        thead
   --       + +  tr
   --         +  ul
   --     +      video
   --     +      anonymous custom element

   type Element_Kinds is
     (Void, Template, Raw_Text, Escapable_Text, Normal, Foreign);

   type Text_Children is (No, Transparent, Yes);

   type Element_Properties is record
      Kind      : Element_Kinds := Normal;
      Text      : Text_Children := Yes;
      Start_Tag : Omit_Start_Tag_Rules := (May_Be_Omitted => False);
      End_Tag   : Omit_End_Tag_Rules   := (May_Be_Omitted => False);
   end record;

   --  Ignore_First_Child : constant Start_Tag_First_Child_Conditions :=
   --    (Text       => False,
   --     Whitespace => False,
   --     Comment    => False,
   --     Element    => False,
   --     HTML       => (others => False));

   Void_Element_Properties : constant Element_Properties :=
     (Kind      => Void,
      Text      => No,
      Start_Tag => (May_Be_Omitted => False),
      End_Tag   => (May_Be_Omitted => False));

   Properties : constant array (HTML_Element_Kind) of Element_Properties :=
     (
      --  a_Element,
      --  abbr_Element,
      --  address_Element,
      area_Element    => Void_Element_Properties,
      --  article_Element,
      --  aside_Element,
      --  audio_Element,
      --  b_Element,
      base_Element    => Void_Element_Properties,
      --  bdi_Element,
      --  bdo_Element,
      --  blockquote_Element,
      body_Element    =>
        (Kind      => Normal,
         Text      => Yes,
         Start_Tag =>
           (True,
            (others => True),
            (Whitespace => False,
             Text       => True,
             Comment    => False,
             Element    => True,
             HTML       =>
               (link_Element | meta_Element | script_Element | style_Element
                  | template_Element => False,
                others               => True)),
            True),
         End_Tag   =>
           (True,
            (Whitespace => False,
             Text       => False,
             Comment    => False,
             Element    => True,
             HTML       => (others => True)),
            True,
            (others => True))),
      br_Element      => Void_Element_Properties,
      --  button_Element,
      --  canvas_Element,
      --  caption_Element,
      --  cite_Element,
      --  code_Element,
      col_Element     => Void_Element_Properties,
      --  colgroup_Element,
      --  data_Element,
      --  datalist_Element,
      --  dd_Element,
      --  del_Element,
      --  details_Element,
      --  dfn_Element,
      --  dialog_Element,
      --  div_Element,
      --  dl_Element,
      --  dt_Element,
      --  em_Element,
      embed_Element    => Void_Element_Properties,
      --  fieldset_Element,
      --  figcaption_Element,
      --  figure_Element,
      --  footer_Element,
      --  form_Element,
      --  h1_Element,
      --  h2_Element,
      --  h3_Element,
      --  h4_Element,
      --  h5_Element,
      --  h6_Element,
      head_Element     =>
        (Normal, No,
         (True,
          (others => True),
          (Text | Whitespace | Comment => False,
           Element                     => True,
           HTML                        => (others => True)),
          True),
         (True,
          (Whitespace => False,
           Text       => True,
           Comment    => False,
           Element    => True,
           HTML       => (others => True)),
          True,
          (others => True))),
      --  header_Element,
      --  hgroup_Element,
      hr_Element       => Void_Element_Properties,
      html_Element     =>
        (Normal, No,
         (True,
          (others => True),
          (Text | Whitespace | Comment => False,
           Element                     => True,
           HTML                        => (others => True)),
          False),
         (True,
          (Whitespace => False,
           Text       => False,
           Comment    => False,
           Element    => True,
           HTML       => (others => True)),
          True,
          (others => True))),
      --  i_Element,
      --  iframe_Element,
      img_Element      => Void_Element_Properties,
      input_Element    => Void_Element_Properties,
      --  ins_Element,
      --  kbd_Element,
      --  label_Element,
      --  legend_Element,
      li_Element       =>
        (Kind      => Normal,
         Text      => Yes,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   =>
           (May_Be_Omitted     => True,
            Next_Sibling       =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element    => False,
               HTML       => (li_Element => True, others => False)),
            End_Of_Parent      => True,
            End_Of_HTML_Parent => (others => True))),
      link_Element     => Void_Element_Properties,
      --  main_Element,
      --  map_Element,
      --  mark_Element,
      --  menu_Element,
      meta_Element     => Void_Element_Properties,
      --  meter_Element,
      --  nav_Element,
      --  noscript_Element,
      --  object_Element,
      --  ol_Element,
      --  optgroup_Element,
      --  option_Element,
      --  output_Element,
      --  p_Element,
      --  picture_Element,
      --  pre_Element,
      --  progress_Element,
      --  q_Element,
      --  rp_Element,
      --  rt_Element,
      --  ruby_Element,
      --  s_Element,
      --  samp_Element,
      script_Element   => (Raw_Text, Yes, (May_Be_Omitted => False), (May_Be_Omitted => False)),
      --  section_Element,
      --  select_Element,
      --  slot_Element,
      --  small_Element,
      source_Element   => Void_Element_Properties,
      --  span_Element,
      --  strong_Element,
      style_Element    => (Raw_Text, Yes, (May_Be_Omitted => False), (May_Be_Omitted => False)),
      --  sub_Element,
      --  summary_Element,
      --  sup_Element,
      --  table_Element,
      --  tbody_Element,
      --  td_Element,
      --  template_Element,
      textarea_Element => (Escapable_Text, Yes, (May_Be_Omitted => False), (May_Be_Omitted => False)),
      --  tfoot_Element,
      --  th_Element,
      --  thead_Element,
      --  time_Element,
      title_Element    => (Escapable_Text, Yes, (May_Be_Omitted => False), (May_Be_Omitted => False)),
      --  tr_Element,
      track_Element    => Void_Element_Properties,
      --  u_Element,
      --  ul_Element,
      --  var_Element,
      --  video_Element,
      wbr_Element      => Void_Element_Properties,
      others           => (Normal, Yes, (May_Be_Omitted => False), (May_Be_Omitted => False)));

end VSS.XML.Implementation.HTML_Writer_Data;
