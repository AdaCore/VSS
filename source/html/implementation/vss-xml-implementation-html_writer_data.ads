--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;

package VSS.XML.Implementation.HTML_Writer_Data is

   HTML_New_Line_Function : constant VSS.Strings.Line_Terminator_Set :=
     (VSS.Strings.CR | VSS.Strings.LF | VSS.Strings.CRLF => True,
      others                                             => False);
   --  Line terminators allowed by the HTML specification.

   --  HTML elements are reordered to group together all elements that appear
   --  in the tag omitting conditions according to WhatWG's HTML living
   --  standard 2022-06-17.
   --
   --   ^        preciding
   --     ^      parent
   --       ^    child
   --         ^  following
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

   function Is_Boolean_Attribute
     (Name : VSS.Strings.Virtual_String) return Boolean;
   --  Return True when attribute is know HTML boolean attribute.

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

   type Element_Flags is record
      Specific : Condition_Element_Flags := (others => False);
      --  Value for specific kind of elements.
      Other    : Boolean := False;
      --  Common value for all other kinds of elements.
   end record;

   function Value
     (Self    : Element_Flags;
      Element : HTML_Element_Kind) return Boolean;
   --  Return value for the given element.

   type Start_Tag_First_Child_Conditions is record
      Text       : Boolean := False;
      Whitespace : Boolean := False;
      Comment    : Boolean := False;
      Element    : Element_Flags := (others => <>);
   end record;

   function Element
     (Self    : Start_Tag_First_Child_Conditions;
      Element : HTML_Element_Kind) return Boolean;
   --  Returns value of the condition for given kind of the elements.

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
            Next_Sibling  : Start_Tag_First_Child_Conditions;
            End_Of_Parent : Element_Flags;
      end case;
   end record;

   type Element_Kinds is
     (Void, Template, Raw_Text, Escapable_Text, Normal, Foreign);

   type Text_Children is (No, Transparent, Yes);

   type Element_Properties is record
      Kind      : Element_Kinds := Normal;
      Text      : Text_Children := Yes;
      Start_Tag : Omit_Start_Tag_Rules := (May_Be_Omitted => False);
      End_Tag   : Omit_End_Tag_Rules   := (May_Be_Omitted => False);
   end record;

   No_Elements  : constant Element_Flags :=
     (Other => False, Specific => (others => False));
   All_Elements : constant Element_Flags :=
     (Other => True, Specific => (others => True));

   Void_Element_Properties : constant Element_Properties :=
     (Kind      => Void,
      Text      => No,
      Start_Tag => (May_Be_Omitted => False),
      End_Tag   => (May_Be_Omitted => False));

   Normal_No_Text_Properties : constant Element_Properties :=
     (Kind      => Normal,
      Text      => No,
      Start_Tag => (May_Be_Omitted => False),
      End_Tag   => (May_Be_Omitted => False));

   Normal_Transparent_Properties : constant Element_Properties :=
     (Kind      => Normal,
      Text      => Transparent,
      Start_Tag => (May_Be_Omitted => False),
      End_Tag   => (May_Be_Omitted => False));

   Properties : constant array (HTML_Element_Kind) of Element_Properties :=
     (a_Element        => Normal_Transparent_Properties,          --  a
      --  abbr_Element,
      --  address_Element,
      area_Element     => Void_Element_Properties,                --  area
      --  article_Element,
      --  aside_Element,
      audio_Element    => Normal_Transparent_Properties,          --  audio
      --  b_Element,
      base_Element     => Void_Element_Properties,                --  base
      --  bdi_Element,
      --  bdo_Element,
      --  blockquote_Element,
      body_Element     =>                                         --  body
        (Kind      => Normal,
         Text      => Yes,
         Start_Tag =>
           (May_Be_Omitted               => True,
            Previous_Sibling_End_Omitted => (others => True),
            First_Child                  =>
              (Whitespace => False,
               Text       => True,
               Comment    => False,
               Element    =>
                 (Other      => True,
                  Specific   =>
                    (link_Element | meta_Element | script_Element
                         | style_Element | template_Element => False,
                     others                                 => True))),
            Is_Empty                     => True),
         End_Tag   =>
           (True,
            (Whitespace => False,
             Text       => False,
             Comment    => False,
             Element    => All_Elements),
            All_Elements)),
      br_Element       => Void_Element_Properties,                --  br
      --  button_Element,
      canvas_Element   => Normal_Transparent_Properties,          --  canvas
      caption_Element  =>                                         --  caption
        (Kind      => Normal,
         Text      => Yes,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace => False,
               Text       => True,
               Comment    => False,
               Element    => All_Elements),
            End_Of_Parent  => No_Elements)),
      --  cite_Element,
      --  code_Element,
      col_Element      => Void_Element_Properties,                --  col
      colgroup_Element =>                                         --  colgroup
        (Kind      => Normal,
         Text      => No,
         Start_Tag =>
           (May_Be_Omitted               => True,
            Previous_Sibling_End_Omitted =>
              (colgroup_Element => False, others => True),
            First_Child                  =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element    =>
                 ((col_Element => True, others => False), Other => False)),
            Is_Empty                     => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace => False,
               Text       => True,
               Comment    => False,
               Element    => All_Elements),
            End_Of_Parent  => No_Elements)),
      --  data_Element,
      --  datalist_Element,
      dd_Element       =>                                         --  dd
        (Kind      => Normal,
         Text      => Yes,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element        =>
                 ((dt_Element | dd_Element => True, others => False),
                  False)),
            End_Of_Parent  => All_Elements)),
      del_Element      => Normal_Transparent_Properties,          --  del
      --  details_Element,
      --  dfn_Element,
      --  dialog_Element,
      --  div_Element,
      dl_Element       => Normal_No_Text_Properties,              --  dl
      dt_Element       =>                                         --  dt
        (Kind      => Normal,
         Text      => Yes,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace     => False,
               Text           => False,
               Comment        => False,
               Element        =>
                 ((dt_Element | dd_Element => True, others => False),
                  False)),
            End_Of_Parent  => No_Elements)),
      --  em_Element,
      embed_Element    => Void_Element_Properties,                --  embed
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
      head_Element     =>                                         --  head
        (Kind      => Normal,
         Text      => No,
         Start_Tag =>
           (True,
            (others => True),
            (Text | Whitespace | Comment => False,
             Element                     => All_Elements),
            True),
         End_Tag   =>
           (True,
            (Whitespace => False,
             Text       => True,
             Comment    => False,
             Element    => All_Elements),
            All_Elements)),
      --  header_Element,
      hgroup_Element   => Normal_No_Text_Properties,              --  hgroup
      hr_Element       => Void_Element_Properties,                --  hr
      html_Element     =>                                         --  html
        (Kind      => Normal,
         Text      => No,
         Start_Tag =>
           (True,
            (others => True),
            (Text | Whitespace | Comment => False,
             Element                     => All_Elements),
            False),
         End_Tag =>
           (True,
            (Whitespace => False,
             Text       => False,
             Comment    => False,
             Element    => All_Elements),
            All_Elements)),
      --  i_Element,
      --  iframe_Element,
      img_Element      => Void_Element_Properties,                --  img
      input_Element    => Void_Element_Properties,                --  input
      ins_Element      => Normal_Transparent_Properties,          --  ins
      --  kbd_Element,
      --  label_Element,
      --  legend_Element,
      li_Element       =>                                         --  li
        (Kind      => Normal,
         Text      => Yes,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element    =>
                 (Other    => False,
                  Specific => (li_Element => True, others => False))),
            End_Of_Parent  => All_Elements)),
      link_Element     => Void_Element_Properties,                --  link
      --  main_Element,
      map_Element      => Normal_Transparent_Properties,          --  map
      --  mark_Element,
      menu_Element     => Normal_No_Text_Properties,              --  menu
      meta_Element     => Void_Element_Properties,                --  meta
      --  meter_Element,
      --  nav_Element,
      --  noscript_Element,
      object_Element   => Normal_Transparent_Properties,          --  object
      ol_Element       => Normal_No_Text_Properties,              --  ol
      optgroup_Element =>                                         --  optgroup
        (Kind      => Normal,
         Text      => No,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element    =>
                 ((optgroup_Element => True, others => False),
                  others => False)),
            End_Of_Parent  => All_Elements)),
      option_Element   =>                                         --  option
        (Kind      => Normal,
         Text      => Yes,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element    =>
                 ((option_Element | optgroup_Element => True,
                   others                            => False),
                  others => False)),
            End_Of_Parent  => All_Elements)),
      --  output_Element,
      p_Element        =>                                         --  p
        (Kind      => Normal,
         Text      => Yes,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element    =>
                 (Other    => False,
                  Specific =>
                    (address_Element | article_Element | aside_Element
                       | blockquote_Element | details_Element | div_Element
                       | dl_Element | fieldset_Element | figcaption_Element
                       | figure_Element | footer_Element | form_Element
                       | h1_Element | h2_Element | h3_Element | h4_Element
                       | h5_Element | h6_Element | header_Element
                       | hgroup_Element | hr_Element | main_Element
                       | menu_Element | nav_Element | ol_Element | p_Element
                       | pre_Element | section_Element | table_Element
                       | ul_Element | Anonymous_Custom_Element => True,
                     others => False))),
            End_Of_Parent  =>
              ((a_Element | audio_Element | del_Element | ins_Element
                | map_Element | noscript_Element | video_Element => False,
                others => True),
               others => True))),
      --  picture_Element,
      --  pre_Element,
      --  progress_Element,
      --  q_Element,
      rp_Element       =>                                         --  rp
        (Kind      => Normal,
         Text      => Yes,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element        =>
                 ((rp_Element | rt_Element => True, others => False),
                  False)),
            End_Of_Parent  => All_Elements)),
      rt_Element       =>                                         --  rt
        (Kind      => Normal,
         Text      => Yes,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element        =>
                 ((rp_Element | rt_Element => True, others => False),
                  False)),
            End_Of_Parent  => All_Elements)),
      --  ruby_Element,
      --  s_Element,
      --  samp_Element,
      script_Element   =>                                         --  script
        (Raw_Text, Yes, (May_Be_Omitted => False), (May_Be_Omitted => False)),
      --  section_Element,
      select_Element   => Normal_No_Text_Properties,              --  select
      slot_Element     => Normal_Transparent_Properties,          --  slot
      --  small_Element,
      source_Element   => Void_Element_Properties,                --  source
      --  span_Element,
      --  strong_Element,
      style_Element    =>                                         --  style
        (Raw_Text, Yes, (May_Be_Omitted => False), (May_Be_Omitted => False)),
      --  sub_Element,
      --  summary_Element,
      --  sup_Element,
      table_Element    => Normal_No_Text_Properties,              --  table
      tbody_Element    =>                                         --  tbody
        (Kind      => Normal,
         Text      => No,
         Start_Tag =>
           (May_Be_Omitted               => True,
            Previous_Sibling_End_Omitted =>
              (tbody_Element | thead_Element | tfoot_Element => False,
               others                                        => True),
            First_Child                  =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element    =>
                 ((tr_Element => True, others => False), others => False)),
            Is_Empty                     => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element    =>
                 ((tbody_Element | tfoot_Element => True, others => False),
                  others => False)),
            End_Of_Parent  => All_Elements)),
      td_Element       =>                                         --  td
        (Kind      => Normal,
         Text      => Yes,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element    =>
                 ((td_Element | th_Element => True, others => False),
                  others => False)),
            End_Of_Parent  => All_Elements)),
      --  template_Element,
      textarea_Element =>                                         --  textarea
        (Kind      => Escapable_Text,
         Text      => Yes,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   => (May_Be_Omitted => False)),
      tfoot_Element    =>                                         --  tfoot
        (Kind      => Normal,
         Text      => No,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element    => No_Elements),
            End_Of_Parent  => All_Elements)),
      th_Element       =>                                         --  th
        (Kind      => Normal,
         Text      => Yes,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element    =>
                 ((td_Element | th_Element => True, others => False),
                  others => False)),
            End_Of_Parent  => All_Elements)),
      thead_Element    =>                                         --  thead
        (Kind      => Normal,
         Text      => No,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element    =>
                 ((tbody_Element | tfoot_Element => True, others => False),
                  others => False)),
            End_Of_Parent  => No_Elements)),
      --  time_Element,
      title_Element    =>                                         --  title
        (Kind      => Escapable_Text,
         Text      => Yes,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   => (May_Be_Omitted => False)),
      tr_Element       =>                                         --  tr
        (Kind      => Normal,
         Text      => No,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   =>
           (May_Be_Omitted => True,
            Next_Sibling   =>
              (Whitespace => False,
               Text       => False,
               Comment    => False,
               Element    =>
                 ((tr_Element => True, others => False), others => False)),
            End_Of_Parent  => All_Elements)),
      track_Element    => Void_Element_Properties,                --  track
      --  u_Element,
      ul_Element       => Normal_No_Text_Properties,              --  ul
      --  var_Element,
      video_Element    => Normal_Transparent_Properties,          --  video
      wbr_Element      => Void_Element_Properties,                --  wbr
      Foreign          =>
        (Kind      => Foreign,
         Text      => Yes,
         Start_Tag => (May_Be_Omitted => False),
         End_Tag   => (May_Be_Omitted => False)),
      others           =>
        (Normal, Yes, (May_Be_Omitted => False), (May_Be_Omitted => False)));

end VSS.XML.Implementation.HTML_Writer_Data;
