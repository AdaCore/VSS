--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.XML.Implementation.HTML_Writer_Data is

   use type VSS.Strings.Virtual_String;

   a_Prefix : constant VSS.Strings.Virtual_String := "a";
   b_Prefix : constant VSS.Strings.Virtual_String := "b";
   c_Prefix : constant VSS.Strings.Virtual_String := "c";
   d_Prefix : constant VSS.Strings.Virtual_String := "d";
   e_Prefix : constant VSS.Strings.Virtual_String := "e";
   f_Prefix : constant VSS.Strings.Virtual_String := "f";
   h_Prefix : constant VSS.Strings.Virtual_String := "h";
   i_Prefix : constant VSS.Strings.Virtual_String := "i";
   l_Prefix : constant VSS.Strings.Virtual_String := "l";
   m_Prefix : constant VSS.Strings.Virtual_String := "m";
   n_Prefix : constant VSS.Strings.Virtual_String := "n";
   o_Prefix : constant VSS.Strings.Virtual_String := "o";
   p_Prefix : constant VSS.Strings.Virtual_String := "p";
   r_Prefix : constant VSS.Strings.Virtual_String := "r";
   s_Prefix : constant VSS.Strings.Virtual_String := "s";
   t_Prefix : constant VSS.Strings.Virtual_String := "t";
   u_Prefix : constant VSS.Strings.Virtual_String := "u";
   v_Prefix : constant VSS.Strings.Virtual_String := "v";

   a_Tag          : constant VSS.Strings.Virtual_String := "a";
   abbr_Tag       : constant VSS.Strings.Virtual_String := "abbr";
   address_Tag    : constant VSS.Strings.Virtual_String := "address";
   area_Tag       : constant VSS.Strings.Virtual_String := "area";
   article_Tag    : constant VSS.Strings.Virtual_String := "article";
   aside_Tag      : constant VSS.Strings.Virtual_String := "aside";
   audio_Tag      : constant VSS.Strings.Virtual_String := "audio";
   b_Tag          : constant VSS.Strings.Virtual_String := "b";
   base_Tag       : constant VSS.Strings.Virtual_String := "base";
   bdi_Tag        : constant VSS.Strings.Virtual_String := "bdi";
   bdo_Tag        : constant VSS.Strings.Virtual_String := "bdo";
   blockquote_Tag : constant VSS.Strings.Virtual_String := "blockquote";
   body_Tag       : constant VSS.Strings.Virtual_String := "body";
   br_Tag         : constant VSS.Strings.Virtual_String := "br";
   button_Tag     : constant VSS.Strings.Virtual_String := "button";
   canvas_Tag     : constant VSS.Strings.Virtual_String := "canvas";
   caption_Tag    : constant VSS.Strings.Virtual_String := "caption";
   cite_Tag       : constant VSS.Strings.Virtual_String := "cite";
   code_Tag       : constant VSS.Strings.Virtual_String := "code";
   col_Tag        : constant VSS.Strings.Virtual_String := "col";
   colgroup_Tag   : constant VSS.Strings.Virtual_String := "colgroup";
   data_Tag       : constant VSS.Strings.Virtual_String := "data";
   datalist_Tag   : constant VSS.Strings.Virtual_String := "datalist";
   dd_Tag         : constant VSS.Strings.Virtual_String := "dd";
   del_Tag        : constant VSS.Strings.Virtual_String := "del";
   details_Tag    : constant VSS.Strings.Virtual_String := "details";
   dfn_Tag        : constant VSS.Strings.Virtual_String := "dfn";
   dialog_Tag     : constant VSS.Strings.Virtual_String := "dialog";
   div_Tag        : constant VSS.Strings.Virtual_String := "div";
   dl_Tag         : constant VSS.Strings.Virtual_String := "dl";
   dt_Tag         : constant VSS.Strings.Virtual_String := "dt";
   em_Tag         : constant VSS.Strings.Virtual_String := "em";
   embed_Tag      : constant VSS.Strings.Virtual_String := "embed";
   fieldset_Tag   : constant VSS.Strings.Virtual_String := "fieldset";
   figcaption_Tag : constant VSS.Strings.Virtual_String := "figcaption";
   figure_Tag     : constant VSS.Strings.Virtual_String := "figure";
   footer_Tag     : constant VSS.Strings.Virtual_String := "footer";
   form_Tag       : constant VSS.Strings.Virtual_String := "form";
   h1_Tag         : constant VSS.Strings.Virtual_String := "h1";
   h2_Tag         : constant VSS.Strings.Virtual_String := "h2";
   h3_Tag         : constant VSS.Strings.Virtual_String := "h3";
   h4_Tag         : constant VSS.Strings.Virtual_String := "h4";
   h5_Tag         : constant VSS.Strings.Virtual_String := "h5";
   h6_Tag         : constant VSS.Strings.Virtual_String := "h6";
   head_Tag       : constant VSS.Strings.Virtual_String := "head";
   header_Tag     : constant VSS.Strings.Virtual_String := "header";
   hgroup_Tag     : constant VSS.Strings.Virtual_String := "hgroup";
   hr_Tag         : constant VSS.Strings.Virtual_String := "hr";
   html_Tag       : constant VSS.Strings.Virtual_String := "html";
   i_Tag          : constant VSS.Strings.Virtual_String := "i";
   iframe_Tag     : constant VSS.Strings.Virtual_String := "iframe";
   img_Tag        : constant VSS.Strings.Virtual_String := "img";
   input_Tag      : constant VSS.Strings.Virtual_String := "input";
   ins_Tag        : constant VSS.Strings.Virtual_String := "ins";
   kbd_Tag        : constant VSS.Strings.Virtual_String := "kbd";
   label_Tag      : constant VSS.Strings.Virtual_String := "label";
   legend_Tag     : constant VSS.Strings.Virtual_String := "legend";
   li_Tag         : constant VSS.Strings.Virtual_String := "li";
   link_Tag       : constant VSS.Strings.Virtual_String := "link";
   main_Tag       : constant VSS.Strings.Virtual_String := "main";
   map_Tag        : constant VSS.Strings.Virtual_String := "map";
   mark_Tag       : constant VSS.Strings.Virtual_String := "mark";
   menu_Tag       : constant VSS.Strings.Virtual_String := "menu";
   meta_Tag       : constant VSS.Strings.Virtual_String := "meta";
   meter_Tag      : constant VSS.Strings.Virtual_String := "meter";
   nav_Tag        : constant VSS.Strings.Virtual_String := "nav";
   noscript_Tag   : constant VSS.Strings.Virtual_String := "noscript";
   object_Tag     : constant VSS.Strings.Virtual_String := "object";
   ol_Tag         : constant VSS.Strings.Virtual_String := "ol";
   optgroup_Tag   : constant VSS.Strings.Virtual_String := "optgroup";
   option_Tag     : constant VSS.Strings.Virtual_String := "option";
   output_Tag     : constant VSS.Strings.Virtual_String := "output";
   p_Tag          : constant VSS.Strings.Virtual_String := "p";
   picture_Tag    : constant VSS.Strings.Virtual_String := "picture";
   pre_Tag        : constant VSS.Strings.Virtual_String := "pre";
   progress_Tag   : constant VSS.Strings.Virtual_String := "progress";
   q_Tag          : constant VSS.Strings.Virtual_String := "q";
   rt_Tag         : constant VSS.Strings.Virtual_String := "rt";
   rp_Tag         : constant VSS.Strings.Virtual_String := "rp";
   ruby_Tag       : constant VSS.Strings.Virtual_String := "ruby";
   s_Tag          : constant VSS.Strings.Virtual_String := "s";
   samp_Tag       : constant VSS.Strings.Virtual_String := "samp";
   script_Tag     : constant VSS.Strings.Virtual_String := "script";
   section_Tag    : constant VSS.Strings.Virtual_String := "section";
   select_Tag     : constant VSS.Strings.Virtual_String := "select";
   slot_Tag       : constant VSS.Strings.Virtual_String := "slot";
   small_Tag      : constant VSS.Strings.Virtual_String := "small";
   source_Tag     : constant VSS.Strings.Virtual_String := "source";
   span_Tag       : constant VSS.Strings.Virtual_String := "span";
   strong_Tag     : constant VSS.Strings.Virtual_String := "strong";
   style_Tag      : constant VSS.Strings.Virtual_String := "style";
   sub_Tag        : constant VSS.Strings.Virtual_String := "sub";
   summary_Tag    : constant VSS.Strings.Virtual_String := "summary";
   sup_Tag        : constant VSS.Strings.Virtual_String := "sup";
   table_Tag      : constant VSS.Strings.Virtual_String := "table";
   tbody_Tag      : constant VSS.Strings.Virtual_String := "tbody";
   td_Tag         : constant VSS.Strings.Virtual_String := "td";
   template_Tag   : constant VSS.Strings.Virtual_String := "template";
   textarea_Tag   : constant VSS.Strings.Virtual_String := "textarea";
   tfoot_Tag      : constant VSS.Strings.Virtual_String := "tfoot";
   th_Tag         : constant VSS.Strings.Virtual_String := "th";
   thead_Tag      : constant VSS.Strings.Virtual_String := "thead";
   time_Tag       : constant VSS.Strings.Virtual_String := "time";
   title_Tag      : constant VSS.Strings.Virtual_String := "title";
   tr_Tag         : constant VSS.Strings.Virtual_String := "tr";
   track_Tag      : constant VSS.Strings.Virtual_String := "track";
   u_Tag          : constant VSS.Strings.Virtual_String := "u";
   ul_Tag         : constant VSS.Strings.Virtual_String := "ul";
   var_Tag        : constant VSS.Strings.Virtual_String := "var";
   video_Tag      : constant VSS.Strings.Virtual_String := "video";
   wbr_Tag        : constant VSS.Strings.Virtual_String := "wbr";

   allowfullscreen_Attribute :
     constant VSS.Strings.Virtual_String := "allowfullscreen";
   async_Attribute      : constant VSS.Strings.Virtual_String := "async";
   autofocus_Attribute  : constant VSS.Strings.Virtual_String := "autofocus";
   autoplay_Attribute   : constant VSS.Strings.Virtual_String := "autoplay";
   checked_Attribute    : constant VSS.Strings.Virtual_String := "checked";
   controls_Attribute   : constant VSS.Strings.Virtual_String := "controls";
   default_Attribute    : constant VSS.Strings.Virtual_String := "default";
   defer_Attribute      : constant VSS.Strings.Virtual_String := "defer";
   disabled_Attribute   : constant VSS.Strings.Virtual_String := "disabled";
   formnovalidate_Attribute :
     constant VSS.Strings.Virtual_String := "formnovalidate";
   hidden_Attribute     : constant VSS.Strings.Virtual_String := "hidden";
   inert_Attribute      : constant VSS.Strings.Virtual_String := "inert";
   ismap_Attribute      : constant VSS.Strings.Virtual_String := "ismap";
   itemscope_Attribute  : constant VSS.Strings.Virtual_String := "itemscope";
   loop_Attribute       : constant VSS.Strings.Virtual_String := "loop";
   multiple_Attribute   : constant VSS.Strings.Virtual_String := "multiple";
   muted_Attribute      : constant VSS.Strings.Virtual_String := "muted";
   nomodule_Attribute   : constant VSS.Strings.Virtual_String := "nomodule";
   novalidate_Attribute : constant VSS.Strings.Virtual_String := "novalidate";
   open_Attribute       : constant VSS.Strings.Virtual_String := "open";
   playsinline_Attribute :
     constant VSS.Strings.Virtual_String := "playsinline";
   readonly_Attribute   : constant VSS.Strings.Virtual_String := "readonly";
   required_Attribute   : constant VSS.Strings.Virtual_String := "required";
   reversed_Attribute   : constant VSS.Strings.Virtual_String := "reversed";
   selected_Attribute   : constant VSS.Strings.Virtual_String := "selected";
   truespeed_Attribute  : constant VSS.Strings.Virtual_String := "truespeed";

   -------------
   -- Element --
   -------------

   function Element
     (Self    : Start_Tag_First_Child_Conditions;
      Element : HTML_Element_Kind) return Boolean is
   begin
      return Value (Self.Element, Element);
   end Element;

   --------------------------
   -- Is_Boolean_Attribute --
   --------------------------

   function Is_Boolean_Attribute
     (Name : VSS.Strings.Virtual_String) return Boolean is
   begin
      if Name.Starts_With (a_Prefix) then
         return
           Name in allowfullscreen_Attribute | async_Attribute
                     | autofocus_Attribute | autoplay_Attribute;

      elsif Name.Starts_With (c_Prefix) then
         return Name in checked_Attribute | controls_Attribute;

      elsif Name.Starts_With (d_Prefix) then
         return
           Name in default_Attribute | defer_Attribute | disabled_Attribute;

      elsif Name.Starts_With (f_Prefix) then
         return Name = formnovalidate_Attribute;

      elsif Name.Starts_With (h_Prefix) then
         return Name = hidden_Attribute;

      elsif Name.Starts_With (i_Prefix) then
         return
           Name in inert_Attribute | ismap_Attribute | itemscope_Attribute;

      elsif Name.Starts_With (l_Prefix) then
         return Name = loop_Attribute;

      elsif Name.Starts_With (m_Prefix) then
         return Name in multiple_Attribute | muted_Attribute;

      elsif Name.Starts_With (n_Prefix) then
         return Name in nomodule_Attribute | novalidate_Attribute;

      elsif Name.Starts_With (o_Prefix) then
         return Name = open_Attribute;

      elsif Name.Starts_With (p_Prefix) then
         return Name = playsinline_Attribute;

      elsif Name.Starts_With (r_Prefix) then
         return
           Name in readonly_Attribute | required_Attribute
                     | reversed_Attribute;

      elsif Name.Starts_With (s_Prefix) then
         return Name = selected_Attribute;

      elsif Name.Starts_With (t_Prefix) then
         return Name = truespeed_Attribute;
      end if;

      return False;
   end Is_Boolean_Attribute;

   ---------------------
   -- To_HTML_Element --
   ---------------------

   function To_HTML_Element
     (Name : VSS.Strings.Virtual_String) return HTML_Element_Kind is
   begin
      if Name.Starts_With (a_Prefix) then
         if Name = a_Tag then
            return a_Element;

         elsif Name = abbr_Tag then
            return abbr_Element;

         elsif Name = address_Tag then
            return address_Element;

         elsif Name = area_Tag then
            return area_Element;

         elsif Name = article_Tag then
            return article_Element;

         elsif Name = aside_Tag then
            return aside_Element;

         elsif Name = audio_Tag then
            return audio_Element;
         end if;

      elsif Name.Starts_With (b_Prefix) then
         if Name = b_Tag then
            return b_Element;

         elsif Name = base_Tag then
            return base_Element;

         elsif Name = bdi_Tag then
            return bdi_Element;

         elsif Name = bdo_Tag then
            return bdo_Element;

         elsif Name = blockquote_Tag then
            return blockquote_Element;

         elsif Name = body_Tag then
            return body_Element;

         elsif Name = br_Tag then
            return br_Element;

         elsif Name = button_Tag then
            return button_Element;
         end if;

      elsif Name.Starts_With (c_Prefix) then
         if Name = canvas_Tag then
            return canvas_Element;

         elsif Name = caption_Tag then
            return caption_Element;

         elsif Name = cite_Tag then
            return cite_Element;

         elsif Name = code_Tag then
            return code_Element;

         elsif Name = col_Tag then
            return col_Element;

         elsif Name = colgroup_Tag then
            return colgroup_Element;
         end if;

      elsif Name.Starts_With (d_Prefix) then
         if Name = data_Tag then
            return data_Element;

         elsif Name = datalist_Tag then
            return datalist_Element;

         elsif Name = dd_Tag then
            return dd_Element;

         elsif Name = del_Tag then
            return del_Element;

         elsif Name = details_Tag then
            return details_Element;

         elsif Name = dfn_Tag then
            return dfn_Element;

         elsif Name = dialog_Tag then
            return dialog_Element;

         elsif Name = div_Tag then
            return div_Element;

         elsif Name = dl_Tag then
            return dl_Element;

         elsif Name = dt_Tag then
            return dt_Element;
         end if;

      elsif Name.Starts_With (e_Prefix) then
         if Name = em_Tag then
            return em_Element;

         elsif Name = embed_Tag then
            return embed_Element;
         end if;

      elsif Name.Starts_With (f_Prefix) then
         if Name = fieldset_Tag then
            return fieldset_Element;

         elsif Name = figcaption_Tag then
            return figcaption_Element;

         elsif Name = figure_Tag then
            return figure_Element;

         elsif Name = footer_Tag then
            return footer_Element;

         elsif Name = form_Tag then
            return form_Element;
         end if;

      elsif Name.Starts_With (h_Prefix) then
         if Name = h1_Tag then
            return h1_Element;

         elsif Name = h2_Tag then
            return h2_Element;

         elsif Name = h3_Tag then
            return h3_Element;

         elsif Name = h4_Tag then
            return h4_Element;

         elsif Name = h5_Tag then
            return h5_Element;

         elsif Name = h6_Tag then
            return h6_Element;

         elsif Name = head_Tag then
            return head_Element;

         elsif Name = header_Tag then
            return header_Element;

         elsif Name = hgroup_Tag then
            return hgroup_Element;

         elsif Name = hr_Tag then
            return hr_Element;

         elsif Name = html_Tag then
            return html_Element;
         end if;

      elsif Name.Starts_With (i_Prefix) then
         if Name = i_Tag then
            return i_Element;

         elsif Name = iframe_Tag then
            return iframe_Element;

         elsif Name = img_Tag then
            return img_Element;

         elsif Name = input_Tag then
            return input_Element;

         elsif Name = ins_Tag then
            return ins_Element;
         end if;

      elsif Name = kbd_Tag then
         return kbd_Element;

      elsif Name.Starts_With (l_Prefix) then
         if Name = label_Tag then
            return label_Element;

         elsif Name = legend_Tag then
            return legend_Element;

         elsif Name = li_Tag then
            return li_Element;

         elsif Name = link_Tag then
            return link_Element;
         end if;

      elsif Name.Starts_With (m_Prefix) then
         if Name = main_Tag then
            return main_Element;

         elsif Name = map_Tag then
            return map_Element;

         elsif Name = mark_Tag then
            return mark_Element;

         elsif Name = menu_Tag then
            return menu_Element;

         elsif Name = meta_Tag then
            return meta_Element;

         elsif Name = meter_Tag then
            return meter_Element;
         end if;

      elsif Name.Starts_With (n_Prefix) then
         if Name = nav_Tag then
            return nav_Element;

         elsif Name = noscript_Tag then
            return noscript_Element;
         end if;

      elsif Name.Starts_With (o_Prefix) then
         if Name = object_Tag then
            return object_Element;

         elsif Name = ol_Tag then
            return ol_Element;

         elsif Name = optgroup_Tag then
            return optgroup_Element;

         elsif Name = option_Tag then
            return option_Element;

         elsif Name = output_Tag then
            return output_Element;
         end if;

      elsif Name.Starts_With (p_Prefix) then
         if Name = p_Tag then
            return p_Element;

         elsif Name = picture_Tag then
            return picture_Element;

         elsif Name = pre_Tag then
            return pre_Element;

         elsif Name = progress_Tag then
            return progress_Element;
         end if;

      elsif Name = q_Tag then
         return q_Element;

      elsif Name.Starts_With (r_Prefix) then
         if Name = rp_Tag then
            return rp_Element;

         elsif Name = rt_Tag then
            return rt_Element;

         elsif Name = ruby_Tag then
            return ruby_Element;
         end if;

      elsif Name.Starts_With (s_Prefix) then
         if Name = s_Tag then
            return s_Element;

         elsif Name = samp_Tag then
            return samp_Element;

         elsif Name = script_Tag then
            return script_Element;

         elsif Name = section_Tag then
            return section_Element;

         elsif Name = select_Tag then
            return select_Element;

         elsif Name = slot_Tag then
            return slot_Element;

         elsif Name = small_Tag then
            return small_Element;

         elsif Name = source_Tag then
            return source_Element;

         elsif Name = span_Tag then
            return span_Element;

         elsif Name = strong_Tag then
            return strong_Element;

         elsif Name = style_Tag then
            return style_Element;

         elsif Name = sub_Tag then
            return sub_Element;

         elsif Name = summary_Tag then
            return summary_Element;

         elsif Name = sup_Tag then
            return sup_Element;
         end if;

      elsif Name.Starts_With (t_Prefix) then
         if Name = table_Tag then
            return table_Element;

         elsif Name = tbody_Tag then
            return tbody_Element;

         elsif Name = td_Tag then
            return td_Element;

         elsif Name = template_Tag then
            return template_Element;

         elsif Name = textarea_Tag then
            return textarea_Element;

         elsif Name = tfoot_Tag then
            return tfoot_Element;

         elsif Name = th_Tag then
            return th_Element;

         elsif Name = thead_Tag then
            return thead_Element;

         elsif Name = time_Tag then
            return time_Element;

         elsif Name = title_Tag then
            return title_Element;

         elsif Name = tr_Tag then
            return tr_Element;

         elsif Name = track_Tag then
            return track_Element;
         end if;

      elsif Name.Starts_With (u_Prefix) then
         if Name = u_Tag then
            return u_Element;

         elsif Name = ul_Tag then
            return ul_Element;
         end if;

      elsif Name.Starts_With (v_Prefix) then
         if Name = var_Tag then
            return var_Element;

         elsif Name = video_Tag then
            return video_Element;
         end if;

      elsif Name = wbr_Tag then
         return wbr_Element;
      end if;

      return Anonymous_Custom_Element;
   end To_HTML_Element;

   -----------
   -- Value --
   -----------

   function Value
     (Self    : Element_Flags;
      Element : HTML_Element_Kind) return Boolean is
   begin
      if Element in Self.Specific'Range then
         return Self.Specific (Element);

      else
         return Self.Other;
      end if;
   end Value;

end VSS.XML.Implementation.HTML_Writer_Data;
