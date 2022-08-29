--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Ada.Containers.Hashed_Maps;
with VSS.Strings.Hash;

package body VSS.Regular_Expressions.Name_Sets is

   package Category_Maps is new Ada.Containers.Hashed_Maps
     (VSS.Strings.Virtual_String,
      General_Category_Set,
      VSS.Strings.Hash,
      VSS.Strings."=");

   Map : Category_Maps.Map;

   -----------
   -- "not" --
   -----------

   overriding function "not" (Left : General_Category_Set)
     return General_Category_Set is
   begin
      return General_Category_Set (not Boolean_Array (Left));
   end "not";

   ----------
   -- "or" --
   ----------

   overriding function "or"
     (Left, Right : General_Category_Set) return General_Category_Set is
   begin
      return General_Category_Set
        (Boolean_Array (Left) or Boolean_Array (Right));
   end "or";

   -------------
   -- Include --
   -------------

   procedure Include
     (Self  : in out General_Category_Set;
      Value : VSS.Characters.General_Category) is
   begin
      Self (Value) := True;
   end Include;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use all type VSS.Characters.General_Category;
   begin
      Map.Insert
        ("cased_letter",
         [Uppercase_Letter, Lowercase_Letter, Titlecase_Letter]);

      Map.Insert
        ("lc", [Uppercase_Letter, Lowercase_Letter, Titlecase_Letter]);

      Map.Insert ("close_punctuation",     [Close_Punctuation]);
      Map.Insert ("pe",                    [Close_Punctuation]);
      Map.Insert ("connector_punctuation", [Connector_Punctuation]);
      Map.Insert ("pc",                    [Connector_Punctuation]);
      Map.Insert ("control",               [Control]);
      Map.Insert ("cc",                    [Control]);
      Map.Insert ("cntrl",                 [Control]);
      Map.Insert ("currency_symbol",       [Currency_Symbol]);
      Map.Insert ("sc",                    [Currency_Symbol]);
      Map.Insert ("dash_punctuation",      [Dash_Punctuation]);
      Map.Insert ("pd",                    [Dash_Punctuation]);
      Map.Insert ("decimal_number",        [Decimal_Number]);
      Map.Insert ("nd",                    [Decimal_Number]);
      Map.Insert ("digit",                 [Decimal_Number]);
      Map.Insert ("enclosing_mark",        [Enclosing_Mark]);
      Map.Insert ("me",                    [Enclosing_Mark]);
      Map.Insert ("final_punctuation",     [Final_Punctuation]);
      Map.Insert ("pf",                    [Final_Punctuation]);
      Map.Insert ("format",                [Format]);
      Map.Insert ("cf",                    [Format]);
      Map.Insert ("initial_punctuation",   [Initial_Punctuation]);
      Map.Insert ("pi",                    [Initial_Punctuation]);

      Map.Insert
        ("letter",
         [Uppercase_Letter, Lowercase_Letter, Titlecase_Letter,
         Modifier_Letter, Other_Letter]);

      Map.Insert
        ("l",
         [Uppercase_Letter, Lowercase_Letter, Titlecase_Letter,
         Modifier_Letter, Other_Letter]);

      Map.Insert ("letter_number",    [Letter_Number]);
      Map.Insert ("nl",               [Letter_Number]);
      Map.Insert ("line_separator",   [Line_Separator]);
      Map.Insert ("zl",               [Line_Separator]);
      Map.Insert ("lowercase_letter", [Lowercase_Letter]);
      Map.Insert ("ll",               [Lowercase_Letter]);

      Map.Insert ("mark", [Nonspacing_Mark, Spacing_Mark, Enclosing_Mark]);
      Map.Insert ("m", [Nonspacing_Mark, Spacing_Mark, Enclosing_Mark]);

      Map.Insert
        ("combining_mark", [Nonspacing_Mark, Spacing_Mark, Enclosing_Mark]);

      Map.Insert ("math_symbol",     [Math_Symbol]);
      Map.Insert ("sm",              [Math_Symbol]);
      Map.Insert ("modifier_letter", [Modifier_Letter]);
      Map.Insert ("lm",              [Modifier_Letter]);
      Map.Insert ("modifier_symbol", [Modifier_Symbol]);
      Map.Insert ("sk",              [Modifier_Symbol]);
      Map.Insert ("nonspacing_mark", [Nonspacing_Mark]);
      Map.Insert ("mn",              [Nonspacing_Mark]);

      Map.Insert ("number", [Decimal_Number, Letter_Number, Other_Number]);
      Map.Insert ("n",      [Decimal_Number, Letter_Number, Other_Number]);
      Map.Insert ("open_punctuation", [Open_Punctuation]);
      Map.Insert ("ps",               [Open_Punctuation]);

      Map.Insert
        ("other", [Control, Format, Surrogate, Private_Use, Unassigned]);

      Map.Insert ("c", [Control, Format, Surrogate, Private_Use, Unassigned]);

      Map.Insert ("other_letter",        [Other_Letter]);
      Map.Insert ("lo",                  [Other_Letter]);
      Map.Insert ("other_number",        [Other_Number]);
      Map.Insert ("no",                  [Other_Number]);
      Map.Insert ("other_punctuation",   [Other_Punctuation]);
      Map.Insert ("po",                  [Other_Punctuation]);
      Map.Insert ("other_symbol",        [Other_Symbol]);
      Map.Insert ("so",                  [Other_Symbol]);
      Map.Insert ("paragraph_separator", [Paragraph_Separator]);
      Map.Insert ("zp",                  [Paragraph_Separator]);
      Map.Insert ("private_use",         [Private_Use]);
      Map.Insert ("co",                  [Private_Use]);

      Map.Insert
        ("punctuation",
         [Connector_Punctuation, Dash_Punctuation, Open_Punctuation,
          Close_Punctuation, Initial_Punctuation, Final_Punctuation,
          Other_Punctuation]);

      Map.Insert
        ("p",
         [Connector_Punctuation, Dash_Punctuation, Open_Punctuation,
          Close_Punctuation, Initial_Punctuation, Final_Punctuation,
          Other_Punctuation]);

      Map.Insert
        ("punct",
         [Connector_Punctuation, Dash_Punctuation, Open_Punctuation,
          Close_Punctuation, Initial_Punctuation, Final_Punctuation,
          Other_Punctuation]);

      Map.Insert
        ("separator", [Space_Separator, Line_Separator, Paragraph_Separator]);

      Map.Insert ("z", [Space_Separator, Line_Separator, Paragraph_Separator]);

      Map.Insert ("space_separator", [Space_Separator]);
      Map.Insert ("zs",              [Space_Separator]);
      Map.Insert ("spacing_mark",    [Spacing_Mark]);
      Map.Insert ("mc",              [Spacing_Mark]);
      Map.Insert ("surrogate",       [Surrogate]);
      Map.Insert ("cs",              [Surrogate]);

      Map.Insert
        ("symbol",
         [Math_Symbol, Currency_Symbol, Modifier_Symbol, Other_Symbol]);

      Map.Insert
        ("s", [Math_Symbol, Currency_Symbol, Modifier_Symbol, Other_Symbol]);

      Map.Insert ("titlecase_letter", [Titlecase_Letter]);
      Map.Insert ("lt",               [Titlecase_Letter]);
      Map.Insert ("unassigned",       [Unassigned]);
      Map.Insert ("cn",               [Unassigned]);
      Map.Insert ("uppercase_letter", [Uppercase_Letter]);
      Map.Insert ("lu",               [Uppercase_Letter]);
   end Initialize;

   -----------------------------
   -- To_General_Category_Set --
   -----------------------------

   procedure To_General_Category_Set
     (Name  : VSS.Strings.Virtual_String;
      Value : out General_Category_Set;
      Ok    : out Boolean)
   is
      Cursor : Category_Maps.Cursor;
   begin
      if Map.Is_Empty then
         Initialize;
      end if;

      Cursor := Map.Find (Name.To_Simple_Lowercase);

      if Category_Maps.Has_Element (Cursor) then
         Value := Category_Maps.Element (Cursor);
         Ok := True;
      else
         Ok := False;
      end if;
   end To_General_Category_Set;

end VSS.Regular_Expressions.Name_Sets;
