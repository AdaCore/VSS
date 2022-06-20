--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Data type to represent individual abstract character

limited with VSS.Locales;
limited with VSS.Strings;

package VSS.Characters is

   pragma Preelaborate;
   pragma Remote_Types;

   type Virtual_Character is new Wide_Wide_Character
     range Wide_Wide_Character'Val (16#00_0000#)
       .. Wide_Wide_Character'Val (16#10_FFFF#);

   type General_Category is
     (Uppercase_Letter,
      Lowercase_Letter,
      Titlecase_Letter,
      Modifier_Letter,
      Other_Letter,

      Nonspacing_Mark,
      Spacing_Mark,
      Enclosing_Mark,

      Decimal_Number,
      Letter_Number,
      Other_Number,

      Connector_Punctuation,
      Dash_Punctuation,
      Open_Punctuation,
      Close_Punctuation,
      Initial_Punctuation,
      Final_Punctuation,
      Other_Punctuation,

      Math_Symbol,
      Currency_Symbol,
      Modifier_Symbol,
      Other_Symbol,

      Space_Separator,
      Line_Separator,
      Paragraph_Separator,

      Control,
      Format,
      Surrogate,
      Private_Use,
      Unassigned);

   subtype Cased_Letter is
     General_Category range Uppercase_Letter .. Titlecase_Letter;

   subtype Letter is
     General_Category range Uppercase_Letter .. Other_Letter;

   subtype Mark is
     General_Category range Nonspacing_Mark .. Enclosing_Mark;

   subtype Number is
     General_Category range Decimal_Number .. Other_Number;

   subtype Punctuation is
     General_Category range Connector_Punctuation .. Other_Punctuation;

   subtype Symbol is
     General_Category range Math_Symbol .. Other_Symbol;

   subtype Separator is
     General_Category range Space_Separator .. Paragraph_Separator;

   subtype Other is
     General_Category range Control .. Unassigned;

   function Get_General_Category
     (Self : Virtual_Character) return General_Category;
   --  Return General_Category property for given character.

   function Get_Lowercase (Self : Virtual_Character) return Boolean;
   --  Return value of the Lowercase property of the character.
   --
   --  Note, value of Lowercase property of the given character may be
   --  different from value of Is_Lowercase function of string composed from
   --  the same character.

   function Get_Uppercase (Self : Virtual_Character) return Boolean;
   --  Return value of the Uppercase property of the character.
   --
   --  Note, value of Uppercase property of the given character may be
   --  different from value of Is_Lowercase function of string composed from
   --  the same character.

   function Get_Cased (Self : Virtual_Character) return Boolean;
   --  Return value of the Cased property of the character.

   function Get_Lowercase_Mapping
     (Self   : Virtual_Character) return VSS.Strings.Virtual_String;
   --  Return value of the Lowercase_Mapping property of the character with
   --  tailoring by the locale of the current task.

   function Get_Lowercase_Mapping
     (Self   : Virtual_Character;
      Locale : VSS.Locales.Locale) return VSS.Strings.Virtual_String;
   --  Return value of the Lowercase_Mapping property of the character with
   --  tailoring by the given locale.

   function Get_Titlecase_Mapping
     (Self : Virtual_Character) return VSS.Strings.Virtual_String;
   --  Return value of the Titlecase_Mapping property of the character with
   --  tailoring by the locale of the current task.

   function Get_Titlecase_Mapping
     (Self   : Virtual_Character;
      Locale : VSS.Locales.Locale) return VSS.Strings.Virtual_String;
   --  Return value of the Titlecase_Mapping property of the character with
   --  tailoring by the given locale.

   function Get_Uppercase_Mapping
     (Self : Virtual_Character) return VSS.Strings.Virtual_String;
   --  Return value of the Uppercase_Mapping property of the character with
   --  tailoring by the locale of the current task.

   function Get_Uppercase_Mapping
     (Self   : Virtual_Character;
      Locale : VSS.Locales.Locale) return VSS.Strings.Virtual_String;
   --  Return value of the Uppercase_Mapping property of the character with
   --  tailoring by the given locale.

   function Get_Simple_Lowercase_Mapping
     (Self : Virtual_Character) return Virtual_Character;
   --  Return value of the Simple_Lowercase_Mapping property of the character.

   function Get_Simple_Titlecase_Mapping
     (Self : Virtual_Character) return Virtual_Character;
   --  Return value of the Simple_Titlecase_Mapping property of the character.

   function Get_Simple_Uppercase_Mapping
     (Self : Virtual_Character) return Virtual_Character;
   --  Return value of the Simple_Uppercase_Mapping property of the character.

   function Is_Graphic (Self : Virtual_Character) return Boolean;
   --  Return True when character belong to Graphic type, which includes
   --  characters with general category Letter, Mark, Number, Punctuation,
   --  Symbol, and Space_Separator.

   function Is_Format (Self : Virtual_Character) return Boolean;
   --  Return True when character belong to Format type, which includes
   --  characters with general category Format, Line_Separator, and
   --  Paragraph_Separator.

   function Is_Control (Self : Virtual_Character) return Boolean;
   --  Return True when character belong to Format type, which includes
   --  characters with general category Control.

end VSS.Characters;
