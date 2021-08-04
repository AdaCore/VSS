------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------
--  Data type to represent individual abstract character

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

end VSS.Characters;
