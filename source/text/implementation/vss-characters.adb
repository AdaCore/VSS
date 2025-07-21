--
--  Copyright (C) 2021-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Implementation.Strings;
with VSS.Implementation.UCD_Core;
with VSS.Implementation.UTF8_Casing;
with VSS.Implementation.UTF8_Strings;
with VSS.Locales;
with VSS.Strings.Internals;

package body VSS.Characters is

   use type VSS.Implementation.UCD_Core.GC_Values;

   GC_To_General_Ccategory : constant
     array (VSS.Implementation.UCD_Core.GC_Values) of General_Category :=
     [VSS.Implementation.UCD_Core.GC_Cc => Control,
      VSS.Implementation.UCD_Core.GC_Cf => Format,
      VSS.Implementation.UCD_Core.GC_Cn => Unassigned,
      VSS.Implementation.UCD_Core.GC_Co => Private_Use,
      VSS.Implementation.UCD_Core.GC_Cs => Surrogate,
      VSS.Implementation.UCD_Core.GC_Ll => Lowercase_Letter,
      VSS.Implementation.UCD_Core.GC_Lm => Modifier_Letter,
      VSS.Implementation.UCD_Core.GC_Lo => Other_Letter,
      VSS.Implementation.UCD_Core.GC_Lt => Titlecase_Letter,
      VSS.Implementation.UCD_Core.GC_Lu => Uppercase_Letter,
      VSS.Implementation.UCD_Core.GC_Mc => Spacing_Mark,
      VSS.Implementation.UCD_Core.GC_Me => Enclosing_Mark,
      VSS.Implementation.UCD_Core.GC_Mn => Nonspacing_Mark,
      VSS.Implementation.UCD_Core.GC_Nd => Decimal_Number,
      VSS.Implementation.UCD_Core.GC_Nl => Letter_Number,
      VSS.Implementation.UCD_Core.GC_No => Other_Number,
      VSS.Implementation.UCD_Core.GC_Pc => Connector_Punctuation,
      VSS.Implementation.UCD_Core.GC_Pd => Dash_Punctuation,
      VSS.Implementation.UCD_Core.GC_Pe => Close_Punctuation,
      VSS.Implementation.UCD_Core.GC_Pf => Final_Punctuation,
      VSS.Implementation.UCD_Core.GC_Pi => Initial_Punctuation,
      VSS.Implementation.UCD_Core.GC_Po => Other_Punctuation,
      VSS.Implementation.UCD_Core.GC_Ps => Open_Punctuation,
      VSS.Implementation.UCD_Core.GC_Sc => Currency_Symbol,
      VSS.Implementation.UCD_Core.GC_Sk => Modifier_Symbol,
      VSS.Implementation.UCD_Core.GC_Sm => Math_Symbol,
      VSS.Implementation.UCD_Core.GC_So => Other_Symbol,
      VSS.Implementation.UCD_Core.GC_Zl => Line_Separator,
      VSS.Implementation.UCD_Core.GC_Zp => Paragraph_Separator,
      VSS.Implementation.UCD_Core.GC_Zs => Space_Separator];

   function Extract_Core_Data
     (Code : VSS.Unicode.Code_Point)
      return VSS.Implementation.UCD_Core.Core_Data_Record;
   --  Return core data record for the given character.

   -----------------------
   -- Extract_Core_Data --
   -----------------------

   function Extract_Core_Data
     (Code : VSS.Unicode.Code_Point)
      return VSS.Implementation.UCD_Core.Core_Data_Record
   is
      use type VSS.Implementation.UCD_Core.Core_Offset;
      use type VSS.Unicode.Code_Point;

      Block : constant VSS.Implementation.UCD_Core.Core_Index :=
        VSS.Implementation.UCD_Core.Core_Index
          (Code
           / VSS.Unicode.Code_Point (VSS.Implementation.UCD_Core.Block_Size));
      Offset : constant VSS.Implementation.UCD_Core.Core_Offset :=
        VSS.Implementation.UCD_Core.Core_Offset
          (Code mod VSS.Implementation.UCD_Core.Block_Size);

   begin
      return
        VSS.Implementation.UCD_Core.Core_Data_Table
          (VSS.Implementation.UCD_Core.Core_Index_Table (Block) + Offset);
   end Extract_Core_Data;

   ---------------
   -- Get_Cased --
   ---------------

   function Get_Cased (Self : Virtual_Character) return Boolean is
      Data : constant VSS.Implementation.UCD_Core.Core_Data_Record :=
        Extract_Core_Data (Virtual_Character'Pos (Self));

   begin
      return
        Data.GC in VSS.Implementation.UCD_Core.GC_Ll
                     | VSS.Implementation.UCD_Core.GC_Lu
                     | VSS.Implementation.UCD_Core.GC_Lt
          or Data.OLower
          or Data.OUpper;
   end Get_Cased;

   --------------------------
   -- Get_General_Category --
   --------------------------

   function Get_General_Category
     (Self : Virtual_Character'Base) return General_Category is
   begin
      if Wide_Wide_Character (Self)
           not in VSS.Unicode.Code_Point_Character
      then
         return Unassigned;
      end if;

      declare
         Data : constant VSS.Implementation.UCD_Core.Core_Data_Record :=
           Extract_Core_Data (Virtual_Character'Pos (Self));

      begin
         return GC_To_General_Ccategory (Data.GC);
      end;
   end Get_General_Category;

   -------------------
   -- Get_Lowercase --
   -------------------

   function Get_Lowercase (Self : Virtual_Character) return Boolean is
      Data : constant VSS.Implementation.UCD_Core.Core_Data_Record :=
        Extract_Core_Data (Virtual_Character'Pos (Self));

   begin
      return Data.GC = VSS.Implementation.UCD_Core.GC_Ll or Data.OLower;
   end Get_Lowercase;

   ---------------------------
   -- Get_Lowercase_Mapping --
   ---------------------------

   function Get_Lowercase_Mapping
     (Self : Virtual_Character) return VSS.Strings.Virtual_String is
   begin
      return Get_Lowercase_Mapping (Self, VSS.Locales.Current_Locale);
   end Get_Lowercase_Mapping;

   ---------------------------
   -- Get_Lowercase_Mapping --
   ---------------------------

   function Get_Lowercase_Mapping
     (Self   : Virtual_Character;
      Locale : VSS.Locales.Locale) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Locale);

   begin
      return Result : VSS.Strings.Virtual_String do
         VSS.Implementation.UTF8_Casing.Get_Case_Mapping
           (Virtual_Character'Pos (Self),
            VSS.Implementation.UTF8_Casing.Lowercase,
            VSS.Strings.Internals.Data_Access_Variable (Result).all);
      end return;
   end Get_Lowercase_Mapping;

   -----------------------------
   -- Get_Simple_Case_Folding --
   -----------------------------

   function Get_Simple_Case_Folding
     (Self : Virtual_Character) return Virtual_Character
   is
      Text     : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Position : aliased VSS.Implementation.Strings.Cursor;
      Success  : Boolean with Unreferenced;

   begin
      VSS.Implementation.UTF8_Casing.Get_Case_Mapping
        (Virtual_Character'Pos (Self),
         VSS.Implementation.UTF8_Casing.Simple_Case_Folding,
         Text);

      VSS.Implementation.UTF8_Strings.Before_First_Character (Text, Position);
      Success := VSS.Implementation.UTF8_Strings.Forward (Text, Position);

      return
        Virtual_Character'Val
          (VSS.Implementation.UTF8_Strings.Element (Text, Position));
   end Get_Simple_Case_Folding;

   ----------------------------------
   -- Get_Simple_Lowercase_Mapping --
   ----------------------------------

   function Get_Simple_Lowercase_Mapping
     (Self : Virtual_Character) return Virtual_Character
   is
      Text     : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Position : aliased VSS.Implementation.Strings.Cursor;
      Success  : Boolean with Unreferenced;

   begin
      VSS.Implementation.UTF8_Casing.Get_Case_Mapping
        (Virtual_Character'Pos (Self),
         VSS.Implementation.UTF8_Casing.Simple_Lowercase,
         Text);

      VSS.Implementation.UTF8_Strings.Before_First_Character (Text, Position);
      Success := VSS.Implementation.UTF8_Strings.Forward (Text, Position);

      return
        Virtual_Character'Val
          (VSS.Implementation.UTF8_Strings.Element (Text, Position));
   end Get_Simple_Lowercase_Mapping;

   ----------------------------------
   -- Get_Simple_Titlecase_Mapping --
   ----------------------------------

   function Get_Simple_Titlecase_Mapping
     (Self : Virtual_Character) return Virtual_Character
   is
      Text     : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Position : aliased VSS.Implementation.Strings.Cursor;
      Success  : Boolean with Unreferenced;

   begin
      VSS.Implementation.UTF8_Casing.Get_Case_Mapping
        (Virtual_Character'Pos (Self),
         VSS.Implementation.UTF8_Casing.Simple_Titlecase,
         Text);

      VSS.Implementation.UTF8_Strings.Before_First_Character (Text, Position);
      Success := VSS.Implementation.UTF8_Strings.Forward (Text, Position);

      return
        Virtual_Character'Val
          (VSS.Implementation.UTF8_Strings.Element (Text, Position));
   end Get_Simple_Titlecase_Mapping;

   ----------------------------------
   -- Get_Simple_Uppercase_Mapping --
   ----------------------------------

   function Get_Simple_Uppercase_Mapping
     (Self : Virtual_Character) return Virtual_Character
   is
      Text     : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Position : aliased VSS.Implementation.Strings.Cursor;
      Success  : Boolean with Unreferenced;

   begin
      VSS.Implementation.UTF8_Casing.Get_Case_Mapping
        (Virtual_Character'Pos (Self),
         VSS.Implementation.UTF8_Casing.Simple_Uppercase,
         Text);

      VSS.Implementation.UTF8_Strings.Before_First_Character (Text, Position);
      Success := VSS.Implementation.UTF8_Strings.Forward (Text, Position);

      return
        Virtual_Character'Val
          (VSS.Implementation.UTF8_Strings.Element (Text, Position));
   end Get_Simple_Uppercase_Mapping;

   ---------------------------
   -- Get_Titlecase_Mapping --
   ---------------------------

   function Get_Titlecase_Mapping
     (Self : Virtual_Character) return VSS.Strings.Virtual_String is
   begin
      return Get_Titlecase_Mapping (Self, VSS.Locales.Current_Locale);
   end Get_Titlecase_Mapping;

   ---------------------------
   -- Get_Titlecase_Mapping --
   ---------------------------

   function Get_Titlecase_Mapping
     (Self   : Virtual_Character;
      Locale : VSS.Locales.Locale) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Locale);

   begin
      return Result : VSS.Strings.Virtual_String do
         VSS.Implementation.UTF8_Casing.Get_Case_Mapping
           (Virtual_Character'Pos (Self),
            VSS.Implementation.UTF8_Casing.Titlecase,
            VSS.Strings.Internals.Data_Access_Variable (Result).all);
      end return;
   end Get_Titlecase_Mapping;

   -------------------
   -- Get_Uppercase --
   -------------------

   function Get_Uppercase (Self : Virtual_Character) return Boolean is
      Data : constant VSS.Implementation.UCD_Core.Core_Data_Record :=
        Extract_Core_Data (Virtual_Character'Pos (Self));

   begin
      return Data.GC = VSS.Implementation.UCD_Core.GC_Lu or Data.OUpper;
   end Get_Uppercase;

   ---------------------------
   -- Get_Uppercase_Mapping --
   ---------------------------

   function Get_Uppercase_Mapping
     (Self : Virtual_Character) return VSS.Strings.Virtual_String is
   begin
      return Get_Uppercase_Mapping (Self, VSS.Locales.Current_Locale);
   end Get_Uppercase_Mapping;

   ---------------------------
   -- Get_Uppercase_Mapping --
   ---------------------------

   function Get_Uppercase_Mapping
     (Self   : Virtual_Character;
      Locale : VSS.Locales.Locale) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Locale);

   begin
      return Result : VSS.Strings.Virtual_String do
         VSS.Implementation.UTF8_Casing.Get_Case_Mapping
           (Virtual_Character'Pos (Self),
            VSS.Implementation.UTF8_Casing.Uppercase,
            VSS.Strings.Internals.Data_Access_Variable (Result).all);
      end return;
   end Get_Uppercase_Mapping;

   ----------------
   -- Is_Control --
   ----------------

   function Is_Control (Self : Virtual_Character) return Boolean is
   begin
      return Get_General_Category (Self) = Control;
   end Is_Control;

   ---------------
   -- Is_Format --
   ---------------

   function Is_Format (Self : Virtual_Character) return Boolean is
   begin
      return
        Get_General_Category (Self)
          in Format | Line_Separator | Paragraph_Separator;
   end Is_Format;

   ----------------
   -- Is_Graphic --
   ----------------

   function Is_Graphic (Self : Virtual_Character) return Boolean is
   begin
      return
        Get_General_Category (Self)
          in Letter | Mark | Number | Punctuation | Symbol | Space_Separator;
   end Is_Graphic;

   --------------------------------
   -- Is_Valid_Virtual_Character --
   --------------------------------

   function Is_Valid_Virtual_Character
     (Self : Virtual_Character'Base) return Boolean is
   begin
      return Wide_Wide_Character (Self) in VSS.Unicode.Scalar_Value_Character;
   end Is_Valid_Virtual_Character;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Self : Virtual_Character) return VSS.Strings.Virtual_String is
   begin
      return Result : VSS.Strings.Virtual_String do
         Result.Append (Self);
      end return;
   end To_Virtual_String;

end VSS.Characters;
