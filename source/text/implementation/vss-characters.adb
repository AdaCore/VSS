--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Implementation.String_Handlers;
with VSS.Implementation.Strings;
with VSS.Implementation.UCD_Core;
with VSS.Locales;
with VSS.Strings.Internals;
with VSS.Unicode;

package body VSS.Characters is

   use type VSS.Implementation.UCD_Core.GC_Values;

   GC_To_General_Ccategory : constant
     array (VSS.Implementation.UCD_Core.GC_Values) of General_Category :=
     (VSS.Implementation.UCD_Core.GC_Cc => Control,
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
      VSS.Implementation.UCD_Core.GC_Zs => Space_Separator);

   function Extract_Core_Data
     (Character : Virtual_Character)
      return VSS.Implementation.UCD_Core.Core_Data_Record;
   --  Return core data record for the given character.

   -----------------------
   -- Extract_Core_Data --
   -----------------------

   function Extract_Core_Data
     (Character : Virtual_Character)
      return VSS.Implementation.UCD_Core.Core_Data_Record
   is
      use type VSS.Implementation.UCD_Core.Core_Offset;
      use type VSS.Unicode.Code_Point;

      Block : constant VSS.Implementation.UCD_Core.Core_Index :=
        VSS.Implementation.UCD_Core.Core_Index
          (VSS.Unicode.Code_Point (Virtual_Character'Pos (Character))
           / VSS.Unicode.Code_Point (VSS.Implementation.UCD_Core.Block_Size));
      Offset : constant VSS.Implementation.UCD_Core.Core_Offset :=
        Virtual_Character'Pos (Character)
          mod VSS.Implementation.UCD_Core.Block_Size;

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
        Extract_Core_Data (Self);

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
     (Self : Virtual_Character) return General_Category
   is
      Data : constant VSS.Implementation.UCD_Core.Core_Data_Record :=
        Extract_Core_Data (Self);

   begin
      return GC_To_General_Ccategory (Data.GC);
   end Get_General_Category;

   -------------------
   -- Get_Lowercase --
   -------------------

   function Get_Lowercase (Self : Virtual_Character) return Boolean is
      Data : constant VSS.Implementation.UCD_Core.Core_Data_Record :=
        Extract_Core_Data (Self);

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

      Data : VSS.Implementation.Strings.String_Data;

   begin
      VSS.Implementation.Strings.Handler
        (Data).Get_Case_Mapping
        (Virtual_Character'Pos (Self),
         VSS.Implementation.String_Handlers.Lowercase,
         Data);

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Internals.To_Virtual_String (Data)
      do
         VSS.Implementation.Strings.Unreference (Data);
      end return;
   end Get_Lowercase_Mapping;

   ----------------------------------
   -- Get_Simple_Lowercase_Mapping --
   ----------------------------------

   function Get_Simple_Lowercase_Mapping
     (Self : Virtual_Character) return Virtual_Character
   is
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor;
      Success  : Boolean with Unreferenced;

   begin
      VSS.Implementation.Strings.Handler
        (Data).Get_Case_Mapping
        (Virtual_Character'Pos (Self),
         VSS.Implementation.String_Handlers.Simple_Lowercase,
         Data);

      VSS.Implementation.Strings.Handler
        (Data).Before_First_Character (Data, Position);
      Success := VSS.Implementation.Strings.Handler
        (Data).Forward (Data, Position);

      return
        Virtual_Character'Val
          (VSS.Implementation.Strings.Handler
             (Data).Element (Data, Position));
   end Get_Simple_Lowercase_Mapping;

   ----------------------------------
   -- Get_Simple_Titlecase_Mapping --
   ----------------------------------

   function Get_Simple_Titlecase_Mapping
     (Self : Virtual_Character) return Virtual_Character
   is
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor;
      Success  : Boolean with Unreferenced;

   begin
      VSS.Implementation.Strings.Handler
        (Data).Get_Case_Mapping
        (Virtual_Character'Pos (Self),
         VSS.Implementation.String_Handlers.Simple_Titlecase,
         Data);

      VSS.Implementation.Strings.Handler
        (Data).Before_First_Character (Data, Position);
      Success := VSS.Implementation.Strings.Handler
        (Data).Forward (Data, Position);

      return
        Virtual_Character'Val
          (VSS.Implementation.Strings.Handler
             (Data).Element (Data, Position));
   end Get_Simple_Titlecase_Mapping;

   ----------------------------------
   -- Get_Simple_Uppercase_Mapping --
   ----------------------------------

   function Get_Simple_Uppercase_Mapping
     (Self : Virtual_Character) return Virtual_Character
   is
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor;
      Success  : Boolean with Unreferenced;

   begin
      VSS.Implementation.Strings.Handler
        (Data).Get_Case_Mapping
        (Virtual_Character'Pos (Self),
         VSS.Implementation.String_Handlers.Simple_Uppercase,
         Data);

      VSS.Implementation.Strings.Handler
        (Data).Before_First_Character (Data, Position);
      Success := VSS.Implementation.Strings.Handler
        (Data).Forward (Data, Position);

      return
        Virtual_Character'Val
          (VSS.Implementation.Strings.Handler
             (Data).Element (Data, Position));
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

      Data : VSS.Implementation.Strings.String_Data;

   begin
      VSS.Implementation.Strings.Handler
        (Data).Get_Case_Mapping
        (Virtual_Character'Pos (Self),
         VSS.Implementation.String_Handlers.Titlecase,
         Data);

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Internals.To_Virtual_String (Data)
      do
         VSS.Implementation.Strings.Unreference (Data);
      end return;
   end Get_Titlecase_Mapping;

   -------------------
   -- Get_Uppercase --
   -------------------

   function Get_Uppercase (Self : Virtual_Character) return Boolean is
      Data : constant VSS.Implementation.UCD_Core.Core_Data_Record :=
        Extract_Core_Data (Self);

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

      Data : VSS.Implementation.Strings.String_Data;

   begin
      VSS.Implementation.Strings.Handler
        (Data).Get_Case_Mapping
        (Virtual_Character'Pos (Self),
         VSS.Implementation.String_Handlers.Uppercase,
         Data);

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Internals.To_Virtual_String (Data)
      do
         VSS.Implementation.Strings.Unreference (Data);
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

end VSS.Characters;
