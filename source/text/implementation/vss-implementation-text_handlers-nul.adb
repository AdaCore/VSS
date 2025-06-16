--
--  Copyright (C) 2021-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This version of the package "convert" text handler object into the UTF-8
--  encoded text on modification operations and redispatch operation.

with VSS.Implementation.Text_Handlers.UTF8.Variable;
with VSS.Strings;

package body VSS.Implementation.Text_Handlers.Nul is

   use type VSS.Implementation.Strings.Cursor;
   use type VSS.Unicode.UTF16_Code_Unit_Offset;
   use type VSS.Unicode.UTF8_Code_Unit_Offset;

   Before_First_Character_Cursor : constant
     VSS.Implementation.Strings.Cursor :=
       (Index => 0, UTF8_Offset => -1, UTF16_Offset => -1);
   After_Last_Character_Cursor   : constant
     VSS.Implementation.Strings.Cursor :=
       (Index => 1, UTF8_Offset => 0, UTF16_Offset => 0);
   --  These are only two possible positions of the cursor for null string.

   Code_Point_Max_Encoded_Length : constant := 4;
   --  Maximum number of code units to encode single code point.

   --------------------------
   -- After_Last_Character --
   --------------------------

   overriding procedure After_Last_Character
     (Self     : Null_Handler;
      Position : in out VSS.Implementation.Strings.Cursor) is
   begin
      Position := After_Last_Character_Cursor;
   end After_Last_Character;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out Null_Handler;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset) is
   begin
      VSS.Implementation.Text_Handlers.UTF8.Variable.Unsafe_Initialize
        (Self, Code_Point_Max_Encoded_Length);
      VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class
        (Self).Append (Code, Offset);
   end Append;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out Null_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset) is
   begin
      --  Append to a null string, just copy data.

      Data := Suffix;
      VSS.Implementation.Strings.Variable_Handler (Data).Reference;
   end Append;

   --------------
   -- Backward --
   --------------

   overriding function Backward
     (Self     : Null_Handler;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean is
   begin
      if Position = After_Last_Character_Cursor then
         Position := Before_First_Character_Cursor;
      end if;

      return False;
   end Backward;

   ----------------------------
   -- Before_First_Character --
   ----------------------------

   overriding procedure Before_First_Character
     (Self     : Null_Handler;
      Position : in out VSS.Implementation.Strings.Cursor)
   is
   begin
      Position := Before_First_Character_Cursor;
   end Before_First_Character;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete
     (Self : in out Null_Handler;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset) is null;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self     : Null_Handler;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base is
        (VSS.Implementation.Strings.No_Character);

   ---------------
   -- Ends_With --
   ---------------

   overriding function Ends_With
     (Self   : Null_Handler;
      Suffix : VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class)
      return Boolean is
   begin
      return Suffix.Is_Empty;
   end Ends_With;

   -------------
   -- Forward --
   -------------

   overriding function Forward
     (Self     : Null_Handler;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean is
   begin
      if Position = Before_First_Character_Cursor then
         Position := After_Last_Character_Cursor;
      end if;

      return False;
   end Forward;

   -----------------------
   -- From_UTF_8_String --
   -----------------------

   overriding procedure From_UTF_8_String
     (Self    : in out Null_Handler;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Success : out Boolean) is
   begin
      VSS.Implementation.Text_Handlers.UTF8.Variable.Unsafe_Initialize
        (Self, Item'Length);

      VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class
        (Self).From_UTF_8_String (Item, Success);
   end From_UTF_8_String;

   ---------------------------
   -- From_Wide_Wide_String --
   ---------------------------

   overriding procedure From_Wide_Wide_String
     (Self    : in out Null_Handler;
      Item    : Wide_Wide_String;
      Success : out Boolean) is
   begin
      VSS.Implementation.Text_Handlers.UTF8.Variable.Unsafe_Initialize
        (Self, Item'Length);
      --  Request text data storage size enough to store ASCII text. Storage
      --  will reallocated when necessary. It helps to use static storage when
      --  possible.

      VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class
        (Self).From_Wide_Wide_String (Item, Success);
   end From_Wide_Wide_String;

   -------------------
   -- Has_Character --
   -------------------

   overriding function Has_Character
     (Self     : Null_Handler;
      Position : VSS.Implementation.Strings.Cursor) return Boolean is (False);

   ----------
   -- Hash --
   ----------

   overriding procedure Hash
     (Self      : Null_Handler;
      Generator : in out VSS.Implementation.FNV_Hash.FNV_1a_Generator) is null;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Self   : in out Null_Handler;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      Text : VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class
        renames VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class
                  (Self);

   begin
      VSS.Implementation.Text_Handlers.UTF8.Variable.Unsafe_Initialize
        (Self, Code_Point_Max_Encoded_Length);
      Text.Insert (From, Item, Offset);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty (Self : Null_Handler) return Boolean is (True);

   -------------
   -- Is_Null --
   -------------

   overriding function Is_Null (Self : Null_Handler) return Boolean is (True);

   ------------
   -- Length --
   ------------

   overriding function Length
     (Self : Null_Handler) return VSS.Implementation.Strings.Character_Count is
        (0);

   ---------------
   -- Reference --
   ---------------

   overriding procedure Reference (Self : in out Null_Handler) is null;

   -----------------
   -- Split_Lines --
   -----------------

   overriding procedure Split_Lines
     (Self            : Null_Handler;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access) is
   begin
      Lines := null;
   end Split_Lines;

   -----------------
   -- Starts_With --
   -----------------

   overriding function Starts_With
     (Self   : Null_Handler;
      Prefix : VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class)
      return Boolean is
   begin
      return Prefix.Is_Empty;
   end Starts_With;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   overriding function To_UTF_8_String
     (Self : Null_Handler) return Ada.Strings.UTF_Encoding.UTF_8_String is
        ("");

   -----------------
   -- Unreference --
   -----------------

   overriding procedure Unreference (Self : in out Null_Handler) is null;

end VSS.Implementation.Text_Handlers.Nul;
