--
--  Copyright (C) 2021-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Implementation.UTF8_String_Handlers;

package body VSS.Implementation.Null_String_Handlers is

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

   --------------------------
   -- After_Last_Character --
   --------------------------

   overriding procedure After_Last_Character
     (Self     : Null_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor) is
   begin
      Position := After_Last_Character_Cursor;
   end After_Last_Character;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out Null_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      Handler : VSS.Implementation.Strings.Variable_Text_Handler_Access;

   begin
      VSS.Implementation.UTF8_String_Handlers.Unsafe_Initialize (Self, 1, 0);
      Handler := VSS.Implementation.Strings.Variable_Handler (Data);
      Handler.Append (Data, Code, Offset);
   end Append;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out Null_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      Handler : VSS.Implementation.Strings.Variable_Text_Handler_Access;

   begin
      --  Append to a null string, just copy data.

      Data := Suffix;
      Handler := VSS.Implementation.Strings.Variable_Handler (Data);
      Handler.Reference (Data);
   end Append;

   --------------
   -- Backward --
   --------------

   overriding function Backward
     (Self     : Null_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
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
     (Self     : Null_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor)
   is
   begin
      Position := Before_First_Character_Cursor;
   end Before_First_Character;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete
     (Self : in out Null_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset) is null;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self     : Null_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base is
        (VSS.Implementation.Strings.No_Character);

   ---------------
   -- Ends_With --
   ---------------

   overriding function Ends_With
     (Self           : Null_String_Handler;
      Data           : VSS.Implementation.Strings.String_Data;
      Suffix_Handler :
        VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Suffix_Data    : VSS.Implementation.Strings.String_Data)
      return Boolean is
   begin
      return Suffix_Handler.Is_Empty (Suffix_Data);
   end Ends_With;

   -------------
   -- Forward --
   -------------

   overriding function Forward
     (Self     : Null_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean is
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
     (Self    : in out Null_String_Handler;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Data    : out VSS.Implementation.Strings.String_Data;
      Success : out Boolean)
   is
      pragma Unreferenced (Data);

   begin
      --  XXX Should this subprogram do string conversion usign both ip-place
      --  and default string handlers?

      Success := False;
   end From_UTF_8_String;

   ---------------------------
   -- From_Wide_Wide_String --
   ---------------------------

   overriding procedure From_Wide_Wide_String
     (Self    : in out Null_String_Handler;
      Item    : Wide_Wide_String;
      Data    : out VSS.Implementation.Strings.String_Data;
      Success : out Boolean)
   is
      pragma Unreferenced (Data);

   begin
      --  XXX Should this subprogram do string conversion usign both ip-place
      --  and default string handlers?

      Success := False;
   end From_Wide_Wide_String;

   -------------------
   -- Has_Character --
   -------------------

   overriding function Has_Character
     (Self     : Null_String_Handler;
      Data     : VSS.Implementation.Strings.String_Data;
      Position : VSS.Implementation.Strings.Cursor) return Boolean is (False);

   ----------
   -- Hash --
   ----------

   overriding procedure Hash
     (Self      : Null_String_Handler;
      Data      : VSS.Implementation.Strings.String_Data;
      Generator : in out VSS.Implementation.FNV_Hash.FNV_1a_Generator) is null;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Self   : in out Null_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      Handler : VSS.Implementation.Strings.Variable_Text_Handler_Access;

   begin
      VSS.Implementation.UTF8_String_Handlers.Unsafe_Initialize (Self, 1, 0);
      Handler := VSS.Implementation.Strings.Variable_Handler (Data);
      Handler.Insert (Data, From, Item, Offset);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty
     (Self : Null_String_Handler;
      Data : VSS.Implementation.Strings.String_Data) return Boolean is (True);

   -------------
   -- Is_Null --
   -------------

   overriding function Is_Null
     (Self : Null_String_Handler;
      Data : VSS.Implementation.Strings.String_Data) return Boolean is (True);

   ------------
   -- Length --
   ------------

   overriding function Length
     (Self : Null_String_Handler;
      Data : VSS.Implementation.Strings.String_Data)
      return VSS.Implementation.Strings.Character_Count is (0);

   ---------------
   -- Reference --
   ---------------

   overriding procedure Reference
     (Self : in out Null_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data) is null;

   -----------------
   -- Split_Lines --
   -----------------

   overriding procedure Split_Lines
     (Self            : Null_String_Handler;
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
     (Self           : Null_String_Handler;
      Data           : VSS.Implementation.Strings.String_Data;
      Prefix_Handler :
        VSS.Implementation.Text_Handlers.Abstract_String_Handler'Class;
      Prefix_Data    : VSS.Implementation.Strings.String_Data)
      return Boolean is
   begin
      return Prefix_Handler.Is_Empty (Prefix_Data);
   end Starts_With;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   overriding function To_UTF_8_String
     (Self : Null_String_Handler;
      Data : VSS.Implementation.Strings.String_Data)
      return Ada.Strings.UTF_Encoding.UTF_8_String is ("");

   -----------------
   -- Unreference --
   -----------------

   overriding procedure Unreference
     (Self : in out Null_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data) is null;

end VSS.Implementation.Null_String_Handlers;
