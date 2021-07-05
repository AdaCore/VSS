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

with VSS.Implementation.String_Configuration;

package body VSS.Implementation.Null_String_Handlers is

   use type VSS.Implementation.Strings.Cursor;
   use type VSS.Unicode.UTF16_Code_Unit_Offset;
   use type VSS.Unicode.UTF8_Code_Unit_Offset;

   Before_First_Character_Cursor : constant
     VSS.Implementation.Strings.Cursor := (0, -1, -1);
   After_Last_Character_Cursor   : constant
     VSS.Implementation.Strings.Cursor := (1, 0, 0);
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
     (Self   : Null_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset) is
   begin
      VSS.Implementation.String_Configuration.In_Place_Handler.Initialize
        (Data);
      VSS.Implementation.Strings.Handler (Data).Append (Data, Code, Offset);
   end Append;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : Null_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset) is
   begin
      --  Append to a null string, just copy data.

      Data := Suffix;
      VSS.Implementation.Strings.Handler (Data).Reference (Data);
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
     (Self : Null_String_Handler;
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
      return VSS.Unicode.Code_Point is (0);

   ---------------
   -- Ends_With --
   ---------------

   overriding function Ends_With
     (Self           : Null_String_Handler;
      Data           : VSS.Implementation.Strings.String_Data;
      Suffix_Handler :
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class;
      Suffix_Data    : VSS.Implementation.Strings.String_Data)
      return Boolean is
   begin
      return Suffix_Handler.Is_Empty (Suffix_Data);
   end Ends_With;

   -----------------
   -- Starts_With --
   -----------------

   overriding function Starts_With
     (Self           : Null_String_Handler;
      Data           : VSS.Implementation.Strings.String_Data;
      Prefix_Handler :
        VSS.Implementation.String_Handlers.Abstract_String_Handler'Class;
      Prefix_Data    : VSS.Implementation.Strings.String_Data)
      return Boolean is
   begin
      return Prefix_Handler.Is_Empty (Prefix_Data);
   end Starts_With;

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
     (Self   : Null_String_Handler;
      Data   : in out VSS.Implementation.Strings.String_Data;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset) is
   begin
      VSS.Implementation.String_Configuration.In_Place_Handler.Initialize
        (Data);
      VSS.Implementation.Strings.Handler
        (Data).Insert (Data, From, Item, Offset);
   end Insert;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self : Null_String_Handler;
      Data : out VSS.Implementation.Strings.String_Data) is null;

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
     (Self : Null_String_Handler;
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
     (Self : Null_String_Handler;
      Data : in out VSS.Implementation.Strings.String_Data) is null;

end VSS.Implementation.Null_String_Handlers;
