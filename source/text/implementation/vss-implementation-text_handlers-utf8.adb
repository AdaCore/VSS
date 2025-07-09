--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

--  with Ada.Tags;
--  with Interfaces;
--
--  with VSS.Implementation.GCC;
--  with VSS.Implementation.Line_Iterators;
--  with VSS.Implementation.Text_Handlers.UTF8.Variable.Dynamic;
--  with VSS.Implementation.Text_Handlers.UTF8.Variable.Static;
with VSS.Implementation.UTF8_Strings.Mutable_Operations;
with VSS.Strings;

package body VSS.Implementation.Text_Handlers.UTF8 is

   --  use type VSS.Unicode.UTF16_Code_Unit_Offset;
   --  use type VSS.Unicode.UTF8_Code_Unit_Offset;
   --  use type VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
   --
   --  --------------
   --  -- Is_Equal --
   --  --------------
   --
   --  overriding function Is_Equal
   --    (Self  : Abstract_UTF8_Text;
   --     Other : Abstract_Text_Handler'Class) return Boolean is
   --  begin
   --     if Other.Is_UTF8 then
   --        declare
   --           Self_Pointer  : constant
   --             VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access :=
   --               Abstract_UTF8_Text'Class (Self).UTF8_Constant_Storage_Poiner;
   --           Other_Text    : Abstract_UTF8_Text'Class
   --             renames Abstract_UTF8_Text'Class (Other);
   --           Other_Pointer : constant
   --             VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access :=
   --               Other_Text.UTF8_Constant_Storage_Poiner;
   --
   --        begin
   --           declare
   --              Self_Storage  : constant
   --                VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
   --                  (0 .. Self.Size - 1)
   --                with Import,
   --                     Convention => Ada,
   --                     Address => Self_Pointer.all'Address;
   --              Other_Storage : constant
   --                VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
   --                  (0 .. Other_Text.Size - 1)
   --                with Import,
   --                     Convention => Ada,
   --                     Address => Other_Pointer.all'Address;
   --
   --           begin
   --              return Self_Storage = Other_Storage;
   --           end;
   --        end;
   --
   --     else
   --        return Abstract_Text_Handler (Self).Is_Equal (Other);
   --     end if;
   --  end Is_Equal;
   --
   --  -------------
   --  -- Is_Less --
   --  -------------
   --
   --  overriding function Is_Less
   --    (Self  : Abstract_UTF8_Text;
   --     Other : Abstract_Text_Handler'Class) return Boolean is
   --  begin
   --     if Other.Is_UTF8 then
   --        declare
   --           Self_Pointer  : constant
   --             VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access :=
   --               Abstract_UTF8_Text'Class (Self).UTF8_Constant_Storage_Poiner;
   --           Other_Text    : Abstract_UTF8_Text'Class
   --             renames Abstract_UTF8_Text'Class (Other);
   --           Other_Pointer : constant
   --             VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access :=
   --               Other_Text.UTF8_Constant_Storage_Poiner;
   --
   --        begin
   --           declare
   --              Self_Storage  : constant
   --                VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
   --                  (0 .. Self.Size - 1)
   --                with Import,
   --                     Convention => Ada,
   --                     Address => Self_Pointer.all'Address;
   --              Other_Storage : constant
   --                VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
   --                  (0 .. Other_Text.Size - 1)
   --                with Import,
   --                     Convention => Ada,
   --                     Address => Other_Pointer.all'Address;
   --
   --           begin
   --              return Self_Storage < Other_Storage;
   --           end;
   --        end;
   --
   --     else
   --        return Abstract_Text_Handler (Self).Is_Less (Other);
   --     end if;
   --  end Is_Less;
   --
   --  ----------------------
   --  -- Is_Less_Or_Equal --
   --  ----------------------
   --
   --  overriding function Is_Less_Or_Equal
   --    (Self  : Abstract_UTF8_Text;
   --     Other : Abstract_Text_Handler'Class) return Boolean is
   --  begin
   --     if Other.Is_UTF8 then
   --        declare
   --           Self_Pointer  : constant
   --             VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access :=
   --               Abstract_UTF8_Text'Class (Self).UTF8_Constant_Storage_Poiner;
   --           Other_Text    : Abstract_UTF8_Text'Class
   --             renames Abstract_UTF8_Text'Class (Other);
   --           Other_Pointer : constant
   --             VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access :=
   --               Other_Text.UTF8_Constant_Storage_Poiner;
   --
   --        begin
   --           declare
   --              Self_Storage  : constant
   --                VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
   --                  (0 .. Self.Size - 1)
   --                with Import,
   --                     Convention => Ada,
   --                     Address => Self_Pointer.all'Address;
   --              Other_Storage : constant
   --                VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
   --                  (0 .. Other_Text.Size - 1)
   --                with Import,
   --                     Convention => Ada,
   --                     Address => Other_Pointer.all'Address;
   --
   --           begin
   --              return Self_Storage <= Other_Storage;
   --           end;
   --        end;
   --
   --     else
   --        return Abstract_Text_Handler (Self).Is_Less_Or_Equal (Other);
   --     end if;
   --  end Is_Less_Or_Equal;
   --
   --  ------------
   --  -- Length --
   --  ------------
   --
   --  overriding function Length
   --    (Self : Abstract_UTF8_Text)
   --     return VSS.Implementation.Strings.Character_Count is
   --  begin
   --     return Self.Length;
   --  end Length;
   --
   --  ----------------------------------
   --  -- UTF8_Constant_Storage_Poiner --
   --  ----------------------------------
   --
   --  function UTF8_Constant_Storage_Poiner
   --    (Self : Abstract_UTF8_Text'Class)
   --     return not null
   --       VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access
   --  is
   --     use type Ada.Tags.Tag;
   --
   --     pragma Warnings (Off, """Overlay"" overlays smaller object");
   --     Overlay : constant Variable.Static.Static_UTF8_Handler
   --       with Import, Convention => Ada, Address => Self'Address;
   --     pragma Warnings (On, """Overlay"" overlays smaller object");
   --
   --  begin
   --     if Self'Tag = Variable.Static.Static_UTF8_Handler'Tag then
   --        return Overlay.Storage (Overlay.Storage'First)'Unchecked_Access;
   --
   --     else
   --        declare
   --           Storage :
   --             VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access
   --               with Import,
   --                    Convention => Ada,
   --                    Address    => Overlay.Storage'Address;
   --
   --        begin
   --           return Storage;
   --        end;
   --     end if;
   --  end UTF8_Constant_Storage_Poiner;

   --------------------------
   -- After_Last_Character --
   --------------------------

   overriding procedure After_Last_Character
     (Self     : UTF8_Text;
      Position : in out VSS.Implementation.Strings.Cursor) is
   begin
      Position :=
        (Index        => Self.Data.Length + 1,
         UTF8_Offset  => Self.Data.Size,
         UTF16_Offset => 0);
   end After_Last_Character;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out UTF8_Text;
      Code   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset) is
   begin
      VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
        (Self.Data, Code, Offset);
   end Append;

   --------------
   -- Backward --
   --------------

   overriding function Backward
     (Self     : UTF8_Text;
      Position : in out VSS.Implementation.Strings.Cursor) return Boolean is
   begin
      if Position.Index = 0 then
         return False;

      else
         VSS.Implementation.UTF8_Strings.Unchecked_Backward
           (Self.Data, Position);
      end if;

      return Position.Index > 0;
   end Backward;

   ----------------------------
   -- Before_First_Character --
   ----------------------------

   overriding procedure Before_First_Character
     (Self     : UTF8_Text;
      Position : in out VSS.Implementation.Strings.Cursor)
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;
      use type VSS.Unicode.UTF16_Code_Unit_Offset;

   begin
      Position := (Index => 0, UTF8_Offset => -1, UTF16_Offset => -1);
   end Before_First_Character;

   ------------
   -- Delete --
   ------------

   overriding procedure Delete
     (Self : in out UTF8_Text;
      From : VSS.Implementation.Strings.Cursor;
      Size : VSS.Implementation.Strings.Cursor_Offset) is
   begin
      VSS.Implementation.UTF8_Strings.Mutable_Operations.Delete
        (Self.Data, From, Size);
   end Delete;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self     : UTF8_Text;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base is
   begin
      return VSS.Implementation.UTF8_Strings.Element (Self.Data, Position);
   end Element;

   -------------
   -- Forward --
   -------------

   overriding function Forward
     (Self     : UTF8_Text;
      Position : aliased in out VSS.Implementation.Strings.Cursor)
      return Boolean is
   begin
      if Position.Index > Self.Data.Length then
         return False;

      else
         VSS.Implementation.UTF8_Strings.Unchecked_Forward
           (Self.Data, Position);
      end if;

      return Position.Index <= Self.Length;
   end Forward;

   ---------------------------
   -- From_Wide_Wide_String --
   ---------------------------

   overriding procedure From_Wide_Wide_String
     (Self    : in out UTF8_Text;
      Item    : Wide_Wide_String;
      Success : out Boolean) is
   begin
      VSS.Implementation.UTF8_Strings.Mutable_Operations.From_Wide_Wide_String
        (Self.Data, Item, Success);
   end From_Wide_Wide_String;

   -------------------
   -- Has_Character --
   -------------------

   overriding function Has_Character
     (Self     : UTF8_Text;
      Position : VSS.Implementation.Strings.Cursor) return Boolean is
   begin
      return Position.Index in 1 .. Self.Data.Length;
   end Has_Character;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Self   : in out UTF8_Text;
      From   : VSS.Implementation.Strings.Cursor;
      Item   : VSS.Unicode.Code_Point;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset) is
   begin
      VSS.Implementation.UTF8_Strings.Mutable_Operations.Insert
        (Self.Data, From, Item, Offset);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty (Self : UTF8_Text) return Boolean is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

   begin
      return Self.Data.Size = 0;
   end Is_Empty;

   ------------
   -- Length --
   ------------

   overriding function Length
     (Self : UTF8_Text) return VSS.Implementation.Strings.Character_Count is
   begin
      return Self.Data.Length;
   end Length;

   ---------------
   -- Reference --
   ---------------

   overriding procedure Reference (Self : in out UTF8_Text) is
   begin
      VSS.Implementation.UTF8_Strings.Reference (Self.Data);
   end Reference;

   -----------------
   -- Split_Lines --
   -----------------

   overriding procedure Split_Lines
     (Self            : UTF8_Text;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access) is
   begin
      VSS.Implementation.UTF8_Strings.Split_Lines
        (Self.Data,
         Data,
         Terminators,
         Keep_Terminator,
         Lines);
   end Split_Lines;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   function To_UTF_8_String
     (Self : UTF8_Text) return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      return VSS.Implementation.UTF8_Strings.To_UTF_8_String (Self.Data);
   end To_UTF_8_String;

   -----------------
   -- Unreference --
   -----------------

   overriding procedure Unreference (Self : in out UTF8_Text) is
   begin
      VSS.Implementation.UTF8_Strings.Unreference (Self.Data);
   end Unreference;

   -----------------------
   -- Unsafe_Initialize --
   -----------------------

   procedure Unsafe_Initialize
     (Text : in out
        VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class) is
   begin
      declare
         pragma Warnings (Off, """Overlay"" overlays smaller object");
         Overlay : UTF8_Text := (others => <>)
           with Address => Text'Address;
         pragma Warnings (On, """Overlay"" overlays smaller object");

      begin
         null;
      end;
   end Unsafe_Initialize;

   -----------------------
   -- Unsafe_Initialize --
   -----------------------

   procedure Unsafe_Initialize
     (Text : in out
        VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class;
      Size : VSS.Unicode.UTF8_Code_Unit_Count) is
   begin
      declare
         pragma Warnings (Off, """Overlay"" overlays smaller object");
         Overlay : UTF8_Text := (others => <>)
           with Address => Text'Address;
         pragma Warnings (On, """Overlay"" overlays smaller object");

      begin
         VSS.Implementation.UTF8_Strings.Mutable_Operations.Initialize
           (Overlay.Data, Size);
      end;
   end Unsafe_Initialize;

end VSS.Implementation.Text_Handlers.UTF8;
