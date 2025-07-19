--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Ada.Tags;

with VSS.Implementation.UTF8_Encoding;
with VSS.Implementation.UTF8_Strings.Mutable_Operations;
with VSS.Strings;

package body VSS.Implementation.Text_Handlers.UTF8 is

   use type Ada.Tags.Tag;
   use type VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
   use type VSS.Unicode.UTF8_Code_Unit_Offset;
   use type VSS.Unicode.UTF16_Code_Unit_Offset;

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

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out UTF8_Text;
      Data   : in out VSS.Implementation.Strings.String_Data;
      Suffix : VSS.Implementation.Strings.String_Data;
      Offset : in out VSS.Implementation.Strings.Cursor_Offset)
   is
      Suffix_Handler : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Suffix);

   begin
      if Suffix_Handler'Tag = UTF8_Text'Tag then
         declare
            Suffix_Text : UTF8_Text renames UTF8_Text (Suffix_Handler.all);

         begin
            VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
              (Self.Data, Suffix_Text.Data, Offset);
         end;

      else
         --  Suffix is not an UTF-8 text, no other optimization is possible
         --  here, copy character by character.

         Abstract_Text_Handler (Self).Append (Data, Suffix, Offset);
      end if;
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
      Position : in out VSS.Implementation.Strings.Cursor) is
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

   ---------------------
   -- Forward_Element --
   ---------------------

   overriding function Forward_Element
     (Self     : UTF8_Text;
      Position : aliased in out VSS.Implementation.Strings.Cursor;
      Element  : out VSS.Unicode.Code_Point'Base) return Boolean
   is
      Storage : constant VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
        (0 .. Self.Data.Size - 1)
        with Import, Address => Self.Data.Storage_Address;
      Code    : VSS.Unicode.Code_Point'Base :=
        VSS.Implementation.Strings.No_Character;
      Result  : Boolean := False;

   begin
      if Position.Index <= Self.Length then
         VSS.Implementation.UTF8_Strings.Unchecked_Forward
           (Self.Data, Position);

         if Position.Index <= Self.Length then
            Code :=
              VSS.Implementation.UTF8_Encoding.Unchecked_Decode
                (Storage, Position.UTF8_Offset);
            Result := True;
         end if;
      end if;

      Element := Code;

      return Result;
   end Forward_Element;

   -----------------------
   -- From_UTF_8_String --
   -----------------------

   overriding procedure From_UTF_8_String
     (Self    : in out UTF8_Text;
      Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Success : out Boolean) is
   begin
      VSS.Implementation.UTF8_Strings.Mutable_Operations.From_UTF_8_String
        (Self.Data, Item, Success);
   end From_UTF_8_String;

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
   begin
      return VSS.Implementation.UTF8_Strings.Is_Empty (Self.Data);
   end Is_Empty;

   --------------
   -- Is_Equal --
   --------------

   overriding function Is_Equal
     (Self  : UTF8_Text;
      Other : Abstract_Text_Handler'Class) return Boolean is
   begin
      if Other'Tag = UTF8_Text'Tag then
         declare
            Other_Text : UTF8_Text renames UTF8_Text (Other);

         begin
            if Self.Data.Size /= Other_Text.Data.Size then
               return False;

            elsif Self.Data.Size = 0 then
               return True;

            else
               declare
                  Self_Storage  : constant
                    VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                      (0 .. Self.Data.Size - 1)
                    with Import, Address => Self.Data.Storage_Address;
                  Other_Storage : constant
                    VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                      (0 .. Other_Text.Data.Size - 1)
                    with Import, Address => Other_Text.Data.Storage_Address;

               begin
                  return Self_Storage = Other_Storage;
               end;
            end if;
         end;

      else
         return Abstract_Text_Handler (Self).Is_Equal (Other);
      end if;
   end Is_Equal;

   -------------
   -- Is_Less --
   -------------

   overriding function Is_Less
     (Self  : UTF8_Text;
      Other : Abstract_Text_Handler'Class) return Boolean is
   begin
      if Other'Tag = UTF8_Text'Tag then
         declare
            Other_Text : UTF8_Text renames UTF8_Text (Other);

         begin
            declare
               Self_Storage  : constant
                 VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                   (0 .. Self.Data.Size - 1)
                 with Import, Address => Self.Data.Storage_Address;
               Other_Storage : constant
                 VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                   (0 .. Other_Text.Data.Size - 1)
                 with Import, Address => Other_Text.Data.Storage_Address;

            begin
               return Self_Storage < Other_Storage;
            end;
         end;

      else
         return Abstract_Text_Handler (Self).Is_Less (Other);
      end if;
   end Is_Less;

   ----------------------
   -- Is_Less_Or_Equal --
   ----------------------

   overriding function Is_Less_Or_Equal
     (Self  : UTF8_Text;
      Other : Abstract_Text_Handler'Class) return Boolean is
   begin
      if Other'Tag = UTF8_Text'Tag then
         declare
            Other_Text : UTF8_Text renames UTF8_Text (Other);

         begin
            declare
               Self_Storage  : constant
                 VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                   (0 .. Self.Data.Size - 1)
                 with Import, Address => Self.Data.Storage_Address;
               Other_Storage : constant
                 VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                   (0 .. Other_Text.Data.Size - 1)
                 with Import, Address => Other_Text.Data.Storage_Address;

            begin
               return Self_Storage <= Other_Storage;
            end;
         end;

      else
         return Abstract_Text_Handler (Self).Is_Less_Or_Equal (Other);
      end if;
   end Is_Less_Or_Equal;

   -------------
   -- Is_Null --
   -------------

   overriding function Is_Null (Self : UTF8_Text) return Boolean is
   begin
      return VSS.Implementation.UTF8_Strings.Is_Null (Self.Data);
   end Is_Null;

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

   -----------
   -- Slice --
   -----------

   overriding procedure Slice
     (Self   : UTF8_Text;
      From   : VSS.Implementation.Strings.Cursor;
      To     : VSS.Implementation.Strings.Cursor;
      Target : out VSS.Implementation.Strings.String_Data)
   is
   begin
      Unsafe_Initialize
        (VSS.Implementation.Strings.Variable_Handler (Target).all);

      VSS.Implementation.UTF8_Strings.Slice
        (Self.Data,
         From,
         To,
         UTF8_Text
           (VSS.Implementation.Strings.Variable_Handler (Target).all).Data);
   end Slice;

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
