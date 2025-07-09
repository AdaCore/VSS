--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Implementation.GCC;
with VSS.Implementation.Line_Iterators;
with VSS.Implementation.Storage_Managers;
with VSS.Implementation.Text_Handlers.UTF8;
with VSS.Implementation.UTF8_Encoding;
with VSS.Implementation.UTF8_Strings.Mutable_Operations;

package body VSS.Implementation.UTF8_Strings is

   use type VSS.Implementation.Strings.Character_Offset;
   use type VSS.Unicode.UTF8_Code_Unit_Offset;
   use type VSS.Unicode.UTF16_Code_Unit_Offset;

   procedure Unchecked_Backward_Decode
     (Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Offset  : in out VSS.Unicode.UTF8_Code_Unit_Index;
      Code    : out VSS.Unicode.Code_Point);
   --  Change offset to the point of the previous character and decode
   --  character at this position.

   -------------
   -- Element --
   -------------

   function Element
     (Self     : UTF8_String_Data;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Unicode.Code_Point'Base
   is
      Storage : constant VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
        (0 .. Self.Size)
        with Import, Address => Self.Storage_Address;

   begin
      if Position.Index < 1
        or else Position.Index > Self.Length
      then
         return VSS.Implementation.Strings.No_Character;
      end if;

      return
        VSS.Implementation.UTF8_Encoding.Unchecked_Decode
          (Storage, Position.UTF8_Offset);
   end Element;

   ------------
   -- Is_SSO --
   ------------

   function Is_SSO (Self : UTF8_String_Data) return Boolean is
      use type Interfaces.Unsigned_32;

   begin
      return Self.Flags = 0;
   end Is_SSO;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Self : in out UTF8_String_Data) is
   begin
      if Is_SSO (Self) then
         Self.Storage_Address := Self.Manager'Address;

      else
         declare
            Manager :
              VSS.Implementation.Storage_Managers.Abstract_Storage_Manager
              with Import, Address => Self.Manager'Address;

         begin
            VSS.Implementation.Storage_Managers.Abstract_Storage_Manager'Class
              (Manager).Reference;
         end;
      end if;
   end Reference;

   -----------------
   -- Split_Lines --
   -----------------

   procedure Split_Lines
     (Text            : UTF8_String_Data;
      Data            : VSS.Implementation.Strings.String_Data;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access)
   is
      Initial    : VSS.Implementation.Strings.Cursor :=
        (Index => 0, UTF8_Offset => -1, UTF16_Offset => -1);
      At_First   : aliased VSS.Implementation.Strings.Cursor;
      At_Last    : aliased VSS.Implementation.Strings.Cursor;
      After_Last : aliased VSS.Implementation.Strings.Cursor;
      Terminator : VSS.Implementation.Strings.Cursor;

   begin
      VSS.Implementation.String_Vectors.Unreference (Lines);

      while VSS.Implementation.Line_Iterators.Forward
        (Data,
         Terminators,
         Initial,
         At_First,
         At_Last,
         Terminator)
      loop
         Initial := At_Last;

         if VSS.Implementation.Strings.Is_Invalid (Terminator) then
            After_Last := At_Last;
            Unchecked_Forward (Text, After_Last);
            --  Dummy      := Text.Forward (After_Last);

         elsif Keep_Terminator then
            After_Last := At_Last;
            Unchecked_Forward (Text, After_Last);
            --  Dummy      := Text.Forward (After_Last);

         else
            After_Last := Terminator;
         end if;

         declare
            Size      : constant VSS.Unicode.UTF8_Code_Unit_Count :=
              After_Last.UTF8_Offset - At_First.UTF8_Offset;
            Storage   : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
              (0 .. Text.Size)
              with Import, Address => Text.Storage_Address;
            Line_Data : VSS.Implementation.Strings.String_Data;
            Line_Text : VSS.Implementation.Text_Handlers.UTF8.UTF8_Text :=
              (others => <>)
              with Address => Line_Data.Storage'Address;

         begin
            if Size /= 0 then
               VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
                 (Line_Text.Data,
                  Storage (At_First.UTF8_Offset .. After_Last.UTF8_Offset - 1),
                  After_Last.Index - At_First.Index);
            end if;

            VSS.Implementation.String_Vectors.Append_And_Move_Ownership
              (Lines, Line_Data);
         end;
      end loop;
   end Split_Lines;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   function To_UTF_8_String
     (Self : UTF8_String_Data) return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      if Self.Size = 0 then
         return "";

      else
         declare
            Result : constant Ada.Strings.UTF_Encoding.UTF_8_String
              (1 .. Natural (Self.Size))
                with Import, Address => Self.Storage_Address;
         begin
            return Result;
         end;
      end if;
   end To_UTF_8_String;

   ------------------------
   -- Unchecked_Backward --
   ------------------------

   procedure Unchecked_Backward
     (Self     : UTF8_String_Data;
      Position : in out VSS.Implementation.Strings.Cursor)
   is
      Storage : constant VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
        (0 .. Self.Size)
        with Import, Address => Self.Storage_Address;

   begin
      Position.Index        := Position.Index - 1;
      Position.UTF8_Offset  := Position.UTF8_Offset - 1;
      Position.UTF16_Offset := Position.UTF16_Offset - 1;

      if Position.Index /= 0 then
         loop
            declare
               Code : constant VSS.Unicode.UTF8_Code_Unit :=
                 Storage (Position.UTF8_Offset);

            begin
               case Code is
                  when 16#80# .. 16#BF# =>
                     Position.UTF8_Offset  := Position.UTF8_Offset - 1;

                  when 16#00# .. 16#7F#
                     | 16#C2# .. 16#DF#
                     | 16#E0# .. 16#EF# =>

                     exit;

                  when 16#F0# .. 16#F4# =>
                     Position.UTF16_Offset := Position.UTF16_Offset - 1;
                     exit;

                  when others =>
                     raise Program_Error with "string data is corrupted";
               end case;
            end;
         end loop;
      end if;
   end Unchecked_Backward;

   -------------------------------
   -- Unchecked_Backward_Decode --
   -------------------------------

   procedure Unchecked_Backward_Decode
     (Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Offset  : in out VSS.Unicode.UTF8_Code_Unit_Index;
      Code    : out VSS.Unicode.Code_Point) is
   begin
      Offset := Offset - 1;

      loop
         declare
            Code : constant VSS.Unicode.UTF8_Code_Unit := Storage (Offset);

         begin
            case Code is
               when 16#80# .. 16#BF# =>
                  Offset  := Offset - 1;

               when 16#00# .. 16#7F#
                  | 16#C2# .. 16#DF#
                  | 16#E0# .. 16#EF# =>

                  exit;

               when 16#F0# .. 16#F4# =>
                  exit;

               when others =>
                  raise Program_Error with "string data is corrupted";
            end case;
         end;
      end loop;

      Code :=
        VSS.Implementation.UTF8_Encoding.Unchecked_Decode (Storage, Offset);
   end Unchecked_Backward_Decode;

   -------------------------------
   -- Unchecked_Backward_Decode --
   -------------------------------

   procedure Unchecked_Backward_Decode
     (Text   : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Offset : in out VSS.Unicode.UTF8_Code_Unit_Index;
      Code   : out VSS.Unicode.Code_Point)
   is
      Storage : constant VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
        (0 .. Text.Size)
        with Import, Address => Text.Storage_Address;

   begin
      Unchecked_Backward_Decode (Storage, Offset, Code);
   end Unchecked_Backward_Decode;

   ------------------------------
   -- Unchecked_Decode_Forward --
   ------------------------------

   procedure Unchecked_Decode_Forward
     (Text   : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Offset : in out VSS.Unicode.UTF8_Code_Unit_Index;
      Code   : out VSS.Unicode.Code_Point)
   is
      Storage : constant VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
        (0 .. Text.Size)
        with Import, Address => Text.Storage_Address;

   begin
      VSS.Implementation.UTF8_Encoding.Unchecked_Decode_Forward
        (Storage, Offset, Code);
   end Unchecked_Decode_Forward;

   -----------------------
   -- Unchecked_Forward --
   -----------------------

   procedure Unchecked_Forward
     (Self     : UTF8_String_Data;
      Position : in out VSS.Implementation.Strings.Cursor)
   is
      pragma Suppress (Overflow_Check);
      pragma Suppress (Range_Check);
      --  These checks slowdown execution, but can happen on large text
      --  data or invalid input only. Would be nice to verify that they
      --  are impossible, or modify something to make them impossible.

      Storage : constant VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
        (0 .. Self.Size)
        with Import, Address => Self.Storage_Address;

   begin
      Position.Index := @ + 1;

      if Position.Index = 1 then
         Position.UTF8_Offset  := 0;
         Position.UTF16_Offset := 0;

         return;
      end if;

      declare
         use type Interfaces.Integer_32;
         use type VSS.Unicode.UTF8_Code_Unit;

         --  This code is based on the fact that starting byte of the
         --  multibyte sequence in UTF-8 has N most significant bits set
         --  to one followed by zero bit. So, first byte of the sequence
         --  is negated and number of leading zero bits is counting.

         Code   : constant VSS.Unicode.UTF8_Code_Unit :=
           Storage (Position.UTF8_Offset);
         Length : constant Interfaces.Integer_32 :=
           VSS.Implementation.GCC.clz (Interfaces.Unsigned_32 (not Code))
             - 24;

      begin
         if Code <= 16#7F# then
            Position.UTF8_Offset  := @ + 1;
            Position.UTF16_Offset := @ + 1;

         else
            Position.UTF8_Offset  :=
              @ + VSS.Unicode.UTF8_Code_Unit_Offset (Length);
            Position.UTF16_Offset :=
              @ + VSS.Unicode.UTF16_Code_Unit_Offset (Length / 4 + 1);
         end if;
      end;
   end Unchecked_Forward;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Self : in out UTF8_String_Data) is
   begin
      if not Is_SSO (Self) then
         declare
            Manager :
              VSS.Implementation.Storage_Managers.Abstract_Storage_Manager
              with Import, Address => Self.Manager'Address;

         begin
            VSS.Implementation.Storage_Managers.Abstract_Storage_Manager'Class
              (Manager).Unreference;
         end;
      end if;

      Self.Manager         := [others => 0];
      Self.Storage_Address := System.Null_Address;
      Self.Flags           := 0;
      Self.Size            := 0;
      Self.Length          := 0;
   end Unreference;

end VSS.Implementation.UTF8_Strings;
