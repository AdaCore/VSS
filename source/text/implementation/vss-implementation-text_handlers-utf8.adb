--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Interfaces;

with VSS.Implementation.GCC;
with VSS.Implementation.Line_Iterators;
with VSS.Implementation.Text_Handlers.UTF8.Variable.Dynamic;
with VSS.Implementation.Text_Handlers.UTF8.Variable.Static;
with VSS.Strings;

package body VSS.Implementation.Text_Handlers.UTF8 is

   use type VSS.Unicode.UTF16_Code_Unit_Offset;
   use type VSS.Unicode.UTF8_Code_Unit_Offset;
   use type VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;

   --------------
   -- Is_Equal --
   --------------

   overriding function Is_Equal
     (Self  : Abstract_UTF8_Text;
      Other : Abstract_Text_Handler'Class) return Boolean is
   begin
      if Other in Abstract_UTF8_Text'Class then
         declare
            Self_Pointer  :
              VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
            Self_Size     : VSS.Unicode.UTF8_Code_Unit_Count;
            Other_Pointer :
              VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
            Other_Size    : VSS.Unicode.UTF8_Code_Unit_Count;

         begin
            Abstract_UTF8_Text'Class (Self).UTF8_Constant_Storage_And_Size
              (Self_Pointer, Self_Size);
            Abstract_UTF8_Text'Class (Other).UTF8_Constant_Storage_And_Size
              (Other_Pointer, Other_Size);

            declare
               Self_Storage  : constant
                 VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                   (0 .. Self_Size - 1)
                 with Import,
                      Convention => Ada,
                      Address => Self_Pointer.all'Address;
               Other_Storage : constant
                 VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                   (0 .. Other_Size - 1)
                 with Import,
                      Convention => Ada,
                      Address => Other_Pointer.all'Address;

            begin
               return Self_Storage = Other_Storage;
            end;
         end;

      else
         return Abstract_Text_Handler (Self).Is_Equal (Other);
      end if;
   end Is_Equal;

   -------------
   -- Is_Less --
   -------------

   overriding function Is_Less
     (Self  : Abstract_UTF8_Text;
      Other : Abstract_Text_Handler'Class) return Boolean is
   begin
      if Other in Abstract_UTF8_Text'Class then
         declare
            Self_Pointer  :
              VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
            Self_Size     : VSS.Unicode.UTF8_Code_Unit_Count;
            Other_Pointer :
              VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
            Other_Size    : VSS.Unicode.UTF8_Code_Unit_Count;

         begin
            Abstract_UTF8_Text'Class (Self).UTF8_Constant_Storage_And_Size
              (Self_Pointer, Self_Size);
            Abstract_UTF8_Text'Class (Other).UTF8_Constant_Storage_And_Size
              (Other_Pointer, Other_Size);

            declare
               Self_Storage  : constant
                 VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                   (0 .. Self_Size - 1)
                 with Import,
                      Convention => Ada,
                      Address => Self_Pointer.all'Address;
               Other_Storage : constant
                 VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                   (0 .. Other_Size - 1)
                 with Import,
                      Convention => Ada,
                      Address => Other_Pointer.all'Address;

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
     (Self  : Abstract_UTF8_Text;
      Other : Abstract_Text_Handler'Class) return Boolean is
   begin
      if Other in Abstract_UTF8_Text'Class then
         declare
            Self_Pointer  :
              VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
            Self_Size     : VSS.Unicode.UTF8_Code_Unit_Count;
            Other_Pointer :
              VSS.Implementation.Interfaces_C.UTF8_Code_Unit_Constant_Access;
            Other_Size    : VSS.Unicode.UTF8_Code_Unit_Count;

         begin
            Abstract_UTF8_Text'Class (Self).UTF8_Constant_Storage_And_Size
              (Self_Pointer, Self_Size);
            Abstract_UTF8_Text'Class (Other).UTF8_Constant_Storage_And_Size
              (Other_Pointer, Other_Size);

            declare
               Self_Storage  : constant
                 VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                   (0 .. Self_Size - 1)
                 with Import,
                      Convention => Ada,
                      Address => Self_Pointer.all'Address;
               Other_Storage : constant
                 VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                   (0 .. Other_Size - 1)
                 with Import,
                      Convention => Ada,
                      Address => Other_Pointer.all'Address;

            begin
               return Self_Storage <= Other_Storage;
            end;
         end;

      else
         return Abstract_Text_Handler (Self).Is_Less_Or_Equal (Other);
      end if;
   end Is_Less_Or_Equal;

   ------------------------
   -- Split_Lines_Common --
   ------------------------

   procedure Split_Lines_Common
     (Text            :
        VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class;
      Data            : VSS.Implementation.Strings.String_Data;
      Storage         : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Terminators     : VSS.Strings.Line_Terminator_Set;
      Keep_Terminator : Boolean;
      Lines           : in out
        VSS.Implementation.String_Vectors.String_Vector_Data_Access)
   is
      procedure Append
        (Source_Storage :
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
         First          : VSS.Implementation.Strings.Cursor;
         After_Last     : VSS.Implementation.Strings.Cursor);

      ------------
      -- Append --
      ------------

      procedure Append
        (Source_Storage :
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
         First          : VSS.Implementation.Strings.Cursor;
         After_Last     : VSS.Implementation.Strings.Cursor)
      is
         Size : constant VSS.Unicode.UTF8_Code_Unit_Count :=
           After_Last.UTF8_Offset - First.UTF8_Offset;
         Data : VSS.Implementation.Strings.String_Data;

      begin
         if Size = 0 then
            null;

         elsif Size <= Variable.Static.In_Place_Storage_Capacity then
            --  Static storage can be used.

            declare
               Static : Variable.Static.Static_UTF8_Handler := (others => <>)
                 with Address => Data.Storage'Address;

            begin
               Static.Storage (0 .. Size - 1) :=
                 Source_Storage
                   (First.UTF8_Offset .. After_Last.UTF8_Offset - 1);
               Static.Storage (Size) := 0;
               Static.Size   := Size;
               Static.Length := After_Last.Index - First.Index;
            end;

         else
            declare
               Pointer : Variable.Dynamic.UTF8_String_Data_Access;
               Dynamic : Variable.Dynamic.Dynamic_UTF8_Handler :=
                 (others => <>)
                 with Address => Data.Storage'Address;

            begin
               Pointer := UTF8.Variable.Dynamic.Allocate (0, Size);

               Pointer.Storage (0 .. Size - 1) :=
                 Source_Storage
                   (First.UTF8_Offset .. After_Last.UTF8_Offset - 1);
               Pointer.Size   := Size;
               Pointer.Length := After_Last.Index - First.Index;
               Pointer.Storage (Pointer.Size) := 16#00#;

               Dynamic.Pointer := Pointer;
            end;
         end if;

         VSS.Implementation.String_Vectors.Append_And_Move_Ownership
           (Lines, Data);
      end Append;

      Initial    : VSS.Implementation.Strings.Cursor;
      At_First   : aliased VSS.Implementation.Strings.Cursor;
      At_Last    : aliased VSS.Implementation.Strings.Cursor;
      After_Last : aliased VSS.Implementation.Strings.Cursor;
      Terminator : VSS.Implementation.Strings.Cursor;
      Dummy      : Boolean;

   begin
      VSS.Implementation.String_Vectors.Unreference (Lines);

      Text.Before_First_Character (Initial);

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
            Dummy      := Text.Forward (After_Last);

         elsif Keep_Terminator then
            After_Last := At_Last;
            Dummy      := Text.Forward (After_Last);

         else
            After_Last := Terminator;
         end if;

         Append (Storage, At_First, After_Last);
      end loop;
   end Split_Lines_Common;

   ------------------------
   -- Unchecked_Backward --
   ------------------------

   procedure Unchecked_Backward
     (Storage  : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Position : in out VSS.Implementation.Strings.Cursor) is
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

   -----------------------
   -- Unchecked_Forward --
   -----------------------

   procedure Unchecked_Forward
     (Storage  : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Position : in out VSS.Implementation.Strings.Cursor)
   is
      pragma Suppress (Overflow_Check);
      pragma Suppress (Range_Check);
      --  These checks slowdown execution, but can happen on large text
      --  data or invalid input only. Would be nice to verify that they
      --  are impossible, or modify something to make them impossible.

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

end VSS.Implementation.Text_Handlers.UTF8;
