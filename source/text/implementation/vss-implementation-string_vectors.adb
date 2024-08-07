--
--  Copyright (C) 2020-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Ada.Unchecked_Deallocation;
with System.Storage_Elements;

with VSS.Implementation.Character_Codes;
with VSS.Implementation.Text_Handlers;
with VSS.Unicode;

package body VSS.Implementation.String_Vectors is

   procedure Free is
     new Ada.Unchecked_Deallocation
       (VSS.Implementation.String_Vectors.String_Vector_Data,
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);

   Line_Terminator_To_Code_Point : constant
     array (VSS.Strings.Line_Terminator) of VSS.Unicode.Code_Point :=
       [VSS.Strings.CR   => VSS.Implementation.Character_Codes.Carriage_Return,
        VSS.Strings.LF   => VSS.Implementation.Character_Codes.Line_Feed,
        VSS.Strings.CRLF => VSS.Implementation.Character_Codes.Carriage_Return,
        VSS.Strings.NEL  => VSS.Implementation.Character_Codes.Next_Line,
        VSS.Strings.VT   => VSS.Implementation.Character_Codes.Line_Tabulation,
        VSS.Strings.FF   => VSS.Implementation.Character_Codes.Form_Feed,
        VSS.Strings.LS   => VSS.Implementation.Character_Codes.Line_Separator,
        VSS.Strings.PS   =>
          VSS.Implementation.Character_Codes.Paragraph_Separator];
   --  Mapping from Line_Terminator to code point of first character of the
   --  line terminator sequence. Only CRLF case requires longer sequence and
   --  it is processed separately.

   Growth_Factor : constant := 2;
   --  The growth factor controls how much extra space is allocated when
   --  we have to increase the size of an allocated vector storage. By
   --  allocating extra space, we avoid the need to reallocate on every
   --  append, particularly important when a vector is built up by repeated
   --  append operations of an individual items. This is expressed as a
   --  factor so 2 means add 1/2 of the length of the vector as growth space.

   Min_Mul_Alloc : constant := Standard'Maximum_Alignment;
   --  Allocation will be done by a multiple of Min_Mul_Alloc. This causes
   --  no memory loss as most (all?) malloc implementations are obliged to
   --  align the returned memory on the maximum alignment as malloc does not
   --  know the target alignment.

   procedure Mutate
     (Self     : in out String_Vector_Data_Access;
      Required : Natural;
      Reserved : Natural);
   --  Prepare object to be modified and reserve space for at least Required
   --  number of items and up to additional Reserved number of items.
   --  Parameters Required and Reserve are separated to prevent potential
   --  integer overflow at the caller side.

   function Aligned_Length
     (Required : Natural;
      Reserved : Natural) return Natural;
   --  Return recommended length of the vector storage which is enough to
   --  store at least Required number of items and up to Reserved number of
   --  items additionally. Calculation takes into account alignment of the
   --  allocated memory segments to use memory effectively by
   --  Append/Insert/etc operations.

   --------------------
   -- Aligned_Length --
   --------------------

   function Aligned_Length
     (Required : Natural;
      Reserved : Natural) return Natural
   is
      use type System.Storage_Elements.Storage_Offset;

      subtype Empty_String_Vector_Data is String_Vector_Data (0);

      Element_Size : constant System.Storage_Elements.Storage_Count :=
        VSS.Implementation.Strings.String_Data'Max_Size_In_Storage_Elements;

      Static_Size  : constant System.Storage_Elements.Storage_Count :=
        Empty_String_Vector_Data'Max_Size_In_Storage_Elements;

   begin
      if Required > Natural'Last - Reserved then
         --  Total requested number of items is large than maximum number,
         --  so limit it to maximum number of items.

         return Natural'Last;

      else
         return
           Natural
             ((((Static_Size
              + System.Storage_Elements.Storage_Count (Required + Reserved)
              * Element_Size + Min_Mul_Alloc - 1) / Min_Mul_Alloc)
              * Min_Mul_Alloc - Static_Size) / Element_Size);
      end if;
   end Aligned_Length;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out String_Vector_Data_Access;
      Item : VSS.Implementation.Strings.String_Data) is
   begin
      if Self = null then
         Mutate (Self, 1, 0);

      else
         Mutate (Self, Self.Last + 1, Self.Last / Growth_Factor);
      end if;

      Self.Last := Self.Last + 1;
      Self.Data (Self.Last) := Item;

      VSS.Implementation.Strings.Reference (Self.Data (Self.Last));
   end Append;

   -------------------------------
   -- Append_And_Move_Ownership --
   -------------------------------

   procedure Append_And_Move_Ownership
     (Self : in out String_Vector_Data_Access;
      Item : VSS.Implementation.Strings.String_Data) is
   begin
      if Self = null then
         Mutate (Self, 1, 0);

      else
         Mutate (Self, Self.Last + 1, Self.Last / Growth_Factor);
      end if;

      Self.Last := Self.Last + 1;
      Self.Data (Self.Last) := Item;
   end Append_And_Move_Ownership;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self : String_Vector_Data_Access;
      Item : VSS.Implementation.Strings.String_Data) return Boolean
   is
      Item_Text : VSS.Implementation.Text_Handlers.Abstract_Text_Handler'Class
        renames VSS.Implementation.Strings.Constant_Handler (Item).all;

   begin
      if Self = null then
         return False;
      end if;

      for J in Self.Data'First .. Self.Last loop
         if Item_Text.Is_Equal
              (VSS.Implementation.Strings.Constant_Handler (Self.Data (J)).all)
         then
            return True;
         end if;
      end loop;

      return False;
   end Contains;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self  : in out not null String_Vector_Data_Access;
      Index : Positive) is
   begin
      if Index <= Self.Last then
         Mutate (Self, Self.Last, 0);

         VSS.Implementation.Strings.Unreference (Self.Data (Index));

         Self.Data (Index .. Self.Last - 1) :=
           Self.Data (Index + 1 .. Self.Last);
         Self.Last := Self.Last - 1;
      end if;
   end Delete;

   ----------------
   -- Join_Lines --
   ----------------

   procedure Join_Lines
     (Self           : String_Vector_Data_Access;
      Result         : in out VSS.Implementation.Strings.String_Data;
      Terminator     : VSS.Strings.Line_Terminator;
      Terminate_Last : Boolean) is
   begin
      VSS.Implementation.Strings.Unreference (Result);

      if Self = null then
         return;
      end if;

      declare
         use type VSS.Strings.Line_Terminator;

         Offset : VSS.Implementation.Strings.Cursor_Offset;
         Text   : constant not null
           VSS.Implementation.Strings.Variable_Text_Handler_Access :=
             VSS.Implementation.Strings.Variable_Handler (Result);

      begin
         for J in 1 .. Self.Last loop
            Text.Append (Result, Self.Data (J), Offset);

            if J /= Self.Last or Terminate_Last then
               Text.Append
                 (Line_Terminator_To_Code_Point (Terminator), Offset);

               if Terminator = VSS.Strings.CRLF then
                  Text.Append
                    (VSS.Implementation.Character_Codes.Line_Feed, Offset);
               end if;
            end if;
         end loop;
      end;
   end Join_Lines;

   ------------
   -- Mutate --
   ------------

   procedure Mutate
     (Self     : in out String_Vector_Data_Access;
      Required : Natural;
      Reserved : Natural)
   is
   begin
      if Self = null then
         Self := new String_Vector_Data (Aligned_Length (Required, Reserved));

      elsif not System.Atomic_Counters.Is_One (Self.Counter)
        or else Self.Bulk < Required
      then
         declare
            Old : String_Vector_Data_Access := Self;

         begin
            Self :=
              new String_Vector_Data (Aligned_Length (Required, Reserved));
            Self.Last := Old.Last;
            Self.Data (1 .. Old.Last) := Old.Data (1 .. Old.Last);

            if not System.Atomic_Counters.Is_One (Old.Counter) then
               for Data of Self.Data (1 .. Self.Last) loop
                  VSS.Implementation.Strings.Reference (Data);
               end loop;

               Unreference (Old);

            else
               Free (Old);
            end if;
         end;
      end if;
   end Mutate;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Self : in out String_Vector_Data_Access;
      Item : VSS.Implementation.Strings.String_Data) is
   begin
      if Self = null then
         Mutate (Self, 1, 0);

      else
         Mutate (Self, Self.Last + 1, Self.Last / Growth_Factor);
      end if;

      Self.Data (2 .. Self.Last + 1) := Self.Data (1 .. Self.Last);
      Self.Last := Self.Last + 1;
      Self.Data (1) := Item;

      VSS.Implementation.Strings.Reference (Self.Data (1));
   end Prepend;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Self : String_Vector_Data_Access) is
   begin
      if Self /= null then
         System.Atomic_Counters.Increment (Self.Counter);
      end if;
   end Reference;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Self  : in out not null String_Vector_Data_Access;
      Index : Positive;
      Item  : VSS.Implementation.Strings.String_Data) is
   begin
      Mutate (Self, Self.Last, 0);
      VSS.Implementation.Strings.Unreference (Self.Data (Index));
      Self.Data (Index) := Item;
      VSS.Implementation.Strings.Reference (Self.Data (Index));
   end Replace;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Self : in out String_Vector_Data_Access) is
   begin
      if Self /= null then
         if System.Atomic_Counters.Decrement (Self.Counter) then
            for J in 1 .. Self.Last loop
               VSS.Implementation.Strings.Unreference (Self.Data (J));
            end loop;

            Free (Self);

         else
            --  Data is shared, reset own pointer to null because counter
            --  is decremented.

            Self := null;
         end if;
      end if;
   end Unreference;

end VSS.Implementation.String_Vectors;
