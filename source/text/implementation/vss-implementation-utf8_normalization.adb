--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Implementation.UCD_Normalization_Common;
with VSS.Implementation.UTF8_String_Handlers;

package body VSS.Implementation.UTF8_Normalization is

   use type VSS.Implementation.Strings.Character_Count;
   use type VSS.Unicode.UTF8_Code_Unit_Offset;

   procedure Append_Reordered
     (Result_Data        : in out VSS.Implementation.Strings.String_Data;
      Result_Size        : in out VSS.Unicode.UTF8_Code_Unit_Count;
      Decomposition_Data :
        VSS.Implementation.UCD_Normalization_UTF8.Mapping_Data_Offset_Array;
      Last_CCC           :
        in out VSS.Implementation.UCD_Normalization_UTF8.CCC_Values;
      Source_Storage     :
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Source_Offset      : VSS.Unicode.UTF8_Code_Unit_Offset;
      Source_Size        : VSS.Unicode.UTF8_Code_Unit_Count);
   --  Append full decomposition mapping specified by Info to the end of the
   --  Result_Data with canonical reordering when necessary.

   function Get_Decomposition_Information
     (Decomposition_Data :
        VSS.Implementation.UCD_Normalization_UTF8.Mapping_Data_Offset_Array;
      Code               : VSS.Unicode.Code_Point)
      return VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information;
   --  Returns decomposition information for given data and character.

   procedure Unchecked_Decode_Forward
     (Source_Data : VSS.Implementation.Strings.String_Data;
      Offset      : in out VSS.Unicode.UTF8_Code_Unit_Index;
      Code        : out VSS.Unicode.Code_Point);
   --  Decode UTF8 encoded character started at given offset and change offset
   --  to point to the beginning of the next character.

   procedure Unchecked_Backward_Decode
     (Source_Data : VSS.Implementation.Strings.String_Data;
      Offset      : in out VSS.Unicode.UTF8_Code_Unit_Index;
      Code        : out VSS.Unicode.Code_Point);
   --  Change offset to the point of the previous character and decode
   --  character at this position.

   procedure Unchecked_Backward_Decode
     (Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Offset  : in out VSS.Unicode.UTF8_Code_Unit_Index;
      Code    : out VSS.Unicode.Code_Point);
   --  Change offset to the point of the previous character and decode
   --  character at this position.

   procedure Unchecked_Append
     (Target_Data : in out VSS.Implementation.Strings.String_Data;
      Target_Size : out VSS.Unicode.UTF8_Code_Unit_Count;
      Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From        : VSS.Unicode.UTF8_Code_Unit_Index;
      Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      Length      : VSS.Implementation.Strings.Character_Count;
      Terminator  : Boolean := False);
   --  Append given slice of the data to the target. Convert target
   --  from in-place to heap based implementation when necessary.
   --
   --  XXX Duplicate with subprogram in UTF8_String_Handlers

   procedure Unchecked_Insert
     (Target_Data : in out VSS.Implementation.Strings.String_Data;
      Target_Size : out VSS.Unicode.UTF8_Code_Unit_Count;
      Into        : VSS.Unicode.UTF8_Code_Unit_Index;
      Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From        : VSS.Unicode.UTF8_Code_Unit_Index;
      Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      Length      : VSS.Implementation.Strings.Character_Count);
   --  Insert given slice of the data into the target starting from the given
   --  position. Convert target from in-place to heap based implementation
   --  when necessary.

   procedure Unchecked_Delete
     (Target_Data   : in out VSS.Implementation.Strings.String_Data;
      Target_Size   : out VSS.Unicode.UTF8_Code_Unit_Count;
      Delete_From   : VSS.Unicode.UTF8_Code_Unit_Index;
      Delete_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
      Delete_Length : VSS.Implementation.Strings.Character_Count);
   --  Delete given slice of the data from the target starting from the given
   --  position.

   procedure Unchecked_Replace
     (Target_Data    : in out VSS.Implementation.Strings.String_Data;
      Target_Size    : out VSS.Unicode.UTF8_Code_Unit_Count;
      Replace_From   : VSS.Unicode.UTF8_Code_Unit_Index;
      Replace_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
      Replace_Length : VSS.Implementation.Strings.Character_Count;
      Storage        : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Insert_From    : VSS.Unicode.UTF8_Code_Unit_Index;
      Insert_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Insert_Length  : VSS.Implementation.Strings.Character_Count);
   --  Replace given slice of the target data by another data. Convert target
   --  from in-place to heap based implementation when necessary.

   procedure Unchecked_Move_Slice
     (Data : in out VSS.Implementation.Strings.String_Data;
      From : VSS.Unicode.UTF8_Code_Unit_Index;
      Size : VSS.Unicode.UTF8_Code_Unit_Count;
      Into : VSS.Unicode.UTF8_Code_Unit_Index);
   --  Move given slice of the give size of the data starting from the given
   --  position. From and into positions must be valid positions in UTF-8
   --  encoded data, thus size and length of the string is not changed by
   --  this operation.

   ----------------------
   -- Append_Reordered --
   ----------------------

   procedure Append_Reordered
     (Result_Data        : in out VSS.Implementation.Strings.String_Data;
      Result_Size        : in out VSS.Unicode.UTF8_Code_Unit_Count;
      Decomposition_Data :
        VSS.Implementation.UCD_Normalization_UTF8.Mapping_Data_Offset_Array;
      Last_CCC           :
        in out VSS.Implementation.UCD_Normalization_UTF8.CCC_Values;
      Source_Storage     :
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Source_Offset      : VSS.Unicode.UTF8_Code_Unit_Offset;
      Source_Size        : VSS.Unicode.UTF8_Code_Unit_Count)
   is
      use all type VSS.Implementation.UCD_Normalization_UTF8.CCC_Values;

      procedure Reorder_And_Insert
        (Result_Data : in out VSS.Implementation.Strings.String_Data;
         Result_Size : in out VSS.Unicode.UTF8_Code_Unit_Count;
         CCC         : VSS.Implementation.UCD_Normalization_UTF8.CCC_Values;
         Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
         Offset      : VSS.Unicode.UTF8_Code_Unit_Offset;
         Size        : VSS.Unicode.UTF8_Code_Unit_Count);
      --  Insert given encoded character into the string preserving canonical
      --  ordering.

      ------------------------
      -- Reorder_And_Insert --
      ------------------------

      procedure Reorder_And_Insert
        (Result_Data : in out VSS.Implementation.Strings.String_Data;
         Result_Size : in out VSS.Unicode.UTF8_Code_Unit_Count;
         CCC         : VSS.Implementation.UCD_Normalization_UTF8.CCC_Values;
         Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
         Offset      : VSS.Unicode.UTF8_Code_Unit_Offset;
         Size        : VSS.Unicode.UTF8_Code_Unit_Count)
      is
         Previous_Offset : VSS.Unicode.UTF8_Code_Unit_Offset;
         Previous_Code   : VSS.Unicode.Code_Point;
         Previous_Info   :
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information;
         Insert_Offset   : VSS.Unicode.UTF8_Code_Unit_Offset;

      begin
         Previous_Offset := Result_Size;

         loop
            Insert_Offset := Previous_Offset;

            exit when Previous_Offset = 0;

            Unchecked_Backward_Decode
              (Result_Data, Previous_Offset, Previous_Code);

            Previous_Info :=
              Get_Decomposition_Information
                (Decomposition_Data, Previous_Code);

            exit when Previous_Info.CCC = CCC_NR
                        or Previous_Info.CCC <= CCC;
         end loop;

         Unchecked_Insert
           (Result_Data,
            Result_Size,
            Insert_Offset,
            Storage,
            Offset,
            Size,
            1);
      end Reorder_And_Insert;

      Next_Offset     : VSS.Unicode.UTF8_Code_Unit_Offset := Source_Offset;
      Current_Offset  : VSS.Unicode.UTF8_Code_Unit_Offset;
      Current_Code    : VSS.Unicode.Code_Point;
      Current_Info    :
        VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information;

   begin
      loop
         Current_Offset := Next_Offset;

         exit when Next_Offset >= Source_Offset + Source_Size;

         VSS.Implementation.UTF8_Encoding.Unchecked_Decode_Forward
           (Source_Storage, Next_Offset, Current_Code);

         Current_Info :=
           Get_Decomposition_Information (Decomposition_Data, Current_Code);

         if Last_CCC > Current_Info.CCC then
            Reorder_And_Insert
              (Result_Data,
               Result_Size,
               Current_Info.CCC,
               Source_Storage,
               Current_Offset,
               Next_Offset - Current_Offset);

         else
            pragma Warnings (Off);
            --  Disable warnings, this code is never executed for now,
            --  however, may need to be completed later.

            Last_CCC := Current_Info.CCC;

            raise Program_Error;

            pragma Warnings (On);
         end if;
      end loop;
   end Append_Reordered;

   -----------------
   -- Decomposite --
   -----------------

   procedure Decompose
     (Source_Storage     :
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Source_Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      Decomposition_Data :
        VSS.Implementation.UCD_Normalization_UTF8.Mapping_Data_Offset_Array;
      Result_Data        : out VSS.Implementation.Strings.String_Data)
   is
      use all type VSS.Implementation.UCD_Normalization_UTF8.CCC_Values;
      use type VSS.Unicode.Code_Point;

      procedure Reorder_And_Insert
        (Result_Data : in out VSS.Implementation.Strings.String_Data;
         Result_Size : in out VSS.Unicode.UTF8_Code_Unit_Count;
         CCC         : VSS.Implementation.UCD_Normalization_UTF8.CCC_Values;
         Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
         Offset      : VSS.Unicode.UTF8_Code_Unit_Offset;
         Size        : VSS.Unicode.UTF8_Code_Unit_Count);
      --  Insert given encoded character into the string preserving canonical
      --  ordering.

      ------------------------
      -- Reorder_And_Insert --
      ------------------------

      procedure Reorder_And_Insert
        (Result_Data : in out VSS.Implementation.Strings.String_Data;
         Result_Size : in out VSS.Unicode.UTF8_Code_Unit_Count;
         CCC         : VSS.Implementation.UCD_Normalization_UTF8.CCC_Values;
         Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
         Offset      : VSS.Unicode.UTF8_Code_Unit_Offset;
         Size        : VSS.Unicode.UTF8_Code_Unit_Count)
      is
         Previous_Offset : VSS.Unicode.UTF8_Code_Unit_Offset;
         Previous_Code   : VSS.Unicode.Code_Point;
         Previous_Info   :
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information;
         Insert_Offset   : VSS.Unicode.UTF8_Code_Unit_Offset;

      begin
         Previous_Offset := Result_Size;

         loop
            Insert_Offset := Previous_Offset;

            exit when Previous_Offset = 0;

            Unchecked_Backward_Decode
              (Result_Data, Previous_Offset, Previous_Code);

            Previous_Info :=
              Get_Decomposition_Information
                (Decomposition_Data, Previous_Code);

            exit when Previous_Info.CCC = CCC_NR
                        or Previous_Info.CCC <= CCC;
         end loop;

         Unchecked_Insert
           (Result_Data,
            Result_Size,
            Insert_Offset,
            Storage,
            Offset,
            Size,
            1);
      end Reorder_And_Insert;

      Result_Size : VSS.Unicode.UTF8_Code_Unit_Count := 0;
      Code        : VSS.Unicode.Code_Point;
      Info        :
        VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information;
      Start       : VSS.Unicode.UTF8_Code_Unit_Offset;
      Out_Start   : VSS.Unicode.UTF8_Code_Unit_Offset;
      Offset      : VSS.Unicode.UTF8_Code_Unit_Offset := 0;
      Length      : VSS.Implementation.Strings.Character_Count;
      Last_CCC    : VSS.Implementation.UCD_Normalization_UTF8.CCC_Values :=
        VSS.Implementation.UCD_Normalization_UTF8.CCC_NR;

   begin
      loop
         --  Check whether source string is in normalization form, and attempt
         --  to lookup for maximum length of normalized data.

         Start  := Offset;
         Length := 0;

         loop
            Out_Start := Offset;

            exit when Offset >= Source_Size;

            VSS.Implementation.UTF8_Encoding.Unchecked_Decode_Forward
              (Source_Storage, Offset, Code);

            Info := Get_Decomposition_Information (Decomposition_Data, Code);

            exit when not Info.Decomposition_QC;
            --  Copy data and run normalization

            exit when Last_CCC > Info.CCC and Info.CCC /= CCC_NR;
            --  Copy data and run reordering

            Last_CCC := Info.CCC;
            Length   := Length + 1;
         end loop;

         --  Copy found normalized data if any

         if Start /= Out_Start then
            Unchecked_Append
              (Result_Data,
               Result_Size,
               Source_Storage,
               Start,
               Out_Start - Start,
               Length);
         end if;

         exit when Out_Start >= Source_Size;
         --  Source text has been processed completely, exit.

         if Info.Decomposition_QC then
            --  Apply canonical ordering algoriphm to the next character in
            --  the source string.

            Reorder_And_Insert
              (Result_Data,
               Result_Size,
               Info.CCC,
               Source_Storage,
               Out_Start,
               Offset - Out_Start);

         else
            --  Apply decomposition mapping

            if Code in 16#AC00# .. 16#AC00# + 11_172 then
               --  Hangul syllables are decomposed algorithmically.

               declare
                  use type VSS.Unicode.UTF8_Code_Unit;

                  S_Base  : constant := 16#AC00#;
                  L_Base  : constant := 16#1100#;
                  V_Base  : constant := 16#1161#;
                  T_Base  : constant := 16#11A7#;

                  T_Count : constant := 28;
                  N_Count : constant := 588;  --  V_Count * T_Count

                  S_Index : constant VSS.Unicode.Code_Point := Code - S_Base;
                  L_Index : constant VSS.Unicode.Code_Point :=
                    S_Index / N_Count;
                  V_Index : constant VSS.Unicode.Code_Point :=
                    (S_Index mod N_Count) / T_Count;
                  T_Index : constant VSS.Unicode.Code_Point :=
                    S_Index mod T_Count;
                  L_Part  : constant VSS.Unicode.Code_Point :=
                    L_Base + L_Index;
                  V_Part  : constant VSS.Unicode.Code_Point :=
                    V_Base + V_Index;
                  T_Part  : constant VSS.Unicode.Code_Point :=
                    T_Base + T_Index;

                  Aux : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                    (0 .. 8);

               begin
                  --  First byte of encoded sequence for all characters of the
                  --  decomposition is always 16#E1#, thus don't compute it and
                  --  ignore corresponding bits in L_Part/V_Part/T_Part.

                  Aux (0) := 16#E1#;
                  Aux (1) :=
                    16#80#
                      or VSS.Unicode.UTF8_Code_Unit
                           ((L_Part / 16#40#) mod 16#40#);
                  Aux (2) :=
                    16#80# or VSS.Unicode.UTF8_Code_Unit (L_Part mod 16#40#);

                  Aux (3) := 16#E1#;
                  Aux (4) :=
                    16#80#
                      or VSS.Unicode.UTF8_Code_Unit
                           ((V_Part / 16#40#) mod 16#40#);
                  Aux (5) :=
                    16#80# or VSS.Unicode.UTF8_Code_Unit (V_Part mod 16#40#);

                  if T_Index = 0 then
                     Unchecked_Append
                       (Result_Data,
                        Result_Size,
                        Aux,
                        0,
                        6,
                        2);

                  else
                     Aux (6) := 16#E1#;
                     Aux (7) :=
                       16#80#
                         or VSS.Unicode.UTF8_Code_Unit
                              ((T_Part / 16#40#) mod 16#40#);
                     Aux (8) :=
                       16#80#
                         or VSS.Unicode.UTF8_Code_Unit (T_Part mod 16#40#);

                     Unchecked_Append
                       (Result_Data,
                        Result_Size,
                        Aux,
                        0,
                        9,
                        3);
                  end if;

                  Last_CCC := CCC_NR;
               end;

            elsif Info.First_CCC /= CCC_NR
              and Last_CCC /= CCC_NR
              and Last_CCC > Info.First_CCC
            then
               --  Reordering is necessary

               Append_Reordered
                 (Result_Data,
                  Result_Size,
                  Decomposition_Data,
                  Last_CCC,
                  VSS.Implementation.UCD_Normalization_UTF8.UTF8_Data_Table,
                  Info.Offset,
                  Info.Size);

            else
               Unchecked_Append
                 (Result_Data,
                  Result_Size,
                  VSS.Implementation.UCD_Normalization_UTF8.UTF8_Data_Table,
                  Info.Offset,
                  Info.Size,
                  Info.Length);
               Last_CCC := Info.Last_CCC;
            end if;
         end if;

         exit when Offset >= Source_Size;
      end loop;
   end Decompose;

   ---------------------------
   -- Decompose_And_Compose --
   ---------------------------

   procedure Decompose_And_Compose
     (Source_Storage     :
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Source_Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      Decomposition_Data :
        VSS.Implementation.UCD_Normalization_UTF8.Mapping_Data_Offset_Array;
      Result_Data        : out VSS.Implementation.Strings.String_Data)
   is
      use type
        VSS.Implementation.UCD_Normalization_Common.First_Mapping_Code_Offset;
      use type
        VSS.Implementation.UCD_Normalization_Common.Last_Mapping_Code_Offset;
      use all type VSS.Implementation.UCD_Normalization_UTF8.CCC_Values;
      use all type
        VSS.Implementation.UCD_Normalization_UTF8.Composition_Quick_Check;
      use type VSS.Unicode.Code_Point;

      S_Base  : constant := 16#AC00#;
      L_Base  : constant := 16#1100#;
      V_Base  : constant := 16#1161#;
      T_Base  : constant := 16#11A7#;
      V_Count : constant := 21;
      T_Count : constant := 28;
      N_Count : constant := V_Count * T_Count;  --  588;

      function Backward
        (Data   : VSS.Implementation.Strings.String_Data;
         From   : VSS.Unicode.UTF8_Code_Unit_Offset;
         Offset : out VSS.Unicode.UTF8_Code_Unit_Offset;
         Size   : out VSS.Unicode.UTF8_Code_Unit_Count;
         Code   : out VSS.Unicode.Code_Point;
         Info   : out
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information)
         return Boolean;
      --  Decode previous character, obtain its normalization information
      --  and return True on success. Return False when given character is
      --  first character of the string.

      function Has_Decomposition
        (Info : VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information)
         return Boolean;
      --  Return True when character has full decomposition.

      procedure Apply_Decomposition
        (Result_Data     : in out VSS.Implementation.Strings.String_Data;
         Result_Size     : in out VSS.Unicode.UTF8_Code_Unit_Count;
         From_Offset     : VSS.Unicode.UTF8_Code_Unit_Offset;
         From_Size       : VSS.Unicode.UTF8_Code_Unit_Count;
         From_Info       :
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information;
         Skip_Compositon : out Boolean;
         Starter_Offset  : out VSS.Unicode.UTF8_Code_Unit_Offset;
         Starter_Size    : out VSS.Unicode.UTF8_Code_Unit_Count;
         Starter_Code    : out VSS.Unicode.Code_Point;
         Starter_Info    : out
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information;
         Previous_CCC    : out
           VSS.Implementation.UCD_Normalization_UTF8.CCC_Values);
      --  Apply decomposition to the slice of the string starting from given
      --  offset.

      procedure Apply_Canonical_Composition
        (Result_Data : in out VSS.Implementation.Strings.String_Data;
         Result_Size : in out VSS.Unicode.UTF8_Code_Unit_Count;
         From_Offset : VSS.Unicode.UTF8_Code_Unit_Offset;
         From_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
         From_Code   : VSS.Unicode.Code_Point;
         From_Info   :
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information)
             with Pre => From_Info.CCC = CCC_NR;
      --  Apply canonical composition to the slice of the string starting
      --  from the given position till the end of the string. Character at
      --  the position must be starter.

      procedure Lookup_Starter
        (Result_Data        : VSS.Implementation.Strings.String_Data;
         Result_Size        : VSS.Unicode.UTF8_Code_Unit_Offset;
         Need_Decomposition : out Boolean;
         Skip_Composition   : out Boolean;
         Starter_Found      : out Boolean;
         Starter_Offset     : out VSS.Unicode.UTF8_Code_Unit_Offset;
         Starter_Size       : out VSS.Unicode.UTF8_Code_Unit_Count;
         Starter_Code       : out VSS.Unicode.Code_Point;
         Starter_Info       : out
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information);
      --  Lookup for the last character in the string that is starter
      --  or its decomposition will provide starter.

      ---------------------------------
      -- Apply_Canonical_Composition --
      ---------------------------------

      procedure Apply_Canonical_Composition
        (Result_Data : in out VSS.Implementation.Strings.String_Data;
         Result_Size : in out VSS.Unicode.UTF8_Code_Unit_Count;
         From_Offset : VSS.Unicode.UTF8_Code_Unit_Offset;
         From_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
         From_Code   : VSS.Unicode.Code_Point;
         From_Info   :
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information)
      is
         Previous_CCC   : VSS.Implementation.UCD_Normalization_UTF8.CCC_Values;
         Current_Offset : VSS.Unicode.UTF8_Code_Unit_Offset;
         Current_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
         Current_Code   : VSS.Unicode.Code_Point;
         Current_Info   :
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information;
         Current_Consumed : Boolean;
         Next_Offset    : VSS.Unicode.UTF8_Code_Unit_Offset;

         Current_Starter_Offset : VSS.Unicode.UTF8_Code_Unit_Offset;
         Current_Starter_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
         Current_Starter_Code   : VSS.Unicode.Code_Point;
         Current_Starter_Info   :
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information;

         New_Starter_Code   : VSS.Unicode.Code_Point;
         New_Starter_Buffer :
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array (0 .. 3);
         New_Starter_Size   : VSS.Unicode.UTF8_Code_Unit_Count;

      begin
         Previous_CCC := CCC_NR;
         Next_Offset  := From_Offset + From_Size;

         Current_Starter_Offset := From_Offset;
         Current_Starter_Size   := From_Size;
         Current_Starter_Code   := From_Code;
         Current_Starter_Info   := From_Info;

         loop
            exit when Next_Offset >= Result_Size;

            Current_Offset := Next_Offset;

            Unchecked_Decode_Forward
              (Result_Data, Next_Offset, Current_Code);

            Current_Info :=
              Get_Decomposition_Information
                (Decomposition_Data, Current_Code);

            Current_Size     := Next_Offset - Current_Offset;
            Current_Consumed := False;

            --  Current character may compose with the starter

            if Current_Starter_Info.First_Index /= 0
              and Current_Info.Last_Index /= 0
            then
               if Previous_CCC < Current_Info.CCC then
               --  Current character is not blocked from the starter and can
               --  be last character of the decomposition mapping of the
               --  some primary composite; thus lookup for primary composite.

                  New_Starter_Code :=
                    VSS.Implementation.UCD_Normalization_Common
                      .Composition_Mapping
                        (Current_Info.Last_Index,
                         Current_Starter_Info.First_Index);

                  if New_Starter_Code /= 0 then
                     --  Starter and current character is decomposition mapping
                     --  of the primary composite, remove current character and
                     --  replace starter by the primary composite found.

                     Unchecked_Delete
                       (Result_Data,
                        Result_Size,
                        Current_Offset,
                        Current_Size,
                        1);

                     VSS.Implementation.UTF8_Encoding.Encode
                       (New_Starter_Code,
                        New_Starter_Size,
                        New_Starter_Buffer (0),
                        New_Starter_Buffer (1),
                        New_Starter_Buffer (2),
                        New_Starter_Buffer (3));

                     Unchecked_Replace
                       (Result_Data,
                        Result_Size,
                        Current_Starter_Offset,
                        Current_Starter_Size,
                        1,
                        New_Starter_Buffer,
                        New_Starter_Buffer'First,
                        New_Starter_Size,
                        1);

                     Next_Offset :=
                       Next_Offset + New_Starter_Size
                         - Current_Starter_Size - Current_Size;

                     Current_Starter_Size := New_Starter_Size;
                     Current_Starter_Code := New_Starter_Code;
                     Current_Starter_Info :=
                       Get_Decomposition_Information
                         (Decomposition_Data, New_Starter_Code);

                     Current_Consumed := True;

                  else
                     Previous_CCC := Current_Info.CCC;
                  end if;

               elsif Previous_CCC = CCC_NR and Current_Info.CCC = CCC_NR then
                  --  Two starters

                  New_Starter_Code :=
                    VSS.Implementation.UCD_Normalization_Common
                      .Composition_Mapping
                        (Current_Info.Last_Index,
                         Current_Starter_Info.First_Index);

                  if New_Starter_Code /= 0 then
                     --  Starter and current character is decomposition mapping
                     --  of the primary composite, remove current character and
                     --  replace starter by the primary composite found.

                     Unchecked_Delete
                       (Result_Data,
                        Result_Size,
                        Current_Offset,
                        Current_Size,
                        1);

                     VSS.Implementation.UTF8_Encoding.Encode
                       (New_Starter_Code,
                        New_Starter_Size,
                        New_Starter_Buffer (0),
                        New_Starter_Buffer (1),
                        New_Starter_Buffer (2),
                        New_Starter_Buffer (3));

                     Unchecked_Replace
                       (Result_Data,
                        Result_Size,
                        Current_Starter_Offset,
                        Current_Starter_Size,
                        1,
                        New_Starter_Buffer,
                        New_Starter_Buffer'First,
                        New_Starter_Size,
                        1);

                     Next_Offset :=
                       Next_Offset + New_Starter_Size
                         - Current_Starter_Size - Current_Size;

                     Current_Starter_Size := New_Starter_Size;
                     Current_Starter_Code := New_Starter_Code;
                     Current_Starter_Info :=
                       Get_Decomposition_Information
                         (Decomposition_Data, New_Starter_Code);

                     Current_Consumed := True;
                  end if;
               end if;

            elsif Current_Code in 16#1161# .. 16#1175#
              and Current_Starter_Code in 16#1100# .. 16#1112#
            then
               --  Hangul Syllable Composition:
               --
               --  Leading consonant + Vowel => LV_Syllable

               declare
                  Starter_Buffer :
                    VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                      (0 .. 3);
                  L_Index        : VSS.Unicode.Code_Point;
                  V_Index        : VSS.Unicode.Code_Point;
                  LV_Index       : VSS.Unicode.Code_Point;

               begin
                  L_Index  := Current_Starter_Code - L_Base;
                  V_Index  := Current_Code - V_Base;
                  LV_Index := L_Index * N_Count + V_Index * T_Count;

                  Current_Starter_Code := S_Base + LV_Index;
                  Current_Starter_Info :=
                    Get_Decomposition_Information
                      (Decomposition_Data, Current_Starter_Code);

                  --  Encoded size of all possible characters are, same, so
                  --  reuse variable.

                  Unchecked_Delete
                    (Result_Data,
                     Result_Size,
                     Current_Offset,
                     Current_Size,
                     1);

                  VSS.Implementation.UTF8_Encoding.Encode
                    (Current_Starter_Code,
                     Current_Starter_Size,
                     Starter_Buffer (0),
                     Starter_Buffer (1),
                     Starter_Buffer (2),
                     Starter_Buffer (3));

                  Unchecked_Replace
                    (Result_Data,
                     Result_Size,
                     Current_Starter_Offset,
                     Current_Starter_Size,
                     1,
                     Starter_Buffer,
                     Starter_Buffer'First,
                     Current_Starter_Size,
                     1);

                  Next_Offset := Next_Offset - Current_Size;

                  Current_Consumed := True;
               end;

            elsif Current_Code in 16#11A8# .. 16#11C2#
              and Current_Starter_Code in 16#AC00# .. 16#D7A3#
            then
               --  Hangul Syllable Composition:
               --
               --  LV_Syllable + Trailing consonant => LVT_Syllable

               declare
                  Starter_Buffer :
                    VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                      (0 .. 3);
                  S_Index        : VSS.Unicode.Code_Point;
                  T_Index        : VSS.Unicode.Code_Point;

               begin
                  S_Index := Current_Starter_Code - S_Base;
                  T_Index := S_Index mod T_Count;

                  if T_Index = 0 then
                     --  Starter is LV_Syllable, can compose with
                     --  current T_Jamo.

                     T_Index  := Current_Code - T_Base;

                     Current_Starter_Code := Current_Starter_Code + T_Index;
                     Current_Starter_Info :=
                       Get_Decomposition_Information
                         (Decomposition_Data, Current_Starter_Code);

                     --  Encoded size of all possible characters are, same, so
                     --  reuse variable.

                     Unchecked_Delete
                       (Result_Data,
                        Result_Size,
                        Current_Offset,
                        Current_Size,
                        1);

                     VSS.Implementation.UTF8_Encoding.Encode
                       (Current_Starter_Code,
                        Current_Starter_Size,
                        Starter_Buffer (0),
                        Starter_Buffer (1),
                        Starter_Buffer (2),
                        Starter_Buffer (3));

                     Unchecked_Replace
                       (Result_Data,
                        Result_Size,
                        Current_Starter_Offset,
                        Current_Starter_Size,
                        1,
                        Starter_Buffer,
                        Starter_Buffer'First,
                        Current_Starter_Size,
                        1);

                     Next_Offset := Next_Offset - Current_Size;

                     Current_Consumed := True;
                  end if;
               end;
            end if;

            if not Current_Consumed then
               Previous_CCC := Current_Info.CCC;

               --  Current character is new starter

               if Current_Info.CCC = CCC_NR then
                  Current_Starter_Offset := Current_Offset;
                  Current_Starter_Size   := Current_Size;
                  Current_Starter_Code   := Current_Code;
                  Current_Starter_Info   := Current_Info;
               end if;
            end if;
         end loop;
      end Apply_Canonical_Composition;

      -------------------------
      -- Apply_Decomposition --
      -------------------------

      procedure Apply_Decomposition
        (Result_Data     : in out VSS.Implementation.Strings.String_Data;
         Result_Size     : in out VSS.Unicode.UTF8_Code_Unit_Count;
         From_Offset     : VSS.Unicode.UTF8_Code_Unit_Offset;
         From_Size       : VSS.Unicode.UTF8_Code_Unit_Count;
         From_Info       :
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information;
         Skip_Compositon : out Boolean;
         Starter_Offset  : out VSS.Unicode.UTF8_Code_Unit_Offset;
         Starter_Size    : out VSS.Unicode.UTF8_Code_Unit_Count;
         Starter_Code    : out VSS.Unicode.Code_Point;
         Starter_Info    : out
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information;
         Previous_CCC    : out
           VSS.Implementation.UCD_Normalization_UTF8.CCC_Values)
      is
         Current_Offset : VSS.Unicode.UTF8_Code_Unit_Offset := From_Offset;
         Current_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
         Current_Code   : VSS.Unicode.Code_Point;
         Current_Info   :
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information :=
             From_Info;
         Next_Offset    : VSS.Unicode.UTF8_Code_Unit_Offset :=
           From_Offset + From_Size;

      begin
         --  By convention, this subprogram called only when there is at least
         --  one character is available.

         Skip_Compositon := True;
         Previous_CCC    := CCC_NR;

         loop
            if Current_Info.Has_Starter then
               --  XXX By convention, when full decomposition has starter
               --  character(s) the first characters of the decomposition is
               --  starter too. This should be checked by code generator.

               if Has_Decomposition (Current_Info) then
                  Current_Size := Next_Offset - Current_Offset;

                  Unchecked_Replace
                    (Result_Data,
                     Result_Size,
                     Current_Offset,
                     Current_Size,
                     1,
                     VSS.Implementation.UCD_Normalization_UTF8.UTF8_Data_Table,
                     Current_Info.Offset,
                     Current_Info.Size,
                     Current_Info.Length);

                  Next_Offset  :=
                    Next_Offset + Current_Info.Size - Current_Size;
                  Previous_CCC := Current_Info.Last_CCC;

               else
                  raise Program_Error;
               end if;

            else
               --  Not a starter, do decompositon with canonical reordering.

               if Has_Decomposition (Current_Info) then
                  raise Program_Error;

               else
                  if Previous_CCC <= Current_Info.CCC then
                     --  No decomposition, canonical ordering preserved.

                     Previous_CCC := Current_Info.CCC;

                  else
                     --  Violation of canonical reordering, move character
                     --  into appropriate position.

                     declare
                        Previous_Offset : VSS.Unicode.UTF8_Code_Unit_Offset;
                        Previous_Code   : VSS.Unicode.Code_Point;
                        Previous_Info   :
                          VSS.Implementation.UCD_Normalization_UTF8
                            .Mapping_Information;
                        Insert_Offset   : VSS.Unicode.UTF8_Code_Unit_Offset;

                     begin
                        Current_Size := Next_Offset - Current_Offset;
                        Previous_Offset := Current_Offset;

                        loop
                           Insert_Offset := Previous_Offset;

                           Unchecked_Backward_Decode
                             (Result_Data, Previous_Offset, Previous_Code);

                           Previous_Info :=
                             Get_Decomposition_Information
                               (Decomposition_Data, Previous_Code);

                           exit when Previous_Info.CCC = CCC_NR
                             or Previous_Info.CCC <= Current_Info.CCC;
                        end loop;

                        Unchecked_Move_Slice
                          (Result_Data,
                           Current_Offset,
                           Current_Size,
                           Insert_Offset);
                     end;
                  end if;
               end if;
            end if;

            exit when Next_Offset >= Result_Size;

            Current_Offset := Next_Offset;

            Unchecked_Decode_Forward
              (Result_Data, Next_Offset, Current_Code);

            Current_Info :=
              Get_Decomposition_Information (Decomposition_Data, Current_Code);
         end loop;

         Next_Offset := From_Offset;

         Unchecked_Decode_Forward (Result_Data, Next_Offset, Current_Code);

         Current_Info :=
           Get_Decomposition_Information (Decomposition_Data, Current_Code);

         if Current_Info.CCC = CCC_NR then
            Skip_Compositon := Current_Info.First_Index = 0;
            Starter_Offset  := From_Offset;
            Starter_Size    := Next_Offset - From_Offset;
            Starter_Code    := Current_Code;
            Starter_Info    := Current_Info;
         end if;
      end Apply_Decomposition;

      --------------
      -- Backward --
      --------------

      function Backward
        (Data   : VSS.Implementation.Strings.String_Data;
         From   : VSS.Unicode.UTF8_Code_Unit_Offset;
         Offset : out VSS.Unicode.UTF8_Code_Unit_Offset;
         Size   : out VSS.Unicode.UTF8_Code_Unit_Count;
         Code   : out VSS.Unicode.Code_Point;
         Info   : out
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information)
         return Boolean is

      begin
         if From = 0 then
            return False;
         end if;

         Offset := From;

         Unchecked_Backward_Decode (Data, Offset, Code);

         Info := Get_Decomposition_Information (Decomposition_Data, Code);
         Size := From - Offset;

         return True;
      end Backward;

      -----------------------
      -- Has_Decomposition --
      -----------------------

      function Has_Decomposition
        (Info : VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information)
         return Boolean is
      begin
         return Info.Size /= 0;
      end Has_Decomposition;

      --------------------
      -- Lookup_Starter --
      --------------------

      procedure Lookup_Starter
        (Result_Data        : VSS.Implementation.Strings.String_Data;
         Result_Size        : VSS.Unicode.UTF8_Code_Unit_Offset;
         Need_Decomposition : out Boolean;
         Skip_Composition   : out Boolean;
         Starter_Found      : out Boolean;
         Starter_Offset     : out VSS.Unicode.UTF8_Code_Unit_Offset;
         Starter_Size       : out VSS.Unicode.UTF8_Code_Unit_Count;
         Starter_Code       : out VSS.Unicode.Code_Point;
         Starter_Info       : out
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information)
      is
         Current_Offset : VSS.Unicode.UTF8_Code_Unit_Offset := Result_Size;
         Current_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
         Current_Code   : VSS.Unicode.Code_Point;
         Current_Info   :
           VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information;

      begin
         Need_Decomposition := False;
         Skip_Composition   := True;
         Starter_Found      := False;

         if Result_Size /= 0 then
            loop
               exit when not Backward
                 (Result_Data,
                  Current_Offset,
                  Current_Offset,
                  Current_Size,
                  Current_Code,
                  Current_Info);

               Need_Decomposition :=
                 Need_Decomposition or Has_Decomposition (Current_Info);

               if Current_Info.Has_Starter then
                  Skip_Composition := Current_Info.First_Index = 0;
                  Starter_Found    := True;
                  Starter_Offset   := Current_Offset;
                  Starter_Size     := Current_Size;
                  Starter_Code     := Current_Code;
                  Starter_Info     := Current_Info;

                  exit;
               end if;
            end loop;
         end if;
      end Lookup_Starter;

      Result_Size           : VSS.Unicode.UTF8_Code_Unit_Count := 0;
      Source_Current_Offset : VSS.Unicode.UTF8_Code_Unit_Offset;
      Source_Next_Offset    : VSS.Unicode.UTF8_Code_Unit_Offset := 0;
      Source_Copy_Offset    : VSS.Unicode.UTF8_Code_Unit_Offset;
      Source_Length_Delta   : VSS.Implementation.Strings.Character_Count;
      Source_Code           : VSS.Unicode.Code_Point;
      Source_Info           :
        VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information;
      Last_CCC              :
        VSS.Implementation.UCD_Normalization_UTF8.CCC_Values :=
          CCC_NR;

   begin
      loop
         exit when Source_Next_Offset >= Source_Size;

         Source_Copy_Offset  := Source_Next_Offset;
         Source_Length_Delta := 0;

         --  Lookup source string for the longest slice already in
         --  normalization form.

         loop
            Source_Current_Offset := Source_Next_Offset;

            exit when Source_Next_Offset >= Source_Size;

            VSS.Implementation.UTF8_Encoding.Unchecked_Decode_Forward
              (Source_Storage, Source_Next_Offset, Source_Code);

            Source_Info :=
              Get_Decomposition_Information (Decomposition_Data, Source_Code);

            case Source_Info.Composition_QC is
               when No =>
                  --  Character that cannot ever occur in the respective
                  --  normalization form.

                  exit;

               when Maybe =>
                  --  Character that may occur in the respective
                  --  normalization form, depending on the context.

                  exit;

               when Yes =>
                  --  Character that may occur in the respective
                  --  normalization form.

                  if Last_CCC > Source_Info.CCC
                    and Source_Info.CCC /= CCC_NR
                  then
                     --  Violation of the canonical ordering.

                     exit;
                  end if;
            end case;

            Last_CCC := Source_Info.CCC;
            Source_Length_Delta := Source_Length_Delta + 1;
         end loop;

         --  Copy found normalized data

         if Source_Copy_Offset /= Source_Current_Offset then
            Unchecked_Append
              (Result_Data,
               Result_Size,
               Source_Storage,
               Source_Copy_Offset,
               Source_Current_Offset - Source_Copy_Offset,
               Source_Length_Delta);
         end if;

         exit when Source_Current_Offset >= Source_Size;

         case Source_Info.Composition_QC is
            when No =>
               --  Character that cannot ever occur in the respective
               --  normalization form.

               if Source_Info.Has_Starter then
                  pragma Assert (Source_Info.First_CCC = CCC_NR);
                  pragma Assert (Source_Info.Last_Index = 0);
                  pragma Assert (Has_Decomposition (Source_Info));

                  declare
                     Starter_Code     : VSS.Unicode.Code_Point;
                     Next_Offset      : VSS.Unicode.UTF8_Code_Unit_Offset;
                     Skip_Composition : Boolean;
                     Starter_Offset   : VSS.Unicode.UTF8_Code_Unit_Offset;
                     Starter_Size     : VSS.Unicode.UTF8_Code_Unit_Count;
                     Starter_Info     :
                       VSS.Implementation.UCD_Normalization_UTF8
                         .Mapping_Information;

                  begin
                     Starter_Offset := Result_Size;

                     Unchecked_Append
                       (Result_Data,
                        Result_Size,
                        VSS.Implementation.UCD_Normalization_UTF8
                          .UTF8_Data_Table,
                        Source_Info.Offset,
                        Source_Info.Size,
                        Source_Info.Length);
                     Last_CCC := Source_Info.Last_CCC;

                     --  If last starter in full decomposition can be first
                     --  character of the pair of canonical decomposition of
                     --  the primary composite - process all following
                     --  characters till next starter/end of string and do
                     --  composition.
                     --
                     --  XXX this is not implemented yet

                     --  XXX There are 2 bits available in Info record, can it
                     --  be used to encode length of the starter in the full
                     --  decomposition?

                     Next_Offset := Starter_Offset;

                     Unchecked_Decode_Forward
                       (Result_Data, Next_Offset, Starter_Code);

                     Starter_Size := Next_Offset - Starter_Offset;
                     Starter_Info :=
                       Get_Decomposition_Information
                         (Decomposition_Data, Starter_Code);
                     Skip_Composition   := False;

                     pragma Assert (Starter_Info.Last_Index = 0);

                     if not Skip_Composition then
                        loop
                           exit when Source_Next_Offset >= Source_Size;

                           Source_Current_Offset := Source_Next_Offset;

                           VSS.Implementation.UTF8_Encoding
                             .Unchecked_Decode_Forward
                               (Source_Storage,
                                Source_Next_Offset,
                                Source_Code);

                           Source_Info :=
                             Get_Decomposition_Information
                               (Decomposition_Data, Source_Code);

                           --  Append decompositions of all characters from
                           --  the source string till next starter.

                           if Source_Info.Has_Starter then
                              --  It is tested previously, and need to be
                              --  analyzed: first character in the full
                              --  decomposition should be starter by our
                              --  convention, but it may composite with the
                              --  previous starter. NF * C_QC may be used
                              --  here to do check, or Last_Index /= 0...

                              raise Program_Error;
                           end if;

                           if Has_Decomposition (Source_Info) then
                              raise Program_Error;
               --           if Source_Info.Has_Starter then
               --              raise Program_Error;
               --
               --           else
               --              if Last_CCC <= Source_Info.First_CCC then
               --                 Unchecked_Append
               --                   (Result_Data,
               --                    Result_Size,
               --                    VSS.Implementation.UCD_Normalization_UTF8
               --                      .UTF8_Data_Table,
               --                    Source_Info.Offset,
               --                    Source_Info.Size,
               --                    Source_Info.Length);
               --                 Last_CCC := Source_Info.Last_CCC;
               --
               --              else
               --                 raise Program_Error;
               --              end if;
               --
               --           end if;

                           else
               --           if Source_Info.CCC = CCC_NR then
               --              raise Program_Error;
               --
               --           else
                              if Last_CCC <= Source_Info.CCC then
                                 Unchecked_Append
                                   (Result_Data,
                                    Result_Size,
                                    Source_Storage,
                                    Source_Current_Offset,
                                    Source_Next_Offset - Source_Current_Offset,
                                    1);
                                 Last_CCC := Source_Info.CCC;

                              else
                                 raise Program_Error;

                                 --  XXX Only single character is processed
                                 --  here, thus Append_Reordered do
                                 --  unnecessary action, because it can
                                 --  process sequence of characters. So, it
                                 --  is possible to optimize code here by
                                 --  adding another subprogram to process
                                 --  single character only.

                                 --  Append_Reordered
                                 --    (Result_Data,
                                 --     Result_Size,
                                 --     Decomposition_Data,
                                 --     Last_CCC,
                                 --     Source_Storage,
                                 --     Source_Current_Offset,
                                 --     Source_Next_Offset
                                 --       - Source_Current_Offset);
                              end if;
                           end if;
                        end loop;

                        --  Do canonical composition

                        Apply_Canonical_Composition
                          (Result_Data,
                           Result_Size,
                           Starter_Offset,
                           Starter_Size,
                           Starter_Code,
                           Starter_Info);
                     end if;

                  end;

               else
                  --  XXX Duplicated with Maybe/non-starter case?

                  declare
                     Need_Decomposition : Boolean;
                     Skip_Composition   : Boolean;
                     Starter_Found      : Boolean;
                     Starter_Offset     : VSS.Unicode.UTF8_Code_Unit_Offset;
                     Starter_Size       : VSS.Unicode.UTF8_Code_Unit_Count;
                     Starter_Code       : VSS.Unicode.Code_Point;
                     Starter_Info       :
                       VSS.Implementation.UCD_Normalization_UTF8
                         .Mapping_Information;

                  begin
                     --  Lookup backward till starter character will be found

                     Lookup_Starter
                       (Result_Data,
                        Result_Size,
                        Need_Decomposition,
                        Skip_Composition,
                        Starter_Found,
                        Starter_Offset,
                        Starter_Size,
                        Starter_Code,
                        Starter_Info);

                     if Need_Decomposition then
                        declare
                           Aux_Starter_Info : constant
                             VSS.Implementation.UCD_Normalization_UTF8
                               .Mapping_Information := Starter_Info;
                           --  Workaround for GNAT warning about overlapped
                           --  parameters.

                        begin
                           Apply_Decomposition
                             (Result_Data,
                              Result_Size,
                              Starter_Offset,
                              Starter_Size,
                              Aux_Starter_Info,
                              Skip_Composition,
                              Starter_Offset,
                              Starter_Size,
                              Starter_Code,
                              Starter_Info,
                              Last_CCC);
                        end;
                     end if;

                     --  Append decompositions of all characters from the
                     --  source string till next starter.

                     loop
                        if Has_Decomposition (Source_Info) then
                           if Starter_Info.First_CCC = CCC_NR then
                              Unchecked_Append
                                (Result_Data,
                                 Result_Size,
                                 VSS.Implementation.UCD_Normalization_UTF8
                                   .UTF8_Data_Table,
                                 Source_Info.Offset,
                                 Source_Info.Size,
                                 Source_Info.Length);
                              Last_CCC := Source_Info.Last_CCC;

                              exit;

                           elsif Last_CCC <= Source_Info.First_CCC then
                              Unchecked_Append
                                (Result_Data,
                                 Result_Size,
                                 VSS.Implementation.UCD_Normalization_UTF8
                                   .UTF8_Data_Table,
                                 Source_Info.Offset,
                                 Source_Info.Size,
                                 Source_Info.Length);
                              Last_CCC := Source_Info.Last_CCC;

                           else
                              raise Program_Error;
                           end if;

                        else
                           if Source_Info.CCC = CCC_NR then
                              raise Program_Error;

                           elsif Last_CCC <= Source_Info.CCC then
                              Unchecked_Append
                                (Result_Data,
                                 Result_Size,
                                 Source_Storage,
                                 Source_Current_Offset,
                                 Source_Next_Offset - Source_Current_Offset,
                                 1);
                              Last_CCC := Source_Info.CCC;

                           else
                              --  XXX Only single character is processed here,
                              --  thus Append_Reordered do unnecessary action,
                              --  because it can process sequence of
                              --  characters. So, it is possible to optimize
                              --  code here by adding another subprogram to
                              --  process single character only.

                              Append_Reordered
                                (Result_Data,
                                 Result_Size,
                                 Decomposition_Data,
                                 Last_CCC,
                                 Source_Storage,
                                 Source_Current_Offset,
                                 Source_Next_Offset - Source_Current_Offset);
                           end if;
                        end if;

                        exit when Source_Next_Offset >= Source_Size;

                        Source_Current_Offset := Source_Next_Offset;

                        VSS.Implementation.UTF8_Encoding
                          .Unchecked_Decode_Forward
                            (Source_Storage, Source_Next_Offset, Source_Code);

                        Source_Info :=
                          Get_Decomposition_Information
                            (Decomposition_Data, Source_Code);
                     end loop;

                     --  Do canonical composition when necessary

                     if not Skip_Composition then
                        Apply_Canonical_Composition
                          (Result_Data,
                           Result_Size,
                           Starter_Offset,
                           Starter_Size,
                           Starter_Code,
                           Starter_Info);
                     end if;
                  end;
               end if;

            when Maybe =>
               --  Character that may occur in the respective normalization
               --  form, depending on the context.

               if Source_Info.Has_Starter then
                  if Has_Decomposition (Source_Info) then
                     raise Program_Error;

                  elsif Source_Code in 16#1161# .. 16#1175# then
                     pragma Assert (Source_Info.Last_Index = 0);
                     --  Algorithmic composition, character must not compose
                     --  with previous character in ordinary way.

                     declare
                        Starter_Code   : VSS.Unicode.Code_Point;
                        Starter_Offset : VSS.Unicode.UTF8_Code_Unit_Offset;
                        Starter_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
                        Starter_Buffer :
                          VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                            (0 .. 3);
                        L_Index        : VSS.Unicode.Code_Point;
                        V_Index        : VSS.Unicode.Code_Point;
                        LV_Index       : VSS.Unicode.Code_Point;
                        Append_Source  : Boolean := True;

                     begin
                        if Result_Size /= 0 then
                           Starter_Offset := Result_Size;

                           Unchecked_Backward_Decode
                             (Result_Data, Starter_Offset, Starter_Code);

                           if Starter_Code in 16#1100# .. 16#1112# then
                              L_Index  := Starter_Code - L_Base;
                              V_Index  := Source_Code - V_Base;
                              LV_Index :=
                                L_Index * N_Count + V_Index * T_Count;

                              Starter_Code := S_Base + LV_Index;

                              VSS.Implementation.UTF8_Encoding.Encode
                                (Starter_Code,
                                 Starter_Size,
                                 Starter_Buffer (0),
                                 Starter_Buffer (1),
                                 Starter_Buffer (2),
                                 Starter_Buffer (3));

                              --  Encoded size of all possible characters are,
                              --  same, so use it.

                              Unchecked_Replace
                                (Result_Data,
                                 Result_Size,
                                 Starter_Offset,
                                 Starter_Size,
                                 1,
                                 Starter_Buffer,
                                 Starter_Buffer'First,
                                 Starter_Size,
                                 1);

                              Append_Source := False;
                           end if;
                        end if;

                        if Append_Source then
                           --  Composition is impossible, append character
                           --  to the result and reset canonical combining
                           --  class of the last character.

                           Unchecked_Append
                             (Result_Data,
                              Result_Size,
                              Source_Storage,
                              Source_Current_Offset,
                              Source_Next_Offset - Source_Current_Offset,
                              1);

                           Last_CCC := CCC_NR;
                        end if;
                     end;

                  elsif Source_Code in 16#11A8# .. 16#11C2# then
                     pragma Assert (Source_Info.Last_Index = 0);
                     --  Algorithmic composition, character must not compose
                     --  with previous character in ordinary way.

                     declare
                        Starter_Code   : VSS.Unicode.Code_Point;
                        Starter_Offset : VSS.Unicode.UTF8_Code_Unit_Offset;
                        Starter_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
                        Starter_Buffer :
                          VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                            (0 .. 3);
                        S_Index        : VSS.Unicode.Code_Point;
                        T_Index        : VSS.Unicode.Code_Point;
                        Append_Source  : Boolean := True;

                     begin
                        if Result_Size /= 0 then
                           Starter_Offset := Result_Size;

                           Unchecked_Backward_Decode
                             (Result_Data, Starter_Offset, Starter_Code);

                           if Starter_Code in 16#AC00# .. 16#D7A3# then
                              S_Index := Starter_Code - S_Base;
                              T_Index := S_Index mod T_Count;

                              if T_Index = 0 then
                                 --  Starter is LV_Syllable, can compose with
                                 --  current T_Jamo.

                                 T_Index  := Source_Code - T_Base;

                                 Starter_Code := Starter_Code + T_Index;

                                 VSS.Implementation.UTF8_Encoding.Encode
                                   (Starter_Code,
                                    Starter_Size,
                                    Starter_Buffer (0),
                                    Starter_Buffer (1),
                                    Starter_Buffer (2),
                                    Starter_Buffer (3));

                                 --  Encoded size of all possible characters
                                 --  are same, so use it.

                                 Unchecked_Replace
                                   (Result_Data,
                                    Result_Size,
                                    Starter_Offset,
                                    Starter_Size,
                                    1,
                                    Starter_Buffer,
                                    Starter_Buffer'First,
                                    Starter_Size,
                                    1);

                                 Append_Source := False;
                              end if;
                           end if;
                        end if;

                        if Append_Source then
                           --  Composition is impossible, append character
                           --  to the result and reset canonical combining
                           --  class of the last character.

                           Unchecked_Append
                             (Result_Data,
                              Result_Size,
                              Source_Storage,
                              Source_Current_Offset,
                              Source_Next_Offset - Source_Current_Offset,
                              1);

                           Last_CCC := CCC_NR;
                        end if;
                     end;

                  else
                     pragma Assert (Source_Info.Last_Index /= 0);
                     --  Character may compose with previous character.

                     declare
                        Starter_Offset : VSS.Unicode.UTF8_Code_Unit_Offset;
                        Starter_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
                        Starter_Code   : VSS.Unicode.Code_Point;
                        Starter_Info   :
                          VSS.Implementation.UCD_Normalization_UTF8
                            .Mapping_Information;

                     begin
                        Starter_Offset := Result_Size;

                        Unchecked_Backward_Decode
                          (Result_Data, Starter_Offset, Starter_Code);

                        Starter_Info :=
                          Get_Decomposition_Information
                            (Decomposition_Data, Starter_Code);

                        if Starter_Info.Has_Starter then
                           --  XXX Don't need to decomposite previous
                           --  character ???
                           --  if Has_Decomposition (Starter_Info) then
                           --     raise Program_Error;
                           --
                           --  else
                           if Starter_Info.First_Index /= 0 then
                              Starter_Size := Result_Size - Starter_Offset;

                              Unchecked_Append
                                (Result_Data,
                                 Result_Size,
                                 Source_Storage,
                                 Source_Current_Offset,
                                 Source_Next_Offset - Source_Current_Offset,
                                 1);

                              Apply_Canonical_Composition
                                (Result_Data,
                                 Result_Size,
                                 Starter_Offset,
                                 Starter_Size,
                                 Starter_Code,
                                 Starter_Info);
                           end if;

                        else
                           Unchecked_Append
                             (Result_Data,
                              Result_Size,
                              Source_Storage,
                              Source_Current_Offset,
                              Source_Next_Offset - Source_Current_Offset,
                              1);

                           Last_CCC := CCC_NR;
                        end if;
                     end;
                  end if;

               else
                  declare
                     Need_Decomposition : Boolean;
                     Skip_Composition   : Boolean;
                     Starter_Found      : Boolean;
                     Starter_Offset     : VSS.Unicode.UTF8_Code_Unit_Offset;
                     Starter_Size       : VSS.Unicode.UTF8_Code_Unit_Count;
                     Starter_Code       : VSS.Unicode.Code_Point;
                     Starter_Info       :
                       VSS.Implementation.UCD_Normalization_UTF8
                         .Mapping_Information;

                  begin
                     --  Lookup backward till starter character will be found

                     Lookup_Starter
                       (Result_Data,
                        Result_Size,
                        Need_Decomposition,
                        Skip_Composition,
                        Starter_Found,
                        Starter_Offset,
                        Starter_Size,
                        Starter_Code,
                        Starter_Info);

                     if Need_Decomposition then
                        declare
                           Aux_Starter_Info : constant
                             VSS.Implementation.UCD_Normalization_UTF8
                               .Mapping_Information := Starter_Info;
                           --  Workaround of GNAT warning about overlapping
                           --  parameters.

                        begin
                           Apply_Decomposition
                             (Result_Data,
                              Result_Size,
                              Starter_Offset,
                              Starter_Size,
                              Aux_Starter_Info,
                              Skip_Composition,
                              Starter_Offset,
                              Starter_Size,
                              Starter_Code,
                              Starter_Info,
                              Last_CCC);
                        end;
                     end if;

                     --  Append decompositions of all characters from the
                     --  source string till next starter.

                     loop
                        --  if Source_Info.Has_Starter then
                        --  It is tested previously, and need to be
                        --  analyzed: first character in the full decomposition
                        --  should be starter by our convention, but it may
                        --  composite with the previous starter. NF*C_QC may
                        --  be used here to do check, or Last_Index /= 0...

                        --     raise Program_Error;
                        --  end if;

                        if Has_Decomposition (Source_Info) then
                           if Source_Info.First_CCC = CCC_NR then
                              raise Program_Error;

                           elsif Last_CCC <= Source_Info.First_CCC then
                              raise Program_Error;

                           else
                              Append_Reordered
                                (Result_Data,
                                 Result_Size,
                                 Decomposition_Data,
                                 Last_CCC,
                                 VSS.Implementation.UCD_Normalization_UTF8
                                   .UTF8_Data_Table,
                                 Source_Info.Offset,
                                 Source_Info.Size);
                           end if;

                        else
                           if Source_Info.CCC = CCC_NR then
                              Unchecked_Append
                                (Result_Data,
                                 Result_Size,
                                 Source_Storage,
                                 Source_Current_Offset,
                                 Source_Next_Offset - Source_Current_Offset,
                                 1);
                              Last_CCC := Source_Info.CCC;

                              exit;

                           elsif Last_CCC <= Source_Info.CCC then
                              Unchecked_Append
                                (Result_Data,
                                 Result_Size,
                                 Source_Storage,
                                 Source_Current_Offset,
                                 Source_Next_Offset - Source_Current_Offset,
                                 1);
                              Last_CCC := Source_Info.CCC;

                           else
                              --  XXX Only single character is processed here,
                              --  thus Append_Reordered do unnecessary action,
                              --  because it can process sequence of
                              --  characters. So, it is possible to optimize
                              --  code here by adding another subprogram to
                              --  process single character only.

                              Append_Reordered
                                (Result_Data,
                                 Result_Size,
                                 Decomposition_Data,
                                 Last_CCC,
                                 Source_Storage,
                                 Source_Current_Offset,
                                 Source_Next_Offset - Source_Current_Offset);
                           end if;
                        end if;

                        exit when Source_Next_Offset >= Source_Size;

                        Source_Current_Offset := Source_Next_Offset;

                        VSS.Implementation.UTF8_Encoding
                          .Unchecked_Decode_Forward
                            (Source_Storage, Source_Next_Offset, Source_Code);

                        Source_Info :=
                          Get_Decomposition_Information
                            (Decomposition_Data, Source_Code);
                     end loop;

                     --  Do canonical composition when necessary

                     if not Skip_Composition then
                        Apply_Canonical_Composition
                          (Result_Data,
                           Result_Size,
                           Starter_Offset,
                           Starter_Size,
                           Starter_Code,
                           Starter_Info);
                     end if;
                  end;
               end if;

            when Yes =>
               if Last_CCC <= Source_Info.CCC and Source_Info.CCC = CCC_NR then
                  --  Precondition, should be removed.

                  raise Program_Error;
               end if;

               --  XXX Only single character is appended here! May be
               --  optimized?

               Append_Reordered
                 (Result_Data,
                  Result_Size,
                  Decomposition_Data,
                  Last_CCC,
                  Source_Storage,
                  Source_Current_Offset,
                  Source_Next_Offset - Source_Current_Offset);
         end case;
      end loop;
   end Decompose_And_Compose;

   -----------------------------------
   -- Get_Decomposition_Information --
   -----------------------------------

   function Get_Decomposition_Information
     (Decomposition_Data :
        VSS.Implementation.UCD_Normalization_UTF8.Mapping_Data_Offset_Array;
      Code               : VSS.Unicode.Code_Point)
      return VSS.Implementation.UCD_Normalization_UTF8.Mapping_Information
   is
      use type VSS.Implementation.UCD_Normalization_UTF8.Mapping_Data_Offset;
      use type VSS.Unicode.Code_Point;

      Group  :
        constant VSS.Implementation.UCD_Normalization_UTF8.Mapping_Group :=
          VSS.Implementation.UCD_Normalization_UTF8.Mapping_Group
            (Code
               / VSS.Implementation.UCD_Normalization_UTF8.Mapping_Group_Size);
      Offset : constant
        VSS.Implementation.UCD_Normalization_UTF8.Mapping_Data_Offset :=
          VSS.Implementation.UCD_Normalization_UTF8.Mapping_Data_Offset
            (Code
             mod VSS.Implementation.UCD_Normalization_UTF8.Mapping_Group_Size);

   begin
      return
        VSS.Implementation.UCD_Normalization_UTF8.Mapping_Data_Table
          (Decomposition_Data (Group) + Offset);
   end Get_Decomposition_Information;

   ----------------------
   -- Unchecked_Append --
   ----------------------

   procedure Unchecked_Append
     (Target_Data : in out VSS.Implementation.Strings.String_Data;
      Target_Size : out VSS.Unicode.UTF8_Code_Unit_Count;
      Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From        : VSS.Unicode.UTF8_Code_Unit_Index;
      Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      Length      : VSS.Implementation.Strings.Character_Count;
      Terminator  : Boolean := False)
   is
      use type VSS.Implementation.UTF8_String_Handlers.UTF8_String_Data_Access;

   begin
      if Target_Data.In_Place then
         declare
            Target : VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_Data
              with Import, Convention => Ada, Address => Target_Data'Address;

         begin
            if Target.Size + Size
              <= VSS.Implementation.UTF8_String_Handlers
                   .In_Place_Storage_Capacity
            then
               Target.Storage (Target.Size .. Target.Size + Size - 1) :=
                 Storage (From .. From + Size - 1);
               Target.Size   := Target.Size + Size;
               Target.Length := Target.Length + Length;

               if Terminator then
                  Target.Storage (Target.Size) := 16#00#;
               end if;

               Target_Size   := Target.Size;

               return;

            else
               VSS.Implementation.UTF8_String_Handlers.Copy_To_Heap
                 (Target_Data, 0, Target.Size + Size);
            end if;
         end;
      end if;

      declare
         Target :
           VSS.Implementation.UTF8_String_Handlers.UTF8_String_Data_Access
             with Import, Convention => Ada,
                  Address => Target_Data.Pointer'Address;

      begin
         if Target = null then
            Target :=
              VSS.Implementation.UTF8_String_Handlers.Allocate (0, Size);

         elsif Target.Size + Size > Target.Bulk then
            VSS.Implementation.UTF8_String_Handlers.Reallocate
              (Target, 0, Target.Size + Size);
         end if;

         Target.Storage (Target.Size .. Target.Size + Size - 1) :=
           Storage (From .. From + Size - 1);
         Target.Size   := Target.Size + Size;
         Target.Length := Target.Length + Length;

         if Terminator then
            Target.Storage (Target.Size) := 16#00#;
         end if;

         Target_Size   := Target.Size;
      end;
   end Unchecked_Append;

   -------------------------------
   -- Unchecked_Backward_Decode --
   -------------------------------

   procedure Unchecked_Backward_Decode
     (Source_Data : VSS.Implementation.Strings.String_Data;
      Offset      : in out VSS.Unicode.UTF8_Code_Unit_Index;
      Code        : out VSS.Unicode.Code_Point) is
   begin
      if Source_Data.In_Place then
         declare
            Source : VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_Data
              with Import, Convention => Ada, Address => Source_Data'Address;

         begin
            Unchecked_Backward_Decode (Source.Storage, Offset, Code);
         end;

      else
         declare
            Source :
              VSS.Implementation.UTF8_String_Handlers.UTF8_String_Data_Access
                with Import, Convention => Ada,
                     Address => Source_Data.Pointer'Address;

         begin
            Unchecked_Backward_Decode (Source.Storage, Offset, Code);
         end;
      end if;
   end Unchecked_Backward_Decode;

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

   ------------------------------
   -- Unchecked_Decode_Forward --
   ------------------------------

   procedure Unchecked_Decode_Forward
     (Source_Data : VSS.Implementation.Strings.String_Data;
      Offset      : in out VSS.Unicode.UTF8_Code_Unit_Index;
      Code        : out VSS.Unicode.Code_Point) is
   begin
      if Source_Data.In_Place then
         declare
            Source : VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_Data
              with Import, Convention => Ada, Address => Source_Data'Address;

         begin
            VSS.Implementation.UTF8_Encoding.Unchecked_Decode_Forward
              (Source.Storage, Offset, Code);
         end;

      else
         declare
            Source :
              VSS.Implementation.UTF8_String_Handlers.UTF8_String_Data_Access
                with Import, Convention => Ada,
                     Address => Source_Data.Pointer'Address;

         begin
            VSS.Implementation.UTF8_Encoding.Unchecked_Decode_Forward
              (Source.Storage, Offset, Code);
         end;
      end if;
   end Unchecked_Decode_Forward;

   ----------------------
   -- Unchecked_Delete --
   ----------------------

   procedure Unchecked_Delete
     (Target_Data   : in out VSS.Implementation.Strings.String_Data;
      Target_Size   : out VSS.Unicode.UTF8_Code_Unit_Count;
      Delete_From   : VSS.Unicode.UTF8_Code_Unit_Index;
      Delete_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
      Delete_Length : VSS.Implementation.Strings.Character_Count) is
   begin
      if Target_Data.In_Place then
         declare
            Target : VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_Data
              with Import, Convention => Ada, Address => Target_Data'Address;

         begin
            if Delete_Size = 0 then
               Target_Size := Target.Size;

            elsif Delete_From + Delete_Size = Target.Size then
               --  End of string is deleted, no data moved, update size and
               --  length only.

               Target.Size   := Target.Size - Delete_Size;
               Target.Length := Target.Length - Delete_Length;

               Target_Size   := Target.Size;

            else
               Target.Storage
                 (Delete_From .. Target.Size - Delete_Size - 1) :=
                   Target.Storage
                     (Delete_From + Delete_Size .. Target.Size - 1);

               Target.Size   := Target.Size - Delete_Size;
               Target.Length := Target.Length - Delete_Length;

               Target_Size   := Target.Size;
            end if;
         end;

      else
         declare
            Target :
              VSS.Implementation.UTF8_String_Handlers.UTF8_String_Data_Access
                with Import, Convention => Ada,
                     Address => Target_Data.Pointer'Address;

         begin
            if Delete_Size = 0 then
               Target_Size := Target.Size;

            elsif Delete_From + Delete_Size = Target.Size then
               --  End of string is deleted, no data moved, update size and
               --  length only.

               Target.Size   := Target.Size - Delete_Size;
               Target.Length := Target.Length - Delete_Length;

               Target_Size   := Target.Size;

            else
               Target.Storage
                 (Delete_From .. Target.Size - Delete_Size - 1) :=
                   Target.Storage
                     (Delete_From + Delete_Size .. Target.Size - 1);

               Target.Size   := Target.Size - Delete_Size;
               Target.Length := Target.Length - Delete_Length;

               Target_Size   := Target.Size;
            end if;
         end;
      end if;
   end Unchecked_Delete;

   ----------------------
   -- Unchecked_Insert --
   ----------------------

   procedure Unchecked_Insert
     (Target_Data : in out VSS.Implementation.Strings.String_Data;
      Target_Size : out VSS.Unicode.UTF8_Code_Unit_Count;
      Into        : VSS.Unicode.UTF8_Code_Unit_Index;
      Storage     : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From        : VSS.Unicode.UTF8_Code_Unit_Index;
      Size        : VSS.Unicode.UTF8_Code_Unit_Count;
      Length      : VSS.Implementation.Strings.Character_Count)
   is
      use type VSS.Implementation.UTF8_String_Handlers.UTF8_String_Data_Access;

   begin
      if Target_Data.In_Place then
         declare
            Target : VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_Data
              with Import, Convention => Ada, Address => Target_Data'Address;

         begin
            if Target.Size + Size
              <= VSS.Implementation.UTF8_String_Handlers
                   .In_Place_Storage_Capacity
            then
               Target.Storage (Into + Size .. Target.Size + Size - 1) :=
                 Target.Storage (Into .. Target.Size - 1);
               Target.Storage (Into .. Into + Size - 1) :=
                 Storage (From .. From + Size - 1);
               Target.Size   := Target.Size + Size;
               Target.Length := Target.Length + Length;

               Target_Size   := Target.Size;

            else
               raise Program_Error;
            end if;
         end;

      else
         declare
            Target :
              VSS.Implementation.UTF8_String_Handlers.UTF8_String_Data_Access
                with Import, Convention => Ada,
                     Address => Target_Data.Pointer'Address;

         begin
            if Target = null then
               Target :=
                 VSS.Implementation.UTF8_String_Handlers.Allocate (0, Size);

            elsif Target.Size + Size > Target.Bulk then
               --  Reallocate (Target, 0, Target.Size + Size);
               raise Program_Error;
            end if;

            raise Program_Error;
            --  Target.Storage (Target.Size .. Target.Size + Size - 1) :=
            --    Storage (From .. From + Size - 1);
            --  Target.Size := Target.Size + Size;
            --  Target.Length := Target.Length + Length;
         end;
      end if;
   end Unchecked_Insert;

   --------------------------
   -- Unchecked_Move_Slice --
   --------------------------

   procedure Unchecked_Move_Slice
     (Data : in out VSS.Implementation.Strings.String_Data;
      From : VSS.Unicode.UTF8_Code_Unit_Index;
      Size : VSS.Unicode.UTF8_Code_Unit_Count;
      Into : VSS.Unicode.UTF8_Code_Unit_Index) is
   begin
      if From = Into then
         return;
      end if;

      if Data.In_Place then
         if From < Into then
            raise Program_Error;
         end if;

         declare
            Target    :
              VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_Data
                with Import, Convention => Ada, Address => Data'Address;
            Buffer    : constant
              VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
                (0 .. Size - 1) := Target.Storage (From .. From + Size - 1);
            Move_Size : constant VSS.Unicode.UTF8_Code_Unit_Offset :=
              From - Into;

         begin
            Target.Storage (Into + Size .. Into + Size + Move_Size - 1) :=
              Target.Storage (Into .. Into + Move_Size - 1);
            Target.Storage (Into .. Into + Size - 1) := Buffer;
         end;

      else
         raise Program_Error;
      end if;
   end Unchecked_Move_Slice;

   -----------------------
   -- Unchecked_Replace --
   -----------------------

   procedure Unchecked_Replace
     (Target_Data    : in out VSS.Implementation.Strings.String_Data;
      Target_Size    : out VSS.Unicode.UTF8_Code_Unit_Count;
      Replace_From   : VSS.Unicode.UTF8_Code_Unit_Index;
      Replace_Size   : VSS.Unicode.UTF8_Code_Unit_Count;
      Replace_Length : VSS.Implementation.Strings.Character_Count;
      Storage        : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Insert_From    : VSS.Unicode.UTF8_Code_Unit_Index;
      Insert_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Insert_Length  : VSS.Implementation.Strings.Character_Count) is
   begin
      if Target_Data.In_Place then
         declare
            Target : VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_Data
              with Import, Convention => Ada, Address => Target_Data'Address;

         begin
            if Target.Size - Replace_Size + Insert_Size
              <= VSS.Implementation.UTF8_String_Handlers
                   .In_Place_Storage_Capacity
            then
               if Replace_From + Replace_Size = Target.Size then
                  --  Replace of string suffix, just overwrite it
                  --  XXX May be check for overwrite????

                  Target.Storage
                    (Replace_From .. Replace_From + Insert_Size - 1) :=
                    Storage (Insert_From .. Insert_From + Insert_Size - 1);

                  Target.Size := Target.Size + Insert_Size - Replace_Size;
                  Target.Length :=
                    Target.Length + Insert_Length - Replace_Length;

                  Target_Size := Target.Size;

               else
                  Target.Storage
                    (Replace_From + Insert_Size
                       .. Target.Size - Replace_Size + Insert_Size - 1) :=
                      Target.Storage
                        (Replace_From + Replace_Size .. Target.Size - 1);

                  Target.Storage
                    (Replace_From .. Replace_From + Insert_Size - 1) :=
                    Storage (Insert_From .. Insert_From + Insert_Size - 1);

                  Target.Size := Target.Size + Insert_Size - Replace_Size;
                  Target.Length :=
                    Target.Length + Insert_Length - Replace_Length;

                  Target_Size := Target.Size;
               end if;

            else
               raise Program_Error;
            end if;
         end;

      else
         declare
            Target :
              VSS.Implementation.UTF8_String_Handlers.UTF8_String_Data_Access
                with Import, Convention => Ada,
                     Address => Target_Data.Pointer'Address;

         begin
            if Replace_Size = Insert_Size then
               Target.Storage
                 (Replace_From .. Replace_From + Insert_Size - 1) :=
                    Storage (Insert_From .. Insert_From + Insert_Size - 1);

               Target.Length := Target.Length + Insert_Length - Replace_Length;

               Target_Size := Target.Size;

            else
               raise Program_Error;
            end if;
         end;
      end if;
   end Unchecked_Replace;

end VSS.Implementation.UTF8_Normalization;
