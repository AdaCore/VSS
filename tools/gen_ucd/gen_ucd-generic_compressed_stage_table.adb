--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

package body Gen_UCD.Generic_Compressed_Stage_Table is

   type Compressed_Array is array (Gen_UCD.Unsigned_32 range <>) of Data_Type;

   type Compressed_Array_Access is access all Compressed_Array;

   Result_Data       : Compressed_Array_Access;
   Result_Data_Last  : Gen_UCD.Unsigned_32;

   function Is_Equal
     (Left : Compressed_Array; Right : Compressed_Array) return Boolean;
   --  Compare two arrays. Default implementation doesn't work when Data_Type
   --  is Unchecked_Union type.

   -----------
   -- Build --
   -----------

   procedure Build
     (Self : in out Compressed_Stage_Table'Class; Data : Data_Type_Array)
   is
      use type UCD.Code_Point;

      Initial : Unsigned_32;
      Reused  : Boolean;

   begin
      Self.Group_Data :=
        new Group_Array
          (0 .. (Unsigned_32 (UCD.Code_Point'Last) + 1) / Group_Size - 1);

      if Result_Data = null then
         --  Allocate memory.

         Result_Data :=
           new Compressed_Array (0 .. Unsigned_32 (UCD.Code_Point'Last));

         --  Copy first block

         Result_Data (0 .. Group_Size - 1) :=
           Compressed_Array (Data (0 .. Group_Size - 1));
         Result_Data_Last := Group_Size - 1;
         Self.Group_Data (0) := 0;
         Initial := 1;

      else
         Initial := 0;
      end if;

      --  Process all other blocks

      for Group in Initial .. Self.Group_Data'Last loop
         declare
            Source : Compressed_Array renames
              Compressed_Array
                (Data (UCD.Code_Point (Group * Group_Size)
                   .. UCD.Code_Point ((Group + 1) * Group_Size - 1)));

         begin
            Reused := False;

            for Offset in 0 .. Result_Data_Last - Group_Size + 1 loop
               if Is_Equal
                    (Result_Data (Offset .. Offset + Group_Size - 1), Source)
               then
                  Self.Group_Data (Group) := Offset;
                  Reused := True;

                  exit;
               end if;
            end loop;

            if not Reused then
               Self.Group_Data (Group) := Result_Data_Last + 1;
               Result_Data_Last := Result_Data_Last + Group_Size;
               Result_Data
                 (Result_Data_Last - Group_Size + 1 .. Result_Data_Last) :=
                    Source;
            end if;
         end;
      end loop;
   end Build;

   ------------------------
   -- Data_Table_Element --
   ------------------------

   function Data_Table_Element (Offset : Data_Offset) return Data_Type is
   begin
      return Result_Data (Gen_UCD.Unsigned_32 (Offset));
   end Data_Table_Element;

   ---------------------
   -- Data_Table_Last --
   ---------------------

   function Data_Table_Last return Data_Count is
   begin
      return Data_Count (Result_Data_Last);
   end Data_Table_Last;

   -------------------------
   -- Index_Table_Element --
   -------------------------

   function Index_Table_Element
     (Self   : Compressed_Stage_Table'Class;
      Offset : Group_Offset) return Data_Offset is
   begin
      return Data_Offset (Self.Group_Data (Gen_UCD.Unsigned_32 (Offset)));
   end Index_Table_Element;

   ----------------------
   -- Index_Table_Last --
   ----------------------

   function Index_Table_Last
     (Self : Compressed_Stage_Table'Class) return Group_Count is
   begin
      return Group_Count (Self.Group_Data'Last);
   end Index_Table_Last;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (Left : Compressed_Array; Right : Compressed_Array) return Boolean is
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;

      for J in 0 .. Gen_UCD.Unsigned_32 (Left'Length) - 1 loop
         if Left (Left'First + J) /= Right (Right'First + J) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Equal;

end Gen_UCD.Generic_Compressed_Stage_Table;
