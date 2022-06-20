--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Gen_UCD.Compressed_UTF_8_Data is

   function Encode
     (Data : UCD.Code_Point_Vectors.Vector)
      return UTF_8_Code_Unit_Vectors.Vector;
   --  Encode given vector of code points into UTF-8

   -----------------
   -- Append_Data --
   -----------------

   procedure Append_Data
     (Self   : in out Compressed_UTF_8_Data;
      Data   : UCD.Code_Point_Vectors.Vector;
      Offset : out Gen_UCD.UTF_8_Offset;
      Size   : out Gen_UCD.UTF_8_Count;
      Length : out Natural)
   is
      Encoded : constant UTF_8_Code_Unit_Vectors.Vector := Encode (Data);
      Found   : Boolean := False;

   begin
      if Data.Is_Empty then
         Offset := 0;
         Size   := 0;
         Length := 0;

         return;
      end if;

      Size   := Gen_UCD.UTF_8_Offset (Encoded.Length);
      Length := Natural (Data.Length);

      for Position in Self.Data.First_Index
        .. Self.Data.Last_Index - UTF_8_Offset (Encoded.Length) + 1
      loop
         if Self.Data.Element (Position) = Encoded.First_Element then
            Found  := True;
            Offset := Position;

            for Offset in 1 .. Gen_UCD.UTF_8_Offset (Encoded.Length) - 1 loop
               if Self.Data.Element (Position + Offset)
                 /= Encoded.Element (Encoded.First_Index + Offset)
               then
                  Found := False;

                  exit;
               end if;
            end loop;

            exit when Found;
         end if;
      end loop;

      if not Found then
         Offset := Gen_UCD.UTF_8_Offset (Self.Data.Length);
         Self.Data.Append_Vector (Encoded);
      end if;
   end Append_Data;

   -------------
   -- Element --
   -------------

   function Element
     (Self   : Compressed_UTF_8_Data;
      Offset : Gen_UCD.UTF_8_Offset) return Gen_UCD.UTF_8_Code_Unit is
   begin
      return Self.Data.Element (Offset);
   end Element;

   ------------
   -- Encode --
   ------------

   function Encode
     (Data : UCD.Code_Point_Vectors.Vector)
      return UTF_8_Code_Unit_Vectors.Vector
   is
      C : Unsigned_32;

   begin
      return Result : UTF_8_Code_Unit_Vectors.Vector do
         for Position in Data.First_Index .. Data.Last_Index loop
            C := Unsigned_32 (Data.Element (Position));

            if C <= 16#00_007F# then
               Result.Append (UTF_8_Code_Unit (C));

            elsif C <= 16#00_07FF# then
               Result.Append
                 (UTF_8_Code_Unit
                    (2#1100_0000#
                     or ((C and 2#111_1100_0000#) / 2#100_0000#)));
               Result.Append
                 (UTF_8_Code_Unit
                    (2#1000_0000#
                     or (C and 2#000_0011_1111#)));

            elsif C <= 16#00_FFFF# then
               Result.Append
                 (UTF_8_Code_Unit
                    (2#1110_0000#
                     or ((C and 2#1111_0000_0000_0000#)
                       / 2#1_0000_0000_0000#)));
               Result.Append
                 (UTF_8_Code_Unit
                    (2#1000_0000#
                     or ((C and 2#0000_1111_1100_0000#) / 2#100_0000#)));
               Result.Append
                 (UTF_8_Code_Unit
                    (2#1000_0000# or (C and 2#0000_0000_0011_1111#)));

            else
               Result.Append
                 (UTF_8_Code_Unit
                    (2#1111_0000#
                     or ((C and 2#1_1100_0000_0000_0000_0000#)
                       / 2#100_0000_0000_0000_0000#)));
               Result.Append
                 (UTF_8_Code_Unit
                    (2#1000_0000#
                     or ((C and 2#0_0011_1111_0000_0000_0000#)
                       / 2#1_0000_0000_0000#)));
               Result.Append
                 (UTF_8_Code_Unit
                    (2#1000_0000#
                     or ((C and 2#0_0000_0000_1111_1100_0000#)
                       / 2#100_0000#)));
               Result.Append
                 (UTF_8_Code_Unit
                    (2#1000_0000#
                     or (C and 2#0_0000_0000_0000_0011_1111#)));
            end if;
         end loop;
      end return;
   end Encode;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index
     (Self : Compressed_UTF_8_Data) return Gen_UCD.UTF_8_Offset is
   begin
      return Self.Data.Last_Index;
   end Last_Index;

end Gen_UCD.Compressed_UTF_8_Data;
