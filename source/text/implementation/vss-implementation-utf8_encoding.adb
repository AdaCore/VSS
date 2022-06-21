--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.Implementation.UTF8_Encoding is

   ------------
   -- Decode --
   ------------

   procedure Decode
     (Data    : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Index   : in out Ada.Streams.Stream_Element_Count;
      Code    : out VSS.Unicode.Code_Point;
      Success : in out Boolean;
      Error   : out UTF8_Decode_Error)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type VSS.Unicode.Code_Point;

      procedure Report_Error (E : UTF8_Decode_Error);

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (E : UTF8_Decode_Error) is
      begin
         Code    := 0;
         Success := False;
         Error   := E;
      end Report_Error;

      U1 : VSS.Unicode.Code_Point;
      U2 : VSS.Unicode.Code_Point;
      U3 : VSS.Unicode.Code_Point;
      U4 : VSS.Unicode.Code_Point;

   begin
      if Index > Data.Length then
         Success := False;
         Error   := None;
         Code    := 0;

         return;
      end if;

      case Data.Element (Index) is
         when 16#00# .. 16#7F# =>
            --  1x code units sequence
            --
            --  00 .. 7F

            Code  := VSS.Unicode.Code_Point (Data.Element (Index));
            Index := Index + 1;
            Error := None;

         when 16#C2# .. 16#DF# =>
            --  2x code units sequence
            --
            --  C2 .. DF | 80 .. BF

            if Index + 1 > Data.Length then
               Report_Error (Incomplete_2);

               return;
            end if;

            U1 := VSS.Unicode.Code_Point (Data.Element (Index));
            U2 := VSS.Unicode.Code_Point (Data.Element (Index + 1));

            if U2 not in 16#80# .. 16#BF# then
               Report_Error (Invalid_2_Of_2);

               return;
            end if;

            U1 := (U1 and 2#0001_1111#) * 2#0100_0000#;
            U2 := U2 and 2#0011_1111#;

            Code  := U1 or U2;
            Index := Index + 2;
            Error := None;

         when 16#E0# .. 16#EF# =>
            --  3x code units sequence
            --
            --  E0       | A0 .. BF | 80 .. BF
            --  E1 .. EC | 80 .. BF | 80 .. BF
            --  ED       | 80 .. 9F | 80 .. BF
            --  EE .. EF | 80 .. BF | 80 .. BF

            if Index + 2 > Data.Length then
               Report_Error (Incomplete_3);

               return;
            end if;

            U1 := VSS.Unicode.Code_Point (Data.Element (Index));
            U2 := VSS.Unicode.Code_Point (Data.Element (Index + 1));
            U3 := VSS.Unicode.Code_Point (Data.Element (Index + 2));

            if U1 = 16#E0# and U2 not in 16#A0# .. 16#BF# then
               Report_Error (Invalid_2_Of_3);

               return;

            elsif U1 in 16#E1# .. 16#EC# and U2 not in 16#80# .. 16#BF# then
               Report_Error (Invalid_2_Of_3);

               return;

            elsif U1 = 16#ED# and U2 not in 16#80# .. 16#9F# then
               Report_Error (Invalid_2_Of_3);

               return;

            elsif U1 in 16#EE# .. 16#EF# and U2 not in 16#80# .. 16#BF# then
               Report_Error (Invalid_2_Of_3);

               return;
            end if;

            if U3 not in 16#80# .. 16#BF# then
               Report_Error (Invalid_3_Of_3);

               return;
            end if;

            U1 := (U1 and 2#0000_1111#) * 2#01_0000_0000_0000#;
            U2 := (U2 and 2#0011_1111#) * 2#0100_0000#;
            U3 := U3 and 2#0011_1111#;

            Code := U1 or U2 or U3;
            Index := Index + 3;
            Error := None;

         when 16#F0# .. 16#F4# =>
            --  4x code units sequence
            --
            --  F0       | 90 .. BF | 80 .. BF | 80 .. BF
            --  F1 .. F3 | 80 .. BF | 80 .. BF | 80 .. BF
            --  F4       | 80 .. 8F | 80 .. BF | 80 .. BF

            if Index + 3 > Data.Length then
               Report_Error (Incomplete_4);

               return;
            end if;

            U1 := VSS.Unicode.Code_Point (Data.Element (Index));
            U2 := VSS.Unicode.Code_Point (Data.Element (Index + 1));
            U3 := VSS.Unicode.Code_Point (Data.Element (Index + 2));
            U4 := VSS.Unicode.Code_Point (Data.Element (Index + 3));

            if U1 = 16#F0# and U2 not in 16#90# .. 16#BF# then
               Report_Error (Invalid_2_Of_4);

               return;

            elsif U1 in 16#F1# .. 16#F3# and U2 not in 16#80# .. 16#BF# then
               Report_Error (Invalid_2_Of_4);

               return;

            elsif U1 = 16#F4# and U2 not in 16#80# .. 16#8F# then
               Report_Error (Invalid_2_Of_4);

               return;
            end if;

            if U3 not in 16#80# .. 16#BF# then
               Report_Error (Invalid_3_Of_4);

               return;
            end if;

            if U4 not in 16#80# .. 16#BF# then
               Report_Error (Invalid_4_Of_4);

               return;
            end if;

            U1 := (U1 and 2#0000_0111#) * 2#0100_0000_0000_0000_0000#;
            U2 := (U2 and 2#0011_1111#) * 2#010_000_0000_0000#;
            U3 := (U3 and 2#0011_1111#) * 2#0100_0000#;
            U4 := U4 and 2#0011_1111#;

            Code  := U1 or U2 or U3 or U4;
            Index := Index + 4;
            Error := None;

         when others =>
            Report_Error (Invalid_1);
      end case;
   end Decode;

   ------------
   -- Encode --
   ------------

   procedure Encode
     (Code   : VSS.Unicode.Code_Point;
      Length : out UTF8_Sequence_Length;
      Unit_1 : out VSS.Unicode.UTF8_Code_Unit;
      Unit_2 : out VSS.Unicode.UTF8_Code_Unit;
      Unit_3 : out VSS.Unicode.UTF8_Code_Unit;
      Unit_4 : out VSS.Unicode.UTF8_Code_Unit)
   is
      use type VSS.Unicode.Code_Point;
      use type VSS.Unicode.UTF8_Code_Unit;

   begin
      --  Protection against uninitialized values
      --  ??? This block is here to prevent CodePeer from flagging these
      --  as uninitialized out parameters. We should profile this on a
      --  real use case and assess whether this has a performance impact.
      --  Another option might be to pack this into a 32-bit structure
      --  and rely on the compiler to optimize this further.
      Unit_1 := 0;
      Unit_2 := 0;
      Unit_3 := 0;
      Unit_4 := 0;

      case Code is
         when 16#00_0000# .. 16#00_007F# =>
            Length := 1;
            Unit_1 := VSS.Unicode.UTF8_Code_Unit (Code and 16#7F#);

         when 16#00_0080# .. 16#00_07FF# =>
            Length := 2;
            Unit_1 :=
              2#1100_0000#
              or VSS.Unicode.UTF8_Code_Unit
                ((Code and 2#111_1100_0000#) / 2#100_0000#);
            Unit_2 :=
              2#1000_0000#
              or VSS.Unicode.UTF8_Code_Unit (Code and 2#000_0011_1111#);

         when 16#00_0800# .. 16#00_FFFF# =>
            Length := 3;
            Unit_1 :=
              2#1110_0000#
              or VSS.Unicode.UTF8_Code_Unit
                ((Code and 2#1111_0000_0000_0000#)
                 / 2#1_0000_0000_0000#);
            Unit_2 :=
              2#1000_0000#
              or VSS.Unicode.UTF8_Code_Unit
                ((Code and 2#0000_1111_1100_0000#) / 2#100_0000#);
            Unit_3 :=
              2#1000_0000#
              or VSS.Unicode.UTF8_Code_Unit
                (Code and 2#0000_0000_0011_1111#);

         when 16#01_0000# .. 16#10_FFFF# =>
            Length := 4;
            Unit_1 :=
              2#1111_0000#
              or VSS.Unicode.UTF8_Code_Unit
                ((Code and 2#1_1100_0000_0000_0000_0000#)
                 / 2#100_0000_0000_0000_0000#);
            Unit_2 :=
              2#1000_0000#
              or VSS.Unicode.UTF8_Code_Unit
                ((Code and 2#0_0011_1111_0000_0000_0000#)
                 / 2#1_0000_0000_0000#);
            Unit_3 :=
              2#1000_0000#
              or VSS.Unicode.UTF8_Code_Unit
                ((Code and 2#0_0000_0000_1111_1100_0000#)
                 / 2#100_0000#);
            Unit_4 :=
              2#1000_0000#
              or VSS.Unicode.UTF8_Code_Unit
                (Code and 2#0_0000_0000_0000_0011_1111#);
      end case;
   end Encode;

   ---------------------
   -- Unchecked_Store --
   ---------------------

   procedure Unchecked_Store
     (Storage : in out VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From    : VSS.Unicode.UTF8_Code_Unit_Offset;
      Length  : VSS.Implementation.UTF8_Encoding.UTF8_Sequence_Length;
      Unit_1  : VSS.Unicode.UTF8_Code_Unit;
      Unit_2  : VSS.Unicode.UTF8_Code_Unit;
      Unit_3  : VSS.Unicode.UTF8_Code_Unit;
      Unit_4  : VSS.Unicode.UTF8_Code_Unit)
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

   begin
      Storage (From) := Unit_1;

      if Length >= 2 then
         Storage (From + 1) := Unit_2;

         if Length >= 3 then
            Storage (From + 2) := Unit_3;

            if Length = 4 then
               Storage (From + 3) := Unit_4;
            end if;
         end if;
      end if;
   end Unchecked_Store;

end VSS.Implementation.UTF8_Encoding;
