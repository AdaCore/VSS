------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

package body VSS.Implementation.UTF8_Encoding is

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

end VSS.Implementation.UTF8_Encoding;
