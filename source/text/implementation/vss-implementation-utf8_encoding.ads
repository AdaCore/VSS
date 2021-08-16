------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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
--  Utilities to encode/decode UTF-8 encoded data.

with Ada.Streams;

with VSS.Stream_Element_Vectors;
with VSS.Unicode;

package VSS.Implementation.UTF8_Encoding is

   pragma Preelaborate;

   type UTF8_Code_Unit_Array is
     array (VSS.Unicode.UTF8_Code_Unit_Count range <>)
     of VSS.Unicode.UTF8_Code_Unit;

   subtype UTF8_Sequence_Length
     is VSS.Unicode.UTF8_Code_Unit_Count range 1 .. 4;

   pragma Warnings (Off, "aspect ""PRE"" not enforced on inlined subprogram");
   procedure Encode
     (Code   : VSS.Unicode.Code_Point;
      Length : out UTF8_Sequence_Length;
      Unit_1 : out VSS.Unicode.UTF8_Code_Unit;
      Unit_2 : out VSS.Unicode.UTF8_Code_Unit;
      Unit_3 : out VSS.Unicode.UTF8_Code_Unit;
      Unit_4 : out VSS.Unicode.UTF8_Code_Unit)
     with Inline_Always, Pre => Code not in 16#D800# .. 16#DFFF#;
   pragma Warnings (On, "aspect ""PRE"" not enforced on inlined subprogram");
   --  Encode code point. Size is actual number of code units, U1 .. U4 values
   --  of code units.

   type UTF8_Decode_Error is
     (None,
      Incomplete_2,
      Incomplete_3,
      Incomplete_4,
      Invalid_1,
      Invalid_2_Of_2,
      Invalid_2_Of_3,
      Invalid_3_Of_3,
      Invalid_2_Of_4,
      Invalid_3_Of_4,
      Invalid_4_Of_4);

   procedure Decode
     (Data    : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Index   : in out Ada.Streams.Stream_Element_Count;
      Code    : out VSS.Unicode.Code_Point;
      Success : in out Boolean;
      Error   : out UTF8_Decode_Error);

   procedure Unchecked_Store
     (Storage : in out VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      From    : VSS.Unicode.UTF8_Code_Unit_Offset;
      Length  : VSS.Implementation.UTF8_Encoding.UTF8_Sequence_Length;
      Unit_1  : VSS.Unicode.UTF8_Code_Unit;
      Unit_2  : VSS.Unicode.UTF8_Code_Unit;
      Unit_3  : VSS.Unicode.UTF8_Code_Unit;
      Unit_4  : VSS.Unicode.UTF8_Code_Unit) with Inline_Always;
   --  Store encoded character.

end VSS.Implementation.UTF8_Encoding;
