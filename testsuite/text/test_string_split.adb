------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with VSS.String_Vectors;
with VSS.Strings;

with Test_Support;

procedure Test_String_Split is
   use type VSS.Strings.Virtual_String;

begin
   --  Common use case

   declare
      S : constant VSS.Strings.Virtual_String := "A:BC:DEF:GHIJ";
      V : constant VSS.String_Vectors.Virtual_String_Vector := S.Split (':');

   begin
      Test_Support.Assert (V.Length = 4);
      Test_Support.Assert (V (1) = "A");
      Test_Support.Assert (V (2) = "BC");
      Test_Support.Assert (V (3) = "DEF");
      Test_Support.Assert (V (4) = "GHIJ");
   end;

   --  Keep empty segments

   declare
      S : constant VSS.Strings.Virtual_String := ":A:BC::DEF:GHIJ:";
      V : constant VSS.String_Vectors.Virtual_String_Vector := S.Split (':');

   begin
      Test_Support.Assert (V.Length = 7);
      Test_Support.Assert (V (1).Is_Empty);
      Test_Support.Assert (V (2) = "A");
      Test_Support.Assert (V (3) = "BC");
      Test_Support.Assert (V (4).Is_Empty);
      Test_Support.Assert (V (5) = "DEF");
      Test_Support.Assert (V (6) = "GHIJ");
      Test_Support.Assert (V (7).Is_Empty);
   end;

   --  Drop empty segments

   declare
      S : constant VSS.Strings.Virtual_String := ":A:BC::DEF:GHIJ:";
      V : constant VSS.String_Vectors.Virtual_String_Vector :=
        S.Split (':', False);

   begin
      Test_Support.Assert (V.Length = 4);
      Test_Support.Assert (V (1) = "A");
      Test_Support.Assert (V (2) = "BC");
      Test_Support.Assert (V (3) = "DEF");
      Test_Support.Assert (V (4) = "GHIJ");
   end;

   --  Empty string

   declare
      S : constant VSS.Strings.Virtual_String := "";
      V : constant VSS.String_Vectors.Virtual_String_Vector := S.Split (':');

   begin
      Test_Support.Assert (V.Length = 1);
      Test_Support.Assert (V (1).Is_Empty);
   end;

   --  Single string

   declare
      S : constant VSS.Strings.Virtual_String := "ABCD";
      V : constant VSS.String_Vectors.Virtual_String_Vector := S.Split (':');

   begin
      Test_Support.Assert (V.Length = 1);
      Test_Support.Assert (V (1) = "ABCD");
   end;
end Test_String_Split;
