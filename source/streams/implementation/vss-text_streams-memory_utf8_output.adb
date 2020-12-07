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

with Ada.Streams;

with VSS.Implementation.UTF8_Encoding;
with VSS.Unicode;

package body VSS.Text_Streams.Memory_UTF8_Output is

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Self    : in out Memory_UTF8_Output_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

      use type VSS.Implementation.UTF8_Encoding.UTF8_Sequence_Length;

      Code : constant VSS.Unicode.Code_Point :=
        VSS.Characters.Virtual_Character'Pos (Item);
      L    : VSS.Implementation.UTF8_Encoding.UTF8_Sequence_Length;
      U1   : VSS.Unicode.UTF8_Code_Unit;
      U2   : VSS.Unicode.UTF8_Code_Unit;
      U3   : VSS.Unicode.UTF8_Code_Unit;
      U4   : VSS.Unicode.UTF8_Code_Unit;

   begin
      VSS.Implementation.UTF8_Encoding.Encode (Code, L, U1, U2, U3, U4);

      Self.Buffer.Append (Ada.Streams.Stream_Element (U1));

      if L >= 2 then
         Self.Buffer.Append (Ada.Streams.Stream_Element (U2));

         if L >= 3 then
            Self.Buffer.Append (Ada.Streams.Stream_Element (U3));

            if L = 4 then
               Self.Buffer.Append (Ada.Streams.Stream_Element (U4));
            end if;
         end if;
      end if;
   end Put;

end VSS.Text_Streams.Memory_UTF8_Output;
