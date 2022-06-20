--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
