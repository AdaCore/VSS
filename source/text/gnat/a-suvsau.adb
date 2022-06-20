--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This child package of Ada.Strings.Unbounded provides some specialized
--  access functions which are intended to allow more efficient use of the
--  facilities of Ada.Strings.Unbounded by VSS.

package body Ada.Strings.Unbounded.VSS_Aux is

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String
     (U      : out Unbounded_String;
      Length : Positive;
      Set    : not null access procedure (S : out String))
   is
      TR : constant Shared_String_Access := U.Reference;
      DR : Shared_String_Access;
   begin
      --  Try to reuse existing shared string

      if Can_Be_Reused (TR, Length) then
         Reference (TR);
         DR := TR;

      --  Otherwise allocate new shared string

      else
         DR := Allocate (Length);
         U.Reference := DR;
      end if;

      Set (DR.Data (1 .. Length));
      DR.Last := Length;
      Unreference (TR);
   end Set_String;

end Ada.Strings.Unbounded.VSS_Aux;
