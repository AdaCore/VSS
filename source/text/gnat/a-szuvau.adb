--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Ada.Strings.Wide_Wide_Unbounded.VSS_Aux is

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String
     (U      : out Unbounded_Wide_Wide_String;
      Length : Positive;
      Set    : not null access procedure (S : out Wide_Wide_String))
   is
      TR : constant Shared_Wide_Wide_String_Access := U.Reference;
      DR : Shared_Wide_Wide_String_Access;
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

end Ada.Strings.Wide_Wide_Unbounded.VSS_Aux;
