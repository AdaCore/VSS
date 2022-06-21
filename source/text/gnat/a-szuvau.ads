--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This child package of Ada.Strings.Wide_Wide_Unbounded provides some
--  specialized access functions which are intended to allow more efficient
--  use of the facilities of Ada.Strings.Wide_Wide_Unbounded by VSS.

package Ada.Strings.Wide_Wide_Unbounded.VSS_Aux is

   pragma Preelaborate;

   procedure Set_String
     (U      : out Unbounded_Wide_Wide_String;
      Length : Positive;
      Set    : not null access procedure (S : out Wide_Wide_String));
   pragma Inline (Set_String);
   --  Create an unbounded string U with the given Length, using Set to fill
   --  the contents of U.

end Ada.Strings.Wide_Wide_Unbounded.VSS_Aux;
