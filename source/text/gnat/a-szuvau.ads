------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUN-TIME COMPONENTS                        --
--                                                                          --
--                  ADA.STRINGS.WIDE_WIDE_UNBOUNDED.VSS_AUX                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2021, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This child package of Ada.Strings.Wide_Wide_Unbounded provides some
--  specialized access functions which are intended to allow more efficient
--  use of the facilities of Ada.Strings.Wide_Wide_Unbounded by VSS.

package Ada.Strings.Wide_Wide_Unbounded.VSS_Aux is

   procedure Set_String
     (U      : out Unbounded_Wide_Wide_String;
      Length : Positive;
      Set    : not null access procedure (S : out Wide_Wide_String));
   pragma Inline (Set_String);
   --  Create an unbounded string U with the given Length, using Set to fill
   --  the contents of U.

end Ada.Strings.Wide_Wide_Unbounded.VSS_Aux;
