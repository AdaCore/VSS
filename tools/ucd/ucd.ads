--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Vectors;

package UCD is

   pragma Preelaborate;

   type Code_Point is range 16#00_0000# .. 16#10_FFFF#;

   package Code_Point_Vectors is
     new Ada.Containers.Vectors (Positive, UCD.Code_Point);

end UCD;
