--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Wide_Wide_Text_IO;

package Gen_UCD.Casing is

   procedure Build;

   procedure Generate (File : Ada.Wide_Wide_Text_IO.File_Type);

end Gen_UCD.Casing;
