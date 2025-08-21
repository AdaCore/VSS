--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Strings.Cursors.Iterators.Grapheme_Clusters;

package body VSS.Strings.Utilities is

   -------------------
   -- Display_Width --
   -------------------

   function Display_Width
     (Item : VSS.Strings.Virtual_String)
      return VSS.Strings.Display_Cell_Count
   is
      Iterator : VSS.Strings.Cursors.Iterators.Grapheme_Clusters
                   .Grapheme_Cluster_Iterator;

   begin
      Iterator.Set_Before_First (Item);

      return Result : VSS.Strings.Display_Cell_Count := 0 do
         while Iterator.Forward loop
            Result := @ + Iterator.Display_Width;
         end loop;
      end return;
   end Display_Width;

end VSS.Strings.Utilities;
