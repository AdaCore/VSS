------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with VSS.Implementation.String_Handlers;

package body VSS.Strings.Cursors.Iterators.Grapheme_Clusters.Internals is

   ----------------------------
   -- First_Grapheme_Cluster --
   ----------------------------

   function First_Grapheme_Cluster
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Grapheme_Clusters
               .Grapheme_Cluster_Iterator
   is
      Handler  :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Self.Data);
      Position : VSS.Implementation.Strings.Cursor;
      Dummy    : Boolean;

   begin
      return Result :
        VSS.Strings.Cursors.Iterators.Grapheme_Clusters
          .Grapheme_Cluster_Iterator
      do
         Handler.Before_First_Character (Self.Data, Position);
         Dummy := Handler.Forward (Self.Data, Position);
         Result.Initialize (Self, Position);
      end return;
   end First_Grapheme_Cluster;

   ----------------------
   -- Grapheme_Cluster --
   ----------------------

   function Grapheme_Cluster
     (Self     : Virtual_String'Class;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Strings.Cursors.Iterators.Grapheme_Clusters
               .Grapheme_Cluster_Iterator is
   begin
      return Result :
        VSS.Strings.Cursors.Iterators.Grapheme_Clusters
          .Grapheme_Cluster_Iterator
      do
         Result.Initialize (Self, Position);
      end return;
   end Grapheme_Cluster;

   ---------------------------
   -- Last_Grapheme_Cluster --
   ---------------------------

   function Last_Grapheme_Cluster
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Grapheme_Clusters
               .Grapheme_Cluster_Iterator
   is
      Handler  :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Self.Data);
      Position : VSS.Implementation.Strings.Cursor;
      Dummy    : Boolean;

   begin
      return Result :
        VSS.Strings.Cursors.Iterators.Grapheme_Clusters
          .Grapheme_Cluster_Iterator
      do
         Handler.After_Last_Character (Self.Data, Position);
         Dummy := Handler.Backward (Self.Data, Position);
         Result.Initialize (Self, Position);
      end return;
   end Last_Grapheme_Cluster;

end VSS.Strings.Cursors.Iterators.Grapheme_Clusters.Internals;
