--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
