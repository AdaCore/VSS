--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  @private
--
--  This package is for internal use only.

with VSS.Implementation.Strings;

package VSS.Strings.Cursors.Iterators.Grapheme_Clusters.Internals is

   pragma Preelaborate;

   function First_Grapheme_Cluster
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Grapheme_Clusters
               .Grapheme_Cluster_Iterator;
   --  Return grapheme cluster iterator pointing to the first grapheme cluster
   --  of the string.

   function Last_Grapheme_Cluster
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Grapheme_Clusters
               .Grapheme_Cluster_Iterator;
   --  Return grapheme cluster iterator pointing to the last grapheme cluster
   --  of the string.

   function Grapheme_Cluster
     (Self     : Virtual_String'Class;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Strings.Cursors.Iterators.Grapheme_Clusters
               .Grapheme_Cluster_Iterator;
   --  Return grapheme cluster iterator pointing to the grapheme cluster at
   --  the given position.

end VSS.Strings.Cursors.Iterators.Grapheme_Clusters.Internals;
