--
--  Copyright (C) 2021-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package VSS.Strings.Cursors.Iterators.Grapheme_Clusters is

   pragma Preelaborate;

   type Grapheme_Cluster_Iterator is
     new Abstract_Segment_Iterator with private;

   function Backward (Self : in out Grapheme_Cluster_Iterator) return Boolean;
   --  Move iterator to previous grapheme cluster.

   --  function Before_First
   --    (Item : VSS.Strings.Virtual_String'Class)
   --     return Grapheme_Cluster_Iterator;
   --  Create iterator pointing before the first grapheme cluster of the given
   --  string.

   procedure Set_At_First
     (Self : in out Grapheme_Cluster_Iterator;
      On   : VSS.Strings.Virtual_String'Class);
   --  Set iterator to point to the first grapheme cluster of the string.

   procedure Set_At
     (Self     : in out Grapheme_Cluster_Iterator;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class);
   --  Set iterator to point to the grapheme cluster at the given position.

   procedure Set_At_Last
     (Self : in out Grapheme_Cluster_Iterator;
      On   : VSS.Strings.Virtual_String'Class);
   --  Set iterator to point to the last grapheme cluster of the string.

   --  function After_Last
   --    (Item : VSS.Strings.Virtual_String'Class)
   --     return Grapheme_Cluster_Iterator;
   --  Create iterator pointing after the last grapheme cluster of the given
   --  string.

private

   type Grapheme_Cluster_Iterator is new Abstract_Segment_Iterator with record
      null;
   end record;

   overriding procedure Invalidate (Self : in out Grapheme_Cluster_Iterator);

   overriding procedure String_Modified
     (Self     : in out Grapheme_Cluster_Iterator;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset);

   overriding function Forward
     (Self : in out Grapheme_Cluster_Iterator) return Boolean;

   overriding function Has_Element
     (Self : Grapheme_Cluster_Iterator) return Boolean;

end VSS.Strings.Cursors.Iterators.Grapheme_Clusters;
