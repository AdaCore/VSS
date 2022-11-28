--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
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

   function At_First
     (Item : Virtual_String'Class) return Grapheme_Cluster_Iterator;
   --  Return iterator pointing to the first grapheme cluster of the string.

   function At_Position
     (Item     : Virtual_String'Class;
      Position : VSS.Strings.Cursors.Abstract_Character_Cursor'Class)
      return Grapheme_Cluster_Iterator;
   --  Return iterator pointing to the grapheme cluster of the string at the
   --  given position.

   function At_Last
     (Item : Virtual_String'Class) return Grapheme_Cluster_Iterator;
   --  Return iterator pointing to the last grapheme cluster of the string.

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

   procedure Initialize
     (Self            : in out Grapheme_Cluster_Iterator'Class;
      String          : Virtual_String'Class;
      Position        : VSS.Implementation.Strings.Cursor);
   --  Initialize iterator and lookup for grapheme boundaries around the given
   --  position.

end VSS.Strings.Cursors.Iterators.Grapheme_Clusters;
