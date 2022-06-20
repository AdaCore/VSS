--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  @private
--
--  This package is for internal use only.

with VSS.Implementation.Strings;

package VSS.Strings.Cursors.Iterators.Words.Internals is

   pragma Preelaborate;

   function First_Word
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Words.Word_Iterator;
   --  Return word iterator pointing to the first word of the string.

   function Last_Word
     (Self : Virtual_String'Class)
      return VSS.Strings.Cursors.Iterators.Words.Word_Iterator;
   --  Return word iterator pointing to the last word of the string.

   function Word
     (Self     : Virtual_String'Class;
      Position : VSS.Implementation.Strings.Cursor)
      return VSS.Strings.Cursors.Iterators.Words.Word_Iterator;
   --  Return word iterator pointing to the word at the given position.

end VSS.Strings.Cursors.Iterators.Words.Internals;
