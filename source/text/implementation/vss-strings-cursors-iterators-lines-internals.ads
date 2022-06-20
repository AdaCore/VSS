--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  @private
--
--  This package is for internal use only.

with VSS.Implementation.Strings;

package VSS.Strings.Cursors.Iterators.Lines.Internals is

   pragma Preelaborate;

   function First_Line
     (Self            : Virtual_String'Class;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean             := False)
      return VSS.Strings.Cursors.Iterators.Lines.Line_Iterator;
   --  Return line iterator pointing to the first line in the string.

   function Line
     (Self            : Virtual_String'Class;
      Position        : VSS.Implementation.Strings.Cursor;
      Terminators     : Line_Terminator_Set := New_Line_Function;
      Keep_Terminator : Boolean             := False)
      return VSS.Strings.Cursors.Iterators.Lines.Line_Iterator;
   --  Return line iterator pointing to the line containing given position
   --  in the string.

end VSS.Strings.Cursors.Iterators.Lines.Internals;
