--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.Regular_Expressions.Matches is

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate (Self : in out Match) is
   begin
      Self.Has_Match := False;
   end Invalidate;

   ---------------------
   -- String_Modified --
   ---------------------

   overriding procedure String_Modified
     (Self     : in out Match;
      Start    : VSS.Implementation.Strings.Cursor;
      Removed  : VSS.Implementation.Strings.Cursor_Offset;
      Inserted : VSS.Implementation.Strings.Cursor_Offset) is
   begin
      Self.Invalidate;
   end String_Modified;

end VSS.Regular_Expressions.Matches;
