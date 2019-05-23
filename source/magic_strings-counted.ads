------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------
--  Base type for reference counted shared data.

private with System.Atomic_Counters;

private package Magic_Strings.Counted is

   pragma Preelaborate;

   type Abstract_Shared_String is abstract new Abstract_String with private;

   procedure Finalize (Self : in out Abstract_Shared_String) is null;
   --  Called before deallocation of the data.

   overriding function Reference
     (Self : in out Abstract_Shared_String) return String_Access;

   overriding procedure Unreference (Self : in out Abstract_Shared_String);

private

   type Abstract_Shared_String is abstract new Abstract_String with record
      Counter : System.Atomic_Counters.Atomic_Counter;
   end record;

end Magic_Strings.Counted;
