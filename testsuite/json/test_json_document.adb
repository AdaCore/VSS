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

with VSS.JSON.Documents.Objects;
with VSS.JSON.Documents.Values;
with VSS.Strings;

with Test_Support;

procedure Test_JSON_Document is

   use type VSS.Strings.Virtual_String;

   O1 : VSS.JSON.Documents.Objects.JSON_Object;
   O2 : VSS.JSON.Documents.Objects.JSON_Object;

begin
   O1.Include ("key1", "value1");

   Test_Support.Assert (O1.Element ("key1").String_Value = "value1");

   O1.Include ("key1", "value12");

   Test_Support.Assert (O1.Element ("key1").String_Value = "value12");

   O2 := O1;

   O2.Include ("key2", "value21");

   Test_Support.Assert (O2.Element ("key1").String_Value = "value12");
   Test_Support.Assert (O2.Element ("key2").String_Value = "value21");

   O1.Include ("key3", O2);
end Test_JSON_Document;
