------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with VSS.Strings.Iterators.Characters;
with VSS.String_Vectors;
with VSS.Strings;

procedure Test_String_Vector is

   use type VSS.Strings.Virtual_String;

   S1 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ("a");
   S2 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ("b");
   S3 : constant VSS.Strings.Virtual_String :=
     VSS.Strings.To_Virtual_String ("c");

   V1 : VSS.String_Vectors.Virtual_String_Vector;
   V2 : VSS.String_Vectors.Virtual_String_Vector;

   Revert : VSS.Strings.Virtual_String;
begin
   --  Construct vector and check its content

   V1.Append (S1);
   V1.Append (VSS.Strings.Empty_Virtual_String);
   V1.Append (S2);

   if V1.Length /= 3 then
      raise Program_Error;
   end if;

   if V1 (1) /= S1 then
      raise Program_Error;
   end if;

   if not V1 (2).Is_Empty then
      raise Program_Error;
   end if;

   if V1 (3) /= S2 then
      raise Program_Error;
   end if;

   --  Copy vector and append more data

   V2 := V1;

   V2.Append (S3);

   if V2.Length /= 4 then
      raise Program_Error;
   end if;

   if V2 (1) /= S1 then
      raise Program_Error;
   end if;

   if not V2 (2).Is_Empty then
      raise Program_Error;
   end if;

   if V2 (3) /= S2 then
      raise Program_Error;
   end if;

   if V2 (4) /= S3 then
      raise Program_Error;
   end if;

   --  Check that first vector was not modified.

   if V1.Length /= 3 then
      raise Program_Error;
   end if;

   if V1 (1) /= S1 then
      raise Program_Error;
   end if;

   if not V1 (2).Is_Empty then
      raise Program_Error;
   end if;

   if V1 (3) /= S2 then
      raise Program_Error;
   end if;

   for Item of V2 loop
      if not Item.Is_Empty then
         Revert.Append (Item.First_Character.Element);
      end if;
   end loop;

   for Item of reverse V2 loop
      if not Item.Is_Empty then
         Revert.Append (Item.First_Character.Element);
      end if;
   end loop;

   if Revert /= VSS.Strings.To_Virtual_String ("abccba") then
      raise Program_Error;
   end if;

end Test_String_Vector;
