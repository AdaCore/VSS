------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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

with VSS.Implementation.String_Configuration;
pragma Warnings (Off, ".* is an internal GNAT unit");
with Ada.Strings.Wide_Wide_Unbounded.VSS_Aux;
pragma Warnings (On, ".* is an internal GNAT unit");

package body VSS.Strings.Conversions is

   use type VSS.Implementation.Strings.String_Handler_Access;

   procedure Set_Wide_Wide_String
     (Item   : Virtual_String'Class;
      String : out Wide_Wide_String);
   --  Set given string to content of virtual string. Length of the string
   --  must be equal to the length in characters of the virtual string;
   --  otherwise Constraint_Error is raised.

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   function To_UTF_8_String
     (Item : Virtual_String'Class)
      return Ada.Strings.UTF_Encoding.UTF_8_String
   is
      Handler : constant VSS.Implementation.Strings.String_Handler_Access :=
        Item.Handler;

   begin
      if Handler = null then
         return "";

      else
         return Handler.To_UTF_8_String (Item.Data);
      end if;
   end To_UTF_8_String;

   -----------------------------------
   -- To_Unbounded_Wide_Wide_String --
   -----------------------------------

   function To_Unbounded_Wide_Wide_String
     (Item : Virtual_String'Class)
      return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String
   is
      procedure Set (String : out Wide_Wide_String);

      ---------
      -- Set --
      ---------

      procedure Set (String : out Wide_Wide_String) is
      begin
         Set_Wide_Wide_String (Item, String);
      end Set;

   begin
      return Result :
        Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String
      do
         if not Item.Is_Empty then
            Ada.Strings.Wide_Wide_Unbounded.VSS_Aux.Set_String
              (Result, Integer (Item.Character_Length), Set'Access);
         end if;
      end return;
   end To_Unbounded_Wide_Wide_String;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Item : Ada.Strings.UTF_Encoding.UTF_8_String) return Virtual_String
   is
      Success : Boolean;

   begin
      return Result : Virtual_String do
         --  First, attempt to place data in the storage inside the object of
         --  Magic_String type.

         VSS.Implementation.String_Configuration.In_Place_Handler
           .From_UTF_8_String
             (Item, Result.Data, Success);

         if not Success then
            --  Operation may fail for two reasons: source data is not
            --  well-formed UTF-8 or there is not enoght memory to store
            --  string in in-place storage.

            VSS.Implementation.String_Configuration.Default_Handler
              .From_UTF_8_String
                (Item, Result.Data, Success);
         end if;

         if not Success then
            raise Constraint_Error with "Ill-formed UTF-8 data";
         end if;
      end return;
   end To_Virtual_String;

   -------------------------
   -- Set_Wide_Wide_String --
   -------------------------

   procedure Set_Wide_Wide_String
     (Item   : Virtual_String'Class;
      String : out Wide_Wide_String)
   is
      Handler  : constant VSS.Implementation.Strings.String_Handler_Access :=
        Item.Handler;
      Position : VSS.Implementation.Strings.Cursor;

   begin
      if Item.Character_Length /= String'Length then
         raise Constraint_Error;
      end if;

      if Item.Is_Empty then
         return;
      end if;

      Handler.Before_First_Character (Item.Data, Position);

      while Handler.Forward (Item.Data, Position) loop
         String (String'First + Integer (Position.Index) - 1) :=
           Wide_Wide_Character'Val
             (Handler.Element (Item.Data, Position));
      end loop;
   end Set_Wide_Wide_String;

   -------------------------
   -- To_Wide_Wide_String --
   -------------------------

   function To_Wide_Wide_String
     (Item : Virtual_String'Class) return Wide_Wide_String is
   begin
      if Item.Is_Empty then
         return "";

      else
         return Result :
           Wide_Wide_String (1 .. Integer (Item.Character_Length))
         do
            Set_Wide_Wide_String (Item, Result);
         end return;
      end if;
   end To_Wide_Wide_String;

end VSS.Strings.Conversions;
