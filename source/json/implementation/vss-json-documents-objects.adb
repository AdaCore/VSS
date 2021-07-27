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

with VSS.Implementation.JSON_Values.Utilities;
with VSS.JSON.Documents.Arrays.Internals;
with VSS.JSON.Documents.Values;

package body VSS.JSON.Documents.Objects is

   use type VSS.Implementation.JSON_Values.Node_Access;

   -------------
   -- Element --
   -------------

   function Element
     (Self : JSON_Object'Class;
      Key  : VSS.Strings.Virtual_String)
      return VSS.JSON.Documents.Values.JSON_Value is
   begin
      return
        VSS.Implementation.JSON_Values.Utilities.To_JSON_Value
          (VSS.Implementation.JSON_Values.Element (Self.Node, Key));
   end Element;

   -------------
   -- Include --
   -------------

   procedure Include
     (Self  : in out JSON_Object'Class;
      Key   : VSS.Strings.Virtual_String;
      Value : VSS.JSON.Documents.Values.JSON_Value)
   is
      use type VSS.JSON.Documents.Values.JSON_Value_Kind;

   begin
      if Key.Is_Empty
        or else Value.Kind = VSS.JSON.Documents.Values.No_JSON_Value
      then
         return;
      end if;

      case Value.Kind is
         when VSS.JSON.Documents.Values.No_JSON_Value =>
            null;

         when VSS.JSON.Documents.Values.JSON_Array_Value =>
            raise Program_Error;

         when VSS.JSON.Documents.Values.JSON_Object_Value =>
            VSS.Implementation.JSON_Values.Insert
              (Self.Node, Key, Value.Object_Value.Node);

         when VSS.JSON.Documents.Values.JSON_String_Value =>
            VSS.Implementation.JSON_Values.Insert
              (Self.Node, Key, Value.String_Value);

         when VSS.JSON.Documents.Values.JSON_Number =>
            raise Program_Error;

         when VSS.JSON.Documents.Values.JSON_Boolean =>
            raise Program_Error;

         when VSS.JSON.Documents.Values.JSON_Null =>
            raise Program_Error;
      end case;
   end Include;

   -------------
   -- Include --
   -------------

   procedure Include
     (Self  : in out JSON_Object'Class;
      Key   : VSS.Strings.Virtual_String;
      Value : VSS.JSON.Documents.Arrays.JSON_Array) is
   begin
      if Key.Is_Empty then
         return;
      end if;

      VSS.Implementation.JSON_Values.Insert
        (Self.Node,
         Key,
         VSS.JSON.Documents.Arrays.Internals.Get_Node (Value));
   end Include;

   -------------
   -- Include --
   -------------

   procedure Include
     (Self  : in out JSON_Object'Class;
      Key   : VSS.Strings.Virtual_String;
      Value : VSS.JSON.Documents.Objects.JSON_Object) is
   begin
      if Key.Is_Empty then
         return;
      end if;

      VSS.Implementation.JSON_Values.Insert (Self.Node, Key, Value.Node);
   end Include;

   -------------
   -- Include --
   -------------

   procedure Include
     (Self  : in out JSON_Object'Class;
      Key   : VSS.Strings.Virtual_String;
      Value : VSS.Strings.Virtual_String) is
   begin
      if Key.Is_Empty then
         return;
      end if;

      VSS.Implementation.JSON_Values.Insert (Self.Node, Key, Value);
   end Include;

   -------------
   -- Include --
   -------------

   procedure Include
     (Self  : in out JSON_Object'Class;
      Key   : VSS.Strings.Virtual_String;
      Value : VSS.JSON.JSON_Number) is
   begin
      if Key.Is_Empty then
         return;
      end if;

      VSS.Implementation.JSON_Values.Insert (Self.Node, Key, Value);
   end Include;

   -------------
   -- Include --
   -------------

   procedure Include
     (Self  : in out JSON_Object'Class;
      Key   : VSS.Strings.Virtual_String;
      Value : Boolean) is
   begin
      if Key.Is_Empty then
         return;
      end if;

      VSS.Implementation.JSON_Values.Insert (Self.Node, Key, Value);
   end Include;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : JSON_Object'Class) return Boolean is
   begin
      return Self.Node = null;
   end Is_Empty;

end VSS.JSON.Documents.Objects;
