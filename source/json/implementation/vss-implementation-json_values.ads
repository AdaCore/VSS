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
--  Design description
--
--  JSON document is represented as tree of nodes. There are no cycles possible
--  by the nature of the JSON format.

pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

with VSS.JSON;
with VSS.Strings;
with VSS.Implementation.Node_References;

package VSS.Implementation.JSON_Values is

   pragma Preelaborate;

   type Node_Kind is
     (Array_Node,
      Object_Node,
      String_Node,
      Number_Node,
      Boolean_Node,
      Null_Node);

   type Node;

   type Node_Access is access all Node;

   package Node_Vectors is
     new Ada.Containers.Vectors (Positive, Node_Access);

   function Hash
     (Item : VSS.Strings.Virtual_String) return Ada.Containers.Hash_Type;

   package String_Node_Maps is
     new Ada.Containers.Hashed_Maps
           (VSS.Strings.Virtual_String,
            Node_Access,
            Hash,
            VSS.Strings."=");

   type Node (Kind : Node_Kind := Null_Node) is record
      Controller : VSS.Implementation.Node_References.Controller;

      case Kind is
         when Array_Node =>
            Array_Data   : Node_Vectors.Vector;

         when Object_Node =>
            Object_Data  : String_Node_Maps.Map;

         when String_Node =>
            String_Data  : VSS.Strings.Virtual_String;

         when Number_Node =>
            Number_Data  : VSS.JSON.JSON_Number;

         when Boolean_Node =>
            Boolean_Data : Boolean;

         when Null_Node =>
            null;
      end case;
   end record;

   procedure Reference (Self : Node_Access);
   --  Increment internal reference counter.

   procedure Unreference (Self : in out Node_Access);
   --  Decrement internal reference counter and release data when it reach
   --  zero value and node is root node of the tree.

   --  Object's operations

   function Element
     (Self : Node_Access;
      Key  : VSS.Strings.Virtual_String) return Node_Access;

   procedure Insert
     (Self  : in out Node_Access;
      Key   : VSS.Strings.Virtual_String;
      Value : not null Node_Access)
     with Pre => Self = null or else Self.Kind = Object_Node;

   procedure Insert
     (Self  : in out Node_Access;
      Key   : VSS.Strings.Virtual_String;
      Value : VSS.Strings.Virtual_String)
     with Pre => Self = null or else Self.Kind = Object_Node;

   procedure Insert
     (Self  : in out Node_Access;
      Key   : VSS.Strings.Virtual_String;
      Value : VSS.JSON.JSON_Number)
     with Pre => Self = null or else Self.Kind = Object_Node;

   procedure Insert
     (Self  : in out Node_Access;
      Key   : VSS.Strings.Virtual_String;
      Value : Boolean)
     with Pre => Self = null or else Self.Kind = Object_Node;

end VSS.Implementation.JSON_Values;
