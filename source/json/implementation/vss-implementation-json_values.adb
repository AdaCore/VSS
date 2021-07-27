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

with Ada.Strings.Wide_Wide_Hash;
with Ada.Unchecked_Deallocation;

with VSS.Strings.Conversions;

package body VSS.Implementation.JSON_Values is

   procedure Mutate (Self : in out not null Node_Access);
   --  Prepare node for modification. It may result in deep copy of the
   --  subtree.

   function Clone
     (Self : Node_Access; Parent : Node_Access) return Node_Access;
   --  Clone node and its subtree. When Parent is not null, created node
   --  is internally referenced, its pointer points to Parent node. Created
   --  node need to be added to parent's container manually. When Parent is
   --  null, externalluy referenced node is created.

   procedure Release (Node : in out Node_Access);
   --  Release resources used by given node and its subtree.

   procedure Common_Insert
     (Self  : in out Node_Access;
      Key   : VSS.Strings.Virtual_String);
   --  Common code for of object.insert operation: it prepare object for
   --  modification and removes given key if any.

   package Controller_Operations is
     new VSS.Implementation.Node_References.Generic_Controller_Operations
       (Node, Node_Access);

   -----------
   -- Clone --
   -----------

   function Clone
     (Self : Node_Access; Parent : Node_Access) return Node_Access
   is
      New_Node  : Node_Access;

   begin
      if Self = null then
         return null;
      end if;

      case Self.Kind is
         when Array_Node =>
            raise Program_Error;

         when Object_Node =>
            declare
               New_Child : Node_Access;
               Position  : String_Node_Maps.Cursor := Self.Object_Data.First;

            begin
               New_Node :=
                 new Node'
                   (Kind        => Object_Node,
                    Controller  =>
                      (if Parent = null
                       then VSS.Implementation.Node_References
                              .External_Reference
                       else Controller_Operations.Internal_Reference (Parent)),
                    Object_Data => <>);

               while String_Node_Maps.Has_Element (Position) loop
                  New_Child :=
                    Clone (String_Node_Maps.Element (Position), New_Node);
                  New_Node.Object_Data.Insert
                    (String_Node_Maps.Key (Position), New_Child);

                  String_Node_Maps.Next (Position);
               end loop;
            end;

         when String_Node =>
            New_Node :=
              new Node'
                (Kind     => String_Node,
                 Controller  =>
                   (if Parent = null
                    then VSS.Implementation.Node_References.External_Reference
                    else Controller_Operations.Internal_Reference (Parent)),
                 String_Data => Self.String_Data);

         when Number_Node =>
            raise Program_Error;
         when Boolean_Node =>
            raise Program_Error;
         when Null_Node =>
            raise Program_Error;
      end case;

      return New_Node;
   end Clone;

   -------------------
   -- Common_Insert --
   -------------------

   procedure Common_Insert
     (Self  : in out Node_Access;
      Key   : VSS.Strings.Virtual_String)
   is
      Value_Node : Node_Access;
      Position   : String_Node_Maps.Cursor;

   begin
      --  Create node or prepare node for modification.

      if Self = null then
         Self := new Node'
           (Kind        => Object_Node,
            Controller  =>
              VSS.Implementation.Node_References.External_Reference,
            Object_Data => <>);

      else
         Mutate (Self);
      end if;

      --  At this point it is know that we are only user of the node, there
      --  are no checks in code below, but there are some optimization to avoid
      --  unnecessary changes of the node reference controller's state.

      Position := Self.Object_Data.Find (Key);

      if String_Node_Maps.Has_Element (Position) then
         Value_Node := String_Node_Maps.Element (Position);

         Self.Object_Data.Delete (Position);

         if VSS.Implementation.Node_References.Detach
              (Value_Node.Controller)
         then
            Release (Value_Node);
         end if;
      end if;
   end Common_Insert;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Node_Access;
      Key  : VSS.Strings.Virtual_String) return Node_Access
   is
      Position : String_Node_Maps.Cursor;

   begin
      if Self = null or else Self.Kind /= Object_Node then
         return null;
      end if;

      Position := Self.Object_Data.Find (Key);

      if String_Node_Maps.Has_Element (Position) then
         return String_Node_Maps.Element (Position);

      else
         return null;
      end if;
   end Element;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : VSS.Strings.Virtual_String) return Ada.Containers.Hash_Type is
   begin
      return
        Ada.Strings.Wide_Wide_Hash
          (VSS.Strings.Conversions.To_Wide_Wide_String (Item));
   end Hash;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self  : in out Node_Access;
      Key   : VSS.Strings.Virtual_String;
      Value : not null Node_Access)
   is
      Value_Node : Node_Access;

   begin
      Common_Insert (Self, Key);

      if Controller_Operations.Attach (Value.Controller, Self) then
         Value_Node := Value;

      else
         Value_Node := Clone (Value, Self);
      end if;

      Self.Object_Data.Insert (Key, Value_Node);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self  : in out Node_Access;
      Key   : VSS.Strings.Virtual_String;
      Value : VSS.Strings.Virtual_String)
   is
      Value_Node : Node_Access;

   begin
      Common_Insert (Self, Key);

      Value_Node :=
        new Node'
          (Kind        => String_Node,
           Controller  => Controller_Operations.Internal_Reference (Self),
           String_Data => Value);
      Self.Object_Data.Insert (Key, Value_Node);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self  : in out Node_Access;
      Key   : VSS.Strings.Virtual_String;
      Value : VSS.JSON.JSON_Number)
   is
      Value_Node : Node_Access;

   begin
      Common_Insert (Self, Key);

      Value_Node :=
        new Node'
          (Kind        => Number_Node,
           Controller  => Controller_Operations.Internal_Reference (Self),
           Number_Data => Value);
      Self.Object_Data.Insert (Key, Value_Node);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self  : in out Node_Access;
      Key   : VSS.Strings.Virtual_String;
      Value : Boolean)
   is
      Value_Node : Node_Access;

   begin
      Common_Insert (Self, Key);

      Value_Node :=
        new Node'
          (Kind         => Boolean_Node,
           Controller   => Controller_Operations.Internal_Reference (Self),
           Boolean_Data => Value);
      Self.Object_Data.Insert (Key, Value_Node);
   end Insert;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Self : Node_Access) is
   begin
      if Self /= null then
         VSS.Implementation.Node_References.Reference (Self.Controller);
         --  Atomic_Counters.Increment (Self.Counter);
      end if;
   end Reference;

   -------------
   -- Release --
   -------------

   procedure Release (Node : in out Node_Access) is

      procedure Free is
        new Ada.Unchecked_Deallocation (JSON_Values.Node, Node_Access);

   begin
      case Node.Kind is
         when Array_Node =>
            raise Program_Error;

         when Object_Node =>
            declare
               Position : String_Node_Maps.Cursor := Node.Object_Data.First;
               Child    : Node_Access;

            begin
               while String_Node_Maps.Has_Element (Position) loop
                  Child := String_Node_Maps.Element (Position);

                  if VSS.Implementation.Node_References.Detach
                    (Child.Controller)
                  then
                     Release (Child);
                  end if;

                  String_Node_Maps.Next (Position);
               end loop;

               Node.Object_Data.Clear;
            end;

         when String_Node =>
            null;

         when Number_Node =>
            null;

         when Boolean_Node =>
            null;

         when Null_Node =>
            null;
      end case;

      Free (Node);
   end Release;

   ------------
   -- Mutate --
   ------------

   procedure Mutate (Self : in out not null Node_Access) is
      Old_Node : Node_Access;
      New_Node : Node_Access;

   begin
      if not VSS.Implementation.Node_References.Is_Exclusive (Self.Controller)
      then
         Old_Node := Self;

         New_Node := Clone (Old_Node, null);

         Unreference (Old_Node);
         --  Reference (New_Node);

         Self := New_Node;
      end if;
   end Mutate;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Self : in out Node_Access) is
   begin
      if VSS.Implementation.Node_References.Unreference (Self.Controller) then
         Release (Self);
      end if;

      Self := null;
   end Unreference;

end VSS.Implementation.JSON_Values;
