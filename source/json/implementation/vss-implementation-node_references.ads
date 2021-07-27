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
--  This package provides node reference counter for tree data structures.
--
--  There are two kind of references for each node:
--   - reference from the parent node, it is stored as address of the parent
--     node in the Pointer member. Null_Address means that there is no
--     parent-child references in the node tree.
--   - reference from outside of the tree, reference counter is used to
--     count them.
--
--  This package is intended to be used on 64bit platforms that supports
--  128bit atomic compare and swap operation.

private with System;

package VSS.Implementation.Node_References is

   pragma Preelaborate;

   type Controller is limited private;

   function External_Reference return Controller with Inline_Always;

   function Is_Exclusive (Self : Controller) return Boolean;
   --  Return True when there is no pointer set and refernce counter is equal
   --  to one.

   procedure Reference (Self : in out Controller);
   --  Increment reference counter of external references.

   function Unreference (Self : in out Controller) return Boolean;
   --  Decrement reference counter of external references. Return True when
   --  current node can be deallocated safely.

   function Detach (Self : in out Controller) return Boolean;
   --  Detach current node from the parent node. Return True when current
   --  node can be deallocated safely.

   generic
      type Node (<>) is limited private;
      type Node_Access is access all Node;

   package Generic_Controller_Operations is

      function Internal_Reference
        (Parent : not null Node_Access) return Controller with Inline_Always;

      function Attach
        (Self   : in out Controller;
         Parent : not null Node_Access) return Boolean;

   end Generic_Controller_Operations;

private

   type uint64 is mod 2 ** 64;
   for uint64'Size use 64;

   type Internal_Data is record
      Pointer : System.Address;
      Counter : uint64;
   end record;
   for Internal_Data'Size use 128;
   for Internal_Data'Alignment use 16;
   for Internal_Data use record
      Pointer at 0 range 0 .. 63;
      Counter at 0 range 64 .. 127;
   end record;

   type Controller is limited record
      Data : Internal_Data with Volatile;
   end record;

   function Internal_Reference
     (Parent : System.Address) return Controller with Inline_Always;

   function Attach
     (Self    : in out Controller;
      Pointer : System.Address) return Boolean;
   --  Attach current node to parent node. Return True on success, and false
   --  overwise. Operation may fail only when current node already attached
   --  (and it can be done by another thread during execution of this
   --  subprogram).

end VSS.Implementation.Node_References;
