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

limited with VSS.JSON.Documents.Arrays;
limited with VSS.JSON.Documents.Values;
--  with VSS.String_Vectors;

package VSS.JSON.Documents.Objects is

   pragma Preelaborate;

   type JSON_Object is tagged private;

   function Is_Empty (Self : JSON_Object'Class) return Boolean;

   --  function Length (Self : JSON_Object'Class) return Natural;

   function Element
     (Self : JSON_Object'Class;
      Key  : VSS.Strings.Virtual_String)
      return VSS.JSON.Documents.Values.JSON_Value;

   --  function Contains
   --    (Self : JSON_Object'Class;
   --     Key  : VSS.Strings.Virtual_String) return Boolean;
   --
   --  function Keys
   --    (Self : JSON_Object'Class)
   --     return VSS.String_Vectors.Virtual_String_Vector;

   procedure Include
     (Self  : in out JSON_Object'Class;
      Key   : VSS.Strings.Virtual_String;
      Value : VSS.JSON.Documents.Values.JSON_Value);

   procedure Include
     (Self  : in out JSON_Object'Class;
      Key   : VSS.Strings.Virtual_String;
      Value : VSS.JSON.Documents.Arrays.JSON_Array);

   procedure Include
     (Self  : in out JSON_Object'Class;
      Key   : VSS.Strings.Virtual_String;
      Value : VSS.JSON.Documents.Objects.JSON_Object);

   procedure Include
     (Self  : in out JSON_Object'Class;
      Key   : VSS.Strings.Virtual_String;
      Value : VSS.Strings.Virtual_String);

   procedure Include
     (Self  : in out JSON_Object'Class;
      Key   : VSS.Strings.Virtual_String;
      Value : VSS.JSON.JSON_Number);

   procedure Include
     (Self  : in out JSON_Object'Class;
      Key   : VSS.Strings.Virtual_String;
      Value : Boolean);

   --  procedure Exclude
   --    (Self : in out JSON_Object'Class;
   --     Key  : VSS.Strings.Virtual_String);
   --
   --  function Take
   --    (Self : in out JSON_Object'Class;
   --     Key  : VSS.Strings.Virtual_String) return JSON_Value;

private

   type JSON_Object is new Abstract_JSON_Node_Wrapper with null record;

end VSS.JSON.Documents.Objects;
