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

--  limited with VSS.JSON.Documents.Values;

package VSS.JSON.Documents.Arrays is

   pragma Preelaborate;

   type JSON_Array is tagged private;

   --  function Is_Empty (Self : JSON_Array'Class) return Boolean;

   --  function Length (Self : JSON_Array'Class) return Natural;

   --  function Element
   --    (Self  : JSON_Object'Class;
   --     Index : Positive)
   --     return VSS.JSON.Documents.Values.JSON_Value;

   --  function Contains
   --    (Self  : JSON_Array'Class;
   --     Value : VSS.JSON.Documents.Values.JSON_Value) return Boolean;

   --  function First_Element
   --    (Self : JSON_Object'Class)
   --     return VSS.JSON.Documents.Values.JSON_Value;
   --
   --  function Last_Element
   --    (Self : JSON_Object'Class)
   --     return VSS.JSON.Documents.Values.JSON_Value;

   --  procedure Append
   --    (Self  : in out JSON_Array'Class;
   --     Value : VSS.JSON.Documents.Values.JSON_Value);
   --
   --  procedure Append
   --    (Self  : in out JSON_Array'Class;
   --     Value : VSS.JSON.Documents.Objects.JSON_Object);
   --
   --  procedure Append
   --    (Self  : in out JSON_Array'Class;
   --     Value : VSS.Strings.Virtual_String);
   --
   --  procedure Append
   --    (Self  : in out JSON_Array'Class;
   --     Value : VSS.JSON.JSON_Number);
   --
   --  procedure Append
   --    (Self  : in out JSON_Array'Class;
   --     Value : Boolean);

   --  procedure Prepend
   --    (Self  : in out JSON_Array'Class;
   --     Value : VSS.JSON.Documents.Values.JSON_Value);
   --
   --  procedure Prepend
   --    (Self  : in out JSON_Array'Class;
   --     Value : VSS.JSON.Documents.Objects.JSON_Object);
   --
   --  procedure Prepend
   --    (Self  : in out JSON_Array'Class;
   --     Value : VSS.Strings.Virtual_String);
   --
   --  procedure Prepend
   --    (Self  : in out JSON_Array'Class;
   --     Value : VSS.JSON.JSON_Number);
   --
   --  procedure Prepend
   --    (Self  : in out JSON_Array'Class;
   --     Value : Boolean);

   --  procedure Insert
   --    (Self  : in out JSON_Array'Class;
   --     Index : Positive;
   --     Value : VSS.JSON.Documents.Values.JSON_Value);

   --  procedure Delete
   --    (Self  : in out JSON_Array'Class;
   --     Index : Positive);

   --  procedure Delete_First (Self : in out JSON_Array'Class);

   --  procedure Delete_Last (Self : in out JSON_Array'Class);

   --  procedure Replace
   --    (Self  : in out JSON_Array'Class;
   --     Index : Positive;
   --     Value : VSS.JSON.Documents.Values.JSON_Value);

   --  function Take
   --    (Self  : in out JSON_Array'Class;
   --     Index : Positive) return JSON_Value;

private

   type JSON_Array is new Abstract_JSON_Node_Wrapper with null record;

end VSS.JSON.Documents.Arrays;
