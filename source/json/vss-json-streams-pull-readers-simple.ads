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

private with VSS.JSON.Implementation.Parsers;
with VSS.Text_Streams;

package VSS.JSON.Streams.Pull.Readers.Simple is

   type JSON_Simple_Pull_Reader is limited new JSON_Pull_Reader with private;

   procedure Set_Stream
     (Self   : in out JSON_Simple_Pull_Reader'Class;
      Stream : not null VSS.Text_Streams.Input_Text_Stream_Access);

private

   type JSON_Simple_Pull_Reader is limited new JSON_Pull_Reader with record
      Parser : VSS.JSON.Implementation.Parsers.JSON_Parser;
   end record;

   overriding function At_End (Self : JSON_Simple_Pull_Reader) return Boolean;

   overriding function Boolean_Value
     (Self : JSON_Simple_Pull_Reader) return Boolean;

   overriding procedure Clear (Self : in out JSON_Simple_Pull_Reader);

   overriding function Error
     (Self : JSON_Simple_Pull_Reader)
      return VSS.JSON.Streams.Pull.Readers.JSON_Reader_Error;

   overriding function Error_Message
     (Self : JSON_Simple_Pull_Reader) return VSS.Strings.Virtual_String;

   overriding function Event_Kind
     (Self : JSON_Simple_Pull_Reader)
      return VSS.JSON.Streams.Pull.Readers.JSON_Event_Kind;

   overriding function Key_Name
     (Self : JSON_Simple_Pull_Reader) return VSS.Strings.Virtual_String;

   overriding function Number_Value
     (Self : JSON_Simple_Pull_Reader) return VSS.JSON.JSON_Number;

   overriding procedure Raise_Error
     (Self    : in out JSON_Simple_Pull_Reader;
      Message : VSS.Strings.Virtual_String);

   overriding function Read_Next
     (Self : in out JSON_Simple_Pull_Reader)
      return VSS.JSON.Streams.Pull.Readers.JSON_Event_Kind;

   overriding procedure Skip_Current_Array
     (Self : in out JSON_Simple_Pull_Reader);

   overriding procedure Skip_Current_Object
     (Self : in out JSON_Simple_Pull_Reader);

   overriding procedure Skip_Current_Value
     (Self : in out JSON_Simple_Pull_Reader);

   overriding function String_Value
     (Self : JSON_Simple_Pull_Reader) return VSS.Strings.Virtual_String;

end VSS.JSON.Streams.Pull.Readers.Simple;
