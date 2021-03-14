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
--  Special implementation of text streams to be used for testing.

with Ada.Streams;

with VSS.Characters;
with VSS.Stream_Element_Vectors;
with VSS.Strings;
with VSS.Text_Streams;

package Tests_Text_Streams is

   ------------------------------
   -- Memory_UTF8_Input_Stream --
   ------------------------------

   type Memory_UTF8_Input_Stream is
   limited new VSS.Text_Streams.Input_Text_Stream with record
      Buffer      : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Current     : Ada.Streams.Stream_Element_Count := 1;
      Skip        : Boolean := False;
      Incremental : Boolean := False;
      Diagnosis   : VSS.Strings.Virtual_String;
   end record;

   overriding procedure Get
     (Self    : in out Memory_UTF8_Input_Stream;
      Item    : out VSS.Characters.Virtual_Character;
      Success : in out Boolean);

   overriding function Is_End_Of_Data
     (Self : Memory_UTF8_Input_Stream) return Boolean;

   overriding function Is_End_Of_Stream
     (Self : Memory_UTF8_Input_Stream) return Boolean;

   overriding function Has_Error
     (Self : Memory_UTF8_Input_Stream) return Boolean;

   overriding function Error_Message
     (Self : Memory_UTF8_Input_Stream) return VSS.Strings.Virtual_String;

   procedure Set_Incremental
     (Self : in out Memory_UTF8_Input_Stream'Class;
      To   : Boolean);

   -------------------------------
   -- Memory_UTF8_Output_Stream --
   -------------------------------

   type Memory_UTF8_Output_Stream is
   limited new VSS.Text_Streams.Output_Text_Stream with record
      Buffer : VSS.Stream_Element_Vectors.Stream_Element_Vector;
      Limit  : VSS.Strings.Character_Count := VSS.Strings.Character_Count'Last;
      Count  : VSS.Strings.Character_Count := 0;
      --  Count of the processed characters and limiting amount, Put operation
      --  returns failure when this limit has been reached.
   end record;

   overriding procedure Put
     (Self    : in out Memory_UTF8_Output_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean);

   procedure Set_Limit
     (Self : in out Memory_UTF8_Output_Stream'Class;
      To   : VSS.Strings.Character_Count);
   --  Set limiting number of character that can be consumed by the text stream
   --  successfully. After reaching of this limit all subsequential Put
   --  operations will fail.

   --------------------------
   -- String_Output_Stream --
   --------------------------

   type String_Output_Stream is
   limited new VSS.Text_Streams.Output_Text_Stream with record
      Buffer : VSS.Strings.Virtual_String;
      Limit  : VSS.Strings.Character_Count := VSS.Strings.Character_Count'Last;
      --  Limiting amount of accumulated characters, Put operation returns
      --  failure when this limit has been reached.
   end record;

   overriding procedure Put
     (Self    : in out String_Output_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean);

   procedure Set_Limit
     (Self : in out String_Output_Stream'Class;
      To   : VSS.Strings.Character_Count);
   --  Set limiting number of character that can be consumed by the text stream
   --  successfully. After reaching of this limit all subsequential Put
   --  operations will fail.

end Tests_Text_Streams;
