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

package body VSS.Strings.Converters.Decoders is

   ------------
   -- Decode --
   ------------

   function Decode
     (Self : in out Virtual_String_Decoder'Class;
      Data : VSS.Stream_Element_Buffers.Stream_Element_Buffer)
      return VSS.Strings.Virtual_String is
   begin
      return VSS.Strings.Empty_Virtual_String;
   end Decode;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message
     (Self : Virtual_String_Decoder'Class) return VSS.Strings.Virtual_String is
   begin
      return VSS.Strings.Empty_Virtual_String;
   end Error_Message;

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error (Self : Virtual_String_Decoder'Class) return Boolean is
   begin
      return True;
   end Has_Error;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Virtual_String_Decoder'Class;
      Encoding : VSS.Strings.Virtual_String;
      Flags    : Converter_Flags) is
   begin
      null;
   end Initialize;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Self : Virtual_String_Decoder'Class) return Boolean is
   begin
      return False;
   end Is_Valid;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State (Self : in out Virtual_String_Decoder'Class) is
   begin
      null;
   end Reset_State;

end VSS.Strings.Converters.Decoders;
