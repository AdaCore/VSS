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

with VSS.Implementation.String_Handlers;
with VSS.Unicode;

package body VSS.Implementation.Line_Iterators is

   Line_Feed           : constant VSS.Unicode.Code_Point := 16#00_000A#;
   Line_Tabulation     : constant VSS.Unicode.Code_Point := 16#00_000B#;
   Form_Feed           : constant VSS.Unicode.Code_Point := 16#00_000C#;
   Carriage_Return     : constant VSS.Unicode.Code_Point := 16#00_000D#;
   Next_Line           : constant VSS.Unicode.Code_Point := 16#00_0085#;
   Line_Separator      : constant VSS.Unicode.Code_Point := 16#00_2028#;
   Paragraph_Separator : constant VSS.Unicode.Code_Point := 16#00_2029#;

   -------------
   -- Forward --
   -------------

   function Forward
     (Data                : VSS.Implementation.Strings.String_Data;
      Terminators         : VSS.Strings.Line_Terminator_Set;
      Initial_Position    : VSS.Implementation.Strings.Cursor;
      First_Position      : out VSS.Implementation.Strings.Cursor;
      Last_Position       : out VSS.Implementation.Strings.Cursor;
      Terminator_Position : out VSS.Implementation.Strings.Cursor)
      return Boolean
   is
      Handler  : constant VSS.Implementation.Strings.String_Handler_Access :=
        VSS.Implementation.Strings.Handler (Data);

      CR_Found : Boolean := False;
      Dummy    : Boolean;

   begin
      First_Position      := Initial_Position;
      Last_Position       := Initial_Position;
      Terminator_Position := (others => <>);

      if not Handler.Forward (Data, First_Position) then
         Last_Position       := Initial_Position;
         Terminator_Position := (others => <>);

         return False;
      end if;

      while Handler.Forward (Data, Last_Position) loop
         declare
            use type VSS.Unicode.Code_Point;

            C : constant VSS.Unicode.Code_Point :=
              Handler.Element (Data, Last_Position);

         begin
            if CR_Found then
               if C /= Line_Feed and Terminators (VSS.Strings.CR) then
                  --  It is special case to handle single CR when both CR and
                  --  CRLF are allowed.

                  CR_Found := False;
                  Dummy    := Handler.Backward (Data, Last_Position);

                  exit;
               end if;
            end if;

            case C is
               when Line_Feed =>
                  if Terminators (VSS.Strings.CRLF) and CR_Found then
                     CR_Found := False;

                     exit;

                  elsif Terminators (VSS.Strings.LF) then
                     Terminator_Position := Last_Position;

                     exit;
                  end if;

               when Line_Tabulation =>
                  if Terminators (VSS.Strings.VT) then
                     Terminator_Position := Last_Position;

                     exit;
                  end if;

               when Form_Feed =>
                  if Terminators (VSS.Strings.FF) then
                     Terminator_Position := Last_Position;

                     exit;
                  end if;

               when Carriage_Return =>
                  if Terminators (VSS.Strings.CRLF) then
                     Terminator_Position := Last_Position;
                     CR_Found            := True;

                  elsif Terminators (VSS.Strings.CR) then
                     Terminator_Position := Last_Position;

                     exit;
                  end if;

               when Next_Line =>
                  if Terminators (VSS.Strings.NEL) then
                     Terminator_Position := Last_Position;

                     exit;
                  end if;

               when Line_Separator =>
                  if Terminators (VSS.Strings.LS) then
                     Terminator_Position := Last_Position;

                     exit;
                  end if;

               when Paragraph_Separator =>
                  if Terminators (VSS.Strings.PS) then
                     Terminator_Position := Last_Position;

                     exit;
                  end if;

               when others =>
                  null;
            end case;
         end;
      end loop;

      if CR_Found then
         if Terminators (VSS.Strings.CR) then
            --  It is special case to handle single CR at the end of string
            --  when both CR and CRLF are allowed.

            Dummy := Handler.Backward (Data, Last_Position);

         else
            raise Program_Error;
         end if;
      end if;

      if VSS.Implementation.Strings.Is_Invalid (Terminator_Position) then
         --  Last_Position should point to the last character of the line
         --  found, but when line terminator sequence is now found it points
         --  to the character after last character of the string. Thus, it
         --  should be moved one character backward.

         Dummy := Handler.Backward (Data, Last_Position);
      end if;

      return True;
   end Forward;

end VSS.Implementation.Line_Iterators;
