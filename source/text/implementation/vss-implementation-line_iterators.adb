--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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

   --------------
   -- Backward --
   --------------

   function Backward
     (Data                : VSS.Implementation.Strings.String_Data;
      Terminators         : VSS.Strings.Line_Terminator_Set;
      Initial_Position    : VSS.Implementation.Strings.Cursor;
      First_Position      : out VSS.Implementation.Strings.Cursor;
      Last_Position       : out VSS.Implementation.Strings.Cursor;
      Terminator_Position : out VSS.Implementation.Strings.Cursor)
      return Boolean
   is
      use type VSS.Unicode.Code_Point;

      Handler :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Data);

      Current_Position : VSS.Implementation.Strings.Cursor := Initial_Position;
      Aux_Position     : VSS.Implementation.Strings.Cursor;
      LF_Found         : Boolean := False;
      Dummy            : Boolean;

   begin
      if not Handler.Backward (Data, Current_Position) then
         --  There is no any characters before initial position.

         First_Position      := (others => <>);
         Last_Position       := (others => <>);
         Terminator_Position := (others => <>);

         return False;
      end if;

      Last_Position       := Current_Position;
      First_Position      := Current_Position;
      Terminator_Position := (others => <>);

      declare
         C : constant VSS.Unicode.Code_Point :=
           Handler.Element (Data, Current_Position);

      begin
         case C is
            when Line_Feed =>
               if Terminators (VSS.Strings.CRLF) then
                  --  Set flag to check for CR prefix when CRLF line
                  --  termination sequence is enabled. First position
                  --  and terminator's position will be set later in this
                  --  case.

                  LF_Found := True;
               end if;

               if Terminators (VSS.Strings.LF) then
                  --  Set first position and terminator position when LF only
                  --  line termination sequence is enabled.

                  First_Position      := Current_Position;
                  Terminator_Position := Current_Position;

               elsif not Terminators (VSS.Strings.CRLF) then
                  Current_Position := Initial_Position;
               end if;

            when Line_Tabulation =>
               if Terminators (VSS.Strings.VT) then
                  First_Position      := Current_Position;
                  Terminator_Position := Current_Position;

               else
                  Current_Position := Initial_Position;
               end if;

            when Form_Feed =>
               if Terminators (VSS.Strings.FF) then
                  First_Position      := Current_Position;
                  Terminator_Position := Current_Position;

               else
                  Current_Position := Initial_Position;
               end if;

            when Carriage_Return =>
               if Terminators (VSS.Strings.CR) then
                  First_Position      := Current_Position;
                  Terminator_Position := Current_Position;

               else
                  Current_Position := Initial_Position;
               end if;

            when Next_Line =>
               if Terminators (VSS.Strings.NEL) then
                  First_Position      := Current_Position;
                  Terminator_Position := Current_Position;

               else
                  Current_Position := Initial_Position;
               end if;

            when Line_Separator =>
               if Terminators (VSS.Strings.LS) then
                  First_Position      := Current_Position;
                  Terminator_Position := Current_Position;

               else
                  Current_Position := Initial_Position;
               end if;

            when Paragraph_Separator =>
               if Terminators (VSS.Strings.PS) then
                  First_Position      := Current_Position;
                  Terminator_Position := Current_Position;

               else
                  Current_Position := Initial_Position;
               end if;

            when others =>
               Current_Position := Initial_Position;
         end case;
      end;

      if LF_Found then
         Aux_Position := Current_Position;

         if Handler.Backward (Data, Aux_Position) then
            if Handler.Element (Data, Aux_Position) = Carriage_Return then
               Current_Position    := Aux_Position;
               First_Position      := Aux_Position;
               Terminator_Position := Aux_Position;
            end if;
         end if;
      end if;

      LF_Found := False;

      while Handler.Backward (Data, Current_Position) loop
         declare
            C : constant VSS.Unicode.Code_Point :=
              Handler.Element (Data, Current_Position);
            D : Boolean with Unreferenced;

         begin
            case C is
               when Line_Feed =>
                  if Terminators (VSS.Strings.LF) then
                     --  ??? Commented-out code
                     --  LF_Found := True;
                     --  First_Position := Current_Position;
                     --  D := Handler.Forward (Data, First_Position);

                     exit;

                  elsif Terminators (VSS.Strings.CRLF) then
                     raise Program_Error;

                  else
                     raise Program_Error;
                  end if;

                  --  ??? Commented-out code

                  --  if Terminators (VSS.Strings.CRLF) then
                  --     LF_Found := True;
                  --     First_Position := Current_Position;
                  --     D := Handler.Forward (Data, First_Position);
                  --
                  --  elsif Terminators (VSS.Strings.LF) then
                  --     raise Program_Error;
                  --  else
                  --     raise Program_Error;
                  --  end if;

               when Line_Tabulation =>
                  if Terminators (VSS.Strings.VT) then
                     --  ??? Commented-out code

                     --  First_Position := Current_Position;
                     --  D := Handler.Forward (Data, First_Position);
                     --
                     --  return True;
                     exit;

                  else
                     --  ??? Commented-out code
                     --  LF_Found := False;

                     --  CodePeer reports here that LF_Found is always false
                     --  (same for the 4 other occurrences of LF_Found below)
                     if LF_Found then
                        raise Program_Error;
                     end if;
                  end if;

               when Form_Feed =>
                  if Terminators (VSS.Strings.FF) then
                     --  ??? Commented-out code
                  --     First_Position := Current_Position;
                  --     D := Handler.Forward (Data, First_Position);
                  --
                  --     --  return True;
                     exit;
                  else
                     if LF_Found then
                        raise Program_Error;
                     end if;
                  end if;

               when Carriage_Return =>
                  if Terminators (VSS.Strings.CR) then
                     --  ??? Commented-out code
                     --  First_Position := Current_Position;
                     --  D := Handler.Forward (Data, First_Position);
                     --
                     --  return True;
                     exit;

                  else
                     raise Program_Error;
                  end if;

               when Next_Line =>
                  if Terminators (VSS.Strings.NEL) then
                     First_Position := Current_Position;
                     D := Handler.Forward (Data, First_Position);

                     exit;

                  else
                     raise Program_Error;
                  end if;

               when Line_Separator =>
                  if Terminators (VSS.Strings.LS) then
                     --  ??? Commented-out code
                     --  First_Position := Current_Position;
                     --  D := Handler.Forward (Data, First_Position);
                     --
                     exit;

                  else
                     if LF_Found then
                        raise Program_Error;
                     end if;
                  end if;

               when Paragraph_Separator =>
                  if Terminators (VSS.Strings.PS) then
                     --  ??? Commented-out code
                     --  First_Position := Current_Position;
                     --  D := Handler.Forward (Data, First_Position);
                     --
                     exit;

                  else
                     raise Program_Error;
                  end if;

               when others =>
                  if LF_Found then
                     raise Program_Error;
                  end if;
                     --  ??? Commented-out code
                  --  if LF_Found then
                  --     return True;
                  --  end if;
            end case;

            First_Position := Current_Position;
         end;
      end loop;

      if LF_Found then
         raise Program_Error;
      end if;

      return True;
   end Backward;

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
      Handler  :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
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
            --  CR at the end of the string is not a line terminator sequence.

            Terminator_Position := (others => <>);
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
