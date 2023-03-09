--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Standard input/output streams as text streams.

package VSS.Text_Streams.Standards is

   function Standard_Output return VSS.Text_Streams.Output_Text_Stream'Class;
   --  Return text stream to output to standard output stream.

   function Standard_Error return VSS.Text_Streams.Output_Text_Stream'Class;
   --  Return text stream to output to standard error stream.

end VSS.Text_Streams.Standards;
