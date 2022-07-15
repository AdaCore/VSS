--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Vectors;

with VSS.XML.Events;

package VSS.XML.Event_Vectors is
  new Ada.Containers.Vectors
    (Index_Type   => Positive,
     Element_Type => VSS.XML.Events.XML_Event,
     "="          => VSS.XML.Events."=");
