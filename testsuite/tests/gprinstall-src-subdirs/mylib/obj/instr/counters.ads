--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package Counters is
   type Counter is record
      Value : Integer := 0;
   end record;

   procedure Bump (C : in out Counter);
   procedure Reset (C : in out Counter);
end Counters;
