--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  PlayStation2 controller support.
--
--  This package provides only basic types and some utilities.
--
--  Asynchronous version of the driver see in child package Async.

pragma Restrictions (No_Elaboration_Code);

with A0B.Types;

package A0B.PlayStation2_Controllers is

   pragma Pure;

   type Controller_State is record
      null;
   end record;

end A0B.PlayStation2_Controllers;