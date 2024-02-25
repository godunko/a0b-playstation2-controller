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
      Right_Joystick_Horizontal : A0B.Types.Unsigned_8;  --  Analog
      Right_Joystick_Vertical   : A0B.Types.Unsigned_8;  --  Analog
      Left_Joystick_Horizontal  : A0B.Types.Unsigned_8;  --  Analog
      Left_Joystick_Vertical    : A0B.Types.Unsigned_8;  --  Analog

      Right_Direction_Button    : A0B.Types.Unsigned_8;  --  Digital/Analog
      Left_Direction_Button     : A0B.Types.Unsigned_8;  --  Digital/Analog
      Up_Direction_Button       : A0B.Types.Unsigned_8;  --  Digital/Analog
      Down_Direction_Button     : A0B.Types.Unsigned_8;  --  Digital/Analog

      Triangle_Button           : A0B.Types.Unsigned_8;  --  Digital/Analog, Y
      Circle_Button             : A0B.Types.Unsigned_8;  --  Digital/Analog, B
      Cross_Button              : A0B.Types.Unsigned_8;  --  Digital/Analog, A
      Square_Button             : A0B.Types.Unsigned_8;  --  Digital/Analog, X

      Left_1_Button             : A0B.Types.Unsigned_8;  --  Digital/Analog
      Right_1_Button            : A0B.Types.Unsigned_8;  --  Digital/Analog
      Left_2_Button             : A0B.Types.Unsigned_8;  --  Digital/Analog
      Right_2_Button            : A0B.Types.Unsigned_8;  --  Digital/Analog

      Right_Joystick            : A0B.Types.Unsigned_8;  --  Digital
      Left_Joystick             : A0B.Types.Unsigned_8;  --  Digital
      Select_Button             : A0B.Types.Unsigned_8;  --  Digital
      Start_Button              : A0B.Types.Unsigned_8;  --  Digital
   end record;
   --  Analog joysticks components has range 16#00# .. 16#FF# with neutral
   --  value at 16#80#.
   --
   --  Digital/analog buttons in analog mode has value in range 16#00#..16#FF#,
   --  where 16#00# means that button is released, and other values is value
   --  of pressure.
   --
   --  Digital/analog buttons in digital mode has only 16#00# or 16#FF# value,
   --  where 16#00# means that button is released and 16#FF# value means that
   --  button is pressed.
   --
   --  Digital buttons has only 16#00# or 16#FF# value.

end A0B.PlayStation2_Controllers;