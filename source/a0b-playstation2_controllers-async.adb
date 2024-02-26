--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Ada.Real_Time;

package body A0B.PlayStation2_Controllers.Async is

   Packet_Interval : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Microseconds (20);
   --  Minimum interval between the packets.

   function Polling_Time_Span
     (Rate : Polling_Rate) return Ada.Real_Time.Time_Span;
   --  Computes polling cycle duration.

   Digital_Mode       : constant := 16#4#;
   Analog_Mode        : constant := 16#7#;
   Configuration_Mode : constant := 16#f#;

   package Packet_Builder is

      procedure Enter_Configuration_Mode (Buffer : out Communication.Buffer);
      --  0x43 command to enter configuration (escape) mode.

      procedure Leave_Configuration_Mode (Buffer : out Communication.Buffer);
      --  0x43 command to leave configuration (escape) mode.

      procedure Controller_Identification (Buffer : out Communication.Buffer);
      --  0x45 command to get identifier and status of analog mode.

      procedure Enable_Digital_Mode
        (Buffer : out Communication.Buffer; Lock : Boolean);
      --  0x44 command to enable digital mode, and lock mode when requested.

      procedure Enable_Analog_Mode
        (Buffer : out Communication.Buffer; Lock : Boolean);
      --  0x44 command to enable analog mode, and lock mode when requested.
      --  Note, it enables joysticks only, analog mode for buttons need to
      --  be enabled and configured separately with 0x4F and 0x40 commands.

      procedure Set_Analog_Polling_Mask (Buffer : out Communication.Buffer);
      --  0x4F command to set polling mask of analog buttons (enable for all
      --  buttons now). Note, each button need to be configured additionaly
      --  with 0x41 command.

      procedure Get_Analog_Polling_Mask (Buffer : out Communication.Buffer);
      --  0x41 command to get polling mask of analog buttons.

      procedure Enable_Analog_Button
        (Buffer : out Communication.Buffer;
         Button : A0B.Types.Unsigned_8);
      --  0x40 command to enables analog mode for the given button.

   end Packet_Builder;

   -----------------------
   -- Build_Poll_Packet --
   -----------------------

   procedure Build_Poll_Packet
     (Buffer : out Communication.Buffer) is
   begin
      Buffer := [others => 0];

      Buffer (0) := 16#01#;
      Buffer (1) := 16#42#;
      Buffer (2) := 16#00#;

      --  Buffer (3) := WW;
      --  Buffer (4) := YY;
      --  XXX motor control is not implemented.
   end Build_Poll_Packet;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Self             : in out PlayStation2_Controller'Class;
      Analog_Joysticks : Boolean;
      Analog_Buttons   : Boolean      := False;
      Rate             : Polling_Rate := 250) is
   begin
      Self.Configuration (Self.Active_Configuration + 1) :=
        (Rate             => Rate,
         Analog_Joysticks => Analog_Joysticks,
         Analog_Buttons   => Analog_Buttons);
      Self.Change_Configuration := True;
   end Configure;

   ---------------------
   -- Controller_Mode --
   ---------------------

   function Controller_Mode
     (Buffer : Communication.Buffer) return A0B.Types.Unsigned_8
   is
      use type A0B.Types.Unsigned_8;

   begin
      return (Buffer (Buffer'First + 1) and 16#F0#) / 16;
   end Controller_Mode;

   ------------
   -- Driver --
   ------------

   task body Driver is

      use type Ada.Real_Time.Time_Span;

      type Driver_State is (Probe, Configure, Poll, Failure);

      ------------------
      -- Do_Configure --
      ------------------

      procedure Do_Configure
        (Cycle_Duration : out Ada.Real_Time.Time_Span)
      is
         Buffer        : Communication.Buffer
           renames Self.Buffer (Self.Communication_Buffer);
         Status        : aliased Communication.Status;
         Configuration : Async.Configuration
           renames Self.Configuration (Self.Active_Configuration + 1);

      begin
         --  Switch controller into configuration mode.

         Packet_Builder.Enter_Configuration_Mode (Buffer);
         Self.Channel.Exchange (Buffer, Status);

         if Status.No_Acknowledge then
            return;

         else
            delay until Ada.Real_Time.Clock + Packet_Interval;
         end if;

         --  Switch controller to digital/analog mode.

         if Configuration.Analog_Joysticks then
            --  Enable analog joysticks

            Packet_Builder.Enable_Analog_Mode (Buffer, True);
            Self.Channel.Exchange (Buffer, Status);

            if Status.No_Acknowledge then
               return;

            else
               delay until Ada.Real_Time.Clock + Packet_Interval;
            end if;

            if Configuration.Analog_Buttons then
               --  Set analog polling mask.

               Packet_Builder.Set_Analog_Polling_Mask (Buffer);
               Self.Channel.Exchange (Buffer, Status);

               if Status.No_Acknowledge then
                  return;

               else
                  delay until Ada.Real_Time.Clock + Packet_Interval;
               end if;

               --  Enable all analog buttons.

               for Button in A0B.Types.Unsigned_8 (0) .. 11 loop
                  Packet_Builder.Enable_Analog_Button (Buffer, Button);
                  Self.Channel.Exchange (Buffer, Status);

                  if Status.No_Acknowledge then
                     return;

                  else
                     delay until Ada.Real_Time.Clock + Packet_Interval;
                  end if;
               end loop;
            end if;

         else
            --  Enable digital mode.

            Packet_Builder.Enable_Digital_Mode (Buffer, True);

            Self.Channel.Exchange (Buffer, Status);

            if Status.No_Acknowledge then
               return;

            else
               delay until Ada.Real_Time.Clock + Packet_Interval;
            end if;
         end if;

         --  Switch controller into normal polling mode.

         Packet_Builder.Leave_Configuration_Mode (Buffer);
         Self.Channel.Exchange (Buffer, Status);

         if Status.No_Acknowledge then
            return;
         end if;

         --  Compute polling cycle duration.

         Cycle_Duration := Polling_Time_Span (Configuration.Rate);

         --  Switch active configuration.

         Self.Active_Configuration := @ + 1;
      end Do_Configure;

      -------------
      -- Do_Poll --
      -------------

      procedure Do_Poll is
         Buffer : Communication.Buffer
           renames Self.Buffer (Self.Communication_Buffer);
         Status : aliased Communication.Status;

      begin
         Build_Poll_Packet (Buffer);

         Self.Channel.Exchange (Buffer, Status);

         if Status.No_Acknowledge then
            --  Communucation failure.

            null;

         else
            Self.Communication_Buffer := @ + 1;
         end if;
      end Do_Poll;

      --------------
      -- Do_Probe --
      --------------

      procedure Do_Probe
        (State          : in out Driver_State;
         Cycle_Duration : out Ada.Real_Time.Time_Span)
      is
         use type A0B.Types.Unsigned_8;

         Buffer : Communication.Buffer
           renames Self.Buffer (Self.Communication_Buffer);
         Status : aliased Communication.Status;

      begin
         --  Try to poll controller

         Build_Poll_Packet (Buffer);
         Self.Channel.Exchange (Buffer, Status);

         if Status.No_Acknowledge then
            if Status.Last < 1 then
               --  Controller is not connected.

               return;
            end if;

            --  Controller acknowledge 1 data word only of the 0x42 (poll)
            --  packet in the configuration mode/digital mode (while as usual
            --  in the configuration mode it reports length of the packet as
            --  3 words), so check whether reported mode is the configuration
            --  mode of not.

            if Controller_Mode (Buffer) /= Configuration_Mode then
               --  Something is going wrong.

               return;
            end if;
         end if;

         --  Enter configuration mode

         if Controller_Mode (Buffer) /= Configuration_Mode then
            Packet_Builder.Enter_Configuration_Mode (Buffer);
            Self.Channel.Exchange (Buffer, Status);

            if Status.No_Acknowledge then
               return;

            else
               delay until Ada.Real_Time.Clock + Packet_Interval;
            end if;
         end if;

         --  Request controller identification.

         Packet_Builder.Controller_Identification (Buffer);
         Self.Channel.Exchange (Buffer, Status);

         if Status.No_Acknowledge then
            return;

         else
            if Buffer (3) /= 16#03# then
               --  Only DualShock 2 controller is supported.

               raise Program_Error;
            end if;

            delay until Ada.Real_Time.Clock + Packet_Interval;
         end if;

         --  Switch controller to digital mode.

         Packet_Builder.Enable_Digital_Mode (Buffer, False);
         Self.Channel.Exchange (Buffer, Status);

         if Status.No_Acknowledge then
            return;

         else
            delay until Ada.Real_Time.Clock + Packet_Interval;
         end if;

         --  Leave configuration mode

         Packet_Builder.Leave_Configuration_Mode (Buffer);
         Self.Channel.Exchange (Buffer, Status);

         if Status.No_Acknowledge then
            return;
         end if;

         Self.Configuration (Self.Active_Configuration) :=
           (Rate             => 250,
            Analog_Joysticks => False,
            Analog_Buttons   => False);

         Cycle_Duration := Polling_Time_Span (250);
         State          := Poll;
      end Do_Probe;

      Cycle_Duration : Ada.Real_Time.Time_Span := Ada.Real_Time.Tick;
      Next_Cycle     : Ada.Real_Time.Time := Ada.Real_Time.Clock;

      State          : Driver_State := Probe;

   begin
      loop
         if not Self.Driver_Enabled then
            --  Driver's task is not enabled, suspend execution.

            Ada.Synchronous_Task_Control.Suspend_Until_True
              (Self.Driver_Suspension);

            --  Reset state.

            Next_Cycle := Ada.Real_Time.Clock;
            State      := Probe;
         end if;

         if State = Poll and Self.Change_Configuration then
            State                     := Configure;
            Self.Change_Configuration := False;
         end if;

         case State is
            when Probe =>
               Do_Probe (State, Cycle_Duration);

            when Poll =>
               Do_Poll;

            when Configure =>
               Do_Configure (Cycle_Duration);
               State := Poll;

            when Failure =>
               null;
         end case;

         Next_Cycle := @ + Cycle_Duration;

         delay until Next_Cycle;
      end loop;
   end Driver;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out PlayStation2_Controller'Class) is
   begin
      Ada.Synchronous_Task_Control.Set_False (Self.Driver_Suspension);
      Self.Driver_Enabled := False;
      --  XXX How to wait till driver completes operations before shutdown
      --  hardware?
      --  XXX Should hardware initialization/shutdown be responsibility of
      --  the driver task?
   end Finalize;

   ---------------
   -- Get_State --
   ---------------

   procedure Get_State
     (Self  : PlayStation2_Controller'Class;
      State : out Controller_State)
   is

      type Digital_Buttons_1 is record
         Select_Button          : Boolean;
         Left_Joystick          : Boolean;
         Right_Joystick         : Boolean;
         Start_Button           : Boolean;
         Up_Direction_Button    : Boolean;
         Right_Direction_Button : Boolean;
         Down_Direction_Button  : Boolean;
         Left_Direction_Button  : Boolean;
      end record with Pack, Size => 8;

      type Digital_Buttons_2 is record
         Left_2_Button   : Boolean;
         Right_2_Button  : Boolean;
         Left_1_Button   : Boolean;
         Right_1_Button  : Boolean;
         Triangle_Button : Boolean;
         Circle_Button   : Boolean;
         Cross_Button    : Boolean;
         Square_Button   : Boolean;
      end record with Pack, Size => 8;

      type Analog_Buttons is record
         Right_Joystick_Horizontal : A0B.Types.Unsigned_8;
         Right_Joystick_Vertical   : A0B.Types.Unsigned_8;
         Left_Joystick_Horizontal  : A0B.Types.Unsigned_8;
         Left_Joystick_Vertical    : A0B.Types.Unsigned_8;

         Right_Direction_Button    : A0B.Types.Unsigned_8;
         Left_Direction_Button     : A0B.Types.Unsigned_8;
         Up_Direction_Button       : A0B.Types.Unsigned_8;
         Down_Direction_Button     : A0B.Types.Unsigned_8;

         Triangle_Button           : A0B.Types.Unsigned_8;
         Circle_Button             : A0B.Types.Unsigned_8;
         Cross_Button              : A0B.Types.Unsigned_8;
         Square_Button             : A0B.Types.Unsigned_8;

         Left_1_Button             : A0B.Types.Unsigned_8;
         Right_1_Button            : A0B.Types.Unsigned_8;
         Left_2_Button             : A0B.Types.Unsigned_8;
         Right_2_Button            : A0B.Types.Unsigned_8;
      end record with Pack, Size => 128;

      Buffer        : Communication.Buffer
        renames Self.Buffer (Self.Communication_Buffer + 1);
      Configuration : Async.Configuration
        renames Self.Configuration (Self.Active_Configuration);

      Digital_1 : constant Digital_Buttons_1
        with Import, Address => Buffer (3)'Address;
      Digital_2 : constant Digital_Buttons_2
        with Import, Address => Buffer (4)'Address;
      Analog    : constant Analog_Buttons
        with Import, Address => Buffer (5)'Address;

   begin
      --  In digital mode, joystick position is not reported. Report of the
      --  middle position is forced.

      State.Right_Joystick_Horizontal := 16#80#;
      State.Right_Joystick_Vertical   := 16#80#;
      State.Left_Joystick_Horizontal  := 16#80#;
      State.Left_Joystick_Vertical    := 16#80#;

      --  Digital state of the buttons is send always. Some buttons doesn't
      --  have analog mode. So, decode digital state of the all buttons and
      --  overwrite values by analog state later, depending from the active
      --  configuration.

      State.Left_Direction_Button :=
        (if Digital_1.Left_Direction_Button then 16#00# else 16#FF#);
      State.Down_Direction_Button :=
        (if Digital_1.Down_Direction_Button then 16#00# else 16#FF#);
      State.Right_Direction_Button :=
        (if Digital_1.Right_Direction_Button then 16#00# else 16#FF#);
      State.Up_Direction_Button :=
        (if Digital_1.Up_Direction_Button then 16#00# else 16#FF#);
      State.Start_Button :=
        (if Digital_1.Start_Button then 16#00# else 16#FF#);
      State.Right_Joystick :=
        (if Digital_1.Right_Joystick then 16#00# else 16#FF#);
      State.Left_Joystick :=
        (if Digital_1.Left_Joystick then 16#00# else 16#FF#);
      State.Select_Button :=
        (if Digital_1.Select_Button then 16#00# else 16#FF#);

      State.Square_Button :=
        (if Digital_2.Square_Button then 16#00# else 16#FF#);
      State.Cross_Button :=
        (if Digital_2.Cross_Button then 16#00# else 16#FF#);
      State.Circle_Button :=
        (if Digital_2.Circle_Button then 16#00# else 16#FF#);
      State.Triangle_Button :=
        (if Digital_2.Triangle_Button then 16#00# else 16#FF#);
      State.Right_1_Button :=
        (if Digital_2.Right_1_Button then 16#00# else 16#FF#);
      State.Left_1_Button :=
        (if Digital_2.Left_1_Button then 16#00# else 16#FF#);
      State.Right_2_Button :=
        (if Digital_2.Right_2_Button then 16#00# else 16#FF#);
      State.Left_2_Button :=
        (if Digital_2.Left_2_Button then 16#00# else 16#FF#);

      if Configuration.Analog_Joysticks then
         State.Right_Joystick_Horizontal := Analog.Right_Joystick_Horizontal;
         State.Right_Joystick_Vertical   := Analog.Right_Joystick_Vertical;
         State.Left_Joystick_Horizontal  := Analog.Left_Joystick_Horizontal;
         State.Left_Joystick_Vertical    := Analog.Left_Joystick_Vertical;

         if Configuration.Analog_Buttons then
            State.Right_Direction_Button := Analog.Right_Direction_Button;
            State.Left_Direction_Button  := Analog.Left_Direction_Button;
            State.Up_Direction_Button    := Analog.Up_Direction_Button;
            State.Down_Direction_Button  := Analog.Down_Direction_Button;

            State.Triangle_Button        := Analog.Triangle_Button;
            State.Circle_Button          := Analog.Circle_Button;
            State.Cross_Button           := Analog.Cross_Button;
            State.Square_Button          := Analog.Square_Button;

            State.Left_1_Button          := Analog.Left_1_Button;
            State.Right_1_Button         := Analog.Right_1_Button;
            State.Left_2_Button          := Analog.Left_2_Button;
            State.Right_2_Button         := Analog.Right_2_Button;
         end if;
      end if;
   end Get_State;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out PlayStation2_Controller'Class) is
   begin
      --  Initialize communication channel.

      Self.Channel.Initialize;

      --  Enable driver's task.

      Self.Driver_Enabled := True;
      Ada.Synchronous_Task_Control.Set_True (Self.Driver_Suspension);
   end Initialize;

   --------------------
   -- Packet_Builder --
   --------------------

   package body Packet_Builder is

      -------------------------------
      -- Controller_Identification --
      -------------------------------

      procedure Controller_Identification (Buffer : out Communication.Buffer) is
      begin
         Buffer := [others => 16#5A#];

         Buffer (0) := 16#01#;
         Buffer (1) := 16#45#;
         Buffer (2) := 16#00#;
      end Controller_Identification;

      --------------------------
      -- Enable_Analog_Button --
      --------------------------

      procedure Enable_Analog_Button
        (Buffer : out Communication.Buffer;
         Button : A0B.Types.Unsigned_8) is
      begin
         Buffer := [others => 16#00#];

         Buffer (0) := 16#01#;
         Buffer (1) := 16#40#;
         Buffer (2) := 16#00#;

         Buffer (3) := Button;
         Buffer (4) := 16#02#;
      end Enable_Analog_Button;

      ------------------------
      -- Enable_Analog_Mode --
      ------------------------

      procedure Enable_Analog_Mode
        (Buffer : out Communication.Buffer; Lock : Boolean) is
      begin
         Buffer := [others => 16#00#];

         Buffer (0) := 16#01#;
         Buffer (1) := 16#44#;
         Buffer (2) := 16#00#;

         Buffer (3) := 16#01#;
         Buffer (4) := (if Lock then 16#03# else 16#00#);
         --  XXX It is unclear what this byte do actually.
      end Enable_Analog_Mode;

      -------------------------
      -- Enable_Digital_Mode --
      -------------------------

      procedure Enable_Digital_Mode
        (Buffer : out Communication.Buffer; Lock : Boolean) is
      begin
         Buffer := [others => 16#00#];

         Buffer (0) := 16#01#;
         Buffer (1) := 16#44#;
         Buffer (2) := 16#00#;

         Buffer (3) := 16#00#;
         Buffer (4) := (if Lock then 16#03# else 16#00#);
         --  XXX It is unclear what this byte do actually.
      end Enable_Digital_Mode;

      ------------------------------
      -- Enter_Configuration_Mode --
      ------------------------------

      procedure Enter_Configuration_Mode (Buffer : out Communication.Buffer) is
      begin
         Buffer := [others => 16#00#];

         Buffer (0) := 16#01#;
         Buffer (1) := 16#43#;
         Buffer (2) := 16#00#;

         Buffer (3) := 16#01#;
      end Enter_Configuration_Mode;

      -----------------------------
      -- Get_Analog_Polling_Mask --
      -----------------------------

      procedure Get_Analog_Polling_Mask (Buffer : out Communication.Buffer) is
      begin
         Buffer := [others => 16#5A#];

         Buffer (0) := 16#01#;
         Buffer (1) := 16#41#;
         Buffer (2) := 16#00#;
      end Get_Analog_Polling_Mask;

      ------------------------------
      -- Leave_Configuration_Mode --
      ------------------------------

      procedure Leave_Configuration_Mode (Buffer : out Communication.Buffer) is
      begin
         Buffer := [others => 16#5A#];

         Buffer (0) := 16#01#;
         Buffer (1) := 16#43#;
         Buffer (2) := 16#00#;

         Buffer (3) := 16#00#;
      end Leave_Configuration_Mode;

      -----------------------------
      -- Set_Analog_Polling_Mask --
      -----------------------------

      procedure Set_Analog_Polling_Mask (Buffer : out Communication.Buffer) is
      begin
         Buffer := [others => 0];

         Buffer (0) := 16#01#;
         Buffer (1) := 16#4F#;
         Buffer (2) := 16#00#;

         Buffer (3) := 16#FF#;
         Buffer (4) := 16#FF#;
         Buffer (5) := 16#03#;
      end Set_Analog_Polling_Mask;

   end Packet_Builder;

   --------------------
   -- Payload_Length --
   --------------------

   function Payload_Length
     (Buffer : Communication.Buffer) return A0B.Types.Unsigned_8
   is
      use type A0B.Types.Unsigned_8;

   begin
      return Buffer (Buffer'First + 1) and 16#0F#;
   end Payload_Length;

   -----------------------
   -- Polling_Time_Span --
   -----------------------

   function Polling_Time_Span
     (Rate : Polling_Rate) return Ada.Real_Time.Time_Span is
   begin
      return Ada.Real_Time.Microseconds (1_000_000 / Integer (Rate));
   end Polling_Time_Span;

end A0B.PlayStation2_Controllers.Async;