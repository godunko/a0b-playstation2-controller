--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Ada.Real_Time;

with Ada.Text_IO; use Ada.Text_IO;

--  with System.tasking.Protected_Objects.Single_Entry;

package body A0B.PlayStation2_Controllers.Async is

   Packet_Interval : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Microseconds (20);
   --  Minimum interval between the packets.

   function Polling_Time_Span
     (Rate : Polling_Rate) return Ada.Real_Time.Time_Span;
   --  Computes polling cycle duration.

   --  procedure Build_Polling_Configuration

   --  Bits:
   --    0:7  Left arrow
   --    0:6  Down arrow
   --    0:5  Right arrow
   --    0:4  Up arrow
   --    0:3  Start
   --    0:2  Right Joystick Press
   --    0:1  Left Joystick Press
   --    0:0  Select
   --
   --    1:7  X button
   --    1:6  A button
   --    1:5  B button
   --    1:4  Y button
   --    1:3  RB
   --    1:2  LB
   --    1:1  RT
   --    1:0  LT
   --
   --  Bytes:
   --    0  RH
   --    1  RV
   --    2  LH
   --    3  LV
   --    4  Right arrow
   --    5  Left arrow
   --    6  Up arrow
   --    7  Down arrow
   --    8  Y button (Tri)
   --    9  B button (O)
   --   10  A button (X)
   --   11  X button (Sqr)
   --   12  LB (L1)
   --   13  RB (R1)
   --   14  LT (L2)
   --   15  RT (R2)

   procedure Build_Identity_Status
     (Buffer : out Communication.Buffer) is
   begin
      Buffer := [others => 16#5A#];

      Buffer (0) := 16#01#;
      Buffer (1) := 16#45#;
      Buffer (2) := 16#00#;
   end Build_Identity_Status;

   ---------------------------------
   -- Build_Enable_Analog_Polling --
   ---------------------------------

   procedure Build_Enable_Analog_Polling
     (Buffer : out Communication.Buffer;
      Enable : Boolean) is
   begin
      Buffer := [others => 0];

      Buffer (0) := 16#01#;
      Buffer (1) := 16#4F#;
      Buffer (2) := 16#00#;

      if Enable then
         Buffer (3) := 16#FF#;
         Buffer (4) := 16#FF#;
         Buffer (5) := 16#03#;

      else
         Buffer (3) := 16#00#;
         Buffer (4) := 16#00#;
         Buffer (5) := 16#00#;
      end if;
   end Build_Enable_Analog_Polling;

   -----------------------------------
   -- Build_Enable_Analog_Joysticks --
   -----------------------------------

   procedure Build_Enable_Analog_Joysticks
     (Buffer : out Communication.Buffer;
      Enable : Boolean;
      Lock   : Boolean) is
   begin
      Buffer := [others => 0];

      Buffer (0) := 16#01#;
      Buffer (1) := 16#44#;
      Buffer (2) := 16#00#;

      if Enable then
         Buffer (3) := 16#01#;

      else
         Buffer (3) := 16#00#;
      end if;

      if Lock then
         Buffer (4) := 16#03#;

      else
         Buffer (4) := 16#00#;
      end if;
   end Build_Enable_Analog_Joysticks;

   -------------------------------------
   -- Build_Change_Configuration_Mode --
   -------------------------------------

   procedure Build_Change_Configuration_Mode
     (Buffer : out Communication.Buffer;
      Enable : Boolean) is
   begin
      Buffer := [others => 0];

      Buffer (0) := 16#01#;
      Buffer (1) := 16#43#;
      Buffer (2) := 16#00#;

      if Enable then
         Buffer (3) := 16#01#;

      else
         Buffer (3) := 16#00#;
      end if;
   end Build_Change_Configuration_Mode;

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

         Build_Change_Configuration_Mode (Buffer, Enable => True);
         Self.Channel.Exchange (Buffer, Status);

         if Status.Not_Acknoledged then
            --  Communication failures.

            return;
         end if;

         delay until Ada.Real_Time.Clock + Packet_Interval;

         --  Request identification/status

         Build_Identity_Status (Buffer);
         Self.Channel.Exchange (Buffer, Status);

         if Status.Not_Acknoledged then
            --  Communication failures.

            return;
         end if;

         delay until Ada.Real_Time.Clock + Packet_Interval;

         --  Enable analog joysticks

         Build_Enable_Analog_Joysticks
           (Buffer, Enable => Configuration.Analog_Joysticks, Lock => True);
         Self.Channel.Exchange (Buffer, Status);

         if Status.Not_Acknoledged then
            --  Communication failures.

            return;
         end if;

         delay until Ada.Real_Time.Clock + Packet_Interval;

         Build_Enable_Analog_Polling
           (Buffer, Enable => Configuration.Analog_Buttons);
         Self.Channel.Exchange (Buffer, Status);

         if Status.Not_Acknoledged then
            --  Communication failures.

            return;
         end if;

         delay until Ada.Real_Time.Clock + Packet_Interval;

         --  Switch controller into normal polling mode.

         Build_Change_Configuration_Mode (Buffer, False);
         Self.Channel.Exchange (Buffer, Status);

         if Status.Not_Acknoledged then
            --  Communication failures.

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

         if Status.Not_Acknoledged then
            --  Communucation failure.

            null;

         else
            Self.Communication_Buffer := @ + 1;
         end if;
      end Do_Poll;

      Cycle_Duration       : Ada.Real_Time.Time_Span := Ada.Real_Time.Tick;
      Next_Cycle           : Ada.Real_Time.Time := Ada.Real_Time.Clock;

      State                : Driver_State := Probe;

      Communication_Status : aliased Communication.Status;
      --  Communication_Done   :
      --    aliased Ada.Synchronous_Task_Control.Suspension_Object;

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
               Build_Poll_Packet (Self.Buffer (Self.Communication_Buffer));

               Self.Channel.Exchange
                 (Self.Buffer (Self.Communication_Buffer),
                  Communication_Status);

               if Communication_Status.Not_Acknoledged then
                  --  Communucation failure.

                  null;

               else
                  --  Guess current configuration from the length of the data
                  --  packet.

                  case Payload_Length
                         (Self.Buffer (Self.Communication_Buffer))
                  is
                     when 1 =>
                        Self.Configuration (Self.Active_Configuration)
                          .Analog_Joysticks := False;
                        Self.Configuration (Self.Active_Configuration)
                          .Analog_Buttons   := False;

                     when 3 =>
                        Self.Configuration (Self.Active_Configuration)
                          .Analog_Joysticks := True;
                        Self.Configuration (Self.Active_Configuration)
                          .Analog_Buttons   := False;

                     when 9 =>
                        Self.Configuration (Self.Active_Configuration)
                          .Analog_Joysticks := True;
                        Self.Configuration (Self.Active_Configuration)
                          .Analog_Buttons   := True;

                     when others =>
                        null;
                  end case;

                  Cycle_Duration :=
                    Polling_Time_Span
                      (Self.Configuration (Self.Active_Configuration).Rate);

                  Self.Communication_Buffer := @ + 1;
                  State := Poll;
               end if;

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
      State : out Controller_State) is
   begin
      null;
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
     (Rate : Polling_Rate) return Ada.Real_Time.Time_Span
   is
      use type Ada.Real_Time.Time_Span;

      Frequency : constant := 1.0 / Ada.Real_Time.Time_Unit;
      --  Timer's frequency is necessary to compute number of ticks of the
      --  polling interval.

   begin
      return Ada.Real_Time.Tick * (Integer (Frequency) / Integer (Rate));
   end Polling_Time_Span;

end A0B.PlayStation2_Controllers.Async;